# Загружаем библиотеку для работы с данными

library(dplyr)

# Выбираем дистанцию, которая определяет считать ли место публичного пользования близким к месту правонарушения
radius = 200

# Загружаем основной массив данных
data <- read.csv("all_crimes.csv", stringsAsFactors = F)

# Вычищаем данные, которые не имеют определенного местоположения и события, которые произошли до 2014 года
data <- data[!is.na(data$lat), ]
data <- data[as.Date(data$format_date) >= as.Date("2014-01-01"), ]

# Загружаем все места общественного пользования
amenities <- read.csv("amenities-nodes.csv", sep = "\t", header = T)

# Поскольку слишком много типов мест, некоторые из них есть смысл объединить
amenities$amenity_type[amenities$amenity_type %in% c("bar", "bbq", "biergarten", "cafe", "fast_food", "food_court", "internet_access", "internet_cafe", "kitchen", "restaurant", "pub")] <- "cafe"
amenities$amenity_type[amenities$amenity_type %in% c("agency", "bookmaker", "casino", "lottery", "office", "pawnbrocker", "pawnshop", "register_office", "community_centre", "coworking_space")] <- "agency"
amenities$amenity_type[amenities$amenity_type %in% c("bicycle_parking", "bicycle_rental", "bicycle_repair_station", "ski_rental", "boat_rental")] <- "bicycle_rental"
amenities$amenity_type[amenities$amenity_type %in% c("atm", "bureau_de_change", "payment_terminal", "post_office;bank;atm")] <- "atm"
amenities$amenity_type[amenities$amenity_type %in% c("bus_station", "substation", "taxi")] <- "taxi"
amenities$amenity_type[amenities$amenity_type %in% c("car_rental", "car_sharing", "car_wash", "charging_station", "fuel", "parking", "parking_entrance", "parking_space", "vehicle_inspection")] <- "parking"
amenities$amenity_type[amenities$amenity_type %in% c("cinema", "arts_centre", "library", "publisher", "stage", "studio", "theatre", "tourism")] <- "arts_centre"
amenities$amenity_type[amenities$amenity_type %in% c("clinic", "emergency_service", "fire_station", "hospital", "rescue_station", "sanatorium", "doctors", "dentist", "pharmacy")] <- "hospital"
amenities$amenity_type[amenities$amenity_type %in% c("drinking_water", "water_point")] <- "drinking_water"
amenities$amenity_type[amenities$amenity_type %in% c("education", "educational", "educational_institute", "kindergarten", "school", "translation", "university", "college")] <- "education"
amenities$amenity_type[amenities$amenity_type %in% c("embassy", "government", "social_facility", "townhall", "wedding", "courthouse", "public_building")] <- "government"
amenities$amenity_type[amenities$amenity_type %in% c("nightclub", "sauna", "stripclub", "club")] <- "nightclub"
amenities$amenity_type[amenities$amenity_type %in% c("post_office", "post_box")] <- "post_box"
amenities$amenity_type[amenities$amenity_type %in% c("grocery", "shop")] <- "shop"
amenities$amenity_type[amenities$amenity_type %in% c("gym", "swimming_pool", "training", "training centr")] <- "gym"
amenities$amenity_type[amenities$amenity_type %in% c("telephone", "thermometer", "toilets", "vending_machine", "clock")] <- "vending_machine"
amenities$amenity_type[amenities$amenity_type %in% c("pet", "veterinary")] <- "veterinary"
amenities$amenity_type[amenities$amenity_type %in% c("recycling", "waste_basket", "waste_disposal")] <- "recycling"

# Создаем факторную переменную
amenities$amenity_type <- factor(amenities$amenity_type)

# Переименовываем в местах переменные pos1 и pos2 на lat и lon соответственно
names(amenities)[5:6] <- c("lat", "lon")

# Создаем для каждой точки наших событий уникальный идентификатор из долготы и широты
data$latlon <- paste0(data$lat, data$lon)

# Оставляем только уникальные точки
unique <- data[!duplicated(data$latlon), c("lat", "lon", "latlon")]

# Для каждой точки создаем переменную, указывающую на наличие того или иного места общественного пользования поблизости
for (i in names(table(amenities$amenity_type))) {
  unique[i] <- FALSE
}

# Задаем функции для измерения расстояния - переходим от долготы и широты к метрам

mes <- function(obj1, obj2){
  R <- 6378.137; # Радиус Земли в км
  dLat <- obj2$lat * pi / 180 - obj1$lat * pi / 180;
  dLon <- obj2$lon * pi / 180 - obj1$lon * pi / 180;
  a <- sin(dLat/2) * sin(dLat/2) +
    cos(obj1$lat * pi / 180) * cos(obj2$lat * pi / 180) *
    sin(dLon/2) * sin(dLon/2);
  c <- 2 * atan2(sqrt(a), sqrt(1 - a));
  d <- R * c;
  return(d * 1000); # метры
}

measure <- function(lat1, lon1, lat2, lon2){
   R <- 6378.137;
   dLat <- lat2 * pi / 180 - lat1 * pi / 180;
   dLon <- lon2 * pi / 180 - lon1 * pi / 180;
   a <- sin(dLat/2) * sin(dLat/2) +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
    sin(dLon/2) * sin(dLon/2);
   c <- 2 * atan2(sqrt(a), sqrt(1 - a));
   d <- R * c;
   return(d * 1000);
}

# Для каждой точки определяем наличие типа обшественного пользования в радиусе 200 м
for (i in 1:nrow(unique)) {
  v <- transmute(amenities,
                distance <- measure(unique$lat[i], unique$lon[i], lat, lon))

  if(length(unique(amenities$amenity_type[v <= radius])) != 0) {
    for (j in unique(amenities$amenity_type[v <= radius])) {
      unique[i,j] <- TRUE
    }
  }
}

# Излишние переменные
unique$lat <- NULL
unique$lon <- NULL

# Делаем JOIN с исходным массивом данных, чтобы для каждого события можно было указано наличие сущности рядом
newdata <- merge(data, unique, by="latlon")

# Считываем данные о погоде
weather <- read.csv("weather.csv", header = T, stringsAsFactors = F)

# Приводим даты по обоим массивам в корректный вид
weather$format_date <- strptime(weather$DateUTC, "%d.%m.%y %H:%M")
newdata$format_date <- strptime(newdata$date, "%d.%m.%Y %H:%M:%S")

# Удаляем дублирующиеся данные из погоды
weather <- weather[!duplicated(weather$format_date), ]

# Разбиваем даты исходного массива по датам, которые есть в данных о погоде
c <- cut.POSIXt(as.POSIXct(newdata$format_date), as.POSIXct(weather$format_date))

# Создаем для обоих массивов переменные для объединения
newdata$join <- as.character(c)
weather$join <- as.character(weather$format_date)

# Делаем JOIN
newdata2 <- merge(newdata, weather[c("join", "TemperatureC", "Wind.SpeedKm.h", "Conditions",
                                     "Humidity", "Sea.Level.PressurehPa")], by="join")

# Добавляем к каждой точке данных расстояние до центра Киева
newdata2$centre_dist <- measure(newdata2$lat, newdata2$lon, 50.401699, 30.252512)

# Загружаем данные о курсе доллара, евро и рубля
exchange <- read.csv("exchange_rate.csv", header = T, stringsAsFactors = F)

# Приводим даты массива в нужный вид
exchange$format_date <- strptime(exchange$date, "%Y-%m-%d")

# Разбиваем исходный массив по датам
c <- cut.POSIXt(as.POSIXct(newdata2$format_date), as.POSIXct(exchange$format_date))

# Создаем переменные для JOIN
newdata2$join <- as.character(c)
exchange$join <- as.character(exchange$format_date)

# Делаем JOIN
newdata3 <- merge(newdata2,exchange,by="join")

# Записываем новый массив
write.csv(newdata3, "all_data.csv")