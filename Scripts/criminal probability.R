# Подключаем необходимые библиотеки

library(dplyr)
library(tidyr)

# Читаем данные

data <- read.csv("criminal/model_data.csv", header = T, stringsAsFactors = F)

# Удаляем номера строк

data$X <- NULL

# Преобразовывем дату в машиночитаемый вид

data$date <- strptime(data$date, "%Y-%m-%d %H:%M:%S")

# Сортируем данные по дате

data <- data[order(data$date), ]

# Преобразовываем переменнные в факторы

data$district <- as.factor(data$district)
data$Conditions <- as.factor(data$Conditions)
data$weekday <- as.factor(data$weekday)
data$event <- as.factor(data$event)
data$date <- NULL

# Удаляем все кейсы с пропущенными значениями

data <- data[complete.cases(data), ]

# Удаляем переменные, которые являются константами для всего массива

remove = lapply(sapply(data, table), length)==1
data <- train[!names(data) %in% names(remove)[remove]]

# Создание массива для обучения и валидации

train.index <- 1:round(nrow(data)*0.8,0)
train <- data[train.index, ]
validation <- data[-train.index, ]

# Подготовка усредненных данных по погоде

weather <- unique(train[c("weekday","month","hour","TemperatureC","Wind.SpeedKm.h","Humidity","Sea.Level.PressurehPa")])
weather <- weather %>% group_by(weekday, month, hour) %>% 
                       summarise(TemperatureC = mean(TemperatureC),
                       Wind.SpeedKm.h = mean(Wind.SpeedKm.h),
                       Humidity = mean(Humidity),
                       Sea.Level.PressurehPa = mean(Sea.Level.PressurehPa))

# Подготовка данных по местам публичного пользования

places <- train[c(4:5,11:37)]
places$latlon <- paste0(places$lat, places$lon)
places <- unique(places)

# Создаем данные с условными вероятностями по совершению преступлений

train$latlon <- paste0(train$lat,train$lon)
train$z <- 1

newdata <- train %>%
  complete(event, latlon, month, weekday, hour, fill = list(z = 0)) %>% 
  group_by(event, latlon, month, weekday, hour) %>% 
  summarise(count = mean(z))


newdata <- merge(x = newdata, y = weather, by = c("weekday","month","hour"), all.x = T)
newdata <- merge(x = newdata, y = places, by = "latlon")

# Готовим данные к моделированию

newdata$latlon <- NULL

# Простейшая регрессионная модель

model <- lm(count ~ ., data=newdata)