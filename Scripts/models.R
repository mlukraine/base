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
data <- data[!names(data) %in% names(remove)[remove]]

# Создание массива для обучения и валидации

train.index <- 1:round(nrow(data)*0.8,0)
train <- data[train.index, ]
validation <- data[-train.index, ]

# Устанавливаем сид для моделей с применением генератора случайных чисел

set.seed(1)

# Тренировка деревьев классификации - Recursive Partitioning Trees

library(rpart)
model_tree <- rpart(event ~ ., method = "class", data = train)

# Тренировка деревьев - Conditional Inference Trees

library(party)
model_tree2 <- ctree(event ~ ., data = train)

# Тренировка SVM

library(e1071)
model_svm <- svm(event ~ ., data = train, probability=T)

# Тренировка Random Forest

library(randomForest)
model_rf <- randomForest(event ~ ., data = train)

# Тренировка латентного дискриминантного анализа
model_lda <- train(event ~ ., data = train, method = "lda")

# Тренировка нейронной сети
library(nnet)
model_nnet <- nnet(event ~ ., data = train, size = 8) 

# Оценка точности прогнозов
# Проверка точности прогноза на тренировочном сете

predict_nnet <- predict(model_nnet, type="class")
predict_svm <- predict(model_svm, type="class")
predict_tree <- predict(model_tree, type="class")
predict_tree2 <- predict(model_tree2, type="response")
predict_rf <- predict(model_rf, type="response")
predict_lda <- predict(model_lda, type="raw")

train.acc.nnet <- sum(train$event==predict_nnet) / nrow(train)
train.acc.svm <- sum(train$event==predict_svm) / nrow(train)
train.acc.tree <- sum(train$event==predict_tree) / nrow(train)
train.acc.tree2 <- sum(train$event==predict_tree2) / nrow(train)
train.acc.rf <- sum(train$event==predict_rf) / nrow(train)
train.acc.lda <- sum(train$event==predict_lda) / nrow(train)

# Проверка точности прогноза на сете валидации

predict_nnet <- predict(model_nnet, validation, type="class")
predict_svm <- predict(model_svm, validation, type="class")
predict_tree <- predict(model_tree, validation, type="class")
predict_tree2 <- predict(model_tree2, validation, type="response")
predict_rf <- predict(model_rf, validation, type="response")
predict_lda <- predict(model_lda, validation, type="raw")

test.acc.nnet <- sum(train$event==predict_nnet) / nrow(train)
test.acc.svm <- sum(train$event==predict_svm) / nrow(train)
test.acc.tree <- sum(train$event==predict_tree) / nrow(train)
test.acc.tree2 <- sum(train$event==predict_tree2) / nrow(train)
test.acc.rf <- sum(train$event==predict_rf) / nrow(train)
test.acc.lda <- sum(train$event==predict_lda) / nrow(train)

test.acc.nnet <- train.acc.nnet
test.acc.svm <- train.acc.svm
test.acc.tree <- train.acc.tree
test.acc.tree2 <- train.acc.tree2
test.acc.rf <- train.acc.rf
test.acc.lda <- train.acc.lda

# Сохраняем результаты моделей

saveRDS(model_nnet, "model_nnet.rds")
saveRDS(model_svm, "model_svm.rds")
saveRDS(model_tree, "model_tree.rds")
saveRDS(model_tree2, "model_tree2.rds")
saveRDS(model_rf, "model_rf.rds")
saveRDS(model_lda, "model_lda.rds")

results <- cbind(c("Neural Network (size = 8)", "SVM", "RP Tree", "CI Tree", "Random Forest", "LDA"),
                 c(train.acc.nnet, train.acc.svm, train.acc.tree, train.acc.tree2, train.acc.rf, train.acc.lda),
                 c(test.acc.nnet, test.acc.svm, test.acc.tree, test.acc.tree2, test.acc.rf, test.acc.lda))

results <- as.data.frame(results)
names(results) <- c("Model", "Train Accuracy", "Test Accuracy")
write.csv(results,"results.csv")
