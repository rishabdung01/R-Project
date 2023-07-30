
library(ggplot2)
library(ggvis)
library(corrplot)
library(caTools)
library(ROCR)

data = read.csv("C:/Users/DELL/Desktop/R PROJECT/diabetes prediction dataset.csv")

head(data)

install.packages('caret')
 
summary(data)
 
str(data)

correlations <- cor(data)
correlations

corrplot(correlations, method="color")

pairs(data, col=data$Outcome)

data %>% ggvis(~Glucose,~Insulin,fill =~Outcome) %>% layer_points()

data %>% ggvis(~BMI,~DiabetesPedigreeFunction,fill =~Outcome) %>% layer_points()

set.seed(88)
split <- sample.split(data$Outcome, SplitRatio = 0.75)
data_train <- subset(data, split == TRUE)
data_test <- subset(data, split == FALSE)

model <- glm (Outcome ~ .-Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = data_train, family = binomial)
summary(model)

head(trainData)

predict_train <- predict(model, type = 'response')
predict_test <- predict(model, newdata = data_test, type = 'response')

summary(trainData)

ROCRpred <- prediction(predict_train, data_train$Outcome)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

predict_test_c = predict_test
i = 1
while(i <= length(predict_test))
{
  if(predict_test[i] < 0.5)
    predict_test_c[i] = 0
  else
    predict_test_c[i] = 1
  i = i + 1;
}
compare <- data.frame(data_test$Outcome,predict_test_c)
colnames(compare) <- c("Observed Values","Predicted values")
ggplot(data = compare,aes(x = "Observed Values", y = "Predicted values")) + geom_abline() +
  xlab("Observed Values") + ylab("Predicted values") + theme_classic()
compare




