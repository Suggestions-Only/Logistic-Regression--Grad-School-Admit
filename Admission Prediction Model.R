library(mlbench)
library(caret)
library(ModelMetrics)

#load in data, change admit to factor
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata$admit <- as.factor(mydata$admit)

#fit logistic regression model
admit.model <- glm(admit ~ gre + gpa + rank, family = binomial(link = "logit"),
                   data = mydata)
summary(admit.model)

#test logistic regression model fit
anova(admit.model, test = "Chisq")

#generate prediction on fake data
hannah <- data.frame(gre = 652, gpa = 3.72, rank = 1)
predict(admit.model, newdata = hannah, type = "response")

#generate predictions on existing data, specify cutoff, and assess prediction accuracy
admit.pred <- predict(admit.model, newdata = mydata, type = "response")
mydata$admit.pred <- admit.pred
mydata %>%
  group_by(admit) %>%
  summarise(med = median(admit.pred))
pred.admit <- ifelse(admit.pred > .379, 1, 0)
auc(actual = mydata$admit, predicted = pred.admit)