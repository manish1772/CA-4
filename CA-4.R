
#reading the data 
simple_crime_data <-read.csv("simpleCrimeData.csv")

#Checking the structure of the final cleaned crime data
str(simple_crime_data)


#we use seed to 1 produce to produce the same sample randomly
set.seed(1)
no_rows_data <- nrow(simple_crime_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

#Splitting the data into train and test
training_data <- simple_crime_data[sample, ]
testing_data <- simple_crime_data[-sample, ]

training_data <- subset(training_data, select =  -c(Region))
testing_data <- subset(testing_data, select =  -c(Region))


#fitting the data using multiple linear regression
fit <- lm(crime_rate ~ Total_MAH,Total_DN + Total_kidnap + Total_REHi + Total_Burglary + Total_Theft + 
            Total_Fraud + Total_weapon + Total_PDE + Total_poSo, data=training_data)

plot(fit)

#The summary function gives the details of the model.How it interpreted the results
summary(fit)

#confint gives the confidence intervel for each feature
confint(fit)


#QQplot helps in determining the outliers in the data
library(car)
qqPlot(fit, labels=row.names(simple_crime_data), id.method="identify", simulate=TRUE, main="Q-Q Plot")



#Lets look at actual crime rate values of the data
training_data[272,]
training_data[125,]
training_data[306,]
training_data[347,]

#This gives the predicted values 
fitted(fit)[272]
fitted(fit)[125]

#OutilerTest is used to check for the outlier in the data
library(car)
outlierTest(fit)



#removing outliers from data
simple_crime_data <- subset(simple_crime_data, simple_crime_data$id!=20164)
simple_crime_data <- subset(simple_crime_data, simple_crime_data$id!=206)
simple_crime_data <- subset(simple_crime_data, simple_crime_data$id!=2030)
simple_crime_data <- subset(simple_crime_data, simple_crime_data$id!=2035)


set.seed(1)
no_rows_data <- nrow(simple_crime_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

#Splitting the data into train and test
training_data <- simple_crime_data[sample, ]
testing_data <- simple_crime_data[-sample, ]

#fitting the data using multiple linear regression
fit <- lm(crime_rate ~ Total_DN + Total_kidnap + Total_REHi + Total_Burglary + Total_Theft + 
            Total_Fraud + Total_weapon + Total_PDE + Total_poSo, data=training_data)

#Outlier test
outlierTest(fit)

#component residual plots to check linearity with each feature
crPlots(fit)

#checking for influence variables
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")


#Added variable plot
avPlots(fit, ask=FALSE)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

library(car)
#mutlicolinearity check
vif(fit)


#To get prediction values
predicted_crime_rate <- predict(fit, testing_data)

#bulding a dataframe which contains predicted and actual variables
actuals_predictions <- data.frame(cbind(actuals = testing_data$crime_rate, predicted = predicted_crime_rate))
head(actuals_predictions)

#Accuracy check
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

#mean absolute percentage error 
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

#Residual Standard Error (RSE)
sigma(fit)/ mean(testing_data$crime_rate)

summary(numerical_pca_list)

df <- data.frame(Total_MAH = c(100),Total_DN = c(600), Total_kidnap =c(5), Total_REHi =c(200), Total_Burglary=c(270), Total_Theft =c(800), 
                 Total_Fraud = c(120), Total_weapon = c(90), Total_PDE = c(1109), Total_poSo = c(2482))

predicted_value <- predict(fit,df)
predicted_value


df1 <- data.frame(Total_MAH = c(1500),Total_DN = c(600), Total_kidnap =c(500), Total_REHi =c(200), Total_Burglary=c(270), Total_Theft =c(800), 
                  Total_Fraud = c(120), Total_weapon = c(1090), Total_PDE = c(1109), Total_poSo = c(2482))

predicted_value1 <- predict(fit,df1)
predicted_value1

df2 <- data.frame(Total_MAH = c(100),Total_DN = c(60), Total_kidnap =c(5), Total_REHi =c(200), Total_Burglary=c(270), Total_Theft =c(800), 
                  Total_Fraud = c(120), Total_weapon = c(900), Total_PDE = c(1109), Total_poSo = c(2482))

predicted_value <- predict(fit,df2)
predicted_value

df3 <- data.frame(Total_MAH = c(2000),Total_DN = c(60), Total_kidnap =c(5), Total_REHi =c(200), Total_Burglary=c(270), Total_Theft =c(800), 
                  Total_Fraud = c(120), Total_weapon = c(500), Total_PDE = c(1109), Total_poSo = c(2482))

predicted_value <- predict(fit,df3)
predicted_value
