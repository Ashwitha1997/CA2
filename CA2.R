#Import the dataset 

heart_data <- read.csv("HeartFailure.csv")
str(heart_data)

#removes any rows that contains NA
heart_data <- na.omit(heart_data)

#drops any duplicate rows
heart_data <- unique(heart_data)

#checking for missing  values
is.na(heart_data)

#Renaming variables so not to create mistakes or get issues with variable names
colnames(heart_data)[colnames(heart_data) == "creatinine_phosphokinase"] <- "cre_pho"
colnames(heart_data)[colnames(heart_data) == "serum_creatinine"] <- "serum_cre"


#Examine the linearity between the variables in the dataset
library(psych)
pairs.panels(heart_data,
             smooth = FALSE,      
             scale = FALSE,      
             density = TRUE,     
             ellipses = FALSE,    
             method = "spearman",
             pch = 21,           
             lm = FALSE,         
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,         
             hist.col = 4,       
             stars = TRUE,       
             ci = TRUE)

#Examine linearity in more detail
scatter.smooth(x = heart_data$ejection_fraction,
               y = heart_data$DEATH_EVENT,
               xlab = "Ejection fraction",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Ejection fraction")
scatter.smooth(x = heart_data$anaemia,
               y = heart_data$DEATH_EVENT,
               xlab = "Anaemia",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Anaemia")
scatter.smooth(x = heart_data$cre_pho,
               y = heart_data$DEATH_EVENT,
               xlab = "Creatinine phosphokinase level",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Creatinine phosphokinase")
scatter.smooth(x = heart_data$diabetes,
               y = heart_data$DEATH_EVENT,
               xlab = "Diabetes",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Diabetes")
scatter.smooth(x = heart_data$high_blood_pressure,
               y = heart_data$DEATH_EVENT,
               xlab = "High BP",
               ylab = "Occurence of Death", main = "Correlation of Death ~ High BP")
scatter.smooth(x = heart_data$platelets,
               y = heart_data$DEATH_EVENT,
               xlab = "Platelets",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Platelets")
scatter.smooth(x = heart_data$serum_sodium,
               y = heart_data$DEATH_EVENT,
               xlab = "Serum sodium level",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Serum sodium")
scatter.smooth(x = heart_data$sex,
               y = heart_data$DEATH_EVENT,
               xlab = "Sex of person",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Sex")
scatter.smooth(x = heart_data$smoking,
               y = heart_data$DEATH_EVENT,
               xlab = "Smoking",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Smoking")


#examining other variables with DEATH_EVENT dependent variable
paste("Correlation for Death event and Age: ", cor(heart_data$age , heart_data$DEATH_EVENT))
paste("Correlation for Death event and Ejection Fraction: ", cor(heart_data$ejection_fraction, heart_data$DEATH_EVENT))
paste("Correlation for Death event and Serum Creatinine: ", cor(heart_data$serum_cre , heart_data$DEATH_EVENT))
paste("Correlation for Death event and anaemia: ", cor(heart_data$anaemia, heart_data$DEATH_EVENT))
paste("Correlation for Death event and creatinine_phosphokinase: ", cor(heart_data$cre_pho, heart_data$DEATH_EVENT))
paste("Correlation for Death event and diabetes: ", cor(heart_data$diabetes, heart_data$DEATH_EVENT))
paste("Correlation for Death event and high_blood_pressure: ", cor(heart_data$high_blood_pressure, heart_data$DEATH_EVENT))
paste("Correlation for Death event and platelets: ", cor(heart_data$platelets, heart_data$DEATH_EVENT))
paste("Correlation for Death event and serum_sodium: ", cor(heart_data$serum_sodium, heart_data$DEATH_EVENT))
paste("Correlation for Death event and sex: ", cor(heart_data$sex, heart_data$DEATH_EVENT))
paste("Correlation for Death event and smoking: ", cor(heart_data$smoking, heart_data$DEATH_EVENT))
paste("Correlation for Death event and time: ", cor(heart_data$time, heart_data$DEATH_EVENT))

# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
#hence removing the variables with low correlation

#Taking independent variables Age and Serum creatinine which has more correlation with Death_Event
scatter.smooth(x = heart_data$age,
               y = heart_data$DEATH_EVENT,
               xlab = "Age of person",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Age")
scatter.smooth(x = heart_data$serum_cre,
               y = heart_data$DEATH_EVENT,
               xlab = "Serum Creatinine level",
               ylab = "Occurence of Death", main = "Correlation of Death ~ Serum creatinine")




#Checking for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(heart_data)

boxplot(heart_data$DEATH_EVENT,
        main = "Death event",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$DEATH_EVENT)$out)) # box plot for DEATH EVENT

boxplot(heart_data$age,
        main = "Age",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$age)$out)) # box plot for Age

boxplot(heart_data$cre_pho,
        main = "Creatinine phosphokinase",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$cre_pho)$out)) # box plot for Creatinine phosphokinase

boxplot(heart_data$diabetes,
        main = "Diabetes",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$diabetes)$out)) # box plot for Diabetes

boxplot(heart_data$ejection_fraction,
        main = "ejection fraction",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$ejection_fraction)$out)) # box plot for Ejection Fraction

boxplot(heart_data$high_blood_pressure,
        main = "high_blood_pressure",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$high_blood_pressure)$out)) # box plot for High blood pressure

boxplot(heart_data$platelets,
        main = "platelets",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$platelets)$out)) # box plot for Platelets

boxplot(heart_data$serum_creatinine,
        main = "serum_creatinine",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$serum_creatinine)$out)) # box plot for Serum creatinine

boxplot(heart_data$serum_sodium,
        main = "serum_sodium",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$serum_sodium)$out)) # box plot for Serum sodium

boxplot(heart_data$sex,
        main = "sex",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$sex)$out)) # box plot for Sex

boxplot(heart_data$smoking,
        main = "Smoking",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$smoking)$out)) # box plot for Smoking

boxplot(heart_data$time,
        main = "Time",
        sub = paste("Outlier rows: ",
                    boxplot.stats(heart_data$time)$out)) # box plot for Time

#From the above cre_pho, ejection fraction, platelets, serum sodium contains outliers 
#whereas other variables does not 

detach(heart_data)
par(opar)

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heart_data$cre_pho)$out # outlier values.
paste("Creatinine phosphokinase outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(heart_data$ejection_fraction)$out # outlier values.
paste("Ejection fraction outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(heart_data$platelets)$out # outlier values.
paste("Platelets outliers: ", paste(outlier_values, collapse=", "))

outlier_values <- boxplot.stats(heart_data$serum_sodium)$out # outlier values.
paste("Serum sodium outliers: ", paste(outlier_values, collapse=", "))


# Remove creatinine phosphokinase outliers
heart_data <- subset(heart_data,
                 heart_data$cre_pho != 7861
                 & heart_data$cre_pho != 2656
                 & heart_data$cre_pho != 1380
                 & heart_data$cre_pho != 3964
                 & heart_data$cre_pho != 7702
                 & heart_data$cre_pho != 5882
                 & heart_data$cre_pho != 5209)

# Remove ejection fraction outliers
heart_data <- subset(heart_data,
                     heart_data$ejection_fraction != 80
                     & heart_data$ejection_fraction != 70)


# Remove platelet outliers
heart_data <- subset(heart_data,
                     heart_data$platelets != 454000
                     & heart_data$platelets != 47000
                     & heart_data$platelets != 451000
                     & heart_data$platelets != 461000
                     & heart_data$platelets != 497000
                     & heart_data$platelets != 621000
                     & heart_data$platelets != 850000)


# Remove serum sodium outliers
heart_data <- subset(heart_data,
                     heart_data$serum_sodium != 116
                     & heart_data$serum_sodium != 121
                     & heart_data$serum_sodium != 124
                     & heart_data$serum_sodium != 113)

# Re-run the box-plots to verify that outliers have now gone.

# To Check for normality
# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,4))
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

plot(density(heart_data$age),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$age), 2)))
# fill the area under the plot
polygon(density(heart_data$age), col = "red")

plot(density(heart_data$anaemia),
     main = "Density plot : Anaemia",
     ylab = "Frequency", xlab = "Anaemia",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$anaemia), 2)))
# fill the area under the plot
polygon(density(heart_data$anaemia), col = "red")

plot(density(heart_data$cre_pho),
     main = "Density plot : Creatinine Phospho",
     ylab = "Frequency", xlab = "Creatinine phospho",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$cre_pho), 2)))
# fill the area under the plot
polygon(density(heart_data$cre_pho), col = "red")

plot(density(heart_data$diabetes),
     main = "Density plot : Diabetes",
     ylab = "Frequency", xlab = "Diabetes",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$diabetes), 2)))
# fill the area under the plot
polygon(density(heart_data$diabetes), col = "red")

plot(density(heart_data$ejection_fraction),
     main = "Density plot : Ejection fraction",
     ylab = "Frequency", xlab = "Ejection fraction",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$ejection_fraction), 2)))
# fill the area under the plot
polygon(density(heart_data$ejection_fraction), col = "red")

plot(density(heart_data$high_blood_pressure),
     main = "Density plot : High blood pressure",
     ylab = "Frequency", xlab = "High blood pressure",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$high_blood_pressure), 2)))
# fill the area under the plot
polygon(density(heart_data$high_blood_pressure), col = "red")

plot(density(heart_data$platelets),
     main = "Density plot : Platelets",
     ylab = "Frequency", xlab = "Platelets",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$platelets), 2)))
# fill the area under the plot
polygon(density(heart_data$platelets), col = "red")

plot(density(heart_data$serum_cre),
     main = "Density plot : Serum creatinine",
     ylab = "Frequency", xlab = "Serum creatinine",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$serum_cre), 2)))
# fill the area under the plot
polygon(density(heart_data$serum_cre), col = "red")

plot(density(heart_data$serum_sodium),
     main = "Density plot : Serum sodium",
     ylab = "Frequency", xlab = "Serum sodium",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$serum_sodium), 2)))
# fill the area under the plot
polygon(density(heart_data$serum_sodium), col = "red")

plot(density(heart_data$sex),
     main = "Density plot : Sex",
     ylab = "Frequency", xlab = "Sex",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$sex), 2)))
# fill the area under the plot
polygon(density(heart_data$sex), col = "red")

plot(density(heart_data$smoking),
     main = "Density plot : Smoking",
     ylab = "Frequency", xlab = "Smoking",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$smoking), 2)))
# fill the area under the plot
polygon(density(heart_data$smoking), col = "red")

plot(density(heart_data$time),
     main = "Density plot : Time",
     ylab = "Frequency", xlab = "Time",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$time), 2)))
# fill the area under the plot
polygon(density(heart_data$time), col = "red")

plot(density(heart_data$DEATH_EVENT),
     main = "Density plot : Death event",
     ylab = "Frequency", xlab = "Death event",
     sub = paste("Skewness : ", round(e1071::skewness(heart_data$DEATH_EVENT), 2)))
# fill the area under the plot
polygon(density(heart_data$DEATH_EVENT), col = "red")


# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.

paste("Skewness for Age : ", round(e1071::skewness(heart_data$age), 2))   #approx symetric
paste("Skewness for Anaemia : ", round(e1071::skewness(heart_data$anaemia), 2))        #approx symetric
paste("Skewness for Creatinine phospho : ", round(e1071::skewness(heart_data$cre_pho), 2))      #highly skewed
paste("Skewness for Diabetes : ", round(e1071::skewness(heart_data$diabetes), 2))      #approx symetric
paste("Skewness for Ejection fraction : ", round(e1071::skewness(heart_data$ejection_fraction), 2))    #moderately skewed
paste("Skewness for High BP : ", round(e1071::skewness(heart_data$high_blood_pressure), 2))     #moderately skewed
paste("Skewness for Platelets : ", round(e1071::skewness(heart_data$platelets), 2))     #highly skewed
paste("Skewness for Serum creatinine : ", round(e1071::skewness(heart_data$serum_cre), 2))     #highly skewed
paste("Skewness for Serum sodium : ", round(e1071::skewness(heart_data$serum_sodium), 2))     #highly skewed
paste("Skewness for Sex : ", round(e1071::skewness(heart_data$sex), 2))         #moderately skewed
paste("Skewness for Smoking : ", round(e1071::skewness(heart_data$smoking), 2))     #moderately skewed
paste("Skewness for Time : ", round(e1071::skewness(heart_data$time), 2))         #approx symetric
paste("Skewness for Death Event : ", round(e1071::skewness(heart_data$DEATH_EVENT), 2))     #moderately skewed


#Creatinine_phosphokinase, platelets, serum_creatinine, serum_sodium are highly skewed
hist(heart_data$cre_pho)
hist(heart_data$platelets)
hist(heart_data$serum_cre)
hist(heart_data$serum_sodium)

#If p-value < 0.05 then variable
# is not normally distributed

#calculating p-value
shapiro.test(heart_data$cre_pho)
shapiro.test(heart_data$platelets)
shapiro.test(heart_data$serum_cre)
shapiro.test(heart_data$serum_sodium)

#creatinine_phosphokinase is not normally distributed
#platelets is not normally distributed
#serum_creatinine is not normally distributed
#serum_sodium is not normally distributed

#Checking normality for other variables
shapiro.test(heart_data$age)
shapiro.test(heart_data$anaemia)
shapiro.test(heart_data$diabetes)
shapiro.test(heart_data$ejection_fraction)
shapiro.test(heart_data$high_blood_pressure)
shapiro.test(heart_data$sex)
shapiro.test(heart_data$smoking)

# If p-value < 0.05 then variable
# is not normally distributed

#Need to transform the variables
#Age and Serum creatinine which has more correlation with Death_Event

library(MASS)

# find optimal lambda for box-cox transform
Box = boxcox(heart_data$age ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)

cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),]
cox_smallest_y[1,]
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_age = (heart_data$age ^ lambda - 1)/lambda
hist(transformed_age)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_age)


#converting serum_creatinine
Box = boxcox(heart_data$serum_cre ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)

cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),]
cox_smallest_y[1,]
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda

transformed_serum = (heart_data$serum_cre ^ lambda - 1)/lambda
hist(transformed_serum)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_serum)

# Converting data in data frame
heart_data$transformed_age <- transformed_age
heart_data$transformed_serum <- transformed_serum

#Building model

attach(heart_data)

set.seed(1)
no_rows_data <- nrow(heart_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- heart_data[sample, ]
testing_data <- heart_data[-sample, ]

unmodified_model <- lm(DEATH_EVENT ~ age + anaemia + cre_pho + diabetes	+ ejection_fraction 
                       +	high_blood_pressure +	platelets +	serum_cre +	serum_sodium 
                       + sex +	smoking +	time, data=training_data)
summary(unmodified_model)

# Examine which combination of independent variables best fits the model
# install.packages("leaps")
library(leaps)

# Regression Subset Selection
MLR_subset <-regsubsets(DEATH_EVENT ~ age + anaemia + cre_pho + diabetes	+ ejection_fraction 
                                  +	high_blood_pressure +	platelets +	serum_cre +	serum_sodium 
                                  + sex +	smoking +	time, data=training_data, nbest=6)
plot(MLR_subset, scale="adjr2")

# Smallest AIC is best
stepAIC(unmodified_model, direction="backward")

#Age and Serum Creatinine has smallestAIC which is best

modified_model <- lm(DEATH_EVENT ~ transformed_age + anaemia + cre_pho + diabetes	+ ejection_fraction 
                     +	high_blood_pressure +	platelets +	transformed_serum +	serum_sodium 
                     + sex +	smoking +	time, data=training_data)
summary(modified_model)

# We can examine best fitting model using leaps
MLR_subset_modified <-regsubsets(DEATH_EVENT ~ transformed_age + anaemia + cre_pho + diabetes	+ ejection_fraction 
                                 +	high_blood_pressure +	platelets +	transformed_serum +	serum_sodium 
                                 + sex +	smoking +	time, data=training_data, nbest=6)
plot(MLR_subset_modified, scale="adjr2")

# It seems that the untransformed model
# could be more accurate than the transformed model 

confint(unmodified_model)

confint(modified_model)


# Examine outliers for the unmodified model
library(car)
qqPlot(unmodified_model, 
       labels=row.names(training_data$name), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot for unmodified model")

studentized_fit <- rstudent(unmodified_model)
hist(studentized_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

# Rug plot is used to show the distribution of the data
rug(jitter(studentized_fit), col="brown")
curve(dnorm(x, mean=mean(studentized_fit), sd=sd(studentized_fit)), add=TRUE, col="blue", lwd=2)
lines(density(studentized_fit)$x, density(studentized_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(unmodified_model)
#before deleting the outlier
#model linearity will be examined
crPlots(unmodified_model)

training_data <- subset(training_data,
                        training_data$name!= "Platelets")

#influential observations
#cook's D-value  > 4/(n - k -1)
#where n = sample size, and k
#is the number of predictor variables

cutoff <- 4/
  (nrow(training_data) - length(unmodified_model$coefficients) - 1)
plot(unmodified_model, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

influencePlot(unmodified_model, 
              main = "Influence plot for unmodified model",
              sub = "circle size is proportional to the cook's distance" )

ncvTest(unmodified_model)

#model transform?
spreadLevelPlot(unmodified_model)

sqrt_death <- sqrt(training_data$DEATH_EVENT)
unmodified_model_sqrt <- lm(sqrt_death ~ age + anaemia + cre_pho + diabetes	+ ejection_fraction 
                            +	high_blood_pressure +	platelets +	serum_cre +	serum_sodium 
                            + sex +	smoking +	time)

summary(unmodified_model_sqrt)
summary(unmodified_model)


#global validation of the model
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(unmodified_model)
summary(gvmodel)

predicted_death <- predict(unmodified_model, testing_data)
actual_predictions <- data.frame(cbind(actuals = testing_data$DEATH_EVENT, 
                                       predicted = predicted_death))
head(actual_predictions)

cor_accuracy <- cor(actual_predictions)
cor_accuracy

#ask the model a question
summary(heart_data)

question <- data.frame(age = c(50),
                       anaemia = c(1),
                       serum_cre = c(125),
                       platelets = c(47000),
                       time = c(10))

predicted_death <- predict(unmodified_model, question)
predicted_death



#Model forecasting


predicted_data <- data.frame(age = 75, anaemia = c("1", "0"),serum_cre = 125, smoking = c("Yes", "No"))
predicted <- predict(gvmodel, predicted_data, type="response")
predicted

predicted_data <- data.frame(age = c(50, 60), anaemia = c("1", "0"), serum_cre = 607, smoking = c("Yes", "No"))
predicted <- predict(gvmodel, predicted_data, type="response")
predicted



















