setwd("/media/sf_AIT-580")#Set working directory to access files and store data.
cardata <- read.csv("Project_data.csv") #Upload the data
par(mfrow=c(2,2)) #Helps to display multiple plots on one screen.
cor(cardata) #Shows correlation data for variables.
pairs(cardata)#Scatterplots showing variable relations
lm_cardata = lm(ATV.Sales ~ Year, data=cardata) #Create the linear regression
summary(lm_cardata) #Review results for the linear regression
plot(lm_cardata) #Displays plots for regression evaluation metrics
plot(ATV.Sales ~ Year, data=cardata) #Scatterplot of ATV Sales across Years
abline(lm_cardata)#Displays simple Linear regression projection on scatterplot

model1 <- lm(ATV.Sales ~ Year * CPI, data=cardata)#Adding variable Consumer Price Index (CPI) to the Regression
summary(model1)#Summary shows better R-squared value, with still acceptable p-value
model2 <- lm(ATV.Sales ~ Year * CPI * Oil.Price, data=cardata)#Adding Oil Price variable
summary(model2)#Improves R-squared value slightly but significantly negatively effects p-value

plot(ATV.Sales ~ Year, data=cardata, main="Multivariable Regression of ATV Sales Over Time",
     xlab="Year (1999-2017 AD)", ylab="ATV Sales",
     xlim=c(1999, 2017), ylim=c(0, 600000))
lines(lowess(cardata$ATV.Sales ~ cardata$Year, f=0.4), col=11)

