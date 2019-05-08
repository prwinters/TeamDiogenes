rm(list=ls())
set.seed(123)
setwd("C:/M2/datasets")

#read data into dataframe
df <- read.csv("cleaned data.csv")

#Total sales and year
sales <- df$Total/1000
cor(sales,df$X)
plot(df$X,sales,xlim=c(1999,2017),ylim=c(0,600),
     main = 'ATV Sales by Year',pch = 16, cex = 1.3,
     xlab='Year',ylab = 'Sales(in thousand)',type = 'b',col = 'blue')
lr <- lm(sales~df$X)
abline(lr)
par(mfrow=c(2,2))
plot(lr)
summary(lr)


#Pie chart
value_1 <- c(sum(df$Toyota),sum(df$Ford),sum(df$Honda),
             sum(df$GM),sum(df$Nissan),sum(df$All.other))
labels <- c('Toyota total', 'Ford total', 'Honda total',
            'GM total', 'Nissan total', 'All.other total')
piepercent <- round(100* value_1/sum(value_1),1)
pie(value_1, labels = piepercent, main = 'ATV Sales by Manufacturer',
    col = rainbow(length(value_1)))
legend('topright',c('Toyota sum','Ford sum','Honda sum','GM sum','Nissan sum',
                    'All.other sum'),cex = 0.8,fill = rainbow(length(value_1)))

#Toyota sales and total
plot(df$Toyota,df$Total,xlim=c(5000,35000),ylim=c(10000,60000),
     main = 'ATV Sales',pch = 16, cex = 1.3,
     xlab='Sales of Toyota',ylab = 'Sales',type = 'b',col = 'blue')
newm <- lm(df$Total~df$Toyota)
abline(newm)
par(mfrow=c(2,2))
summary(newm)
