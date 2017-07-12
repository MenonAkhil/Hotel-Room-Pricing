
# Project Title: Hotel Room Price Analysis in India
# NAME: Akhil Menon
# EMAIL: menonakhil90@gmail.com
# COLLEGE: Sri Venkateswara College of Engineering


# Reading csv file onto a data frame
hotel<-read.csv(paste("MergedHotelPricingData.csv", sep=""))
View(hotel)

# Summary Stats of data frame containing Dataset
summary(hotel)

# Boxplot visualizations of Important Variables
boxplot(hotel$StarRating)

boxplot(hotel$RoomRent)

boxplot(hotel$HasSwimmingPool)

boxplot(hotel$HotelCapacity)


# ScatterPlot Visualizations
attach(hotel)
plot(RoomRent, StarRating, main="Scatterplot ", 
     xlab="Rs ", ylab="Count ", pch=19)
abline(lm(StarRating~RoomRent), col="red") # regression line (y~x) 
lines(lowess(RoomRent,StarRating), col="blue") # lowess line (x,y)

plot(RoomRent, HasSwimmingPool, main="Scatterplot ", 
     xlab="Rs ", ylab="Yes/No ", pch=19)
abline(lm(HasSwimmingPool~RoomRent), col="red") # regression line (y~x) 
lines(lowess(RoomRent,HasSwimmingPool), col="blue") # lowess line (x,y)


plot(RoomRent, HotelCapacity, main="Scatterplot ", 
     xlab="Rs ", ylab="Count ", pch=19)
abline(lm(HotelCapacity~RoomRent), col="red") # regression line (y~x) 
lines(lowess(RoomRent,HotelCapacity), col="blue") # lowess line (x,y)



library(corrgram)

corrgram(hotel)

# Creation of Covariance Matrix
library(Hmisc)
colhotel <- c("StarRating","HasSwimmingPool","HotelCapacity")
corMatrix <- rcorr(as.matrix(hotel[,colhotel]))
corMatrix


library(Hmisc)
library(car)
library(corrgram)

# Creation of Corrgram

corrgram(hotel[,colhotel], order=TRUE,
         main="Hotel Rent dependencies",
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)


# FACTORS INFLUENCING HOTEL PRICES

# INFLUENCE OF IsWeekend on RoomRent

week <- aggregate(RoomRent ~ IsWeekend, data = hotel, mean)
week

boxplot(RoomRent~IsWeekend, data = hotel, main = "Influence of IsWeekend on RoomRent", 
        ylab = "Weekday(0) Weekend(1)", xlab = "Rupees", col = c("Blue","Red"), horizontal = TRUE)

# INFLUENCE of different dates of the year on RoomRent

dts = aggregate(RoomRent ~ Date, data = hotel,mean)
dts

scatterplot(dts$Date,dts$RoomRent, main="Influence of Different Dates on RoomRent", xlab="Date", ylab = "Rupees")

# From above plot we can infer that the Highest RoomRent on New Year's eve (31st Dec 2016); Rent above 6000 Rupees

# INFLUENCE of IsMetroCity on RoomRent

metro = aggregate(RoomRent ~ IsMetroCity, data = hotel, mean)
metro

boxplot(RoomRent~IsMetroCity,data=hotel, main="Influence of IsMetroCity on RoomRent", ylab="Metro city(1) Non-Metro(0)", xlab="Rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)


# INFLUENCE of IsTouristDestination on RoomRent
TouDes = aggregate(RoomRent ~ IsTouristDestination, data = hotel, mean)
TouDes

boxplot(RoomRent~IsTouristDestination,data=hotel, main="Influence of IsTouristDestination on RoomRent", ylab=" IsTouristDestination(1) or NotTouristDestination(2)", xlab="Rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#It is clearly visualized that the RoomRent is more at Tourist destinations than Non Tourist Destinations

#INFLUENCE of FreeWifi on RoomRent
fw = aggregate(RoomRent ~ FreeWifi, data = hotel, mean)
fw


boxplot(RoomRent~FreeWifi,data=hotel, main="Influence of FreeWifi on RoomRent", ylab="Free Wifi available(1)", xlab="Rupees ", col=c("red","blue","green","yellow"),horizontal=TRUE)

#INFLUENCE of FreeBreakfast on RoomRent

fb = aggregate(RoomRent ~ FreeBreakfast, data =hotel, mean)
fb1  = aggregate(RoomRent ~ FreeBreakfast, data =hotel, mean)
fb
fb1

boxplot(RoomRent~FreeBreakfast,data=hotel, main="Influence of FreeBreakfast on RoomRent", ylab="Free Breakfast available(1)", xlab="Rupees ", col=c("green","yellow"),horizontal=TRUE)

# INFLUENCE of Airport Distance on RoomRent
scatterplot(hotel$Airport,hotel$RoomRent, main="Influence of Airport distance on RoomRent", xlab="Airport distance(km)", ylab="Rupees ",cex=1.1)

# t-tests

# Avg. RoomRents for Hotels with and without Swimming Pools

t.test(RoomRent~HasSwimmingPool,data = hotel, alternative="less")

# p-value < 0.05; Rejection of Null Hypothesis of equal mean

# Avg RoomRent based on star-rating
t.test(hotel$RoomRent,hotel$StarRating)

# p-value < 0.05; Rejection of Null Hypothesis of equal mean

# Avg RoomRent in Metro and other cities
t.test(RoomRent~IsMetroCity, data = hotel, alternative="less")
 
# p-value > 0.05; Fail to Reject Null Hypothesis of Equal mean

# Avg RoomRent based on Hotels that provide and don't Provide free breakfast

t.test(RoomRent~FreeBreakfast, data = hotel, alternative="less")

# p-value > 0.05; Fail to Reject Null Hypothesis of equal mean

#Generating a multiple linear regression model for RoomRent

# Model-1 (Using Variables -> RoomRent,HasSwimmingPool,HotelCapacity)
fita<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity-1, data = hotel)
summary(fita)

# Model-1 Coefficients
fita$coefficients

# Model-2 (Using Variables -> RoomRent,HasSwimmingPool, HotelCapacity, Isweekend, IsTouristDestination)
fitb<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsWeekend+IsTouristDestination-1, data = hotel)
summary(fitb)

# Model-2 Coefficients
fitb$coefficients


# Model-3 (Using Variables -> RoomRent,HasSwimmingPool,HotelCapacity,Airport)
fitc<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport-1, data = hotel)
summary(fitc)

# Model-3 Coefficients
fitc$coefficients

