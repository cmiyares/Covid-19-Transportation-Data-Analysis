setwd("C:/Users/chris/Desktop/COVIDPROJ/")

library(readxl)
coviddata = read_excel("C:/Users/chris/Desktop/COVIDPROJ/coviddata.xlsx")
attach(coviddata)
names(coviddata)

confirmedRates = (Confirmed/People_Tested)*100
confirmedRates

fatalRate = (Deaths/Confirmed)*100
fatalRate

# data with data_confirmed
data_confirmed <- data.frame(State=coviddata$Province_State, confirmedRates=confirmedRates, fatalRate = Mortality_Rate)
data_confirmed

trip<-read.csv("Trips_by_Distance.csv")
trip

# delete the county observations.
tripdata<-trip[which(trip$Level=="State"),]
tail(tripdata)
head(tripdata)

#################################################################
# 1.pick up the trip data of Oct 
# 2.aggregate the transportation data by month
require('tidyverse')
trip_1<-as_tibble(tripdata)
trip_a<-trip_1 %>% filter(str_detect(tolower(Date), pattern = "2020/10")) %>% group_by(State.Postal.Code) %>% 
  summarise_at(vars(Number.of.Trips..1:Number.of.Trips...500),~sum(.))
trip_a

names(trip_a)[1] <- "State"

###### set State name of trip_a 

# Get state names of the data
State <- trip_a$State
State
# No "DC" in state.abb
state.abb=="DC"
# Delete the row of DC.
data_name<-trip_a[-8,]
data_name


state_f <- character(50)
for (i in 1:50) {
  state_f[i]<-state.name[which(state.abb==data_name$State[i])]
}
state_f



#data frame with full state name and "stay at home changed rate" in September.
data_2<-data.frame(State=state_f,data_name[2:11])
head(data_2)


data<- merge(data_confirmed,data_2,by="State")
head(data)
str(data)


sort(data$confirmedRates)
hist(data$confirmedRates)
# Grouping data by "stay at home changed rate" low to high
confirmed_rate_group<-ifelse(
  data$confirmedRates<=10,0,1)
#The top 10 out of 50 states as the high confirmed rates
confirmed_rate_group


group_data <- data.frame(data, confirmed_rate_group)
group_data


library(ggplot2)
library(mosaic)

qplot(fatalRate,Number.of.Trips..1,data=group_data)

qplot(fatalRate, Number.of.Trips..1, color=as.factor(confirmed_rate_group),data=group_data,geom=c("point","lm"))

log<- glm(confirmed_rate_group==1 ~ fatalRate+Number.of.Trips..1+Number.of.Trips.1.3 +
            Number.of.Trips.3.5+ Number.of.Trips.5.10+Number.of.Trips.10.25 +
            Number.of.Trips.25.50 + Number.of.Trips.50.100+Number.of.Trips.100.250+
            Number.of.Trips.250.500+Number.of.Trips...500, data=group_data,family = "binomial")

summary(log)

library(car)
library(MASS)
library(olsrr)

# both deriction stepwise
step<- step(log, direction="both")
summary(step)

# check	multicollinearity
vif(step)

outlierTest(step)
# diagnostic plots
par(mfrow = c(2, 2))
plot(step)








#Linear regression
# Logistic regression
lm2<- lm(confirmedRates ~Number.of.Trips..1+Number.of.Trips.3.5+Number.of.Trips.25.50+Number.of.Trips...500, data=group_data)
summary(lm2)
# diagnostic plots
lm3<- lm(confirmedRates ~Number.of.Trips..1+Number.of.Trips.3.5+Number.of.Trips...500, data=group_data)
summary(lm3)
#######################
lm4<- lm(confirmedRates ~log(Number.of.Trips..1)+log(Number.of.Trips.250.500), data=group_data)
summary(lm4)
##########################
lm5<- lm(confirmedRates ~fatality_rate+log(Number.of.Trips..1)+log(Number.of.Trips.250.500), data=group_data)
summary(lm5)
#########################
sort(group_data$fatalRate)
hist(group_data$fatalRate)
group_data$fr_group = as.numeric(group_data$fatality_rate>0.15)
#Create fatality groups
group_data$confirmed_rate
lm6<- lm(confirmed_rate ~fr_group+log(Number.of.Trips..1)+log(Number.of.Trips.250.500), data=group_data)
summary(lm6)
###########
lm7<- lm(confirmed_rate ~fr_group+log(Number.of.Trips.1.3)+log(Number.of.Trips.250.500), data=group_data)
summary(lm7)
################
lm8<- lm(confirmed_rate ~fr_group+log(Number.of.Trips.1.3)+log(Number.of.Trips.250.500)+fr_group*log(Number.of.Trips.1.3), data=group_data)
summary(lm8)



# Logistic regression
log2<- glm(confirmed_rate_group==1 ~fatality_rate+log(Number.of.Trips.250.500)+log(Number.of.Trips.10.25)+log(Number.of.Trips.250.500)*log(Number.of.Trips.10.25), data=group_data,family = "binomial")
summary(log2)
# diagnostic plots

plot(log2)