#### Import Dataset ####
test = read.csv("test.csv", header = TRUE)
test <- test[c(-1)]

train = read.csv("train.csv", header = TRUE)
weather = read.csv("weather.csv", header = TRUE)

#### Merge Data #####
weather1.train <- weather[ which(weather$Station == '1'), ]
merged.train <- merge(weather1.train, train, by = "Date")

weather1.test <- weather[ which(weather$Station == '1'), ]
merged.test <- merge(weather1.test, test, by = "Date")

#### Data Frame #####
df.merged.train <- merged.train[c(1,2,3,4,5,7,8,17,18,19,20,21,22,23,24,27,29,30,32,33)]

df.merged.test <- merged.test[c(1,2,3,4,5,7,8,17,18,19,20,21,22,23,24,27,29,30)]

###### Convert type ######
df.merged.train$Date <- as.Date(df.merged.train$Date)
df.merged.train$Tavg <- as.numeric(as.character(df.merged.train$Tavg))
df.merged.train$WetBulb <- as.numeric(as.character(df.merged.train$WetBulb))
df.merged.train$PrecipTotal <- as.numeric(as.character(df.merged.train$PrecipTotal))
df.merged.train$StnPressure <- as.numeric(as.character(df.merged.train$StnPressure))
df.merged.train$SeaLevel <- as.numeric(as.character(df.merged.train$SeaLevel))
df.merged.train$AvgSpeed <- as.numeric(as.character(df.merged.train$AvgSpeed))

df.merged.test$Date <- as.Date(df.merged.test$Date)
df.merged.test$Tavg <- as.numeric(as.character(df.merged.test$Tavg))
df.merged.test$WetBulb <- as.numeric(as.character(df.merged.test$WetBulb))
df.merged.test$PrecipTotal <- as.numeric(as.character(df.merged.test$PrecipTotal))
df.merged.test$StnPressure <- as.numeric(as.character(df.merged.test$StnPressure))
df.merged.test$SeaLevel <- as.numeric(as.character(df.merged.test$SeaLevel))
df.merged.test$AvgSpeed <- as.numeric(as.character(df.merged.test$AvgSpeed))

#### Replace missing value ####
##install.packages("zoo") ### 
library(zoo)

df.merged.train$WetBulb <- na.locf(df.merged.train$WetBulb)
df.merged.train$PrecipTotal[is.na(df.merged.train$PrecipTotal)] <- 0.001
df.merged.train$StnPressure <- na.locf(df.merged.train$StnPressure)

df.merged.test$PrecipTotal[is.na(df.merged.train$PrecipTotal)] <- 0.001

############### Adding month ###################
df.merged.train$months <- format(df.merged.train$Date, "%m")

df.merged.test$months <- format(df.merged.test$Date, "%m")

###################

###################
train <- df.merged.train[c(-1,-2,-14)]

library(gbm)

train$months <- as.factor(train$months)


model <- gbm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint + WetBulb  + PrecipTotal + StnPressure
             +  months +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed + Species + Latitude + Longitude
             , data=train, n.trees = 5000)

summary(model)

##################################
test <- df.merged.test[c(-1,-2,-14)]
test$months <- as.factor(test$months)

test$Species[test$Species=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"

probWnv <- predict(model, newdata=test, type="response", n.trees = 5000)


test = read.csv("test.csv", header = TRUE)
submissionFile<-cbind(test$Id,probWnv)
colnames(submissionFile)<-c("Id","WnvPresent")
write.csv(submissionFile,"result.csv",row.names=FALSE,quote=FALSE)
