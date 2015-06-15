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
train <- df.merged.train[c(-1,-2)]


train$months <- as.factor(train$months)
train$PrecipTotal <- log(train$PrecipTotal + 001)

#model1 0.70069
#model <- glm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint + WetBulb  + PrecipTotal + StnPressure
#             +  months +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed + Species + Latitude + Longitude
#             , data=train, family=binomial)
#model <- stepAIC(model, direction="both")
#summary(model)

#model2 -
train$Species1 <- (train$Species=="CULEX PIPIENS")*1
train$Species2 <- (train$Species=="CULEX PIPIENS/RESTUANS")*1
train$Species3 <- (train$Species=="CULEX RESTUANS")*1

train$month7 <- (train$months=="07")*1
train$month8 <- (train$months=="08")*1
train$month9 <- (train$months=="09")*1

#Your submission scored 0.70115
model.1 <- glm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint + WetBulb  + Species1 + Species2 + Species3  
               +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed  + Latitude + Longitude + month7
               + month8 + month9
               , data=train, family=binomial)

#Your submission scored 0.70212
model.2 <- stepAIC(model, direction="both")
summary(model.2)

#Multi-co
library(car)
## Detecting multi-collinearity by using VIF, Kaggle Score 0.72
sqrt(vif(model.2))

model.3 <- glm(WnvPresent ~ Tavg + DewPoint + Species1 + Species2 + Species3   
               +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed   + Longitude + month7
               + month8 + month9
               , data=train, family=binomial)

model.3 <- glm(WnvPresent ~ Tavg + DewPoint + Species 
               +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed   + Longitude + months
               , data=train, family=binomial)

model.1 <- glm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint + WetBulb  + Species +  
                 +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed  + Latitude + Longitude + months
               , data=train, family=binomial)
vif(model.1)

install.packages("Deducer")
library(Deducer)
rocplot(model)
library(pROC)
roc(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint + WetBulb + Species1 + Species2 + Species3  
    +  SeaLevel + ResultSpeed + ResultDir + AvgSpeed  + Latitude + Longitude + month7
    + month8 + month9, train, smooth=TRUE)
#Your submission scored 0.67539, no wetbulb and species
#model <- glm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint    
#            +  SeaLevel + ResultSpeed  + AvgSpeed  + Latitude + Longitude + month7
#            + month8 + month9
#            , data=train, family=binomial)

model <- stepAIC(model, direction="both")
library(faraway)
halfnorm(residuals(model))
model <- glm(WnvPresent ~ Tmax + Tmin + Tavg + DewPoint  +  
               +  SeaLevel + ResultSpeed  + AvgSpeed   + Longitude + month7
             + month8 + month9
             , data=train, family=binomial)
summary(model.1)
summary(model.2)
summary(model.3)

##################################
test <- df.merged.test[c(-1,-2)]
test$months <- as.factor(test$months)

test$Tmax <- na.locf(test$Tmax)
test$Tmin <- na.locf(test$Tmin)
test$Tavg <- na.locf(test$Tavg)
test$DewPoint <- na.locf(test$DewPoint)
test$WetBulb <- na.locf(test$WetBulb)
test$PrecipTotal <- na.locf(test$PrecipTotal)
test$StnPressure <- na.locf(test$StnPressure)
test$ResultSpeed <- na.locf(test$ResultSpeed)
test$ResultDir <- na.locf(test$ResultDir)
test$AvgSpeed <- na.locf(test$AvgSpeed)


test$PrecipTotal <- log(test$PrecipTotal + .001)
test$Species[test$Species=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"

test$Species1 <- (test$Species=="CULEX PIPIENS")*1
test$Species2 <- (test$Species=="CULEX PIPIENS/RESTUANS")*1
test$Species3 <- (test$Species=="CULEX RESTUANS")*1

test$month7 <- (test$months=="07")*1
test$month8 <- (test$months=="08")*1
test$month9 <- (test$months=="09")*1

probWnv <- predict(model.3, newdata=test, type="response")



###### Making Sumission File ####################

test = read.csv("test.csv", header = TRUE)
submissionFile<-cbind(test$Id,probWnv)
colnames(submissionFile)<-c("Id","WnvPresent")
write.csv(submissionFile,"result2.csv",row.names=FALSE,quote=FALSE)
