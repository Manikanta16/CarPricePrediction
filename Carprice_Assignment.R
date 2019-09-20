###ASSIGNMENT ON LINEAR REGRESSION
carprices <- read.csv("CarPrice_Assignment.csv")
str(carprices)

#no null values in the data-set
no_of_nulls <- sum(is.na(carprices))

summary(carprices$CarName)

##creating dummy variables
dummy1 <- data.frame(model.matrix(~doornumber,data = carprices))
View(dummy1)
carprices <- cbind(carprices[,-6],dummy1[2])

summary(carprices$fueltype)
dummy1 <- data.frame(model.matrix(~fueltype,data=carprices))
carprices <- cbind(carprices[,-4],dummy1[,2])

summary(factor(carprices$symboling))
dummy1 <- data.frame(model.matrix(~as.character(symboling),data = carprices))
carprices <- cbind(carprices[,-2],dummy1[,-1])

carprices[3:10,25]
colnames(carprices)[25] <- "FuelTypeGas"
colnames(carprices)[26] <- "symboling.-2"
colnames(carprices)[27] <- "symboling.0"
colnames(carprices)[28] <- "symboling.1"
colnames(carprices)[29] <- "symboling.2"
colnames(carprices)[30] <- "symboling.3"

summary(carprices$aspiration)
dummy1 <- data.frame(model.matrix(~aspiration,carprices))
carprices <- cbind(carprices[,-3],dummy1[,2])
colnames(carprices)[30]<-"aspirationTurbo"

summary(carprices$carbody)
dummy1 <- data.frame(model.matrix(~carbody,data = carprices))
carprices <- cbind(carprices[,-3],dummy1[,-1])

summary(carprices$drivewheel)
dummy1 <- data.frame(model.matrix(~drivewheel,data = carprices))
carprices <- cbind(carprices[,-3],dummy1[,-1])

summary(carprices$enginelocation)
dummy1 <- data.frame(model.matrix(~enginelocation,data = carprices))
carprices <- cbind(carprices[,-3],dummy1[,-1])
colnames(carprices)[34] <- "enginelocationrear"

summary(carprices$enginetype)
dummy1 <- data.frame(model.matrix(~enginetype,data = carprices))
carprices <- cbind(carprices[,-8],dummy1[,-1])

summary(carprices$cylindernumber)
dummy1 <- data.frame(model.matrix(~cylindernumber,data = carprices))
carprices <- cbind(carprices[,-8],dummy1[,-1])

summary(carprices$fuelsystem)
dummy1 <- data.frame(model.matrix(~fuelsystem,data = carprices))
carprices <- cbind(carprices[,-9],dummy1[,-1])

##separate the carname column to extract the manufacturer name 
library(reshape2)
dummy1 <- colsplit(carprices$CarName," ",c("Maker","Model"))
carprices <- cbind(carprices[,-2],dummy1[,1])
colnames(carprices)[50] <-"Maker"

#remove abiguity in the names of manufacturers 
summary(carprices$Maker)
carprices$Maker[carprices$Maker=="toyouta"] <- "toyota"
carprices$Maker[carprices$Maker=="vw" | carprices$Maker=="vokswagen"] <- "volkswagen"
carprices$Maker[carprices$Maker=="maxda"] <- "mazda"
carprices$Maker[carprices$Maker=="Nissan"]<- "nissan"
carprices$Maker[carprices$Maker=="porcshce"] <- "porsche"
dummy1 <- data.frame(model.matrix(~Maker,data = carprices))
dummy1 <- dummy1[,c(2:9,11:14,16,17,19:23,26,27)]
carprices <-  cbind(carprices[,-50],dummy1)




###-----------------------------###
#Derived Metrics
###-----------------------------###
U_turn_steering_angle = 60
carprices$turning_radius_mt <- ((carprices$wheelbase*10)/sin(U_turn_steering_angle*0.0174532925))/1000
### multiply by 10 since the values are in cm but wheelbase should be in mm
### 1 degree = 0.0174532925
### divide by 1000 to convert turning radius to mt from mm

#The difference should not be too high or too low
carprices$diff_of_city_hwy_mpg <- carprices$highwaympg - carprices$citympg

#This variable deries if there is a significant effect of weight on peakrmp
carprices$wt_rpm_ratio <- carprices$curbweight/carprices$peakrpm

#significance of horsepower on peakrmp
carprices$hp_rmp_ratio <- carprices$horsepower/carprices$peakrpm

###--------------------------------------------------------------###
#Exploratory data analysis
###--------------------------------------------------------------###
carprices_eda <- read.csv("CarPrice_Assignment.csv")

U_turn_steering_angle = 60
carprices_eda$turning_radius_mt <- ((carprices$wheelbase*10)/sin(U_turn_steering_angle*0.0174532925))/1000
carprices_eda$diff_of_city_hwy_mpg <- carprices$highwaympg - carprices$citympg
carprices_eda$wt_rpm_ratio <- carprices$curbweight/carprices$peakrpm
carprices_eda$hp_rmp_ratio <- carprices$horsepower/carprices$peakrpm

temp <- colsplit(string = carprices_eda$CarName,pattern = " ", names = c("CarName","Model"))
carprices_eda <- cbind(carprices_eda[,-3],temp[,])
carprices_eda$CarName[carprices_eda$CarName=="toyouta"] <- "toyota"
carprices_eda$CarName[carprices_eda$CarName=="vw"] <- "volkswagen"
carprices_eda$CarName[carprices_eda$CarName=="vokswagen"] <- "volkswagen"
carprices_eda$CarName[carprices_eda$CarName=="maxda"] <- "mazda"
carprices_eda$CarName[carprices_eda$CarName=="Nissan"]<- "nissan"
carprices_eda$CarName[carprices_eda$CarName=="porcshce"] <- "porsche"

summary(factor(carprices_eda$CarName))

#mean price for toyota(highest number of cars) 
toyota_mean <- mean(carprices_eda$price[carprices_eda$CarName=="toyota"])
#difference between city and highway milage of cars
ct_vs_hw_mpg <- median(carprices_eda$diff_of_city_hwy_mpg)
mean(carprices_eda$price[carprices_eda$diff_of_city_hwy_mpg==6])
#average price for cars that have meadian diffrence between city and highway mileage-12127

mean(carprices_eda$turning_radius_mt)
#turn radius is 1.1 meters, most of the cars must be compact

ggplot(data = carprices_eda,aes(x=horsepower,y=price))+geom_line()
#horsepower and price are mostly directly proportional

ggplot(data = carprices_eda,aes(x=peakrpm,y=curbweight))+geom_line()
#afterall weight doesn't have anything to do with RPM

ggplot(data = carprices_eda,aes(x=car_ID,y=curbweight))+geom_point(aes(color=CarName))
#isuzu has maximum weight and it makes sense because they deal mostly in trucks & SUV



colnames(carprices_eda)



###----------------------------###
#seperation of training and testing data
###----------------------------###
#seed is set to get same values when the below code is run multiple times
set.seed(3)
training_indices <- sample(1:nrow(carprices),0.7*nrow(carprices))
train <- carprices[training_indices,]
test <- carprices[-training_indices,]

###----------------------------###
#Model creation
###----------------------------###
library(MASS)
library(car)
#Initial modelling with all variables to run stepAIC
model1 <- lm(price~.-car_ID,data = train)
summary(model1)

#stepaic function performs the stepwise selection and suggests which variables to remove
step <- stepAIC(model1,direction = "both")

model2 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +wheelbase +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +cylindernumbersix +hp_rmp_ratio +horsepower +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +curbweight +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model2)
vif(model2)

#Remove hp_rmp_ratio
model3 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +wheelbase +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +cylindernumbersix +horsepower +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +curbweight +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model3)
vif(model3)

#Remove horsepower
model4 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +wheelbase +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +cylindernumbersix +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +curbweight +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model4)
vif(model4)

#Remove cylindernumbersix
model5 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +wheelbase +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +curbweight +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model5)
vif(model5)

#Remove curbweight
model6 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +wheelbase +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model6)
vif(model6)

#Remove wheelbase
model7 <- lm(price~ +fuelsystemspfi +carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model7)
vif(model7)

#Remove fuelsystemsfpi
model8 <- lm(price~ carwidth +Makermercury +symboling.1 +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model8)
vif(model8)

#Remove symboling.1
model9 <- lm(price~ carwidth +Makermercury +Makerisuzu 
             +enginetypeohcf +Makerbuick +fuelsystemmfi +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback +fuelsystemmpfi 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model9)
vif(model9)

#Remove fuelsystemmpfi
model10 <- lm(price~ carwidth +Makermercury +Makerisuzu 
             +enginetypeohcf +Makerbuick +enginetypeohcv +Makervolvo 
             +compressionratio +enginetyperotor +carbodyhatchback 
             +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
             +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
             +boreratio +Makermitsubishi 
             +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
             +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model10)
vif(model10)

#Remove Makervolvo
model11 <- lm(price~ carwidth +Makermercury +Makerisuzu 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +compressionratio +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +fuelsystemspdi +cylindernumberfive
              +boreratio +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model11)
vif(model11)

#Remove fuelsystemspdi
model12 <- lm(price~ carwidth +Makermercury +Makerisuzu +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +boreratio +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +enginesize +Makerdodge +Makerbmw, data = train)
summary(model12)
vif(model12)

#Remove enginesize
model13 <- lm(price~ carwidth +Makerisuzu +Makermercury +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +boreratio +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model13)
vif(model13)

#Remove Makerisuzu
model14 <- lm(price~ carwidth +Makermercury +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +boreratio +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model14)
vif(model14)

#Remove Makermercury
model15 <- lm(price~ carwidth +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +boreratio +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model15)
vif(model15)

#Remove boreratio
model16 <- lm(price~ carwidth +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +peakrpm +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model16)
vif(model16)

#Remove peakrpm
model17 <- lm(price~ carwidth +compressionratio 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model17)
vif(model17)

#Remove compressionratio
model18 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor +carbodyhatchback 
              +Makervolkswagen +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model18)
vif(model18)

#Remove carbodyhatchback
model19 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor 
              +Makervolkswagen +aspirationTurbo +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model19)
vif(model19)

#Remove aspirationTurbo
model20 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor 
              +Makervolkswagen +Makermazda +Makerchevrolet
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model20)
vif(model20)

#Remove Makerchevrolet
model21 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor 
              +Makervolkswagen +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerdodge +Makerbmw, data = train)
summary(model21)
vif(model21)

#Remove Makerdodge
model22 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor 
              +Makervolkswagen +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree +Makerplymouth
              +enginelocationrear +Makerbmw, data = train)
summary(model22)
vif(model21)

#Remove Makerplymouth
model23 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv  
              +enginetyperotor 
              +Makervolkswagen +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model23)
vif(model23)

#Remove enginetyperotor
model24 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makervolkswagen +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makertoyota +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model24)
vif(model24)

#Remove Makertoyota
model25 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makervolkswagen +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model25)
vif(model25)

#Remove Makervolkswagen
model26 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +Makermitsubishi 
              +enginetypel +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model26)
vif(model26)

#Remove Makermitsubishi
model27 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makermazda
              +Makerrenault +Makernissan +cylindernumberfive
              +enginetypel +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model27)
vif(model27)

#Remove Makermazda
model28 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makerrenault +Makernissan +cylindernumberfive
              +enginetypel +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model28)
vif(model28)

#Remove Makernissan
model29 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makerrenault +cylindernumberfive
              +enginetypel +Makerhonda +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model29)
vif(model29)

#Remove Makerhonda
model30 <- lm(price~ carwidth 
              +enginetypeohcf +Makerbuick +enginetypeohcv
              +Makerrenault +cylindernumberfive
              +enginetypel +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model30)
vif(model30)

#Remove enginetypeohcf
model31 <- lm(price~ carwidth 
              +Makerbuick
              +Makerrenault +cylindernumberfive
              +enginetypel +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model31)
vif(model31)

#Remove makerrenault
model32 <- lm(price~ carwidth 
              +Makerbuick
              +cylindernumberfive
              +enginetypel +cylindernumberthree
              +enginelocationrear +Makerbmw, data = train)
summary(model32)
vif(model32)

###--------------------------------------------------------------###
#make predictions
###--------------------------------------------------------------###
predictions <- predict(model32,test)
test$pred_prices <- predictions

cor(test$price,test$pred_prices)

library(ggplot2)
ggplot(data=test,aes(car_ID,price-pred_prices)) + geom_point()
#the plot doesn't follow any pattern asserting that results are certain