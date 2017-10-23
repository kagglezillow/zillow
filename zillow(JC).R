
# 00. setting -------------------------------------------------------
require(geosphere)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
library(lubridate)
library(tidyr)
library(leaflet)
library(mice)
library(VIM)
library(caret)

# function
## data type transformations - factoring 범주형
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# load data
train <- fread("data/train_2016_v2.csv")
propert <- fread("data/properties_2016.csv")


set.seed(1709)

### 01. EDA -------------------------------------------
## 1) train => 사용할 변수 22개 추출하고 NA mice채우기
# 변수 22개 + logerror 선택
useVar <- c("logerror","taxvaluedollarcnt","structuretaxvaluedollarcnt"
            ,"landtaxvaluedollarcnt","taxamount","taxdelinquencyflag"
            # ,"taxdelinquencyyear" 보류 flag 영향도 높으면 사용
            ,"lotsizesquarefeet","finishedsquarefeet12",
            "calculatedfinishedsquarefeet","fullbathcnt","bathroomcnt"
            ,"bedroomcnt","roomcnt","fireplaceflag","hashottuborspa"
            ,"latitude","longitude","fips","regionidzip"
            ,"regionidcity","regionidcounty"
            ,"regionidneighborhood","yearbuilt")


# 일단은 중복 제거하지 않고 진행 했습니다.
total <- merge(x = train, y = propert, by = "parcelid", all.x = TRUE)

# 사용할 22개 변수 추출
total <- total %>% 
  select(useVar)

# NA 채울 변수 list
naDataVar <- c("taxvaluedollarcnt"
               ,"structuretaxvaluedollarcnt"
               ,"landtaxvaluedollarcnt"
               ,"taxamount"
               ,"lotsizesquarefeet"
               ,"finishedsquarefeet12"
               ,"calculatedfinishedsquarefeet")

# split data
naData <- total %>% 
  select(naDataVar)

noNaData <- total %>% 
  select(-one_of(naDataVar))

# mice 결과
miceResult <- mice(naData, m = 5)
completeMice <- complete(miceResult, 1)

# 왜인지 변수 한개가 자꾸 빠지네요...
# 2번 돌려주셔야 될 것 같아요.
miceResult <- mice(completeMice, m = 5)
completeMice <- complete(miceResult, 1)

# cbind (mice 또는 knn)
train <- cbind(completeMice, noNaData)

# 범주화
train$hashottuborspa <- ifelse(train$hashottuborspa == "", 0, 1)
train$fireplaceflag <- ifelse(train$fireplaceflag == "", 0 ,1)
train$taxdelinquencyflag <- ifelse(train$taxdelinquencyflag == "Y", 1,0 )

facVar <- c("hashottuborspa","fireplaceflag","taxdelinquencyflag")
train <- to.factors(train, facVar)
normalData <- train
  
## 2) Feature add ------------------------------------------
addData <- normalData
# Havesine distance
# addData
addData$longmean <- mean((addData$longitude), na.rm=TRUE)/1e6
addData$latmean <- mean((addData$latitude), na.rm=TRUE)/1e6
addData$longitude1 <- addData$longitude/1e6
addData$latitude1 <- addData$latitude/1e6
addData$geodist <- distHaversine(addData[,24:25]
                                   ,addData[,26:27])
# 컬럼 그냥 전부다 삭제
addData <- addData %>% 
  select(-longmean,-latmean,-longitude1,-latitude1)
# addData[,longmean := NULL]
# addData[,latmean := NULL]
# addData[,longitude1 := NULL]
# addData[,latitude1 := NULL]

# Tax based info
# structure를 분자로
addData$landValRatio <- addData$structuretaxvaluedollarcnt                          / (addData$landtaxvaluedollarcnt + addData$structuretaxvaluedollarcnt)
# Bathrooms are important
addData$bathInteraction <- addData$bathroomcnt * 
                              addData$calculatedfinishedsquarefeet
# Sq Ft / Room 
addData$sqftRoom <- ifelse(addData$roomcnt == 0, addData$calculatedfinishedsquarefeet, addData$calculatedfinishedsquarefeet / addData$roomcnt)

# Struc / Lanad
addData$strucLand <- addData$calculatedfinishedsquarefeet / addData$lotsizesquarefeet
# Age
addData$age <- 2017 - addData$yearbuilt


## 3) Normal, add추가 = 2가지 data feature importance 확인
# formular 지정
formulaInit <- "logerror ~ ."
formulaInit <- as.formula(formulaInit)

# 5-fold cv
control <- trainControl(method = "cv", number = 5)

# method RF
# 1) normalData
normalModel <- train(formulaInit, data=normalData, method="rf", 
               trControl=control, 
               na.action = na.omit,
               importance = T)

normalImportance <- varImp(normalModel, scale=FALSE)

# 2) addData
addModel <- train(formulaInit, data=addData, method="rf", 
               trControl=control, 
               na.action = na.omit,
               importance = T)

addImportance <- varImp(addModel, scale=FALSE)

plot(normalImportance, cex.lab=0.5)
plot(addImportance, cex.lab=0.5)


### 02. model -----------------------------------------------
## Normal, add  2가지 데이터셋 importance보고 모델 생성
formulaInit <- "logerror ~ yearbuilt+lotsizesquarefeet+calculatedfinishedsquarefeet+finishedsquarefeet12+structuretaxvaluedollarcnt+taxamount+taxvaluedollarcnt+fullbathcnt+bedroomcnt"

# lm model
formulaInit <- "logerror ~ ." 
formulaInit <- as.formula(formulaInit)
normalModel <- lm(formulaInit, data=normalData)
summary(normalModel)

# 다중공선성 확인
car::vif(normalModel)

# 변수 영향도 확인
lm.beta::lm.beta(normalModel)

# lm model
formulaInit <- "logerror ~ age+structuretaxvaluedollarcnt+finishedsquarefeet12+yearbuilt+taxamount+taxvaluedollarcnt" 
formulaInit <- as.formula(formulaInit)
addModel <- lm(formulaInit, data=addData)
summary(addModel)

# 다중공선성 확인
car::vif(addModel)

# 변수 영향도 확인
lm.beta::lm.beta(addModel)


# 02. submit 파일 -------------------------------------------
# propert에서 mice채운 컬럼들을 대체해줘야한다.
# testMice <- testMice %>% 
#   select(-V1)
# testMice <- fread(file = "data/testMice.csv")
noNaPropertdata <- propert %>% 
  select(-one_of(naDataVar))
testResult <- cbind(noNaPropertdata, testMice)
# submit prediction value calculate
train1 <- train[!duplicated(train$parcelid), ]
normalSubmit <- merge(x = testResult, y = train1, by = "parcelid",
                all.x = TRUE)

# 위 model 과정과 동일하게 시행 - 공통
normalSubmit$hashottuborspa <- ifelse(normalSubmit$hashottuborspa == "", 0, 1)
normalSubmit$fireplaceflag <- ifelse(normalSubmit$fireplaceflag == "", 0 ,1)
normalSubmit$taxdelinquencyflag <- ifelse(normalSubmit$taxdelinquencyflag == "Y", 1, 0)
normalSubmit <- to.factors(normalSubmit, facVar)

# 나중에 feature 추가할 add 데이터 만들어두고
addSubmit <- normalSubmit

# 예측
predictions <- data.frame(predict(lm_model, normalSubmit))
normalSubmit$p_lr1 <- predictions$predict.lm_model..normalSubmit.
normalSubmit$p_lr1[is.na(normalSubmit$p_lr1)] <- mean(normalSubmit$logerror, na.rm = TRUE)

# summit 파일 생성
normalSubmit <- data.frame(normalSubmit[,c("parcelid", "p_lr1")])
normalSubmit$"201610" <- round(normalSubmit$p_lr1,4)
normalSubmit$"201611" <- round(normalSubmit$p_lr1,4)
normalSubmit$"201612" <- round(normalSubmit$p_lr1,4)
normalSubmit$"201710" <- 0
normalSubmit$"201711" <- 0
normalSubmit$"201712" <- 0
normalSubmit$p_lr1<- NULL
write.csv(normalSubmit, file = "normalSubmit.csv", row.names = FALSE)

## 2) Feature add ------------------------------------------
# Havesine distance
# addData
addSubmit$longmean <- mean((addSubmit$longitude), na.rm=TRUE)/1e6
addSubmit$latmean <- mean((addSubmit$latitude), na.rm=TRUE)/1e6
addSubmit$longitude1 <- addSubmit$longitude/1e6
addSubmit$latitude1 <- addSubmit$latitude/1e6
addSubmit$geodist <- distHaversine(addSubmit[,24:25]
                                 ,addSubmit[,26:27])
# 컬럼 그냥 전부다 삭제
addSubmit <- addSubmit %>% 
  select(-longmean,-latmean,-longitude1,-latitude1)
# addSubmit[,longmean := NULL]
# addSubmit[,latmean := NULL]
# addSubmit[,longitude1 := NULL]
# addSubmit[,latitude1 := NULL]

# Tax based info
# structure를 분자로
addSubmit$landValRatio <- addSubmit$structuretaxvaluedollarcnt                          / (addSubmit$landtaxvaluedollarcnt + addSubmit$structuretaxvaluedollarcnt)
# Bathrooms are important
addSubmit$bathInteraction <- addSubmit$bathroomcnt * 
  addSubmit$calculatedfinishedsquarefeet
# Sq Ft / Room
addSubmit$sqftRoom <- addSubmit$calculatedfinishedsquarefeet / addSubmit$roomcnt
# Struc / Lanad
addSubmit$strucLand <- addSubmit$calculatedfinishedsquarefeet / addSubmit$lotsizesquarefeet
# Age
addSubmit$age <- 2017 - addSubmit$yearbuilt


# 예측
predictions <- data.frame(predict(addModel, addSubmit))
addSubmit$p_lr1 <- predictions$predict.addModel..addSubmit.
addSubmit$p_lr1[is.na(addSubmit$p_lr1)] <- mean(addSubmit$logerror, na.rm = TRUE)

# summit 파일 생성
addSubmit <- data.frame(addSubmit[,c("parcelid", "p_lr1")])
addSubmit$"201610" <- round(addSubmit$p_lr1,4)
addSubmit$"201611" <- round(addSubmit$p_lr1,4)
addSubmit$"201612" <- round(addSubmit$p_lr1,4)
addSubmit$"201710" <- 0
addSubmit$"201711" <- 0
addSubmit$"201712" <- 0
addSubmit$p_lr1<- NULL
write.csv(addSubmit, file = "addSubmit.csv", row.names = FALSE)

