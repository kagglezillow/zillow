#--------------------------------------------------------------------
# 00. setting -------------------------------------------------------
#--------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(data.table)
install.packages("DT")
library(DT)
library(lubridate)
library(tidyr)
library(leaflet)
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)
library(caret)
install.packages("h2o")
library(h2o)
# function
## data type transformations - factoring ������
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# load data
train1 <- fread("C:/�ڷ�/���͵�/��ŷĳ��/Zillow Prize/�����ͼ�/train_2016_v2.csv/train_2016_v2.csv")
propert <- fread("C:/�ڷ�/���͵�/��ŷĳ��/Zillow Prize/�����ͼ�/properties_2016.csv/properties_2016.csv")
sample <- fread("C:/�ڷ�/���͵�/��ŷĳ��/Zillow Prize/�����ͼ�/sample_submission.csv/sample_submission.csv")

set.seed(1711)
#--------------------------------------------------------------------
# 01. feature sumamry -----------------------------------------------
#--------------------------------------------------------------------
# ���� 22�� + logerror ����
useVar <- c("logerror","taxvaluedollarcnt"
            ,"structuretaxvaluedollarcnt"
            ,"landtaxvaluedollarcnt"
            ,"taxamount"
            ,"taxdelinquencyflag"
            ,"lotsizesquarefeet"
            ,"finishedsquarefeet12"
            ,"calculatedfinishedsquarefeet"
            ,"fullbathcnt"
            ,"bathroomcnt"
            ,"bedroomcnt"
            ,"roomcnt"
            ,"fireplaceflag"
            ,"hashottuborspa"
            ,"latitude"
            ,"longitude"
            ,"fips"
            ,"regionidzip"
            ,"regionidcity"
            ,"regionidcounty"
            ,"yearbuilt")
            
#--------------------------------------------------------------------
# 02. feature engineering -------------------------------------------
#--------------------------------------------------------------------
total <- merge(x = train1, y = propert, by = "parcelid", all.x = TRUE)

# ����� 22�� ���� ����
total <- total %>%  select(useVar)

# NA ä�� ���� list (7��)
MiceDataVar <- c("taxvaluedollarcnt"
              ,"structuretaxvaluedollarcnt"
              ,"landtaxvaluedollarcnt"
              , "taxamount"
              ,"lotsizesquarefeet"
              , "finishedsquarefeet12"
              ,"calculatedfinishedsquarefeet")

# split data
MiceData <- total %>% select(MiceDataVar)
NaData <- total %>% select(-one_of(MiceDataVar))

#NA Imputation
# mice ���
miceResult <- mice(MiceData, m = 2)
completeMice <- complete(miceResult, 1)
miceResult <- mice(completeMice, m = 2)
completeMice <- complete(miceResult, 1)


# knn ���
#install.packages("DMwR")
#library(DMwR)
#knnResult <- knnImputation(naData,k=5)

# ����ȭ
NaData$hashottuborspa <- ifelse(NaData$hashottuborspa == "", 0, 1)
NaData$fireplaceflag <- ifelse(NaData$fireplaceflag == "", 0 ,1)
NaData$taxdelinquencyflag <- ifelse(NaData$taxdelinquencyflag == "Y", 1,0 )

# train set ���� (mice��� ����)
train <- cbind(completeMice, NaData)
#train <- cbind(knnResult, noNaData)


#--------------------------------------------------------------------
# 02. model(select importance var) ----------------------------------
#--------------------------------------------------------------------
# formular ����
formula.init <- "logerror ~ ."
formula.init <- as.formula(formula.init)

# 5-fold cv
control <- trainControl(method = "cv", number = 5)

### ����������Ʈ
model_rf <- train(formula.init, data=train, method="rf", 
               trControl=control, na.action = na.omit, importance=TRUE)

#������ �߿䵵 Ȯ��
importance <- varImp(model_rf) #, scale=FALSE
plot(importance, cex.lab=0.5)
model_rf$modelInfo$varImp


### xgboost
#trainMat<-model.matrix(~.,data = train)
#params<-list(max.depth = 5, eta = 0.3, objective = "reg:linear", eval_metric = "error")
#model_xgb<-xgboost(trainMat, label = trainMat[,9], params = params, nrounds = 5)


#--------------------------------------------------------------------
#TEST SET�����------------------------------------------------------
#--------------------------------------------------------------------

test_mice <- fread("c:/data/completeMice.csv")

useVar1 <- c("taxvaluedollarcnt" #logerror�� ����
            ,"structuretaxvaluedollarcnt"
            ,"landtaxvaluedollarcnt"
            ,"taxamount"
            ,"taxdelinquencyflag"
            ,"lotsizesquarefeet"
            ,"finishedsquarefeet12"
            ,"calculatedfinishedsquarefeet"
            ,"fullbathcnt"
            ,"bathroomcnt"
            ,"bedroomcnt"
            ,"roomcnt"
            ,"fireplaceflag"
            ,"hashottuborspa"
            ,"latitude"
            ,"longitude"
            ,"fips"
            ,"regionidzip"
            ,"regionidcity"
            ,"regionidcounty"
            ,"yearbuilt")

# Mice�� �̿��� NA ä�� ���� list
MiceDataVar <- c("taxvaluedollarcnt"
               ,"structuretaxvaluedollarcnt"
               ,"landtaxvaluedollarcnt"
               , "taxamount"
               ,"lotsizesquarefeet"
               , "finishedsquarefeet12"
               ,"calculatedfinishedsquarefeet")
# ����� 22�� ���� ����
test_tot <- propert %>%  select(useVar1)
#MiceData <- total %>%   select(MiceDataVar) # mice/knn����  na��ü
NaData <- test_tot %>%   select(-one_of(naDataVar)) #�� �� ����

# TEST SET  mice Imputation
#miceResult <- mice(naData, m = 2)
#completeMice <- complete(miceResult, 1)
#miceResult <- mice(completeMice, m = 5)
#completeMice <- complete(miceResult, 1)
#��� ����
#write.csv(completeMice, "c:/data/completeMice.csv",sep=",")

# ����ȭ
NaData$hashottuborspa <- ifelse(NaData$hashottuborspa == "", 0, 1)
NaData$fireplaceflag <- ifelse(NaData$fireplaceflag == "", 0 ,1)
NaData$taxdelinquencyflag <- ifelse(NaData$taxdelinquencyflag == "Y", 1,0 )

#test set ����
test <- cbind(test_mice, NaData)



## xgboost testset
#testMat<-model.matrix(~.,data = test)
#xgb_pred<-predict(model_xgb, testMat)
#predictions<-xgb_pred


#--------------------------------------------------------------------
#�� ���� �� �������� ����------------------------------------------
#--------------------------------------------------------------------

options(scipen = 10000)

predictions_target <- data.frame(predict(model_rf, propert))

predictions <- round(as.vector(predictions_target$predict), 4)

result <- data.frame(cbind(propert$parcelid, predictions, predictions * .99,
                           predictions * .98, predictions * .97, predictions * .96,
                           predictions * .95))
colnames(result)<-c("parcelid","201610","201611","201612","201710","201711","201712")
options(scipen = 999)
write.csv(result, file = "submission_xgb_1016.csv", row.names = FALSE )






###################################���� �������
######## ADD SOME FEATURES #######

# Dist from centroid
p$longmean <- mean((p$longitude), na.rm=TRUE)/1e6
p$latmean <- mean((p$latitude), na.rm=TRUE)/1e6

# Adjusted long lat
p$longitude1 <- p$longitude/1e6
p$latitude1 <- p$latitude/1e6

# Haversine distance
ncol(p)
p$geodist <- distHaversine(p[,31:32], p[,33:34])
# �Ϲ����� : �� ���� �ִܰŸ��� ���ϴ� ����
p[,longmean := NULL]
p[,latmean := NULL]
p[,longitude1 := NULL]
p[,latitude1 := NULL]

# Tax based info : �� ����� �ǹ��� ��ġ (���� ���� good)
pr[, strValRatio := (pr$structuretaxvaluedollarcnt / (pr$landtaxvaluedollarcnt + pr$structuretaxvaluedollarcnt))]
#'landtaxvaluedollarcnt'	��ȹ�� ���������� ���� �򰡰�ġ		
#'structuretaxvaluedollarcnt'	��ȹ(parcel)�� ������ �ǹ��� �򰡰�ġ		

# Bathrooms are important : bathroomcnt�� �߿䵵�� ���̱� ���� living area ���̿� ���Ͽ� ��ȣ�ۿ��� ����
pr[, bathInteraction := (pr$bathroomcnt * pr$calculatedfinishedsquarefeet)]
#'bathroomcnt'	Bathroom ����		
#'calculatedfinishedsquarefeet'	 Calculated total finished living area of the home 


# Sq Ft / Room : �� 1���� ���� ����
pr[, sqftRoom := (pr$calculatedfinishedsquarefeet / pr$roomcnt)]
#'calculatedfinishedsquarefeet'	 Calculated total finished living area of the home 
#'roomcnt'	 Total number of rooms in the principal residence


# Struc / Lanad : ��ũ���� �������, ���� ���� ���� �ð��� Ȯ���� �ְ� ���� Ŭ���� ������ �� �� ����
pr[, strucLand := (pr$calculatedfinishedsquarefeet / pr$lotsizesquarefeet)]
#'calculatedfinishedsquarefeet'	 Calculated total finished living area of the home
#'lotsizesquarefeet'	 Area of the lot in square feet (�������)
#calculatedfinishedsquarefeet ���� ���ԵǴ� �� : �� ��ü �������

# Age
pr[, age := 2017 - pr$yearbuilt]