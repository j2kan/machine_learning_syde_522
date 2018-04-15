library(plyr)
library(knitr)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(glmnet)
library(Matrix)
library(foreach)
# read the training data
train <- read.csv("~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/train.csv", stringsAsFactors = F)
test <- read.csv("~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/test.csv", stringsAsFactors = F)
# set up the testing sale price, NA need to fill this response variable
test$SalePrice <- NA
all <- rbind(train, test)
# find columns with missing data
NAcol <- which(colSums(is.na(all)) > 0)
# aggregate these columns with missing data and find the number of values missing
# The SalePrice has 1459 NA which is accurate to the test set that is missing the response
colSums(sapply(all[NAcol], is.na))
# find features that needed cleaning
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
# 1.0 POOL quality & squarefootage
###########################################################################
# poolQC, many houses without a pool, convert variable into ordinal
all$PoolQC[is.na(all$PoolQC)] <- 'Np'
pool_qc <- c('Np' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)
all$PoolQC<-as.integer(revalue(all$PoolQC, pool_qc))
# there are only 10 properties out of 2919 properties with a pool
# this tell us there are data with poolArea without a poolQC
table(all$PoolQC)
table(all$PoolArea)
# no obvious correlation with overall Quality and size of pool area
all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
# assign with default TA, average/typical
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 2
all$PoolQC[2600] <- 2
###########################################################################
# 2.0 MISC
###########################################################################
# misc feature is not ordinal. it varies from shed / tennis court... etc
table(all$MiscFeature)
# replace missing as None
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
###########################################################################
# 3.0 ALLEY
###########################################################################
table(all$Alley)
# replace missing as None
all$Alley[is.na(all$Alley)] <- 'None'
###########################################################################
# 4.0 FENCE
###########################################################################
table(all$Fence)
# replace missing as None
all$Fence[is.na(all$Fence)] <- 'None'
###########################################################################
# 5.0 FIREPLACE
###########################################################################
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
table(all$FireplaceQu)
# convert to ordinal
fire_qc <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu, fire_qc))
###########################################################################
# 6.0 LOT
###########################################################################
lotMed <- median(all$LotFrontage, na.rm=TRUE)
for (i in 1:nrow(all)){
if(is.na(all$LotFrontage[i])){
all$LotFrontage[i] <- as.integer(lotMed)
}
}
# LOT shape convert to ordinal
all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
###########################################################################
# 7.0 Garage
###########################################################################
# replace missing garage year built with year remodel
table(all$GarageYrBlt)
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
# There are 157 missing values for garage type but 159 for garageFinish, garageQuality and garageCond
# Find the 2 additional NAs
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
# the issue is with house 2127 and 2577
# 2577 appears to not actually have a garage
# 2127 seems to have a garage, assign average value
all$GarageType[2577] <- NA
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageCond[2127] <- 'TA'
all$GarageQual[2127] <- 'TA'
all$GarageFinish[2127] <- 'RFn'
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
table(all$GarageType)
# Garage Finish change to  Ordinal
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
garageFin <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
all$GarageFinish<-as.integer(revalue(all$GarageFinish, garageFin))
# Garage quality change to ordinal
all$GarageQual[is.na(all$GarageQual)] <- 'None'
garage_qc <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$GarageQual<-as.integer(revalue(all$GarageQual, garage_qc))
# Garage condition change to ordinal
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, garage_qc))
###########################################################################
# 8.0 Basement
###########################################################################
# find basements where there are some overlap of NA
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
all$BS
# Replace with middle value
all$BsmtFinType2[333] <- 'Rec'
all$BsmtExposure[c(949, 1488, 2349)] <- 'Mn'
all$BsmtCond[c(2041, 2186, 2525)] <- 'TA'
all$BsmtQual[c(2218, 2219)] <- 'TA'
# change basement quality to ordinal
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
basement_qc <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$BsmtQual<-as.integer(revalue(all$BsmtQual, basement_qc))
# change basement condition to ordinal
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, basement_qc))
# change basement exposure to ordinal
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
expo <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, expo))
# change basement finish type to ordinal
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
fin <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, fin))
# change basement finish type 2 to ordinal
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, fin))
# replace missing bath, squarefootage with 0
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0
###########################################################################
# 9.0 Masonary
###########################################################################
# find property with msn area without a type
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
# replace with middle value
all$MasVnrType[2611] <- 'BrkFace'
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
# conver masonry to ordinal, there is no cinder block, not included
mason <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, mason))
# replce missing area, with 0
all$MasVnrArea[is.na(all$MasVnrArea)] <-0
###########################################################################
# 10.0 Masonary
###########################################################################
# replace missing zoning with standard residential low
all$MSZoning[is.na(all$MSZoning)] <- 'RL'
###########################################################################
# 11.0 Kitchen
###########################################################################
# replace with average quality
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA'
kitchen_qc <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$KitchenQual<-as.integer(revalue(all$KitchenQual, kitchen_qc))
###########################################################################
# 12.0 Utilities
###########################################################################
# this won't be an effective predictor variable as there is no variable in the data
table(all$Utilities)
all$Utilities <- NULL
###########################################################################
# 13.0 Home Functionality
###########################################################################
# replace the 1 missing value with a middle value
all$Functional[is.na(all$Functional)] <- 'Mod'
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
###########################################################################
# 14.0 External Variable
###########################################################################
# replace 1 missing value with middle value
all$Exterior1st[is.na(all$Exterior1st)] <- 'MetalSd'
all$Exterior2nd[is.na(all$Exterior2nd)] <- 'MetalSd'
###########################################################################
# 15.0 Electricity
###########################################################################
# replace 1 with middle vaue
all$Electrical[is.na(all$Electrical)] <- 'FuseF'
###########################################################################
# 16.0 Sales Type
###########################################################################
# replace with most common value
all$SaleType[is.na(all$SaleType)] <- 'WD'
###########################################################################
# 17.0 Heating
###########################################################################
# make heating quality ordinal
heating_qc <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$HeatingQC<-as.integer(revalue(all$HeatingQC, heating_qc))
# make central heating ordinal
all$CentralAir<-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))
###########################################################################
# 18.0 Land
###########################################################################
# make landscape ordinal
all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
###########################################################################
# 19.0 Land
###########################################################################
# change street to ordinal
all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
# make pave driveway ordinal
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
###########################################################################
# 20.0 Dwelling
###########################################################################
# change from number to categorical
all$MSSubClass <- as.factor(all$MSSubClass)
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
###########################################################################
# 21.0 Feature Engineering
###########################################################################
# combine bathrooms
all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)
all$FullBath <- NULL
all$HalfBath <- NULL
all$BsmtFullBath <- NULL
all$BsmtHalfBath <- NULL
# combine porch space
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
all$OpenPorchSF <- NULL
all$EnclosedPorch <- NULL
all$X3SsnPorch <- NULL
all$ScreenPorch <- NULL
# add remodel variable 0=No Remodeling, 1=Remodeling
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1)
# calculate age of home
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
all$YearBuilt <- NULL
all$YearRemodAdd <- NULL
all$YrSold <- NULL
# combine square ft
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF
all$GrLivArea <- NULL
all$TotalBsmtSF <- NULL
# combine garage quality/cond
all$GarageScore <- all$GarageCond + all$GarageQual
all$GarageCond <- NULL
all$GarageQual <- NULL
# combine basement quality/cond
all$BsmtScore <- all$BsmtCond + all$BsmtQual
all$BsmtCond <- NULL
all$BsmtQual <- NULL
# combine overall quality/cond
all$OverallScore <- all$OverallCond + all$OverallQual
all$OverallCond <- NULL
all$OverallQual <- NULL
# remove based on histogram distribution and correlation to sales price
all$BsmtFinSF2 <- NULL
all$BsmtFinType2 <- NULL
all$PoolArea <- NULL
all$MiscVal <- NULL
all$PavedDrive <- NULL
all$RoofMatl <- NULL
all$ExterCond <- NULL
all$GarageYrBlt <- NULL
all$LowQualFinSF <- NULL
all$BsmtFinType1 <- NULL
all$Street <- NULL
all$MiscFeature <- NULL
all$MoSold <- NULL
all$LandSlope <- NULL
all$Alley <- NULL
all$LandContour <- NULL
all$Condition1 <- NULL
all$Condition2 <- NULL
all$Heating <- NULL
all$Electrical <- NULL
all$KitchenAbvGr <- NULL
all$TotRmsAbvGrd <- NULL
all$Fireplaces <- NULL

# CHECK if saleprice is normal
qqnorm(all$SalePrice)
qqline(all$SalePrice)

# Normalizing any numerical data
num_var <- which(sapply(all, is.numeric))
num_name <- names(num_var)

num_name <- num_name[!(num_name %in% c('Id', 'SalePrice'))]
all <- all %>% mutate_each_(funs(scale(.) %>% as.vector),
vars=num_name)
cat_var <- all[, !(names(all) %in% num_name)]
cat_var <- cat_var[, names(cat_var) != 'SalePrice']
names(cat_var)
num_name
one_hot <- as.data.frame(model.matrix(~.-1, cat_var))
one_hot
table(all$Exterior1st)
# check which one hot isn't in test set and remove from column
not_in_test_set <- which(colSums(one_hot[1459:2917,])==0)
colnames(one_hot[not_in_test_set])
one_hot <- one_hot[,-not_in_test_set]
less_than_15_one_hot <- which(colSums(one_hot[1:1458,])<15)
one_hot <- one_hot[,-less_than_15_one_hot]
numeric_only <- all[, names(all) %in% num_name]
# log sale price to normalize
numeric_only$SalePrice <- log(all$SalePrice)
numeric_with_one_hot <- cbind(numeric_only, one_hot)

# check if saleprice normal after log
qqnorm(numeric_with_one_hot$SalePrice)
qqline(numeric_with_one_hot$SalePrice)

###########################################################################
all$Id <- NULL
numeric_with_one_hot$Id <- NULL

write.csv(all, file = "~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/cleaned_train_test.csv", quote=FALSE, row.names=FALSE)
write.csv(numeric_with_one_hot, file = "~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/cleaned_train_test_numeric_one_hot.csv", quote=FALSE, row.names=FALSE)
train1 <- numeric_with_one_hot[1:1460,]
test1 <- numeric_with_one_hot[1461:2919,]
train1_label <- train1$SalePrice
train1$SalePrice <- NULL
# write.csv(train1, file = "~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/train1.csv", quote=FALSE, row.names=FALSE)
# write.csv(test1, file = "~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/test1.csv", quote=FALSE, row.names=FALSE)


# LASSO MDOEL
###########################################################################

set.seed(1234)
control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=train1_label, method='glmnet', trControl= control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)
test1$SalePrice <- NULL
pred <- predict(lasso_mod, test1)
head(pred)
predictions_lasso <- exp(pred) #need to reverse the log to the real values

#write submission files
sub_lasso <- data.frame(SalePrice = predictions_lasso)
write.csv(sub_lasso, file = '~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/Lasso_model.csv', row.names = F)



# XgBoost MDOEL
###########################################################################
train1$SalePrice <- NULL

dtrain <- xgb.DMatrix(data = as.matrix(train1), label= train1_label)
dtest <- xgb.DMatrix(data = as.matrix(test1))

param<-list(
  objective = "reg:linear",
  eval_metric = "rmse",
  booster = "gbtree",
  eta = 0.1,
  gamma = 0
)
xgb_mod <- xgb.train(data = dtrain, params = param, watchlist = list(train = dtrain), nrounds = 700, early.stopping.rounds = 50, verbose = 1, print_every_n = 100)

xgb_pred <- predict(xgb_mod, dtest) 
predictions_XGB <- exp(XGBpred) 
#write submission files
sub_xgb <- data.frame(SalePrice = predictions_XGB)
write.csv(sub_xgb, file = '~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/xgb_model.csv', row.names = F)


# SVM
###########################################################################
library("SuperLearner")
library("ipred")
set.seed(150)
SL.model <- SuperLearner(train1_label,
                             train1,
                             family = gaussian(),
                             SL.library=list("SL.ksvm"))

SL.model
SL_predictions <- predict.SuperLearner(SL.model, newdata=test1)

write.csv(SL_predictions, file = '~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/ksvm_model.csv', row.names = F)

# SUPER learner
###########################################################################
library("SuperLearner")
set.seed(150)
# SL.ipredbagg

SL.models <- SuperLearner(train1_label,
                         train1,
                         family = gaussian(),
                         SL.library=list("SL.ksvm", "SL.ipredbagg", "SL.bayesglm", "SL.glmnet"))

SL.models
SL_predictions <- predict.SuperLearner(SL.models, newdata=test1)

write.csv(SL_predictions, file = '~/Dropbox/4B/SYDE 522-Machine Intelligence/Term Project - Housing Price/SLensemble.csv', row.names = F)
