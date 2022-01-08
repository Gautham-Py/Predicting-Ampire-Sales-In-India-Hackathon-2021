# Load packages #
library(car)
library(DescTools)
library(corrplot)

getwd()

#Set Working Directory

setwd("C:/Users/Byragi/Desktop/HACKATHON")

getwd()

#Importing Data File
" The Csv data file was inspected in excel, and was found to contain the 
  Monthly column to be split into 2 Rows, thus the data was trimmed and 
  corrected before importing."
# Training Set #

rawdf= read.csv("TRAINING.CSV",header = T, stringsAsFactors = T)

# Test Set #

predictdf= read.csv("TEST.CSV",header = T, stringsAsFactors = T)


# Data Exploration #

View(rawdf)

View (predictdf)

# Dropping additional columns in the dataset #

rawdf$X   = NULL
rawdf$X.1 = NULL

# Columns removed in Training set
 " The  dataset contained 34 columns,now  upon cleaning the data contains 33"
 
predictdf$X  = NULL 
predictdf$X.1= NULL 
 
# Columns removed in Test set
" The  dataset contained 33 columns,now  upon cleaning the data contains 32"


# Data Preprocessing_ Sampling#
set.seed(143)
Rownum= sample(x=1:nrow(rawdf), size = 0.80*nrow(rawdf) )

head(Rownum)

traindf= rawdf[Rownum, ]
testdf= rawdf[-Rownum, ] 

# Creating Source column in both Training and Testing set #

traindf$Source= "Train"

testdf$Source= "Test"

predictdf$Source= "Predict"

predictdf[ ,'Value'] = NA    # adding the value column in the Test Data Set#


# Combining Data Sets  to remove columns(don't assist) during model building #

fulldf= rbind(traindf, testdf, predictdf)


# Columns to be removed! #

fulldf =  subset(fulldf, select = -c(Item.Code, Date, Item.Description,
                                     Item.Classification, Item.Cat..Code,
                                     MRP..BOX,Quality.Code, Customer.Code,
                                     Customer.City,Ship.AC0.to.City, FY,
                                     Review.Zone,Re.Territory))

# Deleted Columns
   "Those columns with date, item_codes and categories, also customer data was 
    removed for predicting a better model"


#Checking Summary #

summary(fulldf [fulldf$Source == "Train", ])


# Check for Missing values # 

colSums(is.na(fulldf))

# variables having missing values 
  " Billing.Rate.Sqm, Buyer.Rate.Sqm, AD4.Sqm....all are Continous variable, 
    hence the missing value is replaced by  median"

   #Billing.Rate.Sqm#
 
# Find Median _ from train data #

median(traindf$Billing.Rate.Sqm)

tmedian= median(traindf$Billing.Rate.Sqm, na.rm = T) 

tmedian      # temporary median #

#missing rows

mvrows= is.na(fulldf[ ,"Billing.Rate.Sqm" ])
sum(mvrows)

#imputing missing values #
fulldf[mvrows,"Billing.Rate.Sqm"] = tmedian

colSums(is.na(fulldf))

   # Buyer.Rate.Sqm #

# Find Median _ from train data #

median(traindf$Buyer.Rate.Sqm)

tmedian= median(traindf$Buyer.Rate.Sqm, na.rm = T) 

tmedian     

#missing rows

mvrows= is.na(fulldf[ ,"Buyer.Rate.Sqm" ])
sum(mvrows)

#imputing missing values #
fulldf[mvrows,"Buyer.Rate.Sqm"] = tmedian

colSums(is.na(fulldf))

    # AD4.Sqm #

# Find Median _ from train data #

median(traindf$AD4.Sqm)

tmedian= median(traindf$AD4.Sqm, na.rm = T) 

tmedian     

#missing rows

mvrows= is.na(fulldf[ ,"AD4.Sqm" ])
sum(mvrows)

#imputing missing values #
fulldf[mvrows,"AD4.Sqm"] = tmedian

colSums(is.na(fulldf))

# all the missing values except Total column is cleared #

summary(fulldf)

# replacing #NAME? in data set to their mode value #
  " AD5.Sqm, AD6.Sqm, AD7.Sqm,"
  
# since the mode of AD5.Sqm is 0, we will replace it with the same#
  
fulldf$AD5.Sqm[fulldf$AD5.Sqm == "#NAME?"]= 0

# since the mode of AD6.Sqm is 0, we will replace it with the same#

fulldf$AD6.Sqm[fulldf$AD6.Sqm == "#NAME?"]= 0

# since the mode of AD7.Sqm is 0, we will replace it with the same#

fulldf$AD7.Sqm[fulldf$AD7.Sqm == "SIPL/1819/06179"]= 0

fulldf$AD7.Sqm[fulldf$AD7.Sqm == "SIPL/1819/06347"]= 0

fulldf$AD7.Sqm[fulldf$AD7.Sqm == "SIPLEXS18/0091"]= 0

fulldf$Sales.Type[fulldf$Sales.Type == " "]= "Govt"


summary(fulldf)

colSums(is.na(fulldf))

# since only total AD contains missing values #
fulldf$AD5.Sqm = as.numeric(fulldf$AD5.Sqm)

fulldf$AD6.Sqm = as.numeric(fulldf$AD6.Sqm)

fulldf$AD7.Sqm = as.numeric(fulldf$AD7.Sqm)

fulldf$Total.AD.Sqm = fulldf$AD1..Sqm + fulldf$AD2.Sqm + fulldf$AD3.Sqm + 
                      fulldf$AD4.Sqm + fulldf$AD5.Sqm + fulldf$AD6.Sqm + 
                      fulldf$AD7.Sqm


# summary after complete data preprocessing #

summary(fulldf)

# since the value column is stil showing errors#
traindf    = subset(fulldf, subset = fulldf$Source == "Train")

testdf     = subset(fulldf, subset = fulldf$Source == "Test") 

predictdf  = subset(fulldf, subset = fulldf$Source == "Predict")  

# making sure the dataset is numerical 

traindf$Value = as.numeric(traindf$Value)

testdf$Value = as.numeric(testdf$Value)

predictdf$Value = as.numeric(predictdf$Value)

# combine back all data set

fulldf= rbind(traindf, testdf, predictdf)

# Summary #
summary(fulldf)


# Correlation check #
library(corrplot)

cont_var_check = function(x)
{
  return(is.numeric(x) | is.integer(x))
}

contvars= sapply(fulldf, cont_var_check)

contvars

corrdf = cor(fulldf[fulldf$Source == "Train", contvars])

windows()
corrplot(corrdf)

# Dummy variable Creation #
fvars = sapply(fulldf, is.factor)

fvars

dummydf =  model.matrix(~ .,  data = fulldf[ , fvars] )
View(dummydf)

dim(dummydf)
fulldf2 = cbind(fulldf[ ,!fvars], dummydf[ , -1 ]) 

View(fulldf2)

 #Dimension check#

dim(fulldf2)


# checking for numeric or integer type #
 str(fulldf2)
 
 
# Segregating the fulldf2 back to train and test data#
 
traindf    = subset(fulldf2, subset = fulldf2$Source == "Train", select = -Source)
 
testdf     = subset(fulldf2, subset = fulldf2$Source == "Test", select = -Source) 
 
predictdf  = subset(fulldf2, subset = fulldf2$Source == "Predict", select = -Source)  


# Muticollinearity check #

# Removing variables  with VIF>5

M1= lm(Value ~ ., data = traindf)

library(car)
vif(M1)

#  Aliased coefficents error #
alias(lm(Value ~ ., data = traindf))

vif(M1)
 # since the model shows perfect collinearity between billing rate & buyer rate#
 # removing one of the variables might better the prediction #


traindf$Billing.Rate.Sqm   = NULL
testdf$Billing.Rate.Sqm    = NULL
predictdf$Billing.Rate.Sqm = NULL

#checking of collinearity once again #

M2 = lm(Value ~ ., data = traindf)

vif(M2)
# since the model shows perfect collinearity between buyer rate & mrp.sqm#
# removing one of the variables might better the prediction #

traindf$MRP..Sqm    = NULL
testdf$MRP..Sqm     = NULL
predictdf$MRP..Sqm  = NULL 

#checking of collinearity once again #

M3 = lm(Value ~ ., data = traindf )

VIF(M3)

# since the model shows perfect collinearity between Ad6 & AD7 #
# removing one of the variables might better the prediction #


traindf$AD6.Sqm = NULL
testdf$AD6.Sqm = NULL
predictdf$AD6.Sqm = NULL

#checking of collinearity once again #

M4 = lm(Value ~ ., data = traindf )

VIF(M4)

# in sufficeint collapse of energy in finding aliased coefficients#
# Moving to the step function #

summary(M4)

M5 = step(M4)

# model   diagnostics #
# homoskadasticity check #

plot(M5$fitted.values,M5$residuals)

#Normality check

summary(M5$residuals)   # for range detection #

#plotting histogram #

hist(M5$residuals, breaks =  seq(-1400, 1600, 20))

#Prediction on test data #

M5_Pred = Predict(M5, testdf)

head(M5_Pred)


#Rmse Calculation #
Actual= testdf$Value
Prediction = M5_Pred

sqrt(mean((Actual - Prediction)^2))


#MApe

mean(abs((Actual - Prediction)/Actual))*100



#Predicting on predictdf

predictdf$Value = predict(M5, predictdf)


#Extracting the datafile
install.packages("writexl")
library("writexl")
write_xlsx(predictdf,"C:/Users/Byragi/Desktop/HACKATHON/predict.xlsx")
