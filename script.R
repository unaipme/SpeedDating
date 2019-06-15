### Some values of variable "field" are the same but written differently. Like:
###   Business- MBA and Business [MBA], ELECTRICAL ENGINEERING and electrical engineering
###   Finance and Finanace
###   Basically, typos, case mismatches
### Question marks (?) are used for missing values. There are no explicit NAs or empty cells.
### (Single) Quotation marks should be removed

#### Treatment of lost values

### 

#### treatmeant of anomalous values

#### treatment of incoherent or incorrect values

#### elimination of irrelevant variables

#### (possible) elimination of redundant variables

#### coding of non-continuous or non-ordered variables (nominal or binary)

#### extraction of new variables that can be useful

#### normalization of the variables (e.g. standardization)

#### transformation of the variables (e.g. correction of skewness and/or kurtosis)


##### As initial procedure, and seeing that most of the features are categorical (factor) or booleans (logical),
#####   Naïve Bayes classifier can be applied to see something. A generalized linear model can be also used to
#####   find the correlations between features and outcome.

#we should start with converting the similar information into 1 consistent shape.
setwd("C:/Users/mehme/Documents/GitHub/SpeedDating")

speed.dating <- read.csv("speeddating.csv")

##SOLVING THE FIELD COLUMN PROBLEM##

#al the unique rows for field column, transformed into a dataframe named field.data
field.column <- unique(speed.dating$field)
field.data <- data.frame(field.column)

#This fcn returns the index of the most similar string if there is any with "maxDist" less than n 
library(stringdist)
ClosestMatch = function(string, stringVector, n = 3){
  flag <- amatch(string, stringVector, maxDist = n)
  if ( !is.na(flag)){
    flag
  }
  else
    -1
}

#dive into the whole field.data and apply the function to every row
for ( i in 1:nrow(field.data)){
  flag <- ClosestMatch(field.data[i,], field.data[-i,])
  if ( flag > 0){
    if ( i <= flag ) #if the found index is bigger than current row's index, increase the found(we discard the current row, so it decreases the no of rows by 1; we need to increase it again)
      field.data[i,] = field.data[flag+1,]
    else
      field.data[i,] = field.data[flag,]
  }
}

#discard the repeating ones
field.data <- unique(field.data)

# TODO: if you think this is fine, we can do following,
# for each row at speed.dating$field, do ClosestMatch with new obtained field.data;
# but I think it would be better if we discard this column entirely.
# Diversity may be too large to obtain any accurate/meaningful results. We have fields like
# Psychology, Educational Psychology, Organizational Psychology, Clinical Psychology etc
# We dont know how strongly connected these types and counting them as one is not a good way to
# handle the situation probably. 


##After that mess with fields is done..

colnames(datingData)
head(datingData)
for (i in 1:ncol(datingData)) { #Missing Values was representing as ? and it was converted as NA 
  if(length(which(datingData[,i] == "?"))>0) datingData[which(datingData[,i] == "?"),i] = NA 
} 
summary(datingData)
str(datingData) 
# TODO: We need to make a decision between factor and interger for variable types.

#Data Transformation
datingData <- datingData %>% mutate(has_null = as.factor(has_null), ##I "assumed" age should be integer, 
                                   age = as.integer(age))

#Creating Numeric Dataset
datingDataNum <- data.frame(datingData$d_age, datingData$age)

#Missing Value Detection
aggr_plot <- aggr(datingData, col=c('green','orange'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(datingData), cex.axis=.5, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

#Missing Value Imputation for Numeric Variables by MICE
imputed_Data <- mice(datingDataNum, m=5, maxit = 50, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,2)
sum(is.na(completeData))
datingData$age = completeData$datingData.age
datingDataNum$age = completeData$datingData.age
sum(is.na(datingDataNum))

#Missing Value Imputation for Categoric Variables by Random Forest
dd = datingData
dataFac=dd %>% mutate_if(is.character, as.factor)
mfImp = missForest(dataFac);
datingImp <- mfImp$ximp
sum(is.na(datingImp))

#Outlier Detection
#Detect multivariate outliers via Mahalanobis distance
#Outlier detection holds for numerical values only
outliers <- Moutlier(datingDataNum, quantile = 0.975, plot = TRUE)
Classic.Mahalanobis <- table(factor(outliers$md > outliers$cutoff, labels=c("Not Outlier", "Outlier")))
Robust.Mahalanobis <- table(factor(outliers$rd > outliers$cutoff, labels=c("Not Outlier", "Outlier")))
c <- rbind(Classic.Mahalanobis, Robust.Mahalanobis) 
c

#table(c) # TRUE if outlier
plot(outliers$rd ~ outliers$md, main="Robust against classical Mahalanobis distances", cex.main=0.8, xlab="Classic Mahalanobis", ylab="Robust Mahalanobis", cex.lab=0.8)
abline(h=outliers$cutoff, col="red") abline(v=outliers$cutoff, col="red")

# Remove multivariate Mahalanobis outliers
datingDataNum_mout <- datingDataNum[outliers$md < outliers$cutoff & outliers$rb < outliers$cutoff,] # See which instances are outliers
datingDataNum[outliers$md >= outliers$cutoff & outliers$rd >= outliers$cutoff,]

#put data to their original location for all the variables
datingData$d_age <- DatingDataNum$datingData.d_age






