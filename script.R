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
#setwd("C:/Users/mehme/Documents/GitHub/SpeedDating")
library(stringdist)

speed.dating <- read.csv("speeddating.csv", na.strings = c("", "?"))

### Fixing feature name typos and inconsistencies
names(speed.dating)[which(names(speed.dating) %in% c("sinsere_o", "ambitous_o", "d_sinsere_o", "d_ambitous_o", "ambtition_important", "d_ambtition_important"))] <- 
  c("sincere_o", "ambitious_o", "d_sincere_o", "d_ambitious_o", "ambition_important", "d_ambition_important")

### Missing data

summary(speed.dating)
summary(speed.dating[,sapply(speed.dating, anyNA)])

# These three are the same size, meaning those 79 observations have a lot of missing data.
nrow(speed.dating[is.na(speed.dating$sports),])
nrow(speed.dating[is.na(speed.dating$sports) & is.na(speed.dating$attractive),])
nrow(speed.dating[is.na(speed.dating$sports) & is.na(speed.dating$attractive) & is.na(speed.dating$attractive_important),])

# ELIMINATE!
speed.dating <- speed.dating[!(is.na(speed.dating$sports)),]
summary(speed.dating[,sapply(speed.dating, anyNA)])

# Still... seems some people refused to rate themselves (intelligence and so on) or to give their preferences (pref_o_attractive and so on)
# but those two groups do not seem to match, because the NAs of the first group do no show up in the summary below
summary(speed.dating[is.na(speed.dating$pref_o_sincere),])

# But having that complete group of features empty is a lot of data lost... DELETE!
speed.dating <- speed.dating[!(is.na(speed.dating$pref_o_sincere)),]

# Same goes for those that do not want to rate themselves, it is not fair, DEMOLISH!
summary(speed.dating[is.na(speed.dating$attractive),])
speed.dating <- speed.dating[!(is.na(speed.dating$attractive)),]

summary(speed.dating[,sapply(speed.dating, anyNA)])

# After these deletions, two groups of variables show similar patterns of missing data,
# age and age_o have the same amount of NAs but
speed.dating[is.na(speed.dating$age_o), c("d_age", "d_d_age")]
# but it seems that even though the age is unknown, the age difference is known, which could still be useful.
# All of the missing values for age are > 18, interesting!
# Relationships with such age gap are indeed socially frowned upon, so... Understandable

# Finally, there seems to be a last pattern... 
summary(speed.dating[,sapply(speed.dating, anyNA)])
# The groups of variables that show the mutual ratings between partners (as with attractive_o and attractive_partner)...
# They seem to show a similar amount of missing values
nrow(speed.dating[is.na(speed.dating$attractive_o),])
nrow(speed.dating[is.na(speed.dating$attractive_o) & is.na(speed.dating$attractive_partner),])
summary(speed.dating[!(is.na(speed.dating$attractive_o)),])
summary(speed.dating[!(is.na(speed.dating$sinsere_o)),])
# But, in the end, it looks it was just coincidence

# Okay, finally those that declined to respond to any given block can also eff off. These two blocks below seem fine.
table(apply(is.na(speed.dating[, 16:21]), 1, all))
table(apply(is.na(speed.dating[, 22:27]), 1, all))

# Oh, the next one has 176 matches!
table(apply(is.na(speed.dating[, 28:33]), 1, all))
# It also seems that, for those subjects with missing values for features 28 to 33, equivalent categorical features
# (those from 34 to 39) do not have any meaningful data either.
unique(speed.dating[which(apply(is.na(speed.dating[, 28:33]), 1, all)),34:39])
# Those columns were probably automatically assigned because of the missing value. We better delete these guys.
speed.dating <- speed.dating[-which(apply(is.na(speed.dating[, 28:33]), 1, all)),]

# These groups are not completely empty...
table(apply(is.na(speed.dating[, 34:39]), 1, all))
table(apply(is.na(speed.dating[, 40:45]), 1, all))
table(apply(is.na(speed.dating[, 52:56]), 1, all))

# This one has another 55!
table(apply(is.na(speed.dating[, 62:67]), 1, all))
# And... the same thing as before happens, in which the categorical equivalent features do not hold any information of value
unique(speed.dating[which(apply(is.na(speed.dating[, 62:67]), 1, all)),68:73])
# Out they go
speed.dating <- speed.dating[-which(apply(is.na(speed.dating[, 62:67]), 1, all)),]

# And finally, no one declined to responde here. So, the cleansening is done
table(apply(is.na(speed.dating[, 74:90]), 1, all))

### Value coherence

# The variables of the summary below are scores out of 10, but they all have values over 10.
summary(speed.dating[,c("gaming", "funny_o", "attractive_o", "reading")])

# Specifically, for feature "gaming", it seems like there are at least five differentiable subjects that graded their
# interest in gaming as a 14 out of 10. It's the same five guys but
# What's wrong with them?
unique(speed.dating[!(is.na(speed.dating$gaming)) &speed.dating$gaming > 10, c(74:90, 98)])
# Anyway, seems like categorical feature "d_gaming" groups them into category "[9-10]"
unique(speed.dating[!(is.na(speed.dating$funny_o)) & speed.dating$funny_o > 10, c(28:33, 37)])
# These guys are also categorized the same way.
unique(speed.dating[!(is.na(speed.dating$attractive_o)) & speed.dating$attractive_o > 10, c(28:33, 34)])
unique(speed.dating[!(is.na(speed.dating$reading)) & speed.dating$reading > 10, c(74:90, 100)])

# Let's just reassign all of them to upper bound value 10.
speed.dating$gaming[!(is.na(speed.dating$gaming)) & speed.dating$gaming > 10] <- 10
speed.dating$funny_o[!(is.na(speed.dating$funny_o)) & speed.dating$funny_o > 10] <- 10
speed.dating$attractive_o[!(is.na(speed.dating$attractive_o)) & speed.dating$attractive_o > 10] <- 10
speed.dating$reading[!(is.na(speed.dating$reading)) & speed.dating$reading > 10] <- 10

# Now the out of 100 scores need to be normalized. To do so, we take the highest score and treat it as the maximum score.
# Then, normalize from it. This function will do it for us.
# Some of the subjects, though, did not distribute completely the 100 points. These scores will be penalized.
norm.score <- function(row) {
  m <- max(row)
  s <- sum(row)
  lapply(row, function(x) 10 * ifelse(is.na(x), 0, x) / (m * (1 + (100 - s) / 100)))
}
# This needs to be applied for column blocks "_important" and "pref_o_", corresponding to indexes 16:21 and 40:45
for (i in 1:nrow(speed.dating)) {
  speed.dating[i,16:21] <- norm.score(speed.dating[i, 16:21])
  speed.dating[i,40:45] <- norm.score(speed.dating[i, 40:45])
}

# Now, looking at the different possible values of "field", it's a mess!
# The categories with more than one word have quotation marks, and many possible catgories might refer to the same field.
# Some have typos, differences between single and double quotation marks, lower vs upper case and whatnot.
levels(speed.dating$field) <- tolower(levels(speed.dating$field))
levels(speed.dating$field) <- ifelse(grepl("'", levels(speed.dating$field)), substr(levels(speed.dating$field), 2, nchar(levels(speed.dating$field)) - 1), levels(speed.dating$field))

table(speed.dating$field)
# We take all of the current different fields and store them apart
field.data <- levels(speed.dating$field)

# Now we dive into the fields: We are trying to find out the categories closest to each other according to
# the Levenshtein edit distance between them (with a max distance of 3). Then, we compare the found matches.
for (i in 1:length(field.data)) {
  (flag <- amatch(field.data[i], field.data[-(1:i)], maxDist=3, method = "lv"))
  #if (!is.na(flag)) field.data[i] <- ifelse(i <= flag, field.data[flag+1], field.data[flag])
  if (!is.na(flag)) field.data[i] <- field.data[i + flag]
}

# We also tried with different max distances like 2 or 5. 
# We have found that, to identify typos, a max distance of 3 is the most appropriate.
cbind(original=levels(speed.dating$field), match=field.data)[which(levels(speed.dating$field) != field.data),]
# Still, "biology" and "ecology" should not be considered a match (as far as we are concerned)
# Now, we need to (unfortunately) *manually* choose the correct categories
#field.data[which(field.data=="ecology" & levels(speed.dating$field) != field.data)] <- "biology"
for (i in c(4, 6, 7, 14)) {
  original.value <- levels(speed.dating$field)[which(levels(speed.dating$field) != field.data)][i]
  match.value <- field.data[which(levels(speed.dating$field) != field.data)][i]
  # if the match value is preferred...
  speed.dating$field[which(original.value == speed.dating$field)] <- match.value
}

# And yet, after all of this procedure, we can see that some categories are too similar...
# There is too much ambiguity in the categories. Let's just scrap the feature.
speed.dating$field <- NULL


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






