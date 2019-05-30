speed.dating <- read.csv("speeddating.csv")
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

