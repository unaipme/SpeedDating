library(stringdist)
library(mice)
library(chemometrics)
library(nnet)
library(caret)
library(FactoMineR)
library(shape)
library(e1071)
library(fpc)
library(cclust)
library(randomForest)

speed.dating <- read.csv("speeddating.csv", na.strings = c("", "?"))

### Fixing feature name typos and inconsistencies
names(speed.dating)[which(names(speed.dating) %in% c("sinsere_o", "ambitous_o", "d_sinsere_o", "d_ambitous_o", "ambtition_important", "d_ambtition_important"))] <- 
  c("sincere_o", "ambitious_o", "d_sincere_o", "d_ambitious_o", "ambition_important", "d_ambition_important")

### MISSING DATA

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

# And finally, no one declined to respond here. So, the cleansening is done
table(apply(is.na(speed.dating[, 74:90]), 1, all))

# Features representing scores from 0 to 10 have NA values. This is the case for blocks "_partner" and "_o".
# For example, "shared_interests_partner" has the largest amount of NAs among these, 861.
# The thing is, the equivalent categorical value of these features do not have any missing value.
unique(speed.dating[is.na(speed.dating$shared_interests_partner),c("d_shared_interests_partner")])
# Moreover, all of the NA values are assigned to group "[0-5]". Instead of performing imputation on these,
# we are filling these values with 0. We think that, if subjects did not want to waste any time filling this question,
# they must really not care about these criteria.
# The same thing can be applied to scores out of 100, "pref_o" and "_important" blocks.
for (i in c(16:21, 28:33, 40:45, 62:67)) {
  speed.dating[is.na(speed.dating[,i]), i] <- 0
}
# The rest of the columns with missing values will be imputed later on

### VARIABLE COHERENCE AND TYPING

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

# Change of topic. There is two blocks of features that represent how the participants distribute a 100 points
# among their preferences. But there seems to be some smart asses between the participants.
# There are, exactly, 123 of these.
length(which(floor(apply(speed.dating[,16:21], 1, sum)) > 100)) + 
  length(which(floor(apply(speed.dating[,40:45], 1, sum)) > 100))
# We use floor because some of these exceed the 100 points by just decimals, which can happen if you try to distribute
# the 100 points in fractions. That's okay. We decide to remove the rest of these guys to avoid problems.
# But not before seeing what their field of study is:
unique(speed.dating[c(which(floor(apply(speed.dating[,16:21], 1, sum)) > 100), which(floor(apply(speed.dating[,40:45], 1, sum)) > 100)),"field"])
# Some mathematicians and engineers there... The future is bright if they do not know how to add up to 100.
speed.dating <- 
  speed.dating[-c(which(floor(apply(speed.dating[,16:21], 1, sum)) > 100), which(floor(apply(speed.dating[,40:45], 1, sum)) > 100)),]
# Now, to avoid future problems with those people that exceed the 100 points by some digits,
speed.dating[which(apply(speed.dating[,16:21], 1, sum) > 100),16:21] <- speed.dating[which(apply(speed.dating[,16:21], 1, sum) > 100),16:21] - ((apply(speed.dating[which(apply(speed.dating[,16:21], 1, sum) > 100),16:21], 1, sum) - 100) / 6)
speed.dating[which(apply(speed.dating[,40:45], 1, sum) > 100),40:45] <- speed.dating[which(apply(speed.dating[,40:45], 1, sum) > 100),40:45] - ((apply(speed.dating[which(apply(speed.dating[,40:45], 1, sum) > 100),40:45], 1, sum) - 100) / 6)

# Before continuing to feature selection and extraction, let's see if all features are correctly typed
sapply(speed.dating, class)
# And we think not. Let's fix this.
speed.dating$has_null <- as.factor(speed.dating$has_null)
speed.dating$wave <- as.factor(speed.dating$wave)
speed.dating$samerace <- as.factor(speed.dating$samerace)
speed.dating$decision <- as.factor(speed.dating$decision)
speed.dating$decision_o <- as.factor(speed.dating$decision_o)
speed.dating$match <- as.factor(speed.dating$match)

### FEATURE EXTRACTION / SELECTION

# Now the out of 100 scores need to be normalized. To do so, we take the highest score and treat it as the maximum score.
# Then, normalize from it. This function will do it for us.
# Some of the subjects, though, did not distribute completely the 100 points. These scores will be penalized.
norm.score <- function(row) {
  m <- max(row, na.rm = T)
  s <- sum(row, na.rm=T)
  lapply(row, function(x) 10 * ifelse(is.na(x), 0, x) / (m * (1 + (100 - s) / 100)))
}
# This needs to be applied for column blocks "_important" and "pref_o_", corresponding to indexes 16:21 and 40:45
for (i in 1:nrow(speed.dating)) {
  speed.dating[i,16:21] <- norm.score(speed.dating[i, 16:21])
  speed.dating[i,40:45] <- norm.score(speed.dating[i, 40:45])
}
# Let's give corresponding new values to the equivalent categorical features
for (i in c(16:21, 40:45)) {
  speed.dating[,i+6] <- cut(speed.dating[,i], breaks=c(0,5,8,10), include.lowest = T, labels=c("[0-5]","[6-8]","[9-10]"))
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

# We are also getting rid of these features. We don't need "age", "age_o" or "d_age" if we have "d_d_age", which
# we find much more representative. And "expected_num_interested_in_me" has waaaay too many NA's.
# Also, we don't really care about `met`, `decision` or `decision_o`
speed.dating$age <- NULL
speed.dating$age_o <- NULL
speed.dating$d_age <- NULL
speed.dating$expected_num_interested_in_me <- NULL
speed.dating$met <- NULL
speed.dating$decision <- NULL
speed.dating$decision_o <- NULL
speed.dating$importance_same_religion <- NULL
speed.dating$d_importance_same_religion <- NULL
speed.dating$has_null <- NULL

### DATA IMPUTATION

# There are only 5 columns with NAs left
which(sapply(speed.dating, anyNA))

# After deleting all useless features and instances, let's impute the missing values. First, we use MICE.
# MICE performs mixed imputation (mixed variables types) and is not determinist.
mice.speed.dating <- mice(speed.dating[,c(which(sapply(speed.dating, anyNA)), 37:42, 47:52)], method = 'pmm', seed = 500)
mice.speed.dating.complete <- complete(mice.speed.dating)
mice.speed.dating.complete[apply(speed.dating, 1, anyNA),]
# The imputed values seem right to us
speed.dating[, which(sapply(speed.dating, anyNA))] <- mice.speed.dating.complete[,1:length(which(sapply(speed.dating, anyNA)))]

### OUTLIER DETECTION

# Detect multivariate outliers via Mahalanobis distance
# Outlier detection holds for numerical values only
dating.outliers <- Moutlier(scale(speed.dating[,sapply(speed.dating, is.numeric)]), plot=F, quantile = 0.975)
par(mfrow=c(1,2))
plot(dating.outliers$md, main="Mahalanobis Distance", xlab="Distance", ylab="Index", col=ifelse(speed.dating$match == "1", "orangered1", "royalblue1"), las=1, cex.axis=.75, pch=20, ylim=c(0, max(dating.outliers$rd)), xaxt="n")
plot(dating.outliers$rd, main="Robustified Mahalanobis", xlab="Distance", ylab="Index", col=ifelse(speed.dating$match == "1", "orangered1", "royalblue1"), las=1, cex.axis=.75, pch=20, ylim=c(0, max(dating.outliers$rd)), xaxt="n")
cutoff <- sort(dating.outliers$rd, decreasing = T)[floor(nrow(speed.dating) * 0.025)]
abline(h = cutoff, lty=2, col="red4", lwd=2.25)
par(mfrow=c(1,1))

speed.dating <- speed.dating[-order(dating.outliers$rd, decreasing = T)[1:floor(nrow(speed.dating) * 0.025)],]

#Classic.Mahalanobis <- table(factor(dating.outliers$md > dating.outliers$cutoff, labels=c("Not Outlier", "Outlier")))
#Robust.Mahalanobis <- table(factor(dating.outliers$rd > dating.outliers$cutoff, labels=c("Not Outlier", "Outlier")))
#c <- rbind(Classic.Mahalanobis, Robust.Mahalanobis) 

#table(c) # TRUE if outlier
#plot(dating.outliers$rd ~ dating.outliers$md, main="Robust against classical Mahalanobis distances", cex.main=0.8, xlab="Classic Mahalanobis", ylab="Robust Mahalanobis", cex.lab=0.8)
#abline(h=dating.outliers$cutoff, col="red")
#abline(v=dating.outliers$cutoff, col="red")

# Remove multivariate Mahalanobis outliers
#datingDataNum_mout <- datingDataNum[outliers$md < outliers$cutoff & outliers$rb < outliers$cutoff,] # See which instances are outliers
#datingDataNum[outliers$md >= outliers$cutoff & outliers$rd >= outliers$cutoff,]

### EXPLORATORY DATA ANALYSIS (VISUALIZATION)

# Okay, for PCA we are just taking the all the numeric features available (we are not going to just omit features from this),
# and also the "match" feature as qualitative supplementary variable.
speed.dating.pca <- speed.dating[,c(which(sapply(speed.dating, is.numeric)), 112)]
# For some reason, the "scale" function fails. Then we will center the data OUR way.
#speed.dating.pca[,-53] <- scale(speed.dating[,-53])
for (i in 1:52) {
  speed.dating.pca[,i] <- speed.dating.pca[,i] - mean(speed.dating.pca[,i])
}

par(mfrow=c(1,2))
pca.output <- PCA(speed.dating.pca, quali.sup = c(53), graph = T)
par(mfrow=c(1,1))
# The hell are those graphs! Let me make my own plots...

# There are too many explanatory variables, so we are going to take those that have the most representation in the 2 first dimensions.
pca.best.repr.vars <- order(sqrt(pca.output$var$coord[,1]^2 + pca.output$var$coord[,2] ^ 2), decreasing = T)[1:10]

# And plot them!
plot(pca.output$var$coord, col=0, xlim=c(-2.75,1.75), ylim=c(-1,1))
abline(h=0, col="gray")
abline(v=0, col="gray")
plotellipse(1, 1)
Arrows(0, 0, pca.output$var$coord[pca.best.repr.vars,1], pca.output$var$coord[pca.best.repr.vars,2], arr.type = "simple", col=rainbow(10))
legend("topleft", legend=row.names(pca.output$var$coord[pca.best.repr.vars,]), col=rainbow(10), lty=1)

# The first plot shows that the first factorial plane is not really able to represent too much of the
# inertia of the variables. Anyway, the 10 most represented variables are somewhat mostly influenced by
# the first dimension

plot(pca.output$ind$coord, col=0)
abline(h=0, col="gray", lwd=2)
abline(v=0, col="gray", lwd=2)
grid()
points(pca.output$ind$coord, col="royalblue1", pch=20)

points(pca.output$quali.sup$coord, col="orangered1", pch=20)
text(pca.output$quali.sup$coord[,1], pca.output$quali.sup$coord[,2] + .8, col="orangered1", labels = c("no match", "match"))

# And this plot shows us that the point cloud is all over the place. They do not seem to follow a specific
# distribution, less so the supplementary qualitative variables, which do not seem to be tied to any dimension
# particularly. Just the opposite, they are pretty much centered...

# The data points all over the place and visually we are not able to identify any cluster or anything.
# Probabilistic clustering methods will probably give nothing of value. But let's give a quick try to kmeans.

par(mfrow=c(2,5), mar=c(1,0,1,0))
for (k in 2:11) {
  kmeans.res <- cclust(pca.output$ind$coord, centers=k, method = "kmeans")
  kmeans.ch <- calinhara(pca.output$ind$coord, kmeans.res$cluster)
  plot(pca.output$ind$coord, col=0, xaxt="n", yaxt="n", xlab="", ylab="", main=paste("k=", k, " (CH=", round(kmeans.ch, 2), ")", sep = ""))
  points(pca.output$ind$coord, col=rainbow(k)[kmeans.res$cluster])
}
par(mfrow=c(1,1), mar=mar)

# And, indeed, the more clusters we add, the less uniform they are and the less they score using the 
# Calinksi-Harabaszz score. Still, due to the limitations of FactoMineR's PCA, it only calculates the 
# representation of the first 5 dimensions (unless otherwise specified). The first 5 dimensions only
# represent up to this percentage of the total inertia:
sum(pca.output$eig[1:5,2])
# It is common to accept only dimensions that represent up to the 80% of the inertia.
# So it's understandable that results are poor. Let's try hierarchical clustering too.

speed.dist <- dist(pca.output$ind$coord)
speed.hclust <- hclust(d = speed.dist)
speed.hclust.ch.scores <- rep(NA, 5)
for (k in 2:6) {
  speed.hclust.tree <- cutree(speed.hclust, k=k)
  speed.hclust.ch.scores[k-1] <- calinhara(x = pca.output$ind$coord, clustering = speed.hclust.tree)
  factoextra::fviz_cluster(list(data = data.frame(pca.output$ind$coord), cluster = speed.hclust.tree),
                           ellipse.type = "norm", geom = "point", stand = FALSE, main = "", legend=NULL)
}
speed.hclust.ch.scores
# More of the same. Is this dataset "inclusterable"?

### ARTIFICIAL NEURAL NETWORKS

N <- nrow(speed.dating)

learn <- sample(1:N, round(2 * N / 3))

nlearn <- length(learn)
ntest <- N - nlearn

speed.dating.cat <- speed.dating[,sapply(speed.dating, is.factor)]
speed.dating.cont <- data.frame(scale(speed.dating[,sapply(speed.dating, is.numeric)], scale=F), match=speed.dating$match)


trControl <- trainControl(method="cv", number=10)
multinom.10x10 <- train(match ~ ., data=speed.dating.cont, subset=learn, method='multinom', trControl=trControl, maxit=200)

prediction <- predict(multinom.10x10$finalModel, newdata=speed.dating[-learn,], type="class")
#cbind(prediction, speed.dating$match[-learn])
table(prediction, original=speed.dating$match[-learn])

#model.nnet0 <- multinom(match ~ ., data=speed.dating.cat, subset=learn, maxit=200)

#model.nnet0 <- multinom(match ~ d_d_age + samerace + d_pref_o_attractive + d_pref_o_sincere + d_pref_o_intelligence +
 #                         d_pref_o_funny + d_pref_o_ambitious + d_pref_o_shared_interests, data=speed.dating, subset=learn, maxit=200)

prediction <- predict(model.nnet0, newdata=speed.dating[-learn,], type="class")
cbind(prediction, speed.dating$match[-learn])
table(prediction, speed.dating$match[-learn])

#model.nnet0 <- multinom(match ~ , data=speed.dating, subset=learn, maxit=200)

### BAYES

##Two ways: i)ignore continuououso variables ii)handle them.
##We decided to ignore them.

bayes.model <- naiveBayes(match ~ ., data=speed.dating.cat[learn,])

#the apparent(training) error
bayes.training <- predict(bayes.model, newdata=speed.dating.cat[learn,-115]) #115th col is the match
confusionMatrix(bayes.prediction, speed.dating.cat$match[-learn])

#test(prediction) error
bayes.prediction <- predict(bayes.model, newdata=speed.dating.cat[-learn,-115])
confusionMatrix(bayes.prediction, speed.dating.cat$match[-learn])

### KNN 
#the categorical data -- not sure about this, it works without any errors but should we do knn for categorical?
##Sampling
set.seed(333)
cat.index <- createDataPartition( y =speed.dating.cat$match, p= 0.75, list= FALSE)
cat.training <- speed.dating.cat[cat.index,]
cat.test <- speed.dating.cat[-cat.index,]

prop.table(table(cat.training$match)) * 100
prop.table(table(speed.dating.cat$match)) * 100

#knn reqs variables to be normalized and/or scaled. Lets go with centralizing and scaling
#cat.train <- cat.training[,names(cat.training) != "match"]
#centering and scaling is not useful for categorical variables
#cat.processed <- preProcess(x = cat.train, method = c("conditionalX"))

##Training & Control
#define the control
cat.control <- trainControl(method="cv", number = 5)
catFit <- train(match ~ ., data= cat.training, method= "knn", trControl= cat.control, preProcess= c("center", "scale"), tuneLength = 20)
catFit
#Couldnt reach this far but this should give the number of neighbours vs accuracy
plot(catFit) 

catPredict <- predict(catFit, newdata = cat.test)
confusionMatrix(catPredict, cat.test$match)
mean(catPredict == cat.test$match)

#the continuousususosus data -- works properly
##Sampling
set.seed(333)
cont.index <- createDataPartition( y =speed.dating.cont$match, p= 0.75, list= FALSE)
cont.training <- speed.dating.cont[cont.index,]
cont.test <- speed.dating.cont[-cont.index,]

prop.table(table(cont.training$match)) * 100
prop.table(table(speed.dating.cont$match)) * 100

#knn reqs variables to be normalized and/or scaled. Lets go with centralizing and scaling
#cont.train <- cont.training[,names(cont.training) != "match"]
#cont.processed <- preProcess(x = cont.train, method = c("center", "scale"))

##Training & Control
#define the control
cont.control <- trainControl(method="cv", number = 5)
contFit <- train(match ~ ., data= cont.training, method= "knn", trControl= cont.control, preProcess= c("center", "scale"), tuneLength = 20)
contFit
#Couldnt reach this far but this should give the number of neighbours vs accuracy
plot(contFit) 

contPredict <- predict(contFit, newdata = cont.test)
confusionMatrix(contPredict, cont.test$match)
mean(contPredict == cont.test$match)

###Random Forest

set.seed(666)
rf.model1 <- randomForest( match ~ ., data = speed.dating[learn,], ntree = 100, proximity=FALSE)
rf.model1

#14.83% OOB test error

rf.pred1 <- predict(rf.model1, speed.dating[-learn,], type="class")

ct <- table(Truth=speed.dating[-learn,]$match, Pred = rf.pred1)
#percentage by class
prop.table(ct ,1)
#total percentage corrent
sum(diag(ct))/sum(ct)

# TODO: try to decrease the oob error with trying different vals for mtry and ntree
#mtry = number of variables randomly sampled as candidates at each split, default is sqrt(nlearn)
rf.model2 <- randomForest( match ~ ., data = speed.dating[learn,], ntree = 200, mtry= (sqrt(nlearn))*3,proximity=FALSE, strata=speed.dating[learn,]$match)
rf.model2

rf.pred2 <- predict(rf.model2, speed.dating[-learn,], type="class")

ct <- table(Truth=speed.dating[-learn,]$match, Pred = rf.pred1)
#percentage by class
prop.table(ct ,1)
#total percentage corrent
sum(diag(ct))/sum(ct)
