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
library(RColorBrewer)

set.seed(666)

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
#features.with.NA <- which(sapply(speed.dating, anyNA))
mice.speed.dating <- mice(speed.dating[,c(which(sapply(speed.dating, anyNA)), 37:42, 47:52)], method = 'pmm')
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
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))

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
# More of the same. Is this dataset "inclusterable"? Is it just one big cluster?

### MACHINE LEARNING oh boy here we go

# Let's create the training and the test sets. The first one will be two thirds of the dataset and the latter one third.
N <- nrow(speed.dating)

learn <- sample(1:N, round(2 * N / 3))

nlearn <- length(learn)
ntest <- N - nlearn

# Now let's also split the dataset into two subsets, one with the categorical variables and the other with the
# continuous variables (plus the target variable)
speed.dating.cat <- speed.dating[,sapply(speed.dating, is.factor)]
speed.dating.cont <- data.frame(scale(speed.dating[,sapply(speed.dating, is.numeric)], scale=F), match=speed.dating$match)

# These variables will be used for as the cross-validation configuration
trc.5CV <- trainControl(method="cv", number = 5)
trc.10CV <- trainControl(method="cv", number = 10)

### ARTIFICIAL NEURAL NETWORKS

# This takes looooong, you can otherwise just run the "readRDS" function if the training has been done before.
# It is commented so that it is not run unintentionally.

#########################################################################################################################
# nnet.train.k <- train(match ~ ., data=speed.dating.cont, subset = learn, method="nnet", trControl=trc.5CV, maxit=400, #
#                      tuneGrid = expand.grid(size = 3:16, decay = seq(from=0.1, to=0.5, by=0.1)))                      #
#########################################################################################################################
nnet.train.k <- readRDS("nnet-train-k-results")

# Let us save it for the report...
saveRDS(nnet.train.k, file="nnet-train-k-results")

# This plot will show us the evolution of the accuracy of the models along the x-axis that represents the size of the
# hidden layer, depending on the decay rate.
plot(nnet.train.k$results[,3], col=0, xlim=c(3,16), las=1, xaxt="n")
axis(1, at=3:16)
grid()
for (decay in seq(from=0.1, to=0.5, by=0.1)) {
  points(nnet.train.k$results[nnet.train.k$results[,2] == decay, c(1,3)], col=brewer.pal(5, "Accent")[decay * 10], type="o", pch=20)
}
legend("bottomleft", legend=paste("decay =", seq(from=0.1, to=0.5, by=0.1)), col=brewer.pal(5, "Accent"), lty=1)
# The best model turns out to be the one with 4 neurons in the hidden layer and a decay of 0.2

# And, still, the best model has a pretty low balanced accuracy. Look at that NPV, it's below 50%
prediction <- predict(nnet.train.k$finalModel, newdata = speed.dating.cont[-learn,], type = "class")
confusionMatrix(table(prediction, speed.dating.cont$match[-learn]))

### RANDOM FORESTS

# We are going to build two random forests, one with the continuous features and the other with the categorical ones
rf.model1.cont <- randomForest(match ~ ., data = speed.dating.cont[learn,], ntree = 100, proximity=FALSE)
rf.model1.cat <- randomForest(match ~ ., data = speed.dating.cat[learn,], ntree = 100, proximity=FALSE)

# These are the confusion matrices for each of the forests. As could be expected, the one with the continuous variables
# is better, but only *slightly*.
rf.model1.cont$confusion
rf.model1.cat$confusion

# Let's see how the prediction works.
rf.pred1.cont <- predict(rf.model1.cont, speed.dating.cont[-learn,], type="class")
rf.pred1.cat <- predict(rf.model1.cat, speed.dating.cat[-learn,], type="class")

conf.matr.cont <- table(rf.pred1.cont, speed.dating.cont$match[-learn])
conf.matr.cat <- table(rf.pred1.cat, speed.dating.cat$match[-learn])

# And these are the percentages of well guessed predictions for each of those.
sum(diag(conf.matr.cont))/sum(conf.matr.cont)
sum(diag(conf.matr.cat))/sum(conf.matr.cat)

# Now, we are going to try parametrizing and cross-validating the previous process, seeing that
# using the continuous variables gives better results

best.rf.model <- NA
accuracies <- rep(NA, (225 - 75) / 25)

# This takes long. Not as long as the ANN, but still a bit long. Let's load the pre-saved file.

###############################################################################################################################
# mtry <- floor(sqrt(ncol(speed.dating.cat)) * 3)                                                                             #
# for (ntree in seq(from = 75, to = 225, by = 25)) {                                                                          #
#   print(paste("ntree =", ntree))                                                                                            #
#   random.tree <- train(match ~ ., data = speed.dating.cont, subset = learn, method="rf", proximity = F, trControl=trc.10CV, #
#                        tuneGrid = expand.grid(mtry = mtry), ntree = ntree)                                                  #
#   accuracies[((ntree - 75) / 25) + 1] <- random.tree$results$Accuracy                                                       #
#   if (!is.na(best.rf.model)) {                                                                                              #
#     if (random.tree$results$Accuracy > best.rf.model$results$Accuracy) {                                                    #
#       best.rf.model <- random.tree                                                                                          #
#     }                                                                                                                       #
#   } else {                                                                                                                  #
#     best.rf.model <- random.tree                                                                                            #
#   }                                                                                                                         #
# }                                                                                                                           #
###############################################################################################################################

best.rf.model <- readRDS("best-rf-model")
accuracies <- readRDS("rf-accuracies")

# Save the results
saveRDS(best.rf.model, "best-rf-model")
saveRDS(accuracies, "rf-accuracies")

# And now, in the plot we can see that 175 is the amount of trees the performs the best, but by little difference
plot(accuracies, xaxt="n", type="o", pch=20, ylim=c(.82,.9), col="royalblue1", ylab="Accuracy", xlab="ntree", las=1)
grid()
axis(1, at=(seq(from = 75, to = 225, by = 25) - 75) / 25 + 1, labels=seq(from = 75, to = 225, by = 25))
points(x = which(accuracies == best.rf.model$results$Accuracy), y = best.rf.model$results$Accuracy, col="orangered1")

best.rf.model$finalModel

# The barplot below will show the most important features of the forest.
rf.imp.barplot <- barplot(sort(best.rf.model$finalModel$importance, decreasing = T)[1:10], col=rev(brewer.pal(10, "RdYlGn")))
text(rf.imp.barplot, y = 1, srt=90, labels=names(speed.dating.cont)[order(best.rf.model$finalModel$importance, decreasing = T)][1:10],
     xpd=T, adj = 0)
# And... who would have guessed? "like" is the most important feature (with a significative gap)
# Looking back at the meaning of "like", it means whether the subject liked (and, we mean, **just liked**) their partner.
# Obviously if one of the subjects likes the other there ir a high probability that there ends up being a match.
# But we can say "like" is an a posteriori, summarizing variable. What does "liking" mean? How does it translate to
# the other characteristics of the subject?


best.rf.model.2 <- NA
accuracies.2 <- rep(NA, (225 - 75) / 25)
#####################################################################################################################################
# mtry <- floor(sqrt(ncol(speed.dating.cat) - 1) * 3)                                                                               #
# for (ntree in seq(from = 75, to = 225, by = 25)) {                                                                                #
#   random.tree <- train(match ~ ., data = speed.dating.cont[,-51], subset = learn, method="rf", proximity = F, trControl=trc.10CV, #
#                        tuneGrid = expand.grid(mtry = mtry), ntree = ntree)                                                        #
#   accuracies.2[((ntree - 75) / 25) + 1] <- random.tree$results$Accuracy                                                           #
#   if (!is.na(best.rf.model.2)) {                                                                                                  #
#     if (random.tree$results$Accuracy > best.rf.model.2$results$Accuracy) {                                                        #
#       best.rf.model.2 <- random.tree                                                                                              #
#     }                                                                                                                             #
#   } else {                                                                                                                        #
#     best.rf.model.2 <- random.tree                                                                                                #
#   }                                                                                                                               #
# }                                                                                                                                 #
#####################################################################################################################################

best.rf.model.2 <- readRDS("best-rf-model2")
accuracies.2 <- readRDS("rf-accuracies2")

# Save the results again
saveRDS(best.rf.model.2, "best-rf-model2")
saveRDS(accuracies.2, "rf-accuracies2")

# Let's plot the accuracies of different mtry values
plot(accuracies.2, xaxt="n", type="o", pch=20, ylim=c(.82,.9), col="royalblue1", ylab="Accuracy", xlab="ntree", las=1)
points(accuracies, type="o", pch=20)
grid()
axis(1, at=(seq(from = 75, to = 225, by = 25) - 75) / 25 + 1, labels=seq(from = 75, to = 225, by = 25))
points(x = which(accuracies == best.rf.model$results$Accuracy), y = best.rf.model$results$Accuracy, col="orangered1")
points(x = which(accuracies.2 == best.rf.model.2$results$Accuracy), y = best.rf.model.2$results$Accuracy, col="orangered1")

# Now, the interests have changed. And it's obvious that the most relevant features are related to the subjects being
# funny and attractive. Who would have thought? This dataset certainly is full of surprises.
rf.imp.barplot.2 <- barplot(sort(best.rf.model.2$finalModel$importance, decreasing = T)[1:10], col=rev(brewer.pal(10, "RdYlGn")))
text(rf.imp.barplot.2, y = 1, srt=90, labels=names(speed.dating.cont[,-51])[order(best.rf.model.2$finalModel$importance, decreasing = T)][1:10],
     xpd=T, adj = 0)

# And so, the final model, built without the "liked" feature, has the following confusion matrix:
best.rf.model.2$finalModel$confusion
# Which is slightly better at classifying true positives.
best.rf.model$finalModel$confusion

# Validation:
prediction <- predict(best.rf.model.2$finalModel, speed.dating.cont[-learn,-51], type="class")
(cm.table <- table(prediction, speed.dating.cont$match[-learn]))
confusionMatrix(cm.table)
# And the PPV and NPV are actually really good, both close to 95%. This model is GOOD!!
# It has score way above in both specificity and (therefore) in NPV, which is the biggest
# issue other models are facing. Might this be the model we are looking for?

### SVM

#Instead of executing the following lines, load the pre-saved file
######################################################################################################################################
#svm.grid.linear <- expand.grid(gamma = c(0.1, 0.5, 1, 1.5, 2),                                                                      #
#                               C = c(3.5, 3.8, 4, 4.2, 4.5, 5, 5.5))                                                                #
#svm.model.linear <- train(match ~ ., data = speed.dating.cont, subset = learn, method = "svmLinear", preProc = c("center", "scale"),# 
#                          trControl = trc.5CV, tuneGrid = svm.grid.linear, tuneLength = 10)                                         #
######################################################################################################################################
svm.model.linear <- readRDS("svm-model-linear")
saveRDS(svm.model.linear, "svm-model-linear")

#Instead of executing the following lines, load the pre-saved file
###############################################################################################################################
#svm.model <- train(match ~ ., data = speed.dating.cont, subset = learn, method = "svmRadial", preProc = c("center", "scale"),# 
#                   trControl = trc.5CV, tuneLength = 10)                                                                     #
###############################################################################################################################
svm.model <- readRDS("svm-model")
saveRDS(svm.model, "svm-model")
(svm.model)
#sigma held const at 0.0108 and C = 4

###Radial kernel is better than the linear one, lets stick with that and do some fine tuning
################################################################################################################################
#svm.grid <- expand.grid(sigma = c(0.005, 0.010, .015, 0.02 ),                                                                 #
#                    C = c(3.3, 3.5, 3.8, 4, 4.2, 4.5, 5))                                                                     #
#svm.model2 <- train(match ~ ., data = speed.dating.cont, subset = learn, method = "svmRadial", preProc = c("center", "scale"),# 
#                                 trControl = trc.5CV, tuneGrid = svm.grid, tuneLength = 10)                                   #
################################################################################################################################
svm.model2 <- readRDS("svm-model2")
saveRDS(svm.model2, "svm-model2")

plot(svm.model2$results[,3], col=0, ylim=c(.83,.85), xlim=c(3.3,5), xlab="C", ylab="Accuracy", las=1)
grid()
for (sigma in unique(svm.model2$results[,1])) {
  points(x = svm.model2$results[svm.model2$results[,1] == sigma, 2], type="o", y = svm.model2$results[svm.model2$results[,1] == sigma, 3], pch=20, col=rainbow(4)[sigma / 0.005])
}
legend("bottomleft", legend = paste("Sigma =", unique(svm.model2$results[,1])), col=rainbow(4), lty=1)
points(svm.model2$results[order(svm.model2$results[,3], decreasing = T)[1],2:3], col="red")

# After the second pass, sigma = 0.01 and C = 4.2
prediction <- predict(svm.model2, speed.dating.cont[-learn,])

table(prediction, speed.dating.cont$match[-learn])
(sum(prediction != speed.dating.cont$match[-learn])/length(speed.dating.cont$match[-learn]))

### LOGISTIC REGRESSION

# Just the regular way of running the logistic regression, nothing to explain here, except for
# that we are naturally using just the continuous variables
glm.model <- glm(match ~ ., data = speed.dating.cont[learn,], family = binomial(link=logit))
prediction <- predict(glm.model, newdata = speed.dating.cont[-learn,], type="response")
(conf.matrix <- confusionMatrix(table(prediction > 0.5, speed.dating.cont$match[-learn] == 1)))
# And well... The results are not bad, they are better than MLP, but there are still lots of false negatives

### PLAYING AROUND WITH THE MODELS

# Now we are going to add two custom rows, with information collected by us, and see what our models predict.
names(speed.dating.cont)
my.data <- data.frame(NA, 10, 20, 20, 20, 20, 10, 7, 7, 9, 9, 5, 7, 25, 15, 20, 15, 15, 10, 6, 8, 8, 8, 7, 7, 8, 6.5, 7, 7, 6, 9, 5, 7, 9, 0, 0, 10, 7, 2, 7, 1, 0, 10, 6, 7, 3, 0, NA, NA, NA, NA, NA, NA)
my.data.2 <- data.frame(NA, 25, 15, 20, 15, 15, 10, 7, 8, 6.5, 7, 7, 6, 10, 20, 20, 20, 20, 10, 6, 8, 6, 5, 8, 7, 7, 9, 9, 5, 7, 7, 0, 8, 8, 7, 8, 8, 3, 5, 3, 0, 5, 8, 8, 9, 3, 0, NA, NA, NA, NA, NA, NA)
names(my.data) <- names(speed.dating.cont)
names(my.data.2) <- names(speed.dating.cont)
new.data <- rbind(my.data, my.data.2)
View(new.data)

# Normalize out of 100 scores
new.data[1,2:7] <- norm.score(new.data[1, 2:7])
new.data[2,2:7] <- norm.score(new.data[2, 2:7])
new.data[1,14:19] <- norm.score(new.data[1, 14:19])
new.data[2,14:19] <- norm.score(new.data[2, 14:19])

new.data[,c(2:7,14:19)] <- scale(new.data[1:2,c(2:7,14:19)], scale = F)

# Let's inputate the data that we could not realistically obtain with MICE
#rbind(speed.dating.cont, new.data)
mice.imp.new.data <- complete(mice(rbind(speed.dating.cont, new.data), method = 'pmm'))
new.data[, c(1, 48:52)] <- mice.imp.new.data[(nrow(mice.imp.new.data) - 1):(nrow(mice.imp.new.data)), c(1, 48:52)]
View(new.data)

# Finally, make the predictions
new.data[, "Neural network"] <- predict(nnet.train.k$finalModel, new.data[,-53], type="class")
new.data[,"Random forest"] <- predict(best.rf.model.2$finalModel, new.data[,-53], type="class")
new.data[, "SVM"] <- predict(svm.model, new.data[,-53])
new.data[, "Logistic regression"] <- ifelse(predict(glm.model, new.data[,-53], type="response") > .5, 1, 0)

# These are the results.
new.data[,54:57]
