###########################
# STAT 5850 Final Project #
###########################
library(MASS)
library(tree)

## Data preparation
# Import csgo dataset
csgo = read.csv(file = "csgo.csv", header = T)
head(csgo)
attach(csgo)

# Data cleaning - include only the useful variables, convert variable types
csgo = csgo[c(1:3,5:11,15,16,97)]
names(csgo)
lapply(csgo, class)
csgo$bomb_planted = as.logical(bomb_planted)
csgo$round_winner = as.factor(round_winner)
summary(csgo)
contrasts(csgo$round_winner)

# Split the data set into train and test sets
set.seed(100)
train = sample(1:dim(csgo)[1], dim(csgo)[1]/2)
csgo.test = csgo[-train, ]
y.test = csgo$round_winner[-train]



## Logistic regression
# Fitting a logistic regression model with all variables
logit.fit = glm(round_winner ~ ., data = csgo, subset = train, family = binomial)
summary(logit.fit)

# Calculating the test error rate
logit.probs = predict(logit.fit, csgo.test, type = "response")
logit.pred = rep("CT", dim(csgo)[1]/2)
logit.pred[logit.probs > 0.5] = "T"
logit.table = table(logit.pred, y.test)
logit.table
logit.error = (logit.table[1,2] + logit.table[2,1])/(sum(logit.table))



## Linear Discriminant Analysis
# Fitting an lda model with all variables
lda.fit = lda(round_winner ~ ., data = csgo, subset = train)

# Calculating the test error rate
lda.pred = predict(lda.fit, csgo.test)
lda.table = table(lda.pred$class, y.test)
lda.table
lda.error = (lda.table[1,2] + lda.table[2,1])/sum(lda.table)



## Quadratic Discriminant Analysis
# Fitting a qda model with all variables
qda.fit = qda(round_winner ~ ., data = csgo, subset = train)

# Calculating the test error rate
qda.pred = predict(qda.fit, csgo.test)
qda.table = table(qda.pred$class, y.test)
qda.table
qda.error = (qda.table[1,2] + qda.table[2,1])/sum(qda.table)



## Classification Tree
# Fitting a classification tree with all variables
tree.fit = tree(round_winner ~ ., data = csgo, subset = train)
summary(tree.fit)

# Plotting the tree 
plot(tree.fit)
text(tree.fit, pretty = 0)

# Calculating the test error rate
tree.pred = predict(tree.fit, csgo.test, type = "class")
tree.table = table(tree.pred, y.test)
tree.table
tree.error = (tree.table[1,2] + tree.table[2,1])/sum(tree.table)



## Pruned Classification Tree
# Pruning the classificaiton tree
cv.fit <- cv.tree(tree.fit, FUN = prune.misclass)
which.min(cv.fit$dev)
cv.fit$size[1]
prune.fit <- prune.misclass(tree.fit, best = 8)

# Plotting the pruned tree
plot(prune.fit)
text(prune.fit, pretty = 0)

# Calculating the test error rate
prune.pred <- predict(prune.fit, csgo.test, type = "class")
prune.table = table(prune.pred, y.test)
prune.table
prune.error = (prune.table[1,2] + prune.table[2,1])/sum(prune.table)



## Validation set approach
# Table for all test error rate
comparison_table <- rbind(logit.error, lda.error, qda.error, tree.error, prune.error)
colnames(comparison_table) <- c("test error rate")
comparison_table
