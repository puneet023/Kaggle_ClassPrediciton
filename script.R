setwd("E:/Analytics/kaggle/product class challenge")

train <- read.csv('train.csv', header = TRUE)
str(train)


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

f <- as.formula(paste('target ~', paste(colnames(train)[2:94], collapse='+')))

#decision tree
fit_all_V <- rpart(f, train, method="class")
test <- read.csv('test.csv', header = TRUE)
tree_predict_WO_Flow <- predict(fit_all_V, newdata = test)
tree_predict_WO_Flow <- as.data.frame(tree_predict_WO_Flow)
tree <- write.csv(tree_predict_WO_Flow, 'rediction_tree.csv')


#recursive partition tree
library(party)
library(mlbench)

ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 500,
                    objfun = logLik, verbose = TRUE)

f1 <- as.formula(paste('target ~', '1 |' , paste(colnames(train)[2:94], collapse='+')))
fit_party <- mob(f1, data = train, model = glinearModel, family = binomial())
fit_party_glm <- glmtree(f1, data = train)
predict_party_glm <- predict.glm(fit_party_glm, test)
tree_party_glm <- as.data.frame(predict_party_glm)
write.csv(tree_party_glm, 'party_tree_glm.csv')


