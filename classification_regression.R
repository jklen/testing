
# naive bayes - classification

library(e1071)

iris_bayes <- naiveBayes(Species ~ Sepal.Length + Sepal.Width, data = iris, laplace = 1) # laplace - add 1 to all classes count,
          # to not dismiss class with 0 count
iris_bayes
iris_bayes_pred <- predict(iris_bayes, iris[1:2], type = 'class') # predicts most probable clas
iris_bayes_pred_raw <- predict(iris_bayes, iris[1:2], type = 'raw') # returns probabilities of all classes predictions
table(iris_bayes_pred, iris$Species, dnn = c('prediction', 'actual'))
iris_bayes$tables$Sepal.Length
iris_bayes$tables$Sepal.Width

prop.table(table(iris_bayes_pred, iris$Species, dnn = c('prediction', 'actual')))


# CART - regression/classification/numeric, categorical inputs

library(rpart)

data(diamonds, package = 'ggplot2')
str(diamonds)
diamonds_cart_clarity <- rpart(clarity ~ carat + cut + color + depth + price,
                               data = diamonds,
                               cp = 10 ^(-6),
                               minsplit = 10)

plotcp(diamonds_cart_clarity)
str(diamonds_cart_clarity) # xerror - average error on 10-fold cross-validation
diamonds_cart_clarity$cptable[1:10,]
diamonds_cart_clarity$cptable[(nrow(diamonds_cart_clarity$cptable) -  10):nrow(diamonds_cart_clarity$cptable),]

diamonds_cart_clarity$cptable[which.min(diamonds_cart_clarity$cptable[,"xerror"]),]

#   automated tree size search

which.min(diamonds_cart_clarity$cptable[, 4])
cpstat <- 
  nrow(diamonds) * diamonds_cart_clarity$cptable[, 'rel error'] + 
  2 * (diamonds_cart_clarity$cptable[,'nsplit'] + 1) # formula to find optimum tree size, penalizing larger trees
round(diamonds_cart_clarity$cptable[which.min(cpstat), ], 3)

#   tree prunning

cp10 = which(diamonds_cart_clarity$cptable[, 'nsplit'] == 10)
diamonds_cart_clarity10 <- prune(diamonds_cart_clarity, diamonds_cart_clarity$cptable[cp10, 1])

# plots

plotcp(diamonds_cart_clarity10)
plot(diamonds_cart_clarity10, uniform = T, main = 'Classification tree - diamonds clarity')
text(diamonds_cart_clarity10, use.n=TRUE, all=TRUE, cex=.6)

#   predicting

diamonds_cart_clarity10_pred <- predict(diamonds_cart_clarity10,
                                        type = 'prob',
                                        newdata = diamonds[1:100, c('carat', 'cut', 'color', 'depth', 'price')])
diamonds_cart_clarity10_pred <- cbind(diamonds[1:100, 'clarity', drop = F], diamonds_cart_clarity10_pred)
diamonds_cart_clarity10_pred$pred_class <- predict(diamonds_cart_clarity10,
                                                   type = 'class',
                                                   newdata = diamonds[1:100, c('carat', 'cut', 'color', 'depth', 'price')])

table(diamonds_cart_clarity10_pred$pred_class,
                 diamonds_cart_clarity10_pred$clarity, dnn = c('prediction', 'actual'))

# CART 2 - conditional inference trees

library(party) # CART with statistical stopping rules, so no prunning should be required

iris_cart2_clarity <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length,
                                data = iris)
summary(iris_cart2_clarity)
plot(iris_cart2_clarity, main = 'Conditional inference tree')

iris_cart2_clarity_pred <- predict(iris_cart2_clarity,
                                   type = 'response', 
                                   newdata = iris[, c('Sepal.Length', 'Sepal.Width', 'Petal.Length')])
iris_cart2_clarity_pred <- cbind(iris[,'Species', drop = F], iris_cart2_clarity_pred)

table(iris_cart2_clarity_pred$iris_cart2_clarity_pred, iris_cart2_clarity_pred$Species, dnn = c('prediction', 'actual'))

table(diamonds_cart_clarity10_pred$pred_class,
      diamonds_cart_clarity10_pred$clarity, dnn = c('prediction', 'actual'))

# KNN - classification, knn from base - works with numeric inputs only

library(class)
library(gmodels) # crosstable
library(scales)

# numeric inputs

diamonds_train <- diamonds[1:35000, c('x', 'price', 'cut')]
diamonds_test <- diamonds[35001:53940,c('x', 'price', 'cut')]

diamonds_train[c('x', 'price')] <- 
  lapply(diamonds_train[c('x', 'price')], rescale)
diamonds_test[c('x', 'price')] <- 
  lapply(diamonds_test[c('x', 'price')], rescale)

labels_train <- diamonds_train$cut
labels_test <- diamonds_test$cut

diamonds_knn_test_pred <- knn(train = diamonds_train[, 1:2],
                              test = diamonds_test[1:2],
                              cl = labels_train,
                              k = round(sqrt(nrow(diamonds_train))))

CrossTable(labels_test, diamonds_knn_test_pred, prop.chisq = F)


# categorical inputs

library(knncat) # works with categoricals only, or numericals only
library(gmodels) # crosstable

diamonds_train <- diamonds[1:35000, c('clarity', 'color', 'cut')]
diamonds_test <- diamonds[35001:53940,c('clarity', 'color', 'cut')]

diamonds_knn <- knncat(diamonds_train, classcol = 3)

diamonds_test_pred <- predict(diamonds_knn, 
                              diamonds_train, 
                              diamonds_test, 
                              train.classcol = 3, 
                              newdata.classcol = 3)

CrossTable(diamonds_test$cut, diamonds_test_pred)
