
# MAHALANOBIS

library(stats) # mahalanobis()
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales) # rescale()
#library(RColorBrewer)
#library(colorspace)

data(diamonds)
str(diamonds)

mdist <- mahalanobis(x = diamonds[, c('table', 'depth')],
                     center = colMeans(diamonds[, c('table', 'depth')]),
                     cov = cov(diamonds[, c('table', 'depth')]))

summary(mdist)
diamonds$mdist <- mdist
diamonds$outlier_maha <- "No"
diamonds$outlier_maha[diamonds$mdist > 3 * sd(diamonds$mdist)] <- "Yes" # mark outliers when its
                                      # more then 3 times standard deviation of mahalanobis distance

ggplot(aes(x = table, y = depth), data = diamonds) +
  stat_density2d(aes(fill = ..level..), geom = 'polygon') +
  geom_smooth(method = 'lm', formula = y ~ x) +
  xlim(c(40,100)) +
  ylim(c(40,80)) +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'yellow',"orange", "red", 'darkred'))(100)) +
  ggtitle('2d density plot')

ggplot(aes(x = table, y = depth), data = diamonds) +
  geom_point(aes(color = outlier_maha)) +
  xlim(c(40,100)) +
  ylim(c(40,80)) +
  ggtitle('Mahalanobis outliers')

# COOK'S DISTANCE

lmmodel <- lm(depth ~ table, data = diamonds)
#plot(lmmodel)
cdist <- cooks.distance(lmmodel)
summary(cdist)
diamonds$cdist <- cdist

# how to extract some info from lmmodel https://www.r-bloggers.com/regression-diagnostic-plots-using-r-and-plotly/

diamonds$outlier_cooks <- 'No'
#diamonds$outlier_cooks[diamonds$cdist > 3 * sd(diamonds$cdist)] <- 'Yes'
diamonds$outlier_cooks[diamonds$cdist > (4 / (nrow(diamonds) - 1 - 1))] <- 'Yes'

ggplot(aes(x = table, y = depth), data = diamonds) +
  geom_point(aes(color = outlier_cooks)) +
  geom_smooth(method = 'lm', formula = y ~ x) +
  xlim(c(40, 100)) +
  ylim(c(40, 80)) +
  ggtitle("Cook's distance outliers")

# LOF

library(DMwR)
iris_LOF <- iris[, 1:4]
