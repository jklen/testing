
# PCA

library(scales)
library(ggplot2)
library(ggbiplot)

data(diamonds, package = 'ggplot2')
str(diamonds)

# PCA from base

diamonds_to_pca1 <- diamonds[sample.int(nrow(diamonds), 1000),]
diamonds_to_pca <- as.data.frame(lapply(diamonds_to_pca1[,c('carat', 'depth', 'table', 'price', 'x', 'y', 'z')], rescale))


diamonds_pca <- prcomp(x = diamonds_to_pca)
summary(diamonds_pca)
plot(diamonds_pca, type = 'l')
plot(diamonds_pca, type = 'b')
diamonds_pca$rotation

to_plot <- as.data.frame(diamonds_pca$x[,1:2])
to_plot$cut <- diamonds_to_pca1$cut
to_plot$color <- diamonds_to_pca1$color
to_plot$clarity <- diamonds_to_pca1$clarity

#ggplot(aes(x = PC1, y = PC2), data = to_plot) + geom_point(aes(color = cut))

ggbiplot(diamonds_pca, obs.scale = 1, var.scale = 1, 
              groups = to_plot$cut, ellipse = TRUE, 
              circle = TRUE, alpha = 0.5) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
               legend.position = 'top')

# MCA

library(FactoMineR)
library(ggplot2)

data(tea, package = 'FactoMineR')
str(tea)

newtea <- tea[, c("Tea", "How", "how", "sugar", "where", "always")]
cats <- apply(newtea, 2, function(x) nlevels(as.factor(x)))

mca1 <- MCA(newtea)

# data frame with variable coordinates
mca1_vars_df <- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df <- data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")

# TSNE

library(Rtsne)
library(ggplot2)

iris_tsne <- Rtsne(unique(iris[,1:4]))
to_plot_iris <- cbind(unique(iris), iris_tsne$Y)
colnames(to_plot_iris)[6:7] <- c('dim1', 'dim2')

ggplot(aes(x = dim1, y = dim2), data = to_plot_iris) +
  geom_point(aes(color = Species)) +
  ggtitle('TSNE plot - default parameters')
