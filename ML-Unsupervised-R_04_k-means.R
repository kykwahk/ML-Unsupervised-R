
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

###########################
## 제4장 k-평균 군집분석 ##
###########################

##############
## 4.4 사례 ##
##############

## 미국 주

head(state.x77)
state.scaled <- scale(state.x77)

library(factoextra)
fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="wss", 
             diss=get_dist(state.scaled, method="euclidean"), k.max=10) 
fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="wss", 
             diss=dist(state.scaled, method="euclidean"), k.max=10) +
  geom_vline(xintercept=2, linetype="dashed", color="steelblue") +
  geom_vline(xintercept=4, linetype="dashed", color="steelblue") +
  labs(subtitle="Elbow method")
fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="silhouette", 
             diss=dist(state.scaled, method="euclidean"), k.max=10) +
  labs(subtitle="Silhouette method")
set.seed(123)
fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="gap_stat", 
             diss=dist(state.scaled, method="euclidean"), k.max=10) +
  labs(subtitle="Gap statistic method")

library(NbClust)
set.seed(123)
nc <- NbClust(data=state.scaled, distance="euclidean", 
              min.nc=2, max.nc=10, method="kmeans")

nc$Best.nc

table(nc$Best.nc[1,])

ggplot(data.frame(table(nc$Best.nc[1,])[-1]), aes(x=Var1, y=Freq)) +
  geom_col(fill="steelblue", col="steelblue") +
  labs(title="Optimal number of clusters",
       subtitle="Majority rule method",
       x="Number of clusters k", y="Number of supporting index") +
  theme_classic() +
  theme(plot.title=element_text(size=14),
        plot.subtitle=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))

# [그림 4-2]
library(factoextra)
p1 <- fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="wss", 
                   diss=dist(state.scaled, method="euclidean"), k.max=10) +
  geom_vline(xintercept=2, linetype="dashed", color="steelblue") +
  geom_vline(xintercept=4, linetype="dashed", color="steelblue") +
  labs(subtitle="Elbow method")
p2 <- fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="silhouette", 
                   diss=dist(state.scaled, method="euclidean"), k.max=10) +
  labs(subtitle="Silhouette method")
set.seed(123)
p3 <- fviz_nbclust(x=state.scaled, FUNcluster=kmeans, method="gap_stat", 
                   diss=dist(state.scaled, method="euclidean"), k.max=10) +
  labs(subtitle="Gap statistic method")
library(NbClust)
set.seed(123)
nc <- NbClust(data=state.scaled, distance="euclidean", 
              min.nc=2, max.nc=10, method="kmeans")
p4 <- ggplot(data.frame(table(nc$Best.nc[1,])[-1]), aes(x=Var1, y=Freq)) +
  geom_col(fill="steelblue", col="steelblue") +
  labs(title="Optimal number of clusters",
       subtitle="Majority rule method",
       x="Number of clusters k", y="Number of supporting index") +
  theme_classic() +
  theme(plot.title=element_text(size=14),
        plot.subtitle=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))
p4 <- ggplot(data.frame(table(nc$Best.nc[1, !(colnames(nc$Best.nc) %in% c("Hubert", "Dindex"))])), 
             aes(x=Var1, y=Freq)) +
  geom_col(fill="steelblue", col="steelblue") +
  labs(title="Optimal number of clusters",
       subtitle="Majority rule method",
       x="Number of clusters k", y="Number of supporting index") +
  theme_classic() +
  theme(plot.title=element_text(size=14),
        plot.subtitle=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12))
windows(width=10.0, height=7.5)
library(patchwork)
p1 + p2 + p3 + p4

set.seed(123)
km <- kmeans(x=state.scaled, centers=3, nstart=25)
class(km)
km

km$cluster
km$centers
km$size

aggregate(state.x77, by=list(cluster=km$cluster), mean)

# [그림 4-3]
windows(width=7.0, height=5.5)
fviz_cluster(km, data=state.scaled, repel=TRUE, ellipse.type="convex", 
             pointsize=2, labelsize=10, ggtheme=theme_classic(), 
             show.legend.text=FALSE)

library(cluster)
set.seed(123)
pm <- pam(state.scaled, k=3, nstart=25)
class(pm)
pm

pm$clusinfo

pm$medoids
pm$id.med

pm$clustering
aggregate(state.x77, by=list(cluster=pm$clustering), mean)

# [그림 4-4]
windows(width=7.0, height=5.5)
fviz_cluster(pm, repel=TRUE, ellipse.type="convex", 
             pointsize=2, labelsize=10, ggtheme=theme_classic(), 
             show.legend.text=FALSE)

set.seed(123)
cl <- clara(state.scaled, k=3, samples=50, pamLike=TRUE)
class(cl)
cl

cl$clusinfo
cl$medoids
cl$clustering

# [그림 4-5]
windows(width=7.0, height=5.5)
fviz_cluster(cl, geom="text", labelsize=10, ellipse.type="euclid", 
             ggtheme=theme_classic(), show.legend.text=FALSE)

library(MASS)
system.time(pam(scale(Boston), k=3, nstart=25))
system.time(clara(scale(Boston), k=3, samples=50, pamLike=TRUE))

## 자동차

mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels=c("V", "S"))
  am <- factor(am, labels=c("automatic", "manual"))
  cyl  <- factor(cyl, ordered=TRUE)
  gear <- factor(gear, ordered=TRUE)
  carb <- factor(carb, ordered=TRUE)
})
str(mtcars2)

library(dplyr)
mtcars2 <- mutate(mtcars2, across(where(is.ordered), as.numeric))
str(mtcars2)

library(caret)
onehot <- dummyVars(~ ., data=mtcars2, fullRank=FALSE)
cars.onehot <- predict(onehot, mtcars2) 

cars.onehotscaled <- scale(cars.onehot)

colnames(cars.onehotscaled)

# [그림 4-6]
windows(width=7.0, height=5.5)
library(factoextra)
set.seed(123)
fviz_nbclust(x=cars.onehotscaled, FUNcluster=kmeans, method="wss") +
  geom_vline(xintercept=2, linetype="dashed", color="steelblue") +
  labs(subtitle="Elbow method")

set.seed(123)
km <- kmeans(x=cars.onehotscaled, centers=2, nstart=25)
km$cluster

# [그림 4-7]
windows(width=7.0, height=5.5)
fviz_cluster(km, data=cars.onehotscaled, repel=TRUE, ellipse.type="norm", 
             pointsize=2, labelsize=10, star.plot=TRUE, ggtheme=theme_minimal(), 
             show.legend.text=FALSE)

library(cluster)
diss <- daisy(mtcars2, metric="gower")

pm <- pam(x=diss, k=2, diss=TRUE)
pm$clustering

# [그림 4-8]
windows(width=7.0, height=5.5)
clusplot(pm, color=TRUE, shade=TRUE, labels=2, lines=0, 
         cex.txt=0.8, main="Cluster Plot", sub="")
