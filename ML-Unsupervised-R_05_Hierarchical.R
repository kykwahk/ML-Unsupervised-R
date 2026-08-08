
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

###########################
## 제5장 계층적 군집분석 ##
###########################

##############
## 5.3 사례 ##
##############

## 식품

library(flexclust)
data(nutrient)
str(nutrient)
head(nutrient)

nutrient.scaled <- scale(nutrient)

diss <- dist(nutrient.scaled, method="euclidean")
hc <- hclust(d=diss, method="average")
class(hc)
hc

# [그림 5-3]
windows(width=7.0, height=5.5)
plot(hc, hang=-1, cex=0.6, col="darkgreen", 
     xlab="Food", main="Agglomerative Hierarchical Clustering")

diss.coph <- cophenetic(hc)
cor(diss, diss.coph)

hc2 <- hclust(d=diss, method="complete")
diss.coph2 <- cophenetic(hc2)
cor(diss, diss.coph2)

library(cluster)
agn <- agnes(x=nutrient.scaled, method="complete")
class(agn)
agn

agn$ac

library(purrr)
map(c( "single", "complete", "average", "ward"), 
    ~agnes(x=nutrient.scaled, method=.x)) |> 
  map_dbl(~.x$ac) 

# [그림 5-4]
windows(width=10.0, height=10.0)
par(mfrow=c(2, 2))
agn.single <- agnes(x=nutrient.scaled, method="single")
pltree(agn.single, hang=-1, cex=0.6, col="darkgreen",
       xlab="Food", main="Single Linkage")
agn.complete <- agnes(x=nutrient.scaled, method="complete")
pltree(agn.complete, hang=-1, cex=0.6, col="darkgreen",
       xlab="Food", main="Complete Linkage")
agn.average <- agnes(x=nutrient.scaled, method="average")
pltree(agn.average, hang=-1, cex=0.6, col="darkgreen",
       xlab="Food", main="Average Linkage")
agn.ward <- agnes(x=nutrient.scaled, method="ward")
pltree(agn.ward, hang=-1, cex=0.6, col="darkgreen",
       xlab="Food", main="Minimum Variance Linkage")

dia <- diana(x=nutrient.scaled)
class(dia)
dia

dia$dc

# [그림 5-5]
windows(width=7.0, height=5.5)
pltree(dia, hang=-1, cex=0.6, col="darkgreen",
       xlab="Food", main="Divisive Hierarchical Clustering")

# [그림 5-6]
library(factoextra)
p1 <- fviz_nbclust(x=nutrient.scaled, FUNcluster=hcut, method="wss", k.max=10) +
  geom_vline(xintercept=3, linetype="dashed", color="steelblue") +
  labs(subtitle="Elbow method")
p2 <- fviz_nbclust(x=nutrient.scaled, FUNcluster=hcut, method="silhouette", k.max=10) +
  labs(subtitle="Silhouette method")
set.seed(123)
p3 <- fviz_nbclust(x=nutrient.scaled, FUNcluster=hcut, method="gap_stat", k.max=10) +
  labs(subtitle="Gap statistic method")
library(NbClust)
set.seed(123)
nc <- NbClust(data=nutrient.scaled, distance="euclidean", 
              min.nc=2, max.nc=10, method="ward.D2")
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
library(patchwork)
windows(width=10.0, height=7.5)
p1 + p2 + p3 + p4

hc <- hclust(d=diss, method="ward.D2")
clusters <- cutree(hc, k=3)
clusters
data.frame(nutrient, cluster=clusters)

table(clusters)

# [그림 5-7]
windows(width=7.0, height=5.5)
plot(hc, hang=-1, cex=0.6, col="darkgreen",
     xlab="Food", main="Agglomerative Hierarchical Clustering with 3 Clusters")
rect.hclust(hc, k=3, border=2:4)

palette()

# [그림 5-8]
windows(width=7.0, height=5.5)
fviz_dend(x=hc, k=3, horiz=TRUE, rect=TRUE, rect_fill=TRUE, 
          k_colors="uchicago", rect_border="uchicago", cex=0.5,
          main="Agglomerative Hierarchical Clustering with 3 Clusters")

# [그림 5-9]
p1 <- fviz_dend(x=hc, k=3, k_colors="npg", type="circular", cex=0.5) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
install.packages("igraph")
p2 <- fviz_dend(x=hc, k=3, k_colors="npg", repel=TRUE, type="phylogenic", cex=0.7) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())
windows(width=10.0, height=5.5)
library(patchwork)
p1 + p2

aggregate(nutrient, by=list(cluster=clusters), mean)

a <- aggregate(nutrient.scaled, by=list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)

# [그림 5-10]
windows(width=7.0, height=5.5)
fviz_cluster(list(data=nutrient, cluster=clusters), repel=TRUE, ellipse.type="convex", 
             labelsize=10, palette="lancet", 
             ggtheme=theme_classic(), show.legend.text=FALSE)
