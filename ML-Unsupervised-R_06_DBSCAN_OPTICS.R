
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

##################################
## 제6장 DBSCAN/OPTICS 군집분석 ##
##################################

##############
## 6.4 사례 ##
##############

## 위조지폐

# DBSCAN

library(mclust)
str(banknote)

banknote.scaled <- scale(banknote[-1])
head(banknote.scaled)

# [그림 6-11]
windows(width=7.0, height=5.5)
library(dbscan)
set.seed(123)
kNNdistplot(x=banknote.scaled, k=6)
abline(h=1.4, col="red", lty="dashed")

set.seed(123)
db <- dbscan(x=banknote.scaled, eps=1.4, minPts=7)
class(db)
db

db$cluster

# [그림 6-12]
windows(width=7.0, height=5.5)
library(factoextra)
fviz_cluster(db, banknote.scaled, stand=FALSE, geom="point", 
             ellipse=FALSE, palette="npg", ggtheme=theme_classic())

hullplot(x=banknote.scaled, cl=db)

# OPTICS

library(mclust)
banknote.scaled <- scale(banknote[-1])

library(dbscan)
set.seed(123)
op <- optics(banknote.scaled, minPts=7)
class(op)
op

head(op$reachdist, n=15)
head(op$order, n=15)

# [그림 6-13]
windows(width=7.0, height=5.5)
plot(op, col="purple")

op$reachdist[op$order[1]]
op$reachdist[op$order[2]]
op$reachdist[op$order[3]]
op$reachdist[op$order[200]]

# [그림 6-14]
windows(width=7.0, height=5.5)
plot(banknote.scaled[, 3], banknote.scaled[, 6], pch=21, col="black", bg="gray", 
     main="Point Ordering", xlab="Right", ylab="Diagonal")
polygon(banknote.scaled[op$order, c(3, 6)], border="cornflowerblue")

op.db <- extractDBSCAN(op, eps_cl=1.4)
op.db
op.db$cluster

# [그림 6-15]
windows(width=7.0, height=5.5)
plot(op.db)

# [그림 6-16]
windows(width=7.0, height=5.5)
library(factoextra)
fviz_cluster(list(data=banknote.scaled, cluster=op.db$cluster), banknote.scaled, stand=FALSE, 
             geom="point", ellipse=FALSE, palette="lancet", ggtheme=theme_classic())

op.xi <- extractXi(op, xi=0.03)
op.xi
op.xi$cluster
table(op.xi$cluster)

# [그림 6-17]
windows(width=7.0, height=5.5)
plot(op.xi)

op.xi$clusters_xi

# [그림 6-18]
windows(width=7.0, height=5.5)
hullplot(x=banknote.scaled, cl=op.xi)

fviz_cluster(list(data=banknote.scaled, cluster=op.xi$cluster), banknote.scaled, 
             stand=FALSE, geom="point", ellipse.type="norm",  palette="lancet",
             ggtheme=theme_classic())

# [그림 6-19]
windows(width=7.0, height=5.5)
dend <- as.dendrogram(op.xi)
dend
library(dendextend)
library(factoextra)
dend.colored <- color_branches(dend, k=6)
fviz_dend(x=dend.colored, show_labels=FALSE, ylab="Reachability distance")
