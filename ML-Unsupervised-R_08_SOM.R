
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

##########################
## 제8장 자기조직화지도 ##
##########################

##############
## 8.3 사례 ##
##############

## 와인

library(rattle)
str(wine)

wine.scaled <- scale(wine[-1])
head(wine.scaled)

library(kohonen)
som.grid <- somgrid(xdim=5, ydim=5, topo="hexagonal", 
                    neighbourhood.fct="bubble", toroidal=FALSE)
class(som.grid)

som.grid

set.seed(123)
som.fit <- som(wine.scaled, grid=som.grid, rlen=500, alpha=c(0.05, 0.01))
class(som.fit)

# [그림 8-7]
windows(width=7.0, height=5.5)
plot(som.fit, type="codes", palette.name=rainbow, shape="straight")

som.fit$codes
getCodes(som.fit)

som.grid$pts

# [그림 8-8]
windows(width=7.0, height=5.5)
plot(som.fit, type="changes", col="cornflowerblue", lwd=2)

som.fit$changes

# [그림 8-9]
windows(width=7.0, height=5.5)
plot(som.fit, type="counts", shape="straight")

# [그림 8-10]
windows(width=7.0, height=5.5)
plot(som.fit, type="dist.neighbours", shape="straight")

# [그림 8-11]
windows(width=7.0, height=5.5)
plot(som.fit, type="mapping", shape="straight",
     pchs=21, col="black", bg="salmon", bgcol="snow")

som.fit$unit.classif

# [그림 8-12]
windows(width=7.0, height=5.5)
plot(som.fit, type="quality", shape="straight")

tapply(som.fit$distances, list(node=som.fit$unit.classif), mean)

# [그림 8-13]
heatmap.som <- function(model){
  for (i in 1:ncol(getCodes(model))) {
    plot(model, type="property", property=getCodes(model)[,i], 
         main=colnames(getCodes(model))[i]) 
  }
}
windows(width=7.0, height=7.0)
par(mfrow=c(5, 3))
heatmap.som(som.fit)

# [그림 8-14]
windows(width=9.0, height=4.5)
par(mfrow = c(1, 2))
cols <- c("steelblue", "goldenrod", "salmon")
plot(som.fit, type="mapping", pchs=21, bg=cols[as.numeric(wine$Type)],
     bgcol="lightgray", shape="straight")

set.seed(123)
clusters <- kmeans(som.fit$codes[[1]], centers=3, nstart=25)$cluster
clusters
library(dplyr)
plot(som.fit, type="mapping", pch=21, bg=cols[as.numeric(wine$Type)],
     bgcol=cols[case_match(clusters, 2 ~ 3, 3 ~ 2, .default=clusters)], 
     shape="straight")
add.cluster.boundaries(som.fit, clusters)

wine.new <- apply(wine[-1], 2, quantile)
wine.new

attributes(wine.scaled)

wine.newscaled <- scale(wine.new, 
                        center=attr(wine.scaled, "scaled:center"),
                        scale=attr(wine.scaled, "scaled:scale"))

pred <- predict(som.fit, newdata=wine.newscaled)
pred$unit.classif

# [그림 8-15]
windows(width=7.0, height=5.5)
plot(som.fit, type="mapping", classif=pred, shape="straight",
     pchs=21, col="black", bg="royalblue", bgcol="snow")

# [그림 8-16]
windows(width=7.0, height=5.5)
plot(som.fit, type="mapping", shape="straight",
     pchs=21, col="black", bg="salmon", bgcol="snow")
bin.center <- som.fit$grid$pts  
node.index <- pred$unit.classif 
points(jitter(bin.center[node.index, 1]), jitter(bin.center[node.index, 2]),
       pch=21, col="black", bg="royalblue", cex=1.5)
