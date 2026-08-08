
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

######################
## 제2장 주성분분석 ##
######################

##############
## 2.3 사례 ##
##############

## 저축률

str(LifeCycleSavings)
head(LifeCycleSavings)

# [그림 2-12]
windows(width=7.0, height=5.5)
library(GGally)
ggpairs(LifeCycleSavings, 
        upper=list(continuous=wrap("cor", color="dimgray")),
        lower=list(continuous=wrap("points", color="royalblue")),
        diag=list(continuous=wrap("densityDiag", color="salmon"))) +
  theme_bw()

pca <- prcomp(LifeCycleSavings, center=TRUE, scale=TRUE)
class(pca)
pca

summary(pca)

library(factoextra)

# [그림 2-13]
windows(width=10.0, height=5.5)
p1 <- fviz_screeplot(pca, addlabels=TRUE, choice="eigenvalue")
p2 <- fviz_screeplot(pca, addlabels=TRUE, choice="variance")
library(patchwork)
p1 + p2

pca$rotation

scale(LifeCycleSavings) %*% pca$rotation

pca$x

# [그림 2-14]
windows(width=10.0, height=5.5)
p1 <- fviz_pca_biplot(pca, label="all", labelsize=3, repel=TRUE)
p2 <- fviz_pca_var(pca)
library(patchwork)
p1 + p2

LifeCycleSavings.new <- sapply(LifeCycleSavings, quantile)
LifeCycleSavings.new

new.score <- predict(pca, newdata=LifeCycleSavings.new)
new.score

# [그림 2-15]
windows(width=7.0, height=5.5)
p <- fviz_pca_ind(pca)
fviz_add(p, df=new.score, color="blue")
