
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

########################
## 제3장 t-SNE와 UMAP ##
########################

##############
## 3.3 사례 ##
##############

## 펭귄: t-SNE

library(modeldata)
str(penguins)
levels(penguins$species)

library(tidyverse)
peng <- drop_na(penguins)

library(Rtsne)
set.seed(123)
tsne <- Rtsne(X=select(peng, -c("species", "island", "sex")), pca_scale=TRUE)
class(tsne)

head(tsne$Y)

peng.tsne <- peng |> 
  mutate(tSNE1=tsne$Y[, 1], tSNE2=tsne$Y[, 2])
peng.tsne

# [그림 3-8]
windows(width=7.0, height=5.5)
ggplot(peng.tsne, aes(x=tSNE1, y=tSNE2, col=species, shape=species)) +
  geom_point(size=2) +
  theme_bw()

hyper.grid <- expand.grid(perplexity=c(1, 10, 20, 30, 40, 50), 
                          theta=c(0.0, 0.25, 0.5, 0.75, 1.0))
hyper.grid

hyper.tsne <- pmap(hyper.grid, Rtsne, 
                   X=select(peng, -c("species", "island", "sex")), 
                   pca_scale=TRUE)
hyper.tsne.df <- data.frame(perplexity=rep(hyper.grid$perplexity, each=nrow(peng)),
                            theta=rep(hyper.grid$theta, each=nrow(peng)),
                            tSNE1=unlist(map(hyper.tsne, ~.$Y[, 1])),
                            tSNE2=unlist(map(hyper.tsne, ~.$Y[, 2])),
                            species=rep(peng$species, times=nrow(hyper.grid)))
str(hyper.tsne.df)

# [그림 3-9]
windows(width=7.0, height=5.5)
ggplot(hyper.tsne.df, aes(tSNE1, tSNE2, col=species, shape=species)) +
  facet_grid(theta ~ perplexity) +
  geom_point() +
  theme_bw() +
  theme(legend.position="bottom")

set.seed(123)
tsne <- Rtsne(X=select(peng, -c("species", "island", "sex")), 
              perplexity=20, theta=0, pca_scale=TRUE)
peng.tsne <- peng |> 
  mutate(tSNE1=tsne$Y[, 1], tSNE2=tsne$Y[, 2])

# [그림 3-10]
windows(width=7.0, height=5.5)
ggplot(peng.tsne, aes(x=tSNE1, y=tSNE2, col=species, shape=species)) +
  geom_point(size=2) +
  theme_bw()

# PCA

pca <- prcomp(select(peng, -c("species", "island", "sex")), 
              center=TRUE, scale=TRUE)
peng.pca <- peng |> 
  mutate(PCA1=pca$x[, 1], PCA2=pca$x[, 2])

# [그림 3-11]
windows(width=7.0, height=5.5)
ggplot(peng.pca, aes(x=PCA1, y=PCA2, col=species, shape=species)) +
  geom_point(size=2) +
  theme_bw()

## 펭귄: UMAP

library(modeldata)
library(tidyverse)
peng <- drop_na(penguins) 

library(uwot)
set.seed(123)
umapr <- umap(X=select(peng, -c("species", "island", "sex")), scale="Z")
class(umapr)

str(umapr)
head(umapr)

peng.umap <- peng |> 
  mutate(UMAP1=umapr[, 1], UMAP2=umapr[, 2])
peng.umap

# [그림 3-12]
windows(width=7.0, height=5.5)
ggplot(peng.umap, aes(x=UMAP1, y=UMAP2, col=species, shape=species)) +
  geom_point(size=2) +
  theme_bw()

hyper.grid <- expand.grid(n_neighbors=c(3, 7, 11, 15, 19, 21), 
                          min_dist=c(0.01, 0.05, 0.1, 0.3, 0.5))
hyper.grid

hyper.umap <- pmap(hyper.grid, umap, 
                   X=select(peng, -c("species", "island", "sex")), scale="Z")
hyper.umap.df <- data.frame(n_neighbors=rep(hyper.grid$n_neighbors, each=nrow(peng)),
                            min_dist=rep(hyper.grid$min_dist, each=nrow(peng)),
                            UMAP1=unlist(map(hyper.umap, ~.[, 1])),
                            UMAP2=unlist(map(hyper.umap, ~.[, 2])),
                            species=rep(peng$species, times=nrow(hyper.grid)))
str(hyper.umap.df)

# [그림 3-13]
windows(width=7.0, height=5.5)
ggplot(hyper.umap.df, aes(UMAP1, UMAP2, col=species, shape=species)) +
  facet_grid(min_dist ~ n_neighbors) +
  geom_point() +
  theme_bw() +
  theme(legend.position="bottom")

set.seed(123)
umapr <- umap(select(peng, -c("species", "island", "sex")),
              n_neighbors=21, min_dist=0.5, scale="Z", ret_model=TRUE)
head(umapr$embedding)

peng.umap <- peng |> 
  mutate(UMAP1=umapr$embedding[, 1], UMAP2=umapr$embedding[, 2])

# [그림 3-14]
windows(width=7.0, height=5.5)
ggplot(peng.umap, aes(x=UMAP1, y=UMAP2, col=species, shape=species)) +
  geom_point(size=2) +
  theme_bw()

peng.new <- sapply(select(peng, -c("species", "island", "sex")), 
                   quantile)
peng.new

library(magrittr)
set.seed(123)
new.score <- umap_transform(X=peng.new, model=umapr) |> 
  set_colnames(c("umap1", "umap2")) |> 
  as_tibble(rownames=NA) |> 
  rownames_to_column(var="quantile") 
new.score  

# [그림 3-15]
windows(width=7.0, height=5.5)
ggplot(peng.umap, aes(x=UMAP1, y=UMAP2, col=species, shape=species)) +
  geom_point(size=2) +
  geom_point(aes(x=umap1, y=umap2), data=new.score, 
             pch=23, color="black", bg="violetred", size=5, inherit.aes=FALSE) +
  geom_text(aes(x=umap1, y=umap2, label=quantile), data=new.score, 
            hjust=1, vjust=1, nudge_x=-0.2, inherit.aes=FALSE) +
  theme_bw()
