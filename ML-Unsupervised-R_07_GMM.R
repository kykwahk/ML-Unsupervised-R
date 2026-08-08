
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

########################
## 제7장 GMM 군집분석 ##
########################

##############
## 7.4 사례 ##
##############

## 범죄율

str(USArrests)
head(USArrests)

library(mclust)
gmm <- Mclust(data=USArrests)
class(gmm)
gmm

summary(gmm)

# [그림 7-5]
windows(width=7.0, height=5.5)
plot(gmm, what="BIC", legendArgs=list(x="bottomright", ncol=5))

gmm$modelName
gmm$G

# [그림 7-6]
windows(width=7.0, height=5.5)
plot(gmm, what="classification")

gmm$classification
gmm$z

library(tidyr)
probs <- gmm$z
colnames(probs) <- paste("Cluster", 1:3)
probs <- pivot_longer(as.data.frame(probs), cols=contains("Cluster"), 
                      names_to="cluster", values_to="probability")
probs

# [그림 7-7]
windows(width=7.0, height=4.0)
library(ggplot2)
ggplot(probs, aes(x=probability)) +
  geom_histogram(color="black", fill="salmon") +
  facet_wrap(~ cluster) +
  labs(x="Probability", y="Count") +
  theme_bw()

# [그림 7-8]
windows(width=7.0, height=5.5)
plot(gmm, what="uncertainty")

gmm$uncertainty
head(sort(gmm$uncertainty, decreasing=TRUE))

library(dplyr)
library(tibble)
uncert <- USArrests |> 
  rownames_to_column(var="states") |> 
  mutate(cluster=gmm$classification,
         uncertainty=gmm$uncertainty)
uncert5 <- uncert |> 
  group_by(cluster) |> 
  slice_max(order_by=uncertainty, n=5) |> 
  ungroup()
uncert5

# [그림 7-9]
windows(width=7.0, height=4.0)
ggplot(uncert5, aes(x=uncertainty, y=reorder(states, uncertainty))) +
  geom_point(shape=21, size=2, color="black", fill="cornflowerblue") +
  facet_wrap(~ paste("Cluster", cluster), scales="free_y", nrow=1) +
  labs(x="Uncertainty", y="States") +
  theme_bw()

# [그림 7-10]
windows(width=7.0, height=5.5)
plot(gmm, what="density")

# [그림 7-11]
library(factoextra)
p1 <- fviz_mclust(gmm, what="BIC", palette="jco", ggtheme=theme_classic())
p2 <- fviz_mclust(gmm, what="classification", 
                  geom=c("point", "text"), pointsize=1, labelsize=7, 
                  palette="jco", ggtheme=theme_classic(), show.legend.text=FALSE) +
  theme(legend.position="bottom")
p3 <- fviz_mclust(gmm, what="uncertainty", palette="npg", ggtheme=theme_classic()) +
  theme(legend.position="bottom")
library(patchwork)
windows(width=8.5, height=8.5)
p1 / (p2 + p3)
