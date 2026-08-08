
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

#######################
## 제10장 추천시스템 ##
#######################

###############
## 10.5 사례 ##
###############

## 조크

library(recommenderlab)
data(package="recommenderlab")
data.recom <- data(package="recommenderlab")
data.recom$results[,"Item"]

data(Jester5k)
class(Jester5k)
Jester5k
nratings(Jester5k)

?ratingMatrix

as(Jester5k, "matrix")[1:10, 1:10]
as(Jester5k, "matrix")[4990:5000, 91:100]

as(Jester5k[5000,], "list")
rowCounts(Jester5k[5000,])

rowMeans(Jester5k[5000,])

mean(rowMeans(Jester5k))

# [그림 10-7]
windows(width=7.0, height=5.5)
library(ggplot2)
library(scales)
ggplot(data=data.frame(x=getRatings(Jester5k)), aes(x=x)) +
  geom_histogram(bins=100, color="salmon", fill="mistyrose") +
  scale_y_continuous(labels=comma) +
  labs(title="Distribution of Ratings", x="Ratings", y="Frequency") +
  theme_classic()

# [그림 10-8]
p1 <- ggplot(data=data.frame(x=getRatings(normalize(Jester5k, method="center"))), aes(x=x)) +
  geom_histogram(bins=100, color="navyblue", fill="cornflowerblue") +
  scale_y_continuous(labels=comma) +
  labs(title="Distribution of Ratings: Mean Centering", x="Ratings", y="Frequency") +
  theme_classic()
p2 <- ggplot(data=data.frame(x=getRatings(normalize(Jester5k, method="Z-score"))), aes(x=x)) +
  geom_histogram(bins=100, color="darkred", fill="tomato") +
  scale_y_continuous(labels=comma) +
  labs(title="Distribution of Ratings: Z-score Standardization", x="Ratings", y="Frequency") +
  theme_classic()
windows(width=9.0, height=4.5)
library(patchwork)
p1 + p2

# [그림 10-9]
p1 <- ggplot(data=data.frame(x=rowCounts(Jester5k)), aes(x=x)) +
  geom_histogram(bins=100, color="darkgreen", fill="olivedrab") +
  scale_y_continuous(labels=comma) +
  labs(title="Distribution of Number of Ratings per User", 
       x="Number of ratings", y="Frequency") +
  theme_classic()
p2 <- ggplot(data=data.frame(x=colCounts(Jester5k)), aes(x=x)) +
  geom_histogram(bins=100, color="darkorange3", fill="orange") +
  scale_x_continuous(labels=comma) +
  labs(title="Distribution of Number of Ratings per Joke", 
       x="Number of ratings", y="Frequency") +
  theme_classic()
windows(width=9.0, height=4.5)
library(patchwork)
p1 + p2

# [그림 10-10]
windows(width=7.0, height=5.5)
ggplot(data=data.frame(x=colMeans(Jester5k)), aes(x=x)) +
  geom_histogram(color="darkgreen", fill="khaki3") +
  scale_y_continuous(breaks=1:10) +
  labs(title="Distribution of Mean Ratings for Each Joke", 
       x="Mean ratings", y="Frequency") +
  theme_classic()

recommenderRegistry$get_entry_names()

recommenderRegistry$get_entries(dataType="realRatingMatrix")

jokes.recom <- Recommender(data=Jester5k[1:4500], method="UBCF")
class(jokes.recom)
jokes.recom

getModel(jokes.recom)

jokes.recom <- Recommender(Jester5k[1:4500], method="UBCF",
                           parameter=list(method="pearson", nn=10, normalize="Z-score"))
getModel(jokes.recom)

jokes.pred <- predict(object=jokes.recom, newdata=Jester5k[4501:4502])
class(jokes.pred)
jokes.pred

as(jokes.pred, "list")

jokes.pred <- predict(jokes.recom, newdata=Jester5k[4501:4502], n=30)
jokes.pred
as(jokes.pred, "list")

jokes.pred <- bestN(jokes.pred, n=3)
as(jokes.pred, "list")

jokes.pred <- predict(jokes.recom, newdata=Jester5k[4501:4502], type="ratings")
jokes.pred
as(jokes.pred, "matrix")[,1:10]

as(Jester5k, "matrix")[4501:4502, 1:10]

as(jokes.pred, "list")

set.seed(123)
eval.scheme <- evaluationScheme(data=Jester5k, method="split", train=0.9, given=10)
class(eval.scheme)
eval.scheme

ubcf.recom <- Recommender(getData(eval.scheme, type="train"), method="UBCF")
ubcf.recom
ibcf.recom <- Recommender(getData(eval.scheme, type="train"), method="IBCF")
ibcf.recom

ubcf.pred <- predict(ubcf.recom, getData(eval.scheme, "known"), type="ratings")
ubcf.pred
ibcf.pred <- predict(ibcf.recom, getData(eval.scheme, "known"), type="ratings")
ibcf.pred

as(ubcf.pred, "matrix")[1,]

as(getData(eval.scheme, "known"), "matrix")[1,]

as(getData(eval.scheme, "unknown"), "matrix")[1,]

as(getData(eval.scheme, "unknown"), "matrix")[1, "j5"]
as(ubcf.pred, "matrix")[1, "j5"]
as(getData(eval.scheme, "unknown"), "matrix")[1, "j5"] - as(ubcf.pred, "matrix")[1, "j5"]
as(getData(eval.scheme, "unknown"), "matrix")[1,] - as(ubcf.pred, "matrix")[1,]

sqrt(mean((as(getData(eval.scheme, "unknown"), "matrix") - as(ubcf.pred, "matrix"))^2, na.rm=TRUE))
mean((as(getData(eval.scheme, "unknown"), "matrix") - as(ubcf.pred, "matrix"))^2, na.rm=TRUE)
mean(abs(as(getData(eval.scheme, "unknown"), "matrix") - as(ubcf.pred, "matrix")), na.rm=TRUE)

error <- rbind(
  calcPredictionAccuracy(ubcf.pred, getData(eval.scheme, "unknown")),
  calcPredictionAccuracy(ibcf.pred, getData(eval.scheme, "unknown"))
  )
rownames(error) <- c("UBCF", "IBCF")
error

set.seed(123)
eval.scheme <- evaluationScheme(Jester5k[1:1000], method="cross-validation", k=5, 
                                given=10, goodRating=5)
eval.scheme

eval.results <- evaluate(eval.scheme, method="UBCF", 
                         type="topNList", n=c(1,3,5,10,15,20))
class(eval.results)

getConfusionMatrix(eval.results)
avg(eval.results)

# [그림 10-11]
windows(width=9.0, height=4.5)
par(mfrow=c(1, 2))
plot(x=eval.results, y="ROC", col="blue", lwd=2, annotate=TRUE, main="ROC Curve")
plot(x=eval.results, y="prec/rec", col="red", lwd=2, 
     annotate=TRUE, main="Precision/Recall Curve")

set.seed(123)
eval.scheme <- evaluationScheme(Jester5k[1:1000], method="cross-validation", k=5, 
                                given=10, goodRating=5)
eval.results <- evaluate(eval.scheme, method="UBCF", type="ratings")
getConfusionMatrix(eval.results)
avg(eval.results)

# [그림 10-12]
windows(width=4.5, height=4.5)
plot(eval.results)

set.seed(123)
eval.scheme <- evaluationScheme(Jester5k[1:1000], method="split", 
                                train=0.9, given=20, goodRating=5)
eval.scheme

algorithms <- list(
  "random items"=list(name="RANDOM", param=list(normalize="Z-score")),
  "popular items"=list(name="POPULAR", param=list(normalize="Z-score")),
  "user-based CF"=list(name="UBCF", param=list(normalize="Z-score", 
                                               method="pearson", nn=200)),
  "item-based CF"=list(name="IBCF", param=list(normalize="Z-score",
                                               method="pearson", k=200))
  ) 
eval.results <- evaluate(eval.scheme, method=algorithms, n=c(1,3,5,10,15,20))

avg(eval.results)

# [그림 10-13]
windows(width=10.0, height=5.5)
par(mfrow=c(1, 2))
plot(x=eval.results, y="ROC", lwd=2, annotate=c(3, 4))
title("ROC Curve")
plot(x=eval.results, y="prec/rec", lwd=2, annotate=c(3, 4))
title("Precision/Recall Curve")

## 영화

library(recommenderlab)
data(MovieLense)
MovieLense

as(MovieLense[1,], "list")[[1]][1:10]

as(MovieLense, "matrix")[1:5, 1:4]

table(getRatings(MovieLense))
mean(rowMeans(MovieLense))

# [그림 10-14]
library(ggplot2)
p1 <- ggplot(data=data.frame(x=rowCounts(MovieLense)), aes(x=x)) +
  geom_histogram(bins=50, color="skyblue4", fill="turquoise") +
  labs(title="Distribution of Number of Ratings per User", 
       x="Number of ratings", y="Frequency") +
  theme_classic()
p2 <- ggplot(data=data.frame(x=colCounts(MovieLense)), aes(x=x)) +
  geom_histogram(bins=50, color="violetred4", fill="thistle") +
  labs(title="Distribution of Number of Ratings per Movie", 
       x="Number of ratings", y="Frequency") +
  theme_classic()
windows(width=9.0, height=4.5)
library(patchwork)
p1 + p2
sum(rowCounts(MovieLense) >= 500)
sum(colCounts(MovieLense) <= 1)

# [그림 10-15]
p1 <- ggplot(data=data.frame(x=colMeans(MovieLense)), aes(x=x)) +
  geom_histogram(bins=50, color="khaki4", fill="lemonchiffon") +
  labs(title="Distribution of Average Ratings", 
       x="Average ratings", y="Frequency") +
  theme_classic()
p2 <- ggplot(data=data.frame(x=colMeans(MovieLense)[colCounts(MovieLense) > 100]), 
       aes(x=x)) +
  geom_histogram(bins=50, color="pink4", fill="peachpuff") +
  labs(title="Distribution of Relevant Average Ratings", 
       x="Average ratings", y="Frequency") +
  theme_classic()
windows(width=9.0, height=4.5)
library(patchwork)
p1 + p2

# [그림 10-16]
windows(width=7.0, height=5.5)
image(MovieLense[rowCounts(MovieLense) > quantile(rowCounts(MovieLense), 0.99),
                 colCounts(MovieLense) > quantile(colCounts(MovieLense), 0.99)],
      col.regions=heat.colors(18, rev=TRUE), 
      main="Heatmap of Top Users and Movies")

set.seed(123)
eval.scheme <- evaluationScheme(MovieLense, method="split", train=0.8, 
                                given=10, goodRating=4)
eval.scheme
algorithms <- list(
  "item-based CF: pearson"=list(name="IBCF", param=list(method="pearson")),
  "item-based CF: cosine"=list(name="IBCF", param=list(method="cosine")),
  "user-based CF: pearson"=list(name="UBCF", param=list(method="pearson")),
  "user-based CF: cosine"=list(name="UBCF", param=list(method="cosine"))
  )

eval.results <- evaluate(eval.scheme, algorithms, n=c(1,3,5,10,15,20))

# [그림 10-17]
windows(width=10.0, height=5.5)
par(mfrow=c(1, 2))
plot(x=eval.results, y="ROC", lwd=2, annotate=c(2, 4), legend="topleft")
title("ROC Curve")
plot(x=eval.results, y="prec/rec", lwd=2, annotate=c(2, 4))
title("Precision/Recall Curve")

set.seed(123)
eval.scheme <- evaluationScheme(MovieLense, method="split", train=0.8, 
                                given=10, goodRating=4)
algorithms <- list(
  "user-based CF10: cosine"=list(name="UBCF", param=list(method="cosine", nn=10)),
  "user-based CF20: cosine"=list(name="UBCF", param=list(method="cosine", nn=20)),
  "user-based CF30: cosine"=list(name="UBCF", param=list(method="cosine", nn=30)),
  "user-based CF40: cosine"=list(name="UBCF", param=list(method="cosine", nn=40)),
  "user-based CF50: cosine"=list(name="UBCF", param=list(method="cosine", nn=50)),
  "user-based CF60: cosine"=list(name="UBCF", param=list(method="cosine", nn=60)),
  "user-based CF70: cosine"=list(name="UBCF", param=list(method="cosine", nn=70))
  )
eval.results <- evaluate(eval.scheme, algorithms, n=c(1,3,5,10,15,20))

# [그림 10-18]
windows(width=12.0, height=5.5)
par(mfrow=c(1, 2))
plot(x=eval.results, y="ROC", lwd=2, annotate=c(1, 2, 3), legend="topleft")
title("ROC Curve")
plot(x=eval.results, y="prec/rec", lwd=2, annotate=c(1, 2, 3))
title("Precision/Recall Curve")

movie.recom <- Recommender(MovieLense[1:800], method="UBCF",
                           parameter=list(method="cosine", nn=10))

movie.pred <- predict(movie.recom, newdata=MovieLense[801:802], n=5)
movie.pred
as(movie.pred, "list")

MovieLense.b <- binarize(MovieLense, minRating=1)

class(MovieLense.b)
as(MovieLense.b, "matrix")[1:5, 1:4]

movie.recom <- Recommender(MovieLense.b[1:800], method="IBCF",
                           parameter=list(method="jaccard", k=20))

movie.pred <- predict(movie.recom, newdata=MovieLense.b[801:802], n=5)
movie.pred
as(movie.pred, "list")
