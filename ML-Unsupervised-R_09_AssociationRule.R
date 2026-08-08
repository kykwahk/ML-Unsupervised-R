
#####################################
## R을 이용한 머신러닝: 비지도학습 ##
## (곽기영, 도서출판 청람)         ## 
#####################################

########################
## 제9장 장바구니분석 ##
########################

##############
## 9.4 사례 ##
##############

## 식료품 쇼핑

library(arules)
data(Groceries)
class(Groceries)
Groceries

summary(Groceries)

inspect(Groceries[1:3])

itemFrequency(Groceries[,1:3])

sort(itemFrequency(Groceries), decreasing=TRUE)[1:10]

# [그림 9-6]
windows(width=7.0, height=5.5)
itemFrequencyPlot(Groceries, support=0.1, type="relative", col="cornflowerblue")

# [그림 9-7]
windows(width=7.0, height=5.5)
itemFrequencyPlot(Groceries, topN=20, type="absolute", col="maroon")

# [그림 9-8]
windows(width=7.0, height=5.5)
image(Groceries[1:30])

# [그림 9-9]
windows(width=7.0, height=5.5)
image(sample(Groceries, 100))

groc.rule <- apriori(Groceries, 
                     parameter=list(support=0.01, confidence=0.3, minlen=2))
class(groc.rule)
groc.rule

summary(groc.rule)

# [그림 9-10]
library(arulesViz)
windows(width=7.0, height=5.5)
plot(groc.rule, method="scatterplot")

# [그림 9-11]
windows(width=7.0, height=7.0)
library(RColorBrewer)
plot(groc.rule, method="grouped", 
     control=list(col=rev(brewer.pal(9, "Greens")[4:9])))

inspect(groc.rule[1:5])

inspect(sort(groc.rule, by="lift")[1:5], linebreak=TRUE)

inspect(subset(groc.rule, subset=items %in% "berries"), linebreak=TRUE)

inspect(subset(groc.rule, subset=items %in% c("berries", "curd"), linebreak=TRUE))

inspect(subset(groc.rule, subset=items %ain% c("yogurt", "curd"), linebreak=TRUE))

veggie.rule <- subset(groc.rule, subset=rhs %pin% "vegetables" & lift > 3)

# [그림 9-12]
windows(width=7.0, height=5.5)
plot(veggie.rule, method="graph")

write(groc.rule, file="groceryrule.csv", sep=",", quote=TRUE, row.names=FALSE)

groc.rule.df <- as(groc.rule, "data.frame")
str(groc.rule.df)
head(groc.rule.df)

## 벨기에 수퍼마켓

url <- "https://raw.githubusercontent.com/kykwahk/ML-Unsupervised-R/main/retail.dat"
retail.base <- readLines(url)

head(retail.base, 3)
tail(retail.base, 3)
summary(retail.base)

retail.list <- strsplit(retail.base, " ")
retail.list <- lapply(retail.list, unique)
head(retail.list, 3)

names(retail.list) <- paste("trans", 1:length(retail.list), sep="")
head(retail.list, 3)
str(retail.list)

library(arules)
retail.trans <- as(retail.list, Class="transactions")
class(retail.trans)
summary(retail.trans)

# [그림 9-14]
windows(width=7.0, height=5.5)
itemFrequencyPlot(retail.trans, support=0.03, cex.names=0.8, 
                  horiz=TRUE, col="turquoise3",
                  xlab="Proportion of Market Baskets Containing Items")

data <- paste(
  "# this is transaction basket data", 
  "item1, item2", 
  "item1", 
  "item2, item3", 
  sep="\n")
cat(data)
write(data, file="trans_basket")
tr <- read.transactions("trans_basket", format="basket", sep=",", skip=1)
inspect(tr)

data <- paste(
  "trans1 item1", 
  "trans2 item1",
  "trans2 item2", 
  "trans3 item3",
  sep="\n")
cat(data)
write(data, file="trans_single")
tr <- read.transactions("trans_single", format="single", cols=c(1, 2))
inspect(tr)

url <- "https://raw.githubusercontent.com/kykwahk/ML-Unsupervised-R/main/retail.dat"
retail.trans <- read.transactions(url, format="basket", sep="", rm.duplicates=TRUE)
class(retail.trans)
summary(retail.trans)

retail.rule <- apriori(retail.trans,
                       parameter=list(support=0.001, confidence=0.4, minlen=2))

# [그림 9-15]
windows(width=7.0, height=5.5)
library(arulesViz)
plot(retail.rule)

# [그림 9-16]
windows(width=7.0, height=5.5)
plot(retail.rule, engine="interactive", control=list(col=topo.colors(9)))

retail.top <- head(sort(retail.rule, by="lift"), 30)
inspect(head(retail.top, 5))

# [그림 9-17]
windows(width=7.0, height=5.5)
plot(retail.top, method="graph")

# [그림 9-18]
plot(retail.top, method="graph", engine="interactive")

## 인구센서스

library(arules)
data(AdultUCI)
str(AdultUCI)
AdultUCI[1:3,]

adult <- AdultUCI
adult[["fnlwgt"]] <- NULL
adult[["education-num"]] <- NULL

adult[["age"]] <- cut(adult[["age"]],
                      breaks=c(15,25,45,65,100),
                      labels=c("Young", "Middle-aged", "Senior", "Old"),
                      right=TRUE, ordered_result=TRUE)

summary(adult$age)

adult[["hours-per-week"]] <- cut(adult[["hours-per-week"]],
                                 breaks=c(0,25,40,60,168),
                                 labels=c("Part-time", "Full-time",
                                          "Over-time", "Workaholic"),
                                 right=TRUE, ordered_result=TRUE)
adult[["capital-gain"]] <- cut(adult[["capital-gain"]],
                               breaks=c(-Inf, 0, 
                                        median(adult[["capital-gain"]]
                                               [adult[["capital-gain"]] > 0]), Inf),
                               labels=c("None", "Low", "High"),
                               right=TRUE, ordered_result=TRUE)
adult[["capital-loss"]] <- cut(adult[["capital-loss"]],
                               breaks=c(-Inf, 0,
                                        median(adult[["capital-loss"]]
                                               [adult[["capital-loss"]] > 0]), Inf),
                               labels=c("none", "low", "high"),
                               right=TRUE, ordered_result=TRUE)
adult[1:3,]

adult.trans <- as(adult, "transactions")
adult.trans
summary(adult.trans)

as(adult.trans, "matrix")[1:2,]

# [그림 9-19]
windows(width=7.0, height=5.5)
itemFrequencyPlot(adult.trans, support=0.3, horiz=TRUE, col="coral2")

adult.rule <- apriori(adult.trans, parameter=list(supp=0.01, conf=0.65, minlen=2))
summary(adult.rule)

# [그림 9-20]
library(arulesViz)
windows(width=7.0, height=5.5)
plot(adult.rule)
plot(adult.rule, engine="interactive")

adult.small <- subset(adult.rule, subset=rhs %in% "income=small" & lift > 1.3)
inspect(sort(adult.small, by="lift")[1:3])

# [그림 9-21]
windows(width=7.0, height=5.5)
adult.small10 <- head(sort(adult.small, by="lift"), 10)
plot(adult.small10, method="graph")

adult.large <- subset(adult.rule, subset=rhs %in% "income=large" & lift > 1.3)
inspect(sort(adult.large, by="lift")[1:3])

# [그림 9-22]
windows(width=7.0, height=5.5)
adult.large10 <- head(sort(adult.large, by="lift"), 10)
plot(adult.large10, method="graph")

adult.large11to20 <- sort(adult.large, by="lift")[11:20]
inspect(adult.large11to20)
