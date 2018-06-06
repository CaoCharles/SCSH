
# 整理資料 --------------------------------------------------------------------

data <- read.csv("federalist.csv")
a <- which(data$author=="HAMILTON")
b <- which(data$author=="MADISON")
c <- which(data$author=="HAMILTON OR MADISON")
newdata <- data[c(a,b,c),]

# 駱 -----------------------------------------------------------------------
library(slam)
library("NLP")

# read in data
x = data[c(a,b,c),]
str(x)

library(tm)
IC = Corpus(VectorSource(x$text))
writeLines(as.character(IC[[1]]))
?Corpus
str(IC)

IC = tm_map(IC, stripWhitespace)
IC = tm_map(IC, removePunctuation)
IC = tm_map(IC, tolower)
IC = tm_map(IC, removeNumbers)
IC = tm_map(IC, removeWords,c("c","p","l","s","q"))
IC = tm_map(IC, removeWords, stopwords("english"))
IC = tm_map(IC, removeWords, stemDocument)
# create document-term matrix
writeLines(as.character(IC[[1]]))
ICdtm = DocumentTermMatrix(IC,control = list(weighting=weightTfIdf))
ICdtm = DocumentTermMatrix(IC)
nrow(ICdtm);ncol(ICdtm)
str(ICdtm)

# 整理H和M的字數頻率 --------------------------------------------------------------
# H先生饌寫 -----------------------------------------------------------------------
Hdtm <- ICdtm[1:51,]
counts <- as.matrix(Hdtm)
freq <- colSums(counts)
Ho <- order(freq,decreasing = T)
freq <- freq[Ho]
counts <- counts[,Ho]
barplot(freq[1:50], las = 2 ,main ="Most frequent words", ylab = "word drequencies")


# M先生饌寫 -------------------------------------------------------------------
Mdtm <- ICdtm[52:65,]
counts <- as.matrix(Mdtm)
freq <- colSums(counts)
Mo <- order(freq,decreasing = T)
freq <- freq[Mo]
counts <- counts[,Mo]
length(counts)
dim(ICdtm)
barplot(freq[1:50], las = 2 ,main ="Most frequent words", ylab = "word drequencies")


# 建立分類矩陣 ------------------------------------------------------------------
# num <- union(Ho[1:30], Mo[1:30])
# num <- base::intersect(Ho[1:100],Mo[1:100])
num <- setdiff(Ho[1:100],Mo[1:100]) # 互斥項
data <- as.matrix(ICdtm)
nrow(data[,num]);ncol(data[,num])
a <- c()
for(i in 1:51){
  a <- c(a,0)
}
for(j in 52:65){
  a <- c(a,1)
}
for(k in 66:77){
  a <- c(a,1)
}
a <- as.factor(a)
a
Main <- data[,num]
train <- cbind(Main[1:65,],a[1:65])
test <- cbind(Main[66:77,],a[66:77])

# 更改依變數座標 -----------------------------------------------------------------

train <- as.data.frame(train)
train[,(length(num)+1)] <- as.factor(train[,(length(num)+1)])
test <- as.data.frame(test)
test[,(length(num)+1)] <- as.factor(test[,(length(num)+1)])
# 建立分類模型 ------------------------------------------------------------------
# 羅吉斯 ---------------------------------------------------------------------

model <- glm(V(length(num)+1)~. , family = binomial(logit), data = train)
predict(model,newdata = test)

# 隨機森林 --------------------------------------------------------------------

library(randomForest)
rf <- randomForest(formula = train[,(length(num)+1)]~. ,
                   data = train[,-(length(num)+1)],
                   importane = T, 
                   proximity = T,
                   v = (length(num)+1),
                   ntree = 10000)
rf
# 錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(rf) 

# OOB錯誤率
C <- table(train[,(length(num)+1)],rf$predicted)
1-sum(diag(C))/sum(C)

# 預測
pred <- predict(rf,newdata = test[,-(length(num)+1)])
pred


# 決策樹(放棄) ---------------------------------------------------------------------

library(rpart)
library(rpart.plot )
control<-rpart.control()
treeorig<-rpart(train[,(length(num)+1)]~., 
                data = train[,-(length(num)+1)],
                method="class",
                control=control)
plot(treeorig)
text(treeorig)
prp(treeorig,          # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2) 


# 多元羅吉斯 -------------------------------------------------------------------

library(glmnet)
x <- model.matrix(train[,(length(num)+1)]~ ., train[,-(length(num)+1)])
y <- train[,(length(num)+1)]
cvfit <- cv.glmnet(x, y, family="binomial", type.measure="class", nfolds=10)
summary(cvfit)
predict.value <- predict(cvfit, x, s = "lambda.min", type = "class")
C <- table(predict.value,y);C 
1-sum(diag(C))/sum(C)
x1 <- model.matrix(test[,(length(num)+1)]~ ., test[,-(length(num)+1)])
pred2 <- predict(cvfit, x1, s = "lambda.min", type = "class")
pred2
sum(1*(pred2==2))/12


# SVM ---------------------------------------------------------------------

library(e1071)
tobj <- tune.svm(train[,-(length(num)+1)],
                 train[,(length(num)+1)], 
                 data=train, 
                 cost= 100*(1:10), 
                 gamma=0.005*(1:10))
summary(tobj)

plot(tobj, xlab = "gamma", ylab = "C")

s1<-svm(train[,-(length(num)+1)],train[,(length(num)+1)], 
        data=train, cost= 500, gamma=0.005,cross=64)
s1
pred1<-predict(s1,train[,-(length(num)+1)])
pred1
table(train[,(length(num)+1)],pred1)

pred2 <- predict(s1, test[,-(length(num)+1)])

pred2


# LDA ---------------------------------------------------------------------

library(MASS)
lda<-lda(train[,(length(num)+1)]~.,data=train[,-(length(num)+1)])
predlda <- predict(lda,train[,-(length(num)+1)])
table(predlda$class,train[,(length(num)+1)])

pred3 <- predict(lda,test[,-(length(num)+1)])
pred3$class

# charles -----------------------------------------------------------------


library(tm)
library(magrittr)
library("NLP")
library(slam)
library(dplyr)
newdata$text%<>%as.character()
#先把不要的髒東西抓出來
IC <- Corpus(VectorSource(newdata$text))
writeLines(as.character(IC[[1]]))
IC
str(IC)
#清除標點符號#
#d.corpus <- tm_map(IC, removePunctuation)
#清除數字#
#d.corpus <- tm_map(IC, removeNumbers)
#轉換大小寫#
#d.corpus <- tm_map(IC, tolower)
#清除指定文字#
#IC <- tm_map(IC, removeWords,c("<c>","<p>","<l>","<S>","<Q>"))

#建立text matrix
tdm <- DocumentTermMatrix(IC, control =list(weighting=weightTfIdf,tolower=FALSE))
tdm
tdm <- DocumentTermMatrix(IC)
tdm
inspect(tdm[1:5,1:10])
# 分類 ----------------------------------------------------------------------

counts <- as.matrix(ICtdm)
freq <- colSums(counts)
o <- order(freq, decreasing = T)
freq <- freq[o]
counts <- counts[,o]
# wordcloud
library(wordcloud)
wordsFreq <- names(freq)
wordcloud(words = wordsFreq,
          freq = freq,
          min.freq = 181,
          random.order = F,
          rot.per = 0.1)

