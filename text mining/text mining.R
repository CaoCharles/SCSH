# 整理資料 --------------------------------------------------------------------

data <- read.csv("federalist.csv")
a <- which(data$author=="HAMILTON")
b <- which(data$author=="MADISON")
c <- which(data$author=="HAMILTON OR MADISON")
newdata <- data[c(a,b,c),]

# 資料整理 -----------------------------------------------------------------------
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

# create document-term matrix
writeLines(as.character(IC[[1]]))
ICdtm = DocumentTermMatrix(IC, control = list(weighting=weightTfIdf))

# ICdtm = DocumentTermMatrix(IC)
nrow(ICdtm);ncol(ICdtm)
str(ICdtm)

# 整理H和M的字數頻率 --------------------------------------------------------------
# H先生饌寫 -----------------------------------------------------------------------

Hdtm <- ICdtm[1:51,]
counts <- as.matrix(Hdtm)
freq <- colSums(counts)
Ho <- order(freq, decreasing = T)
freq <- freq[Ho]
counts <- counts[,Ho]
barplot(freq[1:20], las = 2 ,main ="Most frequent words", ylab = "word drequencies")

# M先生饌寫 -------------------------------------------------------------------

Mdtm <- ICdtm[52:65,]
counts <- as.matrix(Mdtm)
freq <- colSums(counts)
Mo <- order(freq, decreasing = T)
freq <- freq[Mo]
counts <- counts[, Mo]
length(counts)
dim(ICdtm)
barplot(freq[1:20], las = 2 ,main ="Most frequent words", ylab = "word drequencies")

# 建立分類矩陣 ------------------------------------------------------------------
# num <- union(Ho[1:30], Mo[1:30])
# num <- base::intersect(Ho[1:100], Mo[1:100])
num <- setdiff(Ho[1:20],Mo[1:20]) # 互斥項
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
which(colnames(Main)=="upon")
which(colnames(Main)=="military")
train <- cbind(Main[1:65,],a[1:65])
test <- cbind(Main[66:77,],a[66:77])
ncol(train)

# 更改依變數座標 -----------------------------------------------------------------

train <- as.data.frame(train)
train[,ncol(train)] <- as.factor(train[,ncol(train)])
test <- as.data.frame(test)
test[,ncol(train)] <- as.factor(test[,ncol(train)])

# 建立分類模型 ------------------------------------------------------------------

# 隨機森林 --------------------------------------------------------------------

library(randomForest)
rf <- randomForest(formula = train[,ncol(train)]~. ,
                   data = train[,-(ncol(train))],
                   importane = T, 
                   proximity = T,
                   v = (ncol(train)),
                   ntree = 10000)
rf
# 錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(rf) 

# OOB錯誤率
C <- table(train[,ncol(train)],rf$predicted, dnn = c("實際", "預測"))
C
1-sum(diag(C))/sum(C)

# 預測
pred <- predict(rf,newdata = test[,-ncol(train)])
pred


# 決策樹 ---------------------------------------------------------------------

library(rpart)
library(rpart.plot )
control<-rpart.control(cp=0.001,minsplit = 1,minbucket = 1)
treeorig<-rpart(train[,ncol(train)]~., 
                data = train[,-ncol(train)],
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
x <- model.matrix(train[,ncol(train)]~ ., train[,-ncol(train)])
y <- train[,ncol(train)]
cvfit <- cv.glmnet(x, y, family="binomial", type.measure="class", nfolds=10)
summary(cvfit)
predict.value <- predict(cvfit, x, s = "lambda.min", type = "class")
C <- table(y, predict.value, dnn = c("實際", "預測"));C 
1-sum(diag(C))/sum(C)
x1 <- model.matrix(test[,ncol(train)]~ ., test[,-ncol(train)])
pred2 <- predict(cvfit, x1, s = "lambda.min", type = "class")
pred2
sum(1*(pred2==2))/12


# SVM ---------------------------------------------------------------------

library(e1071)
tobj <- tune.svm(train[,-ncol(train)],
                 train[,ncol(train)], 
                 data=train, 
                 cost= 10*(1:10), 
                 gamma=0.0001*(1:10))
summary(tobj)

plot(tobj, xlab = "gamma", ylab = "C")

s1<-svm(train[,-ncol(train)],train[,ncol(train)], 
        data=train, cost= 100, gamma=0.001,cross=64)
s1
preds1<-predict(s1,train[,-ncol(train)])
preds1
table(train[,ncol(train)],preds1,dnn = c("實際", "預測"))

preds2 <- predict(s1, test[,-ncol(train)])

sum(preds2==2)/12


# LDA ---------------------------------------------------------------------

library(corrplot)
corr.matrix <- cor(train[,-ncol(train)])
corrplot.mixed(upper = "circle",corr.matrix)

# 會有共線性可能是片語一起出現
library(MASS)
lda<-lda(train[,ncol(train)]~.,data=train[,-ncol(train)])
predlda <- predict(lda,train[,-ncol(train)])
table(train[,ncol(train)], predlda$class)
which(predlda$class==2)
pred3 <- predict(lda,test[,-ncol(train)])
pred3$class


# k最近鄰居分類法 ----------------------------------------------------------------

# 安裝並載入class套件
library(class)
library(dplyr)

# 計算k值(幾個鄰居)通常可以用資料數的平方根
kv <- round(sqrt(65))
kv
# 建立模型 

prediction <- knn(train = train[,-ncol(train)], 
                  test = test[,-ncol(train)], 
                  cl = train[,ncol(train)], 
                  k = 8)

# 評估正確性
cm <- table(x = test[,ncol(train)], y = prediction, dnn = c("實際", "預測"))
cm

knnaccuracy <- sum(cm[1,2]) / sum(cm)
knnaccuracy


# 文字雲  ----------------------------------------------------------------------

# H先生的文字雲 -----------------------------------------------------------------

counts <- as.matrix(Hdtm)
freq <- colSums(counts)
o <- order(freq, decreasing = T)
freq <- freq[o]
counts <- counts[,o]
# wordcloud
library(wordcloud)

wordsFreq <- names(freq)
wordcloud(words = wordsFreq,
          freq = freq,
          random.order = F,
          max.words = 20,
          rot.per = 0.5,
          colors = brewer.pal(8,"Dark2"))


# M先生的文字雲 -----------------------------------------------------------------

counts <- as.matrix(Mdtm)
freq <- colSums(counts)
o <- order(freq, decreasing = T)
freq <- freq[o]
counts <- counts[,o]
# wordcloud
library(wordcloud)
wordsFreq <- names(freq)
wordcloud(words = wordsFreq,
          freq = freq,
          random.order = F,
          max.words = 20,
          rot.per = 0.5,
          colors = brewer.pal(8,"Dark2"))

