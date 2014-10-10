setwd("E:\\專題\\R\\Textmining\\Facebook詞雲案例\\天蠍座") #斜線要改成\\或是/ #斜線要改成\\或是/
library(Rfacebook)
library(tmcn)
library(tm)
library(Rwordseg)
library(wordcloud)
####第一部分步爬文#######################################################################
load("fb_oauth")
#製作主題(尋找要做的粉絲專頁)
page1 <- getPage(page="star.star.08", n = 100, token = fb_oauth, feed=TRUE)#搜尋幾篇發文
list1 <- list()

for (i in 1:nrow(page1)) {
  list1[[i]] <- getPost(post = page1$id[i], token = fb_oauth, n = 80)
  Sys.sleep(abs(rnorm(1, 1, 0.5)))}
#n=要讀多少粉絲寫的(回覆)
#Sys.sleep系統內建的sleep資料，分隔搜尋時間，不然別人可能會以為是駭客
list2 <- sapply(list1, FUN = function(X) {res <- X$comments;res$post_id <- X$post$id;res})
#留言 id 按讚次數
list3 <- list2[sapply(list2, length) > 1]
#將list2中文字長度要大於1
comment1 <- do.call("rbind", list3)
#將list3變成表單
write.csv(page1, file = "postScorpio.csv", row.names = FALSE)
write.csv(comment1, file = "commentScorpio.csv", row.names = FALSE)

####第二部分製作詞雲#######################################################################
post1 <- read.csv("postScorpio.csv", stringsAsFactors = FALSE)
#讀取csv檔，若設true會將文字變數字=>亂碼
post1 <- post1[!is.na(post1$message), ]
#刪除空格或空行
words1 <- segmentCN(post1$message)
#會把一段字，自行分段，segment是段落的意思
words1 <- unlist(words1)
#簡化
words1 <- gsub("[A-z]", "", words1)
#清除post1中的英文
words1 <- words1[nchar(words1) > 1]
#設定固定長度(至少大於1)
table1 <- table(words1)
#設一個table
wordsDf1 <- data.frame(WORD = names(table1), FREQ = as.vector(table1), stringsAsFactors = FALSE)
#wordsDf1 <- data.frame是做成數據框的意思，freq 文字出現頻率
pdf(file = "postlover.pdf", family = "CNS1")
#以pdf檔呈現，family = "CNS1":central nervous system:中樞神經系統的表式方式，是利用這種表示方式去編排的
wordcloud(wordsDf1$WORD, wordsDf1$FREQ, col = rainbow(length(wordsDf1$FREQ)))
#文字 頻率 顏色(針對頻率區分:彩虹)
wordcloud(wordsDf1$WORD, wordsDf1$FREQ,min.freq=2,random.order=F,ordered.colors=F,colors = rainbow(length(wordsDf1$FREQ)))
wordcloud(wordsDf1$WORD, wordsDf1$FREQ,min.freq=5,random.order=F,ordered.colors=F,colors = rainbow(length(wordsDf1$FREQ)))
dev.off()#裝置關閉;上面只是將pdf檔生成未包含內容需加上此指令將內容釋出

comment1 <- read.csv("commentScorpio.csv", stringsAsFactors = FALSE)
words2 <- segmentCN(comment1$message)
#粉絲資料
words2 <- unlist(words2)
words2 <- gsub("[A-z]", "", words2)
words2 <- words2[nchar(words2) > 1]
table2 <- table(words2)
wordsDf2 <- data.frame(WORD = names(table2), FREQ = as.vector(table2), stringsAsFactors = FALSE)
wordsDf2 <- wordsDf2[wordsDf2$FREQ > 2, ]
pdf(file = "commentlover.pdf", family = "CNS1")
wordcloud(wordsDf2$WORD, wordsDf2$FREQ, col = rainbow(length(wordsDf2$FREQ)))
wordcloud(wordsDf2$WORD, wordsDf2$FREQ,min.freq=5,random.order=F,ordered.colors=F,colors = rainbow(length(wordsDf2$FREQ)))
dev.off()

####第三部分增加詞與刪減詞#######################################################################
#以粉絲回應的資料為例#
comment1 <- read.csv("commentScorpio.csv", stringsAsFactors = FALSE)

#新增詞#
insertedwords <- c("天蠍座" )
#insertedwords新增詞，新增"趙藤雄"一詞，不加3字會被拆開
insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))
#big5:繁體編碼；UTF-8:代碼呈現方式

#斷詞#
d.corpus1<- segmentCN(comment1$message)
d.corpus <- Corpus(VectorSource(d.corpus1 ))
#建構向量為輸入源，構成文章

#去除不需要的字#
myStopWords <- c(stopwordsCN(), "真的","哈哈哈","分享","因為", "比較", "這樣", "目前", "所以", "覺得","變成","這個","沒有","一個","天蠍座","蠍子","全文","可以","他們","25","分析","自己","喜歡","我們","德國","阿根廷")
#tmcn是大陸開發，tm以及Rwordseg為英文的
#stopwordsCN打在下方，可以看到已存在的詞(我看不到Q__Q)

words24 <- tm_map(d.corpus, removeWords, myStopWords)
#tm_map清除標點符號
words24 <- tm_map(words24, function(word) {gsub("[A-Za-z0-9]", "", word)})
#清除英文、數字
words2 <- unlist(words24)
#簡化words24
words2 <- gsub("[A-z]", "", words2)
words2 <- words2[nchar(words2) > 1]
#需字長大於1
table2 <- table(words2)
wordsDf2 <- data.frame(WORD = names(table2), FREQ = as.vector(table2), stringsAsFactors = FALSE)
wordsDf2 <- wordsDf2[wordsDf2$FREQ >2, ]
#需頻率大於2
pdf(file = "comment2Scorpio.pdf", family = "CNS1")
wordcloud(wordsDf2$WORD, wordsDf2$FREQ, col = rainbow(length(wordsDf2$FREQ)))
wordcloud(wordsDf2$WORD, wordsDf2$FREQ,min.freq=5,random.order=F,ordered.colors=F,colors = rainbow(length(wordsDf2$FREQ)))
dev.off()

####第四部分關聯分析#######################################################################
#以專業PO文資料為例#
post1 <- read.csv("postScorpio.csv", stringsAsFactors = FALSE)
post1 <- post1[!is.na(post1$message), ]
#刪除空格或空行
#斷詞#
d.corpus1<- segmentCN(post1$message)
d.corpus <- Corpus(VectorSource(d.corpus1 ))#建構向量為輸入源，構成文章
#去除不需要的字#
myStopWords <- c(stopwordsCN(), "真的","哈哈哈","分享","因為", "比較", "這樣", "目前", "所以", "覺得","變成","這個","沒有","一個","全文","可以","他們","25","分析","自己","喜歡","我們","德國","阿根廷")
words24 <- tm_map(d.corpus, removeWords, myStopWords)
#tm_map清除標點符號
words24 <- tm_map(words24, function(word) {gsub("[A-Za-z0-9]", "", word)})

#關聯分析#
d.vec <- sapply(words24, paste, collapse = " ")
#讀word24貼入以空格方式去切割
d.vec <- unique(d.vec)
#以單一形式讀入ex.阿信出現10次，僅算一次
d.corpus2 <- Corpus(VectorSource(d.vec))
#VectorSource向量方式呈現
d.corpus2
inspect(d.corpus2[1])
#這個指令可以直接將看想將第幾行的字列出，打上想要的行就會出現那一行的字
d.dtm <- DocumentTermMatrix(d.corpus2)
#將處理後的詞庫進行斷字處理，生成詞頻權重矩陣，生成詞雲矩陣，最大長度為5
d.dtm
findFreqTerms(d.dtm, 1)
findFreqTerms(d.dtm, 2)
#抓出頻率超過1次者
findAssocs(d.dtm, "天蠍座", 0.1)
#用findAssocs找出最常與"天蠍座"關聯程度；關聯程度為0.1以上的詞

###以下可以調整相關程度，並將其移除，可以選擇不做###
#生成的矩陣是一個稀疏矩陣，可再進行降維處理，之後轉為標準數據框格格式
#我們可以去掉某些出現頻次太低的詞
dtm1<- removeSparseTerms(d.dtm, sparse=0.9)
#移除關聯程度0.9以上的
inspect(dtm1)
data <- as.data.frame(inspect(dtm1))
####################################################
#開始做集群#
data <- as.data.frame(inspect(d.dtm))
data.scale <- scale(data)
#把data做尺度
d <- dist(data.scale, method = "euclidean")
#用歐基里得距離
fit <- hclust(d,method="ward")
#聚類分析，繪製聚類圖
png(paste("Scorpiotermcluster-1", ".png", sep = ''), width=10, height=10,units="in", res=700)
plot(fit)
dev.off()
plot(fit,labels = FALSE,main ="文件聚類分析")
plot(fit)
###以下是更漂亮的集群分析###########################
dtm01 <- weightTfIdf(d.dtm)
N = 0.95 #這是什麼?? 
dtm02 <- removeSparseTerms(dtm01,N);dtm02
# 注意，為展示方便，調整dtm02詞語數量為50  
tdm = as.TermDocumentMatrix(dtm02)#刪除文件稀少的字詞
tdm <- weightTfIdf(tdm)
mydata.df <- as.data.frame(inspect(tdm))
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
png(paste("Scorpiotermcluster-2", ".png", sep = ''), width=10, height=10,units="in", res=700)
#寬度、高度、解析度
plot(fit) 
dev.off()

####第五部分主成分分析#######################################################################
#以專業PO文資料為例#
post1 <- read.csv("postScorpio.csv", stringsAsFactors = FALSE)
post1 <- post1[!is.na(post1$message), ]
#斷詞#
d.corpus1<- segmentCN(post1$message)
d.corpus <- Corpus(VectorSource(d.corpus1 ))#建構向量為輸入源，構成文章
#去除不需要的字#
myStopWords <- c(stopwordsCN(), "真的","哈哈哈","分享","因為", "比較", "這樣", "目前", "所以", "覺得","變成","這個","沒有","一個","天蠍座","蠍子","全文","可以","他們","25","分析","自己","喜歡","我們","德國","阿根廷")
words24 <- tm_map(d.corpus, removeWords, myStopWords)
#tm_map清除標點符號
words24 <- tm_map(words24, function(word) {gsub("[A-Za-z0-9]", "", word)})

#關聯分析#
d.vec <- sapply(words24, paste, collapse = " ")
#讀word24貼入以空格方式去切割
d.vec <- unique(d.vec)
#以單一形式讀入ex.阿信出現10次，僅算一次
d.corpus2 <- Corpus(VectorSource(d.vec))
#VectorSource向量方式呈現
d.corpus2
inspect(d.corpus2[1])
#這個指令可以直接將看想將第幾行的字列出，打上想要的行就會出現那一行的字
d.dtm <- DocumentTermMatrix(d.corpus2)
#將處理後的詞庫進行斷字處理，生成詞頻權重矩陣，生成詞雲矩陣，最大長度為5
d.dtm


##執行主成分分析
k <- princomp(d.dtm)
summary(k,loadings =TRUE)
plot(k$sdev,type="l")
screeplot(k,npcs=6,type='lines')
biplot(k)  
#雙標圖 越射出來的表式個數越多

#以下是老師主成分分析程式碼
ozMat <- TermDocumentMatrix(makeChunks(d.corpus2),list(weighting = weightBin))
#關鍵字為列，文件是行的矩陣。數據塊，邊長50單位
k <- princomp(as.matrix(ozMat), features = 2)
#執行主成分分析
screeplot(k,npcs=6,type='lines')
summary(k)
biplot(k)  
#雙標圖 越射出來的表式個數越多
