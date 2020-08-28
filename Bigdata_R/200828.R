#219p t검정

x1 <- c(30,-5,55,-30,-20,45)
x2 <- c(12,13,12,13,12,13)
x3 <- c(30,-5,55,-30,-20,45,30,-5,55,-30,-20,45)

t.test(x1,alternative = "greater")
t.test(x2,alternative = "greater")
t.test(x3,alternative = "greater")

#222p t검정

session_times <- read.csv(file="C:/Users/User/Desktop/source/data/web_page_data.csv",header=TRUE)
session_times

t.test(Time~Page,data=session_times,alternative="less")

#229p 분산분석

four_sessions <- read.csv(file="C:/Users/User/Desktop/source/data/four_sessions.csv",header=TRUE)
summary(aov(Time~Page,four_sessions))
oneway.test(four_sessions$Time~four_sessions$Page,var=T)

#239p 카이제곱검정
obs <- c(20,40,40)
obs.probs <- c(2/10,3/10,5/10)
(g.fit <- chisq.test(obs,p=obs.probs))

#p242 
raw_data <- c(7,13,9,12,13,21,10,19,11,18,12,13)
data_mtx <- matrix(raw_data,byrow=TRUE,nrow=3)
data_mtx
dimnames(data_mtx) <- list("Class"=c("Class1","Class2","Class3"),"Score"=c("ScoreA","ScoreB","ScoreC","ScoreF"))
data_mtx
addmargins(prop.table(data_mtx))
barplot(t(data_mtx),beside=TRUE,legend=TRUE,ylim=c(0,30),ylab="Observed frequencies in sample",main="Frequency of math score by class")
(i.fit <- chisq.test(data_mtx))

#248p
row<- c(50,30,20,50,80,70)
data_mtx <- matrix(row,byrow = TRUE,nrow=2)
dimnames(data_mtx) <- list("성별"=c("남학생","여학생"),"교과목"=c("통계","머신러닝","딥러닝"))
addmargins(prop.table(data_mtx))
barplot(t(data_mtx),beside=TRUE,legend=TRUE,ylim=c(0,120),ylab="Observed frequencies in sample",main="데이터 사이언스 교과목 선호 조사 결과")
chisq.test(data_mtx)

