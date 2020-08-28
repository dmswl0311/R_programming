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


#256-257
lung <- read.csv(file="C:/Users/User/Desktop/source/data/LungDisease.csv",header=TRUE)
plot(lung$Exposure,lung$PEFR,xlab="Exposure",ylab="PEER")
model <- lm(PEFR~Exposure,data=lung)
model
abline(model,col="blue")

#258
head(cars)
attach(cars)

plot(cars$speed,cars$dist,xlab="speed",ylab="dist")
lm1 <- lm(dist~speed,data=cars)
abline(lm1,col="blue")

yhat <- predict(lm1)

cbind(dist,yhat)

join <- function(i)
  lines(c(speed[i],speed[i]),c(dist[i],yhat[i]),col="green")
sapply(1:50,join)

#260
model <- lm(PEFR~Exposure,data=lung)
plot(lung$Exposure,lung$PEFR,xlab="Exposure",ylab="PEFR",pch=20,col="red")
abline(model,col="blue")

attach(lung)
head(lung)
str(lung)

yhat <- predict(model)
head(yhat)
cbind(Exposure,yhat)

join <- function(i)
  lines(c(Exposure[i],Exposure[i]),c(PEFR[i],yhat[i]),col="green")
sapply(1:122,join)

#261
model <- lm(PEFR~Exposure,data=lung)
model

fitted <- predict(model)
resid <- residuals(model)

#실습 264p
#데이터 불러오기
student_data <- read.csv(file="C:/Users/User/Desktop/source/data/student90.csv",header=TRUE)
attach(student_data)

#회귀모델 생성
model <- lm(weight_kg~height_cm,student_data)
#예측 값 구하기, 비교
model
coef(model)
fitted(model)[1:4]

#plot(student_data$height_cm,student_data$weight_kg,xlab ="height",ylab="weight",col="blue",pch=20)
plot(model,which=4) #which는 그래프의 종류

x_cooks.d <- cooks.distance(model)
x_cooks.d[1:4]
NROW(x_cooks.d)
x_cooks.d[which(x_cooks.d>qf(0.5,df1=2,df2=88))]

#잔차
residuals(model)[1:4]
student_data$weight_kg[1:4]

#예측값+잔차
fitted(model)[1:4]+residuals(model)[1:4]

#잔차분석
qqnorm(residuals(model))
qqline(residuals(model))

shapiro.test(residuals(model))

#회귀계수의 신뢰구간
confint(model,level=0.95)
model_conf <- predict(model,level=0.95,interval="confidence")
head(model_conf)

plot(weight_kg~height_cm,data=student_data)
lwr <- model_conf[,2] #신뢰구간의 하한값
upr <- model_conf[,3] #신뢰구간의 상한값
sx <- sort(student_data$height_cm,index.return=TRUE)
abline(coef(model),lwd=2)
lines(sx$x,lwr[sx$ix],col="blue",lty=2)
lines(sx$x,upr[sx$ix],col="blue",lty=2)

#예측구간
model_pred <- predict(model,level=0.95,interval="predict")
head(model_pred)
p_lwr <- model_pred[,2]
p_upr <- model_pred[,3]
#lines(student_data$height_cm,p_lwr,col="red",lty=2)
#lines(student_data$height_cm,p_upr,col="red",lty=2)

#잔차 제곱의 합
deviance(model)

#예측하기
predict(model,newdata = data.frame(height_cm=175),interval = "confidence")

#모델 평가
summary(model)
anova(model)

#--------------------------
yhat <- predict(model)
cbind(height_cm,yhat)

join <- function(i)
  lines(c(height_cm[i],height_cm[i]),c(weight_kg[i],yhat[i]),col="green")
sapply(1:92, join)

#회귀식 함수
result <- 0

y=function(x){
  result=32.6604+(0.2247*x)
  return(result)
}

#-----------------------------------------------------------------
#266p
house <- read.csv(file="C:/Users/User/Desktop/source/data/house_sales.csv",sep='\t')
head(house[,c("AdjSalePrice","SqFtTotLiving","SqFtLot","Bathrooms","Bedrooms","BldgGrade")])
house_lm <- lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade,data=house,na.action = na.omit)
house_lm

summary(house_lm)
house_full <- lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade)
