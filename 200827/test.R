score <- c(85,90,93,86,82)
mean(score)
median(score)
var(score)
sd(score)

height <- rnorm(n=100000,mean=168,sd=7)
hist(height,breaks=100,probability=T)

weight <- rnorm(n=100,mean=65,sd=5)
hist(weight,breaks = 30,probability = T)

score1 <- read.csv(file="C:/Users/User/Desktop/source/data/tdata.csv",header = T)
score1
shapiro.test(score1$성적)
t.test(score1$성적)

x <- c(52,60,63,43,46,56,62,50)
y <- c(58,62,62,48,50,55,58,57)

shapiro.test(x)
shapiro.test(y)

t.test(x,y)

fruits1 <- read.csv(file="C:/Users/User/Desktop/source/data/love_fruits.csv",header=T) #빈도분석
prop.table(table(fruits1$선호과일))
round(prop.table(table(fruits1$선호과일))*100,2)
table(fruits1$선호과일)
count <- c(table(fruits1$선호과일))
pct <- c(round(prop.table(table(fruits1$선호과일))*100,2))
table <- data.frame(건수=count,비율=pct)
table

barplot(table$건수,names.arg=c("바나나","복숭아","사과","체리","포도"),ylim=c(0,15),col=rainbow(5))

x <- c(3,5,8,11,13) #상관분석
y <- c(1,2,3,4,5)
cor(x,y)
cor.test(x,y)

x <- c(110,120,130,140,150) #회귀분석
y <- c(100,105,128,115,142)
plot(x,y,pch=20,col="orange")
line <- lm(y~x)
line
y=-4.20+(0.94*117)
y

score <- read.csv(file="C:/Users/User/Desktop/source/data/score.txt",header=T) #다중 회귀분석석
attach(score)
lm3 <- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3
y=23.2992+(0.4684*x1+0.7179*x2-0.8390*x3-1.3854*x4)

