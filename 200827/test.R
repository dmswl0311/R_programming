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

score <- read.csv(file="C:/Users/User/Desktop/source/data/score.txt",header=T) #다중 회귀분석
attach(score)
lm3 <- lm(성적~IQ+다니는학원수+게임하는시간+TV시청시간)
lm3
y=23.2992+(0.4684*130+0.7179*3-0.8390*2-1.3854*1)
round(y,2)
summary(lm3)

#문제1
a <- c(65,87,73,79,81,69,80,77,68,74)
b <- c(75,69,83,81,72,79,90,88,76,82)
c <- c(59,78,67,62,83,76,55,75,49,68)
d <- c(94,89,80,88,90,85,79,93,88,85)

shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)

mydata <- c(a,b,c,d)
group <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10))
oneway.test(mydata~group,var=T)

#문제2
x <-c(52,60,63,43,46,56,62,50) 
y <-c(58,62,62,48,50,55,68,57)
shapiro.test(x)
shapiro.test(y)
t.test(x,y,paired = TRUE)

#문제3
pre <- c(13.2,8.2,10.9,14.3,10.7,6.6,9.5,10.8,8.8,13.3)
post <- c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)
shapiro.test(pre)
shapiro.test(post)
t.test(pre,post,paired = TRUE)

#문제4
new <- c(15,10,13,7,9,8,21,9,14,8)
old <- c(15,14,12,8,14,7,16,10,15,12)
shapiro.test(new)
shapiro.test(old)
t.test(new,old)

#문제5
x1 <- c(23,27,24,25,29,30,26)
x2 <- c(35,32,38,36,32,33,34)
x3 <- c(36,41,38,39,40,38,39)
x4 <- c(32,30,37,34,35,34,32)
shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
shapiro.test(x4)

mydata <- c(x1,x2,x3,x4)
group <- c(rep(1,7),rep(2,7),rep(3,7),rep(4,7))
oneway.test(mydata~group,var=T)


#문제6
x <- c(15,10,13,7,9,8,21,9,14,8)
y <- c(15,14,12,8,14,7,16,10,15,12)
lm1=lm(y~x)
summary(lm1)

#문제7
y <- c(100,90,98,79,81,69,80,77,68,54)
x1 <- c(5,4,5,3,4,3,2,3,2,1)
x2 <- c(5,3,4,3,4,3,2,3,2,1)
x3 <- c(5,3,3,2,3,3,4,3,2,1)
lm2=lm(y~x1+x2+x3)
summary(lm2)

#문제8
y <- c(100,90,98,79,81,69,80,77,68,74)
x1 <- c(5,4,4,3,4,3,4,3,2,3)
x2 <- c(5,3,5,2,3,2,3,3,2,3)
x3 <- c(5,3,4,4,3,2,4,4,2,3)
lm3=lm(y~x1+x2+x3)
summary(lm3)

#문제9
x <- c(58,49,39,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
shapiro.test(x)
t.test(x, mu=55, alternative="greater")
