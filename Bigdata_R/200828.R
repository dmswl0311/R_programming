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