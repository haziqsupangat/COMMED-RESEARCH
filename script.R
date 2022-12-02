#1
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ggpubr)

epal = read_excel("Data.xlsx", sheet = 1)
id <- str_c(epal$villageid, "", epal$hhid, "", epal$indid)
data = data.frame(epal)

#age
data$age <- cut(data$q007 , breaks = c(18,24,29,34,39,44,49,64, Inf), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-64", ">65"))
summary(data$age)

#sex
data$sex <- cut(data$q006, breaks = c(0, 1, Inf), labels = c("Male", "Female"))
summary(data$sex)

#Knowledge
data$K1 <- cut(data$q159, breaks = c(-Inf, 0, 1), labels = c("0", "1"))
data$K1 <- as.numeric(data$K1)
summary(data$K1)
summary(data$q159)

data$K2 <- cut(data$q160, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K2)
summary(data$q160)

data$K3 <- cut(data$q161, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K3)
summary(data$q161)

data$K4 <- cut(data$q162, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K4)
summary(data$q162)

data$K5 <- cut(data$q163, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K5)
summary(data$q163)

data$K6 <- cut(data$q165, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K6)
summary(data$q165)

data$K7 <- cut(data$q166, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K7)
summary(data$q166)

data$K8 <- cut(data$q167, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K8)
summary(data$q167)

data$K9 <- cut(data$q168, breaks = c(-Inf, 0, 1), labels = c("False", "True"))
summary(data$K9)
summary(data$q168)

#Attitude
data$A1 <- cut(data$q169, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A1)
summary(data$q169)

data$A2 <- cut(data$q170, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A2)
summary(data$q170)

data$A3 <- cut(data$q171, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A3)
summary(data$q171)

data$A4 <- cut(data$q172, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A4)
summary(data$q172)

data$A5 <- cut(data$q173, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A5)
summary(data$q173)

data$A6 <- cut(data$q174, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A6)
summary(data$q174)

data$A7 <- cut(data$q175, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A7)
summary(data$q175)

data$A8 <- cut(data$q176, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A8)
summary(data$q176)

data$A9 <- cut(data$q178, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A9)
summary(data$q178)

data$A10 <- cut(data$q179, breaks = c(0,1,2,3,4,5), labels = c("STJ", "TS", "TP", "S", "SS"))
summary(data$A10)
summary(data$q179)

#Practice
data$P1 <- cut(data$q180, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P1)
summary(data$q180)

data$P2 <- cut(data$q181, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P2)
summary(data$q181)

data$P3 <- cut(data$q182, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P3)
summary(data$q182)

data$P4 <- cut(data$q182.2, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P4)
summary(data$q182.2)

data$P5 <- cut(data$q182.3, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P5)
summary(data$q182.3)

data$P6 <- cut(data$q183, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P6)
summary(data$q183)

data$P7 <- cut(data$q184, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P7)
summary(data$q184)

data$P8 <- cut(data$q185, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P8)
summary(data$q185)

data$P9 <- cut(data$q186, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P9)
summary(data$q186)

data$P10 <- cut(data$q187, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P10)
summary(data$q187)

data$P11 <- cut(data$q188, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P11)
summary(data$q188)

data$P12 <- cut(data$q189, breaks = c(0,1,2,3), labels = c("TP", "KK", "S"))
summary(data$P12)
summary(data$q189)

#Table Knowledge
Knowledge <- data.frame(K1 =  data$q159, K2 = data$q160, K3 = data$q161, K4 = data$q162, K5 = data$q163, K6 = data$q165, K7 = data$q166, K8 = data$q167, K9 = data$q168, 
                        Score = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168)
summary(Knowledge)
averageK <- sum(Knowledge)/nrow(Knowledge)
meanK <- mean(Knowledge$Score)
meanK
averageK


#Table Attitude
Attitude <- data.frame(A1 = data$q169, A2 = data$q170, A3 = data$q171, A4 = data$q172, A5 = data$q173, A6 = data$q174, A7 = data$q175, A8 = data$q176, A9 = data$q178, A10 = data$q179,
                       Score = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179)
summary(Attitude)
averageA <- sum(Attitude)/nrow(Attitude)
meanA <- mean(Attitude$Score)
meanA
averageA

#Table Practice
Practice <- data.frame(P1 = data$q180, P2 = data$q181, P3 = data$q182, P4 = data$q182.2, P5 = data$q182.3, P6 = data$q183, P7 = data$q184, P8 = data$q185, P9 = data$q186, P10 = data$q187, P11 = data$q188, P12 = data$q189,
                       Score = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189, Avg = mean(Practice$Score))
summary(Practice)
averageP <- sum(Practice)/nrow(Practice)
meanP <- mean(Practice$Score)
meanP
averageP

write.csv(Knowledge, "/Users/haziqsupangat/Desktop/KG PAGIII/Knowledge.csv")
write.csv(Attitude, "/Users/haziqsupangat/Desktop/KG PAGIII/Attitude.csv")
write.csv(Practice, "/Users/haziqsupangat/Desktop/KG PAGIII/Practice.csv")


#Table Average

Average <- data.frame (K1 =  mean(data$q159), K2 = mean(data$q160), K3 = mean(data$q161), K4 = mean(data$q162), K5 = mean(data$q163), K6 = mean(data$q165), K7 = mean(data$q166), K8 = mean(data$q167), K9 = mean(data$q168))

avgK <- Knowledge$Score/ncol(Knowledge)
Kn <- as.numeric(avgK)
avgA <- Attitude$Score/ncol(Attitude)
At <- as.numeric(avgA)
avgP <- Practice$Score/ncol(Practice)
Pr <- as.numeric(avgP)

TableAverage <- data.frame(K = Kn, A = At, P = Pr)

#Objective 3

hist(Knowledge$Score)
hist(Attitude$Score)
hist(Practice$Score)

shapiro.test(Knowledge$Score)
shapiro.test(Attitude$Score)
shapiro.test(Practice$Score)

cor.test(TableAverage$K, TableAverage$P, method=c("spearman"), exact = FALSE) 
cor.test(Practice$Score, Knowledge$Score, method=c("spearman"), exact = FALSE) 
cor.test(Practice$Score, Attitude$Score, method=c("spearman"))


plot(Knowledge$Score, Practice$Score, main="Relationship between level of Knowledge on Practice of Household Waste Disposal", 
     xlab="Knowledge ", ylab="Practice ", pch=19)
abline(lm(Practice$Score ~ Knowledge$Score), col="red", aes(size = 5))

plot(Attitude$Score, Practice$Score, main="Relationship between level of Attitude on Practice of Household Waste Disposal", 
     xlab="Attitude ", ylab="Practice ", pch=20)
abline(lm(Practice$Score ~ Attitude$Score), col="red")


#Objective 4

eighteen <- data %>% filter(q007 < "25")
eighteen <- data.frame (K = eighteen$q159+eighteen$q160+eighteen$q161+eighteen$q162+eighteen$q163+eighteen$q165+eighteen$q166+eighteen$q167+eighteen$q168,
                        A = eighteen$q169+eighteen$q170+eighteen$q171+eighteen$q172+eighteen$q173+eighteen$q174+eighteen$q175+eighteen$q176+eighteen$q178+eighteen$q179,
                        P = eighteen$q180+eighteen$q181+eighteen$q182+eighteen$q182.2+eighteen$q182.3+eighteen$q183+eighteen$q184+eighteen$q185+eighteen$q186+eighteen$q187+eighteen$q188+eighteen$q189)
mean(eighteen$K)
mean(eighteen$A)
mean(eighteen$P)

twentyfive <- data %>% filter(q007 >= "25", q007 < "30")
twentyfive <- data.frame (K = twentyfive$q159+twentyfive$q160+twentyfive$q161+twentyfive$q162+twentyfive$q163+twentyfive$q165+twentyfive$q166+twentyfive$q167+twentyfive$q168,
                        A = twentyfive$q169+twentyfive$q170+twentyfive$q171+twentyfive$q172+twentyfive$q173+twentyfive$q174+twentyfive$q175+twentyfive$q176+twentyfive$q178+twentyfive$q179,
                        P = twentyfive$q180+twentyfive$q181+twentyfive$q182+twentyfive$q182.2+twentyfive$q182.3+twentyfive$q183+twentyfive$q184+twentyfive$q185+twentyfive$q186+twentyfive$q187+twentyfive$q188+twentyfive$q189)
mean(twentyfive$K)
mean(twentyfive$A)
mean(twentyfive$P)

thirty <- data %>% filter(q007 >= "30", q007 < "35")
thirty <- data.frame (K = thirty$q159+thirty$q160+thirty$q161+thirty$q162+thirty$q163+thirty$q165+thirty$q166+thirty$q167+thirty$q168,
                          A = thirty$q169+thirty$q170+thirty$q171+thirty$q172+thirty$q173+thirty$q174+thirty$q175+thirty$q176+thirty$q178+thirty$q179,
                          P = thirty$q180+thirty$q181+thirty$q182+thirty$q182.2+thirty$q182.3+thirty$q183+thirty$q184+thirty$q185+thirty$q186+thirty$q187+thirty$q188+thirty$q189)
mean(thirty$K)
mean(thirty$A)
mean(thirty$P)

thirtyfive <- data %>% filter(q007 >= "35", q007 < "40")
thirtyfive <- data.frame (K = thirtyfive$q159+thirtyfive$q160+thirtyfive$q161+thirtyfive$q162+thirtyfive$q163+thirtyfive$q165+thirtyfive$q166+thirtyfive$q167+thirtyfive$q168,
                      A = thirtyfive$q169+thirtyfive$q170+thirtyfive$q171+thirtyfive$q172+thirtyfive$q173+thirtyfive$q174+thirtyfive$q175+thirtyfive$q176+thirtyfive$q178+thirtyfive$q179,
                      P = thirtyfive$q180+thirtyfive$q181+thirtyfive$q182+thirtyfive$q182.2+thirtyfive$q182.3+thirtyfive$q183+thirtyfive$q184+thirtyfive$q185+thirtyfive$q186+thirtyfive$q187+thirtyfive$q188+thirtyfive$q189)
mean(thirtyfive$K)
mean(thirtyfive$A)
mean(thirtyfive$P)

forty <- data %>% filter(q007 >= "40", q007 <"45")
forty <- data.frame (K = forty$q159+forty$q160+forty$q161+forty$q162+forty$q163+forty$q165+forty$q166+forty$q167+forty$q168,
                          A = forty$q169+forty$q170+forty$q171+forty$q172+forty$q173+forty$q174+forty$q175+forty$q176+forty$q178+forty$q179,
                          P = forty$q180+forty$q181+forty$q182+forty$q182.2+forty$q182.3+forty$q183+forty$q184+forty$q185+forty$q186+forty$q187+forty$q188+forty$q189)
mean(forty$K)
mean(forty$A)
mean(forty$P)

fortyfive <- data %>% filter(q007 >= "45", q007 < "50")
fortyfive <- data.frame (K = fortyfive$q159+fortyfive$q160+fortyfive$q161+fortyfive$q162+fortyfive$q163+fortyfive$q165+fortyfive$q166+fortyfive$q167+fortyfive$q168,
                     A = fortyfive$q169+fortyfive$q170+fortyfive$q171+fortyfive$q172+fortyfive$q173+fortyfive$q174+fortyfive$q175+fortyfive$q176+fortyfive$q178+fortyfive$q179,
                     P = fortyfive$q180+fortyfive$q181+fortyfive$q182+fortyfive$q182.2+fortyfive$q182.3+fortyfive$q183+fortyfive$q184+fortyfive$q185+fortyfive$q186+fortyfive$q187+fortyfive$q188+fortyfive$q189)
mean(fortyfive$K)
mean(fortyfive$A)
mean(fortyfive$P)

fifty <- data %>% filter(q007 >= "50", q007 < "66")
fifty <- data.frame (K = fifty$q159+fifty$q160+fifty$q161+fifty$q162+fifty$q163+fifty$q165+fifty$q166+fifty$q167+fifty$q168,
                         A = fifty$q169+fifty$q170+fifty$q171+fifty$q172+fifty$q173+fifty$q174+fifty$q175+fifty$q176+fifty$q178+fifty$q179,
                         P = fifty$q180+fifty$q181+fifty$q182+fifty$q182.2+fifty$q182.3+fifty$q183+fifty$q184+fifty$q185+fifty$q186+fifty$q187+fifty$q188+fifty$q189)
mean(fifty$K)
mean(fifty$A)
mean(fifty$P)

sixty <- data %>% filter(q007 >= "65")
sixty <- data.frame (K = sixty$q159+sixty$q160+sixty$q161+sixty$q162+sixty$q163+sixty$q165+sixty$q166+sixty$q167+sixty$q168,
                     A = sixty$q169+sixty$q170+sixty$q171+sixty$q172+sixty$q173+sixty$q174+sixty$q175+sixty$q176+sixty$q178+sixty$q179,
                     P = sixty$q180+sixty$q181+sixty$q182+sixty$q182.2+sixty$q182.3+sixty$q183+sixty$q184+sixty$q185+sixty$q186+sixty$q187+sixty$q188+sixty$q189)
mean(sixty$K)
mean(sixty$A)
mean(sixty$P)

KvA <- data.frame("18-24" = mean(eighteen$K), "25-29" = mean(twentyfive$K), "30-34" = mean(thirty$K), "35-39" = mean(thirtyfive$K), "40-44" = mean(forty$K), "45-49" = mean(fortyfive$K), "50-65" = mean(fifty$K), ">65" = mean(sixty$K))
KvA <- t(KvA)
view(KvA)

AvA <- data.frame("18-24" = mean(eighteen$A), "25-29" = mean(twentyfive$A), "30-34" = mean(thirty$A), "35-39" = mean(thirtyfive$A), "40-44" = mean(forty$A), "45-49" = mean(fortyfive$A), "50-65" = mean(fifty$A), ">65" = mean(sixty$A))
AvA <- t(AvA)
view(AvA)

PvA <- data.frame("18-24" = mean(eighteen$P), "25-29" = mean(twentyfive$P), "30-34" = mean(thirty$P), "35-39" = mean(thirtyfive$P), "40-44" = mean(forty$P), "45-49" = mean(fortyfive$P), "50-65" = mean(fifty$P), ">65" = mean(sixty$P))
PvA <- t(PvA)
view(PvA)

TableObjective4.1 <- data.frame(K = KvA, A = AvA, P = PvA)
View(TableObjective4.1)


