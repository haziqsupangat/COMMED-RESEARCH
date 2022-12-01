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
                       Score = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189)
summary(Practice)
averageP <- sum(Practice)/nrow(Practice)
meanP <- mean(Practice$Score)
meanP
averageP

write.csv(Knowledge, "/Users/haziqsupangat/Desktop/KG PAGIII/Knowledge.csv")
write.csv(Attitude, "/Users/haziqsupangat/Desktop/KG PAGIII/Attitude.csv")
write.csv(Practice, "/Users/haziqsupangat/Desktop/KG PAGIII/Practice.csv")


#Objective 3

hist(Knowledge$Score)
hist(Attitude$Score)
hist(Practice$Score)

shapiro.test(Knowledge$Score)
shapiro.test(Attitude$Score)
shapiro.test(Practice$Score)
