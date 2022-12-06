#Package
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ggpubr)
library(readxl)
library(car)
library(psych)

#meng-data frame kan dia
epal = read_excel("Data.xlsx", sheet = 1)
id <- str_c(epal$villageid, "", epal$hhid, "", epal$indid)
rdata = data.frame(epal) %>% filter(villageid != "77.0")
data = data.frame(rdata)
nrow(data)

#Objective 1
#meng-describe sociodemographic
#age
data$age <- cut(data$q007 , breaks = c(18,24,29,34,39,44,49,64, Inf), labels = c(1, 2, 3, 4, 5, 6, 7, 8))
summary(data$age)

#sex
#"Male", "Female" ~ 1, 2
data$sex <- cut(data$q006, breaks = c(0, 1, Inf), labels = c(1, 2))
summary(data$sex)
data$sex <- data.frame(data$sex)

#education level
data$edu <- cut(data$q013, breaks = c(0, 2, 3, 5), labels = c(1, 2, 3))
data$edu <- data.frame(data$edu)
summary(data$edu)
shapiro.test(as.numeric(unlist(data$edu))) #tak normal

#income
data$income <- cut(data$q047, breaks = c(-Inf, 1000, 2000, 3000, Inf), labels = c(1, 2, 3, 4))
data$income <- data.frame(data$income)
summary(data$income)
shapiro.test(as.numeric(unlist(data$income))) 

#Objective 2
#Knowledge
Knowledge <- data.frame(K1 =  data$q159, K2 = data$q160, K3 = data$q161, K4 = data$q162, K5 = data$q163, K6 = data$q165, K7 = data$q166, K8 = data$q167, K9 = data$q168, 
                        Score = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168)
data$Knowledge <- is.numeric(data$Knowledge)
mean(Knowledge$Score)

#Kira tahu dan tak tahu, tukar bahagian filter kepada soalan == tahu/tak
Knowledge %>% 
  filter(K6 == "1") %>%
  count() 
  
MSD <- Knowledge$K9
sd(MSD)
mean(MSD)


shapiro.test(data$Knowledge)

#Attitude
Attitude <- data.frame(A1 = data$q169, A2 = data$q170, A3 = data$q171, A4 = data$q172, A5 = data$q173, A6 = data$q174, A7 = data$q175, A8 = data$q176, A9 = data$q178, A10 = data$q179,
                       Score = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179)
Attitude %>% 
  filter(A1 == "1") %>%
  count()
MSD <- Attitude$A11
mean(MSD)
sd(MSD)


shapiro.test(data$Attitude)


#Practice
Practice <- data.frame(P1 = data$q180, P2 = data$q181, P3 = data$q182, P4 = data$q182.2, P5 = data$q182.3, P6 = data$q183, P7 = data$q184, P8 = data$q185, P9 = data$q186, P10 = data$q187, P11 = data$q188, P12 = data$q189,
                       Score = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189)


Practice %>% 
  filter(A1 == "1") %>%
  count()
MSD <- Practice$P12
sd(MSD)
mean(MSD)



shapiro.test(data$Attitude)

#save as table kalau hgpa nak, tukaq la pathname tu nanti

write.csv(data$Knowledge, "/Users/haziqsupangat/Desktop/KG PAGIII/Knowledge.csv")
write.csv(data$Attitude, "/Users/haziqsupangat/Desktop/KG PAGIII/Attitude.csv")
write.csv(data$Practice, "/Users/haziqsupangat/Desktop/KG PAGIII/Practice.csv")

#Average

#Table Average for each variables

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

KA <- Knowledge$Score + Attitude$Score
cor.test(Knowledge$Score, Attitude$Score, method=c("spearman")) 
cor.test(TableAverage$K, TableAverage$P, method=c("spearman"), exact = FALSE) 
cor.test(TableAverage$A, TableAverage$P, method=c("spearman"), exact = FALSE) 
cor.test(Practice$Score, Knowledge$Score, method=c("spearman"), exact = FALSE) 
cor.test(Practice$Score, Attitude$Score, method=c("spearman"))


plot(Knowledge$Score, Practice$Score, main="Relationship between level of Knowledge on Practice of Household Waste Disposal", 
     xlab="Knowledge ", ylab="Practice ", pch=19)
abline(lm(Practice$Score ~ Knowledge$Score), col="red", aes(size = 5))

plot(Knowledge$Score, Attitude$Score, main= "Relationship between level of Knowledge on Attitude of Household Waste Dosposal", 
     xlab = "K", ylab = "A", pch=19)
abline(lm(Attitude$Score ~ Knowledge$Score), col = "red", aes(size = 5))
plot(Attitude$Score, Practice$Score, main="Relationship between level of Attitude on Practice of Household Waste Disposal", 
     xlab="Attitude ", ylab="Practice ", pch=20)
abline(lm(Practice$Score ~ Attitude$Score), col="red")


#Objective 4
#Objective 4.1 (Knowledge vs sociodemographic)
#age
ageK <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(edulevel == 1)
MSD <- ageK
mean(MSD)
sd(MSD)
describeBy(data$Knowledge,data$age,mat=TRUE)
kruskal.test(data$age ~ data$Knowledge, data = data)

#sex
sexM <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 1)
mean(sexM$K)

sexF <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 2)
mean(sexF$K)

wilcox.test(data$Knowledge ~ data$sex, data = data)

#edu
edulevel <- data %>% 
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(edu == 4)
mean(edulevel$A)
sd(edulevel$A)
kruskal.test(data$edu ~ data$Attitude, data = data)

#gaji
income <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(income == 1)
mean(income)
sd(income)

kruskal.test(data$income ~ data$Knowledge, data = data)

#Objective 4.2 (Attitude vs sociodemographic)
#Age
kruskal.test(data$age ~ data$Attitude, data = data)

#sex
sexM <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 3)
mean(sexM$K)

sexF <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 2)
mean(sexF$A)

wilcox.test(data$Attitude ~ data$sex, data = data)

#edu
edulevel <- data %>% 
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(edu == 2)
mean(edulevel$K)

kruskal.test(data$edu ~ data$Attitude, data = data)

#income
kruskal.test(data$income ~ data$Attitude, data = data)

#Objective 4.3 (Practice vs sociodemographic)
#Age
describeBy(data$Practice,data$age,mat=TRUE)
leveneTest(data = data, Practice ~ age)
kruskal.test(data$age ~ data$Practice, data = data)

#sex
filter <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 1)  #ni filter, thanks
mean(sexM$P)

sexF <- data %>%
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(sex == 1)
mean(sexF$P)
sd(sexF$P)
leveneTest(Practice ~ sex, data = data)
wilcox.test(data$Practice ~ data$sex, data = data)

#edu
edulevel <- data %>% 
  data.frame(K = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168,
             A = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
             P = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189) %>% 
  filter(edu == 3)
mean(edulevel$P)
sd(edulevel$P)
leveneTest(data = data, Practice ~ edu)
fit <- aov(data$Practice ~ data$edu,
           data=data)
summary(fit)
TukeyHSD(fit)

#income

leveneTest(Practice ~ income, data = data)

kruskal.test(data$income ~ data$Practice, data = data)


