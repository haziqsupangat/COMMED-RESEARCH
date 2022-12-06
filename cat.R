#Categorical

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
nenas = read_excel("KAP cat.xlsx", sheet = 1)
df <- data.frame(nenas)
id <- str_c(epal$villageid, "", epal$hhid, "", epal$indid)
rdata = data.frame(epal) %>% filter(villageid != "77")
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
                        Score = data$q159+data$q160+data$q161+data$q162+data$q163+data$q165+data$q166+data$q167+data$q168, 
                        Percent = Knowledge$Score/9*100, 
                        Kcat = cut(Knowledge$Percent, breaks = c(-Inf, 59, 79, Inf), labels = c(1, 2, 3)))

Kgood <- Knowledge %>%
  filter(Kcat == "3") %>%
  count()%>%
  print()
Kmod <- Knowledge %>%
  filter(Kcat == "2") %>%
  count()%>%
  print()
Kpoo <- Knowledge %>%
  filter(Kcat == "1") %>%
  count()%>%
  print()

TCK <- data.frame(Good = Kgood, Moderate = Kmod, Poor = Kpoo)
TCK <- t(TCK)

#Attitude
Attitude <- data.frame(A1 = data$q169, A2 = data$q170, A3 = data$q171, A4 = data$q172, A5 = data$q173, A6 = data$q174, A7 = data$q175, A8 = data$q176, A9 = data$q178, A10 = data$q179,
                       Score = data$q169+data$q170+data$q171+data$q172+data$q173+data$q174+data$q175+data$q176+data$q178+data$q179,
                       Percent = Attitude$Score/50*100,
                       Acat = cut(Attitude$Percent, breaks= c(-Inf, 59, 79, Inf), labels = c(1, 2, 3)))

Agood <- Attitude %>%
  filter(Acat == "3")%>%
  count() %>%
  print()
Amod <- Attitude %>%
  filter(Acat == "2")%>%
  count()%>%
  print()
Apoo <- Attitude %>%
  filter(Acat == "1")%>%
  count()%>%
  print()

TCA <- data.frame(G = Agood, M = Amod, P = Apoo)

#Practice
Practice <- data.frame(P1 = data$q180, P2 = data$q181, P3 = data$q182, P4 = data$q182.2, P5 = data$q182.3, P6 = data$q183, P7 = data$q184, P8 = data$q185, P9 = data$q186, P10 = data$q187, P11 = data$q188, P12 = data$q189,
                       Score = data$q180+data$q181+data$q182+data$q182.2+data$q182.3+data$q183+data$q184+data$q185+data$q186+data$q187+data$q188+data$q189,
                       Percent = Practice$Score/36*100,
                       Pcat = cut(Practice$Percent, breaks = c(-Inf, 59, 79, Inf), labels = c(1, 2, 3)))

Pgood <- Practice %>%
  filter(Pcat == "3") %>%
  count() %>%
  print()

Pmod <- Practice %>%
  filter(Pcat == "2") %>%
  count() %>%
  print()

Ppoo <- Practice %>%
  filter(Pcat == "1") %>%
  count() %>%
  print()

TCP <- data.frame(G = Pgood, M = Pmod, P = Ppoo)


#Analysis
Tcat <- data.frame(A = Attitude$Acat, K = Knowledge$Kcat, P = Practice$Pcat)

#K vs A
chisq.test(Tcat$K,  Tcat$A, simulate.p.value = TRUE)
tbl = table(Tcat$K, Tcat$A)
tbl
prop.table(tbl,2)
chisq.test(tbl) 

#K vs P
chisq.test(Tcat$K, Tcat$P, simulate.p.value = TRUE)
tbl = table(Tcat$K, Tcat$P)
tbl
prop.table(tbl,2)
chisq.test(tbl) 

#A vs P
chisq.test(Tcat$A, Tcat$P, simulate.p.value = TRUE)
tbl = table(Tcat$A, Tcat$P)
tbl
prop.table(tbl,2)
chisq.test(tbl)
