## Pakker
library(tidyverse)
library(corrplot)

## Indlæs "boligsiden_clean.RDS" fra opgave 1
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA1/") ## Sæt din egen sti
boligsiden_clean <- readRDS("boligsiden_clean.RDS")

#### Opgave 2.1 ####
## For datarensning og datasæt, se opgave 1
summary(boligsiden_clean)

#### Opgave 2.2 ####
cor(boligsiden_clean$størrelse, boligsiden_clean$pris)

#### Opgave 2.3 ####
## Lineære regressioner
lm1 <- lm(kvmpris ~ størrelse, data = boligsiden_clean)
lm2 <- lm(kvmpris ~ mdudg, data = boligsiden_clean)
lm3 <- lm(kvmpris ~ grund, data = boligsiden_clean)
lm4 <- lm(kvmpris ~ opført, data = boligsiden_clean)
lm5 <- lm(kvmpris ~ liggetid, data = boligsiden_clean) ## Pas på afgrænsningen

## Korrelationer
cor(boligsiden_clean$kvmpris, boligsiden_clean$størrelse)
cor(boligsiden_clean$kvmpris, boligsiden_clean$mdudg)
cor(boligsiden_clean$kvmpris, boligsiden_clean$grund)
cor(boligsiden_clean$kvmpris, boligsiden_clean$opført)
cor(boligsiden_clean$kvmpris, boligsiden_clean$liggetid)

## Korrelationsmatrix
boligcor <- cor(boligsiden_clean[,c(7:9,11:12)], use = "complete.obs")
boligcor

corrplot(boligcor, method = "color", 
         type = "upper",
         order = "hclust", 
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 1,
         diag = F,
         cl.pos = "n",
         tl.srt = 45,
         outline = T,
         tl.cex = 1.25)

