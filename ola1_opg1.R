#### Pakker ####
library(tidyverse)

#### Load data ####
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA1/") ## Ændre til din egen wd
boligsiden <- read.csv("boligsiden.csv")

#### Datarensning ####
## Fjerner NA'er
boligsiden_clean <- na.omit(boligsiden) ## Fjerner 185 observationer
rownames(boligsiden_clean) <- NULL

## Renser kvmpris, alt under 500 ganges med 1000. Grundet komma/decimal fejl
boligsiden_clean$kvmpris <- ifelse(
    boligsiden_clean$kvmpris < 500, 
    boligsiden_clean$kvmpris * 1000, 
    boligsiden_clean$kvmpris)

## Rens pris for kr. og konverter til numeric
boligsiden_clean$pris <- gsub("kr\\.\\s*|\\.", "", boligsiden_clean$pris)
boligsiden_clean$pris <- as.numeric(boligsiden_clean$pris)
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$pris > 30000000)),] ## Fjerner 3 huse over 30 mil.

## Liggetid
boligsiden_clean$liggetid <- gsub(" dag", "", boligsiden_clean$liggetid)
boligsiden_clean$liggetid <- as.numeric(boligsiden_clean$liggetid)

## Fjerner outliers i opført år, alt før 1800, dette er 16 rækker
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$opført < 1800)),]

## Renser grund-kolonnen
boligsiden_clean <- boligsiden_clean %>%
mutate(grund = ifelse(
           !is.na(str_extract(grund, "^\\d{3}")),
            str_extract(grund, "^\\d{3}"),
            ifelse(
              !is.na(str_extract(grund, "^\\d{2}")),
              str_extract(grund, "^\\d{2}"),
              grund * 1000)))

boligsiden_clean$grund <- as.numeric(boligsiden_clean$grund)

## Outliers i grund
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$grund > 5000)),]
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$grund < 100)),]

## Renser og fjerner ualmindeligt højre md udgifter
boligsiden_clean$mdudg <- boligsiden_clean$mdudg * 1000
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$mdudg > 100000)),]

## 3 byer har postnummer i bykolonne
boligsiden_clean$postnr <- as.numeric(boligsiden_clean$postnr)
boligsiden_clean$postnr[264] = 9632
boligsiden_clean$postnr[1740] = 6933 
boligsiden_clean$postnr[2031] = 3400 

## Fjern outliers i størrelse
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$størrelse < 50)),]
boligsiden_clean <- boligsiden_clean[-c(which(boligsiden_clean$størrelse > 400)),]

## Find de to ejendomme
which(boligsiden$vej == "egevej" & boligsiden$vejnr == 20)
which(boligsiden$vej == "tousvej" & boligsiden$vejnr == 106)

boligsiden[71,]
boligsiden[2202,]

## Udvælg to tilfældige huse
sample(1:nrow(boligsiden), 2)

## Se info om de to huse
boligsiden[887, ]
boligsiden[2077, ]

## Plot
ggplot(data = boligsiden_clean)+
  geom_point(mapping = aes(x = størrelse, y = pris))+
  geom_smooth(mapping = aes(x = størrelse, y = pris))+
  theme_minimal()
