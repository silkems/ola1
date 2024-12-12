## Pakker 
library(tidyverse)
library(dkstat)
library(patchwork)
library(corrplot)

#### Opgave 4.1 ####
#### Hent alkoholdata via DST API ####
FU02.meta <- dst_meta(table="FU02")

FU02.filter <- list(
  KONSUMGRP = c(
  "02.1.1.1 Spiritus og likør",                                           
  "02.1.1.2 Alkoholiske læskedrikke",                                    
  "02.1.2.1 Vin af druer",                                                
  "02.1.2.2 Vin af andre frugter",                                        
  "02.1.2.3 Hedvin",                                                      
  "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",                   
  "02.1.3.1 Pilsnerøl, guldøl",                                           
  "02.1.3.2 Andre alkoholholdige øl",                                     
  "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",                 
  "02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = "Faste priser",
  Tid = "*")

FU02 <- dst_get_data(table = "FU02", query = FU02.filter)

#### Data transformation ####
## Bredt format
FU02 <- pivot_wider(FU02, names_from = KONSUMGRP, values_from = value)

## Aggregering, danner kategorier
FU02_grp <- FU02 %>% 
  mutate("Vin" = 
           FU02$`02.1.2.1 Vin af druer`+
           FU02$`02.1.2.2 Vin af andre frugter`+
           FU02$`02.1.2.3 Hedvin`) %>% 
  mutate("Øl" =
           FU02$`02.1.3.1 Pilsnerøl, guldøl`+
           FU02$`02.1.3.2 Andre alkoholholdige øl`+
           FU02$`02.1.3.4 Øl-baserede drikkevarer`) %>% 
  mutate("Spiritus" =
           FU02$`02.1.1.1 Spiritus og likør`+
           FU02$`02.1.1.2 Alkoholiske læskedrikke`) %>% 
  mutate("Alkoholfri" = 
           FU02$`02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl`+
           FU02$`02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin`)
FU02_grp <- FU02_grp[,c(-1,-3:-12)]

#### Udviklingen i de forskellige grupper ####
## Vin
vinplot <- ggplot(data = FU02_grp, aes(x = TID))+
  geom_line(aes(y = Vin), linewidth = 1.1, color = "darkred")+
  labs(y = "Årligt forbrug i kr. pr. husstand",
       title = "Vin")+
  scale_y_continuous(limits = c(2200, 3500))+
  theme_minimal()

## Øl
ølplot <- ggplot(data = FU02_grp, aes(x = TID))+
  geom_line(aes(y = Øl), linewidth = 1.1, color = "darkgreen")+
  labs(y = "Årligt forbrug i kr. pr. husstand",
       title = "Øl")+
  scale_y_continuous(limits = c(800, 2300))+
  theme_minimal()

## Spiritus og Alkoholiske Læskedrikke
spiritplot <- ggplot(data = FU02_grp, aes(x = TID))+
  geom_line(aes(y = Spiritus), linewidth = 1.1, color = "darkorange2")+
  labs(y = "Årligt forbrug i kr. pr. husstand",
       title = "Spiritus og Alkoholiske Læskedrikke")+
  scale_y_continuous(limits = c(500, 1000))+
  theme_minimal()

## Alkoholfri øl og vin
alkofriplot <- ggplot(data = FU02_grp, aes(x = TID))+
  geom_line(aes(y = Alkoholfri), linewidth = 1.1, color = "darkblue")+
  labs(y = "Årligt forbrug i kr. pr. husstand",
       title = "Alkoholfri øl og vin")+
  scale_y_continuous(limits = c(0, 110))+
  theme_minimal()

## Samlet plot
samletplot <- (vinplot + ølplot + spiritplot + alkofriplot)+
  plot_annotation(
    title = "Danskerne forbruger mindre øl og mere spiritus"
  )
samletplot

#### Opgave 4.2 ####
alkocor <- cor(FU02_grp[,-1], use = "complete.obs")
alkocor

corrplot(alkocor, method = "color", 
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

