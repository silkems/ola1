#### Opgave 3.1 ####
roll <- function(n){
  res <- sample(1:6, n, replace = TRUE)
  antal <- sum(res == 5)
  sandsynlighed <- round(antal/n*100, digits = 2)
  
  print(paste("Du slog med", n, "terninger på én gang!"))
  print(paste0("Du slog ", antal, " femmere, hvilket svarer til ", sandsynlighed, "% af terningerne!" ))
}


#### Opgave 3.2 ####
## Ny roll funktion
roll2 <- function(n){
  res <- sample(1:6, n, replace = TRUE)
  summen <- sum(res)
  return(summen)
}

## Looper funktionen 10.000 gange og gemmer i DF
total <- data.frame(matrix(nrow = 0, ncol = 1)) ## Opretter tom df til at gemme resultater

for (i in 1:10000) {
  temp <- roll2(6)
  total <- rbind(total, data.frame(Sum = temp))
}


## Gemmer frekvensen af summen i en tabel
total_tab <- data.frame(table(total))

## GGplot 
ggplot(data = total_tab)+
  geom_bar(aes(x = Sum, y = Freq, fill = Freq), stat = "identity")+
  labs(x = "Summen af 6 terninger", 
       y = "Antal",
       title = "Optællingen af summen af 10.000 terningslag, viser næsten en normalfordeling",
       caption = "10.000 simulerede terningslag")+
  theme_minimal()+
  theme(legend.position = "none")


#### Opgave 3.3 ####
## Replicate i stedet for loop
total2 <- data.frame(Sum = replicate(1000000, roll2(6)))

## Gemmer frekvensen af summen i en tabel
total_tab2 <- data.frame(table(total2))

## GGplot 
ggplot(data = total_tab2)+
  geom_bar(aes(x = Sum, y = Freq, fill = Freq), stat = "identity")+
  labs(x = "Summen af 6 terninger", 
       y = "Antal",
       title = "Optællingen af summen af 1 mio. terningslag, viser en perfekt normalfordeling",
       caption = "1.000.000 simulerede terningslag")+
  theme_minimal()+
  theme(legend.position = "none")

#### Opgave 3.4 ####
mat1 <- matrix(nrow = 5, ncol = 1, data = 2:6)
mat2 <- matrix(nrow = 5, ncol = 1, data = sample(c(1,2,3,5,6),5))
mat3 <- cbind(mat1, mat2)
