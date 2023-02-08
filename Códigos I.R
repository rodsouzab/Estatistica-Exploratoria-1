library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(stats)

#Organizando os Dados 
dados <- read_excel(file.choose()) %>%
  filter(City == "GUARUJÁ") %>%
  mutate(Enterococcus = as.numeric(Enterococcus))  
  
pereque = group_by(dados, Beach == "PEREQUÊ")

pernambuco = group_by(dados, Beach == "PERNAMBUCO")

enseada_per = group_by(dados, Beach == "ENSEADA (ESTR. DE PERNAMBUCO)")

enseada_atl = group_by(dados, Beach == "ENSEADA (AV. ATLÂNTICA)")

enseada_chi = group_by(dados, Beach == "ENSEADA (R. CHILE)")

enseada_san = group_by(dados, Beach == "ENSEADA (AV. SANTA MARIA)")

pitangueiras_pug = group_by(dados, Beach == "PITANGUEIRAS (AV. PUGLISI)")

pitangueiras_sil = group_by(dados, Beach == "PITANGUEIRAS (R. SILVIA VALADÃO)")

asturias = group_by(dados, Beach == "ASTÚRIAS")

tombo = group_by(dados, Beach == "TOMBO")

guaiuba = group_by(dados, Beach == "GUAIÚBA")



# Primeiro Item


sum_pereque = summarise(pereque,
                        media = mean(Enterococcus),
                        desvio_padrão = sd(Enterococcus),
                        mediana = median(Enterococcus),
                        Q1 = quantile(Enterococcus,0.25),
                        Q2 = quantile(Enterococcus,0.75),
                        mínimo = min(Enterococcus),
                        máximo = max(Enterococcus))

sum_pernambuco = summarise(pernambuco,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_enseada_per = summarise(enseada_per,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_enseada_atl = summarise(enseada_atl,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_enseada_chi = summarise(enseada_chi,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_enseada_san = summarise(enseada_san,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_pitangueiras_pug = summarise(pitangueiras_pug,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_pitangueiras_sil = summarise(pitangueiras_sil,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_asturias = summarise(asturias,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_tombo = summarise(tombo,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))

sum_guaiuba = summarise(guaiuba,
                           media = mean(Enterococcus),
                           desvio_padrão = sd(Enterococcus),
                           mediana = median(Enterococcus),
                           Q1 = quantile(Enterococcus,0.25),
                           Q2 = quantile(Enterococcus,0.75),
                           mínimo = min(Enterococcus),
                           máximo = max(Enterococcus))


# Segundo Item


graph1 <- ggplot(dados) +
  aes(x=reorder(Beach,-Enterococcus), y=Enterococcus, fill = Beach) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c("darkorchid4", "brown2", "aquamarine3", "burlywood", "gold", "lightpink1", "olivedrab2", "royalblue1", "tan3", "springgreen", "orangered4", "azure4"))



# Terceiro Item

graph2 <- ggplot(dados, aes(x="", y=Enterococcus, fill=Beach))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("darkorchid4", "brown2", "aquamarine3", "burlywood", "gold", "lightpink1", "olivedrab2", "royalblue1", "tan3", "springgreen", "orangered4", "azure4"))


# Quarto Item

graph3 <- ggplot(dados, aes(x=Beach, y=Enterococcus, fill=Enterococcus))+
  geom_histogram(stat = "identity",fill='blue')

# Quinto Item

graph4 <- ggplot(dados, aes(x=Beach, y=Enterococcus, color=Beach))+
  geom_boxplot() +
  scale_color_manual(values=c("darkorchid4", "brown2", "aquamarine3", "burlywood", "gold", "lightpink1", "olivedrab2", "royalblue1", "tan3", "springgreen", "orangered4", "azure4"))
  
  




