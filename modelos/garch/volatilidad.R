
#########################
# VOLATILIDAD SERIES MENSUALES
#########################

rm(list=ls())

setwd("//bcfs1/departamento/PROMIECO/Modelos_Macroeconomicos/Personal_Folders/Nerys Ramirez/2024/Ipom I/exp_incertidumbre") 

library(tidyverse)
library(readxl)
library(stargazer)


data <- read_excel("data_vol.xlsx", sheet = 1)[-c(1)] %>% 
  as.tibble() %>%
  mutate(
    across(everything(),
           ~((log(.)-log(dplyr::lag(.)))*100)
    )) %>%
  na.omit()

head(data)

library(strucchange)

quiebres <- list()
for (b in 1:ncol(data)){
  quiebres[[b]] <- breakpoints(data[[b]] ~ 1)
}

quiebres[[5]]$breakpoints
plot(data$ipc, type = "l")

#bp_tia = breakpoints(data$tia_pp ~ 1)

adf.test(data$tia_pp)
adf.test(data$tia_180)
adf.test(data$tia_5plus)
adf.test(data$imae)
adf.test(data$ipc)
adf.test(data$credito)
adf.test(data$tcn)

#tia_pp %>% plot(type = "l")

models0 <- list()

for (i in 1:ncol(data)){
  models0[[i]] <- garchFit( ~  garch(1, 1), trace = F, data = data[,i])
}

stargazer(models0, type="text")


vols_models <- lapply(models0, function(x) x@sigma.t)
dataVol <- data.frame(do.call(cbind, vols_models))
colnames(dataVol) <- colnames(data)

dataVol %>%  head()

library(openxlsx)
write.xlsx(dataVol, "vol.xlsx")


dataVol %>%
  mutate(fecha = 1:nrow(dataVol)) %>%
  as.data.frame() %>%
  pivot_longer(!c(fecha, vix), names_to = "var", values_to = "value") %>%
  ggplot(aes(x = fecha, y = value)) +
  geom_line(aes(color = var))+
  facet_wrap(~var, scales='free_y', ncol = 2) +
  theme_classic()+
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        strip.background = element_blank()) +
  scale_colour_grey(start = 0.1,end = 0.1)


#########################
# VOLATILIDAD SERIES DIARIAS
#########################

rm(list=ls())

setwd("//bcfs1/departamento/PROMIECO/Modelos_Macroeconomicos/Personal_Folders/Nerys Ramirez/2024/Ipom I/exp_incertidumbre") 

library(tidyverse)
library(readxl)
library(stargazer)


data <- read_excel("data_vol.xlsx", sheet = 3)[-c(1)] %>% 
  as.tibble() %>%
  mutate(
    across(everything(),
           ~((log(.)-log(dplyr::lag(.)))*100)
    )) %>%
  na.omit()

head(data)

library(strucchange)

quiebres <- list()
for (b in 1:ncol(data)){
  quiebres[[b]] <- breakpoints(data[[b]] ~ 1)
}



#bp_tia = breakpoints(data$tia_pp ~ 1)

adf.test(data$tia_pp)
adf.test(data$tia_180)
adf.test(data$tia_5plus)
adf.test(data$tcn)

#tia_pp %>% plot(type = "l")

models0 <- list()

for (i in 1:ncol(data)){
  models0[[i]] <- garchFit( ~  garch(1, 1), trace = F, data = data[,i])
}

stargazer(models0, type="text")

vols_models <- lapply(models0, function(x) x@sigma.t)
dataVol <- data.frame(do.call(cbind, vols_models))
colnames(dataVol) <- colnames(data)

dataVol %>%  head()

plot(dataVol$tcn, type = "l")
plot(dataVol$tia_180, type = "l")
plot(dataVol$tia_5plus, type = "l")
plot(dataVol$tia_pp, type = "l")

library(openxlsx)
write.xlsx(dataVol, "vol_diaria.xlsx")

#########################
# VOLATILIDAD SERIES DIARIAS DE BLOOMBERG
#########################

rm(list=ls())

setwd("//bcfs1/departamento/PROMIECO/Modelos_Macroeconomicos/Personal_Folders/Nerys Ramirez/2024/Ipom I/exp_incertidumbre") 

library(tidyverse)
library(readxl)
library(stargazer)


data <- read_excel("data_diaria_bb.xlsx", sheet = 3)[-c(1)] %>% 
  as.tibble() %>%
  mutate(
    across(everything(),
           ~((log(.)-log(dplyr::lag(.)))*100)
    )) %>%
  na.omit()

head(data)

library(strucchange)

quiebres <- list()
for (b in 1:ncol(data)){
  quiebres[[b]] <- breakpoints(data[[b]] ~ 1)
}



#bp_tia = breakpoints(data$tia_pp ~ 1)

adf.test(data$tia_pp)
adf.test(data$tia_180)
adf.test(data$tia_5plus)
adf.test(data$tcn)

#tia_pp %>% plot(type = "l")

models0 <- list()

for (i in 1:ncol(data)){
  models0[[i]] <- garchFit( ~  garch(1, 1), trace = F, data = data[,i])
}

stargazer(models0, type="text")

vols_models <- lapply(models0, function(x) x@sigma.t)
dataVol <- data.frame(do.call(cbind, vols_models))
colnames(dataVol) <- colnames(data)

dataVol %>%  head()

plot(dataVol$wti, type = "l")
plot(dataVol$maiz, type = "l")
plot(dataVol$trigo, type = "l")
plot(dataVol$sp500, type = "l")

library(openxlsx)
write.xlsx(dataVol, "vol_diaria_bloomberg.xlsx")
