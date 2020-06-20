---
title: "Thesis Statistical Methods"
author: "Mathew Tello"
date: "12 de junio de 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**TITLE:** Hyperkeystone Palm Species Demography: Preliminary Modelling of *Mauritia flexuosa* for Sustainable Harvest in Tena, Ecuador.  
**AUTHORS:** Tello, M. & Penhuela, M.C.  
**INTRODUCTION:**  
**OBJECTIVES:** Assess the model and further identification of the size ranges and processes that contribute the most to the population stability for management purposes.  
**METHODS:** For this, a 1-ha plot was established on a flooded forest dominated by *M.flexuosa* in the Napo river basin where 571 individuals were sampled for three years on a monthly basis.
![Scheme](C:/Users/LENOVO/Desktop/Methods2.jpg)
This documents specifies the detailed R proccess for establishing each vital rate model for the building an IPM.

***

### **Model Option 1:**
##### *Number of leaves present*

````{r,message=FALSE,warning=FALSE}
library(dplyr)
setwd("C:/Users/LENOVO/Documents")
datos <- read.csv2("MISAHUALLI_Morete_Dic_12_2018_MCP-IKIAM.csv")
datos <- filter(datos, Common_name == "Morete")
datos <- dplyr::select(datos,Hojas.Vivas,Hojas.Vivas.25,Notes_Enero_.2019)
for(i in 1:375){
  if(datos$Notes_Enero_.2019[i]=="muerta"){
    datos$surv[i] <- 0
  } else{
    datos$surv[i] <- 1
  }
}
bald1 <- datos$Hojas.Vivas
bald2 <- datos$Hojas.Vivas.25
datos <- datos[-3]
colnames(datos) <- c("size","sizeNext","surv")
m1 <- glm(sizeNext~size,data = datos)
summary(m1)
plot(bald1,bald2)
```

### **Model Option 2:**
##### *Leaf production rate mean*

````{r,message=FALSE,warning=FALSE}
library(dplyr)
setwd("C:/Users/LENOVO/Documents")
datos_p <- read.csv2("MISAHUALLI_Plántulas_25_11_18.csv")
datos <- read.csv2("MISAHUALLI_Morete_Dic_12_2018_MCP-IKIAM.csv")
datos <- dplyr::filter(datos, Common_name == "Morete")
datos <- filter(datos, EstadioClass != "A")
for(i in 1:262){
  if(datos$Notes_Febrero_.2018[i]=="muerta"){
    datos$surv[i] <- 0
  } else{
    datos$surv[i] <- 1
  }
}
datos <- dplyr::select(datos,ID,surv,contains("vivas"))
#datos <- datos[-1]
#datos <- datos[-29]
geo <- c()
for(i in 1:262){
  for(j in 1:25){
    prom = c()
    prom[i] <- datos[i+1,j]/datos[i,j]
    geo[i] <- prod(prom, na.rm = TRUE)
  }
}
df <- cbind(datos,geo)
df <- dplyr::select(df,ID,surv,Hojas.Vivas,geo)

datos <- read.csv2("MISAHUALLI_Morete_Dic_12_2018_MCP-IKIAM.csv")
datos <- dplyr::filter(datos, Common_name == "Morete")
datos <- filter(datos, EstadioClass == "A")
datos$surv <- 1
datos <- dplyr::select(datos,ID,surv,Hojas.Vivas,
                       Hojas.Vivas.1,
                       Hojas.Vivas.2,
                       Hojas.Vivas.3,
                       Hojas.Vivas.25)
geo <- c()
for(i in 1:113){
  for(j in 3:7){
    prom = c()
    prom[i] <- datos[i+1,j]/datos[i,j]
    geo[i] <- prod(prom, na.rm = TRUE)
  }
}

df2 <- cbind(datos,geo)
df2 <- dplyr::select(df2,ID,surv,Hojas.Vivas,geo)

dff <- rbind(df,df2)

datos <- filter(dff, geo!=Inf)

fec <- read.csv2("Curso IPM/Fe.csv")
IPM <- full_join(datos, fec, by = "ID")

size <- datos$Hojas.Vivas
sizeNext <- datos$Hojas.Vivas * datos$geo
surv <- datos$surv
fec <- IPM$Proporcion

df <- cbind(size,sizeNext,surv,fec)

matriz_rec <- read.csv("matriz_rec.csv")
df <- rbind(df, matriz_rec)

write.csv(df, "matriz.csv")

m2 <- glm(sizeNext~size, data = dff)
summary(m2)
plot(size,sizeNext)
```

### Model Option 3:
##### Maximum number of leaves

````{r,message=FALSE,warning=FALSE}
library(dplyr)
setwd("C:/Users/LENOVO/Documents")
datos <- read.csv2("MISAHUALLI_Morete_Dic_12_2018_MCP-IKIAM.csv")
datos <- filter(datos, Common_name == "Morete")
#datos <- filter(datos, EstadioClass != "A")
datos <- dplyr::select(datos,c(H,contains("vivas")))
datos <- datos[-1]
datos <- datos[-29]
maxim=c()
for(i in 1:375){
  for(j in 1:12){
    maxim[i] <- max(datos[i,],na.rm = TRUE)
  }
}
size <- datos$Hojas.Vivas
sizeNext <- maxim 
m3 <- glm(sizeNext~size)
summary(m3)
plot(size,sizeNext)
```
