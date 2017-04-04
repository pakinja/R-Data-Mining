library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(plyr)
library(gmodels)
library(RColorBrewer)


#pv <- read.csv('Rapsodiaecom.csv', fileEncoding = "UTF-8")
pv <- read.csv('Tommyecom.csv', fileEncoding = "UTF-8")

pv$FechaRegistro <- as.Date(pv$FechaRegistro,  "%m/%d/%Y")
pv$FechaOrden <- as.Date(pv$FechaOrden,  "%m/%d/%Y")
pv$Cumpleaños <- as.Date(pv$Cumpleaños,  "%m/%d/%Y")

pv$LugarEnvio <- as.factor(gsub(" ","", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub(" ","", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("Mexico","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("Ciudad de Mexico","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("MExico","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("México D.F.","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("México","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("CDMX D.F.","CDMX", pv$LugarEnvio))
pv$LugarEnvio <- as.factor(gsub("OAXACA DE JUÁREZ","OAXACA", pv$LugarEnvio))
pv$LugarEnvio <- toupper(pv$LugarEnvio)

pv$Talla <- tolower(as.factor(gsub(" ","", pv$Talla)))
