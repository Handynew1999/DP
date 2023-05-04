library(readxl)
library(xlsx)
library("writexl")
library(dplyr)
library(tidyverse)
library(tidymodels)
library(bestNormalize)
library(data.table)
library(broom)
library(boot)
library(purrr)
library(plyr)
library(Metrics)
library(multDM)
library(ggplot2)
library(plotrix)
library(XLConnect)


usek <- "Starek/zeleznice/11_Poprad-Lucivna/Zmluvný rozpoèet/UCS_PS_SO_401.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
colnames <- colnames(data$`PS 403-21-01`)
colnames = c("PÈ","Typ","Kód","Popis","m1","m2","m3", "MJ","Množstvo", "J.cena [EUR]","m4", "Cena celkom [EUR]")
colnames2 <- colnames(data$`PS 402-22-05.1`)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("PÈ","Kód","Popis","MJ","Množstvo", "J.cena [EUR]","Cena celkom [EUR]")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{

  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  if (is.na(df_objekt$Popis[3]))
  {
    obj <- df_objekt$Popis[7]
    df_objekt$PÈ <- obj
  }
  
  
  df_clean <- df_objekt 
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)


write_xlsx(df_fullcomplete,"Starek/zeleznice/PPLC_format_UHP_objekty.xlsx")

df_fullcomplete <- data.frame(colnames(stlpce_zachovanie))

######USEK 02###########

usek <- "Starek/zeleznice/6_ERTMS/oceneny VV zo zmluvy/usek02.xls"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 6))
names(data) <- nazvy
colnames <- c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom","technologia")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- num_obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
write_xlsx(df_full,"Starek/zeleznice/PPLC_usek02_format_UHP.xlsx")


######ETCS
usek <- "Starek/zeleznice/6_ERTMS/oceneny VV zo zmluvy/ETCS.xls"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 6))
names(data) <- nazvy
colnames <- c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom","technologia")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- num_obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
  df_full$jednotkova_cena <- as.numeric(df_full$jednotkova_cena)
  df_full$cena_celkom <- as.numeric(df_full$cena_celkom)
}



write_xlsx(df_full,"Starek/zeleznice/ERTMS_ETCS_format_UHP.xlsx")



######################TEORETICKY ROZPOCET###################################################
usek <- "Starek/zeleznice/11_Poprad-Lucivna/teoretický rozpoèet/UÈS 403.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
colnames <- data$`PS 401-21-01`
colnames <- c("objekt", "m1","kod_kp", "nazov_polozky","m2","m3","m4","jednotka","mnozstvo", "jednotkova_cena","m5","cena_celkom")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- num_obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}

df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)

write_xlsx(df_fullcomplete,"Starek/zeleznice/11_Poprad-Lucivna/Zmluvný rozpoèet/PPLC_format_UHP.xlsx")
