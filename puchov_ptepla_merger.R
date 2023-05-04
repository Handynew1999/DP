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


usek <- "Starek/zeleznice/8_Puchov-P.Tepla_I.etapa/Zmluvný rozpoèet/0501_I_DVZ_UCS_47.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
colnames <- c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom","techno")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  obj <- df_objekt$objekt[2]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na("jednotkova_cena")
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)
write_xlsx(df_fullcomplete,"Starek/zeleznice/8_Puchov-P.Tepla_I.etapa/Zmluvný rozpoèet/Puchov_objekty_UHP.xlsx")

df_fullcomplete <- data.frame(colnames(stlpce_zachovanie))




######USEK 02###########

usek <- "Starek/zeleznice/8_Puchov-P.Tepla_I.etapa/usek00.xls"
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
write_xlsx(df_full,"Starek/zeleznice/ERTMS_usek02_format_UHP.xlsx")


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

#############TEORETICKY ROZPOCET################################################
usek <- "Starek/zeleznice/8_Puchov-P.Tepla_I.etapa/Teoretický rozpoèet/0501_I_DVZ_UÈS_47_ocenené VV.xls"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
colnames <- c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom","techno")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  obj <- df_objekt$objekt[2]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}

df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)

write_xlsx(df_fullcomplete,"Starek/zeleznice/8_Puchov-P.Tepla_I.etapa/Teoretický rozpoèet/PuchovPT_PHZ_UHP.xlsx")
