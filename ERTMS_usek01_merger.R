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


usek <- "Starek/zeleznice/6_ERTMS/Zmluvný rozpoèet/usek02.xls"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
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
  obj <- df_objekt$objekt[4]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}

df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)
write_xlsx(df_fullcomplete,"Starek/zeleznice/6_ERTMS/Zmluvný rozpoèet/ERTMS_UHP_zmluvný.xlsx")

df_fullcomplete <- data.frame(colnames(stlpce_zachovanie))

###################################TEORETICKÝ ROZPOÈET############################

usek <- "Starek/zeleznice/6_ERTMS/Teoretický rozpoèet/1004_GSM-R_úsek02 - DVZ_VV-ocenený.xls"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
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
  obj <- df_objekt$objekt[4]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}

df_fullcomplete <- rbind.data.frame(df_fullcomplete,df_full)
write_xlsx(df_fullcomplete,"Starek/zeleznice/6_ERTMS/Teoretický rozpoèet/ERTMS_UHP_teoretický.xlsx")

