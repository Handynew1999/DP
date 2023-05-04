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


usek <- "Starek/zeleznice/14_Uzol ZA/Zmluvný rozpoèet/Uzol ZA.xlsx"
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
  obj <- df_objekt$objekt[2]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
df_full <-df_full[-1,]
write_xlsx(df_full,"Starek/zeleznice/14_Uzol ZA/Zmluvný rozpoèet/Uzol ZA_UHP.xlsx")

##############TEORETICKY ROZPOCET################################################
usek <- "Starek/zeleznice/14_Uzol ZA/Teoretický rozpoèet/Uzol_ZA_PHZ.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 6))
names(data) <- nazvy
colnames <- c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
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
df_full <-df_full[-1,]
write_xlsx(df_full,"Starek/zeleznice/14_Uzol ZA/Teoretický rozpoèet/Uzol_ZA_PHZ_UHP.xlsx")
