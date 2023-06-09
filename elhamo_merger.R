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


usek <- "Starek/zeleznice/15_ELHAMO/Zmluvn� rozpo�et/elhamo.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0,col_types = "text"))
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
  obj <- df_objekt$nazov_polozky[3]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na("jednotkova_cena")
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
write_xlsx(df_full,"Starek/zeleznice/15_ELHAMO/Zmluvn� rozpo�et/elhamo_objekt_UHP.xlsx")

######################TEORETICKY ROZPOCET#########################################
usek <- "Starek/zeleznice/15_ELHAMO/Teoretick� rozpo�et/12XK24001_02_Z_S�hrnn� Rozpo�et.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 113))
names(data) <- nazvy
colnames <- colnames(data$`PS 00-22-01 - PS 00-22-01...`)
colnames = c("objekt","Typ","K�d","Popis","m1","m2","m3", "MJ","Mno�stvo", "J.cena [EUR]","m4", "Cena celkom [EUR]")
colnames2 <- colnames(data$`PS 00-22-01 - PS 00-22-01...`)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt","K�d","Popis","MJ","Mno�stvo", "J.cena [EUR]","Cena celkom [EUR]")
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
  df_clean <- df_objekt %>% drop_na("J.cena [EUR]")
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
write_xlsx(df_full,"Starek/zeleznice/15_ELHAMO/Teoretick� rozpo�et/elhamo_UHP_teoretick� rozpo�et.xlsx")
