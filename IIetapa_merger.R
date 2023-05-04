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


usek <- "Starek/zeleznice/4_P.Tepla-Zilina II.etapa/Zmluvný rozpoèet/DVZ_501_2_su_00_PS_VV.xlsx"
nazvy <- excel_sheets(usek)
data <- lapply(nazvy, function(X) readxl::read_excel(usek, sheet = X, skip = 0))
names(data) <- nazvy
colnames <- c("objekt","m1","m2","kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","m3","cena_celkom")
data <- lapply(data, setNames, colnames)
stlpce_zachovanie = c("objekt", "kod_kp", "nazov_polozky","jednotka","mnozstvo", "jednotkova_cena","cena_celkom")
df_full <- data.frame(colnames(stlpce_zachovanie))

for (sheet in nazvy)
{
  num_obj = sheet
  df_objekt <- as.data.frame(copy(data[sheet]))
  colnames(df_objekt) <- colnames
  obj <- df_objekt$nazov_polozky[1]
  if (length(df_objekt$objekt)>0)
  {
    df_objekt$objekt <- obj
  }
  df_clean <- df_objekt %>% drop_na(jednotkova_cena)
  df_clean <-select(df_clean, stlpce_zachovanie) 
  df_full <- rbind.data.frame(df_full,df_clean)
}
df_full <-df_full[-1,]
write_xlsx(df_full,"Starek/zeleznice/4_P.Tepla-Zilina II.etapa/Zmluvný rozpoèet/PT_ZA_II_etapa_UHP.xlsx")
