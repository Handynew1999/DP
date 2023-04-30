library(readxl)
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
library(rstatix)
library(stringr)
library(caret)
library(robustbase)
library(univOutl)
library(moments)
library(MLmetrics)
library(mgcv)
library(splines2)

data <-read_excel("C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/DATABAZA.xlsx")

data <- read_excel("C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/databaza predloha.xlsx")

test_data <- read_excel("C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/TEST_DATABAZA.xlsx")

###########Èistenie dát###########
data <- filter(data, Projekt != "Modernizácia železniènej trate Žilina – Košice, úsek trate Liptovský Mikuláš – Poprad Tatry (mimo), 1.etapa (Poprad - Luèivná)")
data <- filter(data, Projekt != "Elektrifikácia trate Haniska pri Košiciach – Moldava nad Bodvou")
data <- filter(data, Projekt != "Modernizácia žel. trate Nové Mesto nad Váhom - Púchov, úsek: Beluša – Púchov")
data <- filter(data, Projekt != "ŽSR, Modernizácia trate Púchov - Žilina, pre rýchlos do 160 km/hod., II. etapa - (úsek Považská Teplá /mimo/ - Žilina /mimo/)")
data <- filter(data, Projekt != "ŽSR, Dostavba zriaïovacej stanice Žilina-Teplièka a nadväzujúcej železniènej infraštruktúry v uzle Žilina")
data <- filter(data, Projekt != "ŽSR, Modernizácia železniènej trate Púchov – Žilina, pre traovú rýchlos do 160 km/hod. – I. etapa, úsek Púchov – Považská Teplá")
data <- filter(data, Projekt != "Zavedenie ERTMS v úseku Bratislava - Žilina - Èadca - št.hr.SR/ÈR")


data <- filter(data, `Indexovaná cena bez DPH_v2` != 0)
data <- filter(data, `Jednotková cena` != 0)
data <- na.omit(data)
data <- data[!grepl("kpl", data$`Merná jednotka`),]
data <- data[!grepl("KPL", data$`Merná jednotka`),]
data <- data[!grepl("sub", data$`Merná jednotka`),]
data <- data[!grepl("SUB", data$`Merná jednotka`),]
data <- data[!grepl("%", data$`Merná jednotka`),]
data <- data[!grepl("sada", data$`Merná jednotka`),]
data <- data[!grepl("úsek", data$`Merná jednotka`),]
data <- data[!grepl("úsek", data$`Merná jednotka`),]
data <- data[!grepl("poplatok", data$`Názov položky`),]
data <- data[!grepl("poplatky", data$`Názov položky`),]
data <- data[(which(nchar(data$`Èíslo položky`) > 4)),]
vymazat <- grepl("^[^0-9]", data$`Èíslo položky`)
data <- data[!vymazat, ]

####ANALYZA VOLATILITY
###RELATIVNA VOLATILITA PRE V2
relativne_sd <- data %>% 
  unite(`cislo_polozky_kod_indexv1_indexv2`,c(`Èíslo položky_m. j.`,`Kód indexu_v1`,`Kód indexu_nazov`),remove=FALSE) %>%
  group_by(`cislo_polozky_kod_indexv1_indexv2`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)/mean(`Indexovaná jednotková cena_v2`)) %>%
  na.omit()

relativne_sd_desc <- relativne_sd$`Èíslo položky_m. j.`[relativne_sd$`sd(\`Indexovaná jednotková cena_v2\`) / ...`>sqrt(12)]
###ABSOLUTNA VOLATILITA PRE V2
sd <- data_nazov %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)) %>%
  na.omit()

sd_desc <- sd$`Èíslo položky_m. j.`[sd$`sd(\`Indexovaná jednotková cena_v2\`)`>25000]
###RELATIVNA VOLATILITA PRE V1
sd <- data %>%
  unite(`cislo_polozky_kod_indexv1_indexv2`,c(`Èíslo položky_m. j.`,`Kód indexu_v1`,`Kód indexu_nazov`),remove=FALSE) %>%
  group_by(`cislo_polozky_kod_indexv1_indexv2`) %>%
  distinct(Projekt,`Indexovaná jednoková cena_v1`) %>%
  dplyr::summarise(sd(`Indexovaná jednoková cena_v1`)/mean(`Indexovaná jednoková cena_v1`)) %>%
  na.omit()
###RELATIVNA VOLATILITA PRE MD19
sd <- data %>%
  unite(`cislo_polozky_kod_indexv1_indexv2`,c(`Èíslo položky_m. j.`,`Kód indexu_v1`,`Kód indexu_nazov`),remove=FALSE) %>%
  group_by(`cislo_polozky_kod_indexv1_indexv2`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_MD_19`) %>%
  dplyr::summarise(sd(`Indexovaná jednotková cena_MD_19`)/mean(`Indexovaná jednotková cena_MD_19`)) %>%
  na.omit()

sd <- join(sd, pocet_vyskytov, by="Èíslo položky_m. j.", type='left', match='first')


sd <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`), FUN=sd)
sd <- na.omit(sd) 
relativne_sd <- sd
relativne_sd$x <- sd$x/ priemery$x[ match( sd$cislo_a_mj, priemery$cislo_a_mj) ]


########Detekovanie outlierov a ich vylúèenie z dát###############################################################################################################
########PRVY KROK I DRUHY KROK TUKEY 3.0
df_outliers <- data %>% 
  group_by(`Èíslo položky_m. j.`,Projekt) %>% 
  identify_outliers("Jednotková cena") 

df_extremeoutliers <- filter(df_outliers, is.extreme == "TRUE")

cleandata <- data %>% 
  anti_join(df_extremeoutliers, by = "ID") 

cleandata_s_outliermi <- data %>% 
   anti_join(df_extremeoutliers, by = "ID") 


df_outliers_complete <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  identify_outliers("Indexovaná jednotková cena_v2")

df_extremeoutliers_complete <- filter(df_outliers_complete, is.extreme == "TRUE")

#for (i in 1:length(cleandata$ID)) {
#  for (j in 1:length(df_outliers_complete$`Indexovaná jednotková cena_v2`)) {
#    if (cleandata$`Indexovaná jednotková cena_v2`[i]==df_outliers_complete$`Indexovaná jednotková cena_v2`[j] & cleandata$`Èíslo položky_m. j.`[i]==df_outliers_complete$`Èíslo položky_m. j.`[j])
#    {
#      cleandata <- slice(cleandata,-i)
#    }
#    
#  }
#  
#}


cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
df_extremeoutliers_complete$spojenie <- str_c(df_extremeoutliers_complete$`Indexovaná jednotková cena_v2`, df_extremeoutliers_complete$`Èíslo položky_m. j.`)

cleancleandata <- cleandata_s_outliermi %>%
  anti_join(df_extremeoutliers_complete, by = "spojenie")

data_nazov <- cleancleandata[1:(length(cleancleandata)-1)]
#####################################################################

#######PRVY KROK 3.0, DRUHY KROK 1.5################################



###################################################################

#######ŽIADNY PRVY KROK, ROVNO IBA 3.0############################
df_outliers_complete <- data %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  identify_outliers("Indexovaná jednotková cena_v2")

df_extremeoutliers <- filter(df_outliers_complete, is.extreme == "TRUE")

data_test <- data

data_test$spojenie <- str_c(data_test$`Indexovaná jednotková cena_v2`, data_test$`Èíslo položky_m. j.`)
df_extremeoutliers$spojenie <- str_c(df_extremeoutliers$`Indexovaná jednotková cena_v2`, df_extremeoutliers$`Èíslo položky_m. j.`)

cleancleandata <- data_test %>%
  anti_join(df_extremeoutliers, by = "spojenie")



outlierdata <- left_join(data, df_outliers, by = "ID")

outlierdata <- merge(data, df_outliers, by = "ID")

write_xlsx(df_outliers,"outliers.xlsx")

write_xlsx(outlierdata,"outlierdata.xlsx")


data_grouped <- data %>% 
  group_by(`Èíslo položky_m. j.`,Projekt)  
  


#######ADJUSTED BOXPLOT
data_adjoutlier <-  data %>% 
  group_by(`Èíslo položky_m. j.`) %>% 
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  filter(`Indexovaná jednotková cena_v2` %in%  adjboxStats(`Indexovaná jednotková cena_v2`, coef = 1.5,
                                     a = -4, b = 3, do.conf = TRUE, do.out = TRUE)$out)

data_test <- data

data_test$spojenie <- str_c(data_test$`Indexovaná jednotková cena_v2`, data_test$`Èíslo položky_m. j.`)
data_adjoutlier$spojenie <- str_c(data_adjoutlier$`Indexovaná jednotková cena_v2`, data_adjoutlier$`Èíslo položky_m. j.`)

cleancleandata <- data_test %>%
  anti_join(data_adjoutlier, by = "spojenie")



data_adjoutlier_complete <-  cleandata_s_outliermi %>% 
  group_by(`Èíslo položky_m. j.`) %>% 
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  filter(`Indexovaná jednotková cena_v2` %in%  adjboxStats(`Indexovaná jednotková cena_v2`, coef = 1.5,
                                                            a = -4, b = 3, do.conf = TRUE, do.out = TRUE)$out)


cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
data_adjoutlier_complete$spojenie <- str_c(data_adjoutlier_complete$`Indexovaná jednotková cena_v2`, data_adjoutlier_complete$`Èíslo položky_m. j.`)

cleancleandata <- cleandata_s_outliermi %>%
  anti_join(data_adjoutlier_complete, by = "spojenie")

data_nazov <- cleancleandata


data_adjoutlier <- df_adjoutliers

############MATUSOVA METODA###################################

median_metoda <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  filter((`Indexovaná jednotková cena_v2` > 12*median(`Indexovaná jednotková cena_v2`) | `Indexovaná jednotková cena_v2` < median(`Indexovaná jednotková cena_v2`)/12 | `Indexovaná jednotková cena_v2` < median(`Indexovaná jednotková cena_v2`) - 50000 | `Indexovaná jednotková cena_v2` > median(`Indexovaná jednotková cena_v2`) + 50000  ))


cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
median_metoda$spojenie <- str_c(median_metoda$`Indexovaná jednotková cena_v2`, median_metoda$`Èíslo položky_m. j.`)

cleancleandata <- cleandata_s_outliermi %>%
  anti_join(median_metoda, by = "spojenie")


median_metoda <- data %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  filter((`Indexovaná jednotková cena_v2` > 12*median(`Indexovaná jednotková cena_v2`) | `Indexovaná jednotková cena_v2` < median(`Indexovaná jednotková cena_v2`)/12 | `Indexovaná jednotková cena_v2` < median(`Indexovaná jednotková cena_v2`) - 50000 | `Indexovaná jednotková cena_v2` > median(`Indexovaná jednotková cena_v2`) + 50000  ))

data_test <- data

data_test$spojenie <- str_c(data_test$`Indexovaná jednotková cena_v2`, data_test$`Èíslo položky_m. j.`)
median_metoda$spojenie <- str_c(median_metoda$`Indexovaná jednotková cena_v2`, median_metoda$`Èíslo položky_m. j.`)

cleancleandata <- data_test %>%
  anti_join(median_metoda, by = "spojenie")


############RELATIVNA VOLATILITA PRE V2###############################################
relativne_sd <- cleandata_s_outliermi %>% 
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)/mean(`Indexovaná jednotková cena_v2`)) %>%
  na.omit()

relativne_sd_desc <- relativne_sd$`Èíslo položky_m. j.`[relativne_sd$`sd(\`Indexovaná jednotková cena_v2\`) / ...`>sqrt(12)]
############ABSOLUTNA VOLATILITA PRE V2################################################
sd <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)) %>%
  na.omit()

sd_desc <- sd$`Èíslo položky_m. j.`[sd$`sd(\`Indexovaná jednotková cena_v2\`)`>25000]


###########Vypocet priemerov poloziek###########
ceny <- aggregate(cbind(data$`Indexovaná cena bez DPH_v2`,data$Množstvo), by=list(cislo_a_mj=data$`Èíslo položky_m. j.`,projekt=data$Projekt), FUN=sum)

ceny$V1 <- ceny$V1/ceny$V2

priemery <- aggregate(ceny$V1, by=list(cislo_a_mj=ceny$cislo_a_mj),FUN=mean)

#####POZOSTATKY STAREJ VERZIE 
ceny <- aggregate(cbind(data_nazov$`Indexovaná cena bez DPH_v2`,data_nazov$Množstvo), by=list(cislo_a_mj=data_nazov$`Èíslo položky_m. j.`), FUN=sum)

ceny$V1 <- ceny$V1/ceny$V2

priemery <- aggregate(ceny$V1, by=list(cislo_a_mj=ceny$cislo_a_mj),FUN=mean)



###########Unikátne hodnoty jednotkových cien pre každý projekt+min/max statistics with standard deviation###########
data2 <- distinct(data, Projekt,`Èíslo položky_m. j.`,`Indexovaná jednotková cena_v2`, .keep_all= TRUE)
unikat <-data.frame(data2$`Èíslo položky_m. j.`,data2$`Indexovaná jednotková cena_v2`)
unikat_priemer <- aggregate(unikat$data2..Indexovaná.jednotková.cena_v2., by=list(cislo_a_mj=unikat$data2..Èíslo.položky_m..j..), FUN=mean)

unikat <- aggregate(data_nazov$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data_nazov$`Èíslo položky_m. j.`,projekt=data_nazov$Projekt), FUN=mean)
unikat_priemer <- aggregate(unikat$x, by=list(cislo_a_mj=unikat$cislo_a_mj), FUN=mean)


min <- aggregate(data_nazov$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data_nazov$`Èíslo položky_m. j.`), FUN=min)
sd <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`), FUN=sd)
sd <- na.omit(sd) 
relativne_sd <- sd
relativne_sd$x <- sd$x/ priemery$x[ match( sd$cislo_a_mj, priemery$cislo_a_mj) ]

#Najoptimalnejší percentil 
percentil <- aggregate(data_nazov$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data_nazov$`Èíslo položky_m. j.`,projekt=data_nazov$Projekt), FUN=mean)
percentil <- aggregate(percentil$x, by=list(cislo_a_mj=percentil$cislo_a_mj), FUN='quantile',probs=38/100)

#Median
median <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`,projekt=data$Projekt), FUN='quantile',probs=50/100)
median <- aggregate(median$x, by=list(cislo_a_mj=median$cislo_a_mj), FUN='quantile',probs=50/100)


sd_desc <- arrange(sd,desc(sd$x))
sd_desc <- sd_desc[1:20,]

relativne_sd_desc <- arrange(relativne_sd,desc(relativne_sd$x))
relativne_sd_desc <- relativne_sd_desc[1:20,]

write_xlsx(priemery,"Starek/zeleznice/priemery.xlsx")
write_xlsx(unikat_priemer,"Starek/zeleznice/unikat.xlsx")

write_xlsx(priemery,"Starek/zeleznice/priemery_nove.xlsx")
write_xlsx(unikat_priemer,"Starek/zeleznice/unikat_nove.xlsx")

write_xlsx(sd,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/sd.xlsx")
write_xlsx(relativne_sd,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/relativne_sd.xlsx")


###########Vykreslenie histogramu formou unikátne j.c./priemer###########
data2 <- distinct(data, Projekt,`Èíslo položky_m. j.`,`Indexovaná jednotková cena_v2`, .keep_all= TRUE)
vysledok <- data2$`Indexovaná jednotková cena_v2`/ priemery$x[ match( data2$`Èíslo položky_m. j.`, priemery$`Èíslo položky_m. j.`) ]
hist(vysledok,xlim=c(0, 2),breaks=2000)

skewn <-data_nazov %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarize(median_poloziek=median(`Indexovaná jednotková cena_v2`), priemer_polozky = mean(`Indexovaná jednotková cena_v2`)) 
   
pocet_vyskytov <- data_nazov  %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarize("pocet vyskytov polozky v projektoch" = n())

objem_databazy <- data_nazov %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarize("relatívny objem polozky" = sum(`Indexovaná cena bez DPH_v2`))

objem_databazy$`relatívny objem polozky` <- (objem_databazy$`relatívny objem polozky`/sum(data_nazov$`Indexovaná cena bez DPH_v2`))*100

skewn <- join(skewn, objem_databazy, by="Èíslo položky_m. j.", type='left', match='first')

skewn$median_poloziek <- skewn$median_poloziek*objem_databazy$`relatívny objem polozky`
skewn$priemer_polozky <- skewn$priemer_polozky*objem_databazy$`relatívny objem polozky`
skewn$rozdiel <- skewn$median_poloziek - skewn$priemer_polozky
cisielko <- sum(skewn$rozdiel[skewn$rozdiel>0])/sum(abs(skewn$rozdiel[skewn$rozdiel<0]))

test_vyskytov <- data %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarize("pocet vyskytov polozky" = n())


funkcia <- function(benchmark, percentil, min,relativne_sd_desc,sd_desc)
{
  benchmark <- benchmark[!is.na(benchmark$`Benchmark 0`),]
  benchmark <- join(benchmark, min, by="cislo_a_mj", type='left', match='first')
  names(benchmark)[names(benchmark) == "x"] <- "minimum"
  benchmark <- join(benchmark, percentil, by="cislo_a_mj", type='left', match='first')
  names(benchmark)[names(benchmark) == "x"] <- "percentil"
  benchmark[is.na(benchmark)] <- 0
  ###Podmienka benchmarku
  #&& is.element(benchmark$cislo_a_mj[i],relativne_sd_desc$cislo_a_mj)==FALSE && is.element(benchmark$cislo_a_mj[i],sd_desc$cislo_a_mj)==FALSE
  benchmark$benchmarkovat <- replicate(length(benchmark$minimum), 0)
  ##vieme benchmarkovat?
  for(i in 1:length({benchmark$percentil}))
  {
    if (benchmark$benchmarkovat[i]==0 && benchmark$percentil[i]>0 && is.element(benchmark$cislo_a_mj[i],relativne_sd_desc)==FALSE && is.element(benchmark$cislo_a_mj[i],sd_desc)==FALSE)
    {
      benchmark$benchmarkujeme[i] <- TRUE
    }
    else {
      benchmark$benchmarkujeme[i] <- FALSE
      benchmark$benchmarkovat[i] <- 1
    }
  }
  
####PRE KRÍŽOVU VALIDÁCIU BERIEME DO ÚVAHY IBA TO ÈO VIEME BENCHMARKOVA
 # benchmark <- benchmark[benchmark$benchmarkujeme != FALSE, ]
  ##priemerne ceny 

  
  
  ##priemerne opt_ceny 
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$priemery[i] < benchmark$`jednotkova teoreticka cena`[i])
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$priemery[i]
  #     benchmark$lower_to_min[i] <- "FALSE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$`jednotkova teoreticka cena`[i] < benchmark$minimum[i] && benchmark$priemery[i] > benchmark$`jednotkova teoreticka cena`[i] )
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$minimum[i]
  #     #benchmark$opt_priemer[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     benchmark$lower_to_min[i] <- "TRUE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==FALSE || (benchmark$priemery[i] > benchmark$`jednotkova teoreticka cena`[i] && benchmark$`jednotkova teoreticka cena`[i] > benchmark$minimum[i]) )
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     
  #   }
  #   
  # }
  # ###celkove opt_priemerne ceny
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   benchmark$benchmark_opt_priemer_spolu[i] <- benchmark$opt_priemer[i]*benchmark$Množstvo[i]
  # }
  # ###rozdiel benchmarku podla priemeru od zmluvnej ceny položiek
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmark_opt_priemer_spolu[i]>0)
  #   {
  #     benchmark$rozdiel_opt_priemer[i] <- benchmark$benchmark_opt_priemer_spolu[i]-benchmark$`Cena celkom`[i]
  #   }
  #   else {
  #     benchmark$rozdiel_opt_priemer[i] <-0
  #   }
  # }
  ##najoptimalnejší percentil
  for(i in 1:length({benchmark$percentil}))
  {
    if (benchmark$benchmarkujeme[i]==TRUE)
    {
      benchmark$benchmark_percentil[i] <- benchmark$percentil[i]
    }
    else 
    {
      benchmark$benchmark_percentil[i] <- benchmark$`Indexovaná teoretická jednotková cena_v2`[i]
    }
  }
  ###celkove percentilné ceny
  for(i in 1:length({benchmark$minimum}))
  {
    benchmark$benchmark_percentil_spolu[i] <- benchmark$benchmark_percentil[i]*benchmark$Množstvo[i]
  }
  ###rozdiel benchmarku podla percentilu od zmluvnej ceny položiek
  for(i in 1:length({benchmark$minimum}))
  {
    benchmark$rozdiel_percentil[i] <- benchmark$benchmark_percentil_spolu[i]-benchmark$`Indexovaná zmluvná cena bez DPH_v2`[i]
  }
  ##unikatne opt_ceny 
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$unikat[i] < benchmark$`jednotkova teoreticka cena`[i])
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$unikat[i]
  #     benchmark$lower_to_min[i] <- "FALSE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$`jednotkova teoreticka cena`[i] < benchmark$minimum[i] && benchmark$unikat[i] > benchmark$`jednotkova teoreticka cena`[i] )
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$minimum[i]
  #     # benchmark$benchmark_opt_unikat[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     benchmark$lower_to_min[i] <- "TRUE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==FALSE || (benchmark$unikat[i] > benchmark$`jednotkova teoreticka cena`[i]  && benchmark$`jednotkova teoreticka cena`[i] > benchmark$minimum[i]))
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     
  #   }
  # }
  ###celkove opt_priemerne ceny
#   for(i in 1:length({benchmark$minimum}))
#   {
#     benchmark$benchmark_opt_unikat_spolu[i] <- benchmark$benchmark_opt_unikat[i]*benchmark$Množstvo[i]
#   }
#   ###rozdiel benchmarku podla opt_unikatu od zmluvnej ceny položiek
#   for(i in 1:length({benchmark$minimum}))
#   {
#     if (benchmark$benchmark_opt_unikat_spolu[i]>0)
#     {
#       benchmark$rozdiel_opt_unikat[i] <- benchmark$benchmark_opt_unikat_spolu[i]-benchmark$`Cena celkom`[i]
#     }
#     else {
#       benchmark$rozdiel_opt_unikat[i] <-0
#     }
#   }
#   #####WANNABE KONTIGENCKA
  sumarizacia <- aggregate(cbind('Zmluvná cena'=benchmark$`Indexovaná zmluvná cena bez DPH_v2`,'Teoretické ocenenie'=benchmark$`Indexovaná teoretická celková cena bez DPH`,'percentil'=benchmark$benchmark_percentil_spolu,'cenekon'=benchmark$benchmark_cenekon_spolu,'SFDI'=benchmark$benchmark_SFDI_spolu)~`Projekt`, benchmark, sum)
  l <- nrow(sumarizacia)
  sumarizacia_spolu <- sumarizacia
  for (i in 2:ncol(sumarizacia_spolu))
  {
    sumarizacia_spolu[l+1,i] <- sum(sumarizacia_spolu[,i])
    sumarizacia_spolu[is.na(sumarizacia_spolu)] <- 0
  }
  sumarizacia_spolu[nrow(sumarizacia_spolu),1] <- 'SPOLU'
  zaver <- list(benchmark=benchmark,sumarizacia=sumarizacia, sumarizacia_spolu=sumarizacia_spolu)
  return(zaver)
}
#############################PERMUTACIE########################################################################################################################
PPLC <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/PPLC_format_UHP.xlsx")
elhamo_UHP <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/elhamo_UHP.xlsx")
Belusa_Puchov <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Belusa_Puchov.xlsx")
PT_ZA_IIetapa <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/PT-ZA-IIetapa.xlsx")
Uzol_ZA <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Uzol_ZA.xlsx")
Puchov_PT <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Puchov_PT_priprava_na_benchmark.xlsx")
ERTMS <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/ERTMS_usek01_format_UHP.xlsx")



list_benchmark <- funkcia(testing_dataset, priemery, unikat_priemer, percentil, median, min)
list_benchmark <- funkcia(elhamo_UHP, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Belusa_Puchov, priemery, unikat_priemer, min)
list_benchmark <- funkcia(PT_ZA_IIetapa, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Uzol_ZA, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Puchov_PT, priemery, unikat_priemer, min)
list_benchmark <- funkcia(ERTMS, priemery, unikat_priemer, min)

list_benchmark <- funkcia(test_data, percentil, min,relativne_sd_desc,sd_desc)

test_databaza_sumarizacia_spolu <- list_benchmark$sumarizacia_spolu
test_databaza_sumarizacia <- list_benchmark$sumarizacia
test_databaza_benchmark <- list_benchmark$benchmark

# Výber riadkov s platnými hodnotami pre zmluvnú cenu a PHZ
valid_rows_PHZ <- test_databaza_benchmark %>%
  filter(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ")

# Agregácia pod¾a projektu
aggregated_valid_rows_PHZ <- valid_rows_PHZ %>%
  group_by(Projekt) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_PHZ <- test_databaza_benchmark %>%
  group_by(Projekt) %>%
  dplyr::summarise(Benchmarkovaný_objem_PHZ = sum(ifelse(`Benchmark 0`=="PHZ", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_data_PHZ <- left_join(aggregated_valid_rows_PHZ, benchmark_objem_PHZ, by="Projekt")

sum(abs(aggregated_data_CENEKON$Zmluvná_cena-aggregated_data_CENEKON$CENEKON)*aggregated_data_CENEKON$Benchmarkovaný_objem_CENEKON)/sum(aggregated_data_CENEKON$Zmluvná_cena*aggregated_data_CENEKON$Benchmarkovaný_objem_CENEKON)*100


# Agregácia pod¾a objektu
aggregated_objects_valid_rows_PHZ <- valid_rows_PHZ %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_objects_PHZ <- test_databaza_benchmark %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Benchmarkovaný_objem_PHZ = sum(ifelse(`Benchmark 0`=="PHZ", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_objects_data_PHZ <- left_join(aggregated_objects_valid_rows_PHZ, benchmark_objem_objects_PHZ, by="Kód objektu")

###SPOLU BEZ AGREGAGACIE CENEKON
aggregated_valid_rows_PHZ <- valid_rows_PHZ %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()



benchmark_objem_PHZ <- test_databaza_benchmark %>%
  dplyr::summarise(Benchmarkovaný_objem_PHZ = sum(ifelse(`Benchmark 0`=="PHZ", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

SPOLU_PHZ <- c()
SPOLU_PHZ$Projekt <- "SPOLU"
SPOLU_PHZ <- merge(SPOLU_PHZ,aggregated_valid_rows_PHZ)
SPOLU_PHZ <- merge(SPOLU_PHZ,benchmark_objem_PHZ)


aggregated_data_PHZ[length(aggregated_data_PHZ$Projekt)+1,] <- SPOLU_PHZ

sum(abs(aggregated_data_PHZ$Zmluvná_cena-aggregated_data_PHZ$PHZ)*aggregated_data_PHZ$Benchmarkovaný_objem_PHZ)/sum(aggregated_data_PHZ$Zmluvná_cena*aggregated_data_PHZ$Benchmarkovaný_objem_PHZ)*100

write_xlsx(aggregated_data_PHZ,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_data_PHZ.xlsx")

SPOLU_objects_PHZ <- c()
SPOLU_objects_PHZ$Projekt <- "SPOLU"
SPOLU_objects_PHZ <- merge(SPOLU_objects_PHZ,aggregated_valid_rows_PHZ)
SPOLU_objects_PHZ <- merge(SPOLU_objects_PHZ,benchmark_objem_PHZ)


aggregated_objects_data_PHZ[length(aggregated_objects_data_PHZ$`Kód objektu`)+1,] <- SPOLU_objects_PHZ

write_xlsx(aggregated_objects_data_PHZ,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_objects_data_PHZ.xlsx")




# Výber riadkov s platnými hodnotami pre zmluvnú cenu, PHZ a CENEKON
valid_rows_CENEKON <- test_databaza_benchmark %>%
  filter(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ" & `Benchmark 1`=="CENEKON" & benchmarkujeme=="FALSE")

# Agregácia pod¾a projektu
aggregated_valid_rows_CENEKON <- valid_rows_CENEKON %>%
  group_by(Projekt) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   CENEKON = sum(`Cenekon celková cena`), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_CENEKON = MAPE(sum(`Cenekon celková cena`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_CENEKON <- test_databaza_benchmark %>%
  group_by(Projekt) %>%
  dplyr::summarise(Benchmarkovaný_objem_CENEKON = sum(ifelse(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ" & `Benchmark 1`=="CENEKON" & benchmarkujeme=="FALSE", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_data_CENEKON <- left_join(aggregated_valid_rows_CENEKON, benchmark_objem_CENEKON, by="Projekt")

sum(abs(aggregated_data_CENEKON$Zmluvná_cena-aggregated_data_CENEKON$CENEKON)*aggregated_data_CENEKON$Benchmarkovaný_objem_CENEKON)/sum(aggregated_data_CENEKON$Zmluvná_cena*aggregated_data_CENEKON$Benchmarkovaný_objem_CENEKON)*100


# Agregácia pod¾a objektu
aggregated_objects_valid_rows_CENEKON <- valid_rows_CENEKON %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   CENEKON = sum(`Cenekon celková cena`), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_CENEKON = MAPE(sum(`Cenekon celková cena`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_objects_CENEKON <- valid_rows_CENEKON %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Benchmarkovaný_objem_CENEKON = sum(ifelse(`Benchmark 1`=="CENEKON", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_objects_data_CENEKON <- left_join(aggregated_objects_valid_rows_CENEKON, benchmark_objem_objects_CENEKON, by="Kód objektu")

sum(abs(aggregated_objects_data_CENEKON$Zmluvná_cena-aggregated_objects_data_CENEKON$CENEKON)*aggregated_objects_data_CENEKON$Benchmarkovaný_objem_CENEKON)/sum(aggregated_objects_data_CENEKON$Zmluvná_cena*aggregated_objects_data_CENEKON$Benchmarkovaný_objem_CENEKON)*100

###SPOLU BEZ AGREGAGACIE CENEKON
aggregated_valid_rows_CENEKON <- valid_rows_CENEKON %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   CENEKON = sum(`Cenekon celková cena`), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_CENEKON = MAPE(sum(`Cenekon celková cena`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_CENEKON <- test_databaza_benchmark %>%
  dplyr::summarise(Benchmarkovaný_objem_CENEKON = sum(ifelse(`Benchmark 1`=="CENEKON", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

SPOLU_CENEKON <- c()
SPOLU_CENEKON$Projekt <- "SPOLU"
SPOLU_CENEKON <- merge(SPOLU_CENEKON,aggregated_valid_rows_CENEKON)
SPOLU_CENEKON <- merge(SPOLU_CENEKON,benchmark_objem_CENEKON)


aggregated_data_CENEKON[length(aggregated_data_CENEKON$Projekt)+1,] <- SPOLU_CENEKON


write_xlsx(aggregated_data_CENEKON,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_data_CENEKON.xlsx")

SPOLU_objects_CENEKON <- c()
SPOLU_objects_CENEKON$Projekt <- "SPOLU"
SPOLU_objects_CENEKON <- merge(SPOLU_objects_CENEKON,aggregated_valid_rows_CENEKON)
SPOLU_objects_CENEKON <- merge(SPOLU_objects_CENEKON,benchmark_objem_CENEKON)


aggregated_objects_data_CENEKON[length(aggregated_objects_data_CENEKON$`Kód objektu`)+1,] <- SPOLU_objects_CENEKON

write_xlsx(aggregated_objects_data_CENEKON,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_objects_data_CENEKON.xlsx")


# Výber riadkov s platnými hodnotami pre zmluvnú cenu, PHZ a SFDI
valid_rows_SFDI <- test_databaza_benchmark %>%
  filter(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ" & `Benchmark 2`=="SFDI")

# Agregácia pod¾a projektu
aggregated_valid_rows_SFDI <- valid_rows_SFDI %>%
  group_by(Projekt) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   SFDI = sum(`SFDI cena celkom`),
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_SFDI = MAPE(sum(`SFDI cena celkom`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_SFDI <- test_databaza_benchmark %>%
  group_by(Projekt) %>%
  dplyr::summarise(Benchmarkovaný_objem_SFDI = sum(ifelse(`Benchmark 2`=="SFDI", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_data_SFDI <- left_join(aggregated_valid_rows_SFDI, benchmark_objem_SFDI, by="Projekt")

sum(abs(aggregated_data_SFDI$Zmluvná_cena-aggregated_data_SFDI$SFDI)*aggregated_data_SFDI$Benchmarkovaný_objem_SFDI)/sum(aggregated_data_SFDI$Zmluvná_cena*aggregated_data_SFDI$Benchmarkovaný_objem_SFDI)*100


# Agregácia pod¾a objektu
aggregated_objects_valid_rows_SFDI <- valid_rows_SFDI %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   SFDI = sum(`SFDI cena celkom`),
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_SFDI = MAPE(sum(`SFDI cena celkom`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_objects_SFDI <- test_databaza_benchmark %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Benchmarkovaný_objem_SFDI = sum(ifelse(`Benchmark 2`=="SFDI", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

aggregated_objects_data_SFDI <- left_join(aggregated_objects_valid_rows_SFDI, benchmark_objem_objects_SFDI, by="Kód objektu")


###SPOLU BEZ AGREGAGACIE SFDI
aggregated_valid_rows_SFDI <- valid_rows_SFDI %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   SFDI = sum(`SFDI cena celkom`), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_SFDI = MAPE(sum(`SFDI cena celkom`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_SFDI <- test_databaza_benchmark %>%
  dplyr::summarise(Benchmarkovaný_objem_SFDI = sum(ifelse(`Benchmark 2`=="SFDI", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

SPOLU_SFDI <- c()
SPOLU_SFDI$Projekt <- "SPOLU"
SPOLU_SFDI <- merge(SPOLU_SFDI,aggregated_valid_rows_SFDI)
SPOLU_SFDI <- merge(SPOLU_SFDI,benchmark_objem_SFDI)


aggregated_data_SFDI[length(aggregated_data_SFDI$Projekt)+1,] <- SPOLU_SFDI

write_xlsx(aggregated_data_SFDI,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_data_SFDI.xlsx")

SPOLU_objects_SFDI <- c()
SPOLU_objects_SFDI$Projekt <- "SPOLU"
SPOLU_objects_SFDI <- merge(SPOLU_objects_SFDI,aggregated_valid_rows_SFDI)
SPOLU_objects_SFDI <- merge(SPOLU_objects_SFDI,benchmark_objem_SFDI)


aggregated_objects_data_SFDI[length(aggregated_objects_data_SFDI$`Kód objektu`)+1,] <- SPOLU_objects_SFDI

write_xlsx(aggregated_objects_data_SFDI,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_objects_data_SFDI.xlsx")


# Výber riadkov s platnými hodnotami pre zmluvnú cenu, PHZ a DB
valid_rows_DB <- test_databaza_benchmark %>%
  filter(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ" & benchmarkujeme=="TRUE")

# Agregácia pod¾a projektu
aggregated_valid_rows_DB <- valid_rows_DB %>%
  group_by(Projekt) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   DB = sum(benchmark_percentil_spolu), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_DB = MAPE(sum(benchmark_percentil_spolu),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_DB <- test_databaza_benchmark %>%
  group_by(Projekt) %>%
  dplyr::summarise(Benchmarkovaný_objem_DB = sum(ifelse(benchmarkujeme=="TRUE", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()



aggregated_data_DB <- left_join(aggregated_valid_rows_DB, benchmark_objem_DB, by="Projekt")

sum(abs(aggregated_data_DB$Zmluvná_cena-aggregated_data_DB$DB)*aggregated_data_DB$Benchmarkovaný_objem_DB)/sum(aggregated_data_DB$Zmluvná_cena*aggregated_data_DB$Benchmarkovaný_objem_DB)*100

# Agregácia pod¾a objektu
aggregated_objects_valid_rows_DB <- valid_rows_DB %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   DB = sum(benchmark_percentil_spolu), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_DB = MAPE(sum(benchmark_percentil_spolu),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_object_DB <- test_databaza_benchmark %>%
  group_by(`Kód objektu`) %>%
  dplyr::summarise(Benchmarkovaný_objem_DB = sum(ifelse(benchmarkujeme=="TRUE", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()



aggregated_objects_data_DB <- left_join(aggregated_objects_valid_rows_DB, benchmark_objem_object_DB, by="Kód objektu")


###SPOLU BEZ AGREGAGACIE DB
aggregated_valid_rows_DB <- valid_rows_DB %>%
  dplyr::summarise(Zmluvná_cena = sum(`Indexovaná zmluvná cena bez DPH_v2`),
                   PHZ = sum(`Indexovaná teoretická celková cena bez DPH`),
                   DB = sum(benchmark_percentil_spolu), 
                   MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                   MAPE_DB = MAPE(sum(benchmark_percentil_spolu),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
  ungroup()


benchmark_objem_DB <- test_databaza_benchmark %>%
  dplyr::summarise(Benchmarkovaný_objem_DB = sum(ifelse(benchmarkujeme=="TRUE", `Indexovaná zmluvná cena bez DPH_v2`, 0)) / sum(`Indexovaná zmluvná cena bez DPH_v2`) * 100) %>%
  ungroup()

SPOLU_DB <- c()
SPOLU_DB$Projekt <- "SPOLU"
SPOLU_DB <- merge(SPOLU_DB,aggregated_valid_rows_DB)
SPOLU_DB <- merge(SPOLU_DB,benchmark_objem_DB)


aggregated_data_DB[length(aggregated_data_DB$Projekt)+1,] <- SPOLU_DB

write_xlsx(aggregated_data_DB,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_data_DB.xlsx")

SPOLU_objects_DB <- c()
SPOLU_objects_DB$Projekt <- "SPOLU"
SPOLU_objects_DB <- merge(SPOLU_objects_DB,aggregated_valid_rows_DB)
SPOLU_objects_DB <- merge(SPOLU_objects_DB,benchmark_objem_DB)


aggregated_objects_data_DB[length(aggregated_objects_data_DB$`Kód objektu`)+1,] <- SPOLU_objects_DB

write_xlsx(aggregated_objects_data_DB,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/aggregated_objects_data_DB.xlsx")


#############################MALE PROJEKTY########################################################################################################################
BAtunel <- read_excel("C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Bratislava_tunel_benchmark_format.xlsx")
Krompachy <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/ZST Krompachy_benchmark_format.xlsx")
Palarikovo <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Palarikovo_benchmark_format.xlsx")
Dvory <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Dvory_benchmark_format.xlsx")
Barca <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Barca_benchmark_format.xlsx")
Sastin_Kuty <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Sastin_Kuty_benchmark_format.xlsx")
Trnovec <- read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Trnovec_benchmark_format.xlsx")
Levice <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Levice_benchmark_format.xlsx")
Filakovo <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/ZST Filakovo_benchmark_format.xlsx")
Selpice <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Selpice_benchmark_format.xlsx")
balik_projektov <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/balik_projektov.xlsx")
Boleraz <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Boleraz_benchmark_format.xlsx")
Luzianky <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Luzianky_benchmark_format.xlsx")
Sladkovicovo<-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Sladkovicovo_benchmark_format.xlsx")
Velky_Hores <-read_excel("Starek/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/Velky_Hores_benchmark_format.xlsx")
balik <- read_excel("C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/Excel R-benchmarky/Malé projekty benchmark/balik benchmarkov.xlsx")


list_benchmark <- funkcia(BAtunel, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Krompachy, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Palarikovo, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Dvory, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Barca, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Sastin_Kuty, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Trnovec, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Levice, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Filakovo, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Selpice, priemery, unikat_priemer, min)
list_benchmark <- funkcia(balik_projektov, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Boleraz, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Luzianky, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Sladkovicovo, priemery, unikat_priemer, min)
list_benchmark <- funkcia(Velky_Hores, priemery, unikat_priemer, min)
list_benchmark <- funkcia(balik, priemery, unikat_priemer, min)



benchmark <- list_benchmark$benchmark
sumarizacia <- list_benchmark$sumarizacia
sumarizacia_spolu <- list_benchmark$sumarizacia_spolu

write_xlsx(benchmark,"Starek/zeleznice/Benchmarking/R benchmark/VelkyHoresBenchmark.xlsx")
write_xlsx(sumarizacia_spolu,"Starek/zeleznice/Benchmarking/R benchmark/VelkyHoresSumarizacia.xlsx")

######Kumulatívna cena pod¾a velkosti zoradených objektov


zmluvna_cena <- sum(data$`Indexovaná cena bez DPH_v2`)
poc <- vector()
objekty <- aggregate(data$`Indexovaná cena bez DPH_v2`, by=list(nazov_objektu=data$`Èas stavby - èíslo`), FUN=sum)
objekty <- sort(objekty$x, decreasing = FALSE)
cc <- cumsum(objekty)
for (i in 1:length(objekty))
{
  if (i==1)
  {
    par(new = FALSE)
    poc[i] <- objekty[i]
    plot(cc[i],cc[i]/zmluvna_cena, xlim=c(0,max(cc)),ylim = c(0,1))
    
  }
  else {
    poc[i] <- objekty[i] + poc[i-1]
    points(cc[i],cc[i]/zmluvna_cena)
    
  }
}

####Inicializacia funkcii pre RSS a TSS
rsme <- function(x,y)
{
  custom_rsme <- vector()
  custom_rsme <- sqrt(mean((x - y)^2))
  return(custom_rsme)
}
tss <- function(x)
{
  tsss <- vector()
  tss_mean <- mean(x)
  for (i in 1:length(x))
  {
    tsss[i] <- (x[i] - tss_mean)^2
  }
  return(sum(tsss))
}




#########
vizualizacia <- function(data,balik)
{
  ########Detekovanie outlierov a ich vylúèenie z dát###############################################################################################################
  ########PRVY KROK TUKEY 3.0 DRUHY KROK ADJBOX
  df_outliers <- data %>% 
    group_by(`Èíslo položky_m. j.`,Projekt) %>% 
    identify_outliers("Jednotková cena") 
  
  df_extremeoutliers <- filter(df_outliers, is.extreme == "TRUE")
  
  cleandata <- data %>% 
    anti_join(df_extremeoutliers, by = "ID") 
  
  cleandata_s_outliermi <- data %>% 
    anti_join(df_extremeoutliers, by = "ID") 
  
  data_adjoutlier_complete <-  cleandata_s_outliermi %>% 
    group_by(`Èíslo položky_m. j.`) %>% 
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    filter(`Indexovaná jednotková cena_v2` %in%  adjboxStats(`Indexovaná jednotková cena_v2`, coef = 1.5,
                                                             a = -4, b = 3, do.conf = TRUE, do.out = TRUE)$out)
  
  
  cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
  data_adjoutlier_complete$spojenie <- str_c(data_adjoutlier_complete$`Indexovaná jednotková cena_v2`, data_adjoutlier_complete$`Èíslo položky_m. j.`)
  
  cleancleandata <- cleandata_s_outliermi %>%
    anti_join(data_adjoutlier_complete, by = "spojenie")
  
  data <- cleancleandata
  
  ############RELATIVNA VOLATILITA PRE V2###############################################
  relativne_sd <- data %>% 
    group_by(`Èíslo položky_m. j.`) %>%
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)/mean(`Indexovaná jednotková cena_v2`)) %>%
    na.omit()
  
  relativne_sd_desc <- relativne_sd$`Èíslo položky_m. j.`[relativne_sd$`sd(\`Indexovaná jednotková cena_v2\`) / ...`>sqrt(12)]
  ############ABSOLUTNA VOLATILITA PRE V2################################################
  sd <- data %>%
    group_by(`Èíslo položky_m. j.`) %>%
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)) %>%
    na.omit()
  
  sd_desc <- sd$`Èíslo položky_m. j.`[sd$`sd(\`Indexovaná jednotková cena_v2\`)`>25000]
  #Percentil
  percentil <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`,projekt=data$Projekt), FUN=mean)
  percentil <- aggregate(percentil$x, by=list(cislo_a_mj=percentil$cislo_a_mj), FUN='quantile',probs=38/100)
  #########################
  list_benchmark <- funkcia(balik, percentil, min,relativne_sd_desc,sd_desc)
  test_databaza_benchmark <- list_benchmark$benchmark
  
  valid_rows_DB <- test_databaza_benchmark %>%
    filter(`Indexovaná zmluvná cena bez DPH_v2` > 0 & `Benchmark 0`=="PHZ")
  
  aggregated_valid_rows_DB <- valid_rows_DB %>%
    dplyr::summarise(MAE_DB = MAE(sum(benchmark_percentil_spolu),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                     MAE_PHZ = MAE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                     MAPE_DB = MAPE(sum(benchmark_percentil_spolu),sum(`Indexovaná zmluvná cena bez DPH_v2`)),
                     MAPE_PHZ = MAPE(sum(`Indexovaná teoretická celková cena bez DPH`),sum(`Indexovaná zmluvná cena bez DPH_v2`))) %>%
    ungroup()
  
  
  
  vec <- aggregated_valid_rows_DB
  return(vec)
}

#####TESTOVANIE OUTLIEROV NA TESTOVACEJ DATABAZE#######################

####BEZ FILTROVANIA OUTLIEROV
vysledky_s_outliermi <- vizualizacia(data,balik) 
spolu_s_outliermi <-vysledky_s_outliermi$rsm

####S FILTROVANIM EXTREMNYCH OUTLIEROV (3.0)
vysledky_bez_extremnych_outlierov <- vizualizacia(data,balik) 
spolu_bez_extremnych_outlierov <-vysledky_bez_extremnych_outlierov$rsm

table_of_rss_bez_extremnych_outlierov_1 <- as.data.frame(as.vector(table_of_rss_bez_exremnych_outlierov))

table_of_rss_bez_exremnych_outlierov_1 <- rbind(table_of_rss_bez_extremnych_outlierov_1,spolu)
colnames(table_of_rss_bez_extremnych_outlierov_1) <- c('Teoretické ocenenie','Priemer', 'opt_priemer', 'unikat','opt_unikat')


####S FILTROVANIM OUTLIEROV (1.5)
vysledky_bez_outlierov <- vizualizacia(data,balik) 
spolu_bez_outlierov <-vysledky_bez_outlierov$rsm


####Pokus o automatizaciu
projekty <- unique(data$Projekt)
total_projects <- length(projekty)
batch_size <- 3
max_batches <- ceiling(total_projects / batch_size)
pocet_databaza <- seq(0, max_batches * batch_size, by = batch_size)
results <- data.frame(iteration = integer(), projects = integer(), MAE = numeric(), MAPE = numeric())

if (total_projects %% batch_size != 0) {
  pocet_databaza <- append(pocet_databaza[-length(pocet_databaza)], total_projects)
}
u <- sample(projekty, total_projects)
all_y_priemer <- list()

for (j in 1:10) {
  if (j ==1)
  {
    start_time <- Sys.time()
  }
  u <- sample(projekty, total_projects)
  table_of_rss <- data.frame()
  
  i <- 0
  k <- 0
  while (k <= 1){
    if (i == 0) {
      # Handle empty database case
      spolu <- vysledky$MAE_PHZ
      spolu_mape <- 0.08979554
      } else {
    
      projects_to_include <- u[1:min(i, total_projects)]    
      data_not <- filter(data, data$Projekt %in% projects_to_include)
    
    vysledky <- vizualizacia(data_not, test_data)
    spolu <- vysledky$MAE_DB
    spolu_mape <- vysledky$MAPE_DB
      }
    results <- rbind(results, data.frame(iteration = j, projects = min(i, total_projects), MAE = spolu, MAPE = spolu_mape))
    
    table_of_rss <- rbind(table_of_rss, spolu)
    colnames(table_of_rss) <- c('percentil')
    # Increment i
    if (i + batch_size > total_projects) {
      i <- total_projects
      k <- k+1
    } else {
      i <- i + batch_size
    }
  }
  
  y_priemer <- c(table_of_rss$percentil)
  all_y_priemer[[j]] <- y_priemer
  if (j==5)
  {
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
  }
}

#results <- data.frame(iteration = rep(1:length(all_y_priemer), each = length(pocet_databaza)),
 #                     projects = rep(pocet_databaza, times = length(all_y_priemer)),
  #                    MAE = unlist(lapply(all_y_priemer, function(x) {c(y_priemer_test[1], x[-1])})))

ggplot(results, aes(x = projects, y = MAE, group = iteration, color = factor(iteration))) +
  geom_line() +
  labs(title = "MAE v závislosti od poètu projektov v databáze",
       x = "# projektov v databaze",
       y = "MAE",
       color = "Iterácia") +
  theme_minimal()

# Calculate the mean RMSE for each number of projects
mean_MAE <- aggregate(MAE ~ projects, data = results, FUN = mean)
mean_MAPE <- aggregate(MAPE ~ projects, data = results, FUN = mean)
# Plot the graph
ggplot(results, aes(x = projects, y = MAPE, group = iteration, color = factor(iteration))) +
  geom_line() +
  geom_line(data = mean_MAPE, aes(x = projects, y = MAPE, group = 1), color = "black", size = 1.5, linetype = "dashed") +
  labs(title = "RMSE v závislosti od poètu projektov v databáze",
       x = "# projektov v databaze",
       y = "RMSE",
       color = "Iterácia") +
  theme_minimal()

# Pridanie lineárneho regresného modelu
lm_model <- lm(MAE ~ projects, data = results)
summary(lm_model)

# Pridanie regresnej èiary do grafu
ggplot(results, aes(x = projects, y = MAE, group = iteration, color = factor(iteration))) +
  geom_line() +
  geom_smooth(data = mean_MAE, aes(x = projects, y = MAE, group = NULL), method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1.5) +
  labs(title = "MAE v závislosti od poètu projektov v databáze",
       x = "# projektov v databaze",
       y = "MAE",
       color = "Iterácia") +
  theme_minimal()

poly_fit <- lm(MAE ~ poly(projects, 2), data = results)
summary(poly_fit)

# Polynomiálna regresia (stupeò 2)
ggplot(results, aes(x = projects, y = MAE, group = iteration, color = factor(iteration))) +
  geom_line() +
  geom_smooth(data = results, aes(x = projects, y = MAE, group = NULL), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed", size = 1.5) +
  labs(title = "MAE v závislosti od poètu projektov v databáze",
       x = "# projektov v databaze",
       y = "MAE",
       color = "Iterácia") +
  theme_minimal()

# Model s interakciami
interaction_model <- lm(MAE ~ projects * I(projects^2), data = results)
summary(interaction_model)

# Kubická polynomiálna regresia
cubic_model <- lm(MAE ~ poly(projects, 3, raw = TRUE), data = results)
summary(cubic_model)

# GAM
gam_model <- gam(MAE ~ s(projects, k = 4), data = mean_MAE)
summary(gam_model)

# Spline regresia
spline_model <- lm(MAE ~ bs(projects, df = 4), data = results)
summary(spline_model)

# Predikcia
predicted_spline <- predict(spline_model, data.frame(projects = results$projects))

ggplot(results, aes(x = projects, y = MAE)) +
  geom_point() +
  geom_line(aes(y = predicted_spline), color = "blue", linetype = "solid") +
  labs(title = "Spline regresia",
       x = "# projektov v databáze",
       y = "MAE") +
  theme_minimal() +
  theme(legend.position = "none")

###H¾adanie najlepšieho percentilu?


ceny <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`), FUN=sum)

ceny4 <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`), FUN='quantile',probs=0.3)
ukladacka_jednokrok <-c()
rsm_jednokrok <-c()
ukladacka_dvojkrok <-c()
rsm_dvojkrok <-c()

for (j in 1:100)
{
  priemery <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`), FUN='quantile',probs=j/100)
  
  unikat <- aggregate(data$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=data$`Èíslo položky_m. j.`,projekt=data$Projekt), FUN='quantile',probs=j/100)
  unikat_priemer <- aggregate(unikat$x, by=list(cislo_a_mj=unikat$cislo_a_mj), FUN='quantile',probs=j/100)
  
  list_benchmark <- funkcia(balik, priemery, unikat_priemer, min)
  benchmark <- list_benchmark$benchmark
  sumarizacia <- list_benchmark$sumarizacia
  sumarizacia_spolu <- list_benchmark$sumarizacia_spolu
  sumarizacia_spolu$Priemer <- sumarizacia_spolu$`Priemer` / sumarizacia_spolu$`Zmluvná cena`
  sumarizacia_spolu$unikat <- sumarizacia_spolu$unikat / sumarizacia_spolu$`Zmluvná cena`
  
  ukladacka_jednokrok[j] <- sumarizacia_spolu$Priemer[length(sumarizacia_spolu$`Priemer`)]
  rsm_jednokrok[j] <- rsme(sumarizacia$`Zmluvná cena`,sumarizacia$Priemer)
  
  ukladacka_dvojkrok[j] <= sumarizacia_spolu$unikat[length(sumarizacia_spolu$unikat)]
  rsm_dvojkrok[j] <- rsme(sumarizacia$`Zmluvná cena`,sumarizacia$unikat)
  
  
}
j <- 1:100
plot(j,ukladacka_jednokrok,xlab = 'Použitý percentil',ylab = 'Pomer odhadovanej a zmluvnej ceny jednokrok')
plot(j,rsm_jednokrok,xlab = 'Použitý percentil',ylab = 'rsme jednokrok')

plot(j,ukladacka_dvojkrok,xlab = 'Použitý percentil',ylab = 'Pomer odhadovanej a zmluvnej ceny dvojkrok')
plot(j,rsm_dvojkrok,xlab = 'Použitý percentil',ylab = 'rsme dvojkrok')

rsm

j=25

####KRIŽOVÁ VALIDÁCIA
table_of_mape <- vector()
table_of_mape <- as.data.frame(table_of_mape)

priemerrr <- vector()
vychylka <- vector()



for (g in 1:10)
{
  # creating training data as 85% of the dataset
  tim=as.numeric(Sys.time())
  set.seed(tim)
  random_sample <- createDataPartition(data$ID,
                                       p = 0.85, list = FALSE)
  
  # generating training dataset
  # from the random_sample
  training_dataset  <- data[random_sample, ]
  
  ########Detekovanie outlierov a ich vylúèenie z dát###############################################################################################################
  ########PRVY KROK TUKEY 3.0 DRUHY KROK ADJBOX
  df_outliers <- training_dataset %>% 
    group_by(`Èíslo položky_m. j.`,Projekt) %>% 
    identify_outliers("Jednotková cena") 
  
  df_extremeoutliers <- filter(df_outliers, is.extreme == "TRUE")
  
  cleandata <- training_dataset %>% 
    anti_join(df_extremeoutliers, by = "ID") 
  
  cleandata_s_outliermi <- training_dataset %>% 
    anti_join(df_extremeoutliers, by = "ID") 
###################DRUHY KROK
  data_adjoutlier_complete <-  cleandata_s_outliermi %>% 
    group_by(`Èíslo položky_m. j.`) %>% 
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    filter(`Indexovaná jednotková cena_v2` %in%  adjboxStats(`Indexovaná jednotková cena_v2`, coef = 1.5,
                                                             a = -4, b = 3, do.conf = TRUE, do.out = TRUE)$out)
  
  
  cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
  data_adjoutlier_complete$spojenie <- str_c(data_adjoutlier_complete$`Indexovaná jednotková cena_v2`, data_adjoutlier_complete$`Èíslo položky_m. j.`)
  
  cleancleandata <- cleandata_s_outliermi %>%
    anti_join(data_adjoutlier_complete, by = "spojenie")
  
  training_dataset <- cleancleandata
  
  ############RELATIVNA VOLATILITA PRE V2###############################################
  relativne_sd <- training_dataset %>% 
    group_by(`Èíslo položky_m. j.`) %>%
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)/mean(`Indexovaná jednotková cena_v2`)) %>%
    na.omit()
  
  relativne_sd_desc <- relativne_sd$`Èíslo položky_m. j.`[relativne_sd$`sd(\`Indexovaná jednotková cena_v2\`) / ...`>sqrt(12)]
  ############ABSOLUTNA VOLATILITA PRE V2################################################
  sd <- training_dataset %>%
    group_by(`Èíslo položky_m. j.`) %>%
    distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
    dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)) %>%
    na.omit()
  
  sd_desc <- sd$`Èíslo položky_m. j.`[sd$`sd(\`Indexovaná jednotková cena_v2\`)`>25000]

###########Vypocet priemerov poloziek###########
  ceny <- aggregate(cbind(training_dataset$`Indexovaná cena bez DPH_v2`,training_dataset$Množstvo), by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`), FUN=sum)
  
  ceny$V1 <- ceny$V1/ceny$V2
  
  priemery <- aggregate(ceny$V1, by=list(cislo_a_mj=ceny$cislo_a_mj),FUN=mean)

###########Unikátne hodnoty jednotkových cien pre každý projekt+min/max statistics with standard deviation###########
unikat <- aggregate(training_dataset$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`,projekt=training_dataset$Projekt), FUN=mean)
unikat_priemer <- aggregate(unikat$x, by=list(cislo_a_mj=unikat$cislo_a_mj), FUN=mean)

#Najoptimalnejší percentil 
percentil <- aggregate(training_dataset$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`,projekt=training_dataset$Projekt), FUN=mean)
percentil <- aggregate(percentil$x, by=list(cislo_a_mj=percentil$cislo_a_mj), FUN='quantile',probs=38/100)

#Median
median <- aggregate(training_dataset$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`,projekt=training_dataset$Projekt), FUN='quantile',probs=50/100)
median <- aggregate(median$x, by=list(cislo_a_mj=median$cislo_a_mj), FUN='quantile',probs=50/100)

# generating testing dataset
# from rows which are not
# included in random_sample
testing_dataset_raw <- data[-random_sample, ]
testing_dataset <- as.data.table(c())
#Upravovanie testovacieho datasetu do požadovaného formátu
testing_dataset$`Èas stavby` <- testing_dataset_raw$`Èas stavby - èíslo, názov`
testing_dataset$`cislo polozky` <- testing_dataset_raw$`Èíslo položky`
testing_dataset$`Názov položky` <- testing_dataset_raw$`Názov položky`
testing_dataset$`M.j.` <- testing_dataset_raw$`Merná jednotka`
testing_dataset$`Množstvo` <- testing_dataset_raw$Množstvo
testing_dataset$`Jednotková cena` <- testing_dataset_raw$`Indexovaná jednotková cena_v2`
testing_dataset$`Cena celkom` <- testing_dataset_raw$`Indexovaná cena bez DPH_v2`
testing_dataset$`jednotkova teoreticka cena` <- replicate(length(testing_dataset$`cislo polozky`), 0)
testing_dataset$`Teoretické ocenenie` <- replicate(length(testing_dataset$`cislo polozky`), 0)
testing_dataset$`cislo_a_mj` <- testing_dataset_raw$`Èíslo položky_m. j.`

# vysledky <- vizualizacia(training_dataset,testing_dataset)
# spolu <-vysledky$sumarizacia_spolu

list_benchmark <- funkcia(testing_dataset,priemery,unikat_priemer,percentil, median, min)  
tabulky <- list_benchmark$benchmark
sumarizacia <- list_benchmark$sumarizacia

rsmes <- vector()
percentils <- vector()
rsmes[1] <- MAPE(sum(sumarizacia$`Priemer`),sum(sumarizacia$`Zmluvná cena`))
rsmes[2] <- MAPE(sum(sumarizacia$unikat), sum(sumarizacia$`Zmluvná cena`))
rsmes[3] <- MAPE(sum(sumarizacia$median),sum(sumarizacia$`Zmluvná cena`))
rsmes[4] <- MAPE(sum(sumarizacia$percentil),sum(sumarizacia$`Zmluvná cena`))
table_of_mape <- rbind(table_of_mape,rsmes)
colnames(table_of_mape) <- c('Priemer','unikat', 'median', 'percentil')

}


write_xlsx(table_of_mape,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/table_of_mape_nazov_bez_outlierov.xlsx")
write_xlsx(table_of_rsme_MD19_bez_outlierov,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/table_of_rsme_MD19_bez_outlierov.xlsx")
write_xlsx(table_of_rsme_v1,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/table_of_rsme_v1_bez_outlierov.xlsx")
write_xlsx(table_of_rsme_nazov_bez_outlierov_adj_boxplot,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/table_of_rsme_nazov_bez_outlierov_adj_boxplot.xlsx")


write_xlsx(table_of_mape,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/table_of_mape.xlsx")

write_xlsx(test3,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/priemerne_mape_percentil_30_60.xlsx")

write_xlsx(test4,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/priemerna_vychylka_mape_percentil_30_60.xlsx")

test3 <- as.data.frame(priemerrr)

test4 <- as.data.frame(vychylka)

table_of_rsme_nazov_bez_outlierov <- table_of_rsme 
table_of_rsme_MD19_bez_outlierov <- table_of_rsme
table_of_rsme_v1_bez_outlierov <- table_of_rsme
table_of_rsme_nazov_bez_outlierov_adj_boxplot <- table_of_rsme


najobj_polozky <- aggregate(data$`Indexovaná cena bez DPH_v2`, by=list(kod_polozky_m.j.=data$`Èíslo položky_m. j.`), FUN=sum)

write_xlsx(najobj_polozky,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/Benchmarking/najobj_polozky.xlsx")


# Normalizácia jednotkových cien pod¾a ID položky a filtrovanie položiek s viac ako 2 výskyty
data_histogram <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky`) %>%
  mutate(normalizovana_cena = (`Indexovaná jednotková cena_v2` - min(`Indexovaná jednotková cena_v2`)) / (max(`Indexovaná jednotková cena_v2`) - min(`Indexovaná jednotková cena_v2`)),
         pocet_vyskytov = n()) %>%
  ungroup() %>%
  filter(pocet_vyskytov > 2)

data_histogram <- filter(data_histogram, `normalizovana_cena` != 0)
data_histogram <- filter(data_histogram, `normalizovana_cena` != 1)

# Vytvorenie histogramu pomocou ggplot2
histogram <- ggplot(data_histogram, aes(x = normalizovana_cena)) +
  geom_histogram(binwidth = 0.01, # Nastavenie ve¾kosti binov; môžete upravi pod¾a potreby
                 fill = "blue",
                 color = "black") +
  labs(title = "Histogram normalizovaných jednotkových cien pod¾a ID položky (viac ako 2 výskyty)",
       x = "Normalizovaná jednotková cena",
       y = "Poèet položiek") +
  theme_minimal()

# Zobrazenie histogramu
print(histogram)


# Výpoèet koeficientu vychýlenosti pre každú položku
data_skewness <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
  dplyr::summarise(vychylenost = skewness(`Indexovaná jednotková cena_v2`),pocet_vyskytov = n()) %>%
  ungroup() %>%
  filter(pocet_vyskytov > 1)

sum(data_skewness$vychylenost)

skewness_summary <- data_skewness %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarise(miera_porovnania = case_when(
    vychylenost <  -1*10^-7 ~ "negatívna",
    (vychylenost <= 1*10^-7) && (vychylenost >= -1*10^-7) ~ "symetrická",
    vychylenost > 1*10^-7 ~ "pozitívna"
  ), vychylenost=vychylenost)

skewness_porovnanie <- skewness_summary %>%
  group_by(miera_porovnania) %>%
  dplyr::summarise(pocet_položiek = n_distinct(`Èíslo položky_m. j.`))



skewness_porovnanie <- skewness_summary %>%
  group_by(miera_porovnania) %>%
  dplyr::summarise(pocet_položiek = n_distinct(`Èíslo položky_m. j.`))

write_xlsx(skewness_porovnanie,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/skewness_porovnanie.xlsx")

###ANALYZA OBJEMNEJ ZLAVY
databaza_filtrovana <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  filter(n_distinct(Projekt) > 1) %>%
  ungroup()

databaza_porovnana <- databaza_filtrovana %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarise(
    nevazeny_priemer = mean(`Indexovaná jednotková cena_v2`),
    vazeny_priemer = sum(`Indexovaná jednotková cena_v2` * Množstvo) / sum(Množstvo),
    rozdiel = vazeny_priemer - nevazeny_priemer,
    miera_porovnania = case_when(
      rozdiel < -1*10^-7 ~ "nižšia",
      (rozdiel <= 1*10^-7) && (rozdiel >= -1*10^-7) ~ "rovnaká",
      rozdiel > 1*10^-7 ~ "vyššia"
    )
  ) %>%
  ungroup()

pocty_porovnani <- databaza_porovnana %>%
  group_by(miera_porovnania) %>%
  dplyr::summarise(pocet_položiek = n_distinct(`Èíslo položky_m. j.`))

write_xlsx(pocty_porovnani,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/objemna_zlava.xlsx")

###########
unique_prices_data <- cleandata_s_outliermi %>%
  distinct(Projekt,`Èíslo položky`, `Indexovaná jednotková cena_v2` )

item_counts <- as.data.frame(table(unique_prices_data$`Èíslo položky`)) %>%
   filter(Freq > 0)
colnames(item_counts) <- c("Èíslo položky", "poèet")

ggplot(item_counts, aes(x = poèet)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(x = "Poèet unikátnych výskytov položiek s unikátnymi cenami", y = "Poèet položiek") +
  theme_minimal()

####Vytvorenie ståpového grafu pre potreby analýzy objemnej z¾avy

#Vytvorenie nového data.frame-u, ktorý obsahuje poèet jedineèných projektov pre každú položku:

unique_projects <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarize(unique_projects = n_distinct(Projekt)) %>%
  arrange(desc(unique_projects))

#Pridanie poètu jedineèných projektov do zoskupenej štruktúry súhrnu:
unique_prices_per_project <- cleandata_s_outliermi %>%
  distinct(`Èíslo položky_m. j.`, Projekt, `Indexovaná jednotková cena_v2`)


df_summary <- cleandata_s_outliermi %>%
  group_by(`Èíslo položky_m. j.`) %>%
  dplyr::summarize(total_quantity = sum(Množstvo),
            total_cost = sum(Množstvo * `Indexovaná jednotková cena_v2`),
            weighted_avg_price = total_cost / total_quantity,
            unique_projects = n_distinct(Projekt)) %>%
  left_join(unique_prices_per_project %>%
              group_by(`Èíslo položky_m. j.`) %>%
              dplyr::summarize(unweighted_avg_price = mean(`Indexovaná jednotková cena_v2`)),
            by = "Èíslo položky_m. j.") %>%
  arrange(desc(unique_projects)) %>%
  filter(unique_projects > 1) 

#Rozdelenie položiek do 10-percentných skupín pod¾a poètu jedineèných projektov:

group_size <- ceiling(nrow(df_summary) * 0.1)
df_summary$group <- rep(1:ceiling(nrow(df_summary) / group_size), each = group_size, length.out = nrow(df_summary))

#Pre každú skupinu vypoèítame percento položiek s nižším váženým priemerom ako nevážený priemer:

group_summary <- df_summary %>%
  group_by(group) %>%
  dplyr::summarize(percent_lower = sum(weighted_avg_price < unweighted_avg_price) / n() * 100)

#Vytvorte ståpcový graf s výsledkami:
ggplot(group_summary, aes(x = as.numeric(group), y = percent_lower)) +
  geom_col(fill = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") + # Pridanie èiary trendu
  labs(x = "Decil položiek pod¾a výskytu v poète projektov",
       y = "Percento položiek s nižším váženým priemerom ako aritmetický priemer") +
  theme_minimal() +
scale_x_continuous(breaks = 1:max(group_summary$group), labels = 1:max(group_summary$group)) # Zmena èíslovania x-ovej osy
####H¾adanie najoptimalnejšieho percentilu pomocou krížovej validácie
table_of_mape <- vector()
table_of_mape <- as.data.frame(table_of_mape)

priemer_percentil1 <- vector()
priemer_percentil2 <- vector()
x1ky <- vector()
x2ky <- vector()
priemerny_percentil1 <- vector()
priemerny_percentil2 <- vector()
vychylka_percentil1 <- vector()
vychylka_percentil2 <- vector()
vychylky_percentil1 <- vector()
vychylky_percentil2 <- vector()


a <- 25
b <- 50
tol <- 0.5
gr <- (sqrt(5) - 1) / 2
x1 <- a + (1 - gr) * (b - a)
x2 <- a + gr * (b - a)

poc <- 0

while (abs(b - a) > tol) 
{
  table_of_percentil <- vector()
  table_of_percentil <- as.data.frame(table_of_percentil)
  poc <- poc + 1
  x1ky[poc] <- x1
  x2ky[poc] <- x2
  
  for (g in 1:10)
  {
    # creating training data as 85% of the dataset
    tim=as.numeric(Sys.time())
    set.seed(tim)
    random_sample <- createDataPartition(data$ID,
                                         p = 0.85, list = FALSE)
    
    # generating training dataset
    # from the random_sample
    training_dataset  <- data[random_sample, ]
    
    ########Detekovanie outlierov a ich vylúèenie z dát###############################################################################################################
    ########PRVY KROK TUKEY 3.0 DRUHY KROK ADJBOX
    df_outliers <- training_dataset %>% 
      group_by(`Èíslo položky_m. j.`,Projekt) %>% 
      identify_outliers("Jednotková cena") 
    
    df_extremeoutliers <- filter(df_outliers, is.extreme == "TRUE")
    
    cleandata <- training_dataset %>% 
      anti_join(df_extremeoutliers, by = "ID") 
    
    cleandata_s_outliermi <- training_dataset %>% 
      anti_join(df_extremeoutliers, by = "ID") 
    
    data_adjoutlier_complete <-  cleandata_s_outliermi %>% 
      group_by(`Èíslo položky_m. j.`) %>% 
      distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
      filter(`Indexovaná jednotková cena_v2` %in%  adjboxStats(`Indexovaná jednotková cena_v2`, coef = 1.5,
                                                               a = -4, b = 3, do.conf = TRUE, do.out = TRUE)$out)
    
    
    cleandata_s_outliermi$spojenie <- str_c(cleandata_s_outliermi$`Indexovaná jednotková cena_v2`, cleandata_s_outliermi$`Èíslo položky_m. j.`)
    data_adjoutlier_complete$spojenie <- str_c(data_adjoutlier_complete$`Indexovaná jednotková cena_v2`, data_adjoutlier_complete$`Èíslo položky_m. j.`)
    
    cleancleandata <- cleandata_s_outliermi %>%
      anti_join(data_adjoutlier_complete, by = "spojenie")
    
    training_dataset <- cleancleandata
    
############RELATIVNA VOLATILITA PRE V2###############################################
    relativne_sd <- training_dataset %>% 
      group_by(`Èíslo položky_m. j.`) %>%
      distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
      dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)/mean(`Indexovaná jednotková cena_v2`)) %>%
      na.omit()
    
    relativne_sd_desc <- relativne_sd$`Èíslo položky_m. j.`[relativne_sd$`sd(\`Indexovaná jednotková cena_v2\`) / ...`>sqrt(12)]
############ABSOLUTNA VOLATILITA PRE V2################################################
    sd <- training_dataset %>%
      group_by(`Èíslo položky_m. j.`) %>%
      distinct(Projekt,`Indexovaná jednotková cena_v2`) %>%
      dplyr::summarise(sd(`Indexovaná jednotková cena_v2`)) %>%
      na.omit()
    
    sd_desc <- sd$`Èíslo položky_m. j.`[sd$`sd(\`Indexovaná jednotková cena_v2\`)`>25000]
    #Percentil1
    percentil1 <- aggregate(training_dataset$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`,projekt=training_dataset$Projekt), FUN=mean)
    percentil1 <- aggregate(percentil1$x, by=list(cislo_a_mj=percentil1$cislo_a_mj), FUN='quantile',probs=x1/100)
    
    #Percentil2
    percentil2 <- aggregate(training_dataset$`Indexovaná jednotková cena_v2`, by=list(cislo_a_mj=training_dataset$`Èíslo položky_m. j.`,projekt=training_dataset$Projekt), FUN=mean)
    percentil2 <- aggregate(percentil2$x, by=list(cislo_a_mj=percentil2$cislo_a_mj), FUN='quantile',probs=x2/100)
    
    # generating testing dataset
    # from rows which are not
    # included in random_sample
    testing_dataset_raw <- data[-random_sample, ]
    testing_dataset <- as.data.table(c())
    #Upravovanie testovacieho datasetu do požadovaného formátu
    testing_dataset$`Èas stavby` <- testing_dataset_raw$`Èas stavby - èíslo, názov`
    testing_dataset$`cislo polozky` <- testing_dataset_raw$`Èíslo položky`
    testing_dataset$`Názov položky` <- testing_dataset_raw$`Názov položky`
    testing_dataset$`M.j.` <- testing_dataset_raw$`Merná jednotka`
    testing_dataset$`Množstvo` <- testing_dataset_raw$Množstvo
    testing_dataset$`Jednotková cena` <- testing_dataset_raw$`Indexovaná jednotková cena_v2`
    testing_dataset$`Cena celkom` <- testing_dataset_raw$`Indexovaná cena bez DPH_v2`
    testing_dataset$`cislo_a_mj` <- testing_dataset_raw$`Èíslo položky_m. j.`
    
    # vysledky <- vizualizacia(training_dataset,testing_dataset)
    # spolu <-vysledky$sumarizacia_spolu
    
    list_benchmark <- funkcia2(testing_dataset,percentil1,percentil2,relativne_sd_desc, sd_desc)  
    tabulky <- list_benchmark$benchmark
    sumarizacia <- list_benchmark$sumarizacia
    
    percentils <- vector()
    percentils[1] <- MAPE(sum(sumarizacia$percentil1),sum(sumarizacia$`Zmluvná cena`))
    percentils[2] <- MAPE(sum(sumarizacia$percentil2),sum(sumarizacia$`Zmluvná cena`))
    table_of_percentil <- rbind(table_of_percentil,percentils)
    colnames(table_of_percentil) <- c('percentil1','percentil2')
    
  }
  
  priemer_percentil1 <- mean(table_of_percentil$percentil1)
  vychylka_percentil1 <- sd(table_of_percentil$percentil1)
  priemer_percentil2 <- mean(table_of_percentil$percentil2)
  vychylka_percentil2 <- sd(table_of_percentil$percentil2)
  
  priemerny_percentil1[poc] <- priemer_percentil1
  priemerny_percentil2[poc] <- priemer_percentil2
  
  vychylky_percentil1[poc] <- vychylka_percentil1
  vychylky_percentil2[poc] <- vychylka_percentil2
  
  if (priemer_percentil1 < priemer_percentil2) {
    b <- x2
    x2 <- x1
    x1 <- a + (1 - gr) * (b - a)
  } else {
    a <- x1
    x1 <- x2
    x2 <- a + gr * (b - a)
  }
}

write_xlsx(vysledky_hladania_percentilu,"C:/Users/Tomáš/Desktop/Dokumenty/UHP/zeleznice/hladanie_percentilu_krizova_validacia_adjbox.xlsx")

vysledky_hladania_percentilu <- as.data.frame(c())
vysledky_hladania_percentilu <- rbind(x1ky,x2ky, priemerny_percentil1, priemerny_percentil2, vychylky_percentil1, vychylky_percentil2)
vysledky_hladania_percentilu <- as.data.frame(vysledky_hladania_percentilu)

benchmark <- c()
benchmark <- testing_dataset
funkcia2 <- function(benchmark, percentil1, percentil2, relativne_sd_desc, sd_desc)
{
  benchmark <- join(benchmark, min, by="cislo_a_mj", type='left', match='first')
  names(benchmark)[names(benchmark) == "x"] <- "minimum" 
  benchmark <- plyr::join(benchmark, percentil1, by="cislo_a_mj", type='left', match='first')
  names(benchmark)[names(benchmark) == "x"] <- "percentil1"
  benchmark <- plyr::join(benchmark, percentil2, by="cislo_a_mj", type='left', match='first')
  names(benchmark)[names(benchmark) == "x"] <- "percentil2"
  benchmark[is.na(benchmark)] <- 0
  ###Podmienka benchmarku
  #&& is.element(benchmark$cislo_a_mj[i],relativne_sd_desc$cislo_a_mj)==FALSE && is.element(benchmark$cislo_a_mj[i],sd_desc$cislo_a_mj)==FALSE
  benchmark$benchmarkovat <- replicate(length(benchmark$percentil1), 0)
  benchmark$lower_to_min <- replicate(length(benchmark$percentil1), 0)
  ##vieme benchmarkovat?
  for(i in 1:length({benchmark$percentil1}))
  {
    if (benchmark$benchmarkovat[i]==0 && benchmark$percentil1[i]>0 && is.element(benchmark$cislo_a_mj[i],relativne_sd_desc)==FALSE && is.element(benchmark$cislo_a_mj[i],sd_desc)==FALSE)
    {
      benchmark$benchmarkujeme[i] <- TRUE
    }
    else {
      benchmark$benchmarkujeme[i] <- FALSE
      benchmark$benchmarkovat[i] <- 1
    }
  }
  
  ####PRE KRÍŽOVU VALIDÁCIU BERIEME DO ÚVAHY IBA TO ÈO VIEME BENCHMARKOVA
  benchmark <- benchmark[benchmark$benchmarkujeme != FALSE, ]

  
  ##priemerne opt_ceny 
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$priemery[i] < benchmark$`jednotkova teoreticka cena`[i])
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$priemery[i]
  #     benchmark$lower_to_min[i] <- "FALSE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$`jednotkova teoreticka cena`[i] < benchmark$minimum[i] && benchmark$priemery[i] > benchmark$`jednotkova teoreticka cena`[i] )
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$minimum[i]
  #     #benchmark$opt_priemer[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     benchmark$lower_to_min[i] <- "TRUE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==FALSE || (benchmark$priemery[i] > benchmark$`jednotkova teoreticka cena`[i] && benchmark$`jednotkova teoreticka cena`[i] > benchmark$minimum[i]) )
  #   {
  #     benchmark$opt_priemer[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     
  #   }
  #   
  # }
  # ###celkove opt_priemerne ceny
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   benchmark$benchmark_opt_priemer_spolu[i] <- benchmark$opt_priemer[i]*benchmark$Množstvo[i]
  # }
  # ###rozdiel benchmarku podla priemeru od zmluvnej ceny položiek
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmark_opt_priemer_spolu[i]>0)
  #   {
  #     benchmark$rozdiel_opt_priemer[i] <- benchmark$benchmark_opt_priemer_spolu[i]-benchmark$`Cena celkom`[i]
  #   }
  #   else {
  #     benchmark$rozdiel_opt_priemer[i] <-0
  #   }
  # }
  ##najoptimalnejší percentil
  for(i in 1:length({benchmark$percentil1}))
  {
    if (benchmark$benchmarkujeme[i]==TRUE)
    {
      benchmark$benchmark_percentil1[i] <- benchmark$percentil1[i]
    }
    else 
    {
      benchmark$benchmark_percentil1[i] <- benchmark$`jednotkova teoreticka cena`[i]
    }
  }
  ###celkove percentilné ceny
  for(i in 1:length({benchmark$percentil1}))
  {
    benchmark$benchmark_percentil1_spolu[i] <- benchmark$benchmark_percentil1[i]*benchmark$Množstvo[i]
  }
  ##percentil2
  for(i in 1:length({benchmark$percentil2}))
  {
    if (benchmark$benchmarkujeme[i]==TRUE)
    {
      benchmark$benchmark_percentil2[i] <- benchmark$percentil2[i]
    }
    else 
    {
      benchmark$benchmark_percentil2[i] <- benchmark$`jednotkova teoreticka cena`[i]
    }
  }
  ###celkove percentil2 ceny
  for(i in 1:length({benchmark$percentil2}))
  {
    benchmark$benchmark_percentil2_spolu[i] <- benchmark$benchmark_percentil2[i]*benchmark$Množstvo[i]
  }
  ##unikatne opt_ceny 
  # for(i in 1:length({benchmark$minimum}))
  # {
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$unikat[i] < benchmark$`jednotkova teoreticka cena`[i])
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$unikat[i]
  #     benchmark$lower_to_min[i] <- "FALSE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==TRUE && benchmark$`jednotkova teoreticka cena`[i] < benchmark$minimum[i] && benchmark$unikat[i] > benchmark$`jednotkova teoreticka cena`[i] )
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$minimum[i]
  #     # benchmark$benchmark_opt_unikat[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     benchmark$lower_to_min[i] <- "TRUE"
  #   }
  #   if (benchmark$benchmarkujeme[i]==FALSE || (benchmark$unikat[i] > benchmark$`jednotkova teoreticka cena`[i]  && benchmark$`jednotkova teoreticka cena`[i] > benchmark$minimum[i]))
  #   {
  #     benchmark$benchmark_opt_unikat[i] <- benchmark$`jednotkova teoreticka cena`[i]
  #     
  #   }
  # }
  ###celkove opt_priemerne ceny
  #   for(i in 1:length({benchmark$minimum}))
  #   {
  #     benchmark$benchmark_opt_unikat_spolu[i] <- benchmark$benchmark_opt_unikat[i]*benchmark$Množstvo[i]
  #   }
  #   ###rozdiel benchmarku podla opt_unikatu od zmluvnej ceny položiek
  #   for(i in 1:length({benchmark$minimum}))
  #   {
  #     if (benchmark$benchmark_opt_unikat_spolu[i]>0)
  #     {
  #       benchmark$rozdiel_opt_unikat[i] <- benchmark$benchmark_opt_unikat_spolu[i]-benchmark$`Cena celkom`[i]
  #     }
  #     else {
  #       benchmark$rozdiel_opt_unikat[i] <-0
  #     }
  #   }
  #   #####WANNABE KONTIGENCKA
  sumarizacia <- aggregate(cbind('Zmluvná cena'=benchmark$`Cena celkom`,'percentil1'=benchmark$benchmark_percentil1_spolu, 'percentil2'=benchmark$benchmark_percentil2_spolu)~`Èas stavby`, benchmark, sum)
  l <- nrow(sumarizacia)
  sumarizacia_spolu <- sumarizacia
  for (i in 2:ncol(sumarizacia_spolu))
  {
    sumarizacia_spolu[l+1,i] <- sum(sumarizacia_spolu[,i])
    sumarizacia_spolu[is.na(sumarizacia_spolu)] <- 0
  }
  sumarizacia_spolu[nrow(sumarizacia_spolu),1] <- 'SPOLU'
  zaver <- list(benchmark=benchmark,sumarizacia=sumarizacia, sumarizacia_spolu=sumarizacia_spolu)
  return(zaver)
}


testik <- c(0.1,0.2)
skewness(testik)
