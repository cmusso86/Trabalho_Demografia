# Carregando bibliotecas ---------
setwd("/home/laura/UNB/DEMOG/Trabalho1/dados_SIM")
pacman::p_load(foreign, tidyverse, LexisPlotR, lubridate, devtools)

devtools::install_github("danicat/read.dbc")

library(read.dbc)

# Banco de mortalidade - SIM ---------

anos <- 2000:2021
arquivos_nome <- str_c("DOSC",anos,".dbc") %>% as.list()

dados_list <- map(arquivos_nome,~ read.dbc(.x))

dados_SIM <- dplyr::bind_rows(dados_list)

saveRDS(dados_SIM,"dados_SIM.rds")
