# Carrega pacotes
library(tidyverse)
library(LexisPlotR)

setwd("/home/laura/UNB/DEMOG/Trabalho1")
# Importa dados para questao 1 ------------

nascimentos <- read_csv2("Nascidos_vivos_SINASC_tabela.csv", #dados simples de nascimento por ano
                         locale=locale(encoding="latin1"))

names(nascimentos)<- c("Ano_nascimento","Nascimentos")

obitos <- readRDS("dados_SIM.rds") # microdados do SIM


### a) Construir o Diagrama de Lexis para os dados de nascidos vivos de 2000 a 2021 ------
# da UF escolhida (SINASC) e de óbitos menores de 5 anos (idades simples) para o mesmo
# período segundo ano de nascimento.

obitos$ano_nasc <- str_sub(obitos$DTNASC,5,8)
obitos$ano_obt <- str_sub(obitos$DTOBITO, 5,8)

tabela <- obitos %>% mutate(idade_arrumada = case_when(str_sub(IDADE,1,1)<4 ~ 0,
                                                     str_sub(IDADE,1,1)==4 ~ 0+as.numeric(str_sub(IDADE,2)),
                                                     str_sub(IDADE,1,1)==5 ~ 100+as.numeric(str_sub(IDADE,2)))) %>%
  mutate(idade_certa = floor(as.numeric(difftime(dmy(DTOBITO), dmy(DTNASC), units = "days")/365))) %>%
  select(ano_nasc, ano_obt, idade_certa, IDADE) %>%
  filter(idade_certa < 5) %>%
  group_by(ano_nasc, ano_obt, idade_certa) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  distinct(idade_certa, ano_nasc, ano_obt, .keep_all = T) %>%
  arrange(ano_obt, desc(ano_nasc), idade_certa) %>%
  mutate(TRI = case_when(as.numeric(ano_obt) - as.numeric(ano_nasc) == as.numeric(idade_certa) ~ 0,
                         as.numeric(ano_obt) - as.numeric(ano_nasc) != as.numeric(idade_certa) ~1))

# Plotando diagrama de Lexis

diagrama <- lexis_grid(lwd=0.15, year_start = 1995, year_end = 2023, age_start = -1, age_end = 5) + 
  annotate(geom="text", x=as.Date(paste0(tabela$ano_obt[tabela$TRI==1]
                                         ,"-05-07"))
           ,y=tabela$idade_certa[tabela$TRI==1]+0.75,
           label=c(tabela$n[tabela$TRI==1]), size = 2,
           color="black", fontface = "bold") +
  annotate(geom = "text", x= as.Date(paste0(nascimentos$Ano_nascimento,"-07-01")),
           y= -0.2,
           label=nascimentos$Nascimentos,size = 1.6 ,color = "darkred", fontface = "bold") +
  annotate(geom="text", x=as.Date(paste0(tabela$ano_obt[tabela$TRI==0]
                                         ,"-10-01"))
           ,y=tabela$idade_certa[tabela$TRI==0]+0.4,
           label=c(tabela$n[tabela$TRI==0]), size = 2,
           color="black", fontface = "bold") +
  labs(
       x = "Ano",
       y = "Idade") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
diagrama


ggsave("diaglexis_a.png", width = 188, height = 93, units = "mm")
