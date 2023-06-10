# Carrega pacotes
library(tidyverse)
library(LexisPlotR)

setwd("C:/Users/jehhv/OneDrive/Documentos/UnB/2023/2023.1/Demografia_Ana Maria/Trabalho 1 - Natalidade_Fecundidade e Mortalidade")
# Importa dados para questao 1 ------------

nascimentos <- read_csv2("Nascidos_vivos_SINASC_tabela.csv", #dados simples de nascimento por ano
                         locale=locale(encoding="latin1"))

names(nascimentos)<- c("Ano_nascimento","Nascimentos")

obitos <- readRDS("dados_SIM.rds") # microdados do SIM


### a) Construir o Diagrama de Lexis para os dados de nascidos vivos de 2000 a 2021 ------
# da UF escolhida (SINASC) e de obitos menores de 5 anos (idades simples) para o mesmo
# periodo segundo ano de nascimento.

obitos$ano_nasc <- str_sub(obitos$DTNASC,5,8) #obter a parte de uma string; pegar do quinto ao oitavo caractere
obitos$ano_obt <- str_sub(obitos$DTOBITO, 5,8)

tabela <- obitos %>% mutate(idade_arrumada = case_when(str_sub(IDADE,1,1)<4 ~ 0,
                                                     str_sub(IDADE,1,1)==4 ~ 0+as.numeric(str_sub(IDADE,2)),
                                                     str_sub(IDADE,1,1)==5 ~ 100+as.numeric(str_sub(IDADE,2)))) %>%
  mutate(idade_certa = floor(as.numeric(difftime(dmy(DTOBITO), dmy(DTNASC), units = "days")/365))) %>%
  select(ano_nasc, ano_obt, idade_certa, IDADE) %>% #apenas as variaveis q vao para a tabela
  filter(idade_certa < 5) %>% #obitos menores de 5 anos
  group_by(ano_nasc, ano_obt, idade_certa) %>% #faz sentido agrupar pq ha mtos dados em comum
  mutate(n = n()) %>% #cria nova variavel de contagem
  ungroup() %>%
  distinct(idade_certa, ano_nasc, ano_obt, .keep_all = T) %>%
  arrange(ano_obt, desc(ano_nasc), idade_certa) %>%
  mutate(TRI = case_when(as.numeric(ano_obt) - as.numeric(ano_nasc) == as.numeric(idade_certa) ~ 0,
                         as.numeric(ano_obt) - as.numeric(ano_nasc) != as.numeric(idade_certa) ~1))

# Em uma tabela, criou a nova variavel "idade_arrumada" com mutate
# case_when: alterou a codificacao da idade com base no dicionario dos dados de obito (verificar dicio para entender)
# difftime é usada para calcular a diferenca de tempo entre as datas nas unidades necessarias como dias, semanas, meses, anos, etc 
  # parametros: data1, data2: datas para calcular; units: unidades de diferença (dias, semanas, meses, etc)
#dmy(): estabelece explicitamente a ordem dia-mes-ano. 
# idade_certa: faz a diferenca entre data de obito e de nascimento, mas antes faz as transformacoes necessarias
  # o valor arredonda para baixo com floor() porque considera-se apenas idade completa

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

### b) Supondo população fechada (inexistência de migração), calcule a probabilidade de ------
# um recém-nascido na UF ou território de escolha sobreviver à idade exata 5 
# para as coortes de 2000 a 2016.

obt_coorte <- tabela %>% 
  filter(TRI==0 & ano_nasc %in% c(2000,2001,2002,2003,2004,
                                  2005,2006,2007,2008,2009,
                                  2010,2011,2012,2013,2014,
                                  2015,2016))
# npx = 1-nqx
# nqx = ndx/lo

obt_coorte$ano_nasc <- as.numeric(obt_coorte$ano_nasc)

ndx <- obt_coorte %>% 
  select(ano_nasc,n) %>% 
  group_by(ano_nasc) %>% 
  summarise(sum(n)) #numeradores de nqx

lo <- nascimentos %>% 
  filter(Ano_nascimento %in% c(2000,2001,2002,2003,2004,
                               2005,2006,2007,2008,2009,
                               2010,2011,2012,2013,2014,
                               2015,2016))
nqx <- ndx/lo
npx <- 1-nqx

# referencia para tabelas: https://jonnyphillips.github.io/Ciencia_de_Dados/Tabelas.html

### c) Considerando o mesmo pressuposto, calcule a probabilidade de sobreviver -----
# ao primeiro aniversário dos recém-nascidos no período de 2000 a 2020.
  
obt_coorte <- tabela %>% 
  filter(TRI==0 & ano_nasc %in% c(2000,2001,2002,2003,2004,
                                  2005,2006,2007,2008,2009,
                                  2010,2011,2012,2013,2014,
                                  2015,2016,2017,2018,2019,
                                  2020))

obt_coorte$ano_nasc <- as.numeric(obt_coorte$ano_nasc)

lo <- nascimentos %>% 
  filter(Ano_nascimento %in% c(2000,2001,2002,2003,2004,
                               2005,2006,2007,2008,2009,
                               2010,2011,2012,2013,2014,
                               2015,2016,2017,2018,2019,
                               2020))

ndx0 <- obt_coorte %>% 
  filter(idade_certa == 0) %>% 
  select(ano_nasc,n) %>% 
  group_by(ano_nasc) %>% 
  summarise(sum(n)) #numeradores de nqx


nqx0 <- ndx0/lo
npx0 <- 1-nqx0

### d) Comente sobre os valores encontrados. Não esquecer a qualidade da informação trabalhada.

# O Diagrama de Lexis apresentado a seguir auxilia a localizar visualmente os nascimentos vivos e óbitos menores 
#  de 5 anos ocorridos entre os anos de 2000 e 2021 no estado de Santa Catarina. Os valores destacados em vermelho
# indicam o total de nascimentos em cada ano observado, enquanto a visão longitudinal acompanha os grupos de
# indivíduos ao longo do tempo, com nascimentos e óbitos a cada faixa etária. Observa-se, no geral, que a quantidade
# de óbitos diminui não somente ao longo dos anos, mas também a medida em que as crianças envelhecem. 

# Supondo população fechada (inexistência de migração), a probabilidade de um recém-nascido em Santa Catarina sobreviver
# à idade exata de 5 anos para as gerações de 2000 a 2016 está em torno de 98$\%$ para todas as coortes. 
# Resultado próximo foi observado para a probabilidade de sobreviver ao primeiro aniversário de um recém-nascido no
# estado, entre os anos de 2000 a 2020. 

# Ocorre que tais resultados estão condicionados a situações como subnotificação de registros, imprecisões durante
# a coleta dos dados e outras possíveis limitações. 
