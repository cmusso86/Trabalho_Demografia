# Carrega bibliotecas
pacman::p_load(tidyverse, LexisPlotR, readxl, janitor, lubridate, reshape2, cowplot)
pacman::p_load_gh("danicat/read.dbc")

# Dados -------

populacao <- read_excel("Bases/populacao_Sc.xls")
mortalidade <- readRDS("dados_SIM.rds")

# Letra a -------

# a) Com base nos dados sobre óbitos do SIM para 2010, 2019 e  2021 e a população
# por sexo e idade estimada (projetada) para a UF, obtenha os seguintes indicadores:

mortalidade$ano_obt <- str_sub(mortalidade$DTOBITO, 5,8)
# idade em anos (data obito - data nascimento)
mortalidade$data_obito <- lubridate::dmy(mortalidade$DTOBITO)
mortalidade$data_nasc <- lubridate::dmy(mortalidade$DTNASC)
mortalidade$idade <- (mortalidade$data_obito-mortalidade$data_nasc) #diferenca em dias
mortalidade$idade <- as.numeric((mortalidade$idade)/365)
mortalidade <- mortalidade %>% mutate(sexo = case_when(
  SEXO==1 ~ "M",
  SEXO==2 ~ "F",
  SEXO==9 ~ "Ignorado"
))

mort_anos_filtered <- mortalidade %>% filter(ano_obt %in% c("2010","2019","2021"))

pop_grupos <- populacao %>% 
  select(IDADE, Sexo, "2010","2019", "2021" ) %>% 
  mutate(IDADE=str_replace(IDADE, "\\+", "")) %>% 
  mutate(IDADE=as.numeric(IDADE)) %>% 
  mutate(faixa=cut(IDADE, breaks=c(-1, 1, 5, 10, 15, 20,
                                   25, 30, 35, 40, 45, 50, 
                                   55, 60, 65, 70, 75, 80, 
                                   85,90,  200), 
                   labels=c("<1", "1-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29", "30-34", "35-39", 
                            "40-44", "45-49", "50-54", "55-59",
                            "60-64", "65-69", "70-74", "75-79", 
                            "80-84", "85-89", ">90"), right=F)) %>% 
  pivot_longer(cols=c("2010", "2019", "2021"), names_to = "Ano", 
               values_to = "Pop") %>% 
  group_by(Ano, Sexo, faixa) %>% 
  summarise(Pop=sum(Pop)) %>% 
  pivot_wider(id_cols=c(Sexo, faixa), 
              names_from = Ano, values_from = Pop) 


obitos_trat <- mort_anos_filtered %>% 
  select(data_obito, data_nasc, idade, ano_obt,sexo) %>% 
  mutate(faixa=cut(idade, breaks=c(-1, 1, 5, 10, 15, 20,
                                      25, 30, 35, 40, 45, 50, 
                                      55, 60, 65, 70, 75, 80, 
                                      85, 90,  200), 
                   labels=c("<1", "1-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29", "30-34", "35-39", 
                            "40-44", "45-49", "50-54", "55-59",
                            "60-64", "65-69", "70-74", "75-79", 
                            "80-84", "85-89", ">90"), right=F))

  
## Taxa Bruta de Mortalidade (TBM) ----

obitos_tot <- obitos_trat %>% 
  group_by(ano_obt) %>% 
  count(name="obitos")
names(obitos_tot)[1] <- "Ano"

TBM <- pop_grupos %>% 
  adorn_totals() %>% 
  filter(Sexo=="Total") %>% 
  select("2010", "2019", "2021") %>% 
  pivot_longer(everything(), names_to = "Ano",
               values_to = "Pop") %>% 
  left_join(obitos_tot) %>% 
  mutate(TBM = obitos/Pop*1000)

TBM


## Taxas específicas de mortalidade por sexo e idade - nMx (grafique) ----

obitos_tot_idade_sexo <- obitos_trat %>% 
  filter(sexo %in% c("M","F")) %>%
  group_by(ano_obt,faixa,sexo) %>% 
  count(name="obitos")
names(obitos_tot_idade_sexo)[1] <- "Ano"

pop_desagrup <- melt(pop_grupos, c("Sexo","faixa"), value.name="populacao")
names(pop_desagrup)[3] <- "Ano"
names(pop_desagrup)[1] <- "sexo"

nMx <- obitos_tot_idade_sexo %>% 
  left_join(pop_desagrup,by=c("Ano","faixa","sexo")) %>% 
  mutate(nMx = obitos/populacao) %>% 
  filter(!is.na(faixa))

nMx$sexo <- factor(nMx$sexo, levels = c("M","F"),labels=c("Masculino","Feminino"))

plot2010 <- nMx %>% filter(Ano=="2010") %>%
ggplot(aes(x=faixa,y=nMx,group=sexo,colour=sexo))+
geom_line(size=1)+
  scale_y_log10()+
scale_colour_manual(name="Sexo",values=c("darkcyan",
"darkred"))+
labs(x="",y="nMx",title="2010")+
theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")

plot2019 <- nMx %>% filter(Ano=="2019") %>%
  ggplot(aes(x=faixa,y=nMx,group=sexo,colour=sexo))+
  geom_line(size=1)+
  scale_y_log10()+
  scale_colour_manual(name="Sexo",values=c("darkcyan",
                                           "darkred"))+
  labs(x="",y="nMx",title="2019")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")

plot2021 <- nMx %>% filter(Ano=="2021") %>%
  ggplot(aes(x=faixa,y=nMx,group=sexo,colour=sexo))+
  geom_line(size=1)+
  scale_y_log10()+
  scale_colour_manual(name="Sexo",values=c("darkcyan",
                                           "darkred"))+
  labs(x="",y="nMx",title="2021")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")


cowplot::plot_grid(plot2010,plot2019,plot2021)
ggsave('./img/TEM.png', width = 188, height = 113, units = 'mm')

# Letra b -------

# Calcule a TMI, utilizando o número médio de óbitos ocorridos entre 2019 e 2021 
# e o número de nascimentos de 2020. Calcule os indicadores: taxa de mortalidade 
# neonatal, neonatal precoce, neonatal tardia, posneonatal. Agregando a informação 
# sobre óbitos fetais para os mesmos anos, calcule a taxa de mortalidade perinatal.

nasc_2020 <- 97916 # vem dos arquivos do SINASC
mortalidade$idade_dias <- mortalidade$idade*365

## TMI (óbitos em menores de 1 ano)

# nº medio de obitos de menores de 1 ano

obitos_1_ano <- mortalidade %>% filter(ano_obt %in% c("2019","2020","2021")) %>%
  filter(idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_1_ano <- mean(obitos_1_ano$obitos)

q10 <- media_obt_1_ano/nasc_2020*1000

## Mortalidade Neonatal (Risco de um recém-nascido morrer antes de 28 dias de vida)

obitos_neonatal <- mortalidade %>% filter(ano_obt %in% c("2019","2020","2021")) %>%
  filter(idade_dias<28) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_neonatal <- mean(obitos_neonatal$obitos)

Mneonatal <- media_obt_neonatal/nasc_2020*1000


## Mortalidade Posneonatal (Risco de um recém-nascido morrer entre 28 e 364 dias de vida)

obitos_posneonatal <- mortalidade %>% filter(ano_obt %in% c("2019","2020","2021")) %>%
  filter(idade_dias>=28 & idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_posneonatal <- mean(obitos_posneonatal$obitos)

Mposneonatal <- media_obt_posneonatal/nasc_2020*1000

## Mortalidade Precoce (Risco de um recém-nascido morrer antes de 7 dias de vida)

obitos_precoce <- mortalidade %>% filter(ano_obt %in% c("2019","2020","2021")) %>%
  filter(idade_dias<7) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_precoce <- mean(obitos_precoce$obitos)

Mprecoce <- media_obt_precoce/nasc_2020*1000

## Mortalidade Tardia (Risco de um recém-nascido morrer entre 7 e 27 dias de vida)

obitos_tardia <- mortalidade %>% filter(ano_obt %in% c("2019","2020","2021")) %>%
  filter(idade_dias>=7 & idade_dias<27) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_tardia <- mean(obitos_tardia$obitos)

Mtardia <- media_obt_tardia/nasc_2020*1000


## Taxa de Mortalidade Perinatal
