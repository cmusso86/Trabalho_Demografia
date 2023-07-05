# Carrega bibliotecas
pacman::p_load(tidyverse, LexisPlotR, readxl, janitor, lubridate, reshape2, cowplot, kableExtra)
pacman::p_load_gh("danicat/read.dbc")

#b) Assumindo população fechada (ausência de migração), projete a população da UF
# escolhida de 2010 a 2020, segundo o cenário de mortalidade e fecundidade constante.
# Obtenha indicadores de fecundidade e mortalidade para o período 2014-2016 
# (nascimentos e óbitos médios do triênio e população projetada pelo IBGE para 2015).
# Construa uma tábua de vida e obtenha as taxas específicas de fecundidade.

# Dados -------

populacao <- read_excel("Bases/populacao_Sc.xls")
mortalidade <- readRDS("dados_SIM.rds")

#### Tábua de vida para 2014-2016 centrada em 2015 -----

## Calculando as taxas especificas de mortalidade ----

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
mortalidade$idade_dias <- mortalidade$idade*365

mort_anos_filtered <- mortalidade %>% filter(ano_obt %in% c("2014","2015","2016"))

pop_grupos <- populacao %>% 
  select(IDADE, Sexo, "2015") %>% 
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
  pivot_longer(cols=c("2015"), names_to = "Ano", 
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


## Taxas específicas de mortalidade por sexo e idade - nMx
# o calculo é: media de obitos no triênio 2014-2016/populacao de 2015

obitos_tot_idade_sexo <- obitos_trat %>% 
  filter(sexo %in% c("M","F")) %>%
  group_by(ano_obt,faixa,sexo) %>% 
  count(name="obitos")
names(obitos_tot_idade_sexo)[1] <- "Ano"

obitos_medio_trienio <- obitos_tot_idade_sexo %>%
  group_by(faixa,sexo) %>%
  summarise(obitos_medio = mean(obitos))

obitos_medio_trienio$Ano <- factor("2015",levels=c("2015"))

pop_desagrup <- melt(pop_grupos, c("Sexo","faixa"), value.name="populacao")
names(pop_desagrup)[3] <- "Ano"
names(pop_desagrup)[1] <- "sexo"

nMx <- obitos_medio_trienio %>% 
  left_join(pop_desagrup,by=c("Ano","faixa","sexo")) %>% 
  mutate(nMx = obitos_medio/populacao) %>% 
  filter(!is.na(faixa))

nMx$sexo <- factor(nMx$sexo, levels = c("M","F"),labels=c("Masculino","Feminino"))

plotnMx <- nMx %>%
  ggplot(aes(x=faixa,y=nMx,group=sexo,colour=sexo))+
  geom_line(size=1)+
  scale_y_log10()+
  scale_colour_manual(name="Sexo",values=c("darkcyan",
                                           "darkred"))+
  labs(x="",y="nMx",title="óbitos médios 2014-2016")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")

#ggsave('./img/nMx_TP2.png', width = 158, height = 93, units = 'mm',bg="white")


## Calculando as tabuas (código similar ao do T1) ----

l0 <- 100000

# Tábua sexo FEMININO
# calculando a TMI feminina

nasc_2015_feminino <- 47334 #dados do SINASC
nasc_2015_masculino <- 49884

# nº medio de obitos de menores de 1 ano
obitos_1_ano_mulher <- mortalidade %>% filter(ano_obt %in% c("2014","2015","2016")) %>%
  filter(sexo=="F") %>%
  filter(idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_1_ano_F <- mean(obitos_1_ano_mulher$obitos)

q10F <- media_obt_1_ano_F/nasc_2015_feminino*1000

# Estimando os fatores de separacao

calcula_fator_separacao <- function(x,n,dados){
  obitos_periodo <- dados %>% filter(idade>=x & idade<=(x+n))
  tempo_vida_no_intervalo <- (obitos_periodo$idade - x)
  nkx <- mean(tempo_vida_no_intervalo)
  
  return(nkx)
}


mort_feminino <- mortalidade %>% filter(sexo=="F")
x <- c(0,1,seq(5,90,5)) 
n <- c(1,4,rep(5,17),35) # o ultimo n=35 é para aceitar idades até 90+35=125

ks_femininos <- purrr::map2(x,n, ~ calcula_fator_separacao(.x,.y,mort_feminino)) %>%
  unlist()

nMx_feminino <- nMx %>% filter(sexo=="Feminino")
nMx_feminino <- nMx_feminino$nMx

nqxF <- n*nMx_feminino/(1+(n-ks_femininos)*nMx_feminino)
nqxF[1] <- q10F/1000
nqxF[20] <- 1

lx <- c(l0)
ndx <- lx * nqxF[1]

for (i in 2:20){
  lx[i] <- lx[i-1]-ndx[i-1]
  ndx[i] <- lx[i] * nqxF[i]
}

nLxF <- lx*n + ndx*ks_femininos
nLxF[20] <- ndx[20]*ks_femininos[20] 
TxF <- rev(cumsum(rev(nLxF)))

tabua_feminino <- data.frame("x"=x,"n"=c(n[-20],"+"),
                             "nMx"=round(nMx_feminino,4),
                             "nkx"=round(ks_femininos,3),
                             "nqx"=round(nqxF,4),
                             "lx"=lx,
                             "ndx"=round(ndx,3),
                             "nLx"=nLxF,
                             "Tx"=TxF,
                             "ex"=round(TxF/lx,3),
                             check.names = F)

tabua_feminino %>%
  kableExtra::kbl(.,align=rep('r',10),booktabs = T,
                  caption = 'Tábua de vida para o sexo feminino com média de três anos centrada
em 2015 - Santa Catarina') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")


# Tábua sexo MASCULINO
# calculando a TMI masculina

# nº medio de obitos de menores de 1 ano
obitos_1_ano_homem <- mortalidade %>% filter(ano_obt %in% c("2014","2015","2016")) %>%
  filter(sexo=="M") %>%
  filter(idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_1_ano_M <- mean(obitos_1_ano_homem$obitos)

q10M <- media_obt_1_ano_M/nasc_2015_masculino*1000

# Estimando os fatores de separacao
mort_masc <- mortalidade %>% filter(sexo=="M")
x <- c(0,1,seq(5,90,5)) 
n <- c(1,4,rep(5,17),35) # o ultimo n=35 é para aceitar idades até 90+35=125

ks_masc <- purrr::map2(x,n, ~ calcula_fator_separacao(.x,.y,mort_masc)) %>%
  unlist()

nMx_masc <- nMx %>% filter(sexo=="Masculino")
nMx_masc <- nMx_masc$nMx

nqxM <- n*nMx_masc/(1+(n-ks_masc)*nMx_masc)
nqxM[1] <- q10M/1000
nqxM[20] <- 1

lx_masc <- c(l0)
ndx_masc <- lx_masc * nqxM[1]

for (i in 2:20){
  lx_masc[i] <- lx_masc[i-1]-ndx_masc[i-1]
  ndx_masc[i] <- lx_masc[i] * nqxM[i]
}

nLxM <- lx_masc*n + ndx_masc*ks_masc
nLxM[20] <- ndx_masc[20]*ks_masc[20] 
TxM <- rev(cumsum(rev(nLxM)))

tabua_masculino <- data.frame("x"=x,"n"=c(n[-20],"+"),
                              "nMx"=round(nMx_masc,4),
                              "nkx"=round(ks_masc,3),
                              "nqx"=round(nqxM,4),
                              "lx"=lx_masc,
                              "ndx"=round(ndx_masc,3),
                              "nLx"=nLxM,
                              "Tx"=TxM,
                              "ex"=round(TxM/lx_masc,3),
                              check.names = F)

tabua_masculino %>%
  kableExtra::kbl(.,align=rep('r',10),booktabs = T,
                  caption = 'Tábua de vida para o sexo masculino com média de três anos centrada
em 2015 - Santa Catarina') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")


### Taxas específicas de fecundidade -----

arquivos_nasc <- str_c("Bases/", 
                       list.files(path="Bases/", pattern="^TP2"))

nascimentos <-  map_dfr(arquivos_nasc,~ read.dbc(.x)) %>% 
  mutate_all(~as.character(.))

nascimentos_trat <- nascimentos %>% 
  select(IDADEMAE, DTNASC, SEXO, DTNASCMAE) %>% 
  mutate(DTNASC=dmy(DTNASC),
         DTNASCMAE=dmy(DTNASCMAE),
         IDADEMAE=as.numeric(IDADEMAE)) %>% 
  mutate(Ano=as.character(year(DTNASC)),
         faixa=cut(IDADEMAE, breaks=c(-1, 1, 5, 10, 15, 20,
                                      25, 30, 35, 40, 45, 50, 
                                      55, 60, 65, 70, 75, 80, 
                                      85,90,  200), 
                   labels=c("<1", "1-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29", "30-34", "35-39", 
                            "40-44", "45-49", "50-54", "55-59",
                            "60-64", "65-69", "70-74", "75-79", 
                            "80-84", "85-89", ">90"), right=F))


pop_fem_fertil <- pop_grupos %>% 
  filter(Sexo=="F", 
         faixa %in% c("15-19", "20-24", "25-29",
                      "30-34", "35-39", "40-44",
                      "45-49")) 

pop_fem_fertil_faixa <- pop_fem_fertil %>% 
  pivot_longer(c("2015"),
               names_to = "Ano", values_to = "Pop") 

nascimentos_medio <- nascimentos_trat %>% 
  group_by(faixa,Ano) %>% 
  count(name = "nasc") %>% 
  group_by(faixa) %>%
  summarise(nasc_medio=mean(nasc))

nascimentos_medio$Ano <- "2015"

nfx <- nascimentos_medio %>% 
  right_join(pop_fem_fertil_faixa) %>% 
  mutate(nfx = nasc_medio/Pop) %>% 
  select(Ano, faixa, nfx) %>% 
  pivot_wider(id_cols = faixa, 
              names_from = Ano, 
              values_from = nfx) 

nfx %>%
  kableExtra::kbl(.,align=c('l','r'),booktabs = T,
                  caption = 'Taxas específicas de fecundidade com média de três anos centrada
em 2015 - Santa Catarina',col.names=c("Faixa etária","nfx") ) %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")
