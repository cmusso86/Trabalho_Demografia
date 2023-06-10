# Carrega bibliotecas
pacman::p_load(tidyverse, LexisPlotR, readxl, janitor, lubridate, reshape2, cowplot, kableExtra)
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
ggsave('./img/TEM.png', width = 188, height = 113, units = 'mm',bg="white")

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

obitos_fetais <- data.frame("Ano"= 2019:2021,"Obt_fetal" = c(810,765,771)) #consultado no TabNet SIM
media_obt_fetal <- mean(obitos_fetais$Obt_fetal)

TMPerinatal <- (media_obt_precoce+media_obt_fetal)/(nasc_2020+media_obt_fetal)*1000

# Letra c -------

# Compare os seus resultados com os valores obtidos pelo estudo GBD, pela Nações Unidas
# (UN Population) e aqueles publicados no site do Datasus (RIPSA - Indicadores e
# dados básicos - http://tabnet.datasus.gov.br/cgi/idb2012/matriz.htm ).
# Para a TMI, compare com os valores obtidos na questão 1. Comente sobre os aspectos
# metodológicos dessas duas formas de cálculo.

# Letra d -------

# Compare a estrutura de mortalidade por causas entre 2010 e 2021. Utilize 20 primeiras
# grupos de causas segundo Grupos CID-10 (ver Tabnet - Datasus),  segundo sexo para
# os anos selecionado. Comente os resultados. Destacar a mortalidade por Covid-19 (CID B34.2). 

obts_causas <- mortalidade %>% filter(ano_obt %in% c("2010","2021")) %>%
  select(CAUSABAS,ano_obt,sexo) %>%
  filter(sexo %in% c("M","F"))

obts_causas_top_20 <- obts_causas %>% group_by(ano_obt,sexo,CAUSABAS) %>%
  summarise(causas_count = n()) %>% ungroup() %>%
  group_by(ano_obt,sexo) %>%
  arrange(desc(causas_count),.by_group = TRUE) %>%
  slice_head(n=20)

# conferindo oq é cada código da CID

CID <- data.frame("CAUSABAS"=unique(obts_causas_top_20$CAUSABAS),
                  "significado"=c("Infarto agudo do miocárdio não especificado",
                                  "Acidente vascular cerebral, não especificado como hemorrágico ou isquêmico",
                                  "Diabetes mellitus não especificado - sem complicações",
                                  "Pneumonia não especificada",
                                  "Neoplasia maligna da mama, não especificada",
                                  "Hipertensão essencial (primária)",
                                  "Doença pulmonar obstrutiva crônica não especificada",
                                  "Neoplasia maligna dos brônquios ou pulmões, não especificado",
                                  "Outras causas mal definidas e as não especificadas de mortalidade",
                                  "Insuficiência cardíaca congestiva",
                                  "Insuficiência cardíaca não especificada",
                                  "Outras doenças cerebrovasculares especificadas",
                                  "Doença cardíaca hipertensiva com insuficiência cardíaca (congestiva)",
                                  "Doença de Alzheimer não especificada",
                                  "Septicemia não especificada",
                                  "Doença pulmonar obstrutiva crônica com infecção respiratória aguda do trato respiratório inferior",
                                  "Morte sem assistência",
                                  "Neoplasia maligna do estômago, não especificado",
                                  "Infecção do trato urinário de localização não especificada",
                                  "Hemorragia intracerebral não especificada",
                                  "Neoplasia maligna da próstata",
                                  "Neoplasia maligna do esôfago, não especificado",
                                  "Lesão autoprovocada intencionalmente por enforcamento, estrangulamento e sufocação",
                                  "Outras formas de cirrose hepática e as não especificadas",
                                  "Cirrose hepática alcoólica",
                                  "Pessoa traumatizada em um acidente de trânsito com um veículo a motor não especificado",
                                  "Infecção por coronavírus de localização não especificada",
                                  "Pneumonia bacteriana não especificada",
                                  "Sequelas de acidente vascular cerebral não especificado como hemorrágico ou isquêmico"))

obts_causas_top_20 <- left_join(obts_causas_top_20, CID, by="CAUSABAS") 

# tabela para os anos de 2010 e 2021 FEMININO

df_mulheres <- obts_causas_top_20 %>% filter(sexo=="F")

tbl_mulheres <- data.frame("significado"= df_mulheres$significado[1:20],
                           "2010" = df_mulheres$causas_count[1:20],
                           check.names = F) %>%
            left_join(.,df_mulheres[21:40,c("causas_count","significado")],
            by="significado")

names(tbl_mulheres)[3] <- "2021"

tbl_mulheres <- rbind(tbl_mulheres, data.frame(significado=df_mulheres$significado[21],"2010"="-",
                                               "2021"=df_mulheres$causas_count[21],check.names = F))

tbl_mulheres <- rbind(tbl_mulheres, data.frame(significado=df_mulheres$significado[37],"2010"="-",
                                               "2021"=df_mulheres$causas_count[37],check.names = F))

tbl_mulheres$`2021`[is.na(tbl_mulheres$`2021`)] <- "-"
names(tbl_mulheres)[1] <- "Causa básica da DO"

tbl_mulheres %>%
  kableExtra::kbl(.,align=c('l','r','r'),booktabs = T,
  caption = 'Estrutura de mortalidade por causas de 2010 e 2021 para o sexo feminino') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")

# tabela para os anos de 2010 e 2021 MASCULINO

df_homens <- obts_causas_top_20 %>% filter(sexo=="M")

tbl_homens <- data.frame("significado"= df_homens$significado[1:20],
                           "2010" = df_homens$causas_count[1:20],
                           check.names = F) %>%
  left_join(.,df_homens[21:40,c("causas_count","significado")],
            by="significado")

names(tbl_homens)[3] <- "2021"

#causas de 2021 que nao estavam em 2010
tbl_homens <- rbind(tbl_homens, data.frame(significado=df_homens$significado[21],"2010"="-",
                                               "2021"=df_homens$causas_count[21],check.names = F))

tbl_homens <- rbind(tbl_homens, data.frame(significado=df_homens$significado[33:34],"2010"="-",
                                               "2021"=df_homens$causas_count[33:34],check.names = F))

tbl_homens <- rbind(tbl_homens, data.frame(significado=df_homens$significado[38:39],"2010"="-",
                                           "2021"=df_homens$causas_count[38:39],check.names = F))

tbl_homens$`2021`[is.na(tbl_homens$`2021`)] <- "-"
names(tbl_homens)[1] <- "Causa básica da DO"

tbl_homens %>%
  kableExtra::kbl(.,align=c('l','r','r'),booktabs = T,
                  caption = 'Estrutura de mortalidade por causas de 2010 e 2021 para o sexo masculino') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")

# Letra e -------

# Construa Tábuas de Vida para cada sexo para a UF escolhida para 2021, a partir das
# taxas específicas de mortalidade obtidas no item a:

# Utilize a TMI obtida no item b ou do estudo Global Burden of Disease - GBD.
# Lembre que deve-se obter a TMI para cada sexo em separado.

# Estime os fatores de separação, para cada sexo, para as idades 0-1 e 1-4 com base nos microdados do SIM.

l0 <- 100000

# Tábua sexo FEMININO
# calculando a TMI feminina

nasc_2021_feminino <- 46911 #dados do SINASC
nasc_2021_masculino <- 49577
# nº medio de obitos de menores de 1 ano
obitos_1_ano_mulher <- mortalidade %>% filter(ano_obt %in% c("2021")) %>%
  filter(sexo=="F") %>%
  filter(idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_1_ano_F <- mean(obitos_1_ano_mulher$obitos)

q10F <- media_obt_1_ano_F/nasc_2021_feminino*1000

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

nMx_feminino <- nMx %>% filter(Ano==2021 & sexo=="Feminino")
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
                  caption = 'Tábua de vida para o sexo feminino em 2021 - Santa Catarina') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")

# Tábua sexo MASCULINO
# calculando a TMI masculina

# nº medio de obitos de menores de 1 ano
obitos_1_ano_homem <- mortalidade %>% filter(ano_obt %in% c("2021")) %>%
  filter(sexo=="M") %>%
  filter(idade_dias<365) %>% group_by(ano_obt) %>%
  summarise(obitos=n())

media_obt_1_ano_M <- mean(obitos_1_ano_homem$obitos)

q10M <- media_obt_1_ano_M/nasc_2021_masculino*1000

# Estimando os fatores de separacao
mort_masc <- mortalidade %>% filter(sexo=="M")
x <- c(0,1,seq(5,90,5)) 
n <- c(1,4,rep(5,17),35) # o ultimo n=35 é para aceitar idades até 90+35=125

ks_masc <- purrr::map2(x,n, ~ calcula_fator_separacao(.x,.y,mort_masc)) %>%
  unlist()

nMx_masc <- nMx %>% filter(Ano==2021 & sexo=="Masculino")
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
                  caption = 'Tábua de vida para o sexo masculino em 2021 - Santa Catarina') %>% 
  kableExtra::kable_classic(full_width=FALSE,latex_options = "HOLD_position")

## Com base na TV calculada, grafique as funções lx e nqx para cada sexo e comente os resultados.

# sobrevivencia lx

df_lx <- data.frame("faixa"=unique(nMx$faixa),"Sexo"=c(rep("Masculino",20),rep("Feminino",20)),
                    "lx"=c(tabua_masculino$lx,tabua_feminino$lx))

df_lx$Sexo <- factor(df_lx$Sexo,levels = c("Masculino","Feminino"))

df_lx %>%
  ggplot(aes(x=faixa,y=lx,group=Sexo,colour=Sexo))+
  geom_line(size=1)+
  scale_colour_manual(name="Sexo",values=c("darkcyan",
                                           "darkred"))+
  labs(x="",y="lx")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")
ggsave('./img/graf_lx.png', width = 158, height = 93, units = 'mm',bg="white")

# grafico nqx

df_nqx <- data.frame("faixa"=unique(nMx$faixa),"Sexo"=c(rep("Masculino",20),rep("Feminino",20)),
                    "nqx"=c(tabua_masculino$nqx,tabua_feminino$nqx))

df_nqx$Sexo <- factor(df_nqx$Sexo,levels = c("Masculino","Feminino"))

df_nqx %>%
  ggplot(aes(x=faixa,y=nqx,group=Sexo,colour=Sexo))+
  geom_line(size=1)+
  scale_colour_manual(name="Sexo",values=c("darkcyan",
                                           "darkred"))+
  scale_y_log10()+
  labs(x="",y="nqx")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  theme(legend.position = 'top', legend.direction = "horizontal")
ggsave('./img/graf_nqx.png', width = 158, height = 93, units = 'mm',bg="white")

