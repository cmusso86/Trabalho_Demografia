# 
# 
# Questão 2: Natalidade/Fecundidade 
# 
# a) Com base nos dados do SINASC para os de 2010, 2019 e  
# 2021 e na população por sexo e idade estimada (projetada), construa os seguintes indicadores para a Unidade da Federação:
#   

pacman::p_load(tidyverse, LexisPlotR, readxl, janitor)
pacman::p_load_gh("danicat/read.dbc")

# Dados -------

arquivos_nasc <- str_c("Bases/", 
                  list.files(path="Bases/", pattern="^DNS"))

nascimentos <-  map_dfr(arquivos_nasc,~ read.dbc(.x)) %>% 
  mutate_all(~as.character(.))

obitos <- readRDS("Bases/dados_SIM.rds")


populacao <- read_excel("Bases/populacao_Sc.xls")


ibge_indicadores <- read_excel("Bases/indicadores_IBGE_SC.xlsx")
# Taxa Bruta de Natalidade

pop_grupos<- populacao %>% 
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


# chequei só tem SC mesmo

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
  

#   Taxa Bruta de Natalidade

nascimentos_tot <- nascimentos_trat %>% 
  group_by(Ano) %>% 
  count(name="nasc")

TBN <-pop_grupos %>% 
  adorn_totals() %>% 
  filter(Sexo=="Total")%>% 
  select("2010", "2019", "2021") %>% 
  pivot_longer(everything(), names_to = "Ano",
               values_to = "Pop") %>% 
  left_join(nascimentos_tot) %>% 
  mutate(TBN = nasc/Pop*1000)

TBN

# Taxa Fecundidade Geral (TFG) e Taxas específicas de fecundidade - nfx (Grafique esses valores)

pop_fem_fertil <- pop_grupos %>% 
  filter(Sexo=="F", 
         faixa %in% c("15-19", "20-24", "25-29",
                      "30-34", "35-39", "40-44",
                      "45-49")) 

TFG <- pop_fem_fertil %>% 
  adorn_totals() %>% 
  filter(Sexo=="Total") %>% 
  select("2010", "2019", "2021") %>% 
  pivot_longer(everything(), names_to = "Ano",
               values_to = "Pop_fem") %>% 
  left_join(nascimentos_tot) %>% 
  mutate(TFG = nasc/Pop_fem*1000)

TFG



pop_fem_fertil_faixa <- pop_fem_fertil %>% 
  pivot_longer(c("2010", "2019", "2021"),
               names_to = "Ano", values_to = "Pop") 
 
nfx <- nascimentos_trat %>% 
  group_by(Ano, faixa) %>% 
  count(name = "nasc") %>% 
  right_join(pop_fem_fertil_faixa) %>% 
  mutate(nfx = nasc/Pop) %>% 
  select(Ano, faixa, nfx) %>% 
  pivot_wider(id_cols = faixa, 
              names_from = Ano, 
              values_from = nfx) 




# Taxa de Fecundidade Total (TFT) ou Índice Sintético de Fecundidade

TFT <- nfx %>% 
  pivot_longer(c("2010", "2019", "2021"),
               names_to = "Ano", values_to = "nfx") %>% 
  mutate(nfx_5=5*nfx) %>% 
  select(-nfx) %>% 
  pivot_wider(id_cols = faixa, 
              names_from = Ano, 
              values_from = nfx_5) %>% 
  adorn_totals(name="TFT") %>% 
  filter(faixa=="TFT") %>% 
  pivot_longer(-faixa, names_to =  "Ano", values_to = "TFT") %>% 
  select(-faixa)

TFT

# Taxas específicas de fecundidade feminina (apenas os nascimentos femininos)

nffx <- nascimentos_trat %>% 
  filter(SEXO==2) %>% 
  group_by(Ano, faixa) %>% 
  count(name = "nasc") %>% 
  right_join(pop_fem_fertil_faixa) %>% 
  mutate(nffx = nasc/Pop) %>% 
  select(Ano, faixa, nffx) %>% 
  pivot_wider(id_cols = faixa, 
              names_from = Ano, 
              values_from = nffx) 


# Taxa Bruta de Reprodução

TBR <- nffx %>% 
  pivot_longer(c("2010", "2019", "2021"),
               names_to = "Ano", values_to = "nffx") %>% 
  mutate(nffx_5=5*nffx) %>% 
  select(-nffx) %>% 
  pivot_wider(id_cols = faixa, 
              names_from = Ano, 
              values_from = nffx_5) %>% 
  adorn_totals(name="TBR") %>% 
  filter(faixa=="TBR")

TBR


# Taxa Líquida de Reprodução (é necessária a informação da função L da Tábua de Vida)


obitos_trat <- obitos %>% 
  select(DTNASC, SEXO, IDADE, DTOBITO) %>% 
  mutate_all(~as.character(.)) %>% 
  mutate(DTOBITO=dmy(DTOBITO),
         )

# b) Compare os seus resultados com os valores obtidos  pelo IBGE (projeções), e para o Brasil, pelo estudo GBD, 
# pelas Nações Unidas (UN Population) e aqueles publicados no site do Datasus para 2010 (RIPSA - Indicadores e dados básicos - http://tabnet.datasus.gov.br/cgi/idb2012/matriz.htm ). 
# Como  os indicadores de reprodução não aparecem nessas listas, a partir das TFT, calcule esses indicadores para comparação.


ibge_indicadores_menor<- ibge_indicadores %>% 
  filter(Ano %in% c( 2010, 2019, 2021))


# 
# TBN

TBN_ibge <- ibge_indicadores_menor %>% 
  select(Ano, TBN) %>% 
  mutate(Ano=as.character(Ano)) %>% 
  rename(TBN_ibge=TBN) %>% 
  left_join(TBN) %>% 
  select(-Pop, -nasc)

# - A taxa Bruta de natalidade segundo a 2010 (RIPSA - Indicadores e dados básicos 
#- http://tabnet.datasus.gov.br/cgi/idb2012/matriz.htm )foi de 13.7 para 2010. 
# # 
# TEF 
#


nfx_IBGE <- ibge_indicadores_menor %>% 
  select(Ano, "15-19":"45-49" ) %>% 
  pivot_longer(-Ano, names_to = "faixa", values_to = "nfx") %>% 
  pivot_wider(id_cols = faixa, names_from = Ano, values_from = nfx) %>% 
  rename(IBGE_2019=`2019`,
         IBGE_2010=`2010`,
         IBGE_2021=`2021`) %>% 
  left_join(nfx)

# - segundo RIPSA a taxa específica de fecundidade para 2010
# 	0,05088	0,07962	0,08163	0,06592	0,03475	0,00865	0,00055
# 
# 	
# TFT 


TFT_ibge <- ibge_indicadores_menor %>% 
  select(Ano, TFT) %>% 
  mutate(Ano=as.character(Ano)) %>% 
  rename(TFT_ibge=TFT) %>% 
  left_join(TFT) 

TFT_ibge
# - segundo RIPSA a taxa  de fecundidade total para 2010 foi de 1,61
# - segundo UN https://data.un.org/en/iso/br.html TFT foi de 1.9 para 2010, 1.8 para 2015, 1.7 para 2021
# segundo GDB foi de 1.4 para 2019 https://www.healthdata.org/brazil-santa-catarina


#TBR 
# rpsa_2010

TBR_ripsa_2010 <- 1.61/2.05 # dado do site
TBR_UN_2010 <- 1.9/2.05
TBR_UN_2021 <- 1.7/2.05
TBR_GDB_2021 <- 1.4/2.05

# c) Comente esses resultados (inclusive os gráficos das nfx), fazendo referência a artigos já publicados sobre o assunto.

nfx %>% 
  pivot_longer(-faixa, names_to = "Ano", values_to = "nfx") %>% 
  mutate(faixa=as.numeric(str_sub(faixa, 1, 2))) %>% 
  ggplot(aes(x=faixa, color=Ano))+
  geom_path(aes(y=nfx),)+
  geom_point(aes(y=nfx))
  theme_classic()
# 
# d) Para os dados do SINASC para 2021, analise a associação entre (apresente ao menos uma medida de associação):
#   
#   idade e escolaridade da mãe 
# tipo de parto e escolaridade da mãe
# Apresente graficamente os dados e obtenha medidas para essa associação.
