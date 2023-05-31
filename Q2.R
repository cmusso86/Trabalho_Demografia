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

nascimentos <-  map_dfr(arquivos_nasc,~ read.dbc(.x))


populacao <- read_excel("Bases/populacao_Sc.xls")



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
              names_from = Ano, values_from = Pop) %>% 
  adorn_totals()


# chequei só tem SC mesmo

nascimentos_trat <- nascimentos %>% 
  select(IDADEMAE, DTNASC, SEXO) %>% 
  mutate(Ano=str_sub(DTNASC, 5, 8))

#   Taxa Bruta de Natalidade

nascimentos_tot <- nascimentos_trat %>% 
  group_by(Ano) %>% 
  count(name="nasc")

pop_grupos %>% 
  filter(Sexo=="Total")%>% 
  select("2010", "2019", "2021") %>% 
  pivot_longer(everything(), names_to = "Ano",
               values_to = "Pop") %>% 
  left_join(nascimentos_tot) %>% 
  mutate(TBN = nasc/Pop*1000)

# Taxa Fecundidade Geral (TFG) e Taxas específicas de fecundidade - nfx (Grafique esses valores)




# Taxa de Fecundidade Total (TFT) ou Índice Sintético de Fecundidade
# Taxas específicas de fecundidade feminina (apenas os nascimentos femininos)
# Taxa Bruta de Reprodução
# Taxa Líquida de Reprodução (é necessária a informação da função L da Tábua de Vida)
# b) Compare os seus resultados com os valores obtidos  pelo IBGE (projeções), e para o Brasil, pelo estudo GBD, pelas Nações Unidas (UN Population) e aqueles publicados no site do Datasus para 2010 (RIPSA - Indicadores e dados básicos - http://tabnet.datasus.gov.br/cgi/idb2012/matriz.htm ). Como  os indicadores de reprodução não aparecem nessas listas, a partir das TFT, calcule esses indicadores para comparação.
# 
# c) Comente esses resultados (inclusive os gráficos das nfx), fazendo referência a artigos já publicados sobre o assunto.
# 
# d) Para os dados do SINASC para 2021, analise a associação entre (apresente ao menos uma medida de associação):
#   
#   idade e escolaridade da mãe 
# tipo de parto e escolaridade da mãe
# Apresente graficamente os dados e obtenha medidas para essa associação.