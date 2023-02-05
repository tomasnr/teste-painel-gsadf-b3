##
########################### PAINEL SAUDE
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: SAUDE
## QUANTIDADE DE TICKERS: 21

## AALR3	BIOM3	BLAU3	DASA3	DMVF3	FLRY3	HAPV3	HYPE3	KRSA3	MATD3	ODPV3	OFSA3
## ONCO3	PARD3	PFRM3	PGMN3	PNVL3	QUAL3	RADL3	RDOR3	VVEO3

##########################################################
#
## CARREGANDO PACOTES

library("exuber")
library("exuberdata")
library("ggplot2")
library("patchwork")
library("tidyr")
library("plm")
library("xlsx")

## 
###### DADOS (painel_outros)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelSaude <- summary(painel_Saude)
resumo_painelSaude

#### SALVANDO SUMARIO EM EXCEL
Saude <- data.frame(resumo_painelSaude)
write.xlsx(Saude, "sumario_Saude.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsSaude <- nrow(painel_Saude)
r0Saude <- 0.01+1.8/sqrt(obsSaude)
minwSaude <- floor(r0Saude*obsSaude) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelSaude <- radf(painel_Saude[1:5185,2:22], minwSaude, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durSaude <- psy_ds(obsSaude, rule = 1, delta = 1)
min_durSaude

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesSaude <- radf_mc_cv(obsSaude, minwSaude, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcSaude <- summary(radf_painelSaude, mc_critical_valuesSaude)
estatistica_mcSaude#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_Saude <- data.frame(estatistica_mcSaude)
write.xlsx(estat_Saude, "estat_Saude.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesSaude <- radf_sb_cv(painel_Saude[1:5185,2:22], minwSaude, lag = 1,
                                         nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbSaude <- summary(radf_painelSaude, sb_critical_valuesSaude)
estatistica_sbSaude

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_Saude <- data.frame(estatistica_sbSaude)
write.xlsx(boot_Saude, "boot_Saude.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcSaude <- datestamp(radf_painelSaude, mc_critical_valuesSaude, 
                                     min_duration = min_durSaude)
carimbo_data_mcSaude#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_Saude <- data.frame(carimbo_data_mcSaude)
write.xlsx(datas_Saude, "datas_Saude.xlsx")
####

## PARA O PAINEL Petroleo
carimbo_data_sbSaude <- datestamp(radf_painelSaude, sb_critical_valuesSaude,
                                     min_duration = min_durSaude)
carimbo_data_sbSaude

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_Saude <- data.frame(carimbo_data_sbSaude)
write.xlsx(datas_painel_Saude, "datas_painel_Saude.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcSaude <- diagnostics(radf_painelSaude, mc_critical_valuesSaude)
diagnostico_mcSaude
#
#
## PARA O PAINEL
diagnostico_sbSaude <- diagnostics(radf_painelSaude, sb_critical_valuesSaude)
diagnostico_sbSaude

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_Saude <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                              min_duration = min_durSaude)
figura1_Saude # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# AALR3 E BLAU3
figura1_Saude_AALR3BLAU3 <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                              min_duration = min_durSaude, select_series = c("AALR3","BLAU3"))
figura1_Saude_AALR3BLAU3

# HAPV3 E MATD3
figura1_Saude_HAPV3MATD3 <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                                     min_duration = min_durSaude, select_series = c("HAPV3","MATD3"))
figura1_Saude_HAPV3MATD3

# OFSA3 E ONCO3
figura1_Saude_OFSA3ONCO3 <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                                     min_duration = min_durSaude, select_series = c("OFSA3","ONCO3"))
figura1_Saude_OFSA3ONCO3

# HYPE3 E QUAL3
figura1_Saude_HYPE3QUAL3 <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                                     min_duration = min_durSaude, select_series = c("HYPE3","QUAL3"))
figura1_Saude_HYPE3QUAL3

# RADL3 E RDOR3
figura1_Saude_RADL3RDOR3 <- autoplot(radf_painelSaude, mc_critical_valuesSaude, 
                                     min_duration = min_durSaude, select_series = c("RADL3","RDOR3"))
figura1_Saude_RADL3RDOR3


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaSaude <- autoplot(carimbo_data_mcSaude)
figura2_periodos_exuberanciaSaude

## 
figura3_painel_exuberanciaSaude <- autoplot(radf_painelSaude, cv=sb_critical_valuesSaude,
                                               min_duration = min_durSaude)
figura3_painel_exuberanciaSaude


############################## F I M #########################################
