##
########################### PAINEL CONSUMO NÃO CICLICO (CnC)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: CONSUMO NÃO CICLICO
## QUANTIDADE DE TICKERS: 23

## ABEV3	AGRO3	AGXY3	ASAI3	BEEF3	BOBR4	BRFS3	CAML3	CRFB3	GMAT3	JALL3	JBSS3
## LAND3	MDIA3	MNPR3	MRFG3	NTCO3	PCAR3	RAIZ4	SLCE3	SMTO3	SOJA3	TTEN3



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
###### DADOS (painel_CnC)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelCnC <- summary(painel_CnC)
resumo_painelCnC

#### SALVANDO SUMARIO EM EXCEL
CnC <- data.frame(resumo_painelCnC)
write.xlsx(CnC, "sumario_CnC.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsCnC <- nrow(painel_CnC)
r0CnC <- 0.01+1.8/sqrt(obsCnC)
minwCnC <- floor(r0CnC*obsCnC) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelCnC <- radf(painel_CnC[1:5186,2:24], minwCnC, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durCnC <- psy_ds(obsCnC, rule = 1, delta = 1)
min_durCnC

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesCnC <- radf_mc_cv(obsCnC, minwCnC, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcCnC <- summary(radf_painelCnC, mc_critical_valuesCnC)
estatistica_mcCnC#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_CnC <- data.frame(estatistica_mcCnC)
write.xlsx(estat_CnC, "estat_CnC.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesCnC <- radf_sb_cv(painel_CnC[1:5186,2:23], minwCnC, lag = 1,
                                   nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbCnC <- summary(radf_painelCnC, sb_critical_valuesCnC)
estatistica_sbCnC

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_CnC <- data.frame(estatistica_sbCnC)
write.xlsx(boot_CnC, "boot_CnC.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcCnC <- datestamp(radf_painelCnC, mc_critical_valuesCnC, 
                               min_duration = min_durCnC)
carimbo_data_mcCnC#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_CnC <- data.frame(carimbo_data_mcCnC)
write.xlsx(datas_CnC, "datas_CnC.xlsx")
####

## PARA O PAINEL PGB
carimbo_data_sbCnC <- datestamp(radf_painelCnC, sb_critical_valuesCnC,
                               min_duration = min_durCnC)
carimbo_data_sbCnC

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_CnC <- data.frame(carimbo_data_sbCnC)
write.xlsx(datas_painel_CnC, "datas_painel_CnC.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcCnC <- diagnostics(radf_painelCnC, mc_critical_valuesCnC)
diagnostico_mcCnC
#
#
## PARA O PAINEL
diagnostico_sbCnC <- diagnostics(radf_painelCnC, sb_critical_valuesCnC)
diagnostico_sbCnC

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_CnC <- autoplot(radf_painelCnC, mc_critical_valuesCnC, min_duration = min_durCnC)
figura1_CnC # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_CnC_ATIVO <- autoplot(radf_painelCnC, mc_critical_valuesCnC, 
#                              min_duration = min_durCnC, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaCnC <- autoplot(carimbo_data_mcCnC)
figura2_periodos_exuberanciaCnC

## 
figura3_painel_exuberanciaCnC <- autoplot(radf_painelCnC, cv=sb_critical_valuesCnC,
                                         min_duration = min_durCnC)
figura3_painel_exuberanciaCnC

############################## F I M #########################################