##
########################### PAINEL BENS INDUSTRIAIS (BI)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: BENS INDUSTRIAIS
## QUANTIDADE DE TICKERS: 41

## AERI3	ALPK3	ARML3	ATMP3	AZUL4	CCRO3	EALT4	ECOR3	EMBR3	FRAS3	GGPS3	GOLL4
## HBSA3	INEP3	INEP4	JSLG3	KEPL3	LOGN3	MILS3	MWET4	POMO3	POMO4	PORT3	PRNR3	
## RAIL3	RAPT3	RAPT4	RCSL3	RCSL4	ROMI3	SEQL3	SHUL4	STBP3	TASA3	TASA4	TGMA3	
## TPIS3	TUPY3	VLID3	WEGE3	WLMM4

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
###### DADOS (painel_BI)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelBI <- summary(painel_BI)
resumo_painelBI

#### SALVANDO SUMARIO EM EXCEL
BI <- data.frame(resumo_painelBI)
write.xlsx(BI, "sumario_BI.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsBI <- nrow(painel_BI)
r0BI <- 0.01+1.8/sqrt(obsBI)
minwBI <- floor(r0BI*obsBI) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelBI <- radf(painel_BI[1:5185,2:42], minwBI, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durBI <- psy_ds(obsBI, rule = 1, delta = 1)
min_durBI

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesBI <- radf_mc_cv(obsBI, minwBI, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcBI <- summary(radf_painelBI, mc_critical_valuesBI)
estatistica_mcBI#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_BI <- data.frame(estatistica_mcBI)
write.xlsx(estat_BI, "estat_BI.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesBI <- radf_sb_cv(painel_BI[1:5186,2:42], minwBI, lag = 1,
                                 nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbBI <- summary(radf_painelBI, sb_critical_valuesBI)
estatistica_sbBI

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_BI <- data.frame(estatistica_sbBI)
write.xlsx(boot_BI, "boot_BI.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcBI <- datestamp(radf_painelBI, mc_critical_valuesBI, 
                             min_duration = min_durBI)
carimbo_data_mcBI#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_BI <- data.frame(carimbo_data_mcBI)
write.xlsx(datas_BI, "datas_BI.xlsx")
####

## PARA O PAINEL PGB
carimbo_data_sbBI <- datestamp(radf_painelBI, sb_critical_valuesBI,
                             min_duration = min_durBI)
carimbo_data_sbBI

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_BI <- data.frame(carimbo_data_sbBI)
write.xlsx(datas_painel_BI, "datas_painel_BI.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcBI <- diagnostics(radf_painelBI, mc_critical_valuesBI)
diagnostico_mcBI
#
#
## PARA O PAINEL
diagnostico_sbBI <- diagnostics(radf_painelBI, sb_critical_valuesBI)
diagnostico_sbBI

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_BI <- autoplot(radf_painelBI, mc_critical_valuesBI, min_duration = min_durBI)
figura1_BI # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_BI_ATIVO <- autoplot(radf_painelBI, mc_critical_valuesBI, 
#                              min_duration = min_durBI, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaBI <- autoplot(carimbo_data_mcBI)
figura2_periodos_exuberanciaBI

## 
figura3_painel_exuberanciaBI <- autoplot(radf_painelBI, cv=sb_critical_valuesBI,
                                         min_duration = min_durBI)
figura3_painel_exuberanciaBI

############################## F I M #########################################