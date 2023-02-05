##
########################### PAINEL COMUNICAÇÕES (Com)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: COMUNICAÕES
## QUANTIDADE DE TICKERS: 10

## BRIT3	DESK3	ELMD3	FIQE3	OIBR3	OIBR4	TELB3	TELB4	TIMS3	VIVT3

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
###### DADOS (painel_Com)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelCom <- summary(painel_Com)
resumo_painelCom

#### SALVANDO SUMARIO EM EXCEL
Com <- data.frame(resumo_painelCom)
write.xlsx(Com, "sumario_Com.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsCom <- nrow(painel_Com)
r0Com <- 0.01+1.8/sqrt(obsCom)
minwCom <- floor(r0Com*obsCom) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelCom <- radf(painel_Com[1:5185,2:11], minwCom, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durCom <- psy_ds(obsCom, rule = 1, delta = 1)
min_durCom

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
#mc_critical_valuesCom <- radf_mc_cv(obsCom, minwCom, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcCom <- summary(radf_painelCom, mc_critical_valuesTI) #UTILISZANDO MONTE CARLO DO TI
estatistica_mcCom#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_Com <- data.frame(estatistica_mcCom)
write.xlsx(estat_Com, "estat_Com.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesCom <- radf_sb_cv(painel_Com[1:5185,2:11], minwCom, lag = 1,
                                    nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbCom <- summary(radf_painelCom, sb_critical_valuesCom)
estatistica_sbCom

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_Com <- data.frame(estatistica_sbCom)
write.xlsx(boot_Com, "boot_Com.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcCom <- datestamp(radf_painelCom, mc_critical_valuesTI, 
                                min_duration = min_durCom) #UTILIZANDO MONTE CARLO DO TI
carimbo_data_mcCom#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_Com <- data.frame(carimbo_data_mcCom)
write.xlsx(datas_Com, "datas_Com.xlsx")
####

## PARA O PAINEL Com
carimbo_data_sbCom <- datestamp(radf_painelCom, sb_critical_valuesCom,
                                min_duration = min_durCom)
carimbo_data_sbCom

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_Com <- data.frame(carimbo_data_sbCom)
write.xlsx(datas_painel_Com, "datas_painel_Com.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcCom <- diagnostics(radf_painelCom, mc_critical_valuesTI)
diagnostico_mcCom #UTILIZANDO MONTE CARLO DO TI
#
#
## PARA O PAINEL
diagnostico_sbCom <- diagnostics(radf_painelCom, sb_critical_valuesCom)
diagnostico_sbCom

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_Com <- autoplot(radf_painelCom, mc_critical_valuesTI, min_duration = min_durCom)
#UTILIZANDO MONTE CARLO DO TI
figura1_Com # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO UTILIZANDO MONTE CARLO DO TI
#figura1_Com_ATIVO <- autoplot(radf_painelCom, mc_critical_valuesTI, 
#                              min_duration = min_durCom, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaCom <- autoplot(carimbo_data_mcCom)
figura2_periodos_exuberanciaCom

## 
figura3_painel_exuberanciaCom <- autoplot(radf_painelCom, cv=sb_critical_valuesCom,
                                          min_duration = min_durCom)
figura3_painel_exuberanciaCom

############################## F I M #########################################