##
########################### PAINEL MATERIAIS BASICOS (MB)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: MATERIAIS BASICOS
## QUANTIDADE DE TICKERS: 33

## BRAP3	BRAP4	BRKM3	BRKM5	CBAV3	CMIN3	CRPG5	CRPG6	CSNA3	DXCO3	EUCA3	EUCA4
## FESA4	FHER3	GGBR3	GGBR4	GOAU3	GOAU4	KLBN11	KLBN3	KLBN4	MTIG4	PATI3	PMAM3
## RANI3	SNSY5	SUZB3	UNIP3	UNIP6	USIM3	USIM5	VALE3	VITT3




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
###### DADOS (painel_Geral)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelMB <- summary(painel_MB)
resumo_painelMB

#### SALVANDO SUMARIO EM EXCEL
MB <- data.frame(resumo_painelMB)
write.xlsx(MB, "sumario_MB.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsMB <- nrow(painel_MB)
r0MB <- 0.01+1.8/sqrt(obsMB)
minwMB <- floor(r0MB*obsMB) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelMB <- radf(painel_MB[1:5185,2:34], minwMB, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durMB <- psy_ds(obsMB, rule = 1, delta = 1)
min_durMB

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesMB <- radf_mc_cv(obsMB, minwMB, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcMB <- summary(radf_painelMB, mc_critical_valuesMB)
estatistica_mcMB#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_MB <- data.frame(estatistica_mcMB)
write.xlsx(estat_MB, "estat_MB.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesMB <- radf_sb_cv(painel_MB[1:5185,2:34], minwMB, lag = 1,
                                      nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbMB <- summary(radf_painelMB, sb_critical_valuesMB)
estatistica_sbMB

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_MB <- data.frame(estatistica_sbMB)
write.xlsx(boot_MB, "boot_MB.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcMB <- datestamp(radf_painelMB, mc_critical_valuesMB, 
                                  min_duration = min_durMB)
carimbo_data_mcMB#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_MB <- data.frame(carimbo_data_mcMB)
write.xlsx(datas_MB, "datas_MB.xlsx")
####

## PARA O PAINEL MB
carimbo_data_sbMB <- datestamp(radf_painelMB, sb_critical_valuesMB,
                                  min_duration = min_durMB)
carimbo_data_sbMB

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_MB <- data.frame(carimbo_data_sbMB)
write.xlsx(datas_painel_MB, "datas_painel_MB.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcMB <- diagnostics(radf_painelMB, mc_critical_valuesMB)
diagnostico_mcMB
#
#
## PARA O PAINEL
diagnostico_sbMB <- diagnostics(radf_painelMB, sb_critical_valuesMB)
diagnostico_sbMB

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_MB <- autoplot(radf_painelMB, mc_critical_valuesMB, 
                          min_duration = min_durMB)
figura1_MB # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_MB_ATIVO <- autoplot(radf_painelMB, mc_critical_valuesMB, 
#                              min_duration = min_durMB, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaMB <- autoplot(carimbo_data_mcMB)
figura2_periodos_exuberanciaMB

## 
figura3_painel_exuberanciaMB <- autoplot(radf_painelMB, cv=sb_critical_valuesMB,
                                            min_duration = min_durMB)
figura3_painel_exuberanciaMB

############################## F I M #########################################