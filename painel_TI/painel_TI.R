##
########################### PAINEL TECNOLOGIA DA INFORMAÇÃO (TI)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: TECNOLOGIA DA INFORMAÇÃO
## QUANTIDADE DE TICKERS: 17

## BMOB3	CASH3	ENJU3	IFCM3	INTB3	LVTC3	LWSA3	MBLY3	MLAS3	NGRD3	NINJ3	PDTC3
## POSI3	SQIA3	TOTS3	TRAD3	WEST3

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
resumo_painelTI <- summary(painel_TI)
resumo_painelTI

#### SALVANDO SUMARIO EM EXCEL
TI <- data.frame(resumo_painelTI)
write.xlsx(TI, "sumario_TI.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsTI <- nrow(painel_TI)
r0TI <- 0.01+1.8/sqrt(obsTI)
minwTI <- floor(r0TI*obsTI) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelTI <- radf(painel_TI[1:5185,2:18], minwTI, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durTI <- psy_ds(obsTI, rule = 1, delta = 1)
min_durTI

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesTI <- radf_mc_cv(obsTI, minwTI, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcTI <- summary(radf_painelTI, mc_critical_valuesTI)
estatistica_mcTI#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_TI <- data.frame(estatistica_mcTI)
write.xlsx(estat_TI, "estat_TI.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesTI <- radf_sb_cv(painel_TI[1:5185,2:18], minwTI, lag = 1,
                                      nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbTI <- summary(radf_painelTI, sb_critical_valuesTI)
estatistica_sbTI

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_TI <- data.frame(estatistica_sbTI)
write.xlsx(boot_TI, "boot_TI.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcTI <- datestamp(radf_painelTI, mc_critical_valuesTI, 
                                  min_duration = min_durTI)
carimbo_data_mcTI#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_TI <- data.frame(carimbo_data_mcTI)
write.xlsx(datas_TI, "datas_TI.xlsx")
####

## PARA O PAINEL Petroleo
carimbo_data_sbTI <- datestamp(radf_painelTI, sb_critical_valuesTI,
                                  min_duration = min_durTI)
carimbo_data_sbTI

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_TI <- data.frame(carimbo_data_sbTI)
write.xlsx(datas_painel_TI, "datas_painel_TI.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcTI <- diagnostics(radf_painelTI, mc_critical_valuesTI)
diagnostico_mcTI
#
#
## PARA O PAINEL
diagnostico_sbTI <- diagnostics(radf_painelTI, sb_critical_valuesTI)
diagnostico_sbTI

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_TI <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                          min_duration = min_durTI)
figura1_TI # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# BMOB3
figura1_TI_BMOB3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                              min_duration = min_durTI, select_series = ("BMOB3"))
figura1_TI_BMOB3

# IFCM3
figura1_TI_IFCM3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("IFCM3"))
figura1_TI_IFCM3

# INTB3
figura1_TI_INTB3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("INTB3"))
figura1_TI_INTB3

# MBLY3
figura1_TI_MBLY3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("MBLY3"))
figura1_TI_MBLY3

# MLAS3
figura1_TI_MLAS3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("MLAS3"))
figura1_TI_MLAS3

# POSI3
figura1_TI_POSI3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("POSI3"))
figura1_TI_POSI3

# SQIA3
figura1_TI_SQIA3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("SQIA3"))
figura1_TI_SQIA3

# TOTS3
figura1_TI_TOTS3 <- autoplot(radf_painelTI, mc_critical_valuesTI, 
                             min_duration = min_durTI, select_series = ("TOTS3"))
figura1_TI_TOTS3

#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaTI <- autoplot(carimbo_data_mcTI)
figura2_periodos_exuberanciaTI

## 
figura3_painel_exuberanciaTI <- autoplot(radf_painelTI, cv=sb_critical_valuesTI,
                                            min_duration = min_durTI)
figura3_painel_exuberanciaTI

############################## F I M #########################################