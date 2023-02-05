##
########################### PAINEL FINANCEIRO (Fin)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: FINANCEIRO
## QUANTIDADE DE TICKERS: 56

## ABCB4	ALSO3	B3SA3	BAZA3	BBAS3	BBDC3	BBDC4	BBSE3	BEES3	BEES4	BMEB4	BMGB4
## BOAS3	BPAC11	BPAC3	BPAC5	BPAN4	BRBI11	BRML3	BRPR3	BRSR3	BRSR6	BSLI3
## BSLI4	CIEL3	CLSA3	CXSE3	G2DI33	GETT11	GETT3	GETT4	HBRE3	IGTI11	IGTI3
## IRBR3	ITSA3	ITSA4	ITUB3	ITUB4	LOGG3	LPSB3	MODL3	MULT3	NEXP3	PINE4	PSSA3
## SANB11	SANB3	SANB4	SCAR3	SIMH3	SULA11	SULA3	SULA4	SYNE3	WIZS3


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
###### DADOS (painel_Fin)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelFin <- summary(painel_Fin)
resumo_painelFin

#### SALVANDO SUMARIO EM EXCEL
Fin <- data.frame(resumo_painelFin)
write.xlsx(Fin, "sumario_Fin.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsFin <- nrow(painel_Fin)
r0Fin <- 0.01+1.8/sqrt(obsFin)
minwFin <- floor(r0Fin*obsFin) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelFin <- radf(painel_Fin[1:5185,2:57], minwFin, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durFin <- psy_ds(obsFin, rule = 1, delta = 1)
min_durFin

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesFin <- radf_mc_cv(obsFin, minwFin, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcFin <- summary(radf_painelFin, mc_critical_valuesFin)
estatistica_mcFin#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_Fin <- data.frame(estatistica_mcFin)
write.xlsx(estat_Fin, "estat_Fin.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesFin <- radf_sb_cv(painel_Fin[1:5185,2:57], minwFin, lag = 1,
                                    nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbFin <- summary(radf_painelFin, sb_critical_valuesFin)
estatistica_sbFin

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_Fin <- data.frame(estatistica_sbFin)
write.xlsx(boot_Fin, "boot_Fin.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcFin <- datestamp(radf_painelFin, mc_critical_valuesFin, 
                                min_duration = min_durFin)
carimbo_data_mcFin#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_Fin <- data.frame(carimbo_data_mcFin)
write.xlsx(datas_Fin, "datas_Fin.xlsx")
####

## PARA O PAINEL Fin
carimbo_data_sbFin <- datestamp(radf_painelFin, sb_critical_valuesFin,
                                min_duration = min_durFin)
carimbo_data_sbFin

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_Fin <- data.frame(carimbo_data_sbFin)
write.xlsx(datas_painel_Fin, "datas_painel_Fin.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcFin <- diagnostics(radf_painelFin, mc_critical_valuesFin)
diagnostico_mcFin
#
#
## PARA O PAINEL
diagnostico_sbFin <- diagnostics(radf_painelFin, sb_critical_valuesFin)
diagnostico_sbFin

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_Fin <- autoplot(radf_painelFin, mc_critical_valuesFin, min_duration = min_durFin)
figura1_Fin # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_Fin_ATIVO <- autoplot(radf_painelFin, mc_critical_valuesFin, 
#                              min_duration = min_durFin, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaFin <- autoplot(carimbo_data_mcFin)
figura2_periodos_exuberanciaFin

## 
figura3_painel_exuberanciaFin <- autoplot(radf_painelFin, cv=sb_critical_valuesFin,
                                          min_duration = min_durFin)
figura3_painel_exuberanciaFin

############################## F I M #########################################