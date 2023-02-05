##
########################### PAINEL UTILIDADE PUBLICA (UP)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: UTILIDADE PUBLICA
## QUANTIDADE DE TICKERS: 50

## AESB3	AFLT3	ALUP11 ALUP3	ALUP4	AMBP3	AURE3	CEBR3	CEBR5	CEBR6	CLSC3	CLSC4	
## CMIG3	CMIG4	COCE5	CPFE3	CPLE11	CPLE3	CPLE6	CSMG3	CSRN6	EGIE3	EKTR4	ELET3	
## ELET6	EMAE4	ENBR3	ENEV3	ENGI11	ENGI3	ENGI4	EQTL3	GEPA4	LIGT3	MEGA3	NEOE3	
## ORVR3	REDE3	RNEW11	RNEW3	RNEW4	SAPR11	SAPR3	SAPR4	SBSP3	TAEE11	TAEE3	TAEE4
## TRPL3	TRPL4


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
resumo_painelUP <- summary(painel_UP)
resumo_painelUP

#### SALVANDO SUMARIO EM EXCEL
UP <- data.frame(resumo_painelUP)
write.xlsx(UP, "sumario_UP.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsUP <- nrow(painel_UP)
r0UP <- 0.01+1.8/sqrt(obsUP)
minwUP <- floor(r0UP*obsUP) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelUP <- radf(painel_UP[1:5185,2:51], minwUP, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durUP <- psy_ds(obsUP, rule = 1, delta = 1)
min_durUP

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesUP <- radf_mc_cv(obsUP, minwUP, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcUP <- summary(radf_painelUP, mc_critical_valuesUP)
estatistica_mcUP#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_UP <- data.frame(estatistica_mcUP)
write.xlsx(estat_UP, "estat_UP.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesUP <- radf_sb_cv(painel_UP[1:5186,2:51], minwUP, lag = 1,
                                   nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbUP <- summary(radf_painelUP, sb_critical_valuesUP)
estatistica_sbUP

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_UP <- data.frame(estatistica_sbUP)
write.xlsx(boot_UP, "boot_UP.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcUP <- datestamp(radf_painelUP, mc_critical_valuesUP, 
                               min_duration = min_durUP)
carimbo_data_mcUP#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_UP <- data.frame(carimbo_data_mcUP)
write.xlsx(datas_UP, "datas_UP.xlsx")
####

## PARA O PAINEL UP
carimbo_data_sbUP <- datestamp(radf_painelUP, sb_critical_valuesUP,
                               min_duration = min_durUP)
carimbo_data_sbUP

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_UP <- data.frame(carimbo_data_sbUP)
write.xlsx(datas_painel_UP, "datas_painel_UP.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcUP <- diagnostics(radf_painelUP, mc_critical_valuesUP)
diagnostico_mcUP
#
#
## PARA O PAINEL
diagnostico_sbUP <- diagnostics(radf_painelUP, sb_critical_valuesUP)
diagnostico_sbUP

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_UP <- autoplot(radf_painelUP, mc_critical_valuesUP, 
                       min_duration = min_durUP)
figura1_UP # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_UP_ATIVO <- autoplot(radf_painelUP, mc_critical_valueUP, 
#                              min_duration = min_durUP, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaUP <- autoplot(carimbo_data_mcUP)
figura2_periodos_exuberanciaUP

## 
figura3_painel_exuberanciaUP <- autoplot(radf_painelUP, cv=sb_critical_valuesUP,
                                         min_duration = min_durUP)
figura3_painel_exuberanciaUP

############################## F I M #########################################