##
########################### PAINEL CONSUMO CICLICO (CC)
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: BENS INDUSTRIAIS
## QUANTIDADE DE TICKERS: 77

## ALLD3	ALPA4	AMAR3	AMER3	ANIM3	ARZZ3	BKBR3	BMKS3	CALI3	CAMB3	CEAB3	CEDO4
## CGRA4	COGN3	CSED3	CTNM4	CTSA3	CTSA4	CURY3	CVCB3	CYRE3	DIRR3	DOHL4	DOTZ3
## ESPA3	EVEN3	EZTC3	GFSA3	GRND3	GUAR3	HBOR3	JFEN3	JHSF3	LAVV3	LEVE3	LJQQ3
## LLIS3	LREN3	MDNE3	MEAL3	MELK3	MGLU3	MOVI3	MRVE3	MTRE3	MYPK3	PDGR3	PETZ3
## PLAS3	PLPL3	PTNT3	PTNT4	RDNI3	RENT3	RSID3	SBFG3	SEER3	SGPS3	SHOW3	SMFT3
## SOMA3	TCSA3	TECN3	TEKA4	TEND3	TFCO4	TRIS3	TXRX4	UCAS3	VAMO3	VIIA3	VIVA3
## VIVR3	VULC3	WHRL3	WHRL4	YDUQ3


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
###### DADOS (painel_CC)  IMPORTADO PELA FERRAMENTA DO R "IMPORT DATASET"
##
## SUMARIO DO PAINEL
resumo_painelCC <- summary(painel_CC)
resumo_painelCC

#### SALVANDO SUMARIO EM EXCEL
CC <- data.frame(resumo_painelCC)
write.xlsx(CC, "sumario_CC.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsCC <- nrow(painel_CC)
r0CC <- 0.01+1.8/sqrt(obsCC)
minwCC <- floor(r0CC*obsCC) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelCC <- radf(painel_CC[1:5185,2:78], minwCC, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durCC <- psy_ds(obsCC, rule = 1, delta = 1)
min_durCC

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesCC <- radf_mc_cv(obsCC, minwCC, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcCC <- summary(radf_painelCC, mc_critical_valuesCC)
estatistica_mcCC#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_CC <- data.frame(estatistica_mcCC)
write.xlsx(estat_CC, "estat_CC.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesCC <- radf_sb_cv(painel_CC[1:5185,2:78], minwCC, lag = 1,
                                   nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbCC <- summary(radf_painelCC, sb_critical_valuesCC)
estatistica_sbCC

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_CC <- data.frame(estatistica_sbCC)
write.xlsx(boot_CC, "boot_CC.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcCC <- datestamp(radf_painelCC, mc_critical_valuesCC, 
                               min_duration = min_durCC)
carimbo_data_mcCC#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_CC <- data.frame(carimbo_data_mcCC)
write.xlsx(datas_CC, "datas_CC.xlsx")
####

## PARA O PAINEL PGB
carimbo_data_sbCC <- datestamp(radf_painelCC, sb_critical_valuesCC,
                               min_duration = min_durCC)
carimbo_data_sbCC

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_CC <- data.frame(carimbo_data_sbCC)
write.xlsx(datas_painel_CC, "datas_painel_CC.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcCC <- diagnostics(radf_painelCC, mc_critical_valuesCC)
diagnostico_mcCC
#
#
## PARA O PAINEL
diagnostico_sbCC <- diagnostics(radf_painelCC, sb_critical_valuesCC)
diagnostico_sbCC

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_CC <- autoplot(radf_painelCC, mc_critical_valuesCC, min_duration = min_durCC)
figura1_CC # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_CC_ATIVO <- autoplot(radf_painelCC, mc_critical_valuesCC, 
#                              min_duration = min_durCC, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaCC <- autoplot(carimbo_data_mcCC)
figura2_periodos_exuberanciaCC

## 
figura3_painel_exuberanciaCC <- autoplot(radf_painelCC, cv=sb_critical_valuesCC,
                                         min_duration = min_durCC)
figura3_painel_exuberanciaCC

############################## F I M #########################################