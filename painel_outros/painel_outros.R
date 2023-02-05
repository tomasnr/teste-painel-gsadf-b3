##
########################### PAINEL OUTROS 
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: OUTROS
## QUANTIDADE DE TICKERS: 3

## ATOM3	BLUT3	BLUT4

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
resumo_paineloutros <- summary(painel_outros)
resumo_paineloutros

#### SALVANDO SUMARIO EM EXCEL
outros <- data.frame(resumo_paineloutros)
write.xlsx(outros, "sumario_outros.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsoutros <- nrow(painel_outros)
r0outros <- 0.01+1.8/sqrt(obsoutros)
minwoutros <- floor(r0outros*obsoutros) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_paineloutros <- radf(painel_outros[1:5185,2:4], minwoutros, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_duroutros <- psy_ds(obsoutros, rule = 1, delta = 1)
min_duroutros

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
#mc_critical_valuesoutros <- radf_mc_cv(obsoutros, minwoutros, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcoutros <- summary(radf_paineloutros, mc_critical_valuesoutros)
estatistica_mcoutros#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_outros <- data.frame(estatistica_mcoutros)
write.xlsx(estat_outros, "estat_outros.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesoutros <- radf_sb_cv(painel_outros[1:5185,2:4], minwoutros, lag = 1,
                                   nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sboutros <- summary(radf_paineloutros, sb_critical_valuesoutros)
estatistica_sboutros

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_outros <- data.frame(estatistica_sboutros)
write.xlsx(boot_outros, "boot_outros.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcoutros <- datestamp(radf_paineloutros, mc_critical_valuesoutros, 
                               min_duration = min_duroutros)
carimbo_data_mcoutros#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_outros <- data.frame(carimbo_data_mcoutros)
write.xlsx(datas_outros, "datas_outros.xlsx")
####

## PARA O PAINEL outros
carimbo_data_sboutros <- datestamp(radf_paineloutros, sb_critical_valuesoutros,
                               min_duration = min_duroutros)
carimbo_data_sboutros

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_outros <- data.frame(carimbo_data_sboutros)
write.xlsx(datas_painel_outros, "datas_painel_outros.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcoutros <- diagnostics(radf_paineloutros, mc_critical_valuesoutros)
diagnostico_mcoutros
#
#
## PARA O PAINEL
diagnostico_sboutros <- diagnostics(radf_paineloutros, sb_critical_valuesoutros)
diagnostico_sboutros

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_outros <- autoplot(radf_paineloutros, mc_critical_valuesoutros, 
                       min_duration = min_duroutros)
figura1_outros # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_outros_ATIVO <- autoplot(radf_paineloutros, mc_critical_valuesoutros, 
#                              min_duration = min_duroutros, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaoutros <- autoplot(carimbo_data_mcoutros)
figura2_periodos_exuberanciaoutros

## 
figura3_painel_exuberanciaoutros <- autoplot(radf_paineloutros, cv=sb_critical_valuesoutros,
                                         min_duration = min_duroutros)
figura3_painel_exuberanciaoutros

############################## F I M #########################################