##
########################### PAINEL PETROLEO
##
## PERIODO: 23/08/2022 A 18/11/2022
## FREQUENCIA: 5 MINUTOS (INTRADAY)
## SETOR ECONOMICO: PETROLEO, GAS E BIOCOMBUSTIVEL
## QUANTIDADE DE TICKERS: 14

## CSAN3	DMMO3	ENAT3	LUPA3	OPCT3	OSXB3	PETR3	PETR4	PRIO3	RECV3	RPMG3	RRRP3
## UGPA3	VBBR3


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
resumo_painelPetroleo <- summary(painel_Petroleo)
resumo_painelPetroleo

#### SALVANDO SUMARIO EM EXCEL
Petroleo <- data.frame(resumo_painelPetroleo)
write.xlsx(Petroleo, "sumario_Petroleo.xlsx")
####

## CALCULO DO TAMANHO MINIMO DA JANELA INICIAL
obsPetroleo <- nrow(painel_Petroleo)
r0Petroleo <- 0.01+1.8/sqrt(obsPetroleo)
minwPetroleo <- floor(r0Petroleo*obsPetroleo) ## TAMANHO MINIMO DA JANELA INICIAL

## TESTE ESTATISTICO UTILIZANDO A FUNCAO radf()
radf_painelPetroleo <- radf(painel_Petroleo[1:5185,2:15], minwPetroleo, lag = 1)

## ESTIMATIVA DA DURACAO MINIMA PARA BOLHA
## psy_ds(n, rule = 1, delta = 1) ____ rule1 = delta(log(n))
min_durPetroleo <- psy_ds(obsPetroleo, rule = 1, delta = 1)
min_durPetroleo

#####
### VALORES CRITICOS

## MONTE CARLO - VALORES CRITICOS
mc_critical_valuesPetroleo <- radf_mc_cv(obsPetroleo, minwPetroleo, nrep = 2000, seed = 123)

## MOSTRANDO OS RESULTADOS DAS ESTATISTICAS COM MONTE CARLO
estatistica_mcPetroleo <- summary(radf_painelPetroleo, mc_critical_valuesPetroleo)
estatistica_mcPetroleo#[c("ATIVO", "ATIVO")]

#### SALVANDO ESTATISTICAS MONTE CARLO EM EXCEL
estat_Petroleo <- data.frame(estatistica_mcPetroleo)
write.xlsx(estat_Petroleo, "estat_Petroleo.xlsx")
####

## SIEVE BOOTSTRAP - VALORES CRITICOS PARA PAINEL
sb_critical_valuesPetroleo <- radf_sb_cv(painel_Petroleo[1:5185,2:15], minwPetroleo, lag = 1,
                                       nboot = 500, seed = 145)

## MOSTRANDO RESULTADOS DAS ESTATISTICAS COM SIEVE BOOTSTRAP
estatistica_sbPetroleo <- summary(radf_painelPetroleo, sb_critical_valuesPetroleo)
estatistica_sbPetroleo

#### SALVANDO ESTATISTICAS BOOTSTRAP EM EXCEL
boot_Petroleo <- data.frame(estatistica_sbPetroleo)
write.xlsx(boot_Petroleo, "boot_Petroleo.xlsx")

#####
### CARIMBO DAS DATAS

## PARA CADA SERIE INDIVIDUAL
carimbo_data_mcPetroleo <- datestamp(radf_painelPetroleo, mc_critical_valuesPetroleo, 
                                   min_duration = min_durPetroleo)
carimbo_data_mcPetroleo#[c("ATIVO", "ATIVO")]

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - SERIES INDIVITUAL
datas_Petroleo <- data.frame(carimbo_data_mcPetroleo)
write.xlsx(datas_Petroleo, "datas_Petroleo.xlsx")
####

## PARA O PAINEL Petroleo
carimbo_data_sbPetroleo <- datestamp(radf_painelPetroleo, sb_critical_valuesPetroleo,
                                   min_duration = min_durPetroleo)
carimbo_data_sbPetroleo

#### SALVANDO CARIMBO DAS DATAS EM EXCEL - PAINEL
datas_painel_Petroleo <- data.frame(carimbo_data_sbPetroleo)
write.xlsx(datas_painel_Petroleo, "datas_painel_Petroleo.xlsx")
####

#####
### DIAGNOSTICO DO TESTE DE HIPOTESE

## SERIES INDIVIDUAIS
diagnostico_mcPetroleo <- diagnostics(radf_painelPetroleo, mc_critical_valuesPetroleo)
diagnostico_mcPetroleo
#
#
## PARA O PAINEL
diagnostico_sbPetroleo <- diagnostics(radf_painelPetroleo, sb_critical_valuesPetroleo)
diagnostico_sbPetroleo

#####
##
## GRAFICOS SERIES INDIVIDUAIS REJEITA H0
figura1_Petroleo <- autoplot(radf_painelPetroleo, mc_critical_valuesPetroleo, 
                           min_duration = min_durPetroleo)
figura1_Petroleo # MOSTRA TODOS OS GRAFICOS COM H0 REJEITADO PARA 95% E 99%

#####
## GRAFICO PARA CADA ATIVO QUE REJEITA H0
## SABENDO QUAIS ATIVOS REJEITA H0 BASTA SUBSTITUIR "ATIVO" PELO TICKER

# ATIVO 
#figura1_Petroeleo_ATIVO <- autoplot(radf_painelPetroleo, mc_critical_valuePetroleo, 
#                              min_duration = min_durPetroleo, select_series = ("ATIVO"))


#####
## GRAFICOS

## PERIODOS DE COMPORTAMENTO EXPLOSIVO
figura2_periodos_exuberanciaPetroleo <- autoplot(carimbo_data_mcPetroleo)
figura2_periodos_exuberanciaPetroleo

## 
figura3_painel_exuberanciaPetroleo <- autoplot(radf_painelPetroleo, cv=sb_critical_valuesPetroleo,
                                             min_duration = min_durPetroleo)
figura3_painel_exuberanciaPetroleo

############################## F I M #########################################
