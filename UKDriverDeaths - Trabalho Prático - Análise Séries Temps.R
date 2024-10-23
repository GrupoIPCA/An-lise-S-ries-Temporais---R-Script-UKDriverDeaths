##########################################
### Objetivos Para o Trabalho de Grupo ###
##########################################

#– efetuar a análise exploratória ajustada à série temporal de interesse;

#- identificar potenciais problemas que interfiram na qualidade dos dados e efetuar o tratamento devido;

#– avaliar comparativamente modelos alternativos para séries temporais;

#- ajustar um modelo e respetiva validação;

#-aplicar os modelos estocásticos para previsão.


#Bibliotecas Usadas
library(trend)
library(fpp2)
library(forecast)
library(gridExtra)
library(ggplot2)
library(Kendall)

# Dados da série temporal
data()
data("UKDriverDeaths")
class("UKDriverDeaths")
dados_st <- window(UKDriverDeaths)
dados_st

#Gráfico da série temporal
autoplot(UKDriverDeaths) + ggtitle("UKDriverDeaths: Road Casualties in Great Britain 1969-84") + ylab("Nº de mortes") + xlab("Tempo (anos)")


#############################
### Medidas de Correlação ###
#############################
  
  #####################
  ### Correlogramas ###
  #####################

ggAcf(UKDriverDeaths, lag=120, xlab="Desfasamento", ylab="Correlações", main="Correlograma")
acf(UKDriverDeaths, lag=120, xlab="Desfasamento", ylab="Correlações", main="Correlograma") #Grafico apresenta tendencia e sazonalidade


  #####################################################
  ### Tendência: Teste de Tendência de Mann-Kendall ###
  #####################################################

mk.test(UKDriverDeaths)

  #Mann-Kendall trend test
  
  data:  UKDriverDeaths
  z = -6.2692, n = 192, p-value = 3.629e-10
  alternative hypothesis: true S is not equal to 0
  sample estimates:
    S          varS           tau 
  -5.582000e+03  7.925027e+05 -3.045946e-01 
  
#1º Hipóteses
  #H0: Não há tendência
  #H1: Há tendência

#2º Estatística de teste
  #Zobs = (S + 1)/σ (S < 0)
  
#3º Valor observado
  #Zobs = -6.2692
  
#4º Nível de significância
  #α = 5%

#5º Conclusão
  #Como valor-p = 3.629e-10 que é menor que α = 5% = 0,05, logo
  #rejeita-se H0 e por isso, a série tem tendência. Como S<0,
  #essa tendência é decrescente

  
  ####################
  ### Sazonalidade ###
  ####################
  
  #A série apresenta um forte padrão sazonal: alto em dezembro e baixo em fevereiro


  
#################################################################
### Homogeneidade: Teste de Variância de Mann-Whitney-Pettitt ###
#################################################################

pettitt.test(UKDriverDeaths)
  
  #Pettitt's test for single change-point detection

  data:  UKDriverDeaths
  U* = 5178, p-value = 3.029e-10
  alternative hypothesis: two.sided
  sample estimates:
  probable change point at time K 
                               72 
  
#1º Hipóteses
  #H0: Não há mudança da variância
  #H1: Há mudança da variância
  
#2º Estatística de teste
  #U1 = Soma(i)(K=1) Soma(n)(j=i+1) (Xk - Xi)
  
#3º Valor observado
  #Uobs = 5178
  
#4º Nível de significância
  #α = 5%
  
#5º Conclusão
  #Como valor-p = 3.029e-10 que é menor que α = 5% = 0,05, logo
  #rejeita-se H0 e por isso, há mudança na variância da série

                               
                               
#############################
### Decomposição clássica ###
#############################
                                 
  ##################################################################
  ### Cálculo manual dos índices sazonais - decomposição Aditiva ###
  ##################################################################

# 1º passo - obter tendência por médias móveis de ordem 4 (trimestral)
ma <- ma(UKDriverDeaths, order = 4)  # Calcular médias móveis de ordem 4
ma
lines(ma, col = "red", lwd = 2)  # Adicionando a tendência ao gráfico

# 2º passo - Subtrair os dados pela tendência X_t - T_t
aux5 <- round(UKDriverDeaths - ma, 4)  # Arredonda a 4 casas decimais
aux5

# 3º passo - Calcular a média por mês (trimestres) dos dados sem a tendência
# Garantir que o número de dados seja múltiplo de 4 para evitar erro
if (length(UKDriverDeaths) %% 4 == 0) {
  aux6 <- colMeans(matrix(aux5, ncol = 4, byrow = TRUE), na.rm = TRUE)
} else {
  stop("O número de elementos em 'dados' não é múltiplo de 4.")
}
aux6

# 4º passo - Modelo aditivo: a média dos índices sazonais deve ser próxima de 0 (não de 1)
aux7 <- mean(aux6)
aux7
#-0.1354167

# Fazer a correção subtraindo os índices sazonais pela média anterior
indice_ajustados <- aux6 - aux7
mean(indice_ajustados)
indice_ajustados

# Decomposição usando método aditivo
adit <- decompose(UKDriverDeaths, type = "additive")
adit
plot(adit)  # Plotar a decomposição



  #########################################################################
  ### Cálculo manual dos índices sazonais - decomposição Multiplicativa ###
  #########################################################################

# 1º passo - obter tendência por médias móveis de ordem 4 (trimestral)
ma <- ma(UKDriverDeaths, order = 4)  # Calcular médias móveis de ordem 4
ma
lines(ma, col = "red", lwd = 2)  # Adicionando a tendência ao gráfico

# 2º passo - Subtrair os dados pela tendência X_t / T_t
aux8 <- round(UKDriverDeaths / ma, 4)  # Arredonda a 4 casas decimais
aux8

# 3º passo - Calcular a média por mês (trimestres) dos dados sem a tendência
# Garantir que o número de dados seja múltiplo de 4 para evitar erro
if (length(UKDriverDeaths) %% 4 == 0) {
  aux9 <- colMeans(matrix(aux8, ncol = 4, byrow = TRUE), na.rm = TRUE)
} else {
  stop("O número de elementos em 'dados' não é múltiplo de 4.")
}
aux9

# 4º passo - Modelo multiplicativo: a média dos índices sazonais deve ser próxima de 1 (100%)
aux10 <- mean(aux6)
aux10
#-0.7380319

# Fazer a correção subtraindo os índices sazonais pela média anterior
indice_ajustados <- aux9 / aux10
mean(indice_ajustados)
indice_ajustados

# Decomposição usando método multiplicativo
mult <- decompose(UKDriverDeaths, type = "multiplicative")
mult
plot(mult)  # Plotar a decomposição



########################
### Decomposição STL ###
########################

stldec <- stl(UKDriverDeaths, t.window=13, s.window="periodic", robust=TRUE) #É aditivo. A componente sazonal não varia
autoplot(stldec)

stldec1 <- stl(UKDriverDeaths, t.window=13, s.window=13, robust=TRUE)#É multiplicativo. A componente sazonal varia
autoplot(stldec1)

stldec2 <- mstl(UKDriverDeaths) #Função automática
autoplot(stldec2)


###########################
### Métodos de Previsão ###
###########################

#Forecast - 1º Verificar melhor método de previsão retirar o último ano e prever

UKDriverDeaths_menos_1_ano <- window(UKDriverDeaths, start = 1992, end = c(2006,4))

autoplot(UKDriverDeaths) +
  
  autolayer(meanf(UKDriverDeaths_menos_1_ano, h = 365), #Previsão utilizando o método média #h = 365 é a previsão para 365 dias
            series = "Média", PI = FALSE) + #PI = Prediction Intervals
  
  autolayer(naive(UKDriverDeaths_menos_1_ano, h = 365), #Previsão utilizando o método naive
            series = "Naive", PI = FALSE) +
  
  autolayer(snaive(UKDriverDeaths_menos_1_ano, h = 365), #Previsão utilizando o método naive sazonal
            series = "Naive sazonal", PI = FALSE) +
  
  autolayer(rwf(UKDriverDeaths_menos_1_ano,drift=T, h = 365), #Previsão utilizando o método drift
            series = "Drift", PI = FALSE) +
  
  ggtitle("Previsões para a produção trimestral de cerveja") +
  xlab("Tempo (anos)") + ylab("Nº de Mortes") +
  guides(colour=guide_legend(title = "Previsões"))


#Forecast - 2º fazer as previsões futuras na séries

autoplot(UKDriverDeaths) +
  
  autolayer(meanf(UKDriverDeaths, h = 365), #Previsão utilizando o método média
            series = "Média", PI = FALSE) + # PI = Prediction Intervals
  
  autolayer(naive(UKDriverDeaths, h = 365), #Previsão utilizando o método naive 
            series = "Naive", PI = FALSE) +
  
  autolayer(snaive(UKDriverDeaths, h = 365), #Previsão utilizando o método naive sazonal
            series = "Naive sazonal", PI = FALSE) +
  
  autolayer(rwf(UKDriverDeaths, drift = T, h = 365), #Previsão utilizando o método drift
            series = "Drift", PI = FALSE) +
  
  ggtitle("Previsões Para as Mortes Rodoviárias na UK") +
  xlab("Tempo (anos)") + ylab("Nº de Mortes") +
  guides(colour=guide_legend(title="Previsões"))


  ##################################################
  ### Resíduos Utilizando Métodos de Previsão ###
  ##################################################

#Resíduos utilizando o método média
res_meanf <- residuals(meanf(UKDriverDeaths))
autoplot(res_meanf) + xlab("Dia") + ylab("") +
  ggtitle("Resíduos utilizando o método média")

gghistogram(res_meanf + ggtitle("Histograma dos residuos utilizando o método média"))


#Resíduos utilizando o método naive
res_naive <- residuals(naive(UKDriverDeaths))
autoplot(res_naive) + xlab("Dia") + ylab("") +
  ggtitle("Resíduos utilizando o método naive")

gghistogram(res_naive + ggtitle("Histograma dos residuos utilizando o metodo naive"))


#Resíduos utilizando o método naive sazonal
res_snaive <- residuals(snaive(UKDriverDeaths))
autoplot(res_snaive) + xlab("Dia") + ylab("") +
  ggtitle("Resíduos utilizando o método naive sazonal")

gghistogram(res_snaive + ggtitle("Histograma dos residuos utilizando o método naive sazonal"))


#Resíduos utilizando o método drift
res_rwf <- residuals(rwf(UKDriverDeaths))
autoplot(res_rwf) + xlab("Dia") + ylab("") +
  ggtitle("Resíduos utilizando o método drift")

gghistogram(res_rwf + ggtitle("Histograma dos residuos utilizando o método drift"))


checkresiduals(res)


  ##############################
  ### Intervalos de Previsao ###
  ##############################

#Intervalos de Previsão utilizando o método média
autoplot(meanf(UKDriverDeaths)) +  #Nível de confiança padrão -> level = c(80, 95)
  ggtitle("Intervalos de Previsão utilizando o método média")

#Intervalos de Previsão utilizando o método naive
autoplot(naive(UKDriverDeaths)) +  #Nível de confiança padrão -> 80 e 95
  ggtitle("Intervalos de Previsão utilizando o método naive")

#Intervalos de Previsão utilizando o método naive sazonal
autoplot(snaive(UKDriverDeaths)) +  #Nível de confiança padrão -> 80 e 95
  ggtitle("Intervalos de Previsão utilizando o método naive sazonal")

#Intervalos de Previsão utilizando o método drift
autoplot(rwf(UKDriverDeaths)) +  #Nível de confiança padrão -> 80 e 95
  ggtitle("Intervalos de Previsão utilizando o método drift")


  #################################################################################
  ### Previsoes utilizando a decomposição STL e os métodos de Previsão ###
  #################################################################################

#Previsoes utilizando a decomposição STL e o método média
fit_stl_meanf <- stl(UKDriverDeaths, t.window=13, robust = TRUE)
prev_stl_meanf <- forecast(fit_stl_meanf, method = "meanf", level = c(95)) #apenas indicamos o metodo para previsao #Nível de confiança de 95%
autoplot(prev_stl_meanf) + ylab("")

#Previsoes utilizando a decomposição STL e o método naive
fit_stl_naive <- stl(UKDriverDeaths, t.window=13, robust = TRUE)
prev_stl_naive <- forecast(fit_stl_naive, method = "naive", level = c(95)) #apenas indicamos o metodo para previsao
autoplot(prev_stl_naive) + ylab("")

#Previsoes utilizando a decomposição STL e o método naive sazonal
fit_stl_snaive <- stl(UKDriverDeaths, t.window=13, robust = TRUE)
prev_stl_snaive <- forecast(fit_stl_snaive, method = "snaive", level = c(95)) #apenas indicamos o metodo para previsao
autoplot(prev_stl_snaive) + ylab("")

#Previsoes utilizando a decomposição STL e o método drift
fit_stl_rwf <- stl(UKDriverDeaths, t.window=13, robust = TRUE)
prev_stl_rwf <- forecast(fit_stl_rwf, method = "rwf", level = c(95)) #apenas indicamos o metodo para previsao
autoplot(prev_stl_rwf) + ylab("")


  ###################################################
  ### Previsões com stlf e os métodos de Previsão ###
  ###################################################
??method
fcast_stlf_naive <- stlf(UKDriverDeaths, method = "naive", level = c(95))
autoplot(fcast)

fcast_stlf <- stlf(UKDriverDeaths, level = c(95)) #usa o método padrão de alisamento exponencial
autoplot(fcast2)

fcast3 <- stlf(UKDriverDeaths, level = c(95), lambda = "auto")
autoplot(fcast3)
fcast3$lambda

fcast4 <- stlf(elecequip, level = c(95), lambda = 0)
autoplot(fcast4, ylim = c(40,140))
