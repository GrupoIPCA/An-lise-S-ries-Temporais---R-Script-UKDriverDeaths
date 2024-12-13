##########################################
### Objetivos Para o Trabalho de Grupo ###
##########################################

#– efetuar a análise exploratória ajustada à série temporal de interesse;

#- identificar potenciais problemas que interfiram na qualidade dos dados e efetuar o tratamento devido;

#– avaliar comparativamente modelos alternativos para séries temporais;

#- ajustar um modelo e respetiva validação;

#- aplicar os modelos estocásticos para previsão.


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

stldec <- stl(UKDriverDeaths, t.window = 13, s.window = "periodic", robust = TRUE) #É aditivo. A componente sazonal não varia
autoplot(stldec)

stldec1 <- stl(UKDriverDeaths, t.window = 13, s.window = 13, robust = TRUE) #É multiplicativo. A componente sazonal varia
autoplot(stldec1)

stldec2 <- mstl(UKDriverDeaths) #Função automática
autoplot(stldec2)


###################################
### Métodos Simples de Previsão ###
###################################

#Forecast - 1º Verificar melhor método de previsão retirar o último ano e prever

UKDriverDeaths_menos_1_ano <- window(UKDriverDeaths, end = 1983)

autoplot(UKDriverDeaths) +
  
  autolayer(meanf(UKDriverDeaths_menos_1_ano, h = 12), #Previsão utilizando o método média #h = 12 é a previsão para 12 meses
            series = "Média", PI = FALSE) + #PI = Prediction Intervals
  
  autolayer(naive(UKDriverDeaths_menos_1_ano, h = 12), #Previsão utilizando o método naive
            series = "Naive", PI = FALSE) +
  
  autolayer(snaive(UKDriverDeaths_menos_1_ano, h = 12), #Previsão utilizando o método naive sazonal
            series = "Naive sazonal", PI = FALSE) +
  
  autolayer(rwf(UKDriverDeaths_menos_1_ano, drift = T, h = 12), #Previsão utilizando o método drift
            series = "Drift", PI = FALSE) +
  
  ggtitle("Previsões para o nº de mortes") +
  xlab("Tempo (anos)") + ylab("Nº de Mortes") +
  guides(colour=guide_legend(title = "Previsões"))


#Forecast - 2º fazer as previsões futuras na séries

autoplot(UKDriverDeaths) +
  
  autolayer(snaive(UKDriverDeaths, h = 12), #Previsão utilizando o método naive sazonal
            series = "Naive sazonal", PI = FALSE) +
  
  ggtitle("Previsões Para as Mortes Rodoviárias na UK") +
  xlab("Tempo (anos)") + ylab("Nº de Mortes") +
  guides(colour=guide_legend(title="Previsões"))


  ##################################################
  ### Resíduos Utilizando Métodos de Previsão ###
  ##################################################

#Resíduos utilizando o método naive sazonal
res_snaive <- residuals(snaive(UKDriverDeaths))

autoplot(res_snaive) + xlab("Tempo (anos)") + ylab("Nº de Mortes") +
  ggtitle("Resíduos utilizando o método naive sazonal")

gghistogram(res_snaive + ggtitle("Histograma dos resíduos utilizando o método naive sazonal"))

    ## OU ##

checkresiduals(res_snaive)


  ##############################
  ### Intervalos de Previsao ###
  ##############################

#Intervalos de Previsão utilizando o método naive sazonal
autoplot(snaive(UKDriverDeaths)) +  #Nível de confiança padrão -> 80 e 95
  ggtitle("Intervalos de Previsão utilizando o método naive sazonal")


  ##############################################################################
  ### Previsões utilizando a decomposição STL e o método de Previsão Naive ###
  ##############################################################################

fit_stl_naive <- mstl(UKDriverDeaths)
prev_stl_naive <- forecast(fit_stl_naive, method = "naive") #apenas indicamos o metodo para previsao
autoplot(prev_stl_naive) + ylab("Nº de Mortes")


  ###################################################
  ### Previsões com stlf e os métodos de Previsão ###
  ###################################################

    ### Previsões com stlf sem e com Box-Cox e com transformação logarítmica ###
      
      ### Sem Box-Cox ###

fcast_stlf_naive <- stlf(UKDriverDeaths, method = "naive") #usa o método Naive
autoplot(fcast_stlf_naive)

fcast_stlf <- stlf(UKDriverDeaths) #usa o método padrão de alisamento exponencial padrão
autoplot(fcast_stlf)


      ### Com Box-Cox ###

fcast_stlf_naive_bc <- stlf(UKDriverDeaths, method = "naive", lambda = "auto") #utiliza o método Box-Cox
autoplot(fcast_stlf_naive_bc)

fcast3$lambda #revela o valor de lambda utilizado

fcast_stlf_bc <- stlf(UKDriverDeaths, lambda = "auto") #usa o método padrão de alisamento exponencial padrão
autoplot(fcast_stlf_bc)


      ### com transformação logarítmica ###

fcast_stlf_naive_tl <- stlf(UKDriverDeaths, method = "naive", lambda = 0) #aplica uma transformação logarítmica
autoplot(fcast_stlf_naive_tl)

fcast3$lambda #revela o valor de lambda utilizado

fcast_stlf_tl <- stlf(UKDriverDeaths, lambda = 0) #usa o método padrão de alisamento exponencial padrão com uma transformação logarítmica
autoplot(fcast_stlf_tl)


    ### Nas previsões STLF, o Alisamento Exponencial é melhor que Naive para a Nossa Série ###
    ### Comparação dos Ruídos utilizando as diferentes Transformações: Sem Box-Cox, Com Box-Cox, Com Transformação Logarítmica ###

    ##Resíduos para STLF Com Transformação Sem Box-Cox
    res_stlf <- fcast_stlf$residuals #verificar os residuos
    autoplot(res_stlf)
    
    ##Resíduos para STLF Com Transformação Com Box-Cox
    res_stlf_bc <- fcast_stlf_bc$residuals #verificar os residuos
    autoplot(res_stlf_bc)
    
    ##Resíduos para STLF Com Transformação Logarítmica
    res_stlf_tl <- fcast_stlf_tl$residuals #verificar os residuos
    autoplot(res_stlf_tl)


#Previsões utilizando o Alisamento Exponencial Simples
aliex_simples <- ses(UKDriverDeaths, h = 12) #previsão para 12 meses utilizando Alis. Exp. Simples
autoplot(aliex_simples)

summary(aliex_simples) #perimite obter alpha e l_0

accuracy(aliex_simples) #devolve todo o tipo de erros
### Estes Tipos de Erros São:
    # ME (Erro Médio)
    # RMSE (Erro Quadrático Médio)
    # MAE (Erro Absoluto Médio)
    # MPE (Erro Percentual Médio)
    # MAPE (Erro Percentual Absoluto Médio)
    # MASE (Erro Absoluto Padronizado Médio)
    # ACF1: autocorrelação do erro na defasagem 1, indicando se há padrão residual não capturado pelo modelo

aliex_simples$fitted #coluna x_(t+1) | x_t previsão em t+l = nível em t

autoplot (aliex_simples) +
  autolayer(fitted(aliex_simples), series="Fitted") +
  ylab("Nº de mortes") + xlab("Year")


#Previsões com Alisamento Exponencial Duplo: Métodos de Holt e de Holt Damped (Amortecido)
aliex_holt <- holt(UKDriverDeaths, h = 12) #previsão utilizando alisamento exponencial com tendência para 12 meses (1 ano)
aliex_holt

aliex_holt_damped_phi <- holt(mortes, damped = TRUE, phi = 0.9, h = 12) #Método de Holt amortecido
summary(aliex_holt_damped_phi)

autoplot(mortes) +
  autolayer(aliex_holt, series = "Método de Holt", PI = FALSE) +
  autolayer(aliex_holt_damped_phi, series = "Método de Holt amortecido", PI = FALSE) +
  ggtitle("Previsões") + xlab("Ano") +
  ylab("Nº de mortes") +
  guides(colour=guide_legend(title = "Previsões"))

summary(aliex_holt)
aliex_holt$fitted


#Previsões com Alisamento Exponencial Triplo: Métodos de Holt-Winters e de Holt-Winters Damped: Previsões aditivas e multiplicativas

##Holt-Winters: Aditivo e Multiplicativo
hw_adit <- hw(UKDriverDeaths, seasonal = "additive")
hw_mult <- hw(UKDriverDeaths, seasonal = "multiplicative")

autoplot(UKDriverDeaths) +
  autolayer(hw_adit, series = "Holt-Winters previsões aditivas", PI = TRUE) +
  autolayer(hw_mult, series = "Holt-Winters previsões multiplicativas", PI = TRUE) +
  xlab("Ano") + ylab("Nº de mortes") +
  ggtitle("Previsões") +
  guides(colour=guide_legend(title = "Previsões"))

summary(hw_adit)
summary(hw_mult)
accuracy(hw_adit)
accuracy(hw_mult)

#Selação do Modelo
#Critério de Informação de Akaike (AIC)
#Critério de Informação Bayesiano (BIC)
#O modelo com o menor AIC e BIC é geralmente considerado o melhor, 
#pois indica uma combinação ideal entre ajuste e simplicidade (penalizando modelos mais complexos).

# Calculando AIC e BIC para o modelo com sazonalidade multiplicativa
AIC_multiplicativo <- AIC(hw_mult$model)
BIC_multiplicativo <- BIC(hw_mult$model)

# Calculando AIC e BIC para o modelo com sazonalidade aditiva
AIC_aditivo <- AIC(hw_adit$model)
BIC_aditivo <- BIC(hw_adit$model)

# Exibindo os valores de AIC e BIC para comparação
cat("Modelo Multiplicativo: AIC =", AIC_multiplicativo, ", BIC =", BIC_multiplicativo, "\n")
cat("Modelo Aditivo: AIC =", AIC_aditivo, ", BIC =", BIC_aditivo, "\n")

# Comparando os valores
if (AIC_multiplicativo < AIC_aditivo) {
  cat("Modelo multiplicativo tem menor AIC\n")
} else {
  cat("Modelo aditivo tem menor AIC\n")
}

if (BIC_multiplicativo < BIC_aditivo) {
  cat("Modelo multiplicativo tem menor BIC\n")
} else {
  cat("Modelo aditivo tem menor BIC\n")
}


#Holt-Winters Damped: Aditivo e Multiplicativo
hw_adit_damped <- hw(UKDriverDeaths, damped = TRUE, seasonal = "additive")
hw_mult_damped <- hw(UKDriverDeaths, damped = TRUE, seasonal = "multiplicative")

autoplot(UKDriverDeaths) +
  autolayer(hw_adit_damped, series = "HW addit damped", PI = TRUE) +
  autolayer(hw_mult_damped, series = "HW multi damped", PI = TRUE) +
  xlab("Ano") + ylab("Nº de mortes") +
  ggtitle("Previsões") +
  guides(colour=guide_legend(title = "Previsões"))


# Resumo dos modelos para observar parâmetros e erros
summary(fc_multiplicativo)
summary(fc_aditivo)

# Calculando AIC e BIC para o modelo com sazonalidade multiplicativa
AIC_multiplicativo <- AIC(hw_mult_damped$model)
BIC_multiplicativo <- BIC(hw_mult_damped$model)

# Calculando AIC e BIC para o modelo com sazonalidade aditiva
AIC_aditivo <- AIC(hw_adit_damped$model)
BIC_aditivo <- BIC(hw_adit_damped$model)

# Exibindo os valores de AIC e BIC para comparação
cat("Modelo Multiplicativo: AIC =", AIC_multiplicativo, ", BIC =", BIC_multiplicativo, "\n")
cat("Modelo Aditivo: AIC =", AIC_aditivo, ", BIC =", BIC_aditivo, "\n")

# Comparando os valores
if (AIC_multiplicativo < AIC_aditivo) {
  cat("Modelo multiplicativo tem menor AIC\n")
} else {
  cat("Modelo aditivo tem menor AIC\n")
}

if (BIC_multiplicativo < BIC_aditivo) {
  cat("Modelo multiplicativo tem menor BIC\n")
} else {
  cat("Modelo aditivo tem menor BIC\n")
}


###AIC e BIC para Verificar Melhor Modelo de Previsões de Alisamento Exponenial 
###Triplo Para A Nossa Série Entre Método de Holt-Winters Multiplicativo e Método de Holt-Winters Damped Multiplicativo

# Calculando AIC e BIC para o modelo com sazonalidade multiplicativa
AIC_multiplicativo <- AIC(hw_mult$model)
BIC_multiplicativo <- BIC(hw_mult$model)

# Calculando AIC e BIC para o modelo com sazonalidade aditiva
AIC_multiplicativo_damped <- AIC(hw_mult_damped$model)
BIC_multiplicativo_damped <- BIC(hw_mult_damped$model)

# Exibindo os valores de AIC e BIC para comparação
cat("Modelo Multiplicativo: AIC =", AIC_multiplicativo, ", BIC =", BIC_multiplicativo, "\n")
cat("Modelo Multiplicativo Damped: AIC =", AIC_multiplicativo_damped, ", BIC =", BIC_multiplicativo_damped, "\n")

# Comparando os valores
if (AIC_multiplicativo < AIC_multiplicativo_damped) {
  cat("Modelo multiplicativo tem menor AIC\n")
} else {
  cat("Modelo multiplicativo damped tem menor AIC\n")
}

if (BIC_multiplicativo < BIC_multiplicativo_damped) {
  cat("Modelo multiplicativo tem menor BIC\n")
} else {
  cat("Modelo multiplicativo damped tem menor BIC\n")
}


###Conclusão Melhor Método de Previsão Para A Nossa Série: STLF com Transformação Logarítmica 
###utilizando o Método de Alisamento Exponencial VS Alisamento Exponencial Triplo Utilizando o 
###Método de Holt-Winters Damped Multiplicativo

# Calculando AIC e BIC para o modelo com sazonalidade multiplicativa
AIC_fcast_stlf_tl <- AIC(fcast_stlf_tl$model)
BIC_fcast_stlf_tl <- BIC(fcast_stlf_tl$model)

# Calculando AIC e BIC para o modelo com sazonalidade aditiva
AIC_multiplicativo_damped <- AIC(hw_mult_damped$model)
BIC_multiplicativo_damped <- BIC(hw_mult_damped$model)

# Exibindo os valores de AIC e BIC para comparação
cat("Modelo STLF Tranformação Logarítmica Alisamento Exponencial: AIC =", AIC_fcast_stlf_tl, ", BIC =", BIC_fcast_stlf_tl, "\n")
cat("Modelo Multiplicativo Damped: AIC =", AIC_multiplicativo_damped, ", BIC =", BIC_multiplicativo_damped, "\n")

# Comparando os valores
if (AIC_fcast_stlf_tl < AIC_multiplicativo_damped) {
  cat("Modelo STLF Tranformação Logarítmica Alisamento Exponencial tem menor AIC\n")
} else {
  cat("Modelo multiplicativo damped tem menor AIC\n")
}

if (BIC_fcast_stlf_tl < BIC_multiplicativo_damped) {
  cat("Modelo STLF Tranformação Logarítmica Alisamento Exponencial tem menor BIC\n")
} else {
  cat("Modelo multiplicativo damped tem menor BIC\n")
}
