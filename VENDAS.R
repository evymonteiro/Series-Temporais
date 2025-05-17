## instalar Pacotes ##

#install.packages("moments")
#install.packages("openxlsx")
#install.packages("nortest")
#install.packages("lmtest")
#install.packages("forecast")
#install.packages("ggplot2")

#############


require(moments)
require(openxlsx)
require(nortest)
require(lmtest)
require(forecast)
require(ggplot2)


rm(list=ls(all=TRUE))
dados <- "D://ARQUIVOS_POS_ESTATISTICA//Series Temporais//prim_aula_pratica_2025.xlsx"
vol_vendas <- read.xlsx(dados, sheet = "Vol vendas")
tx_desocupacao <- read.xlsx(dados, sheet = "Taxa desocup")

p_vendas=12 #frequência em que os dados foram observados

inicial_vendas=c(2000,1) ##Período em que começou a observação.

T_vendas=nrow(vol_vendas) ##Tempo

Ano_vendas=vol_vendas[,1]
M_vendas=vol_vendas[,2] #mes
y_vendas=vol_vendas[,3] #volume de vendas por ano

y_vendas=ts(y_vendas,frequency=p_vendas,start=inicial_vendas) #Transformando os dados em um arquivo
#do tipo Time Serie (ts) e adicionando os parametros definidos anteriormente.


### Questao 01 - Dataset de Vendas. 

### Análise exploratória dos dados:

autoplot(y_vendas,main="Grafico de Linha")

### Visualmente, é possivel identificar uma série não estacionária. 

###  Boxplot por ano:

boxplot(y_vendas~Ano_vendas,main="Boxplot Volume de Vendas por Ano")
### Apresenta tendência crescente entre 2000 e 2015. 

###  Boxplot por mês:
boxplot(y_vendas~M_vendas,main="Boxplot Volume de Vendas por Mês")
### Não apresenta tendência entre os meses, exceto pelo mês de Dezembro, em que
### o volume de vendas aumenta, indicando que o padrão de vendas por mês é similar 
### independente do ano observado, porém o volume mensal sofreu variação
### no período estudado. A sazonalidade expressa no mês de dezembro fica clara 
### devido ao distanciamento de sua mediana com os demais meses.


### Histograma:
hist(y_vendas, freq = FALSE, col = "lightblue", main = "Histograma", xlab = "Volume de Vendas")
curve(dnorm(x, mean = mean(y_vendas), sd = sd(y_vendas)), 
      col = "red", lwd = 2, add = TRUE)

### Estatísticas descritivas:

ed_vendas <- data.frame(
  t(as.matrix(summary(y_vendas)[c(1,2,3,4,5,6)])),
  Var = var(y_vendas),
  Sd = sd(y_vendas),
  Skewness = skewness(y_vendas),
  Kurtosis = kurtosis(y_vendas)
)
ed_vendas <- t(ed_vendas)
ed_vendas

### Decomposição da série: 

plot(decompose(y_vendas))  ##Decompõe a série em componentes:

# No gráfico de componentes é possível observar a tendência da série
# que apresentouum crescimento entre os anos 2000 a 2015, quando teve uma
# leve queda e voltou a subir nos anos subsequentes, é possível também observar
# que todo final de ano, existe um aumento no volume de vendas, justificado
# pelas datas comemorativas.  


### Modelo com tendência:

modelo1=tslm(formula = y_vendas ~ trend)
summary(modelo1)

## Modelo com tendencia e sazonalidade:
modelo2=tslm(formula = y_vendas ~ trend + season)
summary(modelo2)

### os p-values observados indicam significância para a tendência e 
### para sazonalidade no mês 12, com 99% de confiança.

## Teste de Durbin-Watson para autocorrelação dos resíduos:
dwtest(modelo2)

### H0: Não há autocorrelação entre os resíduos.
### H1: Há autocorrelação entre os resíduos.

### Com p-value =2,2e-16, H0 é rejeitada, indicando que os resíduos estão correlacionados.


### Valores Ajustados 
fits_vendas=fitted(modelo2)

### Resíduos: 
res_vendas=residuals(modelo2)
autoplot(y_vendas, series = "Vendas Observadas") +
  autolayer(fits_vendas, series = "Vendas Ajustadas", linetype = "dashed") +
  labs(title = "Vendas Observadas vs Ajustadas",
       x = "Ano", y = "Vendas",
       color = "Série", linetype = "Tipo de Linha") +
  scale_color_manual(values = c("Vendas Observadas" = "black", "Vendas Ajustadas" = "red")) +
  scale_linetype_manual(values = c("Vendas Observadas" = "solid", "Vendas Ajustadas" = "dashed")) +
  theme_classic()

### O gráfico mostra a adequação do modelo proposto, indicando uma baixa 
### adequação no período de 2010 a 2015. 

### Grafico de Linha dos Resíduos:
autoplot(res_vendas)+ 
  theme_classic()+
  labs(title = "Gráfico de Linha dos Resíduos",
       x = "Ano", y = "Resíduos")

### Grafico de Dispersão dos Resíduos:
qplot(fits_vendas, res_vendas)+
  labs(title = "Dispersão de Resíduos")+
  theme_classic()

## Histograma dos Resíduos ##
hist(res_vendas, freq = FALSE, col = "lightblue", main = "Histograma de Resíduos",
     xlab = "Resíduos", ylab = "Densidade")

## QQ-plot dos Resíduos:
qqnorm(res_vendas,
       main = "Gráfico QQ dos Resíduos",
       col = "blue",     
       pch = 1)          

qqline(res_vendas,
       col = "darkred",  
       lwd = 2)          

### O gráfico Q-Q Plot não indica normalidade dos resíduos, assim como o 
### histograma não indicou. 

### Teste de Normalidade de Anderson-Darling
ad.test(res_vendas)
### H0: Os resíduos são normais vs. 
### H1: Os resíduos não seguem distribuição normal. 

### Com p-value = 6.284e-13, H0 é rejeitada. 

## Teste de Normalidade de Shapiro-Wilk ##
shapiro.test(res_vendas)
### H0: Os resíduos são normais vs. 
### H1: Os resíduos não seguem distribuição normal. 

### Com p-value = 9.396e-8, H0 é rejeitada. 


#### Simulação de Processos:

rm(list=ls(all=TRUE))

T=600
#Proceso P1
a=rnorm(T,0,1)
plot.ts(a)
Acf(a)

hist(a)

#Proceso P2
z0=10
z=rep(0,T)

z[1]=0.7*z0 + a[1]

for (t in 2:T){
  z[t]=0.7*z[t-1] + a[t]
}

z=z[101:600]

ts.plot(z,main="Grafico de Linha")
Acf(z)
Pacf(z)
hist(z)
t(t(c(summary(z),Var=var(z),Sd=sd(z),skewness=skewness(z),kurtose=kurtosis(z))))

