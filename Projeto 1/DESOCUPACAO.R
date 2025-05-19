## instalar Pacotes ##

#install.packages("moments")
#install.packages("openxlsx")
#install.packages("nortest")
#install.packages("lmtest")
#install.packages("forecast")
#install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("rstudioapi)
#install.packages("tinytex")
#############


require(moments)
require(openxlsx)
require(nortest)
require(lmtest)
require(forecast)
require(ggplot2)
require(patchwork)
require(rstudioapi)


rm(list=ls(all=TRUE))
tx_desocupacao <- read.xlsx(rstudioapi::selectFile(), sheet = "Taxa deoscup")
print(tx_desocupacao)

p=4 #frequência em que os dados foram observados

inicial=c(2012,1) ##Período em que começou a observação.

N=nrow(tx_desocupacao) ##Quantidade de Observações

### Selecionando as colunas de interesse: 

Ano=tx_desocupacao[,1]
Trimestre=tx_desocupacao[,2]
Homens=tx_desocupacao[,3]
Mulheres=tx_desocupacao[,4]
Total=tx_desocupacao[,5] #Total de desocupação por trimestre

Homens=ts(Homens, frequency=p, start=inicial)
Mulheres=ts(Mulheres, frequency = p, start = inicial)
Total=ts(Total, frequency = p, start=inicial)


### Questao 01 - Dataset de desocupação: 

### Análise exploratória dos dados:

##Criando gráficos: 
g1 <- autoplot(Homens) + ggtitle("Homens")
g2 <- autoplot(Mulheres) + ggtitle("Mulheres")
g3 <- autoplot(Total) + ggtitle("Total")
(g1 | g2) / g3

### Visualmente é possível identificar que as séries não são estacionárias, o
### comportamento da série para homens e mulheres é similar. 

###  Boxplot por ano:

par(mfrow = c(1, 1))

boxplot(Total~Ano,main="Boxplot Total de Desocupação x Ano")
boxplot(Homens~Ano,main="Boxplot Desocupação Homens x Ano")
boxplot(Mulheres~Ano,main="Boxplot Desocupação Mulheres x Ano")

### Não possui alinhamento com a mediana, apresenta crescimento entre 2015 e 2023, maior
### dispersão dos dados se encontra no ano de 2021. 

###  Boxplot por trimestre:
boxplot(Total~Trimestre,main="Boxplot Total de Desocupação x Trimestre")
boxplot(Homens~Trimestre,main="Boxplot Desocupação Homens x Trimestre")
boxplot(Mulheres~Trimestre,main="Boxplot Desocupação Mulheres x Trimestre")

### Valores de mediana praticamente alinhados, não indicando sazonalidade
### entre os trimestres, porem também indica valores dispersos. 

### Histogramas:
par(mfrow = c(2, 2))
hist(Homens, freq = FALSE, col = "lightblue", xlab = "Homens")
hist(Mulheres, freq = FALSE, col = "lightpink", xlab = "Mulheres")
hist(Total, freq = FALSE, col = "lightgray", xlab = "Total")
par(mfrow = c(1, 1))


### Estatísticas descritivas Total de Desocupações:

ed_desocup <- data.frame(
  t(as.matrix(summary(Total)[c(1,2,3,4,5,6)])),
  Var = var(Total),
  Sd = sd(Total),
  Skewness = skewness(Total),
  Kurtosis = kurtosis(Total)
)
ed_desocup <- t(ed_desocup)

ed_Homens <- data.frame(
  t(as.matrix(summary(Homens)[c(1,2,3,4,5,6)])),
  Var = var(Homens),
  Sd = sd(Homens),
  Skewness = skewness(Homens),
  Kurtosis = kurtosis(Homens)
)
ed_Homens <- t(ed_Homens)


ed_Mulheres <- data.frame(
  t(as.matrix(summary(Mulheres)[c(1,2,3,4,5,6)])),
  Var = var(Mulheres),
  Sd = sd(Mulheres),
  Skewness = skewness(Mulheres),
  Kurtosis = kurtosis(Mulheres)
)
ed_Mulheres <- t(ed_Mulheres)


### Dataframe unificado:
estatisticas_df <- data.frame(
  Estatística = rownames(ed_Homens),
  Homens = ed_Homens[,1],
  Mulheres = ed_Mulheres[,1],
  Total = ed_desocup[,1],
  row.names = NULL
)

print(estatisticas_df)

### Decomposição da série (Somente do valor total): 

plot(decompose(Total))  ##Decompõe a série em componentes:

# No gráfico de componentes é possível observar a tendência da série
# que apresentouum crescimento entre os anos 2015 a 2017, onde começou
# a cair, e voltou a subir a partir de 2020. 


### Modelo com tendência:

modelo1=tslm(formula = Total ~ trend)
summary(modelo1)

## Modelo com tendencia e sazonalidade:
modelo2=tslm(formula = Total ~ trend + season)
summary(modelo2)

### os p-values observados indicam significância para a tendência e 
### não possui significância para sazonalidade, sendo assim, o modelo
### mais parcimonioso é o modelo 1.

## Teste de Durbin-Watson para autocorrelação dos resíduos:
dwtest(modelo1)

### H0: Não há autocorrelação entre os resíduos.
### H1: Há autocorrelação entre os resíduos.

### Com p-value =2,2e-16, H0 é rejeitada, indicando que os resíduos estão correlacionados.


### Valores Ajustados 
fits=fitted(modelo1)
plot(fits)

### Resíduos: 

autoplot(Total, series = "Observado") +
  autolayer(fitted(modelo1), series = "Ajustado", color = "blue") +
  labs(title = "Série Total vs Ajuste do Modelo 1",
       x = "Ano", y = "Total de Desocupação") +
  scale_color_manual(values = c("Observado" = "black", "Ajustado" = "blue")) +
  theme_minimal()

autoplot(Total, series = "Observado") +
  autolayer(fitted(modelo2), series = "Ajustado", color = "blue") +
  labs(title = "Série Total vs Ajuste do Modelo 2",
       x = "Ano", y = "Total de Desocupação") +
  scale_color_manual(values = c("Observado" = "black", "Ajustado" = "blue")) +
  theme_minimal()


### O gráfico mostra uma reta, ou seja, o modelo 1 não se adequou a série
### entretanto, o modelo 2, ainda que a componente sazonal não tenha sido 
### significativa, conseguiu contornar melhor o conjunto de dados.  

### Grafico de Linha dos Resíduos:
autoplot(res)+ 
  theme_classic()+
  labs(title = "Gráfico de Linha dos Resíduos",
       x = "Ano", y = "Resíduos")

### Grafico de Dispersão dos Resíduos:
qplot(fits, res)+
  labs(title = "Dispersão de Resíduos")+
  theme_classic()

## Histograma dos Resíduos
hist(res, freq = FALSE, col = "lightblue", main = "Histograma de Resíduos",
     xlab = "Resíduos", ylab = "Densidade")

## QQ-plot dos Resíduos:
qqnorm(res,
       main = "Gráfico QQ dos Resíduos",
       col = "blue",     
       pch = 1)          

qqline(res,
       col = "darkred",  
       lwd = 2)          

### O gráfico Q-Q Plot indica normalidade dos resíduos, assim como o 
### histograma. 

### Teste de Normalidade de Anderson-Darling
ad.test(res)
### H0: Os resíduos são normais vs. 
### H1: Os resíduos não seguem distribuição normal. 

### Com p-value = 0,41, H0 não é rejeitada. 

## Teste de Normalidade de Shapiro-Wilk ##
shapiro.test(res)
### H0: Os resíduos são normais vs. 
### H1: Os resíduos não seguem distribuição normal. 

### Com p-value = 0,2941, H0 não é rejeitada. 


#### Simulação de Processos:

rm(list=ls(all=TRUE))

T=500
#Proceso P1
a=rnorm(T,0,1)
plot.ts(a)
###Estacionaria em torno da média 0. 
Acf(a)

hist(a)

#Proceso P2
z0=10
z=rep(0,T)

z[1]=0.7*z0 + a[1]

for (t in 2:T){
  z[t]=0.7*z[t-1] + a[t]
}

z=z[101:500]

ts.plot(z,main="Grafico de Linha")
Acf(z)
Pacf(z)
hist(z)
t(t(c(summary(z),Var=var(z),Sd=sd(z),skewness=skewness(z),kurtose=kurtosis(z))))
