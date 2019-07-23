library(ggplot2)
library(dplyr)
library(data.table)

#----------------------------------------------------------------------
vendedores <- fread("dados/vendas.csv")

# filtrando apenas por linhas com valores
vendedores <- filter(vendedores, !is.na(vendedores$testes))

# plotando o gráfico
ggplot(data = vendedores, aes(x=testes, y=vendas)) + 
  geom_point() +
  geom_smooth(method="lm")

# gerando a regressão linear
modelo1 <- lm(vendedores, formula=vendas~testes)
summary(modelo1)

#----------------------------------------------------------------------
tvb <- fread("dados/tvb.csv")

# pegando as reclamações com a temperatura do dia anterior
tvbanalise <- data.table(tvb[1:39,2], tvb[2:40,3])
# filtrando as temperaturas maiores ou iguais a 30ºC
tvbanalise <- filter(tvbanalise, tvbanalise$`temperatura (Celsius)` >= 30)

# plotando o gráfico
ggplot(data = tvbanalise, aes(x=tvbanalise$`temperatura (Celsius)`, y=tvbanalise$`reclama‡äes`))+
  geom_point() +
  geom_smooth(method="lm")

# gerando a regressão linear
modelo2 <- lm(tvbanalise, formula=tvbanalise$`reclama‡äes` ~ tvbanalise$`temperatura (Celsius)`)
summary(modelo2)






















