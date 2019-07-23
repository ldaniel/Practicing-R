#---------------------------------------------------------------------------------------------
# Gerando números randômicos

# de 1 a 10, pegando um número aleatoriamente
sample(1:10, 1)

# de 1 a 10, pegando um número aleatoriamente sem repetir
sample(1:10, 1, replace=F) 

# de 5 a 7.5 pegando um número aleatoriamente
runif(1, 5.0, 7.5)

#---------------------------------------------------------------------------------------------
# Calculando Desvio Padrão

valores <- array(1:10)

# aplicando diretamente o cálculo
calculoDesvioPadrao <- sqrt(sum((valores - mean(valores)) ^ 2)/(length(valores)-1))

# criando uma função
desvioPadrao <- function(valores)
{
  media <- mean(valores)   
  difer <- valores - media 
  difer2 <- difer ^ 2      
  SSE <- sum(difer2)       
  n <- length(valores)     
  vari <- SSE/(n-1)        
  result <- sqrt(vari)     

  return(result)
}

calculoDesvioPadrao
desvioPadrao(valores)

# chamando a função do R para Desvio Padrão
sd(valores)

#---------------------------------------------------------------------------------------------


