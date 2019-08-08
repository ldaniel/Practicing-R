library(dplyr)
library(psych)        # para estudo das variáveis quantitativas
library(gmodels)      # para estudo das variáveis qualitativas
library(VIM)          # para análise univariada e bivariada
library(caret)
library(mctest)       # para avaliação de multicolinearidade
library(ppcor)        # para aprofundar a avaliação de multicolinearidade
library(ggcorrplot)   # para plotar multicolinearidade
library(rms)          # para verificar a aderência do ajuste logístico
library(hmeasure)     # para métricas de discriminação de modelos
library(pROC)         # para analisar o ROC
library(rpart)        # para aplicar árvore de decisão
library(rpart.plot)   # para plotar árvores de decisão
library(randomForest) # para aplicar random forest
library(adabag)       # para aplicar boosted trees

# carregando a base de dados --------------------------------------------------
path <- "D:/OneDrive/FGV/06_R/Practicing-R/data/"
base <- read.csv(paste(path,"telco_churn.csv", sep = ""), sep = ",", header = T, stringsAsFactors = T)[,-1] 

# ajustando variavel resposta de YES/NO para 0/1
base <- mutate(base, Churn = ifelse(base$Churn == "YES", as.integer("1"), as.integer("0")))

View(base)
summary(base)
str(base)
colnames(base)
# Age
# NumbLines
# CustTime
# MthIncome
# BillAmount
# TimeCurAdrr
# Region
# CableTV
# AutDebt
# Churn   

# procurando outliers ---------------------------------------------------------
hist(base$Age, col = "lightblue", border = "darkblue", main = "AGE")
hist(base$NumbLines, col = "lightblue", border = "darkblue", main = "NumbLines")
hist(base$CustTime, col = "lightblue", border = "darkblue", main = "CustTime")
hist(base$MthIncome, col = "lightblue", border = "darkblue", main = "MthIncome") # tem 2 outliers significativos?
hist(base$BillAmount, col = "lightblue", border = "darkblue", main = "BillAmount") # tem outliers significativos?
hist(base$TimeCurAdrr, col = "lightblue", border = "darkblue", main = "TimeCurAdrr") # tem 1 outlier significativo?

plot(base$Age, ylab = "AGE")
plot(base$NumbLines, ylab = "NumbLines")
plot(base$CustTime, ylab = "CustTime")
plot(base$MthIncome, ylab = "MthIncome") # tem 2 outliers significativos?
plot(base$BillAmount, ylab = "BillAmount") # tem outliers significativos?
plot(base$TimeCurAdrr, ylab = "TimeCurAdrr") # tem 1 outlier significativo?

quantile(base$MthIncome, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) # identificando o percentil de 99%
quantile(base$BillAmount, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) # identificando o percentil de 99%
quantile(base$TimeCurAdrr, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) # identificando o percentil de 99%

# procurando missing values ---------------------------------------------------
summary(base$Age)
summary(base$NumbLines)
summary(base$CustTime)
summary(base$MthIncome)
summary(base$BillAmount)
summary(base$TimeCurAdrr)

# variáveis quantitativas -----------------------------------------------------
# distribuicao de alguma variavel especifica
describe(base$Age)
boxplot(base$Age ~ base$Churn)

describe(base$NumbLines)
boxplot(base$NumbLines ~ base$Churn)

describe(base$CustTime)
boxplot(base$CustTime ~ base$Churn)

describe(base$MthIncome)
boxplot(base$MthIncome ~ base$Churn)
boxplot(base$MthIncome ~ base$Churn, outline = F)

describe(base$BillAmount)
boxplot(base$BillAmount ~ base$Churn)
boxplot(base$BillAmount ~ base$Churn, outline = F)

describe(base$TimeCurAdrr)
boxplot(base$TimeCurAdrr ~ base$Churn)
boxplot(base$TimeCurAdrr ~ base$Churn, outline = F)

# variáveis qualitativas ------------------------------------------------------
prop.table(table(base$Churn)) # proporcao da variavel resposta

CrossTable(base$Region)
CrossTable(base$Region, base$Churn, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

CrossTable(base$CableTV)
CrossTable(base$CableTV, base$Churn, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

CrossTable(base$AutDebt)
CrossTable(base$AutDebt, base$Churn, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)

# análise univariada ----------------------------------------------------------
matrixplot(base)
aggr(base)

# análise bivariada -----------------------------------------------------------
prop.table(table(base$Churn))
prop.table(table(base$Region, base$Churn),1)
prop.table(table(base$CableTV, base$Churn),1)
prop.table(table(base$AutDebt, base$Churn),1)

# obtendo amostragem dos dados ------------------------------------------------
set.seed(12345)
index <- createDataPartition(base$Churn, p= 0.7,list = F)

data.train <- base[index, ] # base de desenvolvimento: 70%
data.test  <- base[-index,] # base de teste: 30%

# Checando se as proporções das amostras são próximas à base original
prop.table(table(base$Churn))
prop.table(table(data.train$Churn))
prop.table(table(data.test$Churn))

# aplicando a regressão logística ---------------------------------------------
# Avaliando multicolinearidade - vars quantitativas
vars.quant <- data.train[,c(1, 2, 3, 4, 5, 6)]

omcdiag(vars.quant, data.train$Churn) # 2 metodos distintos detectaram algum nivel de correlação entre as variáveis 
imcdiag(vars.quant, data.train$Churn) 
pcor(vars.quant, method = "pearson")  # não foi apontada correlação alta entre as variáveis

# plotando a multicolinearidade
corr <- round(cor(data.train[,c(1, 2, 3, 4, 5, 6)]), 1)
ggcorrplot(corr)
ggcorrplot(corr, method = "circle")
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, type = "lower", outline.col = "white")
ggcorrplot(corr, hc.order = TRUE, type = "upper", outline.col = "white")

# analisando o VIF
# VIF com valores bem elevados > 10 indicam correlação entre as variáveis 
vars.quant <- select_if(base, is.numeric)
VIF <- imcdiag(vars.quant, base$Churn)
VIF_Table <- tibble(variable = names(VIF$idiags[,1]), VIF = VIF$idiags[,1]) %>% arrange(desc(VIF))
VIF_Table

# salvando o nome de todas as variáveis e escrevendo a fórmula
names  <- names(data.train) 
f_full <- as.formula(paste("Churn ~", paste(names[!names %in% "Churn"], collapse = " + ")))

# criando o modelo de regressão logística
glm.full <- glm(f_full, data= data.train, family= binomial(link='logit'))
summary(glm.full)

# observam-se variáveis não significantes, 
# podemos remover uma de cada vez e testar, ou
# usar o método stepwise que escolhe as variáveis 
# que minimizem o AIC

# fazendo seleção de variáveis com stepwise 
glm.step <- stepAIC(glm.full, direction = 'both', trace = TRUE)
summary(glm.step)

# Aplicando o modelo nas amostras e determinando as probabilidades 
glm.prob.train <- predict(glm.step,type = "response")
glm.prob.test  <- predict(glm.step, newdata = data.test, type= "response")

# Verificando a aderência do ajuste logístico
val.prob(glm.prob.train, data.train$Churn, smooth = F)
# p valor > 5%, não podemos rejeitar a hipotese nula

# Comportamento da saida do modelo
hist(glm.prob.test, 
     breaks = 25,
     col = "lightblue",
     xlab= "Probabilidades",
     ylab= "Frequência",
     main= "Regressão Logística")

boxplot(glm.prob.test ~ data.test$Churn, col = c("red", "green"), horizontal = T)

# avaliando a performance do modelo de regressão logística
glm.train <- HMeasure(data.train$Churn, glm.prob.train)
glm.test  <- HMeasure(data.test$Churn, glm.prob.test)
summary(glm.train)
summary(glm.test)

roc1 <- roc(data.test$Churn, glm.prob.test)
y1 <- roc1$sensitivities
x1 <- 1-roc1$specificities

plot(x1, y1, type = "n", xlab = "1 - Especificidade", ylab= "Sensitividade")
abline(0,1, lty=2)
lines(x1, y1, lwd = 3, lty = 1, col = "purple")

# aplicando árvore de decisão -------------------------------------------------
# Algoritmos de arvore necessitam que a variável resposta num problema de classificação seja 
# um factor; convertendo aqui nas amostras de desenvolvimento e teste
data.train$Churn <- as.factor(data.train$Churn)
data.test$Churn  <- as.factor(data.test$Churn)

tree.full <- rpart(data= data.train,
                   f_full,
                   control = rpart.control(minbucket = 50),
                   method = "class")

# saida da árvore
tree.full
summary(tree.full)

# importância das variáveis
round(tree.full$variable.importance, 3)

# avaliando a necessidade de poda da árvore (CP = Complexity Parameters)
printcp(tree.full)
plotcp(tree.full)

# fazendo a poda da árvore
tree.prune <- prune(tree.full, cp = tree.full$cptable[which.min(tree.full$cptable[,"xerror"]),"CP"])

# plotando a árvore
rpart.plot(tree.full, cex = 1.3, type = 0,
           extra = 104, box.palette = "BuRd",
           branch.lty = 3, shadow.col = "gray", nn = TRUE, 
           main="Árvore de Classifação full")


rpart.plot(tree.prune, cex = 1.3, type = 0,
           extra = 104, box.palette = "BuRd",
           branch.lty = 3, shadow.col = "gray", nn = TRUE, 
           main="Árvore de Classifação prune")

# aplicando o modelo nas amostras  e determinando as probabilidades
tree.prob.train <- predict(tree.prune, type = "prob")[,2]
tree.prob.test  <- predict(tree.prune, newdata = data.test, type = "prob")[,2]

# comportamento da saida do modelo
hist(tree.prob.test, breaks = 25, col = "lightblue", xlab = "Probabilidades",
     ylab = "Frequência", main = "árvore de Classificão")

boxplot(tree.prob.test ~ data.test$Churn, col= c("green", "red"), horizontal = T)

# avaliando a performance do modelo de árvore de decisão
tree.train <- HMeasure(data.train$Churn, tree.prob.train)
tree.test  <- HMeasure(data.test$Churn, tree.prob.test)
tree.train$metrics
tree.test$metrics

roc1 <- roc(data.test$Churn, tree.prob.test)
y1 <- roc1$sensitivities
x1 <- 1-roc1$specificities

plot(x1, y1, type = "n", xlab = "1 - Especificidade", ylab= "Sensitividade")
lines(x1, y1, lwd = 3,lty = 1, col = "blue") 
abline(0,1, lty=2)

# aplicando ensemble (random forest) ------------------------------------------
# aqui começamos a construir um modelo de random forest usando sqrt(n var) | mtry = default
# construimos 500 árvores, e permitimos nós finais com no mínimo 50 elementos
rndfor <- randomForest(f_full, data = data.train, importance = T, nodesize = 50, ntree = 500)
rndfor

# avaliando a evolução do erro com o aumento do número de árvores no ensemble
plot(rndfor, main = "Mensuração do erro")
legend("topright", c('Out-of-bag',"1","0"), lty = 1, col = c("black","green","blue"))

# uma avaliação objetiva indica que a partir de ~30 árvores não mais ganhos expressivos
rndfor2 <- randomForest(f_full, data = data.train, importance = T, nodesize = 50, ntree = 30)
rndfor2

# importância das Variáveis
varImpPlot(rndfor2, sort = T, main = "Importância das Variáveis")

# aplicando o modelo nas amostras  e determinando as probabilidades
rndfor2.prob.train <- predict(rndfor2, type = "prob")[,2]
rndfor2.prob.test  <- predict(rndfor2, newdata = data.test, type = "prob")[,2]

# comportamento da saida do modelo
hist(rndfor2.prob.test, breaks = 25, col = "lightblue", xlab = "Probabilidades",
     ylab = "Frequência", main = "Random Forest")

boxplot(rndfor2.prob.test ~ data.test$Churn, col = c("green", "red"), horizontal = T)

# aplicando ensemble (boosting) -----------------------------------------------
# aqui construimos inicialmente um modelo boosting com 1000 iterações, profundidade 1
# e minbucket 50, os pesos das árvores será dado pelo algortimo de Freund
boost <- boosting(f_full, data = data.train, mfinal= 200, 
                  coeflearn = "Freund", 
                  control = rpart.control(minbucket = 50, maxdepth = 1))

# avaliando a evolução do erro conforme o número de iterações aumenta
plot(errorevol(boost, data.train))

# podemos manter em 200 iterações

# importância das Variáveis
var_importance <- boost$importance[order(boost$importance, decreasing = T)]
var_importance
importanceplot(boost)

# aplicando o modelo na amostra de teste e determinando as probabilidades
boost.prob.train <- predict.boosting(boost, data.train)$prob[,2]
boost.prob.test  <- predict.boosting(boost, data.test)$prob[,2]

# comportamento da saida do modelo
hist(boost.prob.test, breaks = 25, col = "lightblue", xlab = "Probabilidades",
     ylab= "Frequência", main = "Boosting")

boxplot(boost.prob.test ~ data.test$Churn, col= c("green", "red"), horizontal = T)

# aplicando ensemble (avaliando performance) ----------------------------------
rndfor.train  <- HMeasure(data.train$Churn, rndfor2.prob.train)
rndfor.test  <- HMeasure(data.test$Churn, rndfor2.prob.test)
rndfor.train$metrics
rndfor.test$metrics

boost.train <- HMeasure(data.train$Churn, boost.prob.train)
boost.test  <- HMeasure(data.test$Churn, boost.prob.test)
boost.train$metrics
boost.test$metrics

roc1 <- roc(data.test$Churn, rndfor2.prob.test)
y1 <- roc1$sensitivities
x1 <- 1-roc1$specificities

roc2 <- roc(data.test$Churn, boost.prob.test)
y2 <- roc2$sensitivities
x2 <- 1-roc2$specificities

plot(x1,y1, type="n", xlab = "1 - Especificidade", ylab = "Sensitividade")
lines(x1, y1, lwd = 3,lty = 1, col = "purple") 
lines(x2, y2, lwd = 3,lty = 1, col = "blue") 
abline(0,1, lty=2)
legend("topright", c('Random Forest', "Boosting"), lty = 1, col = c("purple","blue"))

