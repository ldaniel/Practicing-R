library(dplyr)
library(data.table)
library(ggplot2)

medicamentos <- fread("dados/TA_PRECOS_MEDICAMENTOS.csv")
View(medicamentos)

distinct(medicamentos, medicamentos$TP_PRODUTO)
distinct(medicamentos, medicamentos$TP_LCCT)
distinct(medicamentos, medicamentos$ST_CAP)

medicamentos <- mutate(medicamentos, total = medicamentos$NU_PF0_INTEIRO/medicamentos$NU_PF18_INTEIRO)

ggplot(data = medicamentos) +
  geom_point(mapping = aes(x = medicamentos$TP_LCCT, y = medicamentos$NU_PF18_INTEIRO))

#------------------------------------------------------------------------------------------
got <- fread("dados/game-of-thrones-deaths-data.csv")
View(got)

ggplot(data = got) +
  geom_bar(mapping = aes(x = got$season, fill = got$method_cat))

# getting the total deaths by killer, example 1
totalDeathsByKiller <- got %>% 
  group_by(killer) %>%
  count(killer) 

View(totalDeathsByKiller)

# getting the total deaths by killer, example 2
totalDeathsByKiller <- got[, .(.N), by = .(killer)] 
totalDeathsByKiller <- totalDeathsByKiller[order(N, decreasing = TRUE)]

View(totalDeathsByKiller)

# getting the top 10 killers
topTenKillers <- got[, .(.N), by = .(killer)] 
topTenKillers <- topTenKillers[order(N, decreasing = TRUE)]
topTenKillers <- topTenKillers[1:10]
  
data <- data[with(data,order(-Score)),]

ggplot(data = topTenKillers) +
  geom_point(mapping = aes(x = topTenKillers$killer, y = topTenKillers$N, size = topTenKillers$N))


# para editar parâmetros do ambiente R
# file.edit("~/.Renviron")

# Instalando biblioteca para gerar PDF com o markdown
# tinytex::install_tinytex()

install.packages("TinyTeX")
library(tinytex)





















           
