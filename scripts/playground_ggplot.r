# do site https://cdr.ibpad.com.br/ggplot2.html

# Carregando ggplot e dplyr para trabalhar com gráficos
library("ggplot2")
library("dplyr")

# carregando as tabelas de exemplo do R
data("mtcars")
data("mpg")

# visualizando os dados no RStudio
View(mtcars)
View(mpg)

# gerando o gráfico com ggplot
ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point(aes(color=class, size=cyl)) +
  geom_smooth(method="lm", aes(lty=drv)) + # methods = "auto", "lm", "glm", "gam", "loess"
  facet_wrap(~manufacturer) 
 #facet_wrap(~manufacturer~model)

#--------------------------------------------------------------------------------------------

g <- ggplot(mtcars)
g <- g + geom_point(aes(x=hp, y=mpg, color=factor(am)), size=3)
g <- g + scale_color_manual("Automatic", values=c("red","blue"), labels=c("No","Yes"))
g <- g + labs(title = "Relação entre consumo, potência e tipo de câmbio", y="Consumo", x="Potência")
g

#--------------------------------------------------------------------------------------------

g1 <- ggplot(mtcars, aes(y = mpg, x = disp)) + geom_point()
g1

#--------------------------------------------------------------------------------------------

mtcars <- mtcars %>% mutate(name=rownames(mtcars))
ggplot(mtcars, aes(y=mpg, x=disp)) + geom_point() + geom_smooth() + facet_wrap(~carb)

#--------------------------------------------------------------------------------------------

data("iris")
View(iris)

# Escalas automáticas
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()

# Escalas manuais
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
  geom_point() + 
  scale_color_manual(values=c("orange", "black", "red"))

# Definindo as quebras
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = c("orange", "black", "red")) +
  scale_x_continuous(name = "Petal Length", breaks = 1:7) + 
  scale_y_continuous(name = "Petal Width", breaks = 0:3, limits = c(0, 3))

#--------------------------------------------------------------------------------------------
# Variáveis contínuas
library("ISLR")

View(Wage)

ggplot(Wage, aes(x=age, y=wage, color=education)) + 
  geom_point() +
  scale_x_continuous("Idade", breaks = seq(0,80,5), expand=c(0,5)) +
  scale_y_continuous("Salário", labels = function(x) paste0("US$ ", x), limits = c(0,400))

#--------------------------------------------------------------------------------------------
# Variáveis discretas
View(Default)

ggplot(Default, aes(x = default, y = balance)) +
  geom_boxplot() + 
  scale_x_discrete("Calote", labels = c("Não", "Sim")) +
  labs(y = "Valor devido médio após o pagamento mensal")

ggplot(Default, aes(x = default, y = balance)) +
  geom_boxplot() + 
  scale_x_discrete("Calote", limits = c("Yes", "No"), labels = c("Sim", "Não")) +
  labs(y = "Valor devido médio após o pagamento mensal")

#--------------------------------------------------------------------------------------------
# Variáveis de datas
View(economics)

ggplot(economics, aes(x=date, y=unemploy)) + 
  geom_line() +
  scale_x_date(date_labels = "%b/%Y")

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

#--------------------------------------------------------------------------------------------
library(RColorBrewer)

# Escalas de cores
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)


paleta.gradientn <- brewer.pal(n = 9, name = 'Reds')
Credit %>% 
  group_by(Cards, Student) %>%
  summarise(Balance = mean(Balance), n = n()) %>% 
  ggplot(aes(x = Cards, y = Student, fill = Balance)) +
  geom_tile() +
  scale_fill_gradientn(colors = rev(paleta.gradientn)) +
  scale_x_continuous(breaks = 1:9)

# paleta própria de cores da viridis
Credit %>% 
  group_by(Cards, Student) %>%
  summarise(Balance = mean(Balance), n = n()) %>% 
  ggplot(aes(x = Cards, y = Student, fill = Balance)) +
  geom_tile() +
  viridis::scale_fill_viridis() +
  scale_x_continuous(breaks = 1:9)

# definindo escala de cores manuais
ggplot(Wage, aes(y = wage, x = age, color = education)) +
  geom_point() +
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"))

#--------------------------------------------------------------------------------------------
# 9.4 Subplots (facet)
View(diamonds)

#--
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

#--
ggplot(diamonds, aes(x = carat, y = price, color = factor(color))) + 
  geom_point() + 
  facet_wrap(~cut)

#--
ggplot(diamonds, aes(x = carat, y = price, color = factor(color))) + 
  geom_point() + 
  facet_wrap(~cut, scales="free_y")

#--
ggplot(diamonds, aes(x = carat, y = price, color = factor(color))) + 
  geom_point() + 
  facet_wrap(~clarity~cut)

#--
nomes_cut <- c(
  Fair = "FAIR",
  Good = "GOOD",
  `Very Good` = "VERY GOOD",
  Premium = "PREMIUM",
  Ideal = "IDEAL"
)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(~ cut, scales = "free_y",
             labeller = labeller(cut = nomes_cut))

#--------------------------------------------------------------------------------------------
# 9.5 Temas
install.packages('ggThemeAssist')

ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point() +
  labs(title = "Carat vs Price") +
  theme(text = element_text(face="bold"), 
        panel.grid.major = element_line(colour="gray80"), 
        axis.title = element_text(size = 14), 
        panel.background = element_rect(fill = "gray100"))

# temas disponíveis

p <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() 

p + theme_gray() +
  labs(title = "theme_gray()")

p + theme_bw() +
  labs(title = "theme_bw()")

p + theme_linedraw() +
  labs(title = "theme_linedraw()")

p + theme_light() +
  labs(title = "theme_light()")

p + theme_minimal() +
  labs(title = "theme_minimal()")

p + theme_classic() +
  labs(title = "theme_classic()")

p + theme_dark() +
  labs(title = "theme_dark()")

p + theme_void() +
  labs(title = "theme_void()")

#--------------------------------------------------------------------------------------------
# hrbrthemes
install.packages("hrbrthemes")
library(hrbrthemes)

hrbrthemes::import_roboto_condensed()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  labs(title = "theme_ipsum()") +
  theme_ipsum(plot_title_size = 12,
              axis_title_size = 10)

#--------------------------------------------------------------------------------------------
# Setando o tema globalmente
theme_set(theme_ipsum(plot_title_size = 12,
                     axis_title_size = 10) +
           theme(text = element_text(angle = 0)))



# exemplo usando o guide_legend()
ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = cut)) +
  geom_point() +
  guides(color = guide_legend(title = "Cor", title.position = "left", keywidth = 5),
         shape = guide_legend(title = "Forma", title.position = "right", override.aes = aes(size = 5)))


#--------------------------------------------------------------------------------------------
# 9.7 Escolhendo o tipo de gráfico
install.packages("treemapify")
library(treemapify)
View(G20)

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) +
  theme(legend.position = 'bottom')

#--------------------------------------------------------------------------------------------
# 9.8 Gráficos de dispersão
install.packages("gapminder")
library(gapminder)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(gapminder)


gapminder %>% 
  filter(year==max(year)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  labs(title="Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x="Renda per Capita",
       y="Expectativa de Vida") +
  theme_ipsum(plot_title_size = 12,
              axis_title_size = 10)

# usando logaritmo
gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10)

# usando shape
gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             color = continent,
             shape = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_color_discrete("Continent") +
  scale_shape_discrete("Continent") +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10)

# alterando formas e shapes
gapminder %>%
  filter(year == max(year)) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp,
             color = continent, shape = continent)) +
  geom_point(fill = "black", size = 3, stroke = 1) +
  scale_x_log10() +
  scale_color_discrete("Continente") +
  scale_shape_manual("Continente", values=c(19,21,22,23,24)) +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")
  
#--------------------------------------------------------------------------------------------
# 9.9 Gráficos de Bolhas

gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, 
             size = pop)) +
  geom_point() + 
  scale_size_continuous("População (milhões)", labels = function(x) round(x/1e6)) +
  scale_x_log10() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida") +
  theme_ipsum(plot_title_size = 12,    
              axis_title_size = 10)

#--------------------------------------------------------------------------------------------
# 9.10 Gráficos de Barras

ggplot(diamonds, aes(x=cut)) +
  geom_bar() +
  theme_ipsum(plot_title_size = 12,
              axis_title_size = 10)
  
# outro exemplo
gapminder %>%
  filter(year == max(year),
         continent == "Americas") %>%
  ggplot(aes(x = country, y = lifeExp)) +
  geom_bar(stat = "identity", fill="dodgerblue") +
  labs(title = "Expectativa de vida por país",
       subtitle = "2007",
       x = "País",
       y = "Expectativa de vida")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# usando o geom_col()
gapminder %>% 
  filter(year == max(year),
         continent == "Americas") %>% 
  ggplot(aes(x = country, y = lifeExp)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Expectativa de vida por país",
       subtitle = "2007",
       x = "País",
       y = "Anos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# usando reorder() para ordenar de forma crescente ou decrescente as barras
gapminder %>% 
  filter(year == max(year),
         continent == "Americas") %>% 
  ggplot(aes(x = reorder(country, -lifeExp), y = lifeExp)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Expectativa de vida por país",
       subtitle = "2007",
       x = "País",
       y = "Anos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# outro exemplo
gapminder %>% 
  filter(year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = continent, y = lifeExp, fill = year)) +
  geom_col() +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10)

# outro exemplo com barras lado a lado
gapminder %>% 
  filter(year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = continent, y = lifeExp, fill = year)) +
  geom_col(position = "dodge") +
  labs(title = "Expectativa de vida por continente",
       x = "Continente",
       y = "Anos",
       fill = "Ano") +
  theme_ipsum(plot_title_size = 12, 
              axis_title_size = 10)

# outro exemplo com barras horizontais lado a lado
gapminder %>% 
  filter(year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = continent, y = lifeExp, fill = year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Expectativa de vida por continente",
       x = "Continente",
       y = "Anos",
       fill = "Ano") +
  theme_ipsum(plot_title_size = 12, 
              axis_title_size = 10)


#--------------------------------------------------------------------------------------------
# 9.11 Gráficos de linhas
library(dplyr)
library(ggplot2)
library(gapminder)
library(hrbrthemes)

gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp, color=continent)) +
  geom_line()
  
# com marcações
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  geom_point(aes(shape = continent)) +
  labs(title = "Evolução da expectativa de vida por continente",
       x = "Ano",
       y = "Anos de vida",
       color = "Continente",
       shape = "Continente") +
  theme_ipsum(plot_title_size = 12,    
              axis_title_size = 10)

#--------------------------------------------------------------------------------------------
# 9.12 Histogramas e freqpoly

# histograma
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x=lifeExp)) +
  geom_histogram(binwidth = 5, fill = 'dodgerblue', color = 'black') + 
  labs(title = "Distribuição da expectativa de vida", x = "Anos", y = "Contagem") +
  theme_ipsum(plot_title_size = 12,
              axis_text_size = 10)


# freqpoly
gapminder %>%
  filter(year == "2007") %>%
  ggplot(aes(x=lifeExp)) +
  geom_freqpoly(binwidth = 5) +
  labs(title = "Distribuição da expectativa vida",
       x = "Anos",
       y = "Contagem") +
  theme_ipsum(plot_title_size = 12,     
              axis_title_size = 10)

# transformando em proporção
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x=lifeExp)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 5, fill= "dodgerblue", color = "black") + 
  labs(title = "Distribuição da expectativa de vida", x = "Anos", y = "Contagem") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_ipsum(plot_title_size = 12,
              axis_text_size = 10)

#--------------------------------------------------------------------------------------------
# 9.13 Boxplots, jitterplots e violinplots

# boxplot
ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10) 

# violinplot
ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_violin(fill = "dodgerblue") +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12,  
              axis_title_size = 10) 

# jitterplot
ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_point() +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, 
              axis_title_size = 10) 

ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_jitter() +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12,      
              axis_title_size = 10) 


#--------------------------------------------------------------------------------------------
# 9.14 Anotações

# descobrindo o outlier do gráfico de boxplot
gapminder %>%
  filter(year == 1992, lifeExp == min(lifeExp))

# gerando uama anotação para este outlier
ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_boxplot(fill = "dodgerblue") +
  annotate("text", x = "1992", y = 27, label = "Ruanda") +
  labs(y = "Anos de vida", x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, 
              axis_title_size = 10)

# adicionando um segmento para dar destaque
ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  annotate("text", x = "1992", y = 27, label = "Ruanda") +
  annotate("rect", xmin = "1982", ymin = 20,
           xmax = "2002", ymax = 95, alpha = 0.2) +
  geom_boxplot(fill = "dodgerblue") +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12,      
              axis_title_size = 10) 

#--------------------------------------------------------------------------------------------
# 9.15 Cleveland Dot Plot

gapminder %>% 
  filter(year == 2007,
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_point(size = 3, color = "dodgerblue") + 
  labs(title = "Expectativa de vida por país - 2007",
       y = "País",
       x = "Anos") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# apresentando mais de um ponto por categoria
gapminder %>% 
  filter(year %in% c(1987, 2007),
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = country, color = factor(year))) +
  geom_point(aes(color = factor(year))) + 
  labs(title = "Expectativa de vida por país - 1987 e 2007",
       y = "País",
       x = "Anos",
       color = "Ano") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# exemplo de connected dot plot
gapminder %>% 
  filter(year %in% c(1987, 2007),
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = country)) +
  geom_line(aes(group = country)) +
  geom_point(aes(color = factor(year))) + 
  labs(title = "Expectativa de vida por país - 1987 e 2007",
       y = "País",
       x = "Anos",
       color = "Ano") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# ordenando o eixo y pela expectativa de vida
gapminder %>% 
  filter(year %in% c(1987, 2007),
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = reorder(country, lifeExp, max))) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = factor(year))) + 
  labs(title = "Expectativa de vida por país - 1987 e 2007",
       y = "País",
       x = "Anos",
       color = "Ano") +
  theme_ipsum(plot_title_size = 12,
              axis_title_size = 10)  +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#--------------------------------------------------------------------------------------------
# 9.16 Textos/Rótulos

# Para adicionar textos ou rótulos, utilizamos, respectivamente, o geom_text() e o geom_label()
gapminder %>% 
  filter(year == 2007,
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_segment(x = 0, aes(xend = lifeExp, yend = country),
               color = "grey50") +
  geom_point(size = 3, color = "dodgerblue") + 
  geom_text(aes(label = round(lifeExp))) +
  labs(title = "Expectativa de vida por país - 2007",
       y = "País",
       x = "Anos") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10)

# ajustando a posição dos labels usando hjust, vjust, nudge_x e nudge_y. 
# O hjust e vjust podem assumir valor entre 0 (direita/embaixo) 
# e 1(esquerda/em cima) ou “left”, “middle”, “right”, “bottom”, “center” 
# e “top”, além de “inward” e “outward”.
gapminder %>% 
  filter(year == 2007,
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_point(size = 3, color = "dodgerblue") + 
  geom_text(aes(label = round(lifeExp)), nudge_x = 1) +
  labs(title = "Expectativa de vida por país - 2007",
       y = "País",
       x = "Anos") +
  theme_ipsum(plot_title_size = 12,   
              axis_title_size = 10)  +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# colocando um box no label
gapminder %>% 
  filter(year == 2007,
         continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_point(size = 3, color = "dodgerblue") + 
  geom_label(aes(label = round(lifeExp)), nudge_x = 1, size = 3) +
  labs(title = "Expectativa de vida por país - 2007",
       y = "País",
       x = "Anos") +
  theme_ipsum(plot_title_size = 12,  
              axis_title_size = 10)  +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#--------------------------------------------------------------------------------------------
# 9.17 Plotando fuções

# exemplo 1
reta <- function(a, b, x){
  a + b * x
}

data <- data.frame(x = seq(0, 10, by = 0.1))
ggplot(data, aes(x = x)) +
  stat_function(fun = reta, args = list(a = 1, b = 2)) +
  stat_function(fun = reta, args = list(a = 1, b = 3), col = 'red')

# exemplo 2
sigmoid <- function(a = 1,z){
  1/(1 + exp(-a * z))
}

data <- data.frame(x = -6:6)
ggplot(data, aes(x = x)) +
  stat_function(fun = sigmoid, args = list(a = 1)) +
  stat_function(fun = sigmoid, args = list(a = 0.5), color = "blue") +
  stat_function(fun = sigmoid, args = list(a = 2), color = "red") 

# exemplo 3
logit <- function(a, z){
  log(sigmoid(a, z)/(1 - sigmoid(a, z)))
}

data <- data.frame(x = -6:6)
ggplot(data, aes(x = x)) +
  stat_function(fun = logit, args = list(a = 1), aes(color = "a = 1")) +
  stat_function(fun = logit, args = list(a = 0.5), aes(color = "a = 0.5")) +
  stat_function(fun = logit, args = list(a = 2), aes(color = "a = 2")) 

#--------------------------------------------------------------------------------------------
# 9.18 Mapas
library(readr)

# exemplo carregando dados com coordenadas --------------------
# carregando coordenadas dos países
world_map <- read_delim('dados/world_map.csv', delim = ";",
                        locale = locale(encoding = "ISO-8859-1",
                                        decimal_mark = ","))

# Remover Antarctica
world_map <- world_map %>% 
  filter(id != "Antarctica")

# carregando dados de exportações por país de destino no ano de 2015
exp.2015 <- read_delim('dados/EXP_2015_PAIS.csv', delim = ";",
                       locale = locale(encoding = "ISO-8859-1"),
                       col_types = 'ccd')

# juntando informações dos mapas com as exportações
world_map <- left_join(world_map, exp.2015, by = "NO_PAIS_POR") %>%
  mutate(class = cut(VL_FOB, breaks = c(0, 1e6, 10e6, 100e6, 1e9, 10e9, Inf)))

# plotando o mapa
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = class), col = 'black', size = 0.1) +
  scale_fill_brewer(palette = "Reds", breaks = levels(world_map$class),
                    labels = c("(0, 1 Mi]", "(1 Mi, 10 Mi]", "(10 Mi, 100 Mi]",
                               "(100 Mi, 1 Bi]", "(1 Bi, 10 Bi]", "> 10 Bi"),
                    na.value = 'grey70') +
  labs(title = "Exportações Brasileiras - 2015",
       subtitle = 'Valor FOB') +
  coord_quickmap() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# exemplo carregando dados do IBGE ----------------------------
install.packages("rgdal","maptools","rgeos")
library(rgdal)
library(maptools)
library(rgeos)
library(broom)

ogrListLayers('dados/mapas/mg_municipios/31MUE250GC_SIR.shp')

mg_mapa <- readOGR('dados/mapas/mg_municipios/31MUE250GC_SIR.shp',
                   layer = '31MUE250GC_SIR')

mg_mapa_data <- data.frame(mg_mapa)
  
mg_mapa <- tidy(mg_mapa, region = "CD_GEOCMU")

# Para adicionar os nomes dos municípios
mg_mapa <- left_join(mg_mapa, mg_mapa_data, by = c("id" = "CD_GEOCMU"))


ggplot(mg_mapa, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black', fill = 'white', size = 0.1) +
  coord_quickmap()

# dados que serão plotados
REM_RAIS_MG_2015 <- read_delim('dados/REM_RAIS_MG_2015.csv',
                               delim = ";", 
                               col_types = 'cd',
                               skip = 1,
                               col_names = c("Mun.Trab", "mediana"),
                               locale = locale(encoding = 'ISO-8859-1'))

REM_RAIS_MG_2015 <- REM_RAIS_MG_2015 %>%
  mutate(mediana = ifelse(mediana > 1500, 1500, mediana))
head(REM_RAIS_MG_2015)

# plotando...
mg_mapa <- mg_mapa %>%
  mutate(Mun.Trab = substr(id, 1, 6))

mg_mapa <- left_join(mg_mapa, REM_RAIS_MG_2015, by = "Mun.Trab")

cores.viridis <- viridis::viridis(n = 20)
ggplot(mg_mapa, aes(x = long, y = lat, group = group, fill = mediana)) +
  geom_polygon(color = 'black', size = 0.1) +
  scale_fill_gradientn(colours = cores.viridis,
                       breaks = c(800, 1150, 1500),
                       labels = c("\u2264 800", "1.150", "\u2265 1.500")) +
  labs(title = "Mediana da Remuneração ",
       subtitle = "Minais Gerais - Dezembro/2015. Valores em R$",
       caption = "Fonte: RAIS/MTE") +
  coord_quickmap() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# simplificando o shapefile para tornar a renderização mais leve
library(rgeos)

mg_mapa <- readOGR('dados/mapas/mg_municipios/31MUE250GC_SIR.shp',
                   layer = '31MUE250GC_SIR')


df <- data.frame(mg_mapa)
mg_mapa <- gSimplify(mg_mapa, tol=0.01, topologyPreserve = TRUE)
mg_mapa = SpatialPolygonsDataFrame(mg_mapa, data=df)

mg_mapa <- tidy(mg_mapa, region = "CD_GEOCMU")

# Para adicionar os nomes dos municípios
mg_mapa <- left_join(mg_mapa, mg_mapa_data, by = c("id" = "CD_GEOCMU"))

ggplot(mg_mapa, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black', fill = 'white', size = 0.1) +
  coord_quickmap()

# Encoding dos dados do shapefile
mg_mapa <- readOGR('dados/mapas/mg_municipios/31MUE250GC_SIR.shp',
                   layer = '31MUE250GC_SIR')
mg_mapa@data$NM_MUNICIP <- iconv(mg_mapa@data$NM_MUNICIP,
                                 from = "UTF-8",
                                 to = "ISO-8859-1")

#--------------------------------------------------------------------------------------------
# 9.19 Salvando gráficos

knitr::include_graphics('images/salvar_plot.gif')

# ou

ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE, ...)

#--------------------------------------------------------------------------------------------
# 9.20 Extensões do ggplot2

# Galeria: http://www.ggplot2-exts.org/gallery/

# ggrepel
library(ggrepel)
data(mtcars)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rownames(mtcars))) +
  theme_ipsum(plot_title_size = 12,  
              axis_title_size = 10)

# gganimate
install.packages("devtools")
install.packages("gganimate")

library(devtools)
devtools::install_github("dgrtwo/gganimate")

library(gganimate)

p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp,
                           size = pop, color = continent,
                           frame = year)) +
  geom_point() +
  scale_x_log10()

animation::ani.options(interval = 1)
x <- gganimate(p, filename = 'images/gapminder1.gif',
               ani.width = 750,
               ani.height = 450)

# suavizando a animação com o tweenr
library(tweenr)

years <- unique(gapminder$year)
gapminder_list <- list()
for(i in 1:length(years)){
  j <- years[i]
  gapminder_list[[i]] <- gapminder %>% 
    filter(year == j)
}

tf <- tween_states(gapminder_list,
                   tweenlength = 2,
                   statelength = 0,
                   ease = rep("linear", length(gapminder_list)),
                   nframes = 308)

tf2 <- expand.grid(y = 80, x = 10^(2.5), year = seq(1957, 2007, 5))
tf2 <- split(tf2, tf2$year)
tf2 <- tween_states(tf2,
                    tweenlength = 2,
                    statelength = 0,
                    ease = rep("linear", length(tf2)),
                    nframes = 308)
tf2 <- tf2 %>% 
  mutate(year = rep(seq(1957, 2007, 5), each = 29)[1:310])

p2 <- ggplot(tf,
             aes(x=gdpPercap, y=lifeExp, frame = .frame)) +
  geom_point(aes(size=pop, color=continent), alpha=0.8) +
  geom_text(data = tf2, aes(x = x, y = y, label = year)) +
  xlab("GDP per capita") +
  ylab("Life expectancy at birth") +
  scale_x_log10()
animation::ani.options(interval = 1/20)
x <- gganimate(p2, filename = 'images/gapminder2.gif',
               ani.width = 750,
               ani.height = 450,
               title_frame = FALSE)
































