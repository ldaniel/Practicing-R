# # install.packages("esquisse")
# # with remotes
# remotes::install_github("dreamRs/esquisse")
# 
# # or with install-github.me service (based on remotes)
# source("https://install-github.me/dreamRs/esquisse")
# 
# # or with devtools:
# devtools::install_github("dreamRs/esquisse")
# install.packages("ggthemes")
# devtools::install_github("dreamRs/ggthemes")

library(esquisse)
library(ggthemes)
library(tidyverse)
library(ggplot2)

# carregando o plugin do ggplot2 Builder
esquisse::esquisser()

# código gerado pelo plugin
ggplot(data = diamonds) +
  aes(x = cut, y = carat, fill = clarity, color = depth, size = price) +
  geom_boxplot() +
  labs(title = "Diamonds",
    x = "Carat",
    y = "Cut",
    caption = "by Leandro Daniel",
    subtitle = "Gerando gráficos com o ggplot Builder") +
  theme_minimal()

#-----------------------------------------------------------------------------------------

library(ggplot2)
library(gganimate)
library(gifski)

ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 1,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#-----------------------------------------------------------------------------------------

library(gapminder)

meusdados <- data.frame(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

#-----------------------------------------------------------------------------------------
install.packages("ggalluvial")
library(ggalluvial)

titanic_wide <- data.frame(Titanic)

ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")