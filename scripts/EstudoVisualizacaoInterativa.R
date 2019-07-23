# Projetos de visualização de dados em javascript:
#   https://d3js.org/ / https://github.com/d3/d3/wiki/Gallery
#   https://d3plus.org/
#   https://www.highcharts.com/
#   https://c3js.org/
#   https://leafletjs.com/
#   https://plot.ly/
#   http://gallery.htmlwidgets.org/

# ------------------------------------------------------------------------------------
# 10.2 plotly

library(plotly)
library(tidyverse)

getwd()

p <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
ggplotly(p)

# ------------------------------------------------------------------------------------
# 10.3 dygraphs 

x <- rnorm(24, mean = 100, sd = 10)

# Trasnformando em série mensal a partir de janeiro de 2010
x <- ts(x, freq = 12, start = c(2010, 1))
plot(x)

# outro exemplo de criação de série de tempo
library(xts)
xts_df <- data.frame(y = rnorm(365, 100, 10)) 
xts_df$data <- seq.Date(as.Date("2011-01-01"), length.out = 365,
                        by = "1 day")
xts_df <- xts(x = xts_df[, "y"], order.by =  xts_df[, "data"])

head(xts_df)

install.packages("dygraphs")
library(dygraphs)

lungDeaths <- cbind(mdeaths, fdeaths)
View(lungDeaths)

dygraph(lungDeaths,
        main="Mortes por Doenças Pulmonares - Reino Unido - 1874-1979",
        ylab="Números de mortes") %>%
  dySeries("mdeaths", color = "blue", label = "Homens") %>%
  dySeries("fdeaths", color = "green", label = "Mulheres") %>% 
  dyRangeSelector()

# ------------------------------------------------------------------------------------
# 10.4 Leaflet

library(dplyr)
library(leaflet)

leaflet() %>%
  addTiles()

# Primeiro exemplo
library(ggmap)

loc.ibpad <- data.frame(lon = -47.8838813, lat = -15.8010146)
loc.ibpad$popup <- "Estamos aqui! (teoricamente)"
leaflet(loc.ibpad) %>%
  addTiles(loc.ibpad) %>%
  addMarkers(lat = ~lat, lng = ~lon, popup = ~popup)

# Marcadores

dados.empresas.mt <- read_delim('dados/empresas_exp_mt.csv',
                                delim = ";", 
                                locale = locale(encoding = 'ISO-8859-1',
                                                decimal_mark = ","))
leaflet(dados.empresas.mt) %>%
  addTiles() %>%
  addMarkers(lat = ~lat, lng = ~lon, popup = ~EMPRESA)

leaflet(dados.empresas.mt) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~lat, lng = ~lon, popup = ~EMPRESA, fillOpacity = 0.3)

leaflet(dados.empresas.mt) %>%
  addTiles() %>%
  addCircleMarkers(lat = ~lat, lng = ~lon, popup = ~EMPRESA, fillOpacity = 0.3,
                   clusterOptions = markerClusterOptions())


# Polígonos
library(rgdal)
library(maptools)
library(rgeos)
library(ggplot2)

ogrListLayers('dados/mapas/mg_municipios/31MUE250GC_SIR_simplificado.shp')

mg_mapa <- readOGR('dados/mapas/mg_municipios/31MUE250GC_SIR_simplificado.shp',
                   layer = '31MUE250GC_SIR_simplificado')


mg_mapa$Mun.Trab <- substr(mg_mapa$CD_GEOCMU, 1, 6)

REM_RAIS_MG_2015 <- read_delim('dados/REM_RAIS_MG_2015.csv',
                               delim = ";",
                               locale = locale(encoding = "ISO-8859-1"),
                               col_types = 'cd')

colnames(REM_RAIS_MG_2015)[1] <- "Mun.Trab"
summary(REM_RAIS_MG_2015$mediana)
REM_RAIS_MG_2015 <- REM_RAIS_MG_2015 %>%
  mutate(mediana.original = mediana,
         mediana = ifelse(mediana > 1500, 1500, mediana))
head(REM_RAIS_MG_2015)

mg_mapa@data <- left_join(mg_mapa@data, REM_RAIS_MG_2015, by = "Mun.Trab")

mg_mapa$POPUP <- paste0(mg_mapa$NM_MUNICIP, ": R$ ",
                        format(round(mg_mapa$mediana.original, 2),
                               big.mark = ".", decimal.mark = ","))

viridis.colors<- viridis::viridis(n = 20)

pal <- colorNumeric(viridis.colors, domain = mg_mapa$mediana)

leaflet(mg_mapa) %>%
  addTiles() %>%
  addPolygons(weight = 0.8,
              fillColor = ~pal(mediana), fillOpacity = 0.9,
              popup = ~POPUP)


leaflet(mg_mapa) %>%
  addTiles(urlTemplate = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
  addPolygons(weight = 0.8,
              fillColor = ~pal(mediana), fillOpacity = 0.9,
              popup = ~POPUP) %>% 
  addLegend("topleft", title = "Remuneração (R$)",
            colors = pal(seq(800, 1500, 100)),
            values = seq(800, 1500, 100),
            labels = c(seq(800, 1400, 100), "\u2265  1500"),
            opacity = 1)

























