#install.packages("e1071")
library("tidyverse")
library("readr")
library("dplyr")      #pacote utilizado para o 'select()'
library("tidyselect") #pacote para usar o 'everything()', que seleciona todos os atributos
library("e1071")      #pacote para calcular obliquidade e curtose
library("base")

data <- read_csv("data.csv")

#TOPICO 1- COLOCANDO O ATRIBUTO ALVO NA ÚLTIMA COLUNA
shotMade <- select(data, shot_made_flag)

data$shot_made_flag <-NULL

shots <- data %>% 
  mutate(shotMade, shot_made_flag = shot_made_flag)

View(shots)


#TOPICO 2 E 3 - COMPREENDENDO OS ATRIBUTOS
#gráfico que indica como foi possível analisar sobre o que se referiam os atributos loc_x e loc_y
ggplot(data = shots) +
  geom_point(mapping = aes(x = loc_x, y = loc_y, color = shot_type))

#analisando que a distância foi medida em outra unidade, em pés, enquanto
#a localização (que é outra medida de distância) foi medida em jardas 
ggplot(data = shots) +
  geom_point(mapping = aes(x = loc_x, y = shot_distance, color = shot_type))

#gráfico que indica os locais que são definidos pelo atributo 'shot_zone_area'
ggplot(data = shots) +
  geom_point(mapping = aes(x = loc_x, y = loc_y, color = shot_zone_area))

#gráfico que indica os locais que são definidos pelo atributo 'shot_zone_basic'
ggplot(data = shots) +
  geom_point(mapping = aes(x = loc_x, y = loc_y, color = shot_zone_basic))

#gráfico que indica os locais que são definidos pelo atributo 'shot_zone_range'
ggplot(data = shots) +
  geom_point(mapping = aes(x = loc_x, y = loc_y, color = shot_zone_range))



#TÓPICO 4 - MEDIDAS DE LOCALIDADE
moda(shots$action_type)
moda(shots$combined_shot_type)
moda(shots$game_event_id)
moda(shots$game_id)
moda(shots$playoffs)
moda(shots$season)
moda(shots$shot_type)
moda(shots$shot_zone_area)
moda(shots$shot_zone_basic)
moda(shots$team_id)
moda(shots$team_name)
moda(shots$matchup)
moda(shots$opponent)
moda(shots$shot_id)
moda(shots$period)
moda(shots$shot_zone_range)
moda(shots$game_date)

moda(shots$lat)
media(shots$lat)
mediana(shots$lat)
boxplot(shots$lat, main = "Latitude (atributo 'lat')")

moda(shots$lon)
media(shots$lon)
mediana(shots$lon)
boxplot(shots$lon, main = "Longitude (atributo 'lon')")

moda(shots$loc_x)
media(shots$loc_x)
mediana(shots$loc_x)
boxplot(shots$loc_x, main = "Localização no eixo x (atributo 'loc_x')")

moda(shots$loc_y)
media(shots$loc_y)
mediana(shots$loc_y)
boxplot(shots$loc_y, main = "Localização no eixo y (atributo 'loc_y')")

moda(shots$shot_distance)
media(shots$shot_distance)
mediana(shots$shot_distance)
boxplot(shots$shot_distance, main = "Distância do arremesso (atributo 'shot_distance')")

moda(shots$minutes_remaining)
media(shots$minutes_remaining)
mediana(shots$minutes_remaining)
boxplot(shots$minutes_remaining, main = "Minutos faltantes (atributo 'minutes_remaining')")

moda(shots$seconds_remaining)
media(shots$seconds_remaining)
mediana(shots$seconds_remaining)
boxplot(shots$seconds_remaining, main = "Segundos faltantes (atributo 'seconds_remaining')")

#funções das medidas de localidade
moda <- function(atributo){
  subset (table(atributo), table(atributo) == max (table (atributo)))
}

mediana <- function(atributo){
  median(atributo)
}

media <- function(atributo){
  mean(atributo)
}



#TÓPICO 5 - MEDIDAS DE ESPALHAMENTO
variancia(shots$lat)
desvio(shots$lat)
amplitude(shots$lat)

variancia(shots$lon)
desvio(shots$lon)
amplitude(shots$lon)

variancia(shots$loc_x)
desvio(shots$loc_x)
amplitude(shots$loc_x)

variancia(shots$loc_y)
desvio(shots$loc_y)
amplitude(shots$loc_y)

variancia(shots$shot_distance)
desvio(shots$shot_distance)
amplitude(shots$shot_distance)

variancia(shots$minutes_remaining)
desvio(shots$minutes_remaining)
amplitude(shots$minutes_remaining)

variancia(shots$seconds_remaining)
desvio(shots$seconds_remaining)
amplitude(shots$seconds_remaining)


variancia <- function(atributo){
  var(atributo)
}
desvio <- function(atributo){
  sd(atributo)
}

amplitude <- function(atributo){
  range(atributo)
}



#TÓPICO 6 - MEDIDAS DE DISTRIBUIÇÃO
#chamando a função passando um atributo

#atributos quantitativos
curtose(shots$lat)
obliq(shots$lat)
distrib(shots$lat, "lat")
normal(shots$lat, "lat")

curtose(shots$lon)
obliq(shots$lon)
distrib(shots$lon, "lon")
normal(shots$lon, "lon")

curtose(shots$loc_x)
obliq(shots$loc_x)
distrib(shots$loc_x, "loc_x")
normal(shots$loc_x, "loc_x")

curtose(shots$loc_y)
obliq(shots$loc_y)
distrib(shots$loc_y, "loc_y")
normal(shots$loc_y, "loc_y")

curtose(shots$shot_distance)
obliq(shots$shot_distance)
distrib(shots$shot_distance, "shot_distance")
normal(shots$shot_distance, "shot_distance")

curtose(shots$minutes_remaining)
obliq(shots$minutes_remaining)
distrib(shots$minutes_remaining, "minutes_remaining")
normal(shots$minutes_remaining, "minutes_remaining")

curtose(shots$seconds_remaining)
obliq(shots$seconds_remaining)
distrib(shots$seconds_remaining, "seconds_remaining")
normal(shots$seconds_remaining, "seconds_remaining")

#atributos qualitativos
frequencia(shots$action_type, "action_type")
frequencia(shots$combined_shot_type, "combined_shot_type")
frequencia(shots$game_event_id, "game_event_id")
frequencia(shots$game_id, "game_id")
frequencia(shots$playoffs, "playoffs")
frequencia(shots$season, "season")
frequencia(shots$shot_type, "shot_type")
frequencia(shots$shot_zone_area, "shot_zone_area")
frequencia(shots$shot_zone_basic, "shot_zone_basic")
frequencia(shots$team_id, "team_id")
frequencia(shots$team_name, "team_name")
frequencia(shots$matchup, "matchup")
frequencia(shots$opponent, "opponent")
frequencia(shots$shot_id, "shot_id")
frequencia(shots$period, "period")
frequencia(shots$shot_zone_range, "shot_zone_range")
frequencia(shots$game_date, "game_date")

curtose <- function(atributo){
  kurtosis(atributo)
}

obliq <- function(atributo){
  skewness(atributo)
}

#funcao para plotar a distribuição das classes dos atributos com barras
distrib <- function(atributo, eixoXName){
  hist(atributo, main = eixoXName)
}

#função para plotar a curva normal
normal <- function(atributo, eixoXName){
  media <- mean(atributo)
  desvio <- sd(atributo)
  maxim <- max(atributo)
  minim <- min(atributo)
  ggplot(data = data.frame(x = c(minim, 
                                 maxim)), aes(x)) +
    stat_function(fun = dnorm, 
                  args = list(mean = media, 
                              sd = desvio)) +
    labs(y = "f(x)", x = eixoXName) +
    geom_vline(xintercept = media, color = "red") +
    labs(x = eixoXName) +
    geom_vline(xintercept = desvio, color = "blue")
}

#funcao para atributos qualitativos
frequencia <- function(atributo, eixoXName){
  x <- table(atributo)
  barplot(x,
          xlab = eixoXName,
          ylab = "Frequência")
}



#TÓPICO 7 - SEPARANDO OS CASOS TESTE
#selecionando aleatoriamente os casos teste
teste <- shots[sample(nrow(shots), 6000), replace = FALSE]
#separando os casos de treinamento a partir da diferença entre o teste e 'shots'
shotsTraining <- setdiff(shots, teste)



#TÓPICO 8 e 9 (em partes) - ELIMINAÇÃO DE ATRIBUTOS
shotsTraining$game_event_id <- NULL
shotsTraining$game_id <- NULL
shotsTraining$team_id <- NULL
shotsTraining$team_name <- NULL
shotsTraining$shot_id <- NULL
shotsTraining$matchup <- NULL

teste$game_event_id <- NULL
teste$game_id <- NULL
teste$team_id <- NULL
teste$team_name <- NULL
teste$shot_id <- NULL
teste$matchup <- NULL


#TÓPICO 10 - AMOSTRAGEM
shotsTraining <- shotsTraining[sample(nrow(shotsTraining), 10000), replace = FALSE]
teste <- teste[sample(nrow(teste), 2430), replace = FALSE]

table(shotsTraining$shot_made_flag)
table(teste$shot_made_flag)
