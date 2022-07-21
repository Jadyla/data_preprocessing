#NOME: JÁDYLA MARIA CESÁRIO FIRMINO
#7º PERÍODO - ENGENHARIA DE COMPUTAÇÃO
#ANÁLISE DE DADOS - KOBE BRYANT

#Observações:
#O teste e treinamento foram gerados aleatoriamente, por isso em todo início do projeto novos valores
#irão surgir. Com isso, junto ao trabalho foi anexado o conjunto teste e treinamento após amostragem
#sem passar pelos devidos tratamentos. Com os valores lá apresentados é possível gerar os resultados aqui
#presentes passando pelas etapas, realizando a troca da amostragem aleatória para a leitura dos
#dados das tabelas

#os resultados finais representados por 'dataTeste' e 'dataTreino' também foram adicionados à pasta do trabalho,
#como 'teste.csv' e 'treinamento.csv'


install.packages("factoextra")
library("ISwR")
library("stats")
library("factoextra")
library("tidyverse")
library("readr")
library("dplyr")      #pacote utilizado para o 'select()'
library("tidyselect") #pacote para usar o 'everything()', que seleciona todos os atributos
library("e1071")      #pacote para calcular obliquidade e curtose
library("base")

data <- read_csv("data.csv")




#------------------------------------------------------------------------------------------------
#TOPICO 1- COLOCANDO O ATRIBUTO ALVO NA ÚLTIMA COLUNA
shotMade <- select(data, shot_made_flag)

data$shot_made_flag <-NULL

shots <- data %>% 
  mutate(shotMade, shot_made_flag = shot_made_flag)

View(shots)




#------------------------------------------------------------------------------------------------
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




#------------------------------------------------------------------------------------------------
#TÓPICO 4 - MEDIDAS DE LOCALIDADE
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




#------------------------------------------------------------------------------------------------
#TÓPICO 5 - MEDIDAS DE ESPALHAMENTO
#funcoes para as medidas
variancia <- function(atributo){
  var(atributo)
}
desvio <- function(atributo){
  sd(atributo)
}

amplitude <- function(atributo){
  range(atributo)
}

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




#------------------------------------------------------------------------------------------------
#TÓPICO 6 - MEDIDAS DE DISTRIBUIÇÃO
#funcoes da curtose e obliquidade
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




#------------------------------------------------------------------------------------------------
#TÓPICO 7 - SEPARANDO OS CASOS TESTE
#selecionando aleatoriamente os casos teste
teste <- shots[sample(nrow(shots), 6000), replace = FALSE]
#separando os casos de treinamento a partir da diferença entre o teste e 'shots'
shotsTraining <- setdiff(shots, teste)




#------------------------------------------------------------------------------------------------
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




#------------------------------------------------------------------------------------------------
#TÓPICO 10 - AMOSTRAGEM
shotsTraining <- shotsTraining[sample(nrow(shotsTraining), 10000), replace = FALSE]
teste <- teste[sample(nrow(teste), 2430), replace = FALSE]

table(shotsTraining$shot_made_flag)
table(teste$shot_made_flag)
#write.table(shotsTraining, file = "/Users/jadyl/Desktop/ENGENHARIA DE COMPUTAÇÃO/7º PERÌODO/IA/Trabalho 2/amostragemTreinamento.csv", sep=",")
#write.table(teste, file = "/Users/jadyl/Desktop/ENGENHARIA DE COMPUTAÇÃO/7º PERÌODO/IA/Trabalho 2/amostragemTeste.csv", sep=",")




#------------------------------------------------------------------------------------------------
#TÓPICO 11 - BALANCEAMENTO
#não necessário




#------------------------------------------------------------------------------------------------
#TÓPICO 12 - LIMPEZA

#12.b - inconsistentes
View(shotsTraining %>% count(action_type, combined_shot_type, lat, loc_x, loc_y,
                             lon, minutes_remaining, period, playoffs, season, seconds_remaining,
                             shot_distance, shot_type, shot_zone_area, shot_zone_basic,
                             shot_zone_range, game_date, opponent, sort = TRUE))

View(teste %>% count(action_type, combined_shot_type, lat, loc_x, loc_y,
                     lon, minutes_remaining, period, playoffs, season, seconds_remaining,
                     shot_distance, shot_type, shot_zone_area, shot_zone_basic,
                     shot_zone_range, game_date, opponent, sort = TRUE))


#12.c - redundantes
?count()

#exemplo de funcionamento da função count()
#View(shotsTraining %>% 
#  count(action_type, combined_shot_type, sort = TRUE))

View(shotsTraining %>% count(action_type, combined_shot_type, lat, loc_x, loc_y,
                             lon, minutes_remaining, period, playoffs, season, seconds_remaining,
                             shot_distance, shot_type, shot_zone_area, shot_zone_basic,
                             shot_zone_range, game_date, opponent, shot_made_flag, sort = TRUE))

View(teste %>% count(action_type, combined_shot_type, lat, loc_x, loc_y,
                     lon, minutes_remaining, period, playoffs, season, seconds_remaining,
                     shot_distance, shot_type, shot_zone_area, shot_zone_basic,
                     shot_zone_range, game_date, opponent, shot_made_flag, sort = TRUE))



#12.a - ruidosos/outliers
#lat, loc_y, shot_distance

###CONJUNTO TREINAMENTO


#retirando de shot_distance
boxplot(shotsTraining$shot_distance)
aux <- shotsTraining
aux2 <- boxplot.stats(aux$shot_distance)$out
?seq_along
#for para atribuir -1 para todos os valores que são outiliers no atributo shot_distance
for (i in seq_along(aux$shot_distance)) {
  for (j in seq_along(aux2)) {
    if (aux$shot_distance[i] == aux2[j]){
      aux$shot_distance[i] <- -1
    }
  }
}
#vai retirar as linhas que possuem valor -1 no atributo shot_distance (outliers), atribuir e
#depois provar plotando o bloxpot
retiraOutShotDistance <- intersect(shotsTraining, aux)
boxplot(retiraOutShotDistance$shot_distance)


#retirando de lat
boxplot(retiraOutShotDistance$lat)
aux3 <- retiraOutShotDistance
aux4 <- boxplot.stats(aux3$lat)$out

for (i in seq_along(aux3$lat)) {
  for (j in seq_along(aux4)) {
    if (aux3$lat[i] == aux4[j]){
      aux3$lat[i] <- -1
    }
  }
}

retiraOutShotDistanceLat <- intersect(shotsTraining, aux3)
boxplot(retiraOutShotDistanceLat$lat)


#retirando de loc_y
boxplot(retiraOutShotDistanceLat$loc_y)
aux5 <- retiraOutShotDistanceLat
aux6 <- boxplot.stats(aux5$loc_y)$out #nenhum outlier
shotsLimpeza <- retiraOutShotDistanceLat


###CONJUNTO TESTE


#retirando shot_distance
boxplot(teste$shot_distance)
aux7 <- teste
aux8 <- boxplot.stats(aux7$shot_distance)$out

for (i in seq_along(aux7$shot_distance)) {
  for (j in seq_along(aux8)) {
    if (aux7$shot_distance[i] == aux8[j]){
      aux7$shot_distance[i] <- -1
    }
  }
}
testeRetiraShot <- intersect(teste, aux7)
boxplot(testeRetiraShot$shot_distance)


#retirando lat
boxplot(testeRetiraShot$lat) #nenhum outlier


#retirando loc_x
boxplot(testeRetiraShot$loc_y)
aux9 <- testeRetiraShot
aux10 <- boxplot.stats(aux9$loc_y)$out

for (i in seq_along(aux9$loc_y)) {
  for (j in seq_along(aux10)) {
    if (aux9$loc_y[i] == aux10[j]){
      aux9$loc_y[i] <- -1000 #possui em seus valores o -1, por isso a troca
    }
  }
}
aux9
testeRetiraShotLocx <- intersect(teste, aux9)
testeLimpeza <- testeRetiraShotLocx


#12.d - faltantes
shotsLimpeza <- na.omit(shotsLimpeza)
testeLimpeza <- na.omit(testeLimpeza)




#------------------------------------------------------------------------------------------------
#TÓPICO 13 - CONVERSÃO DOS DADOS
#13.a
testeConversao <- testeLimpeza
shotsConversao <- shotsLimpeza
#shot_type - nominal -> binário
for (i in seq_along(testeConversao$shot_type)) {
  if (testeConversao$shot_type[i] == "2PT Field Goal"){
    testeConversao$shot_type[i] = 0
  }
  else{
    testeConversao$shot_type[i] = 1
  }
}
for (i in seq_along(shotsConversao$shot_type)) {
  if (shotsConversao$shot_type[i] == "2PT Field Goal"){
    shotsConversao$shot_type[i] = 0
  }
  else{
    shotsConversao$shot_type[i] = 1
  }
}

#shot_zone_range - ordinal -> numérico
for (i in seq_along(testeConversao$shot_zone_range)) {
  if (testeConversao$shot_zone_range[i] == "Less Than 8 ft."){
    testeConversao$shot_zone_range[i] = 1
  }
  if(testeConversao$shot_zone_range[i] == "8-16 ft."){
    testeConversao$shot_zone_range[i] = 2
  }
  if(testeConversao$shot_zone_range[i] == "16-24 ft."){
    testeConversao$shot_zone_range[i] = 3
  }
  if(testeConversao$shot_zone_range[i] == "24+ ft."){
    testeConversao$shot_zone_range[i] = 4
  }
  if(testeConversao$shot_zone_range[i] == "Back Court Shot"){
    testeConversao$shot_zone_range[i] = 5
  }
}
for (i in seq_along(shotsConversao$shot_zone_range)) {
  if (shotsConversao$shot_zone_range[i] == "Less Than 8 ft."){
    shotsConversao$shot_zone_range[i] = 1
  }
  if(shotsConversao$shot_zone_range[i] == "8-16 ft."){
    shotsConversao$shot_zone_range[i] = 2
  }
  if(shotsConversao$shot_zone_range[i] == "16-24 ft."){
    shotsConversao$shot_zone_range[i] = 3
  }
  if(shotsConversao$shot_zone_range[i] == "24+ ft."){
    shotsConversao$shot_zone_range[i] = 4
  }
  if(shotsConversao$shot_zone_range[i] == "Back Court Shot"){
    shotsConversao$shot_zone_range[i] = 5
  }
}

#season - ordinal -> numérico
for (i in seq_along(testeConversao$season)) {
  if (testeConversao$season[i] == "1996-97"){
    testeConversao$season[i] = 1
  }
  if(testeConversao$season[i] == "1997-98"){
    testeConversao$season[i] = 2
  }
  if(testeConversao$season[i] == "1998-99"){
    testeConversao$season[i] = 3
  }
  if(testeConversao$season[i] == "1999-00"){
    testeConversao$season[i] = 4
  }
  if(testeConversao$season[i] == "2000-01"){
    testeConversao$season[i] = 5
  }
  if(testeConversao$season[i] == "2001-02"){
    testeConversao$season[i] = 6
  }
  if(testeConversao$season[i] == "2002-03"){
    testeConversao$season[i] = 7
  }
  if(testeConversao$season[i] == "2003-04"){
    testeConversao$season[i] = 8
  }
  if(testeConversao$season[i] == "2004-05"){
    testeConversao$season[i] = 9
  }
  if(testeConversao$season[i] == "2005-06"){
    testeConversao$season[i] = 10
  }
  if(testeConversao$season[i] == "2006-07"){
    testeConversao$season[i] = 11
  }
  if(testeConversao$season[i] == "2007-08"){
    testeConversao$season[i] = 12
  }
  if(testeConversao$season[i] == "2008-09"){
    testeConversao$season[i] = 13
  }
  if(testeConversao$season[i] == "2009-10"){
    testeConversao$season[i] = 14
  }
  if(testeConversao$season[i] == "2010-11"){
    testeConversao$season[i] = 15
  }
  if(testeConversao$season[i] == "2011-12"){
    testeConversao$season[i] = 16
  }
  if(testeConversao$season[i] == "2012-13"){
    testeConversao$season[i] = 17
  }
  if(testeConversao$season[i] == "2013-14"){
    testeConversao$season[i] = 18
  }
  if(testeConversao$season[i] == "2014-15"){
    testeConversao$season[i] = 19
  }
  if(testeConversao$season[i] == "2015-16"){
    testeConversao$season[i] = 20
  }
}
for (i in seq_along(shotsConversao$season)) {
  if (shotsConversao$season[i] == "1996-97"){
    shotsConversao$season[i] = 1
  }
  if(shotsConversao$season[i] == "1997-98"){
    shotsConversao$season[i] = 2
  }
  if(shotsConversao$season[i] == "1998-99"){
    shotsConversao$season[i] = 3
  }
  if(shotsConversao$season[i] == "1999-00"){
    shotsConversao$season[i] = 4
  }
  if(shotsConversao$season[i] == "2000-01"){
    shotsConversao$season[i] = 5
  }
  if(shotsConversao$season[i] == "2001-02"){
    shotsConversao$season[i] = 6
  }
  if(shotsConversao$season[i] == "2002-03"){
    shotsConversao$season[i] = 7
  }
  if(shotsConversao$season[i] == "2003-04"){
    shotsConversao$season[i] = 8
  }
  if(shotsConversao$season[i] == "2004-05"){
    shotsConversao$season[i] = 9
  }
  if(shotsConversao$season[i] == "2005-06"){
    shotsConversao$season[i] = 10
  }
  if(shotsConversao$season[i] == "2006-07"){
    shotsConversao$season[i] = 11
  }
  if(shotsConversao$season[i] == "2007-08"){
    shotsConversao$season[i] = 12
  }
  if(shotsConversao$season[i] == "2008-09"){
    shotsConversao$season[i] = 13
  }
  if(shotsConversao$season[i] == "2009-10"){
    shotsConversao$season[i] = 14
  }
  if(shotsConversao$season[i] == "2010-11"){
    shotsConversao$season[i] = 15
  }
  if(shotsConversao$season[i] == "2011-12"){
    shotsConversao$season[i] = 16
  }
  if(shotsConversao$season[i] == "2012-13"){
    shotsConversao$season[i] = 17
  }
  if(shotsConversao$season[i] == "2013-14"){
    shotsConversao$season[i] = 18
  }
  if(shotsConversao$season[i] == "2014-15"){
    shotsConversao$season[i] = 19
  }
  if(shotsConversao$season[i] == "2015-16"){
    shotsConversao$season[i] = 20
  }
}

#game_date - retirado
testeConversao$game_date <- NULL
shotsConversao$game_date <- NULL

#combined_shot_type - nominal -> binario
#verificando o que mais se repete e atribuindo para um vetor
table(testeConversao$combined_shot_type)
repeticaoCombined <- c("Jump Shot", "Layup", "Dunk", "Tip Shot", "Bank Shot", "Hook Shot")
repeticaoCombined

for (i in seq_along(testeConversao$combined_shot_type)) {
  for (j in seq_along(repeticaoCombined)) {
    if (testeConversao$combined_shot_type[i] == repeticaoCombined[j]){
      testeConversao$combined_shot_type[i] <- j
    }
  }
}
for (i in seq_along(shotsConversao$combined_shot_type)) {
  for (j in seq_along(repeticaoCombined)) {
    if (shotsConversao$combined_shot_type[i] == repeticaoCombined[j]){
      shotsConversao$combined_shot_type[i] <- j
    }
  }
}

for (i in seq_along(testeConversao$combined_shot_type)) {
  if (testeConversao$combined_shot_type[i] == 1){
    v <- "100000"
    testeConversao$combined_shot_type[i] <- v
  }
  if (testeConversao$combined_shot_type[i] == 2){
    v <- "010000"
    testeConversao$combined_shot_type[i] <- v
  }
  if (testeConversao$combined_shot_type[i] == 3){
    v <- "001000"
    testeConversao$combined_shot_type[i] <- v
  }
  if (testeConversao$combined_shot_type[i] == 4){
    v <- "000100"
    testeConversao$combined_shot_type[i] <- v
  }
  if (testeConversao$combined_shot_type[i] == 5){
    v <- "000010"
    testeConversao$combined_shot_type[i] <- v
  }
  if (testeConversao$combined_shot_type[i] == 6){
    v <- "000001"
    testeConversao$combined_shot_type[i] <- v
  }
}
for (i in seq_along(shotsConversao$combined_shot_type)) {
  if (shotsConversao$combined_shot_type[i] == 1){
    v <- "100000"
    shotsConversao$combined_shot_type[i] <- v
  }
  if (shotsConversao$combined_shot_type[i] == 2){
    v <- "010000"
    shotsConversao$combined_shot_type[i] <- v
  }
  if (shotsConversao$combined_shot_type[i] == 3){
    v <- "001000"
    shotsConversao$combined_shot_type[i] <- v
  }
  if (shotsConversao$combined_shot_type[i] == 4){
    v <- "000100"
    shotsConversao$combined_shot_type[i] <- v
  }
  if (shotsConversao$combined_shot_type[i] == 5){
    v <- "000010"
    shotsConversao$combined_shot_type[i] <- v
  }
  if (shotsConversao$combined_shot_type[i] == 6){
    v <- "000001"
    shotsConversao$combined_shot_type[i] <- v
  }
}

#shot_zone_basic - nominal -> binario
sort(table(testeConversao$shot_zone_basic))
for (i in seq_along(testeConversao$shot_zone_basic)) {
  if (testeConversao$shot_zone_basic[i] == "Mid-Range"){
    testeConversao$shot_zone_basic[i] = "100000"
  }
  if(testeConversao$shot_zone_basic[i] == "Restricted Area"){
    testeConversao$shot_zone_basic[i] = "010000"
  }
  if(testeConversao$shot_zone_basic[i] == "Above the Break 3"){
    testeConversao$shot_zone_basic[i] = "001000"
  }
  if(testeConversao$shot_zone_basic[i] == "In The Paint (Non-RA)"){
    testeConversao$shot_zone_basic[i] = "000100"
  }
  if(testeConversao$shot_zone_basic[i] == "Right Corner 3"){
    testeConversao$shot_zone_basic[i] = "000010"
  }
  if(testeConversao$shot_zone_basic[i] == "Left Corner 3"){
    testeConversao$shot_zone_basic[i] = "000001"
  }
}
for (i in seq_along(shotsConversao$shot_zone_basic)) {
  if (shotsConversao$shot_zone_basic[i] == "Mid-Range"){
    shotsConversao$shot_zone_basic[i] = "100000"
  }
  if(shotsConversao$shot_zone_basic[i] == "Restricted Area"){
    shotsConversao$shot_zone_basic[i] = "010000"
  }
  if(shotsConversao$shot_zone_basic[i] == "Above the Break 3"){
    shotsConversao$shot_zone_basic[i] = "001000"
  }
  if(shotsConversao$shot_zone_basic[i] == "In The Paint (Non-RA)"){
    shotsConversao$shot_zone_basic[i] = "000100"
  }
  if(shotsConversao$shot_zone_basic[i] == "Right Corner 3"){
    shotsConversao$shot_zone_basic[i] = "000010"
  }
  if(shotsConversao$shot_zone_basic[i] == "Left Corner 3"){
    shotsConversao$shot_zone_basic[i] = "000001"
  }
}


#shot_zone_area - nominal -> binario
sort(table(testeConversao$shot_zone_area))
for (i in seq_along(testeConversao$shot_zone_area)) {
  if (testeConversao$shot_zone_area[i] == "Center(C)"){
    testeConversao$shot_zone_area[i] = "100000"
  }
  if(testeConversao$shot_zone_area[i] == "Right Side Center(RC)"){
    testeConversao$shot_zone_area[i] = "010000"
  }
  if(testeConversao$shot_zone_area[i] == "Right Side(R)"){
    testeConversao$shot_zone_area[i] = "001000"
  }
  if(testeConversao$shot_zone_area[i] == "Left Side Center(LC)"){
    testeConversao$shot_zone_area[i] = "000100"
  }
  if(testeConversao$shot_zone_area[i] == "Left Side(L)"){
    testeConversao$shot_zone_area[i] = "000010"
  }
  if(testeConversao$shot_zone_area[i] == "Back Court(BC)"){
    testeConversao$shot_zone_area[i] = "000001"
  }
}
for (i in seq_along(shotsConversao$shot_zone_area)) {
  if (shotsConversao$shot_zone_area[i] == "Center(C)"){
    shotsConversao$shot_zone_area[i] = "100000"
  }
  if(shotsConversao$shot_zone_area[i] == "Right Side Center(RC)"){
    shotsConversao$shot_zone_area[i] = "010000"
  }
  if(shotsConversao$shot_zone_area[i] == "Right Side(R)"){
    shotsConversao$shot_zone_area[i] = "001000"
  }
  if(shotsConversao$shot_zone_area[i] == "Left Side Center(LC)"){
    shotsConversao$shot_zone_area[i] = "000100"
  }
  if(shotsConversao$shot_zone_area[i] == "Left Side(L)"){
    shotsConversao$shot_zone_area[i] = "000010"
  }
  if(shotsConversao$shot_zone_area[i] == "Back Court(BC)"){
    shotsConversao$shot_zone_area[i] = "000001"
  }
}

#opponent
sort(table(testeConversao$opponent))
for (i in seq_along(testeConversao$opponent)) {
  if (testeConversao$opponent[i] == "PHX"){
    testeConversao$opponent[i] = "100000000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "SAC"){
    testeConversao$opponent[i] = "010000000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "SAS"){
    testeConversao$opponent[i] = "001000000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "MIN"){
    testeConversao$opponent[i] = "000100000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "POR"){
    testeConversao$opponent[i] = "000010000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "DEN"){
    testeConversao$opponent[i] = "000001000000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "UTA"){
    testeConversao$opponent[i] = "000000100000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "HOU"){
    testeConversao$opponent[i] = "000000010000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "GSW"){
    testeConversao$opponent[i] = "000000001000000000000000000000000"
  }
  if(testeConversao$opponent[i] == "MEM"){
    testeConversao$opponent[i] = "000000000100000000000000000000000"
  }
  if(testeConversao$opponent[i] == "LAC"){
    testeConversao$opponent[i] = "000000000010000000000000000000000"
  }
  if(testeConversao$opponent[i] == "BOS"){
    testeConversao$opponent[i] = "000000000001000000000000000000000"
  }
  if(testeConversao$opponent[i] == "DAL"){
    testeConversao$opponent[i] = "000000000000100000000000000000000"
  }
  if(testeConversao$opponent[i] == "SEA"){
    testeConversao$opponent[i] = "000000000000010000000000000000000"
  }
  if(testeConversao$opponent[i] == "TOR"){
    testeConversao$opponent[i] = "000000000000001000000000000000000"
  }
  if(testeConversao$opponent[i] == "CHA"){
    testeConversao$opponent[i] = "000000000000000100000000000000000"
  }
  if(testeConversao$opponent[i] == "PHI"){
    testeConversao$opponent[i] = "000000000000000010000000000000000"
  }
  if(testeConversao$opponent[i] == "NYK"){
    testeConversao$opponent[i] = "000000000000000001000000000000000"
  }
  if(testeConversao$opponent[i] == "CLE"){
    testeConversao$opponent[i] = "000000000000000000100000000000000"
  }
  if(testeConversao$opponent[i] == "WAS"){
    testeConversao$opponent[i] = "000000000000000000010000000000000"
  }
  if(testeConversao$opponent[i] == "ORL"){
    testeConversao$opponent[i] = "000000000000000000001000000000000"
  }
  if(testeConversao$opponent[i] == "DET"){
    testeConversao$opponent[i] = "000000000000000000000100000000000"
  }
  if(testeConversao$opponent[i] == "IND"){
    testeConversao$opponent[i] = "000000000000000000000010000000000"
  }
  if(testeConversao$opponent[i] == "OKC"){
    testeConversao$opponent[i] = "000000000000000000000001000000000"
  }
  if(testeConversao$opponent[i] == "MIA"){
    testeConversao$opponent[i] = "000000000000000000000000100000000"
  }
  if(testeConversao$opponent[i] == "ATL"){
    testeConversao$opponent[i] = "000000000000000000000000010000000"
  }
  if(testeConversao$opponent[i] == "NJN"){
    testeConversao$opponent[i] = "000000000000000000000000001000000"
  }
  if(testeConversao$opponent[i] == "CHI"){
    testeConversao$opponent[i] = "000000000000000000000000000100000"
  }
  if(testeConversao$opponent[i] == "NOH"){
    testeConversao$opponent[i] = "000000000000000000000000000010000"
  }
  if(testeConversao$opponent[i] == "MIL"){
    testeConversao$opponent[i] = "000000000000000000000000000001000"
  }
  if(testeConversao$opponent[i] == "NOP"){
    testeConversao$opponent[i] = "000000000000000000000000000000100"
  }
  if(testeConversao$opponent[i] == "VAN"){
    testeConversao$opponent[i] = "000000000000000000000000000000010"
  }
  if(testeConversao$opponent[i] == "BKN"){
    testeConversao$opponent[i] = "000000000000000000000000000000001"
  }
}
for (i in seq_along(shotsConversao$opponent)) {
  if (shotsConversao$opponent[i] == "PHX"){
    shotsConversao$opponent[i] = "100000000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "SAC"){
    shotsConversao$opponent[i] = "010000000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "SAS"){
    shotsConversao$opponent[i] = "001000000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "MIN"){
    shotsConversao$opponent[i] = "000100000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "POR"){
    shotsConversao$opponent[i] = "000010000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "DEN"){
    shotsConversao$opponent[i] = "000001000000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "UTA"){
    shotsConversao$opponent[i] = "000000100000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "HOU"){
    shotsConversao$opponent[i] = "000000010000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "GSW"){
    shotsConversao$opponent[i] = "000000001000000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "MEM"){
    shotsConversao$opponent[i] = "000000000100000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "LAC"){
    shotsConversao$opponent[i] = "000000000010000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "BOS"){
    shotsConversao$opponent[i] = "000000000001000000000000000000000"
  }
  if(shotsConversao$opponent[i] == "DAL"){
    shotsConversao$opponent[i] = "000000000000100000000000000000000"
  }
  if(shotsConversao$opponent[i] == "SEA"){
    shotsConversao$opponent[i] = "000000000000010000000000000000000"
  }
  if(shotsConversao$opponent[i] == "TOR"){
    shotsConversao$opponent[i] = "000000000000001000000000000000000"
  }
  if(shotsConversao$opponent[i] == "CHA"){
    shotsConversao$opponent[i] = "000000000000000100000000000000000"
  }
  if(shotsConversao$opponent[i] == "PHI"){
    shotsConversao$opponent[i] = "000000000000000010000000000000000"
  }
  if(shotsConversao$opponent[i] == "NYK"){
    shotsConversao$opponent[i] = "000000000000000001000000000000000"
  }
  if(shotsConversao$opponent[i] == "CLE"){
    shotsConversao$opponent[i] = "000000000000000000100000000000000"
  }
  if(shotsConversao$opponent[i] == "WAS"){
    shotsConversao$opponent[i] = "000000000000000000010000000000000"
  }
  if(shotsConversao$opponent[i] == "ORL"){
    shotsConversao$opponent[i] = "000000000000000000001000000000000"
  }
  if(shotsConversao$opponent[i] == "DET"){
    shotsConversao$opponent[i] = "000000000000000000000100000000000"
  }
  if(shotsConversao$opponent[i] == "IND"){
    shotsConversao$opponent[i] = "000000000000000000000010000000000"
  }
  if(shotsConversao$opponent[i] == "OKC"){
    shotsConversao$opponent[i] = "000000000000000000000001000000000"
  }
  if(shotsConversao$opponent[i] == "MIA"){
    shotsConversao$opponent[i] = "000000000000000000000000100000000"
  }
  if(shotsConversao$opponent[i] == "ATL"){
    shotsConversao$opponent[i] = "000000000000000000000000010000000"
  }
  if(shotsConversao$opponent[i] == "NJN"){
    shotsConversao$opponent[i] = "000000000000000000000000001000000"
  }
  if(shotsConversao$opponent[i] == "CHI"){
    shotsConversao$opponent[i] = "000000000000000000000000000100000"
  }
  if(shotsConversao$opponent[i] == "NOH"){
    shotsConversao$opponent[i] = "000000000000000000000000000010000"
  }
  if(shotsConversao$opponent[i] == "MIL"){
    shotsConversao$opponent[i] = "000000000000000000000000000001000"
  }
  if(shotsConversao$opponent[i] == "NOP"){
    shotsConversao$opponent[i] = "000000000000000000000000000000100"
  }
  if(shotsConversao$opponent[i] == "VAN"){
    shotsConversao$opponent[i] = "000000000000000000000000000000010"
  }
  if(shotsConversao$opponent[i] == "BKN"){
    shotsConversao$opponent[i] = "000000000000000000000000000000001"
  }
}

#action_type - nominal -> nominal/numérico (necessária conversão para binário)
table(shotsConversao$action_type)
actionTypes <- c("Jump Shot", "Layup Shot", "Driving Layup Shot", "Turnaround Jump Shot",
                 "Fadeaway Jump Shot", "Running Jump Shot", "Turnaround Fadeaway shot", 
                 "Pullup Jump shot", "Reverse Layup Shot", "Jump Bank Shot", "Slam Dunk Shot",
                 "Dunk Shot", "Driving Dunk Shot", "Step Back Jump shot", "Tip Shot", "Floating Jump shot",
                 "Reverse Dunk Shot", "Finger Roll Layup Shot", "Driving Reverse Layup Shot", 
                 "Driving Finger Roll Shot", "Alley Oop Layup shot", "Alley Oop Dunk Shot", "Running Layup Shot",
                 "Reverse Slam Dunk Shot", "Pullup Bank shot", "Jump Hook Shot", "Hook Shot", "Turnaround Bank shot",
                 "Running Hook Shot", "Fadeaway Bank shot", "Driving Jump shot", "Driving Finger Roll Layup Shot",
                 "Running Reverse Layup Shot", "Running Finger Roll Layup Shot", "Running Dunk Shot", "Running Bank shot",
                 "Follow Up Dunk Shot", "Finger Roll Shot", "Driving Slam Dunk Shot", "Driving Hook Shot",
                 "Driving Floating Jump Shot", "Cutting Layup Shot", "Driving Bank shot", "Hook Bank Shot",
                 "Putback Dunk Shot", "Putback Layup Shot", "Putback Slam Dunk Shot", "Running Finger Roll Shot",
                 "Running Pull-Up Jump Shot", "Tip Layup Shot", "Turnaround Finger Roll Shot", "Turnaround Hook Shot")

for (i in seq_along(testeConversao$action_type)) {
  for (j in seq_along(actionTypes)) {
    if (testeConversao$action_type[i] == actionTypes[j]){
      testeConversao$action_type[i] <- j
    }
  }
}
class(testeConversao$action_type)

for (i in seq_along(shotsConversao$action_type)) {
  for (j in seq_along(actionTypes)) {
    if (shotsConversao$action_type[i] == actionTypes[j]){
      shotsConversao$action_type[i] <- j
    }
  }
}

class(shotsConversao$action_type)

#13.b
#minutes_remaining
#funcao para reescala
#seconds_remaining, minutes_remaining, loc_y, loc_x, shot_distance, lon, lat

reescala <- function(maximo, minimo, dmax, dmin, atributo){
  return(dmin + ((dmax - dmin)/(maximo - minimo)) * (atributo - minimo))
}

#funcao de padronização
padronizacao <- function(atributo){
  media <- mean(atributo)
  desvioP <- sd(atributo)
  
  return((atributo - media)/desvioP)
}

#minutes_remaining
testeConversao$minutes_remaining <- reescala(max(testeConversao$minutes_remaining), 
                                             min(testeConversao$minutes_remaining),
                                             1, 0, testeConversao$minutes_remaining)
shotsConversao$minutes_remaining <- reescala(max(shotsConversao$minutes_remaining), 
                                             min(shotsConversao$minutes_remaining),
                                             1, 0, shotsConversao$minutes_remaining)
#period
testeConversao$period <- reescala(max(testeConversao$period), 
                                             min(testeConversao$period),
                                             1, 0, testeConversao$period)
shotsConversao$period <- reescala(max(shotsConversao$period), 
                                             min(shotsConversao$period),
                                             1, 0, shotsConversao$period)
#seconds_remaining
testeConversao$seconds_remaining <- reescala(max(testeConversao$seconds_remaining), 
                                             min(testeConversao$seconds_remaining),
                                             1, 0, testeConversao$seconds_remaining)
shotsConversao$seconds_remaining <- reescala(max(shotsConversao$seconds_remaining), 
                                             min(shotsConversao$seconds_remaining),
                                             1, 0, shotsConversao$seconds_remaining)
#shot_zone_range
testeConversao$shot_zone_range <- as.numeric(testeConversao$shot_zone_range)
shotsConversao$shot_zone_range <- as.numeric(shotsConversao$shot_zone_range)

testeConversao$shot_zone_range <- reescala(max(testeConversao$shot_zone_range), 
                                             min(testeConversao$shot_zone_range),
                                             1, 0, testeConversao$shot_zone_range)
shotsConversao$shot_zone_range <- reescala(max(shotsConversao$shot_zone_range), 
                                             min(shotsConversao$shot_zone_range),
                                             1, 0, shotsConversao$shot_zone_range)

#shot_distance
testeConversao$shot_distance <- reescala(max(testeConversao$shot_distance), 
                                           min(testeConversao$shot_distance),
                                           1, 0, testeConversao$shot_distance)
shotsConversao$shot_distance <- reescala(max(shotsConversao$shot_distance), 
                                           min(shotsConversao$shot_distance),
                                           1, 0, shotsConversao$shot_distance)

#season
testeConversao$season <- as.numeric(testeConversao$season)
shotsConversao$season <- as.numeric(shotsConversao$season)

testeConversao$season <- reescala(max(testeConversao$season), 
                                         min(testeConversao$season),
                                         1, 0, testeConversao$season)
shotsConversao$season <- reescala(max(shotsConversao$season), 
                                         min(shotsConversao$season),
                                         1, 0, shotsConversao$season)

#lat
testeConversao$lat <- reescala(max(testeConversao$lat), 
                                  min(testeConversao$lat),
                                  1, 0, testeConversao$lat)
shotsConversao$lat <- reescala(max(shotsConversao$lat), 
                                  min(shotsConversao$lat),
                                  1, 0, shotsConversao$lat)

#lon
testeConversao$lon <- reescala(max(testeConversao$lon), 
                               min(testeConversao$lon),
                               1, 0, testeConversao$lon)
shotsConversao$lon <- reescala(max(shotsConversao$lon), 
                               min(shotsConversao$lon),
                               1, 0, shotsConversao$lon)

#loc_x
testeConversao$loc_x <- reescala(max(testeConversao$loc_x), 
                               min(testeConversao$loc_x),
                               1, 0, testeConversao$loc_x)
shotsConversao$loc_x <- reescala(max(shotsConversao$loc_x), 
                               min(shotsConversao$loc_x),
                               1, 0, shotsConversao$loc_x)

#loc_y
testeConversao$loc_y <- reescala(max(testeConversao$loc_y), 
                                 min(testeConversao$loc_y),
                                 1, 0, testeConversao$loc_y)
shotsConversao$loc_y <- reescala(max(shotsConversao$loc_y), 
                                 min(shotsConversao$loc_y),
                                 1, 0, shotsConversao$loc_y)

testeDimensao <- testeConversao
shotsDimensao <- shotsConversao




#------------------------------------------------------------------------------------------------
#TÓPICO 14 - REDIMENSIONALIDADE


#teste
dados_pca <- testeDimensao[,c(3:8, 10:12, 16)]
sapply(dados_pca, sd)

pca_cov <- prcomp(dados_pca)
summary(pca_cov)

pca_corr <- prcomp(dados_pca, center = TRUE, scale = TRUE, rank. = 2)
summary(pca_corr)

fviz_eig(pca_corr)

summary(pca_corr)$rotation
summary(pca_corr)$x

fviz_pca_ind(pca_corr,
             col.ind = "cos2", #Cor pela qualidade de representação
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Texto não sobreposto
             legend.title = "Representação"
)

fviz_pca_var(pca_corr,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribuição"
)

testeDimensao[,c(3:8, 10:12, 16)] <- NULL
testeDimensao <- testeDimensao %>% 
  mutate(testeDimensao, summary(pca_corr)$x[,1])
testeDimensao <- testeDimensao %>% 
  mutate(testeDimensao, summary(pca_corr)$x[,2])


#treinamento
dados_pca <- shotsDimensao[,c(3:8, 10:12, 16)]
sapply(dados_pca, sd)

pca_cov <- prcomp(dados_pca)
summary(pca_cov)

pca_corr <- prcomp(dados_pca, center = TRUE, scale = TRUE, rank. = 2)
summary(pca_corr)

fviz_eig(pca_corr)

summary(pca_corr)$rotation
summary(pca_corr)$x[,1] #criar duas colunas

fviz_pca_ind(pca_corr,
             col.ind = "cos2", #Cor pela qualidade de representação
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Texto não sobreposto
             legend.title = "Representação"
)

fviz_pca_var(pca_corr,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribuição"
)

shotsDimensao[,c(3:8, 10:12, 16)] <- NULL
shotsDimensao <- shotsDimensao %>% 
  mutate(shotsDimensao, summary(pca_corr)$x[,1])
shotsDimensao <- shotsDimensao %>% 
  mutate(shotsDimensao, summary(pca_corr)$x[,2])




#------------------------------------------------------------------------------------------------
#CONCLUSÃO
dataTeste <- testeDimensao
dataTreino <- shotsDimensao
#write.table(testeConversao, file = "/Users/jadyl/Desktop/ENGENHARIA DE COMPUTAÇÃO/7º PERÌODO/IA/Trabalho 2/teste.csv", sep=",", row.names = FALSE)
#write.table(shotsConversao, file = "/Users/jadyl/Desktop/ENGENHARIA DE COMPUTAÇÃO/7º PERÌODO/IA/Trabalho 2/treino.csv", sep=",", row.names = FALSE)