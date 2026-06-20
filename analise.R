library(ggplot2)
library(crosstable)





#####dando nome aos bancos######
nomes <- read.csv("dados/former_names.csv")
gols <- read.csv("dados/goalscorers.csv")
placares <- read.csv("dados/results.csv")
penaltis <- read.csv("dados/shootouts.csv")

#filtra os gols do brasil 
gols_do_brasil <- gols[gols$team == "Brazil" , ]

#faz uma tabela com os goleadores do brasil
table(gols_do_brasil$scorer)

#filtra todos os jogos da copa do mundo
Jogos_de_Copa <- placares[placares$tournament == "FIFA World Cup", ]

#filtra todos os jogos do brasil
Brasil_Jogos <- placares[placares$home_team == "Brazil" | placares$away_team == "Brazil", ]

#filtra os jogos do brasil na copa
Brasil_na_Copa <-  placares[(placares$home_team == "Brazil" | placares$away_team == "Brazil") &
                              placares$tournament == "FIFA World Cup", ]


#estatisticas descritivas dos gols por minuto
mean(gols$minute, na.rm = TRUE)
median(gols$minute, na.rm = TRUE)
hist(gols$minute)
sd(gols$minute, na.rm = TRUE)


Brasil_Copa_Mandante <- Brasil_na_Copa[Brasil_na_Copa$home_team == "Brazil", ]

Brasil_Copa_Visitante <- Brasil_na_Copa[Brasil_na_Copa$away_team == "Brazil", ]


GOls_Brasil_COpa <- sum(Brasil_Copa_Mandante$home_score, Brasil_Copa_Visitante$away_score, na.rm = TRUE)
n_jogos_copa <- length(Brasil_na_Copa$date)
media_gols_por_jogo <- GOls_Brasil_COpa/n_jogos_copa

