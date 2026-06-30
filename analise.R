library(ggplot2)
library(crosstable)





#dando nome aos bancos######
nomes <- read.csv("dados/former_names.csv")
gols <- read.csv("dados/goalscorers.csv")
placares <- read.csv("dados/results.csv")
penaltis <- read.csv("dados/shootouts.csv")

#Carregando os dados com separador virgula
gols  = read.csv2("goalscorers.csv", sep = ",", dec = ".", encoding = "UTF-8")
jogos = read.csv2("results.csv",     sep = ",", dec = ".", encoding = "UTF-8")

################################################################################
# PARTE 1 - DISTRIBUICAO DOS GOLS POR MINUTO (PRO e CONTRA)
#           Comparacao: Brasil x Argentina x Franca x Alemanha x Media
################################################################################

gols = gols[!is.na(gols$minute), ]

#Criando a variavel FAIXA com if else 
gols$faixa = ifelse(gols$minute <= 15, "1-15",
                    ifelse(gols$minute <= 30, "16-30",
                           ifelse(gols$minute <= 45, "31-45",
                                  ifelse(gols$minute <= 60, "46-60",
                                         ifelse(gols$minute <= 75, "61-75",
                                                ifelse(gols$minute <= 90, "76-90", "90+"))))))

#Transformando em factor com a ordem correta das faixas
gols$faixa = factor(gols$faixa,
                    levels = c("1-15","16-30","31-45","46-60","61-75","76-90","90+"))

#Separando os gols PRO de cada selecao
pro_BR = gols[gols$team == "Brazil",    ]
pro_AR = gols[gols$team == "Argentina", ]
pro_FR = gols[gols$team == "France",    ]
pro_AL = gols[gols$team == "Germany",   ]

#Separando os gols CONTRA de cada selecao
contra_BR = gols[(gols$home_team=="Brazil"    | gols$away_team=="Brazil")    & gols$team!="Brazil",    ]
contra_AR = gols[(gols$home_team=="Argentina" | gols$away_team=="Argentina") & gols$team!="Argentina", ]
contra_FR = gols[(gols$home_team=="France"    | gols$away_team=="France")    & gols$team!="France",    ]
contra_AL = gols[(gols$home_team=="Germany"   | gols$away_team=="Germany")   & gols$team!="Germany",   ]

#Tabela comparativa dos gols PRO (% por faixa)
comp_pro = data.frame(
  faixa     = c("1-15","16-30","31-45","46-60","61-75","76-90","90+"),
  Brasil    = round(as.numeric(prop.table(table(pro_BR$faixa))) * 100, 2),
  Argentina = round(as.numeric(prop.table(table(pro_AR$faixa))) * 100, 2),
  Franca    = round(as.numeric(prop.table(table(pro_FR$faixa))) * 100, 2),
  Alemanha  = round(as.numeric(prop.table(table(pro_AL$faixa))) * 100, 2),
  Media     = round(as.numeric(prop.table(table(gols$faixa)))   * 100, 2)
)
cat("\n=== % DOS GOLS PRO POR FAIXA DE MINUTO ===\n")
comp_pro

#Tabela comparativa dos gols CONTRA (% por faixa)
comp_con = data.frame(
  faixa     = c("1-15","16-30","31-45","46-60","61-75","76-90","90+"),
  Brasil    = round(as.numeric(prop.table(table(contra_BR$faixa))) * 100, 2),
  Argentina = round(as.numeric(prop.table(table(contra_AR$faixa))) * 100, 2),
  Franca    = round(as.numeric(prop.table(table(contra_FR$faixa))) * 100, 2),
  Alemanha  = round(as.numeric(prop.table(table(contra_AL$faixa))) * 100, 2),
  Media     = round(as.numeric(prop.table(table(gols$faixa)))   * 100, 2)
)
cat("\n=== % DOS GOLS CONTRA POR FAIXA DE MINUTO ===\n")
comp_con

#histogramas do Brasil
ggplot(pro_BR, aes(x = minute)) +
  geom_histogram(binwidth = 15, boundary = 0, fill = "steelblue", color = "black") +
  labs(x = "Minuto do jogo",
       y = "Frequencia",
       title = "Gols PRO do Brasil por minuto",
       caption = "Fonte: International football results (Kaggle).")

ggplot(contra_BR, aes(x = minute)) +
  geom_histogram(binwidth = 15, boundary = 0, fill = "firebrick", color = "black") +
  labs(x = "Minuto do jogo",
       y = "Frequencia",
       title = "Gols CONTRA (sofridos) do Brasil por minuto",
       caption = "Fonte: International football results (Kaggle).")


################################################################################
# PARTE 2 - ADVERSARIOS
#  Maiores vitimas do Brasil  x  Quem mais marca contra o Brasil
#  (em numeros ABSOLUTOS e RELATIVOS = gols por jogo)
################################################################################

gols_br = gols[gols$home_team == "Brazil" | gols$away_team == "Brazil", ]

gols_br$adversario = ifelse(gols_br$home_team == "Brazil",
                            gols_br$away_team, gols_br$home_team)

pro_goals = gols_br[gols_br$team == "Brazil", ]   
contra_gols = gols_br[gols_br$team != "Brazil", ]   

tab_pro = table(pro_goals$adversario)  
tab_contra = table(contra_gols$adversario)   

#Numero de jogos por adversario (para o RELATIVO), usando o placar

jogos = jogos[!is.na(jogos$home_score) & !is.na(jogos$away_score), ]
jogos_br = jogos[jogos$home_team == "Brazil" | jogos$away_team == "Brazil", ]
jogos_br$adversario = ifelse(jogos_br$home_team == "Brazil",
                             jogos_br$away_team, jogos_br$home_team)
tab_jogos = table(jogos_br$adversario)





adversarios = names(tab_jogos)
jogos_n  = as.numeric(tab_jogos)
pro_n    = as.numeric(tab_pro[adversarios]);  pro_n = ifelse(is.na(pro_n), 0, pro_n)
contra_n    = as.numeric(tab_contra[adversarios]);  contra_n = ifelse(is.na(contra_n), 0, contra_n)

adv = data.frame(adversario = adversarios,
                 jogos  = jogos_n,
                 pro    = pro_n,
                 contra = contra_n)




# Numeros RELATIVOS = gols por jogo (total dividido pelo numero de jogos)
adv$pro_por_jogo    = round(adv$pro    / adv$jogos, 2)
adv$contra_por_jogo = round(adv$contra / adv$jogos, 2)



# Para o RELATIVO, exigimos no minimo 5 jogos 
adv5 = adv[adv$jogos >= 5, ]






#GRAFICO 3: maiores vitimas em ABSOLUTO
vit_abs = adv[order(adv$pro, decreasing = TRUE), ][1:10, ]
# factor com levels na ordem do grafico: menor embaixo, maior em cima depois do coord_flip().
vit_abs$adversario = factor(vit_abs$adversario,
                            levels = vit_abs$adversario[order(vit_abs$pro)])
cat("\n=== TOP 10 VITIMAS (ABSOLUTO) ===\n"); print(vit_abs[, c("adversario","jogos","pro")], row.names = FALSE)



ggplot(vit_abs, aes(x = adversario, y = pro)) +
  geom_bar(stat = "identity", fill = "#009C3B") +
  coord_flip() +
  labs(x = "Adversario", y = "Gols do Brasil",
       title = "Maiores vitimas do Brasil - total de gols (absoluto)")




#GRAFICO 4: maiores vitimas em RELATIVO (gols por jogo, 5+ jogos)
vit_rel = adv5[order(adv5$pro_por_jogo, decreasing = TRUE), ][1:10, ]
vit_rel$adversario = factor(vit_rel$adversario,
                            levels = vit_rel$adversario[order(vit_rel$pro_por_jogo)])
cat("\n=== TOP 10 VITIMAS (RELATIVO, 5+ jogos) ===\n"); print(vit_rel[, c("adversario","jogos","pro","pro_por_jogo")], row.names = FALSE)

ggplot(vit_rel, aes(x = adversario, y = pro_por_jogo)) +
  geom_bar(stat = "identity", fill = "#3CB371") +
  coord_flip() +
  labs(x = "Adversario", y = "Gols do Brasil por jogo",
       title = "Maiores vitimas do Brasil - gols por jogo (relativo, 5+ jogos)")

#GRAFICO 5: quem mais marca CONTRA o Brasil em ABSOLUTO
contra_abs = adv[order(adv$contra, decreasing = TRUE), ][1:10, ]
contra_abs$adversario = factor(contra_abs$adversario,
                            levels = contra_abs$adversario[order(contra_abs$contra)])
cat("\n=== TOP 10 QUE MAIS MARCAM CONTRA (ABSOLUTO) ===\n"); print(contra_abs[, c("adversario","jogos","contra")], row.names = FALSE)

ggplot(contra_abs, aes(x = adversario, y = contra)) +
  geom_bar(stat = "identity", fill = "#C1272D") +
  coord_flip() +
  labs(x = "Adversario", y = "Gols sofridos pelo Brasil",
       title = "Quem mais marca contra o Brasil - total de gols (absoluto)")

#GRAFICO 6: quem mais marca CONTRA o Brasil em RELATIVO (5+ jogos)
contra_rel = adv5[order(adv5$contra_por_jogo, decreasing = TRUE), ][1:10, ]
contra_rel$adversario = factor(contra_rel$adversario,
                            levels = contra_rel$adversario[order(contra_rel$contra_por_jogo)])
cat("\n=== TOP 10 QUE MAIS MARCAM CONTRA (RELATIVO, 5+ jogos) ===\n"); print(contra_rel[, c("adversario","jogos","contra","contra_por_jogo")], row.names = FALSE)

ggplot(contra_rel, aes(x = adversario, y = contra_por_jogo)) +
  geom_bar(stat = "identity", fill = "#E8736F") +
  coord_flip() +
  labs(x = "Adversario", y = "Gols sofridos por jogo",
       title = "Quem mais marca contra o Brasil - gols por jogo (relativo, 5+ jogos)")
