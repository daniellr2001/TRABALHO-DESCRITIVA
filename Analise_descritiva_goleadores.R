library(dplyr)
library(ggplot2)
library(tidyr)

# Ler CSV com goleadores do Brasil na historia
goleadores_do_brasil <- read.csv("dados/goleadores_do_brasil.csv")

###############################################################################
#################### Maiores goleadores do Brasil #############################
###############################################################################

# Criar tabela com maiores goleadores em ordem descrescente
tabela_frequencia <- goleadores_do_brasil %>%
  group_by(scorer) %>%
  summarise(
    total_gols = n(),
    gols_penalti = sum(penalty == TRUE, na.rm = TRUE),
    gols_nao_penalti = total_gols - gols_penalti,
    .groups = 'drop'
  ) %>%
  arrange(desc(total_gols))

top_20 <- head(tabela_frequencia, 20)
print(top_20)

# Grafico de colunas dos top 20 artilheiros da selecao
grafico <- top_20 %>%
  pivot_longer(
    cols = c(gols_penalti, gols_nao_penalti),
    names_to = "tipo",
    values_to = "quantidade"
  ) %>%
  ggplot(aes(x = reorder(scorer, -total_gols), y = quantidade, fill = tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("gols_penalti" = "#A23B72", "gols_nao_penalti" = "#2E86AB"),
    labels = c("gols_penalti" = "Penalti", "gols_nao_penalti" = "Bola rolando")
  ) +
  labs(
    title = "Top 20 artilheiros da Seleção Brasileira",
    subtitle = "Em competições principais (Copa América, Copa do Mundo, Classificatórias...)",
    x = "Jogador",
    y = "Total de gols",
    fill = "Tipo de gol"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

print(grafico)

