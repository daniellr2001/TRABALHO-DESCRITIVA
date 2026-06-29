library(dplyr)
library(ggplot2)
library(janitor)

setwd("C:/Users/mathe/OneDrive/Desktop/Descritiva/Trabalho_Final")
dados <- read.csv("results.csv")

tabela_resultados <- dados %>%
  # Seleciona apenas jogos que o Brasil participou
  filter(home_team == "Brazil" | away_team == "Brazil") %>% 
  
  # Mantém apenas as linhas onde não há NA nos placares
  filter(!is.na(home_score) & !is.na(away_score)) %>% 
  
  mutate(
    # Separa os gols do Brasil e os gols do adversário
    gols_brasil = ifelse(home_team == "Brazil", home_score, away_score),
    gols_adversario = ifelse(home_team == "Brazil", away_score, home_score),
    
       local_jogo = case_when(
      neutral == TRUE ~ "Neutro",              # Se a coluna neutral for Verdadeira
      country == "Brazil" ~ "Em Casa",         # Se não é neutro e o país sede é o Brasil
      TRUE ~ "Fora de Casa"                    # Tudo que sobrar (não neutro e fora do Brasil)
    ),
    
    # Cria a coluna de Resultados
    resultado = case_when(
      gols_brasil > gols_adversario ~ "Vitória",
      gols_brasil == gols_adversario ~ "Empate",
      gols_brasil < gols_adversario ~ "Derrota"
    )
  )

#Tabela de Frequência (Para o Gráfico)
resumo_frequencia <- tabela_resultados %>% 
  group_by(local_jogo, resultado) %>%  
  summarise(frequencia_absoluta = n(), .groups = 'drop') %>% 
  group_by(local_jogo) %>%  
  mutate(
    porcentagem = (frequencia_absoluta / sum(frequencia_absoluta)) * 100,
    porcentagem = round(porcentagem, 2) 
  )

print(resumo_frequencia)

#Gráfico de Barras
ggplot(resumo_frequencia, aes(x = local_jogo, y = porcentagem, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0(porcentagem, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  
  labs(title = "Desempenho da Seleção Brasileira (Em Casa, Fora e Neutro)",
       x = "Local do Jogo",
       y = "Porcentagem (%)",
       fill = "Resultado") +
  theme_minimal() +
  scale_fill_manual(values = c("Derrota" = "#F8766D", "Empate" = "#B79F00", "Vitória" = "#00BA38"))

#Tabela de Contingência Formatada (Por Linha)
tabela_resultados %>%
  tabyl(local_jogo, resultado) %>%       
  adorn_totals(c("row", "col")) %>%      
  adorn_percentages("row") %>%           
  adorn_pct_formatting(digits = 1) %>%   
  adorn_ns

#Prepara os dados (Porcentagem por Local)
dados_grafico_linha <- tabela_resultados %>%
  group_by(local_jogo, resultado) %>%
  summarise(frequencia = n(), .groups = 'drop') %>%
  group_by(local_jogo) %>% # O foco aqui é o local
  mutate(porcentagem = round((frequencia / sum(frequencia)) * 100, 1))

#Tabela de Contingência Formatada (Por Coluna)
tabela_resultados %>%
  tabyl(local_jogo, resultado) %>%       
  adorn_totals(c("row", "col")) %>%      
  # A MUDANÇA É AQUI: calculando a % por coluna (resultado)
  adorn_percentages("col") %>%           
  adorn_pct_formatting(digits = 1) %>%   
  adorn_ns()

#Prepara os dados (Porcentagem por Resultado)
dados_grafico_coluna <- tabela_resultados %>%
  group_by(resultado, local_jogo) %>%
  summarise(frequencia = n(), .groups = 'drop') %>%
  group_by(resultado) %>% # O foco aqui mudou para o resultado
  mutate(porcentagem = round((frequencia / sum(frequencia)) * 100, 1))

# Gráfico de Resultados x Local do Jogo
ggplot(dados_grafico_coluna, aes(x = resultado, y = porcentagem, fill = local_jogo)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = paste0(porcentagem, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  labs(title = "Onde a Seleção mais vence, empata ou perde?",
       x = "Resultado Final",
       y = "Porcentagem (%)",
       fill = "Local do Jogo") +
  theme_minimal() +
  # Usei cores diferentes aqui para representar os Locais (Azul, Cinza e Laranja) 
  # para não confundir com as cores de Vitória/Empate/Derrota
  scale_fill_manual(values = c("Em Casa" = "#2C3E50", "Fora de Casa" = "#E67E22", "Neutro" = "#95A5A6"))