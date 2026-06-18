library(ggplot2)
library(crosstable)

nomes <- read.csv("dados/former_names.csv")
gols <- read.csv("dados/goalscorers.csv")
placares <- read.csv("dados/results.csv")
penaltis <- read.csv("dados/shootouts.csv")

hist(gols$minute)

