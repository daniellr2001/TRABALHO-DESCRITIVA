library(ggplot2)
library(crosstable)
setwd("/home/daniel/Downloads/Datadecompress/archive")

nomes <- read.csv("former_names.csv")
gols <- read.csv("goalscorers.csv")
placares <- read.csv("results.csv")
penaltis <- read.csv("shootouts.csv")


hist(gols$minute)

