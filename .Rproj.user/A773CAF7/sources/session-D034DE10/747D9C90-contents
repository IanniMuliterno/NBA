#https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
library(tidyverse)
library(summarytools)
library(ggplot2)
library(caret)
library(pROC)

convert_min <- function(x) {
  if (is.na(x)) {
    return(0)
  }
  x <- unlist(strsplit(as.character(x), ':'))
  if (length(x) < 2) {
    return(as.integer(x[1]))
  } else {
    return(as.integer(x[1])*60 + as.integer(x[2]))
  }
}

#dicionário de metricas
stats_cols <- list(
  FGM = 'Field Goals Made',
  FGA = 'Field Goals Attempted',
  FG_PCT = 'Field Goal Percentage',
  FG3M = 'Three Pointers Made',
  FG3A = 'Three Pointers Attempted',
  FG3_PCT = 'Three Point Percentage',
  FTM = 'Free Throws Made',
  FTA = 'Free Throws Attempted',
  FT_PCT = 'Free Throw Percentage',
  OREB = 'Offensive Rebounds',
  DREB = 'Defensive Rebounds',
  REB = 'Rebounds',
  AST = 'Assists',
  TO = 'Turnovers',
  STL = 'Steals',
  BLK = 'Blocked Shots',
  PF = 'Personal Foul',
  PTS = 'Points',
  PLUS_MINUS = 'Plus-Minus'
)



game_details <- read.csv('NBA_data/games_details.csv')
players <- read.csv('NBA_data/players.csv')
games <- read.csv('NBA_data/games.csv') 
ranking <- read.csv('NBA_data/ranking.csv')
teams <- read.csv('NBA_data/teams.csv')

head(game_details)
names(game_details)
view(dfSummary(game_details))


games <- games[!duplicated(games$GAME_ID),]
#rank de jogadores com mais jogos e tempo em campo
game_details |>
  mutate(min2 = ifelse(grepl('-',MIN) | MIN == '',0,sapply(MIN, convert_min))) |> 
  group_by(PLAYER_NAME) |> 
  summarise(n=n_distinct(GAME_ID),
            segundos_jogados = sum(min2)) |>
  arrange(desc(n))

#comparar o jogar mais popular com os outros
jogador_interesse <- "LeBron James"

# Calcular a média de cada estatística para todos os jogadores, exceto o jogador de interesse
media_geral <- game_details  |> 
  filter(PLAYER_NAME != jogador_interesse) |> 
  select(-c(1:10)) |> 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

# Obter as estatísticas do jogador de interesse
estatisticas_jogador <- game_details |> 
  filter(PLAYER_NAME == jogador_interesse) |> 
  select(-c(1:10)) |> 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

# Comparar as estatísticas do jogador de interesse com a média geral dos outros jogadores
comparacao <- bind_rows(estatisticas_jogador, media_geral) |> 
  mutate(jogador = c(jogador_interesse,'media_geral')) |> 
  pivot_longer(1:19) |> 
  pivot_wider(names_from = jogador,values_from = value)

  # Criar o gráfico de barras
  ggplot(data = comparacao, aes(x = name)) +
  geom_bar(aes(y = media_geral), stat = "identity", position = "dodge", fill = "blue") +
  geom_bar(aes(y = `LeBron James`), stat = "identity", position = "dodge", fill = "red") +
  labs(title = "Comparação das Estatísticas",
       x = "Estatísticas",
       y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format())

  # Transformar a lista em um vetor nomeado
  stats_cols_vector <- unlist(stats_cols)
  # Traduzir as siglas na coluna 'name'
  comparacao <- comparacao |>
    mutate(name = recode(name, !!!stats_cols_vector))
  
  
  # Ordenar o dataframe com base na diferença absoluta
  comparacao <- comparacao |> 
    mutate(diferenca_absoluta = `LeBron James` - media_geral) |> 
    arrange(desc(diferenca_absoluta))
  # Criar o gráfico de barras horizontais
  ggplot(data = comparacao, aes(y = fct_reorder(name, diferenca_absoluta))) +
    geom_bar(aes(x = media_geral), stat = "identity", position = "dodge", fill = "blue") +
    geom_bar(aes(x = -`LeBron James`), stat = "identity", position = "dodge", fill = "red") +
    labs(title = "Comparação das Estatísticas",
         y = "Estatísticas",
         x = "Valor") +
    theme_minimal() +
    scale_x_continuous(labels = function(x) abs(x))
  
  
  #############################
  # análise de contribuição # em processo
  winning_teams <- games |>
    mutate(TEAM_ID = ifelse(HOME_TEAM_WINS == 1, HOME_TEAM_ID, VISITOR_TEAM_ID),
           y=1) |>
    select(TEAM_ID,GAME_ID,y)
  
  # game_details2 <- game_details  |> 
  #   pivot_longer(11:29) |> 
  #   mutate(name = recode(name, !!!stats_cols_vector))
  
  #considerar plus_minus?
  data_joined <- game_details |>
    select(TEAM_ID, GAME_ID, 11:29) |>
    mutate(PLUS_MINUS = as.numeric(PLUS_MINUS)) |> 
    group_by(TEAM_ID,GAME_ID) |> 
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) |> 
    left_join(winning_teams, by = c("TEAM_ID","GAME_ID")) |> 
    mutate(y = ifelse(is.na(y),0,y))
  
  
  # Preparar os dados
  train_data <- data_joined |> ungroup() |> 
    select(-TEAM_ID, -GAME_ID,-FG_PCT,-FG3_PCT,-FGM,-FG3M,-REB,-PTS,-PLUS_MINUS)
  
  # Dividir os dados em conjunto de treinamento e teste
  set.seed(123)
  trainIndex <- caret::createDataPartition(train_data$y, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  train_set <- train_data[ trainIndex,]
  test_set  <- train_data[-trainIndex,]
  
  # Treinar o modelo
  model <- glm(y ~ ., data = train_set, family = binomial)
  summary(model)
  # Avaliar o modelo
  predictions <- predict(model, test_set, type = "response")
  hist(predictions)
  
  test_set |> 
    mutate(predictions) |> 
    ggplot(aes(x=predictions,fill=as.character(y)))+
    geom_histogram(alpha=0.5)+
    labs(title = "Predição vs Vitórias e Perdas verdadeiras",
         y = "qtd",
         x = "Prob prediction",
         fill = "Vitórias (Azul) e Perdas (Verm)") +
    theme_minimal()
  
  roc_curve <- roc(test_set$y, predictions)
  coords(roc_curve, "best")
  predictions <- ifelse(predictions < 0.4772611,0,1)
  
  
  caret::confusionMatrix(as.factor(predictions), as.factor(test_set$y))
  
  
  plotty <- OddsPlotty::odds_plot(model,title = "Odds Plot")
  plotty$odds_plot
  
  
  