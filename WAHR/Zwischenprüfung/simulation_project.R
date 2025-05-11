# Teams und aktuelle Werte
teams <- c("FC Basel", "Servette", "Young Boys", "FC Luzern", "FC Lugano", "FC Lausanne-Sport", 
           "FC St. Gallen", "FC Zürich", "FC Sion", "Grasshoppers", "Yverdon Sport FC", "FC Winterthur")

# Tore und Gegentore pro Spiel (33 Spiele)
avg_goals_scored <- c(2.18, 1.58, 1.48, 1.85, 1.45, 1.58, 1.39, 1.33, 1.24, 1.06, 1.00, 0.97)
avg_goals_conceded <- c(0.97, 1.30, 1.27, 1.55, 1.42, 1.33, 1.30, 1.45, 1.55, 1.39, 1.73, 1.85)

# Durchschnittsalter
avg_age <- c(24.0, 26.5, 25.5, 24.5, 25.2, 24.6, 26.4, 24.5, 26.5, 25.0, 26.0, 27.0)

# Marktwert in Millionen Euro
market_value <- c(2.14, 1.09, 2.43, 1.13, 2.09, 1.48, 1.26, 0.95, 0.73, 0.747, 0.814, 0.444)

# Normierung auf Mittelwert = 1
market_factors <- market_value / mean(market_value)
age_factors <- avg_age / mean(avg_age)

team_stats <- data.frame(Team = teams, GF = avg_goals_scored, GA = avg_goals_conceded,
                         MarketFactor = market_factors, AgeFactor = age_factors)

# Basis-Verletzungswahrscheinlichkeit
injury_probability_base <- 0.05
injury_penalty <- 0.9

# Funktion: Simuliere ob ein Team eine Verletzung hat (altersabhängig)
simulate_injury_status <- function(prob_base, age_factor) {
  ifelse(runif(1) < (prob_base * age_factor), injury_penalty, 1)
}

# Funktion: Simuliere ein Spiel mit Marktwert, Heimvorteil und Verletzungen
simulate_match_with_injuries <- function(home_team, away_team, team_stats) {
  injury_factors <- sapply(team_stats$Team, function(team) simulate_injury_status(injury_probability_base, team_stats$AgeFactor[team_stats$Team == team]))
  injury_factors <- setNames(injury_factors, team_stats$Team)
  
  base_lambda_home <- team_stats$GF[team_stats$Team == home_team] * 1.1 # Heimvorteil
  base_lambda_away <- team_stats$GF[team_stats$Team == away_team]
  
  lambda_home <- base_lambda_home * injury_factors[home_team] * team_stats$MarketFactor[team_stats$Team == home_team]
  lambda_away <- base_lambda_away * injury_factors[away_team] * team_stats$MarketFactor[team_stats$Team == away_team]
  
  goals_home <- rpois(1, lambda_home)
  goals_away <- rpois(1, lambda_away)
  
  result <- ifelse(goals_home > goals_away, "Home Win",
                   ifelse(goals_home < goals_away, "Away Win", "Draw"))
  
  return(data.frame(Home = home_team, Away = away_team, Goals_Home = goals_home, Goals_Away = goals_away, Result = result))
}

# Funktion: Simuliere eine Saison mit 44 Spielen pro Team
simulate_season <- function(team_stats, n_simulations = 1000) {
  teams <- team_stats$Team
  n_teams <- length(teams)
  standings <- matrix(0, nrow = n_simulations, ncol = n_teams)
  colnames(standings) <- teams
  ranks <- matrix(0, nrow = n_simulations, ncol = n_teams)
  colnames(ranks) <- teams
  
  for (i in 1:n_simulations) {
    points <- setNames(rep(0, n_teams), teams)
    
    for (home in teams) {
      for (away in teams) {
        if (home != away) {
          for (r in 1:2) {  # Zwei Spiele: Heim und Auswärts
            match <- simulate_match_with_injuries(home, away, team_stats)
            
            if (match$Result == "Home Win") {
              points[home] <- points[home] + 3
            } else if (match$Result == "Away Win") {
              points[away] <- points[away] + 3
            } else {
              points[home] <- points[home] + 1
              points[away] <- points[away] + 1
            }
          }
        }
      }
    }
    standings[i, ] <- points
    ranks[i, ] <- rank(-points, ties.method = "min")
  }
  
  return(list(points = standings, ranks = ranks))
}

# Simulation starten
simulation <- simulate_season(team_stats, n_simulations = 1000)
points <- simulation$points
ranks <- simulation$ranks

# Meisterwahrscheinlichkeiten
champions <- apply(points, 1, function(x) names(which.max(x)))
champion_counts <- table(champions)
champion_probabilities <- prop.table(champion_counts)

par(mfrow = c(1,1))
barplot(champion_probabilities, main = "Meisterwahrscheinlichkeiten Super League 2024/25", las = 2, col = "skyblue")

# Platzierungswahrscheinlichkeiten für alle Teams
position_probs <- matrix(0, nrow = length(teams), ncol = length(teams))
rownames(position_probs) <- teams
colnames(position_probs) <- paste0("Platz ", 1:length(teams))

for (i in 1:length(teams)) {
  for (pos in 1:length(teams)) {
    position_probs[i, pos] <- sum(ranks[, i] == pos) / nrow(ranks)
  }
}

# Visualisierung für alle Teams
old_par <- par(mfrow = c(4, 3), mar = c(4, 4, 2, 1))
for (i in 1:length(teams)) {
  barplot(position_probs[i, ], names.arg = 1:12, ylim = c(0, max(position_probs)),
          main = teams[i], xlab = "Platz", ylab = "Wahrscheinlichkeit", col = "orange")
}
par(old_par)
