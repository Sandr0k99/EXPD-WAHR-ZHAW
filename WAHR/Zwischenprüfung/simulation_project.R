# Fussball Saison-Simulation: Schweizer Super League (echte Daten, Saisonende 2023/24)

# Teams und deren durchschnittliche Tore pro Spiel (38 Spiele)
teams <- c("Young Boys", "Lugano", "Servette", "Zürich", "St. Gallen", "Winterthur", 
           "Luzern", "Basel", "Yverdon-Sport", "Lausanne-Sport", "Grasshoppers", "Stade-Lausanne")

avg_goals_scored <- c(2.00, 1.76, 1.55, 1.39, 1.58, 1.58, 1.24, 1.18, 1.32, 1.26, 1.08, 1.05)
avg_goals_conceded <- c(0.89, 1.34, 1.13, 1.08, 1.34, 1.87, 1.39, 1.37, 1.87, 1.39, 1.29, 2.03)

team_stats <- data.frame(Team = teams, GF = avg_goals_scored, GA = avg_goals_conceded)

# Parameter für Verletzungen
injury_probability <- 0.05 # 5% Verletzungswahrscheinlichkeit pro Spiel
injury_penalty <- 0.9       # Reduktion der Torstärke auf 90% bei Verletzung

# Funktion: Simuliere ob ein Team eine Verletzung hat
simulate_injury_status <- function(prob) {
  ifelse(runif(1) < prob, injury_penalty, 1)
}

# Funktion: Simuliere ein Spiel inkl. Heimvorteil und Verletzungen
simulate_match_with_injuries <- function(home_team, away_team, team_stats, injury_factors) {
  lambda_home <- team_stats$GF[team_stats$Team == home_team] * injury_factors[home_team] * 1.1 # Heimvorteil
  lambda_away <- team_stats$GF[team_stats$Team == away_team] * injury_factors[away_team]
  
  goals_home <- rpois(1, lambda_home)
  goals_away <- rpois(1, lambda_away)
  
  result <- ifelse(goals_home > goals_away, "Home Win",
                   ifelse(goals_home < goals_away, "Away Win", "Draw"))
  
  return(data.frame(Home = home_team, Away = away_team, Goals_Home = goals_home, Goals_Away = goals_away, Result = result))
}

# Funktion: Simuliere eine Saison
simulate_season <- function(team_stats, n_simulations = 1000) {
  teams <- team_stats$Team
  n_teams <- length(teams)
  standings <- matrix(0, nrow = n_simulations, ncol = n_teams)
  colnames(standings) <- teams
  
  for (i in 1:n_simulations) {
    points <- setNames(rep(0, n_teams), teams)
    
    for (home in teams) {
      for (away in teams) {
        if (home != away) {
          injury_factors <- setNames(sapply(teams, function(x) simulate_injury_status(injury_probability)), teams)
          match <- simulate_match_with_injuries(home, away, team_stats, injury_factors)
          
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
    standings[i, ] <- points
  }
  return(standings)
}

# Simulation starten
set.seed(42) # für Reproduzierbarkeit
standings <- simulate_season(team_stats, n_simulations = 1000)

# Analyse: Meisterwahrscheinlichkeiten
champions <- apply(standings, 1, function(x) names(which.max(x)))
champion_counts <- table(champions)
champion_probabilities <- prop.table(champion_counts)

print(champion_probabilities)

# Einfache Visualisierung
barplot(champion_probabilities, main = "Meisterwahrscheinlichkeiten Schweizer Super League", las = 2, col = "skyblue")

