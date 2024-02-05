#library calls
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

#import data
data_2023 <-read_excel("Thesis Data.xlsx", 
                       sheet = 1)
data_2022 <-read_excel("Thesis Data.xlsx", 
                       sheet = 2)
data_2021 <-read_excel("Thesis Data.xlsx", 
                       sheet = 3)
data_2020 <-read_excel("Thesis Data.xlsx", 
                       sheet = 4)
data_2019 <-read_excel("Thesis Data.xlsx", 
                       sheet = 5)
game_2020 <-read_excel("Thesis Data.xlsx", 
                       sheet = 6)
kicks_2020 <-read_excel("Thesis Data.xlsx", 
                       sheet = 7)
game_2019 <-read_excel("Thesis Data.xlsx", 
                       sheet = 8)
kicks_2019 <-read_excel("Thesis Data.xlsx", 
                       sheet = 9)
nfl_stadiums <-read_excel("Thesis Data.xlsx", 
                       sheet = 10)
international_games <-read_excel("Thesis Data.xlsx", 
                       sheet = 11)

#edit data
nfl_stadiums$Stadium_type <- ifelse(nfl_stadiums$Stadium_Direction1 == "Dome", "Dome", "Outdoor")

data_2019$Home_Conf <- ifelse(data_2019$Home %in% c("BUF", "NYJ", "MIA", "NE", "PIT", "BAL", "CLE", "CIN",
                                      "TEN", "HOU", "JAX", "IND", "KC", "LAC", "DEN", "LV"), "AFC", "NFC")
data_2019$Away_Conf <- ifelse(data_2019$Away %in% c("BUF", "NYJ", "MIA", "NE", "PIT", "BAL", "CLE", "CIN",
                                      "TEN", "HOU", "JAX", "IND", "KC", "LAC", "DEN", "LV"), "AFC", "NFC")
assign_division <- function(team) {
  if (team %in% c("BUF", "NYJ", "MIA", "NE")) {
    return("AFC East")
  } else if (team %in% c("PIT", "CLE", "CIN", "BAL")) {
    return("AFC North")
  } else if (team %in% c("HOU", "JAX", "IND", "TEN")) {
    return("AFC South")
  } else if (team %in% c("KC", "DEN", "LAC", "LV")) {
    return("AFC West")
  } else if (team %in% c("PHI", "DAL", "NYG", "WAS")) {
    return("NFC East")
  } else if (team %in% c("GB", "CHI", "DET", "MIN")) {
    return("NFC North")
  } else if (team %in% c("TB", "NO", "CAR", "ATL")) {
    return("NFC South")
  } else if (team %in% c("LAR", "SEA", "SF", "ARI")) {
    return("NFC West")
  } else {
    return("NFC")}}

data_2019$Away_Div <- sapply(data_2019$Away, assign_division)
data_2019$Home_Div <- sapply(data_2019$Home, assign_division)
data_2020$Away_Div <- sapply(data_2020$Away, assign_division)
data_2020$Home_Div <- sapply(data_2020$Home, assign_division)
data_2021$Away_Div <- sapply(data_2021$Away, assign_division)
data_2021$Home_Div <- sapply(data_2021$Home, assign_division)
data_2022$Away_Div <- sapply(data_2022$Away, assign_division)
data_2022$Home_Div <- sapply(data_2022$Home, assign_division)
data_2023$Away_Div <- sapply(data_2023$Away, assign_division)
data_2023$Home_Div <- sapply(data_2023$Home, assign_division)
data_2023$Away_Div <- sapply(data_2023$Away, assign_division)

#stadiums
data_2019$Stadium <-
  with(data_2019, ifelse(Home == "LV" & Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(Home == "TB" & Week == "6", "Tottenham Hotspur Stadium",
                  ifelse(Home == "LAR" & Week == "8", "Wembley Stadium",
                  ifelse(Home == "JAX" & Week == "9", "Wembley Stadium",
                  ifelse(Home == "LAC" & Week == "11", "Estadio Azteca",
                  ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Oakland Collesium",
                  ifelse(Home == "LAC", "Dignity Health Sports Park",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "LA Colliseum",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X"))))))))))))))))))))))))))))))))))))))

game_2019$Stadium <-
  with(game_2019, ifelse(Home == "LV" & Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(Home == "TB" & Week == "6", "Tottenham Hotspur Stadium",
                  ifelse(Home == "LAR" & Week == "8", "Wembley Stadium",
                  ifelse(Home == "JAX" & Week == "9", "Wembley Stadium",
                  ifelse(Home == "LAC" & Week == "11", "Estadio Azteca",
                  ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Oakland Collesium",
                  ifelse(Home == "LAC", "Dignity Health Sports Park",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "LA Colliseum",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X"))))))))))))))))))))))))))))))))))))))

data_2020$Stadium <-
  with(data_2020, ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Allegiant Stadium",
                  ifelse(Home == "LAC", "SoFi Stadium",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "SoFi Stadium",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X")))))))))))))))))))))))))))))))))

game_2020$Stadium <-
  with(game_2020, ifelse(Home == "LV" & Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(Home == "TB" & Week == "6", "Tottenham Hotspur Stadium",
                  ifelse(Home == "LAR" & Week == "8", "Wembley Stadium",
                  ifelse(Home == "JAX" & Week == "9", "Wembley Stadium",
                  ifelse(Home == "LAC" & Week == "11", "Estadio Azteca",
                  ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Oakland Collesium",
                  ifelse(Home == "LAC", "Dignity Health Sports Park",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "LA Colliseum",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X"))))))))))))))))))))))))))))))))))))))

data_2021$Stadium <-
  with(data_2021, ifelse(Home == "ATL" & Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(Home == "JAX" & Week == "6", "Tottenham Hotspur Stadium",
                  ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Allegiant Stadium",
                  ifelse(Home == "LAC", "SoFi Stadium",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "SoFi Stadium",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X")))))))))))))))))))))))))))))))))))

data_2022$Stadium <-
  with(data_2022, ifelse(Home == "NO" & Week == "4", "Tottenham Hotspur Stadium",
                  ifelse(Home == "GB" & Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(Home == "JAX" & Week == "8", "Wembley Stadium",  
                  ifelse(Home == "TB" & Week == "10", "Allianz Arena",
                  ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Allegiant Stadium",
                  ifelse(Home == "LAC", "SoFi Stadium",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "SoFi Stadium",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X")))))))))))))))))))))))))))))))))))))

data_2023$Stadium <-
  with(data_2023, ifelse(Home == "BUF", "Highmark Stadium",
                  ifelse(Home == "MIA", "Hard Rock Stadium",
                  ifelse(Home == "NE", "Gillette Stadium",
                  ifelse(Home == "NYJ", "Metlife Stadium",
                  ifelse(Home == "PIT", "Acrisure Stadium",
                  ifelse(Home == "BAL", "M&T Bank Stadium",
                  ifelse(Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(Home == "CIN", "Paycor Stadium",
                  ifelse(Home == "HOU", "NRG Stadium",
                  ifelse(Home == "IND", "Lucas Oil Stadium",
                  ifelse(Home == "TEN", "Nissan Stadium",
                  ifelse(Home == "JAX", "Everbank Stadium",
                  ifelse(Home == "KC", "Arrowhead Stadium",
                  ifelse(Home == "LV", "Allegiant Stadium",
                  ifelse(Home == "LAC", "SoFi Stadium",
                  ifelse(Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(Home == "NYG", "Metlife Stadium",
                  ifelse(Home == "PHI", "Lincoln Financial Field",
                  ifelse(Home == "DAL", "AT&T Stadium",
                  ifelse(Home == "WAS", "FedEx Field",
                  ifelse(Home == "GB", "Lambeau Field",
                  ifelse(Home == "CHI", "Soldier Field",
                  ifelse(Home == "MIN", "U.S. Bank Stadium",
                  ifelse(Home == "DET", "Ford Field",
                  ifelse(Home == "NO", "Caesars Superdome",
                  ifelse(Home == "CAR", "Bank of America Stadium",
                  ifelse(Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(Home == "TB", "Raymond James Stadium",
                  ifelse(Home == "LAR", "SoFi Stadium",
                  ifelse(Home == "SF", "Levi's Stadium",
                  ifelse(Home == "ARI", "State Farm Stadium",
                  ifelse(Home == "SEA", "Lumen Field", "X"))))))))))))))))))))))))))))))))) 

data_2019$Stadium_Direction1 <-
  with(data_2019, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"
                         | Stadium =="Arrowhead Stadium" | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "NW",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium"
                         | Stadium == "Dignity Health Sports Park" | Stadium == "Oakland Collesium" | Stadium == "Tottenham Hotspur Stadium"| Stadium == "Estadio Azteca", "N",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field"| Stadium == "LA Collesium"| Stadium == "Wembley Stadium", "W",
                  ifelse( Stadium =="Cleveland Browns Stadium", "NE",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))
  
data_2019$Stadium_Direction2 <-
  with(data_2019, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "SE",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium"
                         | Stadium == "Dignity Health Sports Park" | Stadium == "Oakland Collesium" | Stadium == "Tottenham Hotspur Stadium"| Stadium == "Estadio Azteca", "S",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field"| Stadium == "LA Collesium"| Stadium == "Wembley Stadium", "E",
                  ifelse( Stadium =="Cleveland Browns Stadium", "SW",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2020$Stadium_Direction1 <-
  with(data_2020, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "NW",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium", "N",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "W",
                  ifelse( Stadium =="Cleveland Browns Stadium", "NE",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2020$Stadium_Direction2 <-
  with(data_2020, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "SE",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium", "S",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "E",
                  ifelse( Stadium =="Cleveland Browns Stadium", "SW",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2021$Stadium_Direction1 <-
  with(data_2021, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "NW",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium" | Stadium == "Tottenham Hotspur Stadium", "N",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "W",
                  ifelse( Stadium =="Cleveland Browns Stadium", "NE",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2021$Stadium_Direction2 <-
  with(data_2021, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "SE",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium" | Stadium == "Tottenham Hotspur Stadium", "S",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "E",
                  ifelse( Stadium =="Cleveland Browns Stadium", "SW",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2022$Stadium_Direction1 <-
  with(data_2022, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "NW",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium" | Stadium == "Tottenham Hotspur Stadium" | Stadium == "Allianz Arena", "N",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field" | Stadium == "Wembley Stadium", "W",
                  ifelse( Stadium =="Cleveland Browns Stadium", "NE",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2022$Stadium_Direction2 <-
  with(data_2022, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "SE",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium" | Stadium == "Tottenham Hotspur Stadium" | Stadium == "Allianz Arena", "S",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field" | Stadium == "Wembley Stadium", "E",
                  ifelse( Stadium =="Cleveland Browns Stadium", "SW",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2023$Stadium_Direction1 <-
  with(data_2023, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "NW",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium", "N",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "W",
                  ifelse( Stadium =="Cleveland Browns Stadium", "NE",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

data_2023$Stadium_Direction2 <-
  with(data_2023, ifelse(Stadium == "Highmark Stadium"| Stadium =="Hard Rock Stadium"| Stadium == "Paycor Stadium"| Stadium =="Arrowhead Stadium" 
                         | Stadium =="Levi's Stadium" | Stadium =="Bank of America Stadium", "SE",
                  ifelse(Stadium == "Gillette Stadium"| Stadium =="Metlife Stadium"| Stadium =="Acrisure Stadium"| Stadium == "Nissan Stadium"
                         | Stadium == "Everbank Stadium"| Stadium =="Empower Field at Mile High Stadium"| Stadium =="Lincoln Financial Field"
                         | Stadium =="Lambeau Field"| Stadium == "Soldier Field"| Stadium == "Lumen Field"| Stadium =="Raymond James Stadium", "S",
                  ifelse( Stadium == "M&T Bank Stadium"| Stadium == "FedEx Field", "E",
                  ifelse( Stadium =="Cleveland Browns Stadium", "SW",
                  ifelse( Stadium =="NRG Stadium" | Stadium =="Lucas Oil Stadium"| Stadium =="Allegiant Stadium"
                         | Stadium =="SoFi Stadium"| Stadium == "AT&T Stadium"| Stadium =="Caesars Superdome" 
                         | Stadium =="Ford Field" | Stadium =="U.S. Bank Stadium"| Stadium =="Mercedes-Benz Stadium"
                         | Stadium =="State Farm Stadium", "Dome", "X"))))))

#summary stats
summary(data_2019)
summary(data_2020)
summary(game_2019)
summary(game_2020)
summary(kicks_2019)
summary(kicks_2020)

#plots
current_stadiums <- nfl_stadiums[1:32, ]
international_stadiums <- nfl_stadiums[37:40, ]

all <- ggplot(nfl_stadiums, aes(x = Stadium_type)) +
  geom_bar(aes(fill = Stadium_type == "Outdoor"), color = "black") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black") +
  scale_fill_manual(values = c("red","skyblue"), guide = "none") +
  labs(title = "All Stadiums 2018-2024", x = NULL, y = "Count") +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)[-c(1:7)]) +
  geom_text(x = 1, y = 30, label = "Moved Stadiums
LV, LAR, LAC", hjust = 0.5)

current <- ggplot(current_stadiums, aes(x = Stadium_type)) +
  geom_bar(aes(fill = Stadium_type == "Outdoor"), color = "black") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black") +
  scale_fill_manual(values = c("red", "skyblue"), guide = "none") +
  labs(title = "Current Stadiums", x = NULL, y = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)[-c(1:7)])

int <- ggplot(international_stadiums, aes(x = Stadium_type)) +
  geom_bar(aes(fill = Stadium_type == "Outdoor"), color = "black") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black") +
  scale_fill_manual(values = c("skyblue", "red"), guide = "none") +
  labs(title = "International", x = NULL, y = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)[-c(1:7)])

#average temperature
custom_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "WC")

data_2020$Week <- factor(data_2020$Week, levels = custom_order)
data_2019$Week <- factor(data_2019$Week, levels = custom_order)

outdoor_games_2019 <- subset(data_2019, Weather != "Dome")
outdoor_games_2020 <- subset(data_2020, Weather != "Dome")

outdoor_games_2019$Temp <- as.numeric(outdoor_games_2019$Temp)
outdoor_games_2020$Temp <- as.numeric(outdoor_games_2020$Temp)
outdoor_games_2019$Air <- as.numeric(outdoor_games_2019$Air)
outdoor_games_2020$Air <- as.numeric(outdoor_games_2020$Air)

temp_2019 <- ggplot(outdoor_games_2019, aes(x = Week, y = Temp)) +
              scale_x_discrete(labels = function(x) ifelse(is.na(x), "Playoffs", x)) +
              scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
              labs(title = "2019 Outdoor Stadium Temperature", x = "Week", y = "Temperature - Fahrenheit") +
              geom_bin2d(bins = 20, aes(fill = ..count..)) +
              scale_fill_gradient(name = "Frequency", low = "turquoise", high = "blue")

temp_2020 <- ggplot(outdoor_games_2020, aes(x = Week, y = Temp)) +
              scale_x_discrete(labels = function(x) ifelse(is.na(x), "Playoffs", x)) +
              scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
              labs(title = "2020 Outdoor Stadium Temperature", x = "Week", y = "Temperature - Fahrenheit") +
              geom_bin2d(bins = 20, aes(fill = ..count..)) +
              scale_fill_gradient(name = "Frequency", low = "turquoise", high = "blue")

wind_2019 <- ggplot(outdoor_games_2019, aes(x = Week, y = Air)) +
                scale_x_discrete(labels = function(x) ifelse(is.na(x), "Playoffs", x)) +
                scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
                labs(title = "2019 Outdoor Stadium Wind Speed", x = "Week", y = "Wind - MPH") +
                geom_bin2d(bins = 20, aes(fill = ..count..)) +
                scale_fill_gradient(name = "Frequency", low = "turquoise", high = "blue")

wind_2020 <- ggplot(outdoor_games_2020, aes(x = Week, y = Air)) +
                scale_x_discrete(labels = function(x) ifelse(is.na(x), "Playoffs", x)) +
                scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
                labs(title = "2020 Outdoor Stadium Wind Speed", x = "Week", y = "Wind - MPH") +
                geom_bin2d(bins = 20, aes(fill = ..count..)) +
                scale_fill_gradient(name = "Frequency", low = "turquoise", high = "blue")
#filter
buf_kicks_2019 <- subset(game_2019, Stadium == "Highmark Stadium")
mia_kicks_2019 <- subset(game_2019, Stadium == "Hard Rock Stadium")
ne_kicks_2019 <- subset(game_2019, Stadium == "Gillette Stadium")
nyj_kicks_2019 <- subset(game_2019, Stadium == "Metlife Stadium")
pit_kicks_2019 <- subset(game_2019, Stadium == "Acrisure Stadium")
bal_kicks_2019 <- subset(game_2019, Stadium == "M&T Bank Stadium")
cle_kicks_2019 <- subset(game_2019, Stadium == "Cleveland Browns Stadium")
cin_kicks_2019 <- subset(game_2019, Stadium == "Paycor Stadium")
hou_kicks_2019 <- subset(game_2019, Stadium == "NRG Stadium")
ind_kicks_2019 <- subset(game_2019, Stadium == "Lucas Oil Stadium")
ten_kicks_2019 <- subset(game_2019, Stadium == "Nissan Stadium")
jax_kicks_2019 <- subset(game_2019, Stadium == "Everbank Stadium")
kc_kicks_2019 <- subset(game_2019, Stadium == "Arrowhead Stadium")
lv_kicks_2019 <- subset(game_2019, Stadium == "Oakland Collesium")
lac_kicks_2019 <- subset(game_2019, Stadium == "Dignity Health Sports Park")
den_kicks_2019 <- subset(game_2019, Stadium == "Empower Field at Mile High Stadium")
nyg_kicks_2019 <- subset(game_2019, Stadium == "Metlife Stadium")
phi_kicks_2019 <- subset(game_2019, Stadium == "Lincoln Financial Field")
dal_kicks_2019 <- subset(game_2019, Stadium == "AT&T Stadium")
was_kicks_2019 <- subset(game_2019, Stadium == "FedEx Field")
gb_kicks_2019 <- subset(game_2019, Stadium == "Lambeau Field")
chi_kicks_2019 <- subset(game_2019, Stadium == "Soldier Field")
min_kicks_2019 <- subset(game_2019, Stadium == "U.S. Bank Stadium")
det_kicks_2019 <- subset(game_2019, Stadium == "Ford Field")
no_kicks_2019 <- subset(game_2019, Stadium == "Caesars Superdome")
car_kicks_2019 <- subset(game_2019, Stadium == "Bank of America Stadium")
atl_kicks_2019 <- subset(game_2019, Stadium == "Mercedes-Benz Stadium")
tb_kicks_2019 <- subset(game_2019, Stadium == "Raymond James Stadium")
lar_kicks_2019 <- subset(game_2019, Stadium == "LA Colliseum")
sf_kicks_2019 <- subset(game_2019, Stadium == "Levi's Stadium")
ari_kicks_2019 <- subset(game_2019, Stadium == "State Farm Stadium")
sea_kicks_2019 <- subset(game_2019, Stadium == "Lumen Field")

buf_kicks_2020 <- subset(game_2020, Stadium == "Highmark Stadium")
mia_kicks_2020 <- subset(game_2020, Stadium == "Hard Rock Stadium")
ne_kicks_2020 <- subset(game_2020, Stadium == "Gillette Stadium")
nyj_kicks_2020 <- subset(game_2020, Stadium == "Metlife Stadium")
pit_kicks_2020 <- subset(game_2020, Stadium == "Acrisure Stadium")
bal_kicks_2020 <- subset(game_2020, Stadium == "M&T Bank Stadium")
cle_kicks_2020 <- subset(game_2020, Stadium == "Cleveland Browns Stadium")
cin_kicks_2020 <- subset(game_2020, Stadium == "Paycor Stadium")
hou_kicks_2020 <- subset(game_2020, Stadium == "NRG Stadium")
ind_kicks_2020 <- subset(game_2020, Stadium == "Lucas Oil Stadium")
ten_kicks_2020 <- subset(game_2020, Stadium == "Nissan Stadium")
jax_kicks_2020 <- subset(game_2020, Stadium == "Everbank Stadium")
kc_kicks_2020 <- subset(game_2020, Stadium == "Arrowhead Stadium")
lv_kicks_2020 <- subset(game_2020, Stadium == "Oakland Collesium")
lac_kicks_2020 <- subset(game_2020, Stadium == "Dignity Health Sports Park")
den_kicks_2020 <- subset(game_2020, Stadium == "Empower Field at Mile High Stadium")
nyg_kicks_2020 <- subset(game_2020, Stadium == "Metlife Stadium")
phi_kicks_2020 <- subset(game_2020, Stadium == "Lincoln Financial Field")
dal_kicks_2020 <- subset(game_2020, Stadium == "AT&T Stadium")
was_kicks_2020 <- subset(game_2020, Stadium == "FedEx Field")
gb_kicks_2020 <- subset(game_2020, Stadium == "Lambeau Field")
chi_kicks_2020 <- subset(game_2020, Stadium == "Soldier Field")
min_kicks_2020 <- subset(game_2020, Stadium == "U.S. Bank Stadium")
det_kicks_2020 <- subset(game_2020, Stadium == "Ford Field")
no_kicks_2020 <- subset(game_2020, Stadium == "Caesars Superdome")
car_kicks_2020 <- subset(game_2020, Stadium == "Bank of America Stadium")
atl_kicks_2020 <- subset(game_2020, Stadium == "Mercedes-Benz Stadium")
tb_kicks_2020 <- subset(game_2020, Stadium == "Raymond James Stadium")
lar_kicks_2020 <- subset(game_2020, Stadium == "LA Colliseum")
sf_kicks_2020 <- subset(game_2020, Stadium == "Levi's Stadium")
ari_kicks_2020 <- subset(game_2020, Stadium == "State Farm Stadium")
sea_kicks_2020 <- subset(game_2020, Stadium == "Lumen Field")

nfl_stadiums$FG_per_2020 <- NA
nfl_stadiums$FG_per_2020 <- NA
nfl_stadiums$XP_per_2020 <- NA
nfl_stadiums$XP_per_2020 <- NA
nfl_stadiums$FG_per_2019 <- NA
nfl_stadiums$FG_per_2019 <- NA
nfl_stadiums$XP_per_2019 <- NA
nfl_stadiums$XP_per_2019 <- NA

teams <- c("BUF", "MIA", "NE", "NYJ", "PIT", "BAL", "CLE", "CIN", "HOU", "IND", "TEN", "JAX", "KC", "LV", "LAC", "DEN", "NYG", "PHI", "DAL", "WAS", "GB", "CHI", "MIN", "DET", "NO", "CAR", "ATL", "TB", "LAR", "SF", "ARI", "SEA")

for (team in teams) {
  team_kicks <- get(paste(tolower(team), "_kicks_2019", sep = ""))
  nfl_stadiums$FG_per_2019[nfl_stadiums$Team == team] <- weighted.mean(team_kicks$FG_per, team_kicks$Total_FGA)}

for (team in teams) {
  team_kicks <- get(paste(tolower(team), "_kicks_2019", sep = ""))
  nfl_stadiums$XP_per_2019[nfl_stadiums$Team == team] <- weighted.mean(team_kicks$XP_per, team_kicks$Total_XPA)}

for (team in teams) {
  team_kicks <- get(paste(tolower(team), "_kicks_2020", sep = ""))
  nfl_stadiums$FG_per_2020[nfl_stadiums$Team == team] <- weighted.mean(team_kicks$FG_per, team_kicks$Total_FGA)}

for (team in teams) {
  team_kicks <- get(paste(tolower(team), "_kicks_2020", sep = ""))
  nfl_stadiums$XP_per_2020[nfl_stadiums$Team == team] <- weighted.mean(team_kicks$XP_per, team_kicks$Total_XPA)}

top_10_FG_per_2019 <- head(nfl_stadiums[order(-nfl_stadiums$FG_per_2019), ], 10)
top_10_FG_per_2019$Stadium <- factor(top_10_FG_per_2019$Stadium, levels = rev(top_10_FG_per_2019$Stadium[order(top_10_FG_per_2019$FG_per_2019)]))

FG_2019_Graph <- ggplot(top_10_FG_per_2019, aes(x = Stadium, y = FG_per_2019, fill = Stadium)) +
                    labs(title = "2019 Highest FG%", x = NULL, y = "FG%") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    geom_bar(stat = "identity", width = 0.5) +
                    scale_fill_manual(values = c("darkturquoise", "darkblue", "mediumpurple2", "gold4", "darkorange1",
                                                 "goldenrod2", "darkgreen", "purple", "black", "firebrick3")) +
                    scale_x_discrete(breaks = NULL) +
                    coord_cartesian(ylim = c(0.80, 1.00))+
                    geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
                             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
                             color = "black") 

top_10_XP_per_2019 <- head(nfl_stadiums[order(-nfl_stadiums$XP_per_2019), ], 10)
top_10_XP_per_2019$Stadium <- factor(top_10_XP_per_2019$Stadium, levels = rev(top_10_XP_per_2019$Stadium[order(top_10_XP_per_2019$XP_per_2019)]))

XP_2019_Graph <-  ggplot(top_10_XP_per_2019, aes(x = Stadium, y = XP_per_2019, fill = Stadium)) +
                          labs(title = "2019 Highest XP%", x = NULL, y = "XP%") +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                          geom_bar(stat = "identity", width = 0.5) +
                          scale_fill_manual(values = c("darkorange1", "firebrick3", "blue", "yellow1", "grey",
                                                       "navy", "chartreuse2", "black", "mediumpurple2", "darkturquoise")) +
                          scale_x_discrete(breaks = NULL) +
                          coord_cartesian(ylim = c(0.90, 1.00)) +
                          geom_hline(yintercept = c(0.90, 0.925, 0.95, 0.975, 1.00), 
                                     linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
                                     color = "black")

top_10_FG_per_2020 <- head(nfl_stadiums[order(-nfl_stadiums$FG_per_2020), ], 10)
top_10_FG_per_2020 <- top_10_FG_per_2020[!duplicated(top_10_FG_per_2020$Stadium), ]
top_10_FG_per_2020$Stadium <- factor(top_10_FG_per_2020$Stadium, levels = rev(top_10_FG_per_2020$Stadium[order(top_10_FG_per_2020$FG_per_2020)]))

top_10_XP_per_2020 <- head(nfl_stadiums[order(-nfl_stadiums$XP_per_2020), ], 10)
top_10_XP_per_2020 <- top_10_XP_per_2020[!duplicated(top_10_XP_per_2020$Stadium), ]
top_10_XP_per_2020$Stadium <- factor(top_10_XP_per_2020$Stadium, levels = rev(top_10_XP_per_2020$Stadium[order(top_10_XP_per_2020$XP_per_2020)]))

FG_2020_Graph <- ggplot(top_10_FG_per_2020, aes(x = Stadium, y = FG_per_2020, fill = Stadium)) +
  labs(title = "2020 Highest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("darkgreen", "blue", "grey", "black", "darkorange1",
                               "hotpink", "olivedrab3", "firebrick", "navy", "red")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

XP_2020_Graph <-  ggplot(top_10_XP_per_2020, aes(x = Stadium, y = XP_per_2020, fill = Stadium)) +
  labs(title = "2020 Highest XP%", x = NULL, y = "XP%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("royalblue1", "cyan", "black", "darkblue", "goldenrod2",
                               "darkturquoise", "gold4", "yellow1", "grey", "blue")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.90, 1.00)) +
  geom_hline(yintercept = c(0.90, 0.925, 0.95, 0.975, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black")

#worst percentage
last_10_FG_per_2019 <- head(nfl_stadiums[order(nfl_stadiums$FG_per_2019), ], 10)
last_10_FG_per_2019 <- last_10_FG_per_2019[!duplicated(last_10_FG_per_2019$Stadium), ]
last_10_FG_per_2019$Stadium <- factor(last_10_FG_per_2019$Stadium, levels = (last_10_FG_per_2019$Stadium[order(last_10_FG_per_2019$FG_per_2019)]))

last_10_XP_per_2019 <- head(nfl_stadiums[order(nfl_stadiums$XP_per_2019), ], 10)
last_10_XP_per_2019 <- last_10_XP_per_2019[!duplicated(last_10_XP_per_2019$Stadium), ]
last_10_XP_per_2019$Stadium <- factor(last_10_XP_per_2019$Stadium, levels = (last_10_XP_per_2019$Stadium[order(last_10_XP_per_2019$XP_per_2019)]))

last_FG_2019_Graph <- ggplot(last_10_FG_per_2019, aes(x = Stadium, y = FG_per_2019, fill = Stadium)) +
  labs(title = "2019 Lowest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("black", "darkred", "lightcoral", "dodgerblue", "darkorange1",
                               "grey", "cyan", "sienna4", "yellow1", "blue")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.65, 0.85))+
  geom_hline(yintercept = c(0.65, 0.60, 0.75, 0.80, 0.85), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

last_XP_2019_Graph <- ggplot(last_10_XP_per_2019, aes(x = Stadium, y = XP_per_2019, fill = Stadium)) +
  labs(title = "2019 Lowest XP%", x = NULL, y = "XP%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("sienna4", "gold4", "blue", "lightcoral", "darkorange1",
                               "yellow1", "red3", "olivedrab3", "cyan", "cyan")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 
#2020
last_10_FG_per_2020 <- head(nfl_stadiums[order(nfl_stadiums$FG_per_2020), ], 10)
last_10_FG_per_2020 <- last_10_FG_per_2020[!duplicated(last_10_FG_per_2020$Stadium), ]
last_10_FG_per_2020$Stadium <- factor(last_10_FG_per_2020$Stadium, levels = (last_10_FG_per_2020$Stadium[order(last_10_FG_per_2020$FG_per_2020)]))

last_10_XP_per_2020 <- head(nfl_stadiums[order(nfl_stadiums$XP_per_2020), ], 10)
last_10_XP_per_2020 <- last_10_XP_per_2020[!duplicated(last_10_XP_per_2020$Stadium), ]
last_10_XP_per_2020$Stadium <- factor(last_10_XP_per_2020$Stadium, levels = (last_10_XP_per_2020$Stadium[order(last_10_XP_per_2020$XP_per_2020)]))

last_FG_2020_Graph <- ggplot(last_10_FG_per_2020, aes(x = Stadium, y = FG_per_2020, fill = Stadium)) +
  labs(title = "2020 Lowest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("purple", "blue", "darkturquoise", "dodgerblue", "chocolate1",
                               "red3", "green3", "indianred", "darkred", "cyan")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.65, 0.85))+
  geom_hline(yintercept = c(0.65, 0.70, 0.75, 0.80, 0.85), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

last_XP_2020_Graph <- ggplot(last_10_XP_per_2020, aes(x = Stadium, y = XP_per_2020, fill = Stadium)) +
  labs(title = "2020 Lowest XP%", x = NULL, y = "XP%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("black", "chocolate1", "darkred", "dodgerblue", "green",
                               "navy", "mediumpurple2", "darkgreen", "yellow1", "olivedrab3")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

#Final Plots
ggarrange(all, current, int, ncol = 3, nrow = 1, common.legend = TRUE,
          legend = "bottom",  widths = c(1, 1, 0.5))

ggarrange(temp_2019, temp_2020, ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "right")
  
ggarrange(wind_2019, wind_2020, ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "right")

ggarrange(XP_2019_Graph, FG_2019_Graph, XP_2020_Graph,
          FG_2020_Graph, ncol = 2, nrow = 2,
          common.legend = FALSE, legend = "right")

ggarrange(last_XP_2019_Graph, last_FG_2019_Graph, last_XP_2020_Graph,
          last_FG_2020_Graph, ncol = 2, nrow = 2,
          common.legend = FALSE, legend = "right")
