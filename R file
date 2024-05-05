setwd("C:/Users/jason/Downloads/R/Thesis")

#library calls
library(readxl)
library(writexl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(ggpubr)
library(grid)
library(png)
library(lmtest)
library(car)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(shiny)
library(neuralnet)

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

kicks_2019$Stadium <- ifelse(kicks_2019$Home == "LV" & kicks_2019$Week == "5", "Tottenham Hotspur Stadium",
                  ifelse(kicks_2019$Home == "TB" & kicks_2019$Week == "6", "Tottenham Hotspur Stadium",
                  ifelse(kicks_2019$Home == "LAR" & kicks_2019$Week == "8", "Wembley Stadium",
                  ifelse(kicks_2019$Home == "JAX" & kicks_2019$Week == "9", "Wembley Stadium",
                  ifelse(kicks_2019$Home == "LAC" & kicks_2019$Week == "11", "Estadio Azteca",
                  ifelse(kicks_2019$Home == "BUF", "Highmark Stadium",
                  ifelse(kicks_2019$Home == "MIA", "Hard Rock Stadium",
                  ifelse(kicks_2019$Home == "NE", "Gillette Stadium",
                  ifelse(kicks_2019$Home == "NYJ", "Metlife Stadium",
                  ifelse(kicks_2019$Home == "PIT", "Acrisure Stadium",
                  ifelse(kicks_2019$Home == "BAL", "M&T Bank Stadium",
                  ifelse(kicks_2019$Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(kicks_2019$Home == "CIN", "Paycor Stadium",
                  ifelse(kicks_2019$Home == "HOU", "NRG Stadium",
                  ifelse(kicks_2019$Home == "IND", "Lucas Oil Stadium",
                  ifelse(kicks_2019$Home == "TEN", "Nissan Stadium",
                  ifelse(kicks_2019$Home == "JAX", "Everbank Stadium",
                  ifelse(kicks_2019$Home == "KC", "Arrowhead Stadium",
                  ifelse(kicks_2019$Home == "LV", "Oakland Collesium",
                  ifelse(kicks_2019$Home == "LAC", "Dignity Health Sports Park",
                  ifelse(kicks_2019$Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(kicks_2019$Home == "NYG", "Metlife Stadium",
                  ifelse(kicks_2019$Home == "PHI", "Lincoln Financial Field",
                  ifelse(kicks_2019$Home == "DAL", "AT&T Stadium",
                  ifelse(kicks_2019$Home == "WAS", "FedEx Field",
                  ifelse(kicks_2019$Home == "GB", "Lambeau Field",
                  ifelse(kicks_2019$Home == "CHI", "Soldier Field",
                  ifelse(kicks_2019$Home == "MIN", "U.S. Bank Stadium",
                  ifelse(kicks_2019$Home == "DET", "Ford Field",
                  ifelse(kicks_2019$Home == "NO", "Caesars Superdome",
                  ifelse(kicks_2019$Home == "CAR", "Bank of America Stadium",
                  ifelse(kicks_2019$Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(kicks_2019$Home == "TB", "Raymond James Stadium",
                  ifelse(kicks_2019$Home == "LAR", "LA Colliseum",
                  ifelse(kicks_2019$Home == "SF", "Levi's Stadium",
                  ifelse(kicks_2019$Home == "ARI", "State Farm Stadium",
                  ifelse(kicks_2019$Home == "SEA", "Lumen Field", "X")))))))))))))))))))))))))))))))))))))
                  
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

kicks_2020$Stadium <- ifelse(kicks_2020$Home == "BUF", "Highmark Stadium",
                  ifelse(kicks_2020$Home == "MIA", "Hard Rock Stadium",
                  ifelse(kicks_2020$Home == "NE", "Gillette Stadium",
                  ifelse(kicks_2020$Home == "NYJ", "Metlife Stadium",
                  ifelse(kicks_2020$Home == "PIT", "Acrisure Stadium",
                  ifelse(kicks_2020$Home == "BAL", "M&T Bank Stadium",
                  ifelse(kicks_2020$Home == "CLE", "Cleveland Browns Stadium",
                  ifelse(kicks_2020$Home == "CIN", "Paycor Stadium",
                  ifelse(kicks_2020$Home == "HOU", "NRG Stadium",
                  ifelse(kicks_2020$Home == "IND", "Lucas Oil Stadium",
                  ifelse(kicks_2020$Home == "TEN", "Nissan Stadium",
                  ifelse(kicks_2020$Home == "JAX", "Everbank Stadium",
                  ifelse(kicks_2020$Home == "KC", "Arrowhead Stadium",
                  ifelse(kicks_2020$Home == "LV", "Allegiant Stadium",
                  ifelse(kicks_2020$Home == "LAC", "SoFi Stadium",
                  ifelse(kicks_2020$Home == "DEN", "Empower Field at Mile High Stadium",
                  ifelse(kicks_2020$Home == "NYG", "Metlife Stadium",
                  ifelse(kicks_2020$Home == "PHI", "Lincoln Financial Field",
                  ifelse(kicks_2020$Home == "DAL", "AT&T Stadium",
                  ifelse(kicks_2020$Home == "WAS", "FedEx Field",
                  ifelse(kicks_2020$Home == "GB", "Lambeau Field",
                  ifelse(kicks_2020$Home == "CHI", "Soldier Field",
                  ifelse(kicks_2020$Home == "MIN", "U.S. Bank Stadium",
                  ifelse(kicks_2020$Home == "DET", "Ford Field",
                  ifelse(kicks_2020$Home == "NO", "Caesars Superdome",
                  ifelse(kicks_2020$Home == "CAR", "Bank of America Stadium",
                  ifelse(kicks_2020$Home == "ATL", "Mercedes-Benz Stadium",
                  ifelse(kicks_2020$Home == "TB", "Raymond James Stadium",
                  ifelse(kicks_2020$Home == "LAR", "SoFi Stadium",
                  ifelse(kicks_2020$Home == "SF", "Levi's Stadium",
                  ifelse(kicks_2020$Home == "ARI", "State Farm Stadium",
                  ifelse(kicks_2020$Home == "SEA", "Lumen Field", "X"))))))))))))))))))))))))))))))))
                  
game_2020$Stadium <-
  with(game_2020, ifelse(Home == "BUF", "Highmark Stadium",
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

kicks_2019$Stadium_Direction1 <-ifelse(kicks_2019$Stadium == "Highmark Stadium"| kicks_2019$Stadium =="Hard Rock Stadium"| kicks_2019$Stadium == "Paycor Stadium"
                         | kicks_2019$Stadium =="Arrowhead Stadium" | kicks_2019$Stadium =="Levi's Stadium" | kicks_2019$Stadium =="Bank of America Stadium", "NW",
                         ifelse(kicks_2019$Stadium == "Gillette Stadium"| kicks_2019$Stadium =="Metlife Stadium"| kicks_2019$Stadium =="Acrisure Stadium"| kicks_2019$Stadium == "Nissan Stadium"
                                | kicks_2019$Stadium == "Everbank Stadium"| kicks_2019$Stadium =="Empower Field at Mile High Stadium"| kicks_2019$Stadium =="Lincoln Financial Field"
                                | kicks_2019$Stadium =="Lambeau Field"| kicks_2019$Stadium == "Soldier Field"| kicks_2019$Stadium == "Lumen Field"| kicks_2019$Stadium =="Raymond James Stadium"
                                | kicks_2019$Stadium == "Dignity Health Sports Park" | kicks_2019$Stadium == "Oakland Collesium" | kicks_2019$Stadium == "Tottenham Hotspur Stadium"| kicks_2019$Stadium == "Estadio Azteca", "N",
                                ifelse( kicks_2019$Stadium == "M&T Bank Stadium"| kicks_2019$Stadium == "FedEx Field"| kicks_2019$Stadium == "LA Collesium"| kicks_2019$Stadium == "Wembley Stadium", "W",
                                        ifelse( kicks_2019$Stadium =="Cleveland Browns Stadium", "NE",
                                                ifelse( kicks_2019$Stadium =="NRG Stadium" | kicks_2019$Stadium =="Lucas Oil Stadium"| kicks_2019$Stadium =="Allegiant Stadium"
                                                        | kicks_2019$Stadium == "AT&T Stadium"| kicks_2019$Stadium =="Caesars Superdome" 
                                                        | kicks_2019$Stadium =="Ford Field" | kicks_2019$Stadium =="U.S. Bank Stadium"| kicks_2019$Stadium =="Mercedes-Benz Stadium"
                                                        | kicks_2019$Stadium =="State Farm Stadium", "Dome", "X")))))

kicks_2019$Stadium_Direction2 <-ifelse(kicks_2019$Stadium == "Highmark Stadium"| kicks_2019$Stadium =="Hard Rock Stadium"| kicks_2019$Stadium == "Paycor Stadium"
                                       | kicks_2019$Stadium =="Arrowhead Stadium" | kicks_2019$Stadium =="Levi's Stadium" | kicks_2019$Stadium =="Bank of America Stadium", "SE",
                                       ifelse(kicks_2019$Stadium == "Gillette Stadium"| kicks_2019$Stadium =="Metlife Stadium"| kicks_2019$Stadium =="Acrisure Stadium"| kicks_2019$Stadium == "Nissan Stadium"
                                              | kicks_2019$Stadium == "Everbank Stadium"| kicks_2019$Stadium =="Empower Field at Mile High Stadium"| kicks_2019$Stadium =="Lincoln Financial Field"
                                              | kicks_2019$Stadium =="Lambeau Field"| kicks_2019$Stadium == "Soldier Field"| kicks_2019$Stadium == "Lumen Field"| kicks_2019$Stadium =="Raymond James Stadium"
                                              | kicks_2019$Stadium == "Dignity Health Sports Park" | kicks_2019$Stadium == "Oakland Collesium" | kicks_2019$Stadium == "Tottenham Hotspur Stadium"| kicks_2019$Stadium == "Estadio Azteca", "S",
                                              ifelse( kicks_2019$Stadium == "M&T Bank Stadium"| kicks_2019$Stadium == "FedEx Field"| kicks_2019$Stadium == "LA Collesium"| kicks_2019$Stadium == "Wembley Stadium", "E",
                                                      ifelse( kicks_2019$Stadium =="Cleveland Browns Stadium", "SW",
                                                              ifelse( kicks_2019$Stadium =="NRG Stadium" | kicks_2019$Stadium =="Lucas Oil Stadium"| kicks_2019$Stadium =="Allegiant Stadium"
                                                                      | kicks_2019$Stadium == "AT&T Stadium"| kicks_2019$Stadium =="Caesars Superdome" 
                                                                      | kicks_2019$Stadium =="Ford Field" | kicks_2019$Stadium =="U.S. Bank Stadium"| kicks_2019$Stadium =="Mercedes-Benz Stadium"
                                                                      | kicks_2019$Stadium =="State Farm Stadium", "Dome", "X")))))

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

kicks_2020$Stadium_Direction1 <-ifelse(kicks_2020$Stadium == "Highmark Stadium"| kicks_2020$Stadium =="Hard Rock Stadium"| kicks_2020$Stadium == "Paycor Stadium"| kicks_2020$Stadium =="Arrowhead Stadium" 
                         | kicks_2020$Stadium =="Levi's Stadium" | kicks_2020$Stadium =="Bank of America Stadium", "NW",
                         ifelse(kicks_2020$Stadium == "Gillette Stadium"| kicks_2020$Stadium =="Metlife Stadium"| kicks_2020$Stadium =="Acrisure Stadium"| kicks_2020$Stadium == "Nissan Stadium"
                                | kicks_2020$Stadium == "Everbank Stadium"| kicks_2020$Stadium =="Empower Field at Mile High Stadium"| kicks_2020$Stadium =="Lincoln Financial Field"
                                | kicks_2020$Stadium =="Lambeau Field"| kicks_2020$Stadium == "Soldier Field"| kicks_2020$Stadium == "Lumen Field"| kicks_2020$Stadium =="Raymond James Stadium", "N",
                                ifelse(kicks_2020$Stadium == "M&T Bank Stadium"| kicks_2020$Stadium == "FedEx Field", "W",
                                        ifelse(kicks_2020$Stadium =="Cleveland Browns Stadium", "NE",
                                                ifelse( kicks_2020$Stadium =="NRG Stadium" | kicks_2020$Stadium =="Lucas Oil Stadium"| kicks_2020$Stadium =="Allegiant Stadium"
                                                        | kicks_2020$Stadium =="SoFi Stadium"| kicks_2020$Stadium == "AT&T Stadium"| kicks_2020$Stadium =="Caesars Superdome" 
                                                        | kicks_2020$Stadium =="Ford Field" | kicks_2020$Stadium =="U.S. Bank Stadium"| kicks_2020$Stadium =="Mercedes-Benz Stadium"
                                                        | kicks_2020$Stadium =="State Farm Stadium", "Dome", "X")))))

kicks_2020$Stadium_Direction2 <-ifelse(kicks_2020$Stadium == "Highmark Stadium"| kicks_2020$Stadium =="Hard Rock Stadium"| kicks_2020$Stadium == "Paycor Stadium"| kicks_2020$Stadium =="Arrowhead Stadium" 
                                       | kicks_2020$Stadium =="Levi's Stadium" | kicks_2020$Stadium =="Bank of America Stadium", "SE",
                                       ifelse(kicks_2020$Stadium == "Gillette Stadium"| kicks_2020$Stadium =="Metlife Stadium"| kicks_2020$Stadium =="Acrisure Stadium"| kicks_2020$Stadium == "Nissan Stadium"
                                              | kicks_2020$Stadium == "Everbank Stadium"| kicks_2020$Stadium =="Empower Field at Mile High Stadium"| kicks_2020$Stadium =="Lincoln Financial Field"
                                              | kicks_2020$Stadium =="Lambeau Field"| kicks_2020$Stadium == "Soldier Field"| kicks_2020$Stadium == "Lumen Field"| kicks_2020$Stadium =="Raymond James Stadium", "S",
                                              ifelse(kicks_2020$Stadium == "M&T Bank Stadium"| kicks_2020$Stadium == "FedEx Field", "E",
                                                     ifelse(kicks_2020$Stadium =="Cleveland Browns Stadium", "SW",
                                                            ifelse( kicks_2020$Stadium =="NRG Stadium" | kicks_2020$Stadium =="Lucas Oil Stadium"| kicks_2020$Stadium =="Allegiant Stadium"
                                                                    | kicks_2020$Stadium =="SoFi Stadium"| kicks_2020$Stadium == "AT&T Stadium"| kicks_2020$Stadium =="Caesars Superdome" 
                                                                    | kicks_2020$Stadium =="Ford Field" | kicks_2020$Stadium =="U.S. Bank Stadium"| kicks_2020$Stadium =="Mercedes-Benz Stadium"
                                                                    | kicks_2020$Stadium =="State Farm Stadium", "Dome", "X")))))

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

kicker_experience_19 <- c("Eddy Pineiro" = 1, "Mason Crosby" = 13, "Greg Zuerlein" = 8, "Dustin Hopkins" = 5,
                       "Jake Elliott" = 3, "Kaare Vedvik" = 1, "Stephen Hauschka" = 12, "Dan Bailey" = 9,
                       "Justin Tucker" = 8, "Jason Sanders" = 2, "Harrison Butker" = 3, "Josh Lambo" = 5,
                       "Cairo Santos" = 6, "Ty Long" = 1, "Adam Vinatieri" = 24, "Randy Bullock" = 7,
                       "Jason Myers" = 5, "Robbie Gould" = 15, "Matt Gay" = 1, "Aldrick Rosas" = 3, "Austin Seibert" = 1,
                       "Brett Maher" = 2, "Matt Prater" = 13, "Zane Gonzalez" = 3, "Stephen Gostkowski" = 14,
                       "Chris Boswell" = 5, "Kaimi Fairbairn" = 3, "Wil Lutz" = 4, "Daniel Carlson" = 2,
                       "Brandon McManus" = 6, "Matt Bryant" = 18, "Sam Ficken" = 3, "Mike Nugent" = 15,
                       "Chase McLaughlin" = 1, "Cody Parkey" = 6, "Ryan Succop" = 11, "Michael Badgley" = 2,
                       "Nick Folk" = 13, "Younghoe Koo" = 3, "Kai Forbath" = 8, "Greg Joseph" = 2, "Joey Slye" = 1)

kicks_2019$Kicker_Experience <- kicker_experience_19[kicks_2019$Kicker] 

kicker_experience_20 <- c("Mason Crosby" = 14, "Greg Zuerlein" = 9, "Dustin Hopkins" = 6, "Rodrigo Blankenship" = 1, "Taylor Russolino" = 1,
                          "Jake Elliott" = 4, "Stephen Hauschka" = 13, "Dan Bailey" = 10, "Tyler Bass" = 1, "Matthew Wright" = 1,
                          "Justin Tucker" = 9, "Jason Sanders" = 3, "Harrison Butker" = 4, "Josh Lambo" = 6, "Sergio Castillo" = 1,
                          "Cairo Santos" = 7, "Ty Long" = 1, "Adam Vinatieri" = 24, "Randy Bullock" = 8, "Samuel Sloman" = 1,
                          "Jason Myers" = 6, "Robbie Gould" = 16, "Matt Gay" = 2, "Aldrick Rosas" = 4, "Austin Seibert" = 2,
                          "Brett Maher" = 2, "Matt Prater" = 14, "Zane Gonzalez" = 4, "Stephen Gostkowski" = 15, "Graham Gano" = 11,
                          "Chris Boswell" = 6, "Kaimi Fairbairn" = 4, "Wil Lutz" = 5, "Daniel Carlson" = 3, "Elliott Fry" = 1,
                          "Brandon McManus" = 7, "Matt Bryant" = 18, "Sam Ficken" = 4, "Mike Nugent" = 16, "Jon Brown" = 1,
                          "Chase McLaughlin" = 2, "Cody Parkey" = 7, "Ryan Succop" = 12, "Michael Badgley" = 3, "Brandon Wright" = 1,
                          "Nick Folk" = 14, "Younghoe Koo" = 4, "Kai Forbath" = 9, "Greg Joseph" = 2, "Joey Slye" = 2, "Tristian Vizcaino" = 1)

kicks_2020$Kicker_Experience <- kicker_experience_20[kicks_2020$Kicker] 

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

custom_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "WC")

data_2020$Week <- factor(data_2020$Week, levels = custom_order)
data_2019$Week <- factor(data_2019$Week, levels = custom_order)

outdoor_games_2019 <- subset(data_2019, Weather != "Dome")
outdoor_games_2020 <- subset(data_2020, Weather != "Dome")

outdoor_games_2019$Temp <- as.numeric(outdoor_games_2019$Temp)
outdoor_games_2020$Temp <- as.numeric(outdoor_games_2020$Temp)
outdoor_games_2019$Air <- as.numeric(outdoor_games_2019$Air)
outdoor_games_2020$Air <- as.numeric(outdoor_games_2020$Air)

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
lv1_kicks_2019 <- subset(game_2019, Stadium == "Oakland Collesium")
lac1_kicks_2019 <- subset(game_2019, Stadium == "Dignity Health Sports Park")
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
lar1_kicks_2019 <- subset(game_2019, Stadium == "LA Colliseum")
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
lv_kicks_2020 <- subset(game_2020, Stadium == "Allegiant Stadium")
lac_kicks_2020 <- subset(game_2020, Stadium == "SoFi Stadium")
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
lar_kicks_2020 <- subset(game_2020, Stadium == "SoFi Stadium")
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

teams_19 <- c("BUF", "MIA", "NE", "NYJ", "PIT", "BAL", "CLE", "CIN", "HOU", "IND", "TEN", "JAX", "KC", "LV1", "LAC1", "DEN", "NYG", "PHI", "DAL", "WAS", "GB", "CHI", "MIN", "DET", "NO", "CAR", "ATL", "TB", "LAR1", "SF", "ARI", "SEA")
teams <- c("BUF", "MIA", "NE", "NYJ", "PIT", "BAL", "CLE", "CIN", "HOU", "IND", "TEN", "JAX", "KC", "LV", "LAC", "DEN", "NYG", "PHI", "DAL", "WAS", "GB", "CHI", "MIN", "DET", "NO", "CAR", "ATL", "TB", "LAR", "SF", "ARI", "SEA")

for (team in teams_19) {
  team_kicks <- get(paste(tolower(team), "_kicks_2019", sep = ""))
  nfl_stadiums$FG_per_2019[nfl_stadiums$Team == team] <- weighted.mean(team_kicks$FG_per, team_kicks$Total_FGA)}

for (team in teams_19) {
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
top_10_XP_per_2019 <- head(nfl_stadiums[order(-nfl_stadiums$XP_per_2019), ], 10)
top_10_XP_per_2019$Stadium <- factor(top_10_XP_per_2019$Stadium, levels = rev(top_10_XP_per_2019$Stadium[order(top_10_XP_per_2019$XP_per_2019)]))

FG_2019_Graph <- ggplot(top_10_FG_per_2019, aes(x = Stadium, y = FG_per_2019, fill = Stadium)) +
  labs(title = "2019 Highest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("darkturquoise", "darkblue", "mediumpurple1", "gold4", "darkorange1",
                               "black", "darkgreen", "purple3", "yellow", "firebrick3")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00)) +
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black")

XP_2019_Graph <-  ggplot(top_10_XP_per_2019, aes(x = Stadium, y = XP_per_2019, fill = Stadium)) +
                          labs(title = "2019 Highest XP%", x = NULL, y = "XP%") +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                          geom_bar(stat = "identity", width = 0.5) +
                          scale_fill_manual(values = c("darkorange1", "firebrick3", "blue", "gold3", "grey40",
                                                       "navy", "springgreen2", "yellow", "mediumpurple1", "darkturquoise")) +
                          scale_x_discrete(breaks = NULL) +
                          coord_cartesian(ylim = c(0.90, 1.00)) +
                          geom_hline(yintercept = c(0.90, 0.925, 0.95, 0.975, 1.00), 
                                     linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
                                     color = "black")

top_10_FG_per_2020 <- head(nfl_stadiums[order(-nfl_stadiums$FG_per_2020), ], 11)
top_10_FG_per_2020 <- top_10_FG_per_2020[!duplicated(top_10_FG_per_2020$Stadium), ]
top_10_FG_per_2020$Stadium <- factor(top_10_FG_per_2020$Stadium, levels = rev(top_10_FG_per_2020$Stadium[order(top_10_FG_per_2020$FG_per_2020)]))
top_10_XP_per_2020 <- head(nfl_stadiums[order(-nfl_stadiums$XP_per_2020), ], 10)
top_10_XP_per_2020 <- top_10_XP_per_2020[!duplicated(top_10_XP_per_2020$Stadium), ]
top_10_XP_per_2020$Stadium <- factor(top_10_XP_per_2020$Stadium, levels = rev(top_10_XP_per_2020$Stadium[order(top_10_XP_per_2020$XP_per_2020)]))

FG_2020_Graph <- ggplot(top_10_FG_per_2020, aes(x = Stadium, y = FG_per_2020, fill = Stadium)) +
  labs(title = "2020 Highest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("darkgreen", "blue", "grey40", "yellow", "darkorange1",
                               "lightblue", "lawngreen", "firebrick", "navy", "red1")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

XP_2020_Graph <-  ggplot(top_10_XP_per_2020, aes(x = Stadium, y = XP_per_2020, fill = Stadium)) +
  labs(title = "2020 Highest XP%", x = NULL, y = "XP%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("royalblue1", "cyan", "yellow", "darkblue", "black",
                               "gold4", "darkred", "grey40", "blue", "firebrick3")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.90, 1.00)) +
  geom_hline(yintercept = c(0.90, 0.925, 0.95, 0.975, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black")

#worst percentage
last_10_FG_per_2019 <- head(nfl_stadiums[order(nfl_stadiums$FG_per_2019), ], 11)
last_10_FG_per_2019 <- last_10_FG_per_2019[!duplicated(last_10_FG_per_2019$Stadium), ]
last_10_FG_per_2019$Stadium <- factor(last_10_FG_per_2019$Stadium, levels = (last_10_FG_per_2019$Stadium[order(last_10_FG_per_2019$FG_per_2019)]))

last_10_XP_per_2019 <- head(nfl_stadiums[order(nfl_stadiums$XP_per_2019), ], 11)
last_10_XP_per_2019 <- last_10_XP_per_2019[!duplicated(last_10_XP_per_2019$Stadium), ]
last_10_XP_per_2019$Stadium <- factor(last_10_XP_per_2019$Stadium, levels = (last_10_XP_per_2019$Stadium[order(last_10_XP_per_2019$XP_per_2019)]))

last_FG_2019_Graph <- ggplot(last_10_FG_per_2019, aes(x = Stadium, y = FG_per_2019, fill = Stadium)) +
  labs(title = "2019 Lowest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("black", "darkred", "lightcoral", "dodgerblue", "darkorange1",
                               "grey", "cyan", "sienna4", "yellow1", "blue", "green")) +
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
                               "yellow1", "red3", "lawngreen", "cyan", "purple", "red")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 
#2020
last_10_FG_per_2020 <- head(nfl_stadiums[order(nfl_stadiums$FG_per_2020), ], 10)
last_10_FG_per_2020 <- last_10_FG_per_2020[!duplicated(last_10_FG_per_2020$Stadium), ]
last_10_FG_per_2020$Stadium <- factor(last_10_FG_per_2020$Stadium, levels = (last_10_FG_per_2020$Stadium[order(last_10_FG_per_2020$FG_per_2020)]))

last_10_XP_per_2020 <- head(nfl_stadiums[order(nfl_stadiums$XP_per_2020), ], 11)
last_10_XP_per_2020 <- last_10_XP_per_2020[!duplicated(last_10_XP_per_2020$Stadium), ]
last_10_XP_per_2020$Stadium <- factor(last_10_XP_per_2020$Stadium, levels = (last_10_XP_per_2020$Stadium[order(last_10_XP_per_2020$XP_per_2020)]))

last_FG_2020_Graph <- ggplot(last_10_FG_per_2020, aes(x = Stadium, y = FG_per_2020, fill = Stadium)) +
  labs(title = "2020 Lowest FG%", x = NULL, y = "FG%") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("purple", "blue", "dodgerblue", "chocolate1",
                               "red3", "springgreen", "darkred", "darkturquoise", "cyan", "brown")) +
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
                               "navy", "mediumpurple2", "darkgreen", "lawngreen", "gold", "red")) +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0.80, 1.00))+
  geom_hline(yintercept = c(0.80, 0.85, 0.90, 0.95, 1.00), 
             linetype = c("dashed", "dashed", "dashed", "dashed", "dashed"), 
             color = "black") 

#Stadium Temp
outdoor_games_19 <- data_2019 %>%
  filter(!grepl("Dome", Temp, ignore.case = TRUE))

calculate_avg_temp <- function(data, stadium_name) {
  stadium_games <- data %>%
    filter(grepl(stadium_name, Stadium, ignore.case = TRUE))
  stadium_games$Temp <- as.integer(stadium_games$Temp)
  avg_temp <- mean(stadium_games$Temp, na.rm = TRUE)
  return(avg_temp)}

buf_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Highmark Stadium")
mia_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Hard Rock Stadium")
ne_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Gillette Stadium")
nyj_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Metlife Stadium")
pit_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Acrisure Stadium")
bal_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "M&T Bank Stadium")
cle_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Cleveland Browns Stadium")
cin_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Paycor Stadium")
hou_avg_temp_19 <- "Dome"
ind_avg_temp_19 <- "Dome"
ten_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Nissan Stadium")
jax_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Everbank Stadium")
kc_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Arrowhead Stadium")
lv_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Oakland Collesium")
lac_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Dignity Health Sports Park")
den_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Empower Field at Mile High Stadium")
nyg_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Metlife Stadium")
phi_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Lincoln Financial Field")
dal_avg_temp_19 <- "Dome"
was_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "FedEx Field")
gb_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Lambeau Field")
chi_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Soldier Field")
min_avg_temp_19 <- "Dome"
det_avg_temp_19 <- "Dome"
no_avg_temp_19 <- "Dome"
car_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Bank of America Stadium")
atl_avg_temp_19 <- "Dome"
tb_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Raymond James Stadium")
lar_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "LA Colliseum")
sf_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Levi's Stadium")
ari_avg_temp_19 <- "Dome"
sea_avg_temp_19 <- calculate_avg_temp(outdoor_games_19, "Lumen Field")

nfl_stadiums$Avg_temp_2019 <- NA

nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "BUF"] <- buf_avg_temp_19 
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "MIA"] <- mia_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "NE"] <- ne_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "NYJ"] <- nyj_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "PIT"] <- pit_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "BAL"] <- bal_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "CLE"] <- cle_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "CIN"] <- cin_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "HOU"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "IND"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "TEN"] <- ten_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "JAX"] <- jax_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "KC"] <- kc_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "LV1"] <- lv_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "LAC1"] <- lac_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "DEN"] <- den_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "NYG"] <- nyj_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "PHI"] <- phi_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "DAL"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "WAS"] <- was_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "GB"] <- gb_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "CHI"] <- chi_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "MIN"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "DET"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "NO"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "CAR"] <- car_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "DET"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "TB"] <- tb_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "LAR1"] <- lar_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "SF"] <- sf_avg_temp_19
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "ARI"] <- "Dome"
nfl_stadiums$Avg_temp_2019[nfl_stadiums$Team== "SEA"] <- sea_avg_temp_19

outdoor_games_20 <- data_2020 %>%
  filter(!grepl("Dome", Temp, ignore.case = TRUE))

calculate_avg_temp <- function(data, stadium_name) {
  stadium_games <- data %>%
    filter(grepl(stadium_name, Stadium, ignore.case = TRUE))
  stadium_games$Temp <- as.integer(stadium_games$Temp)
  avg_temp <- mean(stadium_games$Temp, na.rm = TRUE)
  return(avg_temp)}

buf_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Highmark Stadium")
mia_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Hard Rock Stadium")
ne_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Gillette Stadium")
nyj_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Metlife Stadium")
pit_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Acrisure Stadium")
bal_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "M&T Bank Stadium")
cle_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Cleveland Browns Stadium")
cin_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Paycor Stadium")
hou_avg_temp_20 <- "Dome"
ind_avg_temp_20 <- "Dome"
ten_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Nissan Stadium")
jax_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Everbank Stadium")
kc_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Arrowhead Stadium")
lv_avg_temp_20 <- "Dome"
lac_avg_temp_20 <- "Dome"
den_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Empower Field at Mile High Stadium")
nyg_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Metlife Stadium")
phi_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Lincoln Financial Field")
dal_avg_temp_20 <- "Dome"
was_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "FedEx Field")
gb_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Lambeau Field")
chi_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Soldier Field")
min_avg_temp_20 <- "Dome"
det_avg_temp_20 <- "Dome"
no_avg_temp_20 <- "Dome"
car_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Bank of America Stadium")
atl_avg_temp_20 <- "Dome"
tb_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Raymond James Stadium")
lar_avg_temp_20 <- "Dome"
sf_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Levi's Stadium")
ari_avg_temp_20 <- "Dome"
sea_avg_temp_20 <- calculate_avg_temp(outdoor_games_20, "Lumen Field")

nfl_stadiums$Avg_temp_2020 <- NA

nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "BUF"] <- buf_avg_temp_20 
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "MIA"] <- mia_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "NE"] <- ne_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "NYJ"] <- nyj_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "PIT"] <- pit_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "BAL"] <- bal_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "CLE"] <- cle_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "CIN"] <- cin_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "HOU"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "IND"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "TEN"] <- ten_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "JAX"] <- jax_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "KC"] <- kc_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "LV"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "LAC"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "DEN"] <- den_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "NYG"] <- nyj_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "PHI"] <- phi_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "DAL"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "WAS"] <- was_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "GB"] <- gb_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "CHI"] <- chi_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "MIN"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "DET"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "NO"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "CAR"] <- car_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "DET"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "TB"] <- tb_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "LAR"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "SF"] <- sf_avg_temp_20
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "ARI"] <- "Dome"
nfl_stadiums$Avg_temp_2020[nfl_stadiums$Team== "SEA"] <- sea_avg_temp_20

nfl_stadiums$Avg_temp_2019 <- as.numeric(nfl_stadiums$Avg_temp_2019)
nfl_stadiums$Avg_temp_2020 <- as.numeric(nfl_stadiums$Avg_temp_2020)

nfl_stadiums$Avg_temp_2019 <- round(nfl_stadiums$Avg_temp_2019, 1)
nfl_stadiums$XP_per_2019 <- round(nfl_stadiums$XP_per_2019, 3)
nfl_stadiums$FG_per_2019 <- round(nfl_stadiums$FG_per_2019, 3)

nfl_stadiums$Avg_temp_2020 <- round(nfl_stadiums$Avg_temp_2020, 1)
nfl_stadiums$XP_per_2020 <- round(nfl_stadiums$XP_per_2020, 3)
nfl_stadiums$FG_per_2020 <- round(nfl_stadiums$FG_per_2020, 3)

plot_temp_19 <- subset(nfl_stadiums, !is.na(Avg_temp_2019))
plot_temp_19$Avg_temp_2019 <- as.numeric(plot_temp_19$Avg_temp_2019)

temp_19 <- ggplot(plot_temp_19, aes(x = Stadium, y = Avg_temp_2019)) +
  geom_point(size = 3) +
  ylim(30, 90) +
  labs(title = "2019 Average Stadium Temperature",
       x = "Stadium",
       y = "Average Temperature (Fahrenheit)")+
  theme_minimal() +
  scale_y_continuous(limits = c(30, 90), breaks = seq(30, 90, by = 10)) +
  geom_hline(yintercept = c(30, 40, 50, 60, 70, 80, 90), 
             linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 9, face = "bold"))

plot_temp_20 <- subset(nfl_stadiums, !is.na(Avg_temp_2020))
plot_temp_20$Avg_temp_2020 <- as.numeric(plot_temp_20$Avg_temp_2020)

temp_20 <- ggplot(plot_temp_20, aes(x = Stadium, y = Avg_temp_2020)) +
  geom_point(size = 3) +
  ylim(30, 90) +
  labs(title = "2020 Average Stadium Temperature",
       x = "Stadium",
       y = "Average Temperature (Fahrenheit)")+
  theme_minimal() +
  scale_y_continuous(limits = c(30, 90), breaks = seq(30, 90, by = 10)) +
  geom_hline(yintercept = c(30, 40, 50, 60, 70, 80, 90), 
             linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 9, face = "bold"))

temp_19 <- ggplot(plot_temp_19, aes(x = Stadium, y = Avg_temp_2019)) +
  geom_point(size = 3) +
  ylim(30, 90) +
  labs(title = "2019 vs 2020 Average Stadium Temperature",
       x = "Stadium",
       y = "Average Temperature (Fahrenheit)")+
  theme_minimal() +
  scale_y_continuous(limits = c(30, 90), breaks = seq(30, 90, by = 10)) +
  geom_hline(yintercept = c(30, 40, 50, 60, 70, 80, 90), 
             linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 9, face = "bold"))

temp_20 <- ggplot(plot_temp_20, aes(x = Stadium, y = Avg_temp_2020)) +
  geom_point(size = 3, color = "red") + 
  ylim(30, 90) +
  labs(title = "2019 vs 2020 Average Stadium Temperature",
       x = "Stadium",
       y = "Average Temperature (Fahrenheit)")+
  theme_minimal() +
  scale_y_continuous(limits = c(30, 90), breaks = seq(30, 90, by = 10)) +
  geom_hline(yintercept = c(30, 40, 50, 60, 70, 80, 90), 
             linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, size = 9, face = "bold"))

kicks_2019$Outcome_binary <- ifelse(kicks_2019$Outcome == "Make", 1, 0)
kicks_2020 <- kicks_2020[, -ncol(kicks_2020)]
kicks_2020$Outcome_binary <- ifelse(kicks_2020$Outcome == "Make", 1, 0)

kicks_19_20 <- rbind(kicks_2019, kicks_2020)
data_19_20 <- rbind(data_2019, data_2020)
game_19_20 <- rbind(game_2019, game_2020)

data_subset1 <- data_19_20[, c(1, 6:15, 19:22)]
data_subset2 <- game_19_20[, c(1, 8:21)]

merged_df1 <- merge(kicks_19_20, data_subset1, by = "Game_ID")
write.xlsx(merged_df1, file = "Data Cleaned.xlsx", rowNames = T)

#Logistic Model
model2 <- glm(Outcome_binary ~ Kick + Yards + Stadium + Kicker_Experience + Week + Weather,
              data = merged_df1, family = "binomial")
summary(model2)
anova(model2)
vif(model2)
bptest(model2)
dwtest(model2)
outlierTest(model2)

y_pred_prob <- predict(model2, newdata = merged_df1, type = "response")
roc_curve <- roc(merged_df1$Outcome_binary, y_pred_prob)

plot(roc_curve, main = "ROC Curve for Logistic Regression Model",
     xlab = "False Positive Rate", ylab = "True Positive Rate")

auc_value <- auc(roc_curve)
print(paste("AUC for Logistic Regression Model:", auc_value))

print(colnames(merged_df1))

#XGBoost Model
X2 <- merged_df1[, c('Kick', 'Yards', 'Stadium', 'Kicker_Experience', 'Week', 'Weather', 'Temp')]
y2 <- merged_df1$Outcome_binary
X2 <- model.matrix(~ . - 1, data = X2)
set.seed(42) 
train_index <- sample(1:nrow(X2), 0.8 * nrow(X2))
X_train <- X2[train_index, ]
y_train <- y2[train_index]
X_test <- X2[-train_index, ]
y_test <- y2[-train_index]
model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 10, objective = "binary:logistic")
y_pred <- predict(model, as.matrix(X_test))
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)
accuracy_xgboost2 <- mean(y_pred_class == y_test)
print(paste("XGBoost Model Accuracy:", accuracy_xgboost2))

#Rshiny

# Define UI for application
ui <- fluidPage(
  titlePanel("XGBoost Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("kick_input", "Kick:", value = XP or FG),
      numericInput("yards_input", "Yards:", value = min = 17 to 106),
      selectInput("stadium_input", "Stadium:",
                  choices = c("Stadium A", "Stadium B", "Stadium C")),
      numericInput("kicker_exp_input", "Kicker Experience:", value = 2),
      numericInput("week_input", "Week:", value = 3),
      selectInput("weather_input", "Weather:",
                  choices = c("Sunny", "Rainy", "Cloudy")),
      numericInput("temp_input", "Temperature:", value = 70),
      actionButton("predict_button", "Predict")),
    mainPanel(
      textOutput("prediction_output"))))

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict_button, {
    new_data <- data.frame(
      Kick = input$kick_input,
      Yards = input$yards_input,
      Stadium = input$stadium_input,
      Kicker_Experience = input$kicker_exp_input,
      Week = input$week_input,
      Weather = input$weather_input,
      Temp = input$temp_input
    )
    
    # Preprocess new data
    new_data$Stadium <- as.factor(new_data$Stadium)
    new_data$Weather <- as.factor(new_data$Weather)
    new_data <- model.matrix(~ . - 1, data = new_data)
    
    # Make prediction
    y_pred <- predict(model, as.matrix(new_data))
    output$prediction_output <- renderText({
      paste("Probability of success:", round(y_pred, 4))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

#Temp + Wind
outdoor_games_19_20 <- rbind(outdoor_games_2019, outdoor_games_2020)

average_temp_by_stadium <- outdoor_games_19_20 %>%
  group_by(Stadium) %>%
  summarise(Average_Temp = mean(Temp, na.rm = TRUE))

coldest_stadiums <- average_temp_by_stadium %>%
  arrange(Average_Temp) %>%
  top_n(-10, Average_Temp)

warmest_stadiums <- average_temp_by_stadium %>%
  arrange(Average_Temp) %>%
  top_n(10, Average_Temp)

average_wind_by_stadium <- outdoor_games_19_20 %>%
  group_by(Stadium) %>%
  summarise(Average_Wind = mean(Air, na.rm = TRUE))

#Final Plots
ggarrange(all, current, int, ncol = 3, nrow = 1, common.legend = TRUE,
          legend = "bottom",  widths = c(1, 1, 0.5))

ggarrange(temp_2019, temp_2020, ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "right")
  
ggarrange(wind_2019, wind_2020, ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "right")

ggarrange(XP_2019_Graph, XP_2020_Graph, FG_2019_Graph,
          FG_2020_Graph, ncol = 2, nrow = 2,
          common.legend = FALSE, legend = "right")

ggarrange(last_XP_2019_Graph, last_FG_2019_Graph, last_XP_2020_Graph,
          last_FG_2020_Graph, ncol = 2, nrow = 2,
          common.legend = FALSE, legend = "right")
