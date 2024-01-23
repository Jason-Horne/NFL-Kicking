#library calls
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

#Import Data
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

#Edit Data
data_2019$Home_Conf <-
  with(data_2019, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE" | Home == "PIT"|
                           Home == "BAL"|Home == "CLE"|Home == "CIN"|Home == "TEN"| Home == "HOU"| Home == "JAX"| 
                           Home == "IND"| Home == "KC"| Home == "LAC"| Home == "DEN"| Home == "LV", "AFC", "NFC" ))
data_2019$Away_Conf <-
  with(data_2019, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE" | Away == "PIT"|
                           Away == "BAL"|Away == "CLE"|Away == "CIN"|Away == "TEN"| Away == "HOU"| Away == "JAX"| 
                           Away == "IND"| Away == "KC"| Away == "LAC"|Away == "DEN"| Away == "LV", "AFC", "NFC" ))

data_2019$Away_Div <-
  with(data_2019, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE", "AFC East",
                         ifelse(Away == "PIT" | Away == "CLE" | Away == "CIN" | Away == "BAL", "AFC North",
                                ifelse(Away == "HOU" | Away == "JAX" | Away == "IND" | Away == "TEN", "AFC South",
                                       ifelse(Away == "KC" | Away == "DEN" | Away == "LAC" | Away == "LV", "AFC West",
                                              ifelse(Away == "PHI" | Away == "DAL" |Away == "NYG" | Away == "WAS", "NFC East",
                                                     ifelse(Away == "GB" | Away == "CHI" |Away == "DET" | Away == "MIN", "NFC North",
                                                            ifelse(Away == "TB" | Away == "NO" |Away == "CAR" | Away == "ATL", "NFC South",
                                                                   ifelse(Away == "LAR" | Away == "SEA" |  Away == "SF" | Away == "ARI", "NFC West","NFC")))))))))

data_2019$Home_Div <-
  with(data_2019, ifelse(Home == "BUF" | Home == "NYJ" |Home == "MIA" | Home == "NE", "AFC East",
                         ifelse(Home == "PIT" | Home == "CLE" |  Home == "CIN" | Home == "BAL", "AFC North",
                                ifelse(Home == "HOU" | Home == "JAX" |Home == "IND" | Home == "TEN", "AFC South",
                                       ifelse(Home == "KC" | Home == "DEN" | Home == "LAC" | Home == "LV", "AFC West",
                                              ifelse(Home == "PHI" | Home == "DAL" | Home == "NYG" | Home == "WAS", "NFC East",
                                                     ifelse(Home == "GB" | Home == "CHI" |Home == "DET" | Home == "MIN", "NFC North",
                                                            ifelse(Home == "TB" | Home == "NO" |Home == "CAR" | Home == "ATL", "NFC South",
                                                                   ifelse(Home == "LAR" | Home == "SEA" |Home == "SF" | Home == "ARI", "NFC West","NFC")))))))))

data_2020$Home_Conf <-
  with(data_2020, ifelse(Home == "BUF" | Home == "NYJ" |Home == "MIA" | Home == "NE" | Home == "PIT"|
                           Home == "BAL"|Home == "CLE"|Home == "CIN"|Home == "TEN"| Home == "HOU"| Home == "JAX"| 
                           Home == "IND"| Home == "KC"| Home == "LAC"|Home == "DEN"| Home == "LV", "AFC", "NFC" ))
data_2020$Away_Conf <-
  with(data_2020, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE" | Away == "PIT"|
                           Away == "BAL"|Away == "CLE"|Away == "CIN"|Away == "TEN"| Away == "HOU"| Away == "JAX"| 
                           Away == "IND"| Away == "KC"| Away == "LAC"| Away == "DEN"| Away == "LV", "AFC", "NFC" ))

data_2020$Away_Div <-
  with(data_2020, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE", "AFC East",
                         ifelse(Away == "PIT" | Away == "CLE" |  Away == "CIN" | Away == "BAL", "AFC North",
                                ifelse(Away == "HOU" | Away == "JAX" | Away == "IND" | Away == "TEN", "AFC South",
                                       ifelse(Away == "KC" | Away == "DEN" | Away == "LAC" | Away == "LV", "AFC West",
                                              ifelse(Away == "PHI" | Away == "DAL" | Away == "NYG" | Away == "WAS", "NFC East",
                                                     ifelse(Away == "GB" | Away == "CHI" | Away == "DET" | Away == "MIN", "NFC North",
                                                            ifelse(Away == "CAR" | Away == "ATL" | Away == "NO" | Away == "TB", "NFC South",
                                                                   ifelse(Away == "LAR" | Away == "SEA" | Away == "SF" | Away == "ARI", "NFC West","NFC")))))))))

data_2020$Home_Div <-
  with(data_2020, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE", "AFC East",
                         ifelse(Home == "PIT" | Home == "CLE" | Home == "CIN" | Home == "BAL", "AFC North",
                                ifelse(Home == "HOU" | Home == "JAX" | Home == "IND" | Home == "TEN", "AFC South",
                                       ifelse(Home == "KC" | Home == "DEN" |  Home == "LAC" | Home == "LV", "AFC West",
                                              ifelse(Home == "PHI" | Home == "DAL" | Home == "NYG" | Home == "WAS", "NFC East",
                                                     ifelse(Home == "GB" | Home == "CHI" | Home == "DET" | Home == "MIN", "NFC North",
                                                            ifelse(Home == "TB" | Home == "NO" | Home == "CAR" | Home == "ATL", "NFC South",
                                                                   ifelse(Home == "LAR" | Home == "SEA" |Home == "SF" | Home == "ARI", "NFC West","NFC")))))))))

data_2021$Home_Conf <-
  with(data_2021, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE" | Home == "PIT"|
                           Home == "BAL"|Home == "CLE"|Home == "CIN"|Home == "TEN"| Home == "HOU"| Home == "JAX"| 
                           Home == "IND"| Home == "KC"| Home == "LAC"| Home == "DEN"| Home == "LV", "AFC", "NFC" ))
data_2021$Away_Conf <-
  with(data_2021, ifelse(Away == "BUF" | Away == "NYJ" |  Away == "MIA" | Away == "NE" | Away == "PIT"|
                           Away == "BAL"|Away == "CLE"|Away == "CIN"| Away == "TEN"| Away == "HOU"| Away == "JAX"| 
                           Away == "IND"| Away == "KC"| Away == "LAC"| Away == "DEN"| Away == "LV", "AFC", "NFC" ))

data_2021$Away_Div <-
  with(data_2021, ifelse(Away == "BUF" | Away == "NYJ" |  Away == "MIA" | Away == "NE", "AFC East",
                         ifelse(Away == "PIT" | Away == "CLE" | Away == "CIN" | Away == "BAL", "AFC North",
                                ifelse(Away == "HOU" | Away == "JAX" | Away == "IND" | Away == "TEN", "AFC South",
                                       ifelse(Away == "KC" | Away == "DEN" | Away == "LAC" | Away == "LV", "AFC West",
                                              ifelse(Away == "PHI" | Away == "DAL" |  Away == "NYG" | Away == "WAS", "NFC East",
                                                     ifelse(Away == "GB" | Away == "CHI" | Away == "DET" | Away == "MIN", "NFC North",
                                                            ifelse(Away == "TB" | Away == "NO" | Away == "CAR" | Away == "ATL", "NFC South",
                                                                   ifelse(Away == "LAR" | Away == "SEA" |Away == "SF" | Away == "ARI", "NFC West","NFC")))))))))

data_2021$Home_Div <-
  with(data_2021, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE", "AFC East",
                         ifelse(Home == "PIT" | Home == "CLE" |  Home == "CIN" | Home == "BAL", "AFC North",
                                ifelse(Home == "HOU" | Home == "JAX" |  Home == "IND" | Home == "TEN", "AFC South",
                                       ifelse(Home == "KC" | Home == "DEN" | Home == "LAC" | Home == "LV", "AFC West",
                                              ifelse(Home == "PHI" | Home == "DAL" |  Home == "NYG" | Home == "WAS", "NFC East",
                                                     ifelse(Home == "GB" | Home == "CHI" | Home == "DET" | Home == "MIN", "NFC North",
                                                            ifelse(Home == "TB" | Home == "NO" |  Home == "CAR" | Home == "ATL", "NFC South",
                                                                   ifelse(Home == "LAR" | Home == "SEA" | Home == "SF" | Home == "ARI", "NFC West","NFC")))))))))

data_2022$Home_Conf <-
  with(data_2022, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE" | Home == "PIT"|
                           Home == "BAL"|Home == "CLE"|Home == "CIN"| Home == "TEN"| Home == "HOU"| Home == "JAX"| 
                           Home == "IND"| Home == "KC"| Home == "LAC"| Home == "DEN"| Home == "LV", "AFC", "NFC" ))
data_2022$Away_Conf <-
  with(data_2022, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE" | Away == "PIT"|
                           Away == "BAL"|Away == "CLE"|Away == "CIN"|Away == "TEN"| Away == "HOU"| Away == "JAX"| 
                           Away == "IND"| Away == "KC"| Away == "LAC"|Away == "DEN"| Away == "LV", "AFC", "NFC" ))

data_2022$Away_Div <-
  with(data_2022, ifelse(Away == "BUF" | Away == "NYJ" |Away == "MIA" | Away == "NE", "AFC East",
                         ifelse(Away == "PIT" | Away == "CLE" | Away == "CIN" | Away == "BAL", "AFC North",
                                ifelse(Away == "HOU" | Away == "JAX" | Away == "IND" | Away == "TEN", "AFC South",
                                       ifelse(Away == "KC" | Away == "DEN" | Away == "LAC" | Away == "LV", "AFC West",
                                              ifelse(Away == "PHI" | Away == "DAL" | Away == "NYG" | Away == "WAS", "NFC East",
                                                     ifelse(Away == "GB" | Away == "CHI" | Away == "DET" | Away == "MIN", "NFC North",
                                                            ifelse(Away == "TB" | Away == "NO" |Away == "CAR" | Away == "ATL", "NFC South",
                                                                   ifelse(Away == "LAR" | Away == "SEA" | Away == "SF" | Away == "ARI", "NFC West","NFC")))))))))

data_2022$Home_Div <-
  with(data_2022, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE", "AFC East",
                         ifelse(Home == "PIT" | Home == "CLE" |Home == "CIN" | Home == "BAL", "AFC North",
                                ifelse(Home == "HOU" | Home == "JAX" |Home == "IND" | Home == "TEN", "AFC South",
                                       ifelse(Home == "KC" | Home == "DEN" |Home == "LAC" | Home == "LV", "AFC West",
                                              ifelse(Home == "PHI" | Home == "DAL" | Home == "NYG" | Home == "WAS", "NFC East",
                                                     ifelse(Home == "GB" | Home == "CHI" |Home == "DET" | Home == "MIN", "NFC North",
                                                            ifelse(Home == "TB" | Home == "NO" |Home == "CAR" | Home == "ATL", "NFC South",
                                                                   ifelse(Home == "LAR" | Home == "SEA" | Home == "SF" | Home == "ARI", "NFC West","NFC")))))))))

data_2023$Home_Conf <-
  with(data_2023, ifelse(Home == "BUF" | Home == "NYJ" |  Home == "MIA" | Home == "NE" | Home == "PIT"|
                           Home == "BAL"|Home == "CLE"|Home == "CIN"|Home == "TEN"| Home == "HOU"| Home == "JAX"| 
                           Home == "IND"| Home == "KC"| Home == "LAC"|Home == "DEN"| Home == "LV", "AFC", "NFC" ))
data_2023$Away_Conf <-
  with(data_2023, ifelse(Away == "BUF" | Away == "NYJ" |Away == "MIA" | Away == "NE" | Away == "PIT"|
                           Away == "BAL"|Away == "CLE"|Away == "CIN"|Away == "TEN"| Away == "HOU"| Away == "JAX"| 
                           Away == "IND"| Away == "KC"| Away == "LAC"|Away == "DEN"| Away == "LV", "AFC", "NFC" ))

data_2023$Away_Div <-
  with(data_2023, ifelse(Away == "BUF" | Away == "NYJ" | Away == "MIA" | Away == "NE", "AFC East",
                         ifelse(Away == "PIT" | Away == "CLE" | Away == "CIN" | Away == "BAL", "AFC North",
                                ifelse(Away == "HOU" | Away == "JAX" | Away == "IND" | Away == "TEN", "AFC South",
                                       ifelse(Away == "KC" | Away == "DEN" | Away == "LAC" | Away == "LV", "AFC West",
                                              ifelse(Away == "PHI" | Away == "DAL" |  Away == "NYG" | Away == "WAS", "NFC East",
                                                     ifelse(Away == "GB" | Away == "CHI" |Away == "DET" | Away == "MIN", "NFC North",
                                                            ifelse(Away == "TB" | Away == "NO" | Away == "CAR" | Away == "ATL", "NFC South",
                                                                   ifelse(Away == "LAR" | Away == "SEA" |Away == "SF" | Away == "ARI", "NFC West","NFC")))))))))

data_2023$Home_Div <-
  with(data_2023, ifelse(Home == "BUF" | Home == "NYJ" | Home == "MIA" | Home == "NE", "AFC East",
                         ifelse(Home == "PIT" | Home == "CLE" |  Home == "CIN" | Home == "BAL", "AFC North",
                                ifelse(Home == "HOU" | Home == "JAX" | Home == "IND" | Home == "TEN", "AFC South",
                                       ifelse(Home == "KC" | Home == "DEN" |Home == "LAC" | Home == "LV", "AFC West",
                                              ifelse(Home == "PHI" | Home == "DAL" | Home == "NYG" | Home == "WAS", "NFC East",
                                                     ifelse(Home == "GB" | Home == "CHI" |Home == "DET" | Home == "MIN", "NFC North",
                                                            ifelse(Home == "TB" | Home == "NO" | Home == "CAR" | Home == "ATL", "NFC South",
                                                                   ifelse(Home == "LAR" | Home == "SEA" | Home == "SF" | Home == "ARI", "NFC West","NFC")))))))))

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

#international games
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

#International Games
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

#International Games
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