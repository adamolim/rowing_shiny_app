############################################################################## #
# Rowing
############################################################################## #

# Auteur: Michele Adamoli 14.03.2021

############################################################################## #
# 0. Préliminaires ####
############################################################################## #

# Palette de couleurs

color_club_official <- "#DA322B"
color_club <- c("#e8847f", "#e15a55", "#da322b", "#98231e", "#571411", "#150504", rep("black", 30))
color_club_light <- c("#f0adaa", "#f3c1bf", "#f7d6d4", "#fbeae9")
color_club_dark <- c("#821e19", "#6d1915", "#571411", "#2b0a08")

# Packages

library(tidyverse)
library(lubridate)
library(randomNames)


############################################################################## #
#
# 1. Nomenclatures ####
#
############################################################################## #


############################################################################## #
# 1.1 Noms des bateaux ####
############################################################################## #

d.BOAT_NAME.0 <- data.frame(BOAT_SIZE = c("1" ,"2", "4", "8"), 
                            NAME_1 = c("Poseidon", "Isis Noreia", "Presto", "Big Wig"), 
                            NAME_2 = c("Stella", "Eisvogel", "Ballerina bianca", "Power Achter"),
                            NAME_3 = c("Unser Einer", "Wallaby", "Wasser Laeufer", "Win or die"),
                            NAME_4 = c("Pole Star", "Ahoi Brause", "Schwanen Stuepfer", "Giga Watt"))

d.BOAT_NAME.1 <- d.BOAT_NAME.0 %>%
  pivot_longer(cols = -BOAT_SIZE, names_to = "TYPE_NAME", values_to = "BOAT_NAME") %>% 
  filter(!is.na(BOAT_NAME)) %>% 
  mutate(BOAT_NAME = as.factor(BOAT_NAME)) %>% 
  mutate(BOAT_TYPE = case_when(BOAT_SIZE == "8" ~ "Sweep",
                               BOAT_NAME == "Wasser Laeufer" ~ "Sweep",
                               BOAT_NAME == "Eisvogel" ~ "Sweep",
                               TRUE ~ "Scull"))

# Einer

BOAT_NAME.1x <- d.BOAT_NAME.1 %>%
  filter(BOAT_SIZE == "1") %>% 
  select(BOAT_NAME) %>% droplevels %>% pull

# Zweier

BOAT_NAME.2x <- d.BOAT_NAME.1 %>%
  filter(BOAT_SIZE == "2") %>% 
  select(BOAT_NAME) %>% droplevels %>% pull

# Vierer

BOAT_NAME.4x <- d.BOAT_NAME.1 %>%
  filter(BOAT_SIZE == "4") %>% 
  select(BOAT_NAME) %>% droplevels %>% pull

# Achter

BOAT_NAME.8x <- d.BOAT_NAME.1 %>%
  filter(BOAT_SIZE == "8") %>% 
  select(BOAT_NAME) %>% droplevels %>% pull



############################################################################## #
# 1.2 Noms des distances ####
############################################################################## #


# Noms des distances

d.DIST.NAMES <- data.frame(DIST_KM = c(8, 10, 12, 14, 16, 17, 18),
                           DIST_NAME = c("Kandelaber", "Ziel", "Praegel", "Teuftal", "Eiau", 
                                         "Ottos Place", "Werk"))

# Dataframe de base

d.CLUB.00 <- data.frame(ID = as.integer(),
                       DATE_TIME = as.numeric(),
                       SEASON = as.character(),
                       BOAT_SIZE = as.character(),
                       BOAT_NAME = as.character(),
                       DIST_KM = as.numeric(),
                       DIST_NAME = as.character(),
                       POSITION = as.character(),
                       SEX = as.character(),
                       SPORT = as.character())


############################################################################## #
# 1.3 Noms des positions dans le bateaux ####
############################################################################## #

# Personnes

d.POSITION <- enframe(list("1" = sample(LETTERS[1:9], 1), 
                         "2" = sample(LETTERS[1:9], 2),
                         "4" = sample(LETTERS[1:9], 4),
                         "8" = sample(LETTERS[1:9], 9))) %>% # avec le cox c'est neuf personnes
  rename("BOAT_SIZE" = "name") %>% 
  rename("POSITION" = "value")


############################################################################## #
# 1.4 Noms des personnes ####
############################################################################## #

# Nom de femmes pour le breitensport

d.PERS_NAME.F_B <- data.frame(NAME_1 = randomNames(9, gender = 1, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_2 = randomNames(9, gender = 1, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_3 = randomNames(9, gender = 1, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_4 = randomNames(9, gender = 1, ethnicity = 1, 
                                                   sample.with.replacement = TRUE),
                              SEX = "FEM", SPORT = "BREIT", POSITION = LETTERS[1:9])

# Nom de femmes pour regatta

d.PERS_NAME.F_R <- data.frame(NAME_1 = randomNames(9, gender = 1, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_2 = randomNames(9, gender = 1, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_3 = randomNames(9, gender = 1, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_4 = randomNames(9, gender = 1, ethnicity = 2, 
                                                   sample.with.replacement = TRUE),
                              SEX = "FEM", SPORT = "REGATTA",  POSITION = LETTERS[1:9])


# Nom d'hommes pour le breitensport

d.PERS_NAME.H_B <- data.frame(NAME_1 = randomNames(9, gender = 0, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_2 = randomNames(9, gender = 0, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_3 = randomNames(9, gender = 0, ethnicity = 1, 
                                                   sample.with.replacement = TRUE), 
                              NAME_4 = randomNames(9, gender = 0, ethnicity = 1, 
                                                   sample.with.replacement = TRUE),
                              SEX = "HOM", SPORT = "BREIT",  POSITION = LETTERS[1:9])

# Nom d'hommes pour regatta

d.PERS_NAME.H_R <- data.frame(NAME_1 = randomNames(9, gender = 0, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_2 = randomNames(9, gender = 0, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_3 = randomNames(9, gender = 0, ethnicity = 2, 
                                                   sample.with.replacement = TRUE), 
                              NAME_4 = randomNames(9, gender = 0, ethnicity = 2, 
                                                   sample.with.replacement = TRUE),
                              SEX = "HOM", SPORT = "REGATTA",  POSITION = LETTERS[1:9])

############################################################################## #
# 1.5 Noms des mois ####
############################################################################## #

# Noms des mois

d.MONTH_NAME <- data.frame(MONTH_NAME = month.name, MONTH = c(1:12))


############################################################################## #
# 1.6 Nombre de sorties et années  ####
############################################################################## #

# Nombre de sorties par groupe

n <- 300

# Première année

y_min <- 2008

# Dernière année

y_max <- 2020



############################################################################## #
#
# 2. Données ####
#
############################################################################## #

############################################################################## #
# 2.1 Fonction pour les trips ####
############################################################################## #

generate_data <- function(year, sex, sport){
  
  # Reproductibilité des données aléatoires
  
  set.seed(year*3)
  
  # Nombre de sorties dans le temps
  
  n <- n*((1+0.01*(1-rnorm(1, 0, 0.1)))^(year-y_min))
  
  # Facteur par groupe de sexe
  
  if(sex == "FEM"){
    n <- n*1.1*(1-rnorm(1, 0, 0.02))
  }
  
  # Facteur par groupe de sport
  
  if(sport == "BREIT"){
    n <- n*1.2*(1-rnorm(1, 0, 0.02))
  }
  
  # Année et mois
  
  assign("d.CLUB.01", 
         data.frame(YEAR = year,
                    MONTH = sample(size = n, c(1:12), replace = TRUE,
                                   prob = 1/1200*c(50, 60, 80, 90, 120, 130, 
                                                   150, 160, 140, 90, 80, 50))),
         envir = .GlobalEnv)
  
  # Saison et jours
  
  assign("d.CLUB.02", 
         d.CLUB.01 %>%
           mutate(MONTH = as.character(MONTH)) %>%
           mutate(SEASON = case_when(MONTH %in% as.character(1:2) ~ "Winter",
                                     MONTH %in% as.character(3:5) ~ "Spring",
                                     MONTH %in% as.character(6:8) ~ "Summer",
                                     MONTH %in% as.character(9:11) ~ "Autumn",
                                     MONTH %in% as.character(12) ~ "Winter")) %>% 
           mutate(MAX_DAY_MONTH = as.numeric(days_in_month(as.numeric(MONTH)))) %>%
           mutate(DAY = case_when(MAX_DAY_MONTH == 31 ~ sample(c(1:31), n, replace = TRUE),
                                  MAX_DAY_MONTH == 30 ~ sample(c(1:30), n, replace = TRUE),
                                  MAX_DAY_MONTH == 28 ~ sample(c(1:28), n, replace = TRUE))) %>% 
           mutate(MONTH = str_pad(MONTH, 2, side = "left", pad = "0")) %>% 
           mutate(DAY = str_pad(DAY, 2, side = "left", pad = "0")) %>% 
           mutate(DATE = paste(YEAR, MONTH, DAY, sep = ":")) %>% 
           select(DATE, SEASON) %>%
           mutate(SEASON = fct_relevel(as.factor(SEASON), 
                                       "Winter", "Spring", "Summer", "Autumn")),
         envir = .GlobalEnv)
  
  
  # Heures, minutes et secondes
  
  assign("d.CLUB.03", 
         d.CLUB.02 %>%
           mutate(HOUR = case_when(SEASON == "Winter" ~ sample(c(8:15), n, replace = TRUE),
                                   SEASON == "Spring" ~ sample(c(7:17), n, replace = TRUE),
                                   SEASON == "Summer" ~ sample(c(6:18), n, replace = TRUE),
                                   SEASON == "Autumn" ~ sample(c(8:16), n, replace = TRUE))) %>% 
           mutate(HOUR = str_pad(HOUR, 2, side = "left", pad = "0")) %>% 
           mutate(MINUTE = round(runif(n, min = 0, max = 59))) %>%
           mutate(MINUTE = str_pad(MINUTE, 2, side = "left", pad = "0")) %>% 
           mutate(SECOND = "00"),
         envir = .GlobalEnv)
  
  # Heures, minutes et secondes
  
  assign("d.CLUB.04", 
         d.CLUB.03 %>%
           mutate(TIME = paste(HOUR, MINUTE, SECOND, sep= ":")) %>%
           mutate(DATE_TIME = as_datetime(paste(DATE, " ", TIME))) %>%
           arrange(DATE_TIME) %>%
           mutate(ID = c(1:n)) %>% 
           select(ID, DATE_TIME, SEASON),
         envir = .GlobalEnv)
  
  # Distances parcourues
  
  assign("d.CLUB.05", 
         d.CLUB.04 %>%
           mutate(DIST_KM = sample(c("8", "10", "12", "14", "16", "17", "18"), size = n, replace = TRUE, 
                                   prob = c(0.05, 0.10, 0.25, 0.20, 0.10, 0.15, 0.15))) %>% 
           mutate(DIST_KM = as.numeric(DIST_KM)),
         envir = .GlobalEnv)
  
  # Unir et rendre compatible les type de colonne
  
  assign("d.CLUB.06", 
         left_join(d.CLUB.05, d.DIST.NAMES, by = "DIST_KM") %>% 
           mutate(DATE_TIME = as.double(DATE_TIME)) %>% 
           mutate(SEASON = as.character(SEASON)),
         envir = .GlobalEnv)
  
  # Taille des équipages
  
  assign("d.CLUB.07", 
         d.CLUB.06 %>%
           mutate(BOAT_SIZE = sample(c("1", "2", "4", "8"), size = n, replace = TRUE, 
                                     prob = c(0.35, 0.30, 0.25, 0.10))) %>% 
           mutate(TYPE_NAME = sample(c("NAME_1", "NAME_2", "NAME_3", "NAME_4"), size = n, replace = TRUE, 
                                     prob = c(0.30, 0.25, 0.25, 0.20))),
         envir = .GlobalEnv)
  
  
  # Unir avec les bateaux
  
  assign("d.CLUB.08", 
         left_join(d.CLUB.07, d.BOAT_NAME.1, by = c("BOAT_SIZE", "TYPE_NAME")) %>% 
           select(-TYPE_NAME),
         envir = .GlobalEnv)
  
  # Unir avec les positions dans le bateaux (A, B, C, D...)
  
  assign("d.CLUB.09", 
         full_join(d.CLUB.08, d.POSITION, by = "BOAT_SIZE") %>%
         unnest(cols = c(POSITION)))
  
  # Assigner le genre et le groupe
  
  assign("d.CLUB.10", 
         d.CLUB.09 %>%
         mutate(SEX = sex) %>% 
         mutate(SPORT = sport))
  
  # Attribuer au dataframe principal
  
  assign(x = "d.CLUB.00", 
         bind_rows(d.CLUB.00, d.CLUB.10),
         envir = .GlobalEnv)
  
}



############################################################################## #
# 2.2 Itération ####
############################################################################## #

# Variables de la loop

years <- c(y_min:y_max)
sexes <- c("FEM", "HOM")
sports <- c("BREIT", "REGATTA")

# Appliquer la lo loop

for (sex in sexes) {
  for (sport in sports) {
    for (year in years) {
      generate_data(year, sex, sport)
    }
  }
}


# Unir avec le nom des personnes

d.PERS_NAME <- bind_rows(d.PERS_NAME.F_B, d.PERS_NAME.H_B, d.PERS_NAME.F_R, d.PERS_NAME.H_R) %>% 
  pivot_longer(c("NAME_1", "NAME_2", "NAME_3", "NAME_4"), names_to = "NAME_TYPE", values_to = "NAME_PERS")


# Préparer l'union avec les observations des sorties et le nom des personnes

d.CLUB.10 <- d.CLUB.00 %>% 
  data.frame(NAME_TYPE = sample(size = nrow(d.CLUB.00), c("NAME_1", "NAME_2", "NAME_3", "NAME_4"), replace = TRUE,
                            prob = 1/100*c(60, 34, 5, 1)))

# Unir

d.CLUB.11 <- full_join(d.CLUB.10, d.PERS_NAME, by = c("SEX", "SPORT", "NAME_TYPE", "POSITION"))


############################################################################## #
# 2.3 Finaliser ####
############################################################################## #

# Avec personnes et jours de la semaine

d.CLUB.30 <- d.CLUB.11 %>%
  mutate(DATE_TIME = as_datetime(DATE_TIME)) %>% 
  mutate(YEAR = year(DATE_TIME)) %>% 
  mutate(MONTH = month(DATE_TIME)) %>% 
  mutate(WEEKDAY = as.factor(wday(DATE_TIME))) %>% 
  mutate(WEEKDAY_NAME = fct_recode(WEEKDAY, 
                                   "Monday" =  "1",
                                   "Tuesday" =  "2",
                                   "Wednesday" =  "3",
                                   "Thursday" =  "4",
                                   "Friday" =  "5",
                                   "Saturday" =  "6",
                                   "Sunday" =  "7")) %>% 
  mutate(SEX = as.factor(SEX)) %>% 
  mutate(SPORT = as.factor(SPORT))

# Sans personnes, que sorties

d.CLUB.40 <- d.CLUB.30 %>% 
  select(-c(SEX, SPORT, NAME_PERS, NAME_TYPE, POSITION)) %>% 
  distinct()


############################################################################## #
#
# 3. Analyses ####
#
############################################################################## #


############################################################################## #
# 3.1 Kilometres ####
############################################################################## #


############################################################################## #
# 3.1.1 Club ####
############################################################################## #

# Nombre sorties des personnes

d.ANA.01.1 <- d.CLUB.30 %>%
  count(YEAR) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP" = "n")

# Nombre de kilometres des personnes

d.ANA.01.2 <- d.CLUB.30 %>%
  group_by(YEAR) %>%
  summarise(DIST_KM = sum(DIST_KM)) %>%
  ungroup()

# Nombre de personnes qui ont ramé au moins une fois

d.ANA.01.3 <- d.CLUB.30 %>%
  select(NAME_PERS, YEAR) %>%
  distinct() %>% 
  group_by(YEAR) %>%
  count(YEAR) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_PERSON" = "n")

# Union

d.ANA.01.4 <- full_join(d.ANA.01.1, d.ANA.01.2, by = "YEAR")
d.ANA.01.5 <- full_join(d.ANA.01.3, d.ANA.01.4, by = "YEAR")

# Moyenne par sortie et par personnes

d.ANA.01.6 <- d.ANA.01.5 %>%
  mutate(AVG_KM_TRIP = round(DIST_KM / NUMBER_TRIP, digits = 2)) %>%
  mutate(AVG_KM_PERSON = round(DIST_KM / NUMBER_PERSON, digits = 2)) %>%
  gather(-YEAR, key = "DIMENSION", value = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "DIST_KM" ~ " km",
    DIMENSION == "NUMBER_TRIP" ~ " trips",
    DIMENSION == "NUMBER_PERSON" ~ " rowers",
                          DIMENSION == "AVG_KM_PERSON" ~ " km/rower",
                          DIMENSION == "AVG_KM_TRIP" ~ " km/trip"))

# # Données du graphique d'exemple
# 
# d.ANA.01.7 <- d.ANA.01.6 %>%
#   filter(DIMENSION == "DIST_KM")
# 
# # Plot
# 
# x.ANA.01 <- ggplot(d.ANA.01.7, mapping = aes(x = YEAR, y = Value)) +
#   
#   geom_point(size = 4.0, color = "red") +
#   geom_path(size = 1.0, color = "red") +
#   
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   
#   scale_x_continuous(breaks = seq(y_min, y_max, 1))
# 
# x.ANA.01


############################################################################## #
# 3.1.2 Groupes ####
############################################################################## #

# Nombre sorties des personnes

d.ANA.06.01 <- d.CLUB.30 %>%
  group_by(SPORT, SEX) %>% 
  count(YEAR) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP" = "n")

# Nombre de kilometres des personnes

d.ANA.06.02 <- d.CLUB.30 %>%
  group_by(YEAR, SPORT, SEX) %>%
  summarise(DIST_KM = sum(DIST_KM)) %>%
  ungroup()

# Nombre de personnes qui ont ramé au moins une fois

d.ANA.06.03 <- d.CLUB.30 %>%
  select(NAME_PERS, YEAR, SPORT, SEX) %>%
  distinct() %>% 
  group_by(YEAR, SPORT, SEX) %>%
  count(YEAR) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_PERSON" = "n")

# Unir

d.ANA.06.04 <- full_join(d.ANA.06.01, d.ANA.06.02)
d.ANA.06.05 <- full_join(d.ANA.06.03, d.ANA.06.04)

# Tidy

d.ANA.06.06 <- d.ANA.06.05 %>%
  pivot_longer(c("NUMBER_TRIP", "NUMBER_PERSON", "DIST_KM"), 
               names_to = "DIMENSION", values_to = "Value")

# Agrégation par sport

d.ANA.06.07 <- d.ANA.06.06 %>%
  group_by(YEAR, SEX, DIMENSION) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SPORT = "_T")

# Agrégation par sexe

d.ANA.06.08 <- d.ANA.06.06 %>%
  group_by(YEAR, SPORT, DIMENSION) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SEX = "_T")

# Agrégation par totale

d.ANA.06.09 <- d.ANA.06.06 %>%
  group_by(YEAR,  DIMENSION) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SEX = "_T") %>% 
  mutate(SPORT = "_T")

# Unir

d.ANA.06.10 <- bind_rows(d.ANA.06.06, d.ANA.06.07, d.ANA.06.08, d.ANA.06.09)

# Calculer les moyennes

d.ANA.06.11 <- d.ANA.06.10 %>%
  spread(key = DIMENSION, value = Value) %>% 
  mutate(AVG_KM_TRIP = round(DIST_KM / NUMBER_TRIP, digits = 2)) %>%
  mutate(AVG_KM_PERSON = round(DIST_KM / NUMBER_PERSON, digits = 2)) %>%
  pivot_longer(cols = -c(YEAR, SPORT, SEX), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "DIST_KM" ~ " km",
                          DIMENSION == "NUMBER_TRIP" ~ " trips",
                          DIMENSION == "NUMBER_PERSON" ~ " rowers",
                          DIMENSION == "AVG_KM_PERSON" ~ " km/rower",
                          DIMENSION == "AVG_KM_TRIP" ~ " km/trip"))%>% 
  #fct recode ici car problème à changer manuellement la légende avec plotly
  mutate(SPORT = as.factor(SPORT)) %>% 
  mutate(SPORT = fct_recode(SPORT, "Breitensport" = "BREIT", "Regatta Team" = "REGATTA")) %>% 
  #fct recode ici car problème à changer manuellement la légende avec plotly
  mutate(SEX = as.factor(SEX)) %>% 
  mutate(SEX = fct_recode(SEX, "male" = "HOM", "female" = "FEM"))


############################################################################## #
# 3.1.3.1 Rowers - yearly ####
############################################################################## #

# Somme pour les individus, par année, par groupe

d.ANA.07.01 <- d.CLUB.30 %>%
  group_by(YEAR, NAME_PERS, SEX, SPORT) %>%
  summarise(DIST_KM = sum(DIST_KM)) %>%
  ungroup() 

# Total par sex

d.ANA.07.02 <- d.ANA.07.01 %>%
  mutate(SEX = "_T")

# Total par sport

d.ANA.07.03 <- d.ANA.07.01 %>%
  mutate(SPORT = "_T")

# Total

d.ANA.07.04 <- d.ANA.07.01 %>%
  mutate(SEX = "_T") %>%
  mutate(SPORT = "_T")

# Unir

d.ANA.07.05 <- bind_rows(d.ANA.07.01, d.ANA.07.02, d.ANA.07.03, d.ANA.07.04)

# # Données du graphique d'exemple
# 
# d.ANA.07 <- d.ANA.07.05 %>%
#   filter(YEAR == "2009") %>%
#   filter(SEX == "FEM") %>% 
#   filter(SPORT == "REGATTA") %>%
#   group_by(YEAR, SEX, SPORT) %>%
#   mutate(RANK = min_rank(-DIST_KM)) %>% 
#   ungroup() %>% 
#   filter(RANK <= 10) %>% 
#   mutate(FIRST = case_when(RANK == 1 ~ TRUE,
#                            TRUE ~ FALSE))
# 
# # Plot
# 
# x.ANA.07 <- ggplot(d.ANA.07, aes(x = reorder(NAME_PERS, DIST_KM), y = DIST_KM,
#                                  fill = FIRST)) +
#   
#   geom_col() +
#   
#   coord_flip() +
# 
# theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# x.ANA.07



############################################################################## #
# 3.1.4.2 Rowers - cumul ####
############################################################################## #

# Données du graphique d'exemple

# Selectionner les personnes qui ont cumulé le plus de km

d.ANA.08.1 <- d.ANA.07.05 %>%
  filter(YEAR == y_max) %>% 
  group_by(NAME_PERS, SEX, SPORT) %>%
  summarise(DIST_KM = sum(DIST_KM)) %>%
  ungroup() %>%
  group_by(SEX, SPORT) %>%
  mutate(RANK = min_rank(-DIST_KM)) %>%
  ungroup() %>% 
  filter(RANK <= 3) %>% 
  select(NAME_PERS, SEX, SPORT) %>% 
  distinct %>% 
  mutate(CUMUL_CHAMPION = TRUE)
  
# # Préparer les données pour le graphique
# 
# d.ANA.08.2 <- left_join(d.ANA.08.1, d.ANA.07.05, by = c("NAME_PERS", "SEX", "SPORT")) %>%
#   filter(YEAR >= 2012) %>% 
#   group_by(NAME_PERS, SEX, SPORT) %>%
#   arrange(YEAR) %>% 
#   mutate(CUMUL_DIST_KM = cumsum(DIST_KM)) %>%
#   ungroup() %>%
#   filter(SEX == "FEM") %>% 
#   filter(SPORT == "_T")
# 
# # Plot
# 
# x.ANA.08 <- ggplot(d.ANA.08.2, aes(x = YEAR, y = CUMUL_DIST_KM, color = reorder(NAME_PERS, -CUMUL_DIST_KM))) +
#   
#   geom_point() +
# 
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
#   x.ANA.08


############################################################################## #
# 3.2 Fréquentation du club  ####
############################################################################## #

############################################################################## #
# 3.2.1 Par mois  ####
############################################################################## #

# Nombre de personnes par mois (moyenne)

d.ANA.02.3 <- d.CLUB.30 %>% 
  count(MONTH) %>% 
  mutate(n = round(n /(y_max - y_min), digits =1)) %>%
  rename("Average" = "n")

# Nombre personnes par mois

d.ANA.02.4 <- d.CLUB.30 %>%
  group_by(YEAR) %>% 
  count(MONTH) %>% 
  rename("Spot" = "n")
  
# Unir 

d.ANA.02.5 <- full_join(d.ANA.02.3, d.ANA.02.4, by = "MONTH")

# Tidy

d.ANA.02.6 <- d.ANA.02.5 %>% 
  pivot_longer(cols = c(Average, Spot), names_to = "MEASURE", values_to = "Value") %>% 
  mutate(MEASURE = as.factor(MEASURE))

# Donner le nom aux mois

d.ANA.02.7 <- left_join(d.ANA.02.6, d.MONTH_NAME) %>% 
  mutate(MONTH_NAME = fct_inorder(as.factor(MONTH_NAME))) %>% 
  spread(key = MEASURE, value = Value)

# 
# # Données du graphique d'exemple
# 
# d.ANA.02.7.1 <- d.ANA.02.7 %>%
#   filter(YEAR == "2009") 
# 
# # Plot
# 
# x.ANA.02 <- ggplot(d.ANA.02.7.1) +
#   
#   geom_col(mapping = aes(x = MONTH_NAME, y = Average),
#            fill = color_club_light[1]) +
#   geom_line(mapping = aes(x = MONTH, y = Spot),
#             size = 1.0,
#             color = "red") +
#   geom_point(mapping = aes(x = MONTH_NAME, y = Spot),
#              size = 4.0,
#              color = "red") +
# 
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# x.ANA.02


############################################################################## #
# 3.2.2 Jours de la semaine  ####
############################################################################## #

# Nombre de personnes par jours de la semaine (moyenne)

d.ANA.05.1 <- d.CLUB.30 %>%
  group_by(SEASON) %>% 
  count(WEEKDAY, WEEKDAY_NAME) %>%
  ungroup() %>% 
  mutate(n = n /(y_max - y_min)) %>%
  rename("Average" = "n") %>% 
  mutate(Average = round(Average / 13, digits = 1)) # Rebasé par une semaine

# Nombre personnes par semaine

d.ANA.05.2 <- d.CLUB.30 %>%
  group_by(YEAR, SEASON) %>% 
  count(WEEKDAY, WEEKDAY_NAME) %>% 
  rename("Spot" = "n") %>% 
  mutate(Spot = round(Spot / 13, digits = 1)) # Rebasé par une semaine

# Unir 

d.ANA.05.3 <- full_join(d.ANA.05.1, d.ANA.05.2, 
                        by = c("WEEKDAY", "WEEKDAY_NAME", "SEASON"))

############################################################################## #
# 3.2.3 Hours du jours  ####
############################################################################## #

# Variable des heurs

d.ANA.10.1 <- d.CLUB.30 %>%
  select(DATE_TIME, NAME_PERS, YEAR, SEASON, WEEKDAY_NAME) %>% 
  mutate(HOURS = hour(DATE_TIME))

# Nombre de personnes par jours de la semaine (moyenne)

d.ANA.10.2 <- d.ANA.10.1 %>%
  group_by(SEASON, WEEKDAY_NAME) %>% 
  count(HOURS) %>%
  ungroup() %>% 
  rename("Average" = "n")

# Nombre personnes par semaine

d.ANA.10.3 <- d.ANA.10.1 %>%
  group_by(YEAR, SEASON, WEEKDAY_NAME) %>% 
  count(HOURS) %>% 
  rename("Spot" = "n")

# Unir 

d.ANA.10.4 <- full_join(d.ANA.10.2, d.ANA.10.3, by = c("SEASON", "WEEKDAY_NAME", "HOURS")) %>% 
  pivot_longer(cols = c("Average", "Spot"), names_to = "MEASURE", values_to = "Value")

# Calculer le total pour transformer en pourcentage

d.ANA.10.5 <- d.ANA.10.4 %>%
  group_by(MEASURE, YEAR, SEASON, WEEKDAY_NAME) %>% 
  summarise(Value_tot = sum(Value, na.rm = TRUE)) %>% 
  ungroup()

# Unir 

d.ANA.10.6 <- left_join(d.ANA.10.4, d.ANA.10.5, by = c("SEASON", "WEEKDAY_NAME", "YEAR", "MEASURE"))

# Calculer le pourcentage

d.ANA.10.7 <- d.ANA.10.6 %>%
  mutate(Value_pc = round(Value / Value_tot *100, digits =1))
  
# Untidy 

d.ANA.10.8 <- d.ANA.10.7 %>%
  select(-c(Value, Value_tot)) %>% 
  pivot_wider(names_from = MEASURE, values_from = Value_pc)

# # Selection pour le plot d'exemple
# 
# d.ANA.10.9 <- d.ANA.10.8 %>%
#   filter(YEAR == 2020) %>% 
#   filter(SEASON == "Spring") %>% 
#   filter(WEEKDAY_NAME == "Monday") %>% 
#   mutate(MEASURE = "Sport")
# 
# # Plot
# 
# x.ANA.10 <- ggplot(d.ANA.10.9) +
#   
#   geom_col(mapping = aes(x = HOURS, y = Average),
#            fill = color_club_light[1]) +
#   geom_line(mapping = aes(x = HOURS, y = Spot),
#             size = 1.0,
#             color = "red") +
#   geom_point(mapping = aes(x = HOURS, y = Spot),
#              size = 4.0,
#              color = "red") +
# 
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# x.ANA.10




############################################################################## #
# 3.3 Disciplines ####
############################################################################## #

############################################################################## #
# 3.3.1 Sweep or scull ####
############################################################################## #

# Nombre sorties des personnes

d.ANA.08.1 <- d.CLUB.30 %>%
  count(YEAR, BOAT_TYPE) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP_PERS" = "n")

# Nombre de kilomètres des personnes

d.ANA.08.2 <- d.CLUB.30 %>% # par personne
  group_by(YEAR, BOAT_TYPE) %>% 
  summarise(DIST_KM_PERS = sum(DIST_KM)) %>% 
  ungroup()

# Nombre sorties des bateaux

d.ANA.08.3 <- d.CLUB.40 %>%
  count(YEAR, BOAT_TYPE) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP_BOAT" = "n")

# Nombre de kilomètres des bateaux

d.ANA.08.4 <- d.CLUB.40 %>% # par personne
  group_by(YEAR, BOAT_TYPE) %>% 
  summarise(DIST_KM_BOAT = sum(DIST_KM)) %>% 
  ungroup()

# Union

d.ANA.08.5 <- full_join(d.ANA.08.1, d.ANA.08.2, by = c("YEAR", "BOAT_TYPE"))
d.ANA.08.6 <- full_join(d.ANA.08.3, d.ANA.08.4, by = c("YEAR", "BOAT_TYPE"))
d.ANA.08.7 <- full_join(d.ANA.08.5, d.ANA.08.6, by = c("YEAR", "BOAT_TYPE"))

# Tidy and unit

d.ANA.08.8 <- d.ANA.08.7 %>% 
  pivot_longer(cols = c(DIST_KM_BOAT, NUMBER_TRIP_BOAT, DIST_KM_PERS, NUMBER_TRIP_PERS), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "NUMBER_TRIP_PERS" ~ " persons' trips",
                          DIMENSION == "DIST_KM_PERS" ~ " persons' km",
                          DIMENSION == "NUMBER_TRIP_BOAT" ~ " boats' trips",
                          DIMENSION == "DIST_KM_BOAT" ~ " persons' km"))


############################################################################## #
# 3.3.2 Boats category ####
############################################################################## #

# Nombre sorties des personnes

d.ANA.09.1 <- d.CLUB.30 %>%
  count(YEAR, BOAT_SIZE) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP_PERS" = "n")

# Nombre de kilomètres des personnes

d.ANA.09.2 <- d.CLUB.30 %>% # par personne
  group_by(YEAR, BOAT_SIZE) %>% 
  summarise(DIST_KM_PERS = sum(DIST_KM)) %>% 
  ungroup()

# Nombre sorties des bateaux

d.ANA.09.3 <- d.CLUB.40 %>%
  count(YEAR, BOAT_SIZE) %>%
  arrange(YEAR) %>% 
  rename("NUMBER_TRIP_BOAT" = "n")

# Nombre de kilomètres des bateaux

d.ANA.09.4 <- d.CLUB.40 %>% # par personne
  group_by(YEAR, BOAT_SIZE) %>% 
  summarise(DIST_KM_BOAT = sum(DIST_KM)) %>% 
  ungroup()

# Union

d.ANA.09.5 <- full_join(d.ANA.09.1, d.ANA.09.2, by = c("YEAR", "BOAT_SIZE"))
d.ANA.09.6 <- full_join(d.ANA.09.3, d.ANA.09.4, by = c("YEAR", "BOAT_SIZE"))
d.ANA.09.7 <- full_join(d.ANA.09.5, d.ANA.09.6, by = c("YEAR", "BOAT_SIZE"))

# Tidy and unit

d.ANA.09.8 <- d.ANA.09.7 %>% 
  pivot_longer(cols = c(DIST_KM_BOAT, NUMBER_TRIP_BOAT, DIST_KM_PERS, NUMBER_TRIP_PERS), names_to = "DIMENSION", values_to = "Value") %>% 
  mutate(UNIT = case_when(DIMENSION == "NUMBER_TRIP_PERS" ~ " persons' trips",
                          DIMENSION == "DIST_KM_PERS" ~ " persons' km",
                          DIMENSION == "NUMBER_TRIP_BOAT" ~ " boats' trips",
                          DIMENSION == "DIST_KM_BOAT" ~ " persons' km"))



############################################################################## #
# 3.3.2 Destinations ####
############################################################################## #

# Par es tailles d'équipage

d.ANA.11.1 <- d.CLUB.40 %>%
  group_by(YEAR, BOAT_SIZE) %>% 
  count(DIST_NAME, DIST_KM) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  rename("Spot_crew" = "n")

# Tous les tailles d'équipage

d.ANA.11.2 <- d.CLUB.40 %>%
  group_by(YEAR) %>% 
  count(DIST_NAME, DIST_KM) %>% 
  ungroup() %>% 
  mutate(n = n / 4) %>% # Nombre de catégories de 
  arrange(desc(n)) %>% 
  rename("Average_crew" = "n")

# Unir

d.ANA.11.3 <- left_join(d.ANA.11.1, d.ANA.11.2, by = c("YEAR", "DIST_NAME", "DIST_KM"))

# Tidy 

d.ANA.11.4 <- d.ANA.11.3 %>%
  arrange(YEAR, DIST_KM) %>% 
  mutate(DIST_NAME = fct_inorder(as.factor(DIST_NAME))) %>% 
  pivot_longer(cols = c("Average_crew", "Spot_crew"), names_to = "MEASURE", values_to = "Value")
  

# Calculer le total pour transformer en pourcentage

d.ANA.11.5 <- d.ANA.11.4 %>%
  group_by(YEAR, BOAT_SIZE, MEASURE) %>% 
  summarise(Value_tot = sum(Value, na.rm = TRUE)) %>% 
  ungroup()

# Unir 

d.ANA.11.6 <- left_join(d.ANA.11.4, d.ANA.11.5, by = c("YEAR", "BOAT_SIZE", "MEASURE"))

# Calculer le pourcentage

d.ANA.11.7 <- d.ANA.11.6 %>%
  mutate(Value_pc = round(Value / Value_tot *100, digits =1))

# Untidy 

d.ANA.11.8 <- d.ANA.11.7 %>%
  select(-c(Value, Value_tot)) %>% 
  pivot_wider(names_from = MEASURE, values_from = Value_pc) %>% 
  mutate(DIST_NAME_2 = paste0(DIST_NAME, " (", DIST_KM, " km)")) %>% 
  arrange(YEAR, DIST_KM) %>% 
  mutate(DIST_NAME = fct_inorder(as.factor(DIST_NAME_2))) 



# # Données du graphique d'exemple
# 
# d.ANA.11.4 <- d.ANA.11.3 %>%
#   filter(YEAR == 2010) %>%
#   filter(BOAT_SIZE == 1)

# # Plot
# 
# x.ANA.11 <- ggplot(d.ANA.11.4) +
# 
#   geom_col(aes(x = DIST_NAME, y = Average_crew),
#            fill = color_club_light[1]) +
# 
#   geom_line(aes(x = DIST_KM, y = Spot_crew),
#             size = 1.0,
#             color = "red") +
# 
#   geom_point(aes(x = DIST_NAME, y = Spot_crew),
#              size = 4.0,
#              color = "red") +
# 
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# x.ANA.11




############################################################################## #
# 3.4 Bateaux ####
############################################################################## #

############################################################################## #
# 3.4.1 Skiff 1x ####
############################################################################## #

# Nombre de kilomètres

d.ANA.03.3 <- d.CLUB.40 %>%
  group_by(BOAT_NAME, YEAR) %>% 
  summarise(DIST_KM = sum(DIST_KM)) %>% 
  ungroup()


# # Données du graphique d'exemple
# 
# d.ANA.03.3.1 <- d.ANA.03.3 %>%
#   filter(BOAT_NAME %in% "Ballerina bianca")
# 
# # Plot
# 
# x.ANA.03 <- ggplot(d.ANA.03.3.1, mapping = aes(x = YEAR, y = DIST_KM)) +
#   
#   geom_point(size = 4.0, color = "red") +
#   geom_path(size = 1.0, color = "red") +
#   
#   theme_classic() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   
#   scale_x_continuous(breaks = seq(y_min, y_max, 1))
# 
# x.ANA.03





