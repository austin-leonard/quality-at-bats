# ----------------------- QUALITY AT BAT BREAKDOWN ---------------------------------

# An investigation into the merits of quality at bats. 
# Author: Austin Leonard (@austin-leonard on github)


# ================ Gather data from 2015 - present ================
library(tidyverse)
library(baseballr)

# Function to scrape data (courtesy of Bill Petti: https://billpetti.github.io/2020-05-26-build-statcast-database-rstats-version-2.0/)
annual_statcast_query <- function(season) {
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  safe_savant <- safely(scrape_statcast_savant)
  
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
  
}

# Store each season as a dataframe
data2015 <- annual_statcast_query(2015)
data2016 <- annual_statcast_query(2016)
data2017 <- annual_statcast_query(2017)
data2018 <- annual_statcast_query(2018)
data2019 <- annual_statcast_query(2019)
# data2020 <- annual_statcast_query(2020) 

# 2020 data is finnicky for whatever reason, but it's small enough that it can be done manually
plate_appearance_2020 <- read_csv("2020 Last Pitches.csv")
plate_appearance_2020_2 <- read_csv("2020 Last Pitches First 2 Weeks.csv")

plate_app_2020 <- rbind(plate_appearance_2020, plate_appearance_2020_2)

# Combine all statcast data (excluding 2020) and isolate final pitch of plate appearances
# Also add QAB filter
statcast_data <- rbind(data2015, data2016, data2017, data2018, data2019)

# Change launch speed
statcast_data$launch_speed <- ifelse(is.na(statcast_data$launch_speed), 0, as.numeric(statcast_data$launch_speed))

statcast_plate_appearances <- statcast_data %>% 
  filter(events != "null") %>% 
  mutate(QAB = ifelse(launch_speed >= 95 | events == "walk" | events == "home_run" | events == "triple" | events == "double" |
                        events == "single" | pitch_number >= 6 | events == "sac_fly" | events == "hit_by_pitch", 1, 0))


# Do the same for 2020 data separately
plate_app_2020$launch_speed <- ifelse(is.na(plate_app_2020$launch_speed), 0, as.numeric(plate_app_2020$launch_speed))

plate_app_2020 <- plate_app_2020 %>% 
  mutate(QAB = ifelse(launch_speed >= 95 | events == "walk" | events == "home_run" | events == "triple" | events == "double" |
                        events == "single" | pitch_number >= 6 | events == "sac_fly" | events == "hit_by_pitch", 1, 0))
  # select(game_date, player_name, batter, launch_speed, events, pitch_number, QAB)


# ================== COMBINE AND WRITE EVERYTHING TO A GIANT CSV TO MAKE FUTURE DATA IMPORTS EASIER ===============

# Combine 
statcast_plate_appearances <- subset(statcast_plate_appearances, select= -c(barrel))

statcast_plate_appearances_full <- rbind(statcast_plate_appearances, plate_app_2020)

# Write to CSV
write.csv(statcast_plate_appearances_full, "Statcast Plate Appearance Data.csv")

# Load
all_statcast_final_pitches <- read_csv("Statcast Plate Appearance Data.csv")

all_statcast_final_pitches <- all_statcast_final_pitches %>% 
  mutate(long_ab = ifelse(pitch_number >= 6, 1, 0))

# ===============================================================================================================

# ========== Fun stats! =============

# QABs by year

# 2015
qab_2015 <- all_statcast_final_pitches %>% 
  filter(game_year==2015)

qab_players_2015 <- aggregate(qab_2015[,94], list(qab_2015$player_name), sum)
total_abs_2015 <- c(table(qab_2015$player_name))

# Add in long_ab counter and percentage
long_abs_2015 <- aggregate(qab_2015[,95], list(qab_2015$player_name), sum)

batter_ids <- all_statcast_final_pitches %>% 
  select(player_name, batter)

batter_ids <- unique(batter_ids)

qab_players_2015$plate_appearances <- total_abs_2015
long_abs_2015$plate_appearances <- total_abs_2015
qab_players_2015$qab_percent <- round(qab_players_2015$QAB/qab_players_2015$plate_appearances, 2)
long_abs_2015$long_ab_percent <- round(long_abs_2015$long_ab/long_abs_2015$plate_appearances, 2)

names(qab_players_2015)[1] <- "player_name"
names(long_abs_2015)[1] <- "player_name"

qab_players_2015 <- merge(qab_players_2015, batter_ids)

qab_qualified_2015 <- qab_players_2015 %>% 
  filter(plate_appearances > 300)

season2015 <- c(rep(2015, nrow(qab_qualified_2015)))

qab_qualified_2015$season <- season2015

qab_qualified_2015 <- merge(qab_qualified_2015, long_abs_2015)

# 2015's top 5:
# 1. Miguel Cabrera 66%
# 2. Bryce Harper 65%
# 3. Mike Trout 64%
# 4. Joey Votto 64%
# 5. Brandon Belt 63% 

# 2015's bottom 5: 
# 1. Omar Infante 37% 
# 2. Billy Hamilton 42%
# 3. Yolmer Sanchez 43% 
# 4. Alexi Amarista 44%
# 5. Alcides Escobar 44%

# 2016
qab_2016 <- all_statcast_final_pitches %>% 
  filter(game_year==2016)

qab_players_2016 <- aggregate(qab_2016[,94], list(qab_2016$player_name), sum)
total_abs_2016 <- c(table(qab_2016$player_name))

long_abs_2016 <- aggregate(qab_2016[,95], list(qab_2016$player_name), sum)

qab_players_2016$plate_appearances <- total_abs_2016
long_abs_2016$plate_appearances <- total_abs_2016
qab_players_2016$qab_percent <- round(qab_players_2016$QAB/qab_players_2016$plate_appearances, 2)
long_abs_2016$long_ab_percent <- round(long_abs_2016$long_ab/long_abs_2016$plate_appearances, 2)

names(qab_players_2016)[1] <- "player_name"
names(long_abs_2016)[1] <- "player_name"

qab_players_2016 <- merge(qab_players_2016, batter_ids)

qab_qualified_2016 <- qab_players_2016 %>% 
  filter(plate_appearances > 300)

season2016 <- c(rep(2016, nrow(qab_qualified_2016)))

qab_qualified_2016$season <- season2016
qab_qualified_2016 <- merge(qab_qualified_2016, long_abs_2016)

# 2016's top 5: 
# 1. DJ Lemahieu 68%
# 2. Josh Donaldson 66%
# 3. Mike Trout 66%
# 4. Joey Votto 66%
# 5. Jayson Worth 64%

# 2016's Bottom 5:
# 1. Billy Burns 39%
# 2. Ben Revere 39%
# 3. Alcides Escobar 44% 
# 4. Johnny Giavotella 44%
# 5. Nick Ahmed 45%


# 2017
qab_2017 <- all_statcast_final_pitches %>% 
  filter(game_year == 2017)

qab_players_2017 <- aggregate(qab_2017[,94], list(qab_2017$player_name), sum)
total_abs_2017 <- c(table(qab_2017$player_name))

long_abs_2017 <- aggregate(qab_2017[,95], list(qab_2017$player_name), sum)

qab_players_2017$plate_appearances <- total_abs_2017
long_abs_2017$plate_appearances <- total_abs_2017
qab_players_2017$qab_percent <- round(qab_players_2017$QAB/qab_players_2017$plate_appearances, 2)
long_abs_2017$long_ab_percent <- round(long_abs_2017$long_ab/long_abs_2017$plate_appearances, 2)

names(qab_players_2017)[1] <- "player_name"
names(long_abs_2017)[1] <- "player_name"

qab_players_2017 <- merge(qab_players_2017, batter_ids)

qab_qualified_2017 <- qab_players_2017 %>% 
  filter(plate_appearances > 300)

season2017 <- c(rep(2017, nrow(qab_qualified_2017)))

qab_qualified_2017$season <- season2017
qab_qualified_2017 <- merge(qab_qualified_2017, long_abs_2017)

# 2017's top 5: 
# 1. Joe Mauer (!!!) 67% 
# 2. Edwin Encarnacion 66%
# 3. Aaron Judge 66%
# 4. Anthony Rendon 65%
# 5. Paul Goldschmidt 64%

# 2017's bottom 5:
# 1. Darwin Barney 42%
# 2. Adam Engel 42%
# 3. Ronald Torreyes 42%
# 4. Erick Aybar 43%
# 5. Alcides Escobar 43%


# 2018 
qab_2018 <- all_statcast_final_pitches %>% 
  filter(game_year == 2018)

qab_players_2018 <- aggregate(qab_2018[,94], list(qab_2018$player_name), sum)
total_abs_2018 <- c(table(qab_2018$player_name))

long_abs_2018 <- aggregate(qab_2018[,95], list(qab_2018$player_name), sum)

qab_players_2018$plate_appearances <- total_abs_2018
long_abs_2018$plate_appearances <- total_abs_2018
qab_players_2018$qab_percent <- round(qab_players_2018$QAB/qab_players_2018$plate_appearances, 2)
long_abs_2018$long_ab_percent <- round(long_abs_2018$long_ab/long_abs_2018$plate_appearances, 2)

names(qab_players_2018)[1] <- "player_name"
names(long_abs_2018)[1] <- "player_name"

qab_players_2018 <- merge(qab_players_2018, batter_ids)

qab_qualified_2018 <- qab_players_2018 %>% 
  filter(plate_appearances > 300)

season2018 <- c(rep(2018, nrow(qab_qualified_2018)))

qab_qualified_2018$season <- season2018

qab_qualified_2018 <- merge(qab_qualified_2018, long_abs_2018)


# 2018's Top 5: 
# 1. Mookie Betts 68%
# 2. Kendrys Morales 67% 
# 3. Aaron Hicks 66%
# 4. Aaron Judge 66%
# 5. Mike Trout 66%

# 2018's Bottom 5:
# 1. Billy Hamilton 43%
# 2. Dee Strange-Gordon 43%
# 3. Alen Hanson 45%
# 4. Martin Maldonado 45%
# 5. Orlando Arcia 46% 


# 2019
qab_2019 <- all_statcast_final_pitches %>% 
  filter(game_year == 2019)

qab_players_2019 <- aggregate(qab_2019[,94], list(qab_2019$player_name), sum)
total_abs_2019 <- c(table(qab_2019$player_name))

long_abs_2019 <- aggregate(qab_2019[,95], list(qab_2019$player_name), sum)

qab_players_2019$plate_appearances <- total_abs_2019
long_abs_2019$plate_appearances <- total_abs_2019
qab_players_2019$qab_percent <- round(qab_players_2019$QAB/qab_players_2019$plate_appearances, 2)
long_abs_2019$long_ab_percent <- round(long_abs_2019$long_ab/long_abs_2019$plate_appearances, 2)

names(qab_players_2019)[1] <- "player_name"
names(long_abs_2019)[1] <- "player_name"

qab_players_2019 <- merge(qab_players_2019, batter_ids)

qab_qualified_2019 <- qab_players_2019 %>% 
  filter(plate_appearances > 300)

season2019 <- c(rep(2019, nrow(qab_qualified_2019)))

qab_qualified_2019$season <- season2019
qab_qualified_2019 <- merge(qab_qualified_2019, long_abs_2019)

# 2019's Top 5:
# 1. Juan Soto 67%
# 2. Mike Trout 67%
# 3. Mookie Betts 66%
# 4. Anthony Rendon 66% 
# 5. cody Bellinger 65%

# 2019's Bottom 5:
# 1. John Hicks 42%
# 2. Austin Hedges 43%
# 3. Gerardo Parra 45%
# 4. Billy Hamilton 46%
# 5. Richie Martin 46%

# 2020
qab_2020 <- all_statcast_final_pitches %>% 
  filter(game_year == 2020)

qab_players_2020 <- aggregate(qab_2020[,94], list(qab_2020$player_name), sum)
total_abs_2020 <- c(table(qab_2020$player_name))

long_abs_2020 <- aggregate(qab_2020[,95], list(qab_2020$player_name), sum)

qab_players_2020$plate_appearances <- total_abs_2020
long_abs_2020$plate_appearances <- total_abs_2020
qab_players_2020$qab_percent <- round(qab_players_2020$QAB/qab_players_2020$plate_appearances, 2)
long_abs_2020$long_ab_percent <- round(long_abs_2020$long_ab/long_abs_2020$plate_appearances, 2)

names(qab_players_2020)[1] <- "player_name"
names(long_abs_2020)[1] <- "player_name"

qab_players_2020 <- merge(qab_players_2020, batter_ids)

qab_qualified_2020 <- qab_players_2020 %>%
  filter(plate_appearances > 100)

season2020 <- c(rep(2020, nrow(qab_qualified_2020)))

qab_qualified_2020$season <- season2020
qab_qualified_2020 <- merge(qab_qualified_2020, long_abs_2020)

# 2020's Top 5: 
# 1. Josh Donaldson 74%
# 2. Freddie Freeman 71% 
# 3. Tommy Pham 69%
# 4. Ronald Acuna Jr 68%
# 5. Bryce Harper 68%

# 2020's Bottom 5: 
# 1. Ender Inciarte 42%
# 2. Jose Peraza 42%
# 3. Shed Long Jr 43%
# 4. Tony Wolters 44%
# 5. Kris Bryant 47%


# NEXT: Collect offensive Stats (wOBA, OPS, hard hit rate, K%, BB%, etc), attach to dataframes, merge into big dataframe,
# observe results

# 2015 Stats
stats2015 <- read_csv("stats2015.csv")
names(stats2015)[3] <- "batter"
names(stats2015)[4] <- "season"

final2015 <- merge(qab_qualified_2015, stats2015)

# 2016 Stats
stats2016 <- read_csv("stats2016.csv")
names(stats2016)[3] <- "batter"
names(stats2016)[4] <- "season"

final2016 <- merge(qab_qualified_2016, stats2016)

# 2017 Stats
stats2017 <- read_csv("stats2017.csv")
names(stats2017)[3] <- "batter"
names(stats2017)[4] <- "season"

final2017 <- merge(qab_qualified_2017, stats2017)

# 2018 Stats
stats2018 <- read_csv("stats2018.csv")
names(stats2018)[3] <- "batter"
names(stats2018)[4] <- "season"

final2018 <- merge(qab_qualified_2018, stats2018)

# 2019 Stats
stats2019 <- read_csv("stats2019.csv")
names(stats2019)[3] <- "batter"
names(stats2019)[4] <- "season"

final2019 <- merge(qab_qualified_2019, stats2019)

# 2020 Stats 
stats2020 <- read_csv("stats2020.csv")
names(stats2020)[3] <- "batter"
names(stats2020)[4] <- "season"

final2020 <- merge(qab_qualified_2020, stats2020)

# Merge all five years...
yearly_qabs_and_stats <- rbind(final2015, final2016, final2017, final2018, final2019, final2020)

# Write to CSV so I don't have to assemble all the data again
write.csv(yearly_qabs_and_stats, "Yearly QABs and Stats.csv")

# Load
qabs_and_stats <- read_csv("Yearly QABs and Stats.csv")


#========================= LONG AB STATS ========================

# Singles in Long ABs
19759/201164 # 9.82 %

# Singles in Short ABs
125728/nrow(allYearsShortAbs) # 15.67 %

# Doubles in Long ABs
6411/201164 # 3.2%

# Doubles in Short ABs
38593/nrow(allYearsShortAbs) # 4.81 %

# Triples in Long ABs
663/201164 # 0.33 %

# Triples in Short ABs
3853/nrow(allYearsShortAbs) # 0.48 %

# Home Runs in Long ABs
4532/nrow(allYearsLongAbs) # 2.25 %

# Home Runs in Short ABs 
27180/nrow(allYearsShortAbs) # 3.39%

# Walks in Long ABs
42220/nrow(allYearsLongAbs) # 20.99%

# Walks in Short ABs
36742/nrow(allYearsShortAbs) # 4.58%

# Strikeouts in Long ABs
(62331+646)/nrow(allYearsLongAbs) # 31.31 %

# Strikeouts in Short ABs
(156273 + 163)/nrow(allYearsShortAbs) # 19.5 %


# Batting Average in Long ABs
(6411 + 4532 + 19759 + 663)/(nrow(allYearsLongAbs) - (784 + 31 + 12 + 42220 + 951 + 14 + 34 + 3 + 3 + 9 + 2 + 1 + 1)) # 0.200

# Batting Average in Short ABs
(125728 + 38593 + 3853 + 27180)/(nrow(allYearsShortAbs) - (89 + 1095 + 75 + 40 + 2 + 6 + 8911 + 1920 + 193 + 56 + 38 + 9 + 7 + 7 + 6 + 1 + 2 + 4909 + 7 + 5597 + 79 + 6 + 1 + 1)) # 0.278

# 0.251

# "Good Outcome Percentage" in Long ABs
(6411 + 951 + 4532 +31 + 784 + 19759 + 663 + 2 + 42220)/(nrow(allYearsLongAbs) - (14 + 34 + 3 + 1 + 9 + 2 + 1 + 1)) # 37.47%

# "Good Outcome Percentage" in Short ABs
(125728 + 38593 + 3853 + 27180 + 8911 + 4909 + 5597 + 36742)/(nrow(allYearsShortAbs) - (89 + 1095 + 75 + 40 + 2 + 6 + 1920 + 193 + 56 + 38 + 9 + 7 + 7 + 6 + 1 + 2 + 6 + 1 + 1))

# 31.49 %

# Interesting -- Fewer hits and more Ks in long ABs, but far more walks. While BA is sig. lower in long ABs, Good outcomes are
# noticeably more common because of the amount of walks drawn.  

# SLG% in Long ABs
((2*6411) + (4*4532) + 19759 + (3*663))/(nrow(allYearsLongAbs) - (784 + 31 + 12 + 42220 + 951 + 14 + 34 + 3 + 3 + 9 + 2 + 1 + 1)) # 0.200
# 0.335

# SLG% in Short ABs
(125728 + (2*38593) + (3*3853) + (4*27180))/(nrow(allYearsShortAbs) - (89 + 1095 + 75 + 40 + 2 + 6 + 8911 + 1920 + 193 + 56 + 38 + 9 + 7 + 7 + 6 + 1 + 2 + 4909 + 7 + 5597 + 79 + 6 + 1 + 1)) # 0.278
# 0.415

# OBP in Long ABs
(6411 + 951 + 4532 + 19759 + 663 + 42220)/(nrow(allYearsLongAbs) - (14 + 34 + 3 + 1 + 9 + 2 + 1 + 1)) # 0.370

# OBP in Short ABs
(125728 + 38593 + 3853 + 27180 + 8911 + 36742)/(nrow(allYearsShortAbs) - (89 + 1095 + 75 + 40 + 2 + 6 + 1920 + 193 + 56 + 38 + 9 + 7 + 7 + 6 + 1 + 2 + 6 + 1 + 1))
# 0.301


# Slash Lines
# Long ABs: 0.200/0.370/0.335 (0.705 OPS)
# Short ABs: 0.251/0.301/0.415 (0.716 OPS)


# Let's answer a very important question: How do CHANGES in QAB% correlate to offensive improvement?

# Assemble yearly df's, then do changes between each year
stuff2015 <- qabs_and_stats %>% 
  filter(season == 2015) %>% 
  select(player_name, qab_percent, long_ab_percent, woba, xwoba, xwobacon)

stuff2016 <- qabs_and_stats %>% 
  filter(season == 2016) %>% 
  select(player_name, qab_percent, long_ab_percent, woba, xwoba, xwobacon)

stuff2017 <- qabs_and_stats %>% 
  filter(season == 2017) %>% 
  select(player_name, qab_percent, long_ab_percent, woba, xwoba, xwobacon)

stuff2018 <- qabs_and_stats %>% 
  filter(season == 2018) %>% 
  select(player_name, qab_percent, long_ab_percent, woba, xwoba, xwobacon)

stuff2019 <- qabs_and_stats %>% 
  filter(season == 2019) %>% 
  select(player_name, qab_percent, long_ab_percent, woba, xwoba, xwobacon)


names(stuff2015)[2] <- "qab_percent2015"
names(stuff2015)[3] <- "long_ab_percent2015"
names(stuff2015)[4] <- "woba2015"
names(stuff2015)[5] <- "xwoba2015"
names(stuff2015)[6] <- "xwobacon2015"

names(stuff2016)[2] <- "qab_percent2016"
names(stuff2016)[3] <- "long_ab_percent2016"
names(stuff2016)[4] <- "woba2016"
names(stuff2016)[5] <- "xwoba2016"
names(stuff2016)[6] <- "xwobacon2016"

names(stuff2017)[2] <- "qab_percent2017"
names(stuff2017)[3] <- "long_ab_percent2017"
names(stuff2017)[4] <- "woba2017"
names(stuff2017)[5] <- "xwoba2017"
names(stuff2017)[6] <- "xwobacon2017"

names(stuff2018)[2] <- "qab_percent2018"
names(stuff2018)[3] <- "long_ab_percent2018"
names(stuff2018)[4] <- "woba2018"
names(stuff2018)[5] <- "xwoba2018"
names(stuff2018)[6] <- "xwobacon2018"

names(stuff2019)[2] <- "qab_percent2019"
names(stuff2019)[3] <- "long_ab_percent2019"
names(stuff2019)[4] <- "woba2019"
names(stuff2019)[5] <- "xwoba2019"
names(stuff2019)[6] <- "xwobacon2019"

changes1516 <- merge(stuff2015, stuff2016)
changes1617 <- merge(stuff2016, stuff2017)
changes1718 <- merge(stuff2017, stuff2018)
changes1819 <- merge(stuff2018, stuff2019)

changes1516 <- changes1516 %>% 
  mutate(delta_qab = qab_percent2016 - qab_percent2015,
         delta_longAb = long_ab_percent2016 - long_ab_percent2015,
         delta_woba = woba2016 - woba2015, 
         delta_xwoba = xwoba2016 - xwoba2015, 
         delta_xwobacon = xwobacon2016 - xwobacon2015)

changes1617 <- changes1617 %>% 
  mutate(delta_qab = qab_percent2017 - qab_percent2016,
         delta_longAb = long_ab_percent2017 - long_ab_percent2016,
         delta_woba = woba2017 - woba2016, 
         delta_xwoba = xwoba2017 - xwoba2016, 
         delta_xwobacon = xwobacon2017 - xwobacon2016)

changes1718 <- changes1718 %>% 
  mutate(delta_qab = qab_percent2018 - qab_percent2017,
         delta_longAb = long_ab_percent2018 - long_ab_percent2017,
         delta_woba = woba2018 - woba2017, 
         delta_xwoba = xwoba2018 - xwoba2017, 
         delta_xwobacon = xwobacon2018 - xwobacon2017)

changes1819 <- changes1819 %>% 
  mutate(delta_qab = qab_percent2019 - qab_percent2018,
         delta_longAb = long_ab_percent2019 - long_ab_percent2018,
         delta_woba = woba2019 - woba2018, 
         delta_xwoba = xwoba2019 - xwoba2018, 
         delta_xwobacon = xwobacon2019 - xwobacon2018)

change1 <- changes1516 %>% 
  select(delta_qab, delta_woba, delta_xwoba, delta_longAb)

change2 <- changes1617 %>% 
  select(delta_qab, delta_woba, delta_xwoba, delta_longAb)

change3 <- changes1718 %>% 
  select(delta_qab, delta_woba, delta_xwoba, delta_longAb)

change4 <- changes1819 %>% 
  select(delta_qab,delta_woba, delta_xwoba, delta_longAb)


changes <- rbind(change1, change2, change3, change4)

write.csv(changes, "Yearly QAB Changes.csv")

# Changes in QAB% vs Changes in wOBA/xwOBA
ggplot(changes, aes(x=delta_qab, y=delta_xwoba))+
  geom_point(size=3, alpha=0.5)+
  stat_smooth(method="loess")+
  ggtitle("Changes in QAB% vs. Changes in xwOBA", subtitle = "Qualified Hitters, 2015-2020") + 
  xlab("Change in QAB%") + 
  ylab("Change in xwOBA")

ggplot(changes, aes(x=delta_qab, y=delta_woba))+
  geom_point(size=3, alpha=0.5)+
  stat_smooth(method="loess")+
  ggtitle("Changes in QAB% vs. Changes in wOBA", subtitle = "Qualified Hitters, 2015-2020") + 
  xlab("Change in QAB%") + 
  ylab("Change in wOBA")


# Not surprisingly, they track quite well. But we expected this. Getting hits, drawing walks, and hitting the ball
# hard are all captured in xwOBA. So what value do we get from examining quality at-bats? 


# Correlations
ggplot(qabs_and_stats, aes(x=qab_percent, y=woba)) +
  geom_point(size=3, alpha=0.5) + 
  stat_smooth(method="lm") + 
  ggtitle("QAB% vs wOBA", subtitle = "Qualified Hitters, 2015-2020") + 
  xlab("Quality At-Bat Percentage") + 
  ylab("wOBA")

ggplot(qabs_and_stats, aes(x=qab_percent, y=xwoba)) +
  geom_point(size=3, alpha=0.5) + 
  stat_smooth(method="lm") + 
  ggtitle("QAB% vs xwOBA", subtitle = "Qualified Hitters, 2015-2020") + 
  xlab("Quality At-Bat Percentage") + 
  ylab("xwOBA")

ggplot(qabs_and_stats, aes(x=qab_percent, y=on_base_plus_slg)) +
  geom_point(size=3, alpha=0.5) + 
  stat_smooth(method="lm") + 
  ggtitle("QAB% vs OPS", subtitle = "Qualified Hitters, 2015-2020") + 
  xlab("Quality At-Bat Percentage") + 
  ylab("OPS")

ggplot(qabs_and_stats, aes(x=long_ab_percent, y=b_bb_percent))+geom_point(size=3, alpha=0.5)+stat_smooth(method="lm")


# Outlier Investigation
outliers <- qabs_and_stats %>% 
  filter(player_name == "Mauer, Joe" & season == 2017 | player_name == "Morales, Kendrys" & season==2018 | 
           player_name == "Cain, Lorenzo" & season==2019 | player_name == "Pham, Tommy"&season==2020)

# Do they have longer at-bats?
mean(outliers$long_ab_percent) # 0.25
mean(qabs_and_stats$long_ab_percent) # 0.203

# But how productive are they?
mean(outliers$woba) # 0.317
mean(qabs_and_stats$woba) # 0.322

mean(outliers$on_base_plus_slg) # 0.723
mean(qabs_and_stats$on_base_plus_slg) # 0.764

# More disciplined?
mean(outliers$b_bb_percent) # 10.425
mean(qabs_and_stats$b_bb_percent) # 8.6

mean(outliers$b_k_percent) # 18.175
mean(qabs_and_stats$b_k_percent) # 20.96

# Quality of contact?
mean(outliers$barrel_batted_rate) # 7
mean(qabs_and_stats$barrel_batted_rate) # 7.09

mean(outliers$hard_hit_percent) # 47.75
mean(qabs_and_stats$hard_hit_percent) # 36.55

mean(outliers$poorlyweak_percent) # 2.28
mean(qabs_and_stats$poorlyweak_percent) # 3.51

# All four drastically underperformed their xwOBA and xwOBAcon.


