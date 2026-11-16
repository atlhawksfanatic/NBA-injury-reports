# 1-tidy-reports.R - Tidy Injury Reports

# ---- start --------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Create a directory for the data
local_dir     <- "1-tidy/reports"
data_source   <- "0-data/reports/"
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# ---- season-info --------------------------------------------------------

# NBA Season time periods
nba_dates_long <- tribble(
  ~season, ~season_section, ~start_date, ~end_date,
  2015, "season", "2014-10-28", "2015-4-15",
  2015, "playoffs", "2015-4-18", "2015-6-16",
  2016, "season", "2015-10-27", "2016-4-13",
  2016, "playoffs", "2016-4-16", "2016-6-19",
  2017, "season", "2016-10-25", "2017-4-12",
  2017, "playoffs", "2017-4-15", "2017-6-12",
  2018, "season", "2017-10-17", "2018-4-11",
  2018, "playoffs", "2018-4-14", "2018-6-8",
  2019, "season", "2018-10-16", "2019-4-10",
  2019, "playoffs", "2019-4-13", "2019-6-13",
  2020, "season", "2019-10-22", "2020-3-11",
  2020, "playoffs", "2020-8-15", "2020-10-13",
  2020, "bubble", "2020-7-30", "2020-8-14",
  2021, "season", "2020-12-22", "2021-5-16",
  2021, "playoffs", "2021-5-18", "2021-7-30",
  2022, "season", "2021-10-18", "2022-4-11",
  2022, "playoffs", "2022-4-12", "2022-6-20",
  2023, "season", "2022-10-17", "2023-4-10",
  2023, "playoffs", "2023-4-11", "2023-6-20",
  2024, "season", "2023-10-23", "2024-04-15",
  2024, "playoffs", "2024-04-16", "2024-06-21"
) |> 
  mutate_at(vars(-season, -season_section), as.Date) |> 
  rowwise() |>
  mutate(days = end_date - start_date + 1,
         date = list(seq(start_date, end_date, by = "days"))) |> 
  unnest(date) |> 
  select(season, season_section, date)


# ---- read ---------------------------------------------------------------

ir_raw <- read_csv("0-data/reports/raw_injury_reports.csv",
                   col_types = cols(.default = "character"))

# ---- manipulate ---------------------------------------------------------

# Only the "reason" when it's Injury/Illness will break across multiple lines.
#  An NA in "reason" when it's not Injury/Illness should be counted as "-"

# Low-hanging fruit: 
ir_temp <- ir_raw |> 
  mutate(team = ifelse(team %in% c("Minnesota", "Timberwolves"),
                       "Minnesota Timberwolves", team),
         current_status = ifelse(is.na(current_status),
                                 current_status_alt, current_status),
         reason = ifelse(is.na(reason), reason_alt, reason),
         date = as.Date(game_date, format = "%m/%d/%Y"),
         report_date = as.POSIXct(str_sub(file, start = -19, end = -5),
                                  format = "%Y-%m-%d_%I%p",
                                  tz = "America/New_York")) |> 
  # NA reason when not Injury/Illness
  
  # Issue with John Wall's Achilles tear
  mutate(reason = case_when(current_status == "tear Out" ~
                              "Left Achilles’ tendon tear",
                            T ~ reason),
         current_status = case_when(current_status == "tear Out" ~
                              "Out",
                            T ~ current_status)) |> 
  # Not submitted yet
  mutate(across(c(player_name, current_status, reason, category),
                ~ifelse((category %in% "NOT YET SUBMITTED") | 
                          (reason %in% "NOT YET SUBMITTED"),
                        "NOT YET SUBMITTED",
                        .x))) |> 
  # All available
  mutate(across(c(player_name, current_status, reason, category),
                ~ifelse((category %in% "ALL PLAYERS AVAILABLE") | 
                          (reason %in% "ALL PLAYERS AVAILABLE"),
                        "ALL PLAYERS AVAILABLE",
                        .x))) |> 
  # Remove the rows when they are all NA
  filter(if_any(c(player_name, reason, current_status, category),
                ~ !is.na(.))) |> 
  # Add in the season information
  left_join(nba_dates_long)

# Issue: sometimes an injury will span multiple lines and it ends up above and
#  below the player's name. Need to grab these 3 lines and concatenate them
#  together but ignore the NAs. Treat it like a streak...when an NA appears we
#  need a new ID which only switches *after* the subsequent NA. Only do this
#  for a page.
ir_grouping <- ir_temp |> 
  filter(report_date < "2019-11-15") |> 
  group_by(file, page) |> 
  mutate(good = (!is.na(player_name) & !is.na(reason)), #  & !is.na(category)
         bad = is.na(player_name) & is.na(category),
         id = cumsum(good),
         bad_player = is.na(player_name),
         # Only increase at odd intervals
         increment = (ceiling(cumsum(bad_player) / 2)),
         lag_id = id + increment) |>
  group_by(file, page, lag_id) |>
  summarise(across(c(player_name, reason, current_status, category),
                   ~ paste(na.omit(.), collapse = " ")))


# Category seems to disappear with 2020/Injury-Report_2019-11-15_01PM.pdf



# This seems to be when the format changed: 2020/Injury-Report_2020-02-02_08PM.pdf


# After the 2019 season this splitting over 3 lines doesn't happen as much, it
#  seems to be just splitting below a line....
ir_grouping2 <- ir_temp |> 
  filter(season > 2019) |> 
  group_by(file, page) |> 
  mutate(good = (!is.na(player_name)),
         id = cumsum(good))

# if ID == 0 then it was the start of the page and needs to be 1

ir_grouping2 |> 
  mutate(id = ifelse(id == 0, 1, id)) |> 
  group_by(file, page, id) |> 
  summarise(across(c(player_name, reason, current_status, category),
                   ~ paste(na.omit(.), collapse = " "))) |> View()



