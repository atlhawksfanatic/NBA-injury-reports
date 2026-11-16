# 0-reports-download.R - Download Injury Reports

# ---- start --------------------------------------------------------------

library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

# Create a directory for the data
local_dir     <- "0-data/reports"
data_source   <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# ignore raw folders
filecon <- file(paste0(local_dir, "/.gitignore"))
writeLines("raw", filecon)
close(filecon)

# ---- nba-dates ----------------------------------------------------------

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
  mutate(days = end_date - start_date + 1) |> 
  # Started in 2019 season
  filter(season > 2018)

# Current Files downloaded
dir_ir <- dir("0-data/reports/raw", pattern = "*.pdf",
              full.names = T, recursive = T)

files_ir <- file.info(dir_ir, extra_cols = F) |> 
  rownames_to_column("file_name") |> 
  mutate(game_date = as.Date(str_extract(file_name, "(?<=_).*(?=_)")),
         time = str_extract(file_name, "(?<=[0-9]_).*(?=.pdf)"))

# Delete any files that have a size of 0 (ie bad download)
files_ir |> 
  filter(size == 0) |> 
  pull(file_name) |> 
  map(unlink)


url_base = "https://ak-static.cms.nba.com/referee/injury/Injury-Report"

hourly_updates <- list(c("12 AM.pdf", "01 AM.pdf", "02 AM.pdf", "03 AM.pdf",
                         "04 AM.pdf", "05 AM.pdf", "06 AM.pdf", "07 AM.pdf",
                         "08 AM.pdf", "09 AM.pdf", "10 AM.pdf", "11 AM.pdf",
                         "12 PM.pdf", "01 PM.pdf", "02 PM.pdf", "03 PM.pdf",
                         "04 PM.pdf", "05 PM.pdf", "06 PM.pdf", "07 PM.pdf",
                         "08 PM.pdf", "09 PM.pdf", "10 PM.pdf", "11 PM.pdf" ))

nba_dates_all <- nba_dates_long |>
  group_by(season, season_section) |> 
  mutate(game_date = list(seq(start_date, end_date, by = "1 day")),
         # The bubble had 11AM, 2PM, and 5:30 PM cadence. Hourly in the 21-22
         url_times = case_when(season == 2020 & season_section != "season" ~
                                 list(c("11AM.pdf", "02PM.pdf", "05PM.pdf")),
                               # season > 2021 ~ hourly_updates,
                               T ~ list(c("01PM.pdf",
                                          "05PM.pdf",
                                          "08PM.pdf")))) |>
  unnest(cols = c(game_date)) |> 
  unnest(cols = c(url_times)) |> 
  mutate(urls = paste(url_base, game_date, url_times, sep = "_"),
         time = str_extract(url_times, ".*(?=.pdf)")) |> 
  left_join(files_ir)

# ---- folders ------------------------------------------------------------

# Folders for each season
map(paste0(data_source, "/", unique(nba_dates_all$season)), function(x) {
  if (!file.exists(x)) dir.create(x, recursive = T)
})

# ---- download -----------------------------------------------------------

d_results <- paste0(local_dir, "/download_results.csv")

if (file.exists(d_results)) {
  nba_results <- read_csv(d_results)
} else {
  nba_results_map <- nba_dates_all |> 
    ungroup() |> 
    select(season, game_date, url_times, urls, size) |> 
    filter(#game_date > "2020-12-31", is.na(size),
           game_date < Sys.Date()) |>
    as.list() |>
    pmap(function(season, game_date, url_times, urls, size) {
      print(paste0(game_date, url_times, " at ", Sys.time()))
      file_x <- paste0(data_source, "/", season, "/", basename(urls))
      
      if (file.exists(file_x)) return(data.frame(game_date, urls,
                                                 result = "good", size))
      Sys.sleep(runif(1, 3, 7))
      
      
      if (!http_error(urls)) {
        result <- tryCatch(download.file(urls, file_x, method = "libcurl"),
                           warning = function(w) {
                             "bad"
                             }, finally = "not sure")
        result = ifelse(result == 0, "good", result)
        size = file.size(file_x)
      } else {
        result = "http error"
      }
      
      # Return a dataframe with the url checked and result
      return(data.frame(game_date, urls, result, size))
    })
  
  nba_results <- bind_rows(nba_results_map)
  
  write_csv(nba_results, d_results)
}

new_results_map <- nba_dates_all |> 
  left_join(nba_results) |> 
  ungroup() |> 
  filter(#game_date > "2020-12-31",
    game_date < Sys.Date(),
    is.na(result)) |>
  select(season, game_date, url_times, urls) |> 
  as.list() |>
  pmap(function(season, game_date, url_times, urls) {
    print(paste0(game_date, url_times, " at ", Sys.time()))
    file_x <- paste0(data_source, "/", season, "/", basename(urls))
    
    if (file.exists(file_x)) return(data.frame(game_date, urls,
                                               result = "good"))
    Sys.sleep(runif(1, 3, 7))
    
    
    if (!http_error(urls)) {
      result <- tryCatch(download.file(urls, file_x, method = "libcurl"),
                         warning = function(w) {
                           "bad"
                           }, finally = "not sure")
      result = ifelse(result == 0, "good", result)
    } else {
      result = "http error"
    }
    
    # Return a dataframe with the url checked and result
    return(data.frame(game_date, urls, result))
  })

new_results <- bind_rows(new_results_map)

bind_rows(nba_results, new_results) |>
  write_csv(d_results)


# ---- hack ---------------------------------------------------------------


# Hacked files to account for the changing file structure in the first two
#  months of injury reports happening.
hack_source   <- paste0(data_source, "/hack")
if (!file.exists(hack_source)) dir.create(hack_source, recursive = T)

url_list <- list(paste0("https://ak-static.cms.nba.com/wp-content/uploads/",
                        "sites/4/2018/",
                        c("10/Injury-Report", "11/Injury-Report",
                          "11/InjuryReport", "12/InjuryReport")))

nba_dates_hack <- 
  tibble(season = 2019,
         game_date = seq(as.Date("2018-10-16"),
                         as.Date("2018-12-17"), by = "1 day"),
         url_bases = url_list,
         url_times1 = list(c("01PM.pdf", "05PM.pdf", "08PM.pdf")),
         url_times2 = list(c("0130PM.pdf", "0530PM.pdf", "0830PM.pdf")),
         url_times3 = list(c("1.30.pdf", "5.30.pdf", "8.30.pdf"))) |>
  unnest(cols = c(url_times1, url_times2, url_times3)) |> 
  unnest(cols = c(url_bases)) |> 
  mutate(game_date_dot = format(game_date, "%m.%d.%y"),
         game_date_und = format(game_date, "%m_%d_%Y"),
         urls = ifelse(game_date < as.Date("2018-12-01"),
                       paste(url_bases, game_date_dot, url_times3, sep = "-"),
                       paste(url_bases, game_date_und, url_times2, sep = "_")),
         file_base = paste("Injury-Report", game_date, url_times1, sep = "_"),
         time = str_extract(url_times1, ".*(?=.pdf)"))

if (file.exists(paste0(hack_source, "/hacked_download_results.csv"))) {
  hack_results <- read_csv(paste0(hack_source, "/hacked_download_results.csv"))
} else {
  hack_results <- nba_dates_hack |> 
    ungroup() |> 
    select(game_date, urls) |> 
    # filter(game_date > "2020-12-31") |> 
    as.list() |>
    pmap(function(game_date, urls) {
      print(paste(game_date, urls, " at ", Sys.time()))
      file_x <- paste0(hack_source, "/", basename(urls))
      
      if (file.exists(file_x)) return()
      Sys.sleep(runif(1, 3, 7))
      
      
      if (!http_error(urls)) {
        result <- tryCatch(download.file(urls, file_x, method = "libcurl"),
                           warning = function(w) {
                             "bad"
                             })
      } else {
        result = NA
      }
      # Return a dataframe with the url checked and result
      return(data.frame(game_date, urls, result))
    })
}


hack_results |> 
  bind_rows() |> 
  left_join(nba_dates_hack) |> 
  write_csv(paste0(hack_source, "/hacked_download_results.csv"))

