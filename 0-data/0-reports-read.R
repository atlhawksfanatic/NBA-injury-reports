# 0-reports-read.R - Read Injury Reports

# ---- start --------------------------------------------------------------

library(tidyverse)
library(pdftools)
library(lubridate)

# Create a directory for the data
local_dir     <- "0-data/reports"
data_source   <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# ---- function -----------------------------------------------------------

read_injury_report <- function(ir) {
  print(ir)
  x <- tryCatch(pdf_text(ir),
                error = function(x) return(data.frame(file = ir)))
  
  if (x[1] == "") return()
  
  # Change the headers
  x <- str_replace_all(x, "Game Date", "Game_Date")
  x <- str_replace_all(x, "Game Time", "Game_Time")
  x <- str_replace_all(x, "Player Name", "Player_Name")
  x <- str_replace_all(x, "Current Status", "Current_Status")
  x <- str_replace_all(x, "Previous Status", "Previous_Status")
  
  # First get all lines of pdf that are relevant
  lines <- strsplit(x, "\n")
  
  head_info <- lines %>% 
    map(~data.frame(x = str_trim(.[[1]]))) %>% 
    bind_rows()
  
  info1 <- lines %>% 
    # Remove the first and last line, read it in 
    imap(function(x, idx) {
      if (length(x) < 3) (return(data.frame()))
      temp = x[2:(length(x)-1)]
      
      # temp_names = find_starts(temp[1])
      # 
      # temp_read <- read_fwf(I(temp), find_starts(temp[1]))
      
      temp_read <- read_fwf(I(temp),
                            fwf_empty(
                              I(temp),
                              col_names = c(
                                "Game_Date",
                                "Game_Time",
                                "Matchup",
                                "Team",
                                "Player_Name",
                                "Category",
                                "Reason",
                                "Current_Status",
                                "Previous_Status"
                              )
                            ),
                            show_col_types = FALSE)
      # return(temp_read)
      
      # Attempt
      temp_ncol = ncol(temp_read)
      temp_names <- t(apply(temp_read[1,], 1, zoo::na.locf))
      colnames(temp_read) <- janitor::make_clean_names(temp_names)
      # colnames(temp_read) <- temp_names
      
      if ("reason_current_status" %in% colnames(temp_read)) {
        temp_read$current_status_alt = word(temp_read$reason_current_status, -1)
        temp_read$reason_alt = temp_read$reason_current_status |> 
          str_squish() |> 
          str_remove_all(temp_read$current_status_alt)
      }
      
      # Combine all the columns that had adjacent NAs
      temp_read1 <- temp_read |> 
        mutate(row = row_number()) |> 
        pivot_longer(-row) |> 
        group_by(row, name = sub("(_\\d+)?$", "", name)) |> 
        summarize(value = value |>
                    str_replace_na("") |> 
                    str_c(collapse = " ") |> 
                    str_trim(), .groups = "drop") |> 
        pivot_wider() |> 
        select(row, any_of(janitor::make_clean_names(temp_names)),
               everything()) |>
        filter(row != 1) |> 
        mutate(ncol = temp_ncol,
               page = idx) |> 
        mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
      
      return(temp_read1)
    }) %>% 
    set_names(head_info$x) |> 
    bind_rows(.id = "head_info") |> 
    mutate(file = ir)
  
  info2 <- fill(info1, any_of(c("game_date", "game_time",
                                "matchup", "team")),
                .direction = "down")
  
  return(info2)
}

# ---- read ---------------------------------------------------------------

ir_file <- paste0(local_dir, "/raw_injury_reports.csv")

if (file.exists(ir_file)) {
  old_info <- read_csv(ir_file, col_types = cols(.default = "c"))
} else {
  old_info <- data.frame(file = character())
}

ir_dir <- dir(data_source, pattern = "*.pdf",
              full.names = T, recursive = T)

ir_files <- file.info(ir_dir, extra_cols = F) |> 
  rownames_to_column("file_name") |> 
  mutate(szn = basename(dirname(file_name)),
         date = str_extract(file_name, "(?<=_).*(?=_)"),
         time = str_extract(file_name, "(?<=[0-9]_).*(?=.pdf)"))

ir_map <- ir_files |> 
  filter(!is.na(date), szn !=  "hack",
         !(file_name %in% unique(old_info$file))) |> 
  pull(file_name) |> 
  map(read_injury_report)

ir_data <- bind_rows(old_info, ir_map)

write_csv(ir_data, ir_file)
