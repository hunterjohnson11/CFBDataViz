cfbfastR::update_cfb_db("C:/Users/Admin/3D Objects")



cfbfastR::load_cfb_pbp(
  seasons = 2024, 
  dbdira = "C:/Users/Admin/3D Objects/cfb_pbp_db"
)


# Make sure you have these loaded
library(DBI)
library(RSQLite)
library(dplyr)

# Define the directory and the EXACT database file name (no extension needed here)
db_dir <- "C:/Users/Admin/3D Objects"
db_filename_no_extension <- "cfb_pbp_db"

# Construct the full path to the database file
db_path <- file.path(db_dir, db_filename_no_extension)

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Get a reference to the play-by-play table
# This does NOT load all data into memory yet, it's just a pointer.
pbp_data <- tbl(con, "cfbfastR_pbp")

# --- Accessing 2024 data ---
# Use the filter() verb to select only rows where 'season' is 2024
# Then, collect() to bring that filtered data into your R session as a tibble
pbp_2024 <- pbp_data %>%
  filter(season == 2023) %>%
  collect()
