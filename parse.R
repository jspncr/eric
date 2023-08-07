library(tidyverse)
library(janitor)
library(readxl)

# First load the new style eric data
eric_origin <- read_csv("data/eric_origin.csv")

eric_trusts <- eric_origin %>%
  filter(trust) %>%
  mutate(file_path = paste0("data/", tidy_file)) %>%
  select(fyear, tidy_file, file_path)

read_eric_csv <- function(path, ...){
  yr <- as.integer(str_extract(path, "\\d+"))
  n_skip <- if_else(between(yr, 201415L, 201516L), 1, 0)
  first_pass <- tryCatch(
    read_csv(path
             , na = c("", "NA", "Not Applicable", "No Data Provided")
             , skip = n_skip
             ,...)
    , error = function(cond) {
       daread_csv(path
                , na = c("", "NA", "Not Applicable", "No Data Provided")
                , skip = n_skip
                , locale = locale(encoding =  "latin1") # NB could lose some special chars
                , ...) 
    }
  )
  
  # Some csv's have annoying rows at the end which ruin data import
  # We can filter them out by removing all rows from when a value in the first column is na
  first_col_nas <- is.na(first_pass[,1])
  n_max_check <- which(first_col_nas == 1)[1] 
  n_max <- if_else(is.na(n_max_check), Inf, n_max_check - 1)
  
  if(is.na(n_max_check)){
    return(first_pass)
  } else {
    second_pass <- tryCatch(
      read_csv(path
               , na = c("", "NA", "Not Applicable", "No Data Provided")
               , skip = n_skip
               , n_max = n_max
               ,...)
      , error = function(cond) {
        read_csv(path
                 , na = c("", "NA", "Not Applicable", "No Data Provided")
                 , skip = n_skip
                 , n_max = n_max
                 , locale = locale(encoding =  "latin1") # NB could lose some special chars
                 , ...) 
      }
    )
    return(second_pass)
  }
  
}

latin1_to_utf8 <- function(x, ...) iconv(x, from = "latin1", to = "UTF-8", ...) 

eric_data <- eric_trusts %>%
  mutate(
    raw_data = map(file_path, read_eric_csv, show_col_types = FALSE)
    , raw_cols = map(raw_data, colnames)
    , utf8_cols = map(raw_cols, latin1_to_utf8)
    , clean_cols = map(utf8_cols, janitor::make_clean_names)
    , named_data = map2(raw_data, clean_cols, function(x, y){
          colnames(x) <- y
          return(x)
        })
    
  ) %>%
  select(-tidy_file, -file_path)

# Check that we can combine the different years of ERIC
mismatch_types <- janitor::compare_df_cols(eric_data$named_data, return = "mismatch")
janitor::compare_df_cols_same(eric_data$named_data)

# Combine the years
combined_eric <- eric_data %>%
  select(fyear, named_data) %>%
  unnest(named_data)


# Historic data ----------------------------------------------------------------

# Load the historic data
historic_eric_origin <- read_csv("data/eric_origin_historic.csv")

historic_eric_trusts <- historic_eric_origin %>%
  filter(trust_and_site) %>%
  mutate(file_path = paste0("data/", tidy_file)) %>%
  select(fyear, tidy_file, file_path)

mode_stats <- function(x, na.rm = TRUE){
  unique_x <- unique(x)
  
  if(na.rm) unique_x <- unique_x[!is.na(unique_x)] # drop na in case that causes problems
  
  ans <- unique_x[which.max(tabulate(match(x, unique_x)))]
  return(ans)
  
}

read_excel_guess_skip <- function(path, sheet, ...){
  # NB would need to amend to handle sheets without column names
  
  # load a blank version of the worksheet
  df <- read_excel(path = path, sheet = sheet, col_names = TRUE
                   , range = cell_limits(c(1, 1), c(NA, NA)), ...)
  
  # get the index of the first non-na value in every column
  first_non_na <- unname(sapply(df, function(x) which(!is.na(x))[1]))
  
  # use the mode
  first_row_guestimate <- mode_stats(first_non_na, na.rm = TRUE)
  
  # Read the file again, this time the data starts after the header
  read_excel(path = path, sheet = sheet, col_names = TRUE, range = cell_rows(c(first_row_guestimate + 1, NA)), ...)
  
}


historic_eric_data <- historic_eric_trusts %>%
  mutate(
    raw_data = map(
        file_path
      , read_excel_guess_skip
      , sheet = "Trust Data"
      , na = c("", "Not Applicable", "No Data Provided", "Not Calculated")
      , .name_repair = "unique_quiet")
    , raw_cols = map(raw_data, colnames)
    , clean_cols = map(raw_cols, janitor::make_clean_names)
    , named_data = map2(raw_data, clean_cols, function(x, y){
      colnames(x) <- y
      return(x)
    })
  ) %>%
  select(-tidy_file, -file_path)

# Check that we can combine the different years of ERIC
historic_mismatch_types <- janitor::compare_df_cols(historic_eric_data$named_data, return = "mismatch")
janitor::compare_df_cols_same(historic_eric_data$named_data)

# Combine the years
historic_combined_eric <- historic_eric_data %>%
  select(fyear, named_data) %>%
  unnest(named_data) %>%
  filter(!is.na(organisation_name)) # two rows in 200506 only


# Time series of trust type by year -------------------------------------------- 

current_trust_type <- combined_eric %>% 
  select(fyear, trust_code, trust_type)

current_type_count <- count(current_trust_type, trust_type)

# There are more options for organisation_type in historic ERIC data
# We try to recategorise these options to the modern set of organisation_types
# This may not always be the right thing to do
historic_trust_type <- historic_combined_eric %>%
  select(fyear, organisation_code, organisation_type) %>%
  rename(trust_code = organisation_code) %>%
  mutate(trust_type = case_when(
      str_detect(organisation_type, "SMALL ACUTE") ~ "ACUTE - SMALL"
    , str_detect(organisation_type, "MEDIUM ACUTE") ~ "ACUTE - MEDIUM"
    , str_detect(organisation_type, "LARGE ACUTE") ~ "ACUTE - LARGE"
    , str_detect(organisation_type, "ACUTE SPECIALIST") ~ "ACUTE - SPECIALIST"
    , str_detect(organisation_type, "SMALL COMMUNITY") ~ "COMMUNITY"
    , str_detect(organisation_type, "MEDIUM COMMUNITY") ~ "COMMUNITY"
    , str_detect(organisation_type, "LARGE COMMUNITY") ~ "COMMUNITY"
    , str_detect(organisation_type, "MENTAL HEALTH") |
      str_detect(organisation_type, "LEARNING DISABILITY") ~ "MENTAL HEALTH AND LEARNING DISABILITY"
    , str_detect(organisation_type, regex("acute", ignore_case = TRUE)) &
      str_detect(organisation_type, regex("teaching", ignore_case = TRUE)) ~
        "ACUTE - TEACHING"
    , str_detect(organisation_type, "MULTI-SERVICE") ~ "ACUTE - MULTI-SERVICE" # is this always true?
    , str_detect(organisation_type, "TEACHING") ~ "ACUTE - TEACHING"
    , str_detect(organisation_type, "SPECIALIST") ~ "ACUTE - SPECIALIST"
    , str_detect(organisation_type, "ORTHOPAEDIC") ~ "ACUTE - SPECIALIST"
    , str_detect(organisation_type, "CHILDREN") ~ "ACUTE - SPECIALIST"
    , str_detect(organisation_type, "LONDON") ~ organisation_type %>% 
        str_remove(., "OUTSIDE") %>% str_remove(., "LONDON") %>% str_squish(.)
    , organisation_type %in% c("PCT", "PRIMARY CARE TRUST") ~ "CARE TRUST"
    , organisation_type == "SOCIAL ENTERPRISE" ~ "INDEPENDENT SECTOR" 
    , .default = str_squish(organisation_type))
  )

# Working code to check organisation types
# test <- count(historic_trust_type, organisation_type)
# test2 <- count(historic_trust_type, trust_code, trust_type) %>%
#   group_by(trust_code) %>%
#   mutate(n_rows = n()) %>%
#   filter(n_rows > 1)
# test3 <- count(historic_trust_type, trust_code, trust_type) %>%
#   anti_join(select(current_type_count, trust_type), by = "trust_type") %>%
#   group_by(trust_type) %>%
#   summarise(n = sum(n)) %>%
#   arrange(desc(n))
# test3

# Combine historic and other years trust type
all_data_trust_type <- bind_rows(
  select(historic_trust_type, -organisation_type)
  , current_trust_type
)

# Save results -----------------------------------------------------------------

# Combined ERIC
saveRDS(combined_eric, file = "data/combined_eric.RDS")

# Historic ERIC
saveRDS(historic_combined_eric, file = "data/historic_combined_eric.RDS")

# Save  out
write_csv(all_data_trust_type, "data/trust_type_lookup.csv")
saveRDS(all_data_trust_type, file = "data/trust_type_lookup.RDS")
