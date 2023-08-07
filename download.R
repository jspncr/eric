library(tidyverse)
library(rvest)
library(curl)

download_files <- FALSE

main_url <- "https://digital.nhs.uk/data-and-information/publications/statistical/estates-returns-information-collection"


main_html <- read_html(main_url)
main_links <- main_html %>% html_elements("a") 


all_links <- tibble(
  title = html_text(main_links)
  , link = html_attr(main_links, "href")
) %>%
  mutate(
    eric = str_detect(title, regex("eric", ignore_case = TRUE)) | 
      str_detect(title, regex("estates return(s?) information collection", ignore_case = TRUE))
    , historic = str_detect(title, "Historical data") & str_detect(title, "England")
  ) %>%
  print()

eric_links <- all_links %>% 
  filter(eric) %>%
  mutate(full_link = paste0("https://digital.nhs.uk", link)) 

historic_eric_link <- all_links %>% 
  filter(historic) %>%
  mutate(full_link = paste0("https://digital.nhs.uk", link))


# Navigate to the individual year pages
all_eric_html <- eric_links %>%
  mutate(html = map(full_link, read_html))

historic_eric_html <- historic_eric_link %>%
  mutate(html = map(full_link, read_html))

make_links_df <- function(html, do_filter = TRUE){
  links <- html_elements(html, "a")
  
  all_links <- tibble(
    title = html_text(links) %>% str_remove_all(., "\\\n") %>% str_squish()
    , link = html_attr(links, "href")
  ) %>% 
  mutate(
    filename = URLdecode(link)
    , file_ext = str_to_lower(tools::file_ext(filename))
    , eric = str_detect(title, "ERIC")
    , dq_report = str_detect(title, regex("data", ignore_case = TRUE)) &
                  str_detect(title, regex("quality", ignore_case = TRUE)) &
                  str_detect(title, regex("report", ignore_case = TRUE))
    , report = str_detect(title, regex("report", ignore_case = TRUE)) &
               str_detect(title, regex("xlsx", ignore_case = TRUE)) 
    , data_definitions = str_detect(title, regex("data", ignore_case = TRUE)) &
                         str_detect(title, regex("definitions", ignore_case = TRUE))
    , trust = str_detect(title, regex("data", ignore_case = TRUE)) &
              str_detect(title, regex("trust", ignore_case = TRUE)) &
              str_detect(link, regex("\\.csv$", ignore_case = TRUE))
    ,  site = str_detect(title, regex("data", ignore_case = TRUE)) &
              str_detect(title, regex("site", ignore_case = TRUE)) &
              str_detect(link, regex("\\.csv$", ignore_case = TRUE))
    ,  pfi = str_detect(title, regex("data", ignore_case = TRUE)) &
              str_detect(title, regex("pfi", ignore_case = TRUE)) &
              str_detect(link, regex("\\.csv$", ignore_case = TRUE))
  ) 
  
  if(do_filter){
    report_links <- all_links %>%
      filter(eric & (dq_report | report | data_definitions | trust | site | pfi))
    report_links
  } else {
    all_links
  } 
}

# filter dataset to one link
# download to folder

all_eric_trust_dl <- all_eric_html %>%
  rename(page_title = title, main_link = full_link) %>%
  mutate(file_links = map(html, make_links_df)) %>%
  select(-html, -link, -eric, -historic) %>%
  unnest(file_links)


historic_eric_dl <- historic_eric_html %>%
  rename(page_title = title, main_link = full_link) %>%
  mutate(file_links = map(html, make_links_df, do_filter = FALSE)) %>%
  select(-html, -link, -eric, -historic) %>%
  unnest(file_links) %>%
  # Need to filter manually since old files are a bit different
  filter(eric) %>%
  select(-dq_report, -report, -trust, -site, -pfi) %>%
  mutate(
    trust_and_site = str_detect(title, regex("trust", ignore_case = TRUE)) &
                     str_detect(title, regex("site", ignore_case = TRUE))
  ) %>%
  filter(trust_and_site | data_definitions) 

year_to_fyear <- function(x, sep = "-"){
  
  if(typeof(x) != "character") x <- as.character(x)
  
  stopifnot(!is.na(as.integer(x))) # need x to coercible to an int
  
  if(nchar(x) == 4){
    y <- as.character(as.integer(x) + 1L)
    
    ans <- paste0(x, sep, substr(y, nchar(y) - 1, nchar(y)))
    
  } else if(nchar(x) == 6){
    ans <- paste0(substr(x, 1, 4), sep, substr(x, 5, 6))
  } 
  
  return(ans) 
}

eric_file_info <- all_eric_trust_dl %>% 
  mutate(year = page_title %>% 
           str_extract(., "\\d+")) %>%
  mutate(fyear = map_chr(year, year_to_fyear, sep = "")) %>% 
  mutate(tidy_file = paste0(
    "ERIC_"
    , fyear, "_"
    , case_when(
        dq_report ~ "data_quality_report",
        report ~ "report",
        data_definitions ~ "data_definitions",
        trust ~ "trust",
        site ~ "site",
        pfi ~ "pfi",
        TRUE ~ "UNKNOWN"
      )
    , "."
    , file_ext
    )
  ) 

historic_file_info <- historic_eric_dl %>%
  mutate(
    year = str_extract(title, "\\d+")
    , fyear = map_chr(year, year_to_fyear, sep = "")
    , tidy_file = paste0(
      "ERIC_"
      , fyear, "_"
      , case_when(
        data_definitions ~ "data_definitions",
        trust_and_site ~ "trust_and_site",
        TRUE ~ "UNKNOWN"
      )
      , "."
      , file_ext
    )
  ) 
  
# This will download all of the files
if(download_files){
  # files in current format
  walk2(eric_file_info$link, paste0("data/", eric_file_info$tidy_file), curl::curl_download)
  # files from historic format
  walk2(historic_file_info$link, paste0("data/", historic_file_info$tidy_file), curl::curl_download)
}


# save the origin of these files since we've renamed them
eric_origin <- eric_file_info %>%
  mutate(date_downloaded = Sys.Date())

eric_origin_historic <- historic_file_info %>%
  mutate(date_downloaded = Sys.Date())

write_csv(eric_origin, file = "data/eric_origin.csv")
write_csv(eric_origin_historic, file = "data/eric_origin_historic.csv")
