# =============================================================================
# SCRIPT: Project-Level Demographic Data Preparation
#
# This script prepares the demographic data for the project-level analysis.
# It fetches data from the U.S. Census, loads project data, matches each
# project to a Census-defined place, and exports the final combined dataset.

# Methods explained in Appendix of report
# =============================================================================


#### SETUP: LOAD LIBRARIES AND SET API KEY ####

# Load necessary packages
library(tidyverse)
library(usdata)
library(readxl)
library(tidycensus)
library(janitor)

# Set the Census API key for data retrieval
# Data also available via census_data.csv in root

census_api_key("YOUR CENSUS KEY")


#### FUNCTIONS ####

# Fetches ACS demographic data for all "places" within a given state.
get_census_places <- function(state_abbr) {
  vars <- c(
    "B01003_001", # Total Population
    "B19013_001", # Median Household Income
    "B17001_002", # Number of people below poverty level
    "B03002_003"  # White alone, Not Hispanic or Latino
  )
  
  get_acs(
    geography = "place",
    variables = vars,
    state = state_abbr,
    year = 2022,
    survey = "acs5",
    output = "wide"
  ) %>%
    mutate(state_abbr = state_abbr)
}

# Matches a project description to the best corresponding Census place.
find_place_and_geoid <- function(description, state, all_census_data) {
  description <- str_replace_all(description, "\\s+", " ") %>% str_trim()
  state_census_data <- all_census_data %>% filter(state_abbr == state)
  
  if (nrow(state_census_data) == 0) {
    return(tibble(Matched_Place = NA_character_, Matched_GEOID = NA_character_))
  }
  
  places_for_state <- state_census_data %>%
    pull(place_name) %>%
    unique() %>%
    .[order(nchar(.), decreasing = TRUE)]
  
  pattern <- paste0("\\b(", paste(places_for_state, collapse = "|"), ")\\b")
  matched_name <- str_extract(description, regex(pattern, ignore_case = TRUE))
  
  if (!is.na(matched_name)) {
    state_census_data %>%
      filter(str_to_lower(place_name) == str_to_lower(matched_name)) %>%
      select(Matched_Place = place_name, Matched_GEOID = GEOID) %>%
      slice(1)
  } else {
    tibble(Matched_Place = NA_character_, Matched_GEOID = NA_character_)
  }
}


#### 1. LOAD AND PREPARE U.S. CENSUS DATA ####

# Define the list of states for which to retrieve data
states_of_interest <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
  "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
  "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# Retrieve and combine Census data for all specified states
census_data_list <- lapply(states_of_interest, get_census_places)
census_data <- bind_rows(census_data_list)

# Clean and transform the raw Census data into analysis-ready variables
census_data <- census_data %>%
  rename(
    population = B01003_001E,
    mhi = B19013_001E,
    below_poverty = B17001_002E,
    white_non_hispanic = B03002_003E
  ) %>%
  mutate(
    pct_below_poverty = (below_poverty / population) * 100,
    pct_poc = 100 - ((white_non_hispanic / population) * 100),
    place_name = str_replace(NAME, "\\s+(CDP|city|town|village|borough)?\\s*,.*$", "")
  )

#Can also create a backup of data so don't need to load it every run
#write_csv(census_data,"census_data.csv")

#### 2. LOAD, COMBINE, AND MATCH PROJECT DATA ####

# Load Earmarks Data (DW and CW)
earmarks_23 <- read_excel("section3/raw_data/earmarks_data.xlsx", sheet = "cds_23") %>%
  clean_names() %>%
  mutate(
    program = case_when(
      str_detect(account, "Drinking Water") ~ "DW",
      str_detect(account, "Clean Water") ~ "CW",
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(state_abbr = state, description = project, type = "earmark", program, fy = 23)

earmarks_24 <- read_excel("section3/raw_data/earmarks_data.xlsx", sheet = "cds_24") %>%
  clean_names() %>%
  mutate(
    program = case_when(
      str_detect(account, "Drinking Water") ~ "DW",
      str_detect(account, "Clean Water") ~ "CW",
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(state_abbr = state, description = project, type = "earmark", program, fy = 24)

# Load Drinking Water (DW) SRF Data
dw_srf_23 <- read_excel("section3/raw_data/National_Drinking Water Assistance Agreement Detail Report_FY23.xlsx", skip = 4) %>%
  clean_names() %>%
  mutate(state_abbr = state2abbr(state)) %>%
  transmute(state_abbr, description = city, type = "srf", program = "DW", fy = 23)

dw_srf_24 <- read_excel("section3/raw_data/National_Drinking Water Assistance Agreement Detail Report_FY24.xlsx", skip = 4) %>%
  clean_names() %>%
  mutate(state_abbr = state2abbr(state)) %>%
  transmute(state_abbr, description = city, type = "srf", program = "DW", fy = 24)

# Load Clean Water (CW) SRF Data
cw_srf_23 <- read_excel("section3/raw_data/FY23_CW Assistance Agreement Detail Report_20250704.xlsx", skip = 4) %>%
  clean_names() %>%
  mutate(state_abbr = state2abbr(state)) %>%
  transmute(state_abbr, description = borrower_name, type = "srf", program = "CW", fy = 23)

cw_srf_24 <- read_excel("section3/raw_data/FY24_CW Assistance Agreement Detail Report_20250704.xlsx", skip = 4) %>%
  clean_names() %>%
  mutate(state_abbr = state2abbr(state)) %>%
  transmute(state_abbr, description = borrower_name, type = "srf", program = "CW", fy = 24)

# Combine all project data sources
all_projects_raw <- bind_rows(earmarks_23, earmarks_24, dw_srf_23, dw_srf_24, cw_srf_23, cw_srf_24)

# Add initial counter  
count_initial_rows <- nrow(all_projects_raw)

all_projects <- all_projects_raw %>%
  filter(state_abbr %in% states_of_interest, !is.na(program))

# Add counter after initial filtering  
count_in_scope <- nrow(all_projects)

# Match all projects to their Census place
match_results <- purrr::map2_dfr(
  all_projects$description,
  all_projects$state_abbr,
  ~ find_place_and_geoid(description = .x, state = .y, all_census_data = census_data)
)

all_projects <- bind_cols(all_projects, match_results)

# Count how many projects failed to match  
count_unmatched <- sum(is.na(all_projects$Matched_GEOID))


#### 3. CREATE AND EXPORT FINAL DATASET ####

# Join project data with demographics and select final columns
projects_before_distinct <- all_projects %>%
  select(program, type, fy, state_abbr, GEOID = Matched_GEOID) %>%
  filter(!is.na(GEOID))

# Add counter for matched projects
count_matched_before_distinct <- nrow(projects_before_distinct)

final_project_data <- projects_before_distinct %>%
  distinct() %>%
  left_join(census_data, by = c("GEOID", "state_abbr"))

# Add final counter  
count_final <- nrow(final_project_data)

#### 4. DIAGNOSTIC REPORTING ####

# Calculate attrition numbers
dropped_by_filter <- count_initial_rows - count_in_scope
dropped_by_dedupe <- count_matched_before_distinct - count_final
retention_rate <- (count_final / count_in_scope) * 100

# Print a summary report to the console
message("\n---")
message("Data Attrition & Matching Report")
message("---")
message(paste0("1. Initial project entries loaded: ", format(count_initial_rows, big.mark = ",")))
message(paste0("   - Dropped (invalid state/program): ", format(dropped_by_filter, big.mark = ",")))
message("--------------------------------------------------")
message(paste0("2. Projects in scope for analysis: ", format(count_in_scope, big.mark = ",")))
message(paste0("   - Failed to match a Census place: ", format(count_unmatched, big.mark = ",")))
message(paste0("   - Successfully matched: ", format(count_matched_before_distinct, big.mark = ",")))
message("--------------------------------------------------")
message(paste0("3. Matched projects after removing duplicates: ", format(count_final, big.mark = ",")))
message(paste0("   - Dropped by deduplication: ", format(dropped_by_dedupe, big.mark = ",")))
message("--------------------------------------------------")
message(sprintf("Final Retention Rate (final / in-scope): %.1f%%", retention_rate))
message("---\n")


# Export the final dataset to a CSV file
write_csv(final_project_data, "section3/raw_data/prj_level_dem_data.csv") 
