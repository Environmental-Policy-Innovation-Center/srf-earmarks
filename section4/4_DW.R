# ==============================================================================
# DWSRF Cash Flow Model
# Author: Katy
# Date: 9/7/25
# Description: This script models the cash flow of Drinking Water State Revolving Funds
#              (DWSRF) for multiple states. It compares a baseline scenario against a
#              scenario that includes federal earmarks to analyze the net financial
#              impact on program funds over a 20-year projection period.

## UPDATED BY GW FOR 2025/2026 Data Feb 13 2026 
## GW: NOTE THAT CURRENT SETUP IS TO RUN 2026 AS HISTORICAL. 
## To run 2025 earmarks - swap historical/projection years in time period definitions & in projection_years
# ==============================================================================

# --- Load Required Libraries ---
library(dplyr)     # For data manipulation (filter, mutate, summarize, etc.)
library(readxl)    # For reading Excel files (.xlsx)
library(tidyr)     # For tidying data (pivot_longer, pivot_wider)
library(stringr)   # For string manipulation (str_extract)
library(readr)     # For reading CSV files efficiently
library(ggplot2)   # For creating visualizations
library(scales)    # For formatting plot axes (e.g., dollar amounts)
library(purrr)     # For functional programming (map functions)
library(forcats)   # For handling factor variables (fct_inorder)
library(showtext)  # For custom font support in plots

# ==============================================================================
# ---- 1. Configuration ----
# Define model parameters, target states, and timeframes.
# ==============================================================================
config <- list(
  # Define the states to include in the analysis
  target_states = c(
    'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA',
    'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD',
    'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ',
    'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC',
    'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY'
  ),
  
  
  # Financial assumptions
  set_asides_pct = 0.15,       # Percentage of capital grant allocated to set-asides
  loan_term_years = 20,        # Standard loan repayment term in years
  repayment_start_lag = 3,     # Years between loan execution and first repayment
  
  # Time period definitions
  historical_years = 2000:2026,   # Years for historical data analysis
  projection_years = 2027:2046,   # Years for future cash flow modeling
  baseline_period = 2003:2022,    # Historical years used to calculate future averages
  exclude_years = 2009            # Year(s) to exclude from baseline calculation (e.g., financial crisis)
)


# ==============================================================================
# ---- 2. Load All Excel Files ----
# Dynamically discover and load raw data files for each state.
# Source Note: Data from EPA DWSRF Annual Summary Reports.
# ==============================================================================
cat("Loading all Excel files...\n")

# --- Define expected filenames based on state codes ---
expected_files <- list.files(path = "data-updates/raw-data/dw_state_data", full.names = FALSE)



state_codes_from_names <- str_extract(expected_files, "^[A-Z]{2}")

# --- Set data directory path ---
data_directory <- "data-updates/raw-data/dw_state_data"

# --- File Discovery Loop ---
# Iterate through expected files, check if they exist, and store paths of found files.
found_files <- character()
found_state_codes <- character()

for(i in seq_along(expected_files)) {
  expected_file_name <- expected_files[i]
  state_code <- state_codes_from_names[i]
  file_path <- file.path(data_directory, expected_file_name)
  
  if(file.exists(file_path)) {
    found_files <- c(found_files, file_path)
    found_state_codes <- c(found_state_codes, state_code)
  } else {
    cat("Warning: Missing expected file:", file_path, "\n")
  }
}

# --- Data Loading ---
# Load all found Excel files into a single list. Each element of the list corresponds
# to a state's data frame. `tryCatch` handles potential errors during file reading.
cat("\nLoading", length(found_files), "Excel files...\n")
all_excel_data <- map(found_files, ~ {
  tryCatch({
    read_excel(.x, sheet = "REPORT", skip = 4) # Skip header rows in Excel file
  }, error = function(e) {
    cat("Error loading file", basename(.x), ":", e$message, "\n")
    return(NULL)
  })
})

# Filter out any files that failed to load and name list elements by state code.
non_null_indices <- !map_lgl(all_excel_data, is.null)
found_state_codes <- found_state_codes[non_null_indices]
all_excel_data <- all_excel_data[non_null_indices]
names(all_excel_data) <- found_state_codes

cat("Successfully loaded", length(all_excel_data), "Excel files for states:", paste(found_state_codes, collapse = ", "), "\n")

# ==============================================================================
# ---- 3. Helper Functions ----
# Define functions to process data, calculate projections, and model cash flows.
# ==============================================================================

#' Calculate Annual Loan Payment (Amortization Formula)
#'
#' Calculates the fixed annual payment for a loan given principal, interest rate, and term.
#' Handles zero interest rate scenarios separately.
#'
#' @param principal Numeric loan principal amount.
#' @param annual_rate_pct Numeric annual interest rate (as percentage, e.g., 2 for 2%).
#' @param term_years Integer loan term in years.
#' @return Numeric annual payment amount.
calculate_loan_payment <- function(principal, annual_rate_pct, term_years) {
  if (is.na(principal) || principal <= 0 || is.na(annual_rate_pct)) return(0)
  
  annual_rate <- annual_rate_pct / 100
  if (annual_rate == 0) return(principal / term_years)
  
  # Standard amortization formula for fixed annual payments
  principal * (annual_rate * (1 + annual_rate)^term_years) /
    ((1 + annual_rate)^term_years - 1)
}


#' Load and Process State-Specific Data
#'
#' Loads historical financial data for a single state from two sources:
#' 1. Earmark data from a prepared CSV file (filtered for DWSRF).
#' 2. SRF financial data (loans, grants, rates) from the pre-loaded Excel data.
#' It cleans and merges these sources into a single time-series data frame.
#'
#' @param state_abbr Character state abbreviation (e.g., "AK").
#' @return A tibble with historical financial data for the specified state.
load_state_data <- function(state_abbr) {
  # --- Load Earmarks Data ---
  # Assumes a df earmarks is generated from section 3 and saved out 
  earmarks_data <- read.csv("data-updates/clean-data/srf_funding_data_update_fy26.csv") %>%
    mutate(state_abbr = if_else(is.na(state_abbr),
                                state.abb[match(state, state.name)], # Convert state name to abbreviation if needed
                                state_abbr)) %>%
    filter(program == "DWSRF", state_abbr == !!state_abbr) %>% # Filter for DWSRF program
    transmute(
      year = as.numeric(str_replace(fy, "FY", "20")),
      earmarks = total_earmarks,
      earmark_impact = difference
    ) %>%
    filter(!is.na(year))
  
  # --- Extract State Financial Data from Excel ---
  if (!state_abbr %in% names(all_excel_data)) {
    stop(paste("Excel data not found for state:", state_abbr))
  }
  raw_data <- all_excel_data[[state_abbr]]
  
  # Map specific line numbers from the EPA report to meaningful variable names.
  # Note: These line numbers are specific to the DWSRF report structure.
  line_mapping <- c(`14` = "cap_grant", `125` = "executed_loans",
                    `430` = "subsidy", `287` = "loan_rate", `283` = "reported_repayments")
  
  # Data wrangling process:
  state_financial <- raw_data %>%
    rename(line_number = 3) %>%
    mutate(line_number = as.numeric(line_number)) %>%
    filter(line_number %in% names(line_mapping)) %>%
    select(line_number, matches("^[0-9]{4}$")) %>% # Select line number and year columns
    pivot_longer(-line_number, names_to = "year", values_to = "value") %>%
    transmute(
      year = as.integer(year),
      variable = line_mapping[as.character(line_number)],
      value = abs(parse_number(value))
    ) %>%
    pivot_wider(names_from = variable, values_from = value, values_fill = 0) %>%
    filter(year >= min(config$historical_years))
  
  # --- Combine Data Sources ---
  state_financial %>%
    left_join(earmarks_data, by = "year") %>%
    mutate(
      earmarks = coalesce(earmarks, 0), # Replace NA with 0 after join
      earmark_impact = coalesce(earmark_impact, 0),
      reported_repayments = coalesce(reported_repayments, 0)
    )
}


#' Create Future Projections based on Historical Averages
#'
#' Generates two projection scenarios ("Baseline" and "With Earmarks") for future years.
#' The baseline projection uses historical averages. The earmark scenario adjusts
#' projections based on recent earmark trends.
#'
#' @param historical_data Tibble containing historical financial data from `load_state_data`.
#' @return A tibble with projected data for both scenarios.
create_projections <- function(historical_data) {
  # Calculate baseline averages using the specified period, excluding outlier years.
  baseline_avg <- historical_data %>%
    filter(year %in% config$baseline_period,
           !year %in% config$exclude_years) %>%
    summarise(across(c(cap_grant, executed_loans, subsidy, loan_rate, reported_repayments),
                     ~mean(.x, na.rm = TRUE)))
  
  # Project earmarks based on a more recent period (e.g., last 2 years).
  ## updated to 2025
  recent_earmarks <- historical_data %>%
    filter(year %in% c(2023,2024,2026)) %>%
    summarise(
      avg_earmarks = mean(earmarks, na.rm = TRUE),
      avg_impact = mean(earmark_impact, na.rm = TRUE)
    )
  
  # --- Create Scenarios ---
  # 1. Baseline Scenario: Assumes historical averages continue, no future earmarks.
  baseline_proj <- tibble(
    year = config$projection_years,
    scenario = "Baseline",
    cap_grant = baseline_avg$cap_grant,
    executed_loans = baseline_avg$executed_loans,
    subsidy = baseline_avg$subsidy,
    loan_rate = baseline_avg$loan_rate,
    reported_repayments = baseline_avg$reported_repayments,
    earmarks = 0,
    earmark_impact = 0
  )
  
  # 2. With Earmarks Scenario: Adjusts baseline with projected earmark amounts.
  earmarks_proj <- baseline_proj %>%
    mutate(
      scenario = "With Earmarks",
      earmarks = coalesce(recent_earmarks$avg_earmarks, 0),
      earmark_impact = coalesce(recent_earmarks$avg_impact, 0),
      cap_grant = cap_grant + earmark_impact # Earmarks adjust total capital grant funding
    )
  
  bind_rows(baseline_proj, earmarks_proj)
}


#' Calculate Loan Repayments for All Years
#'
#' Models loan repayments by creating a schedule for each loan cohort (loans issued
#' in a specific year). It then aggregates repayments across all active loan cohorts
#' for each year in the analysis period.
#'
#' @param data Tibble containing combined historical and projected data for one scenario.
#' @return A tibble with total calculated repayments for each year.
calculate_repayments <- function(data) {
  # Create a data frame of all loans issued across historical and projection years.
  all_loans <- data %>%
    filter(!is.na(executed_loans), executed_loans > 0) %>%
    mutate(
      # Principal to be repaid is reduced by subsidies and earmarks.
      principal_amount = pmax(0, executed_loans - subsidy - earmarks),
      annual_payment = map2_dbl(principal_amount, loan_rate,
                                ~calculate_loan_payment(.x, .y, config$loan_term_years)),
      first_payment_year = year + config$repayment_start_lag,
      last_payment_year = first_payment_year + config$loan_term_years - 1
    ) %>%
    select(loan_year = year, annual_payment, first_payment_year, last_payment_year)
  
  # Aggregate repayments for each year in the simulation.
  all_years <- min(config$historical_years):max(config$projection_years)
  
  map_dfr(all_years, function(yr) {
    # Sum payments from all loan cohorts that are active in year 'yr'.
    repayments <- all_loans %>%
      filter(first_payment_year <= yr, last_payment_year >= yr) %>%
      pull(annual_payment) %>%
      sum(na.rm = TRUE)
    
    tibble(year = yr, repayments = repayments)
  })
}


#' Calculate Net Cash Flow
#'
#' Calculates annual net cash flow and cumulative cash position based on inflows
#' (grants, repayments) and outflows (loans executed, set-asides).
#'
#' @param data Tibble containing scenario data.
#' @param repayments_data Tibble containing calculated repayments from `calculate_repayments`.
#' @return A tibble with detailed cash flow calculations for each year.
calculate_cash_flow <- function(data, repayments_data) {
  data %>%
    left_join(repayments_data, by = "year") %>%
    mutate(
      repayments = coalesce(repayments, 0),
      # Inflows = Capitalization Grants + Loan Repayments
      total_inflows = cap_grant + repayments,
      # Outflows = Loans Executed + Set-Asides (calculated from cap grant)
      set_asides = cap_grant * config$set_asides_pct,
      total_outflows = executed_loans + set_asides,
      # Final calculation
      net_cash_flow = total_inflows - total_outflows,
      cumulative_cash_flow = cumsum(net_cash_flow)
    ) %>%
    arrange(year)
}

# ==============================================================================
# ---- 4. Execute Multi-State Model ----
# Orchestrate the analysis by running the functions for each state and scenario.
# ==============================================================================
analyze_multiple_states <- function(states) {
  all_results <- list()
  
  for (state in states) {
    cat("Analyzing state:", state, "\n")
    
    tryCatch({
      # 1. Load historical data for the state
      historical_data <- load_state_data(state)
      
      # 2. Create future projections for both scenarios
      projections <- create_projections(historical_data)
      
      # 3. Prepare full datasets (historical + projection) for each scenario
      baseline_scenario_data <- bind_rows(
        historical_data %>% mutate(scenario = "Baseline"),
        projections %>% filter(scenario == "Baseline")
      )
      earmarks_scenario_data <- bind_rows(
        historical_data %>% mutate(scenario = "With Earmarks"),
        projections %>% filter(scenario == "With Earmarks")
      )
      
      # 4. Calculate repayments for each scenario independently
      baseline_repayments <- calculate_repayments(baseline_scenario_data)
      earmarks_repayments <- calculate_repayments(earmarks_scenario_data)
      
      # 5. Calculate final cash flow and combine results
      state_results <- bind_rows(
        calculate_cash_flow(baseline_scenario_data, baseline_repayments),
        calculate_cash_flow(earmarks_scenario_data, earmarks_repayments)
      ) %>%
        mutate(state = state)
      
      all_results[[state]] <- state_results
      
    }, error = function(e) {
      cat("Error analyzing", state, ":", e$message, "\n")
    })
  }
  
  bind_rows(all_results) # Combine results from all states into one data frame
}

# Run analysis for all states defined in the configuration.
cat("Running analysis for all target states...\n")
all_state_results <- analyze_multiple_states(config$target_states)

# ==============================================================================
# ---- 5. Stats and Visualizations ----
# Summarize results and create final plots and tables.
# ==============================================================================

# --- Load Custom Font Support ---
tryCatch({
  font_add_google("Lato", "Lato")
  showtext_auto()
  showtext_opts(dpi = 300) # Synchronize DPI with ggsave
}, warning = function(w) {
  cat("Warning: Could not download font 'Lato'. Using system default font.\n", w$message, "\n")
})

# --- Impact Calculation ---
impact <- all_state_results %>%
  filter(year %in% config$projection_years, scenario %in% c("Baseline", "With Earmarks")) %>%
  group_by(state, scenario) %>%
  summarise(
    total_earmarks = sum(earmarks, na.rm = TRUE),
    total_repayments = sum(repayments, na.rm = TRUE),
    total_cap_grants = sum(cap_grant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = scenario,
    values_from = c(total_earmarks, total_repayments, total_cap_grants),
    names_sep = "_"
  ) %>%
  mutate(
    earmark_amount = `total_earmarks_With Earmarks`,
    repayment_loss_amount = `total_repayments_Baseline` - `total_repayments_With Earmarks`,
    net_funding_change = `total_cap_grants_With Earmarks` - total_cap_grants_Baseline,
    net_financial_impact = net_funding_change - repayment_loss_amount
  )

# --- Summary Table Output ---
state_summary_table <- impact %>%
  arrange(desc(net_financial_impact)) %>%
  transmute(
    State = state,
    `20-Year Change in Funding` = paste0("$", round(net_funding_change/1e6, 1), " million"),
    `20-Year Net Impact` = paste0("$", round(net_financial_impact/1e6, 1), " million"),
    `Loss-to-Gain Ratio` = paste0(round(abs(net_financial_impact)/abs(net_funding_change), 2), ":1")
  )

#write_csv(state_summary_table, "results-updates/section4/DW/4_DW_state_summary_26.csv")


#### Waterfall Plot Generation ####
# --- Waterfall Data Preparation ---
waterfall <- impact %>%
  filter(earmark_amount > 0) %>%
  arrange(net_financial_impact) %>%
  mutate(
    state = fct_inorder(state),
    loss_start = net_funding_change,
    loss_end = net_financial_impact,
    funding_change_positive = ifelse(net_funding_change >= 0, net_funding_change, 0),
    funding_change_negative = ifelse(net_funding_change < 0, net_funding_change, 0),
    impact_type = factor(case_when(
      net_financial_impact > 0 ~ "Positive Net Impact",
      net_financial_impact < 0 ~ "Negative Net Impact",
      TRUE ~ "Final Net Impact (Neutral)"
    ), levels = c("Positive Net Impact", "Negative Net Impact"))
  )

#write.csv(waterfall, "results-updates/section4/DW/4_DW_waterfall_26.csv", row.names = FALSE)

# --- Legend Order Definitions ---
marker_legend_order <- c(
  "Total Earmarks", "Repayment Loss", "Positive Net Impact", "Negative Net Impact"
)
fill_legend_order <- c(
  "Decreased Funding", "Increased Funding"
)

# --- Create Plot Object ---
### Through FY 25 ###
dw_waterfall_plot <- ggplot(waterfall, aes(y = state)) +
  
  # Layers 1-4 (Geoms and vline)
  geom_col(aes(x = funding_change_positive / 1e6, fill = "Increased Funding"), width = 0.3, alpha = 0.8) +
  geom_col(aes(x = funding_change_negative / 1e6, fill = "Decreased Funding"), width = 0.3, alpha = 0.8) +
  geom_errorbarh(
    aes(xmin = loss_start / 1e6, xmax = loss_end / 1e6, color = "Repayment Loss"),
    height = 0.2, linewidth = 1.2, alpha = 0.9
  ) +
  geom_point(aes(x = earmark_amount / 1e6, shape = "Total Earmarks", color = "Total Earmarks"), size = 3) +
  geom_point(aes(x = net_financial_impact / 1e6, shape = impact_type, color = impact_type), size = 6) +
  geom_vline(xintercept = 0, linetype = "solid", color = "#333333", linewidth = 0.5) +
  
  # --- Scales and Aesthetics ---
  scale_x_continuous(labels = label_dollar(suffix = "M"), breaks = scales::pretty_breaks(n = 8), expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(name = NULL, values = c("Increased Funding" = "#d9f4cc", "Decreased Funding" = "#f9dcc6"), limits = fill_legend_order) +
  # MODIFIED: Corrected the typo in the color code
  scale_color_manual(name = NULL, values = c("Total Earmarks" = "#172f60", "Repayment Loss" = "#b15712", "Positive Net Impact" = "#4ea324", "Negative Net Impact" = "#b15712"), limits = marker_legend_order) +
  scale_shape_manual(name = NULL, values = c("Total Earmarks" = 8, "Positive Net Impact" = 18, "Negative Net Impact" = 18, "Repayment Loss" = NA), limits = marker_legend_order) +
  
  # --- Labels and Theme ---
  labs(
    y = NULL, # Y-axis label removed
    x = NULL  # X-axis label removed
  ) +
  theme_minimal(base_size = 9, base_family = "Lato") +
  theme(
    plot.title = element_text(size = 9, face = "bold", margin = margin(b = 5), color = "#1A1A1A"),
    plot.subtitle = element_text(size = 9, color = "#555555", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#666666", margin = margin(t = 15), hjust = 0),
    axis.title = element_text(size = 9, color = "#333333"),
    axis.text.y = element_text(size = 9, color = "#444444"),
    axis.text.x = element_text(size = 9, color = "#444444"),
    legend.text = element_text(size = 9),
    
    # Grid and background styling
    panel.grid.major.x = element_line(color = "#E5E5E5", linewidth = 0.5, linetype = "solid"),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    
    # Legend styling
    legend.position = c(0.03, 0.99),
    legend.justification = c(0, 1),
    legend.box = "vertical",
    legend.spacing.y = unit(0.05, "cm"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = "lightgrey", linewidth = 0.3)
  ) +
  
  # --- Legend Key Customization ---
  guides(
    shape = "none",
    fill = guide_legend(order = 1),
    color = guide_legend(
      order = 2,
      override.aes = list(shape = c(8, NA, 18, 18), linetype = c("blank", "solid", "blank", "blank"))
    )
  )

print(dw_waterfall_plot)

#--- Save Final Output ---
output_plot_filename <- "results-updates/section4/DW/dw_waterfall_plot_26.png"
ggsave(
  filename = output_plot_filename,
  plot = dw_waterfall_plot,
  width = 6,
  height = 10,
  units = "in",
  dpi = 300
)

print(dw_waterfall_plot)

##### Maine Plot ####

## Generating State Based Repayment Plots ## 
# --- Create output directories if they don't exist ---

cat_palette <- colorRampPalette(c("#172f60","#1054a8",
                                  "#791a7b","#de9d29",
                                  "#b15712","#4ea324"))

dir.create("results-updates/section4/DW/state_repayment_graphs", recursive = TRUE, showWarnings = FALSE)
dir.create("results-updates/section4/DW/state_repayment_data", recursive = TRUE, showWarnings = FALSE)

# --- Get unique states from all_state_results ---
all_states <- unique(all_state_results$state)

# --- Loop over all states ---
for (state_abbr in all_states) {
  
  # --- Data Preparation ---
  state_repayment_comparison <- all_state_results %>%
    filter(
      state == state_abbr,
      year %in% config$projection_years,
      scenario %in% c("Baseline", "With Earmarks")
    ) %>%
    select(year, scenario, repayments) %>%
    mutate(
      scenario = case_when(
        scenario == "Baseline"      ~ "Without_Earmarks",
        scenario == "With Earmarks" ~ "With_Earmarks",
        TRUE                        ~ scenario
      )
    )
  
  # Skip if no data for this state
  if (nrow(state_repayment_comparison) == 0) {
    message("No data for state: ", state_abbr, " — skipping.")
    next
  }
  
  # --- Save CSV ---
  # write.csv(
  #   state_repayment_comparison,
  #   file = paste0("results-updates/section4/DW/state_repayment_data/",
  #                 state_abbr, "_repayment_data_26.csv"),
  #   row.names = FALSE
  # )
  # 
  # --- Create Plot ---
  state_plot <- ggplot(
    state_repayment_comparison,
    aes(x = year, y = repayments, color = scenario, linetype = scenario)
  ) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    
    scale_color_manual(
      values = c("Without_Earmarks" = "#4ea324", "With_Earmarks" = "#b15712"),
      labels = c("Without_Earmarks" = "Without Earmarks", "With_Earmarks" = "With Earmarks"),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c("Without_Earmarks" = "solid", "With_Earmarks" = "solid"),
      labels = c("Without_Earmarks" = "Without Earmarks", "With_Earmarks" = "With Earmarks"),
      name = NULL
    ) +
    
    scale_y_continuous(
      labels = label_dollar(scale = 1e-6, suffix = "M"),
      limits = c(0, NA)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    
    labs(
      y = "Annual Repayments (in Millions)",
      x = "Year"
    ) +
    
    theme_minimal() +
    theme(
      text          = element_text(size = 25, family = "Lato"),
      legend.text   = element_text(size = 25, family = "Lato"),
      
      axis.text.x   = element_text(size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.x  = element_text(size = 25, margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.text.y   = element_text(size = 25, margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.y  = element_text(size = 25, margin = margin(t = 0, r = 5,  b = 0, l = 0)),
      
      legend.position    = c(0.02, 0.02),
      legend.justification = c(0, 0),
      legend.background  = element_rect(fill = "white", color = "grey90", size = 0.3),
      legend.margin      = margin(t = 4, r = 6, b = 4, l = 6),
      legend.box.spacing = unit(0, "pt"),
      
      panel.grid.major = element_line(color = "#E5E5E5"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
  
  # --- Save Chart ---
  # ggsave(
  #   filename = paste0("results-updates/section4/DW/state_repayment_graphs/",
  #                     state_abbr, "_repayment_chart_26.png"),
  #   plot   = state_plot,
  #   width  = 10,
  #   height = 8,
  #   units  = "in",
  #   dpi    = 600
  # )
  
  message("Saved chart and data for: ", state_abbr)
}

message("Done! Processed ", length(all_states), " states.")


