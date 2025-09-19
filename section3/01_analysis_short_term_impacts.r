# =============================================================================
# ANALYSIS FOR EARMARKS REPORT
#
# Author: Katy Hansen
# Date: September, 2025
#
# Description:
# This script is used to generate the analyses and visualizations for EPIC's 2025 Earmarks report.
# The code is organized sequentially to match the structure of the report.
#
# =============================================================================

#### 1. SETUP: LIBRARIES AND GLOBAL CONFIGURATION ####

# --- Load all required packages ---
# Ensure these packages are installed before running:
# install.packages(c("tidyverse", "readxl", "janitor", "scales", "sysfonts", "showtext", "ggchicklet"))

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(sysfonts)
library(showtext)
library(ggchicklet)

# --- Global Style Configuration ---
# Style from https://epic-ds-resources.s3.us-east-1.amazonaws.com/figure-style-guide.html#top
# Load "Lato" font for all plots to ensure consistency
font_add_google("Lato", "Lato")
showtext_auto()

# Define the report's color palette for consistent visualization
report_colors <- c(
  "Available to States" = "#4ea324", # Green
  "Earmarked"           = "#526489", # Blue-slate
  "Positive"            = "#4ea324", # Green
  "Negative"            = "#A95C52", # Red-brown
  "Minimum Impact"      = "#172f60", # Dark Blue
  "Maximum Impact"      = "#A95C52"  # Red-brown
)

# Define a consistent theme for all plots
report_theme <- theme_minimal(base_family = "Lato", base_size = 12) +
  theme(
    text = element_text(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "grey80"),
    axis.ticks.y = element_line(color = "grey80"),
    legend.position = "top"
  )


#### 2. DATA LOADING AND PREPARATION ####

# --- 2.1 State-Level Funding Data (For Sections 3.1, 3.4, 3.5) ---

# Define file path and base funding totals from the report
file_path <- "section3/raw_data/earmarks_data.xlsx" #see metadata tab for source info
fy_totals <- list(
  FY23 = list(dw = 1103040899, cw = 1620845642),
  FY24 = list(dw = 1103833905, cw = 1619480267)
)

# Load and combine state SRF allotment data
allotment_sheets <- list(
  dw_23 = read_excel(file_path, sheet = "DW_23_cap"),
  cw_23 = read_excel(file_path, sheet = "CW_23_cap"),
  dw_24 = read_excel(file_path, sheet = "DW_24_cap"),
  cw_24 = read_excel(file_path, sheet = "CW_24_cap")
)
allotments <- bind_rows(
  allotment_sheets$dw_23 %>% mutate(fy = "FY23", program = "DWSRF"),
  allotment_sheets$cw_23 %>% mutate(fy = "FY23", program = "CWSRF"),
  allotment_sheets$dw_24 %>% mutate(fy = "FY24", program = "DWSRF"),
  allotment_sheets$cw_24 %>% mutate(fy = "FY24", program = "CWSRF")
) %>%
  clean_names() %>%
  mutate(state = str_to_title(state))

# Load and process earmark data
earmark_sheets <- list(
  cds_23 = read_excel(file_path, sheet = "cds_23"),
  cds_24 = read_excel(file_path, sheet = "cds_24")
)
earmarks <- bind_rows(
  earmark_sheets$cds_23 %>% mutate(fy = "FY23"),
  earmark_sheets$cds_24 %>% mutate(fy = "FY24")
) %>%
  clean_names() %>%
  filter(account != "STAG—Other (CDS)") %>% #removes the few non-SRF CDS projects
  mutate(state = if_else(nchar(state) == 2, state.name[match(state, state.abb)], state)) %>%
  filter(state != "CNMI") %>%
  group_by(state, fy, account) %>%
  summarise(total_earmarks = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    program = case_when(
      str_detect(account, "Drinking Water") ~ "DWSRF",
      str_detect(account, "Clean Water") ~ "CWSRF"
    )
  )

# Calculate hypothetical allotments and merge with earmarks to get final dataset
srf_funding_data <- allotments %>%
  mutate(
    base_total = case_when(
      program == "DWSRF" & fy == "FY23" ~ fy_totals$FY23$dw,
      program == "CWSRF" & fy == "FY23" ~ fy_totals$FY23$cw,
      program == "DWSRF" & fy == "FY24" ~ fy_totals$FY24$dw,
      program == "CWSRF" & fy == "FY24" ~ fy_totals$FY24$cw
    ),
    hypothetical = pct * base_total
  ) %>%
  left_join(earmarks, by = c("state", "fy", "program")) %>%
  mutate(
    total_earmarks = replace_na(total_earmarks, 0),
    actual_total = allotment + total_earmarks,
    difference = actual_total - hypothetical
  )

# --- 2.2 Project-Level Demographic Data (For Section 3.2) ---
# NOTE: This section loads a pre-processed CSV file for efficiency.
# The original script for generating this is in file 00_prj_level_dem_data.
project_demographics_data <- read_csv("section3/raw_data/prj_level_dem_data.csv")


#### 3. ANALYSIS AND VISUALIZATION ####

# --- 3.0 SUMMARY: OVERALL FUNDING TRENDS (REPORT FIGURE 1) ---
message("Generating Figure 1: Overall Funding Trends...")

# Data is based on CW/DW calculations shown in table before figure; combined & weighted here
summary_plot_data <- tibble(
  program = c("CWSRF", "CWSRF", "DWSRF", "DWSRF"),
  category = c("Available to States", "Earmarked", "Available to States", "Earmarked"),
  y2021 = c(100, 0, 100, 0),
  y2022 = c(73, 27, 65, 35),
  y2023 = c(47, 53, 46, 54),
  y2024 = c(52, 48, 44, 56),
  y2025 = c(100, 0, 100, 0)
) %>%
  pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "percentage", names_prefix = "y") %>%
  mutate(
    weight = ifelse(program == "CWSRF", 1639, 1126) / (1639 + 1126),
    weighted_pct = percentage * weight
  ) %>%
  group_by(year, category) %>%
  summarise(total_pct = sum(weighted_pct), .groups = "drop") %>%
  mutate(category = factor(category, levels = c("Available to States", "Earmarked")))

plot_fig1 <- ggplot(summary_plot_data, aes(x = year, y = total_pct, fill = category)) +
  geom_chicklet(position = "dodge") +
  scale_fill_manual(values = report_colors) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Percent of Total Appropriation",
    fill = NULL
  ) +
  report_theme

print(plot_fig1)
ggsave("report_figure_1.png", plot = plot_fig1, width = 5, height = 3.5, dpi = 300)

# --- 3.1 STATE-LEVEL SHIFTS (REPORT FIGURE 2) ---
message("\nGenerating Figure 2: State-Level Net Funding Changes...")

state_shifts_data <- srf_funding_data %>%
  group_by(state) %>%
  summarise(total_difference = sum(difference, na.rm = TRUE), .groups = "drop") %>%
  filter(state != "Puerto Rico") %>%
  mutate(
    impact_type = ifelse(total_difference >= 0, "Positive", "Negative"),
    label_millions = paste0(round(total_difference / 1e6, 0), "M")
  ) %>%
  arrange(total_difference) %>%
  mutate(state = factor(state, levels = unique(state)))

plot_fig2 <- ggplot(state_shifts_data, aes(x = state, y = total_difference, fill = impact_type)) +
  geom_chicklet(width = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(label = label_millions, y = total_difference + ifelse(total_difference >= 0, 4e6, -4e6)),
    size = 11,  
    family = "Lato",
    color = "gray30",
    hjust = ifelse(state_shifts_data$total_difference >= 0, 0, 1)
  ) +
  scale_fill_manual(values = report_colors, guide = "none") +
  scale_y_continuous(labels = label_dollar(suffix = "M", scale = 1e-6), expand = expansion(mult = c(0.1, 0.1))) +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL
  ) +
  report_theme + 
  theme(
    axis.text.x = element_text(size = 22), # Controls the dollar amounts
    axis.text.y = element_text(size = 22)  # Controls the state names
  )


print(plot_fig2)
#ggsave("report_figure_2.png", plot = plot_fig2, width = 6, height = 8, dpi = 300)


# --- 3.2 PROJECT-LEVEL SHIFTS (REPORT FIGURES 3-6) ---
message("\nGenerating Figures 3-6: Project-Level Demographic Comparisons...")

# Reusable function to create the jitter plots
create_demographic_plot <- function(df, y_var, y_label) {
  ggplot(df, aes(x = type, y = .data[[y_var]], color = type)) +
    geom_jitter(width = 0.25, height = 0, alpha = 0.15, size = 2, shape = 16) +
    stat_summary(fun = "median", geom = "crossbar", width = 0.5, size = 0.6, color = "black") +
    facet_wrap(~program, labeller = as_labeller(c(DW = "Drinking Water", CW = "Clean Water"))) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c("earmark" = "#172f60", "srf" = "#4ea324"), guide = "none") +
    scale_x_discrete(labels = c("earmark" = "Earmarks", "srf" = "State-allocated")) +
    labs(y = y_label, x = NULL) +
    report_theme +
    theme(strip.text = element_text(face = "bold"))
}

# Figure 3: Population
plot_fig3 <- create_demographic_plot(
  project_demographics_data %>% filter(population < 50000, !is.na(population)),
  "population",
  "Population (under 50k)"
)
print(plot_fig3)
#ggsave("report_figure_3.png", plot = plot_fig3, width = 8, height = 6, dpi = 300)

# Figure 4: Median Household Income
plot_fig4 <- create_demographic_plot(
  project_demographics_data %>% filter(mhi < 150000, !is.na(mhi)),
  "mhi",
  "Median Household Income ($)"
)
print(plot_fig4)
#ggsave("report_figure_4.png", plot = plot_fig4, width = 8, height = 6, dpi = 300)

# Figure 5: Poverty Rate
plot_fig5 <- create_demographic_plot(
  project_demographics_data %>% filter(pct_below_poverty < 50, !is.na(pct_below_poverty)),
  "pct_below_poverty",
  "Percent Below Poverty Level"
)
print(plot_fig5)
#ggsave("report_figure_5.png", plot = plot_fig5, width = 8, height = 6, dpi = 300)

# Figure 6: People of Color
plot_fig6 <- create_demographic_plot(
  project_demographics_data %>% filter(!is.na(pct_poc)),
  "pct_poc",
  "Percent People of Color"
)
print(plot_fig6)
#ggsave("report_figure_6.png", plot = plot_fig6, width = 8, height = 6, dpi = 300)


# --- 3.4 PRINCIPAL FORGIVENESS (PF) IMPACT (REPORT FIGURE 7) ---
message("\nGenerating Figure 7: Impact on Principal Forgiveness...")

# Define PF set-aside rates
pf_rates <- list(cwsrf_min = 0.10, cwsrf_max = 0.40, dwsrf_min = 0.12, dwsrf_max = 0.49)

# Calculate PF impact
pf_impact_data <- srf_funding_data %>%
  mutate(
    min_pf_impact = case_when(
      program == "CWSRF" ~ (allotment * pf_rates$cwsrf_min) - (hypothetical * pf_rates$cwsrf_min),
      program == "DWSRF" ~ (allotment * pf_rates$dwsrf_min) - (hypothetical * pf_rates$dwsrf_min)
    ),
    max_pf_impact = case_when(
      program == "CWSRF" ~ (allotment * pf_rates$cwsrf_max) - (hypothetical * pf_rates$cwsrf_max),
      program == "DWSRF" ~ (allotment * pf_rates$dwsrf_max) - (hypothetical * pf_rates$dwsrf_max)
    )
  ) %>%
  group_by(state) %>%
  summarise(
    total_min_pf_impact = sum(min_pf_impact, na.rm = TRUE),
    total_max_pf_impact = sum(max_pf_impact, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(total_min_pf_impact, total_max_pf_impact),
    names_to = "impact_type",
    values_to = "impact_value"
  ) %>%
  mutate(
    impact_type = recode(impact_type,
                         "total_min_pf_impact" = "Minimum Impact",
                         "total_max_pf_impact" = "Maximum Impact")
  )

plot_fig7 <- ggplot(pf_impact_data, aes(x = reorder(state, impact_value), y = impact_value / 1e6, fill = impact_type)) +
  geom_chicklet(position = "dodge", alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = report_colors) +
  scale_y_continuous(labels = label_dollar(suffix = "M")) +
  labs(
    x = NULL,
    y = "Change in Available Principal Forgiveness ($ Millions)",
    fill = "Impact Scenario"
  ) +
  report_theme +
  theme(panel.grid.major.y = element_blank())

print(plot_fig7)
#ggsave("report_figure_7.png", plot = plot_fig7, width = 8, height = 10, dpi = 300)

# =============================================================================
# END OF SCRIPT
# =============================================================================