library(ggplot2)
library(patchwork)

## Constants -----
bucket <- "water-team-data"

## Functions -----
### Save locally and to AWS ----
save_to_s3 <- function(object_to_save, type = c("data", "viz"), width_in, height_in, filename_wo_extension, bucket) {
  if (type == "data") {
   
  filename <- paste0(filename_wo_extension, ".csv")
  
  # save dataframe as csv
  readr::write_csv(object_to_save, filename)  

  } else {
  filename <- paste0(filename_wo_extension, ".png")
  
  # save plot as png and html
  ggplot2::ggsave(
    filename = filename,
    plot   = object_to_save,
    width  = width_in,
    height = height_in,
    units  = "in",
    dpi    = 600
  )
  
  aws.s3::put_object(
    file   = filename,
    acl    = "public-read",
    object = paste0("srf-earmarks/2026-earmarks/data_viz/", filename),
    bucket = bucket
  )
  

  }
  
  aws.s3::put_object(
    file   = filename,
    acl    = "public-read",
    object = paste0("srf-earmarks/2026-earmarks/data_viz/", filename),
    bucket = bucket
  )

  message("Uploaded: ", filename)
  }

## Load data -----
FY25_CW <- readr::read_csv("FY25_allotment_CW_SRF_Base_IIJA_Gen_Supp_EC.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(
    cwsrf_base = 1.2*cwsrf_base, #account for state match (20%)
    iija_cwsrf_general_supplemental = 1.2* iija_cwsrf_general_supplemental, #account for state match (20%),
    fy25_total = cwsrf_base + iija_cwsrf_general_supplemental + iija_cwsrf_emerging_contaminants
  ) |>
  dplyr::select(state, fy25_total) |>
  dplyr::mutate(fy25_total = as.numeric(fy25_total))

FY25_DW <- readr::read_csv("FY25_allotment_DW_SRF_Base_IIJA_Gen_Supp_EC_LSLR.csv") |>
  janitor::clean_names() |>
  dplyr::mutate(
    dwsrf_base = 1.2*dwsrf_base, #account for state match (20%)
    iija_dwsrf_general_supplemental = 1.2* iija_dwsrf_general_supplemental, #account for state match (20%),
    fy25_total = dwsrf_base + iija_dwsrf_general_supplemental + iija_dwsrf_emerging_contaminants + iija_dwsrf_lslr
  ) |>
  dplyr::select(state, fy25_total) |>
  dplyr::mutate(fy25_total = as.numeric(fy25_total)) 


funding_data <- readr::read_csv("srf_funding_data_update_fy26_v2.csv")

## Bind data -----
projection_data <- dplyr::bind_rows(FY25_CW, FY25_DW) |>
  dplyr::group_by(state) |>
  dplyr::summarise(
    value = sum(fy25_total, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    category = "total_allotment" #baseline scenario (bar 1)
  ) |>
  dplyr::bind_rows(
    funding_data |>
      dplyr::filter(state %in% state.name) |>
      dplyr::group_by(state) |>
      dplyr::summarise(
        projected_no_earmark = 1.2*sum(allotment[fy == "FY25"], na.rm = TRUE), # correct, allotment column corresponds base funding, spot check with 2025 table
        projected_earmark    = 1.2*sum(allotment[fy == "FY26"], na.rm = TRUE) # allotment column corresponds base funding
      )|>
      dplyr::ungroup() |>
      tidyr::pivot_longer(-state, names_to = "category") 
  ) |>
  dplyr::mutate(
    grouping = ifelse(state %in% state.name[1:25], "set1", "set2"),
    state_abb = state.abb[match(state, state.name)],
    category  = dplyr::case_when(
      category == "projected_earmark" ~ "FFY27, earmarks scenario",
      category == "projected_no_earmark" ~ "FFY27, no-earmarks scenario",
      category == "total_allotment" ~ "FFY26"
    ),
    category = forcats::fct_relevel(category, c("FFY26", "FFY27, no-earmarks scenario", "FFY27, earmarks scenario"))
  ) 
  
## Summarize data -----
projection_data_summary <- projection_data |>
  dplyr::ungroup() |>
  dplyr::select(state_abb, category, value) |>
  dplyr::mutate(category = dplyr::case_when(
    grepl("FY26", category) ~ "base",
    grepl("no", category) ~ "no earmarks scenario",
    .default = "earmarks scenario"
  )) |>
  tidyr::pivot_wider(id_cols = "state_abb", names_from = "category", values_from = "value") |>
  dplyr::mutate(
    fy27_projected_perc_reduction_no_earmarks = round(100*(base - `no earmarks scenario`)/base, 2),
    fy27_projected_perc_reduction_earmarks = round(100*(base - `earmarks scenario`)/base, 2)
  ) |>
  dplyr::rename(
    fy25_allotment = base,
    fy27_no_earmarks = `no earmarks scenario`,
    fy27_earmarks = `earmarks scenario`
  )

readr::write_csv(projection_data_summary, "~/Desktop/2026_03_17_srf_earmarks_perc_reduction.csv")

## Appendix full viz -----
# Objective: Illustrate post-IIJA SRF funding cliff, earmarks make this worse. 
# manual draft: https://drive.google.com/file/d/1t1rY2Ucpj0ufuQhRum4wW7a2qouAIlFQ/view?usp=sharing

# Facet by state with 25 states top, 25 bottom
# * FY25 allotment = FFY25 DW Base, Gen Supp, and EC allotments + FFY25 LSLR allotments + FFY25 CW IIJA allotments (Gen Supp, EC) + FFY26 CW Base allotment. 
# * Projected FY27 allotment, no-earmarks scenario = FFY25 CW Base Allotment + FFY25 DW Base Allotment 
# * Projected FY27 allotment, earmarks scenario = FFY24 CW Base Allotment + FFY24 DW Base Allotment 

projection_plot <- ggplot2::ggplot(data = projection_data, ggplot2::aes(x=state_abb, y=value, fill=category, text=state, group=category)) + 
    ggchicklet::geom_chicklet(alpha = 0.8, position = "dodge") +
    ggplot2::facet_wrap(grouping~state, nrow = 2, scales = "free_x") +
    ggplot2::scale_fill_manual(
      values = c(
        "FFY26" = "#B15712",
        "FFY27, no-earmarks scenario" = "#4EA324",
        "FFY27, earmarks scenario" =  "#1054A8"
      ), name = NULL) + 
    ggplot2::labs(
      x="", 
      y="",
      title= stringr::str_wrap("Expected Change in SRF funding from FFY26 to FFY27, Earmarks and No-Earmarks Scenarios", width= 60), #might change
      caption = "Note: FFY27 projections reflect base appropriations only. IIJA supplemental funding is not expected to continue beyond FFY26."
         ) +
 # --- Scales and Aesthetics ---
    ggplot2::scale_y_continuous(labels = scales::label_currency(scale_cut = scales::cut_short_scale()), breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::theme_minimal(base_size = 16, base_family = "Lato") +
    ggplot2::theme(
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5), color = "#1A1A1A", lineheight = 1),
      plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
      plot.caption = element_text(size = 12, color = "#666666", margin = margin(t = 15), hjust = 0),
      axis.title = element_text(size = 12, color = "#333333"),
      axis.text.y = element_text(size = 12, color = "#444444"),
      axis.text.x = element_text(size = 12, color = "#444444"),
      legend.text = element_text(size = 12)
    ) +
    ggplot2::theme(
      # Grid and background styling
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.25, linetype = "solid"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10),
      # Legend styling
      # legend.position = "bottom",
      legend.direction = "vertical",
      legend.position = c(0.7, 0.99),
      legend.justification = c(0, 1),
      # legend.justification = "center",
      legend.box = "vertical",
      legend.spacing.y = unit(0.005, "cm"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = "lightgrey", linewidth = 0.3),
      legend.margin = margin(2, 4, 2, 4),      # padding between text and legend background
      legend.box.margin = margin(0, 0, 0, 0)   # padding outside the legend box border
    ) +
    ggplot2::theme(strip.text = element_blank()) +
    # --- Legend Key Customization ---
    ggplot2::guides(
      fill = guide_legend(order = 1)
    )

save_to_s3(projection_plot, type = "viz", filename_wo_extension = "SRF_allotment_projection_SFY25_SFY27",width_in = 11, height_in =8,  bucket = bucket)  


## Selected states viz -----
decreasing_fy25 <- projection_data_summary |>
  dplyr::arrange(dplyr::desc(fy25_allotment)) |>
  dplyr::pull(state_abb)

selected_states <- c(decreasing_fy25[1:5], decreasing_fy25[46:50])

p_top <- projection_data |>
  dplyr::mutate(
    state_abb = forcats::fct_relevel(state_abb, selected_states)
  ) |>
  dplyr::filter(state_abb %in% selected_states[1:5]) |>
  ggplot2::ggplot(ggplot2::aes(x=state, y=value, fill=category)) +
  ggchicklet::geom_chicklet(
    width =3,
    position = ggplot2::position_dodge(width = 0.9)
  ) +
  ggplot2::facet_grid(~state_abb, scales = "free_x") +
  ggplot2::scale_y_continuous(
    labels = scales::label_currency(scale_cut = scales::cut_short_scale()), 
    breaks = seq(from = 0, to = 1e9, by = 200e6),
    limits = c(0, 1e9)
  ) +
  ggplot2::labs(subtitle = "States that Receive the Largest SRF Allotments")
  
p_bottom <- projection_data |>
  dplyr::mutate(
    state_abb = forcats::fct_relevel(state_abb, selected_states)
  ) |>
  dplyr::filter(state_abb %in% selected_states[6:10]) |>
  ggplot2::ggplot(ggplot2::aes(x=state, y=value, fill=category)) +
  ggchicklet::geom_chicklet(
    width =3,
    position = ggplot2::position_dodge(width = 0.9)
  ) +
  ggplot2::facet_grid(~state_abb, scales = "free_x") +
  ggplot2::scale_y_continuous(
    labels = scales::label_currency(scale_cut = scales::cut_short_scale()), 
    breaks = seq(from = 0, to = 1.2e8, by = 2e7),
    limits = c(0, 1.2e8)
  ) +
  ggplot2::labs(subtitle = "States that Receive Minimum SRF Allotments")

selected_states <- p_top / p_bottom + 
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = stringr::str_wrap("Expected Change in SRF funding from FFY26 to FFY27, Earmarks and No-Earmarks Scenarios", width = 60),
    caption = "Note: FFY27 projections reflect base appropriations only. IIJA supplemental funding is not expected to continue beyond FFY26."
  ) &
  ggplot2::scale_fill_manual(
    values = c(
      "FFY26" = "#B15712",
      "FFY27, no-earmarks scenario" = "#4EA324",
      "FFY27, earmarks scenario" = "#1054A8"
    ), name = NULL) &
  ggplot2::labs(x = "", y = "") &
  ggplot2::theme_minimal(base_size = 16, base_family = "Lato") +
  ggplot2::theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5), color = "#1A1A1A", lineheight = 1),
    plot.caption = element_text(size = 14, color = "#666666", margin = margin(t = 15), hjust = 0),
    axis.text.y = element_text(size = 14, color = "#444444"),
    axis.text.x = element_text(size = 14, color = "#444444"),
    legend.text = element_text(size = 14),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.25),
    panel.grid.minor.y = element_line(color = "#E5E5E5", linewidth = 0.25),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(5, 5, 5, 5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = "lightgrey", linewidth = 0.3),
    legend.margin = margin(2, 4, 2, 4),
    strip.text = element_blank()
  ) 
  
save_to_s3(selected_states, type = "viz", width_in = 11, height_in = 8, filename_wo_extension = "srf_cliff_selected_states", bucket = bucket) 
