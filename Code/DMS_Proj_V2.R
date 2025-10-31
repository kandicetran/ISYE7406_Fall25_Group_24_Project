############################################################
# SF Crime: Do time periods affect classifications & trends?
# Methods: Time-series decomposition, Rolling-window clustering,
#          Heatmaps (date, season, day, hour)
############################################################

## ---------------------- USER SETTINGS ----------------------
INPUT_FILE <- "SF_Police_Reports_2003_to_2018_cleaned.csv"
OUTPUT_DIR <- file.path("outputs", format(Sys.time(), "%Y-%m-%d_%H%M%S"))

# Decomposition (top categories to decompose individually)
TOP_N_CATEGORIES_FOR_DECOMP <- 3

# Rolling-window clustering parameters (in months)
WINDOW_SIZE_MONTHS <- 12   # initial window length
WINDOW_STEP_MONTHS <- 1    # step between windows
K_CLUSTERS         <- 4    # initial k for k-means

set.seed(1234)

## --------------- PACKAGE SETUP / AUTO-INSTALL ---------------
needed_pkgs <- c(
  "tidyverse", "lubridate", "janitor", "scales", "viridis",
  "ggthemes", "zoo", "slider", "forecast", "forcats"
)
to_install <- needed_pkgs[!suppressWarnings(sapply(needed_pkgs, requireNamespace, quietly = TRUE))]
if (length(to_install) > 0) {
  message("Installing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(scales)
  library(viridis)
  library(ggthemes)
  library(zoo)
  library(slider)
  library(forecast)  # for mstl/stl utilities
  library(forcats)
})

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
message("Outputs will be saved to: ", normalizePath(OUTPUT_DIR))


## ------------------------- LOAD DATA ------------------------
message("Loading data from: ", normalizePath(INPUT_FILE))
raw <- readr::read_csv(
  INPUT_FILE,
  col_types = cols(.default = col_guess()),
  progress = FALSE
)

# Standardize column names we will use
df <- raw %>% clean_names()


library(dplyr)
library(stringr)
library(forcats)

# Normalize Category text to reduce mismatches
df <- df %>%
  mutate(
    category = as.character(category),
    category = str_squish(category),        # collapse extra spaces
    category = toupper(category)            # uppercase to match your lists
  )

high_set <- c(
  "KIDNAPPING","SEX OFFENSES, FORCIBLE","WEAPON LAWS","ARSON","ROBBERY",
  "ASSAULT","BURGLARY","VEHICLE THEFT","DISORDERLY CONDUCT","SEX OFFENSES, NON FORCIBLE"
)

low_set <- c(
  "SUSPICIOUS OCC","FORGERY/COUNTERFEITING","OTHER OFFENSES","STOLEN PROPERTY",
  "TRESPASS","MISSING PERSON","WARRANTS","DRIVING UNDER THE INFLUENCE","TREA",
  "EMBEZZLEMENT","DRUNKENNESS","SECONDARY CODES","RECOVERED VEHICLE",
  "LARCENY/THEFT","SUICIDE","FRAUD","PORNOGRAPHY/OBSCENE MAT","LOITERING",
  "BAD CHECKS","LIQUOR LAWS","GAMBLING","NON-CRIMINAL",
  "BRIBERY","VANDALISM","DRUG/NARCOTIC","PROSTITUTION","EXTORTION"
)

# Overwrite category with High/Low/Other
df <- df %>%
  mutate(
    category = case_when(
      category %in% high_set ~ "High",
      category %in% low_set  ~ "Low",
      TRUE                   ~ "Other"   # anything not in either list
    ),
    category = factor(category, levels = c("High","Low","Other"))
  )


# Basic validation / coercion
if (!all(c("year","month") %in% names(df))) {
  stop("Dataset must include 'Year' and 'Month' columns.")
}
df <- df %>%
  mutate(
    year        = as.integer(year),
    month       = as.integer(month),
    hour        = if ("hour" %in% names(.)) as.integer(hour) else NA_integer_,
    category    = if ("category" %in% names(.)) as.factor(category) else factor(NA),
    pd_district = if ("pd_district" %in% names(.)) as.factor(pd_district) else factor(NA),
    day_of_week = if ("day_of_week" %in% names(.)) as.character(day_of_week) else NA_character_,
    season      = if ("season" %in% names(.)) as.character(season) else NA_character_
  ) %>%
  # Construct a Year-Month Date (1st day of month)
  mutate(year_month = suppressWarnings(ymd(sprintf("%04d-%02d-01", year, month)))) %>%
  filter(!is.na(year_month))

# Harmonize DayOfWeek (ensure Monday..Sunday)
dow_levels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
df <- df %>%
  mutate(
    day_of_week = case_when(
      day_of_week %in% c("Mon","MONDAY","mon","monday","Monday") ~ "Monday",
      day_of_week %in% c("Tue","TUESDAY","tue","tuesday","Tuesday") ~ "Tuesday",
      day_of_week %in% c("Wed","WEDNESDAY","wed","wednesday","Wednesday") ~ "Wednesday",
      day_of_week %in% c("Thu","THURSDAY","thu","thursday","Thursday") ~ "Thursday",
      day_of_week %in% c("Fri","FRIDAY","fri","friday","Friday") ~ "Friday",
      day_of_week %in% c("Sat","SATURDAY","sat","saturday","Saturday") ~ "Saturday",
      day_of_week %in% c("Sun","SUNDAY","sun","sunday","Sunday") ~ "Sunday",
      TRUE ~ day_of_week
    ),
    wday = factor(day_of_week, levels = dow_levels, ordered = TRUE),
    hour = ifelse(is.na(hour) | hour < 0 | hour > 23, NA_integer_, hour),
    season = case_when(
      season %in% c("Winter","winter","WINTER") ~ "Winter",
      season %in% c("Spring","spring","SPRING") ~ "Spring",
      season %in% c("Summer","summer","SUMMER") ~ "Summer",
      season %in% c("Fall","Autumn","fall","autumn","FALL","AUTUMN") ~ "Fall",
      TRUE ~ season
    ),
    season = factor(season, levels = c("Winter","Spring","Summer","Fall"), ordered = TRUE),
    month_lab = factor(month.abb[month], levels = month.abb, ordered = TRUE)
  )

message("Records after cleaning: ", nrow(df))
yr_range <- range(df$year, na.rm = TRUE)
message("Year range: ", yr_range[1], "–", yr_range[2])
if (any(is.na(df$pd_district))) message("Note: Some rows have missing PdDistrict.")
if (any(is.na(df$category)))    message("Note: Some rows have missing Category.")

# Data dictionary (basic)
readr::write_csv(
  tibble::tibble(
    column = names(df),
    class  = sapply(df, function(x) paste(class(x), collapse = ","))
  ),
  file.path(OUTPUT_DIR, "data_dictionary.csv")
)


## -------------------- BASIC SUMMARIES -----------------------
top_cats  <- df %>% count(category, sort = TRUE, name = "count")
top_dists <- df %>% count(pd_district, sort = TRUE, name = "count")
readr::write_csv(top_cats,  file.path(OUTPUT_DIR, "top_categories.csv"))
readr::write_csv(top_dists, file.path(OUTPUT_DIR, "top_districts.csv"))

# Month-by-year plot (context)
monthly_totals <- df %>%
  count(year, month_lab, name = "count") %>%
  group_by(year) %>% mutate(total_year = sum(count)) %>% ungroup()

p_monthly <- ggplot(
  data = monthly_totals,
  mapping = aes(x = month_lab, y = count, group = year, color = factor(year))
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_viridis_d(end = 0.9, option = "D") +
  labs(title = "Monthly Crime Totals by Year",
       x = "Month", y = "Incidents", color = "Year") +
  theme_minimal(base_size = 12)

ggsave(file.path(OUTPUT_DIR, "monthly_totals_by_year.png"), p_monthly, width = 10, height = 6, dpi = 150)


## --------------- TIME-SERIES DECOMPOSITION -----------------
# Aggregate monthly counts; fill missing months
monthly_counts <- df %>%
  count(year_month, name = "count") %>%
  arrange(year_month)

all_months <- tibble(year_month = seq(min(monthly_counts$year_month), max(monthly_counts$year_month), by = "month"))
monthly_filled <- all_months %>%
  left_join(monthly_counts, by = "year_month") %>%
  mutate(count = replace_na(count, 0L)) %>%
  arrange(year_month)

# Build ts object with frequency=12 (monthly)
start_year <- year(min(monthly_filled$year_month))
start_month <- month(min(monthly_filled$year_month))
ts_monthly <- ts(monthly_filled$count, start = c(start_year, start_month), frequency = 12)

# STL decomposition (robust)
decomp <- stats::stl(ts_monthly, s.window = "periodic", robust = TRUE)

# Plot & export
png(file.path(OUTPUT_DIR, "time_series_decomposition_monthly.png"), width = 1200, height = 800, res = 140)
plot(decomp, main = "Monthly Crime: STL Decomposition (Seasonal=12)")
dev.off()

# Export decomposition components to CSV with dates
decomp_df <- tibble(
  year_month = monthly_filled$year_month,
  observed   = as.numeric(decomp$time.series[, "remainder"] + decomp$time.series[, "trend"] + decomp$time.series[, "seasonal"]),
  trend      = as.numeric(decomp$time.series[, "trend"]),
  seasonal   = as.numeric(decomp$time.series[, "seasonal"]),
  remainder  = as.numeric(decomp$time.series[, "remainder"])
)
readr::write_csv(decomp_df, file.path(OUTPUT_DIR, "decomposition_components_monthly.csv"))

# Decompose top-N categories monthly (only applicable if we move from high/low)
if (!is.null(TOP_N_CATEGORIES_FOR_DECOMP) && TOP_N_CATEGORIES_FOR_DECOMP > 0) {
  top_cat_names <- top_cats$category %>% head(TOP_N_CATEGORIES_FOR_DECOMP) %>% as.character()
  for (cat in top_cat_names) {
    cat_month <- df %>%
      filter(category == cat) %>%
      count(year_month, name = "count") %>%
      right_join(all_months, by = "year_month") %>%
      mutate(count = replace_na(count, 0L)) %>%
      arrange(year_month)
    
    ts_cat <- ts(cat_month$count, start = c(start_year, start_month), frequency = 12)
    decomp_cat <- stats::stl(ts_cat, s.window = "periodic", robust = TRUE)
    
    out_png <- file.path(OUTPUT_DIR, paste0("decomposition_monthly_category_", gsub("[^A-Za-z0-9]+","_", cat), ".png"))
    png(out_png, width = 1200, height = 800, res = 140)
    plot(decomp_cat, main = paste0("Monthly Crimes (", cat, "): STL Decomposition"))
    dev.off()
  }
}

message("Decomposition exported: See 'time_series_decomposition_monthly.png' (trend & seasonal components).")


## -------- ROLLING-WINDOW CLUSTERING OVER TIME (MONTHS) -----

# Helper: rolling windows on monthly index
ym_grid <- df %>%
  distinct(year_month) %>%
  arrange(year_month) %>%
  pull(year_month)

attempts <- tibble(
  window_months = c(WINDOW_SIZE_MONTHS, 9, 6, 3),
  k             = c(K_CLUSTERS,        4, 3, 2),
  min_rows      = c(1000,              800, 500, 250) # minimum incidents in window
)

perform_rw_clustering <- function(df, ym_grid, window_months, step_months, k, min_rows) {
  if (length(ym_grid) < window_months) return(NULL)
  
  # Define window starts/ends
  idx_starts <- seq(1, length(ym_grid) - window_months + 1, by = step_months)
  idx_ends   <- idx_starts + window_months - 1
  
  results <- list()
  
  for (i in seq_along(idx_starts)) {
    ws <- ym_grid[idx_starts[i]]
    we <- ym_grid[idx_ends[i]]
    slice <- df %>% filter(year_month >= ws, year_month <= we)
    
    if (nrow(slice) < min_rows) next
    
    # District x Category proportions
    mat <- slice %>%
      filter(!is.na(pd_district), !is.na(category)) %>%
      count(pd_district, category, name = "count") %>%
      group_by(pd_district) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup() %>%
      select(pd_district, category, prop) %>%
      pivot_wider(names_from = category, values_from = prop, values_fill = 0) %>%
      arrange(pd_district)
    
    if (nrow(mat) < k) next  # need at least k districts
    
    rownames_mat <- mat$pd_district
    X <- mat %>% select(-pd_district) %>% as.matrix()
    X_scaled <- scale(X)
    
    km <- kmeans(X_scaled, centers = k, nstart = 50, iter.max = 50)
    
    # Use window midpoint for plotting
    mid_idx <- floor((idx_starts[i] + idx_ends[i]) / 2)
    mid_ym  <- ym_grid[mid_idx]
    
    results[[length(results) + 1]] <- tibble(
      window_start = ws,
      window_end   = we,
      window_mid   = mid_ym,
      pd_district  = rownames_mat,
      cluster      = factor(km$cluster)
    )
  }
  
  if (length(results) == 0) return(NULL)
  bind_rows(results) %>% arrange(window_mid, pd_district)
}

clust_df <- NULL
if (!all(is.na(df$pd_district))) {
  for (a in seq_len(nrow(attempts))) {
    message("Trying clustering with window=", attempts$window_months[a],
            " mo; step=", WINDOW_STEP_MONTHS,
            " mo; k=", attempts$k[a], "; min_rows=", attempts$min_rows[a])
    clust_df <- perform_rw_clustering(
      df, ym_grid,
      window_months = attempts$window_months[a],
      step_months   = WINDOW_STEP_MONTHS,
      k             = attempts$k[a],
      min_rows      = attempts$min_rows[a]
    )
    if (!is.null(clust_df)) {
      message("Success with window=", attempts$window_months[a], " months; k=", attempts$k[a])
      break
    }
  }
  
  if (!is.null(clust_df)) {
    readr::write_csv(clust_df, file.path(OUTPUT_DIR, "rolling_window_clusters_by_district.csv"))
    
    # Order districts by median cluster label to stabilize y-axis
    dist_order <- clust_df %>%
      group_by(pd_district) %>%
      summarize(median_cluster = median(as.integer(cluster)), .groups = "drop") %>%
      arrange(median_cluster, pd_district) %>%
      pull(pd_district)
    
    p_clusters <- ggplot(
      data = clust_df %>% mutate(pd_district = factor(pd_district, levels = dist_order)),
      mapping = aes(x = window_mid, y = pd_district, fill = cluster)
    ) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_viridis_d(option = "D", end = 0.9) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      labs(
        title = paste0("Rolling-Window Clustering of Police Districts (", WINDOW_STEP_MONTHS, " mo step)"),
        subtitle = "Clusters based on CATEGORY mix within each window",
        x = "Window Midpoint (Year-Month)",
        y = "Police District",
        fill = "Cluster"
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    
    ggsave(file.path(OUTPUT_DIR, "rolling_window_clusters_heatmap.png"), p_clusters, width = 11, height = 7, dpi = 160)
  } else {
    message("Could not perform rolling-window clustering after adaptive attempts. ",
            "Consider smaller window_months, fewer clusters, or ensure PdDistrict/Category coverage.")
  }
} else {
  message("PdDistrict mostly NA; skipping rolling-window clustering.")
}


## ------------------------- HEATMAPS -------------------------
short_num <- label_number(scale_cut = cut_short_scale())

# 1) Heatmap: Crimes by Year and Month
ym_counts <- df %>% count(year, month_lab, name = "count")
ym_heat <- ggplot(
  data = ym_counts,
  mapping = aes(x = month_lab, y = factor(year), fill = count)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis(option = "C", trans = "sqrt", labels = short_num) +
  labs(
    title = "Heatmap: Crimes by Year and Month",
    x = "Month",
    y = "Year",
    fill = "Incidents"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUTPUT_DIR, "heatmap_year_month.png"), ym_heat, width = 9, height = 6, dpi = 160)

# 2) Heatmap: Day-of-Week × Hour
dow_hour_counts <- df %>%
  filter(!is.na(wday), !is.na(hour)) %>%
  count(wday, hour, name = "count")
dow_hour <- ggplot(
  data = dow_hour_counts,
  mapping = aes(x = hour, y = wday, fill = count)
) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis(option = "C", trans = "sqrt", labels = short_num) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "Heatmap: Crimes by Day-of-Week and Hour",
    x = "Hour of Day",
    y = "Day of Week",
    fill = "Incidents"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUTPUT_DIR, "heatmap_day_hour.png"), dow_hour, width = 10, height = 6, dpi = 160)

# 3) Heatmap: Season × Hour
season_hour_counts <- df %>%
  filter(!is.na(season), !is.na(hour)) %>%
  count(season, hour, name = "count")
season_hour <- ggplot(
  data = season_hour_counts,
  mapping = aes(x = hour, y = season, fill = count)
) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis(option = "C", trans = "sqrt", labels = short_num) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "Heatmap: Crimes by Season and Hour",
    x = "Hour of Day",
    y = "Season",
    fill = "Incidents"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUTPUT_DIR, "heatmap_season_hour.png"), season_hour, width = 10, height = 5.5, dpi = 160)

# 4) District × Hour (normalized within district)
if (!all(is.na(df$pd_district))) {
  # Aggregate and normalize
  dist_hour <- df %>%
    filter(!is.na(pd_district), !is.na(hour)) %>%
    count(pd_district, hour, name = "count") %>%
    group_by(pd_district) %>%
    mutate(prop = count / sum(count)) %>%
    ungroup()
  
  # Precompute a stable district order (by total incidents)
  dist_order <- dist_hour %>%
    group_by(pd_district) %>%
    summarize(total = sum(count), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(pd_district)
  
  # Plot using the precomputed order
  p_dist_hour <- ggplot(
    data = dist_hour %>% mutate(pd_district = factor(pd_district, levels = dist_order)),
    mapping = aes(x = hour, y = pd_district, fill = prop)
  ) +
    geom_tile() +
    scale_fill_viridis(option = "C", labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(0, 23, by = 2)) +
    labs(
      title = "Normalized Heatmap: Hourly Crime Distribution by District",
      x = "Hour",
      y = "Police District",
      fill = "Within-District Share"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    filename = file.path(OUTPUT_DIR, "heatmap_district_hour_normalized.png"),
    plot     = p_dist_hour,
    width    = 10, height = 7, dpi = 160
  )
}


## ----------------------- PEAK ACTIVITY ----------------------
# Peak hours
hour_totals <- df %>% filter(!is.na(hour)) %>% count(hour, sort = TRUE, name = "count")
top_hours   <- hour_totals %>% slice_max(count, n = 5)

# Peak days of week
dow_totals  <- df %>% filter(!is.na(wday)) %>% count(wday, sort = TRUE, name = "count")
top_days    <- dow_totals %>% slice_max(count, n = 5)

# Peak year-months
ym_totals <- df %>%
  count(year_month, sort = TRUE, name = "count") %>%
  arrange(desc(count))
top_year_months <- ym_totals %>% slice_max(count, n = 10)

readr::write_csv(hour_totals,       file.path(OUTPUT_DIR, "peak_hours_all.csv"))
readr::write_csv(dow_totals,        file.path(OUTPUT_DIR, "peak_days_of_week.csv"))
readr::write_csv(top_year_months,   file.path(OUTPUT_DIR, "peak_year_months.csv"))

message("\n=== Peak Activity Summary (printed) ===")
message("Top hours:")
print(top_hours)
message("\nTop days of week:")
print(top_days)
message("\nTop year-months:")
print(top_year_months %>% head(10))

message("\nAnalysis complete. Check: ", normalizePath(OUTPUT_DIR))
