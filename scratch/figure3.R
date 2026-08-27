library(tidyverse)
source("R/moving-average.R")

# Read in the data
bq1 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca1-Bisley.csv")
bq2 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca2-Bisley.csv")
bq3 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca3-Bisley.csv")
mpr <- read_csv("data/knb-lter-luq.20.4923064/RioMameyesPuenteRoto.csv")

# Calculate moving average for each site, populating result tables
bq1_result <- moving_average(site = bq1)
bq2_result <- moving_average(site = bq2)
bq3_result <- moving_average(site = bq3)
mpr_result <- moving_average(site = mpr)

# Combine data frames
fig3 <- bind_rows(bq1_result, bq2_result, bq3_result, mpr_result)

# Pivot longer
fig3_long <- fig3 |>
  pivot_longer(
    cols = c("NO3-N", "K", "Mg", "Ca", "NH4-N"),
    names_to = "nutrients",
    values_to = "concentration"
  )

# Plot figure 3
ggplot(
  data = fig3_long,
  mapping = aes(x = window_start, y = concentration, linetype = Sample_ID)
) +
  geom_line() +
  geom_vline(xintercept = ymd("1989-09-18"), linetype = 2) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "inside",
    legend.justification = c("right", "top"),
    legend.key.height = unit(.4, "cm"),
    legend.key.width = unit(.5, "cm"),
    strip.placement = "outside"
  ) +
  labs(x = "Years", y = "") +
  scale_x_date(sec.axis = dup_axis()) +
  facet_grid(nutrients ~ ., scales = "free_y", switch = "y")

# Previous attempts ------------------------------------------------------

# Remove unecessary columns from bq1
bq1_filtered <- bq1 |>
  select("Sample_ID", "Sample_Date", "NO3-N", "K", "Mg", "Ca", "NH4-N")
glimpse(bq1_filtered)

# Create tibble with 9wk smoothed windows
bq1_smoothed <- tibble(
  window_start = seq(ymd("1988-01-01"), ymd("1995-01-01"), by = "9 weeks"),
  Sample_ID = "BQ1",
  "NO3-N" = NA,
  K = NA,
  Mg = NA,
  Ca = NA,
  "NH4-N" = NA
)
glimpse(bq1_smoothed)

# Add 9 wk moving average to the bq1_smoothed data frame
for (i in 1:nrow(bq1_smoothed)) {
  w1 <- bq1_smoothed$window_start[i]
  w2 <- bq1_smoothed$window_start[i] + weeks(9)
  # potassium
  potassium <- bq1_filtered$K[
    bq1_filtered$Sample_Date >= w1 & bq1_filtered$Sample_Date < w2
  ]
  mean_potassium <- mean(potassium, na.rm = TRUE)
  bq1_smoothed$K[i] <- mean_potassium
  # nitrate-N
  nitrate <- bq1_filtered$"NO3-N"[
    bq1_filtered$Sample_Date >= w1 & bq1_filtered$Sample_Date < w2
  ]
  mean_nitrate <- mean(nitrate, na.rm = TRUE)
  bq1_smoothed$"NO3-N"[i] <- mean_nitrate
  # calcium
  calcium <- bq1_filtered$Ca[
    bq1_filtered$Sample_Date >= w1 & bq1_filtered$Sample_Date < w2
  ]
  mean_calcium <- mean(calcium, na.rm = TRUE)
  bq1_smoothed$Ca[i] <- mean_calcium
  # ammonium-N
  ammonium <- bq1_filtered$"NH4-N"[
    bq1_filtered$Sample_Date >= w1 & bq1_filtered$Sample_Date < w2
  ]
  mean_ammonium <- mean(ammonium, na.rm = TRUE)
  bq1_smoothed$"NH4-N"[i] <- mean_ammonium
  # magnesium
  magnesium <- bq1_filtered$Mg[
    bq1_filtered$Sample_Date >= w1 & bq1_filtered$Sample_Date < w2
  ]
  mean_magnesium <- mean(magnesium, na.rm = TRUE)
  bq1_smoothed$Mg[i] <- mean_magnesium
}
view(bq1_smoothed)

# Pivot longer
bq1_long <- bq1_smoothed |>
  pivot_longer(
    cols = c("NO3-N", "K", "Mg", "Ca", "NH4-N"),
    names_to = "nutrients",
    values_to = "concentration"
  )
glimpse(bq1_long)

# bq1 figure
ggplot(
  data = bq1_long,
  mapping = aes(x = window_start, y = concentration)
) +
  geom_line() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~nutrients, scales = "free")
