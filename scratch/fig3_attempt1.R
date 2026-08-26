library(tidyverse)

# Read in the data
bq1 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca1-Bisley.csv")
bq2 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca2-Bisley.csv")
bq3 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca3-Bisley.csv")
mpr <- read_csv("data/knb-lter-luq.20.4923064/RioMameyesPuenteRoto.csv")

# Day 2 attempt ----------------------------------------------------------

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

# Day 1 attempt ----------------------------------------------------------

# Combine data frames
fig3_raw <- bind_rows(bq1, bq2, bq3, mpr)
count(fig3_raw, Sample_ID)

# Remove unecessary columns
glimpse(fig3_raw)
fig3_filtered <- fig3_raw |>
  select("Sample_ID", "Sample_Date", "NO3-N", "K", "Mg", "Ca", "NH4-N")
glimpse(fig3_filtered)

# Create tibble with 9wk smoothed windows.
fig3_smoothed <- tibble(
  window_start = seq(
    fig3_filtered$Sample_Date[1],
    fig3_filtered$Sample_Date[nrow(fig3_filtered)],
    by = "9 weeks",
  ),
  Sample_ID = NA,
  "NO3-N" = NA,
  K = NA,
  Mg = NA,
  Ca = NA,
  "NH4-N" = NA
)
glimpse(fig3_smoothed)

# Add 9 wk moving average to the fig3_smoothed data frame
for (i in 1:nrow(fig3_smoothed)) {
  w1 <- fig3_smoothed$window_start[i]
  w2 <- fig3_smoothed$window_start[i] + 63 # 9 weeks
  # potassium
  potassium <- fig3_filtered$K[
    fig3_filtered$Sample_Date >= w1 & fig3_filtered$Sample_Date < w2
  ]
  mean_potassium <- mean(potassium, na.rm = TRUE)
  fig3_smoothed$K[i] <- mean_potassium
  # nitrate-N
  nitrate <- fig3_filtered$"NO3-N"[
    fig3_filtered$Sample_Date >= w1 & fig3_filtered$Sample_Date < w2
  ]
  mean_nitrate <- mean(nitrate, na.rm = TRUE)
  fig3_smoothed$"NO3-N"[i] <- mean_nitrate
  # calcium
  calcium <- fig3_filtered$Ca[
    fig3_filtered$Sample_Date >= w1 & fig3_filtered$Sample_Date < w2
  ]
  mean_calcium <- mean(calcium, na.rm = TRUE)
  fig3_smoothed$Ca[i] <- mean_calcium
  # ammonium-N
  ammonium <- fig3_filtered$"NH4-N"[
    fig3_filtered$Sample_Date >= w1 & fig3_filtered$Sample_Date < w2
  ]
  mean_ammonium <- mean(ammonium, na.rm = TRUE)
  fig3_smoothed$"NH4-N"[i] <- mean_ammonium
  # magnesium
  magnesium <- fig3_filtered$Mg[
    fig3_filtered$Sample_Date >= w1 & fig3_filtered$Sample_Date < w2
  ]
  mean_magnesium <- mean(magnesium, na.rm = TRUE)
  fig3_smoothed$Mg[i] <- mean_magnesium
}
view(fig3_smoothed)

#### Need to figure out how to include the ID before proceed to next steps #####

# Pivot longer
fig3_long <- fig3_filtered |>
  pivot_longer(
    cols = c("NO3-N", "K", "Mg", "Ca", "NH4-N"),
    names_to = "nutrients",
    values_to = "concentration"
  )
glimpse(fig3_long)

# Figure attempt
ggplot(
  data = fig3_long,
  mapping = aes(x = Sample_Date, y = concentration, color = Sample_ID)
) +
  geom_line() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~nutrients, scales = "free")
