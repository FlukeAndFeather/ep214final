library(tidyverse)

# Read in the data
bq1 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca1-Bisley.csv")
bq2 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca2-Bisley.csv")
bq3 <- read_csv("data/knb-lter-luq.20.4923064/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/knb-lter-luq.20.4923064/RioMameyesPuenteRoto.csv")

# Combine data frames
fig3_raw <- bind_rows(bq1, bq2, bq3, prm)
count(fig3_raw, Sample_ID)

# Remove unecessary columns
glimpse(fig3_raw)
fig3_filtered <- fig3_raw |>
  select("Sample_ID", "Sample_Date", "NO3-N", "K", "Mg", "Ca", "NH4-N")
glimpse(fig3_filtered)

# Create tibble with 9wk smoothed windows. Rename nutrients with - to _
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

# Intentional merge conflict