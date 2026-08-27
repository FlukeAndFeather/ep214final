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

write_csv(fig3_long, "output/fig3_long.csv")
