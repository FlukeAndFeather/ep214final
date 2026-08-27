library(tidyverse)

fig3_long <- read_csv("output/fig3_long.csv")

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
