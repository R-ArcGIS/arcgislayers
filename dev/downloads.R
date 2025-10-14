library(dplyr)
library(ggplot2)

downloads <- cranlogs::cran_downloads(
  c(
    "arcgislayers",
    "arcgisgeocode",
    "arcgisplaces",
    "calcite",
    "arcpbf",
    "arcgisutils"
  ),
  from = "2024-01-11",
  to = Sys.Date()
) |>
  as_tibble() |>
  filter(count > 0) |>
  mutate(week_of = lubridate::floor_date(date, "week")) |>
  arrange(week_of) |>
  group_by(package, week_of) |>
  summarise(downloads = sum(count)) |>
  mutate(total_downloads = cumsum(downloads)) |>
  arrange(package) |>
  ungroup()


ggplot(downloads) +
  geom_line(aes(week_of, total_downloads)) +
  facet_wrap("package", scales = "free_y", ncol = 2) +
  labs(y = "Cumulative downloads", x = "")

downloads |>
  summarise(downloads = sum(downloads), .by = package) |>
  arrange(desc(downloads))


weekly <- cranlogs::cran_downloads(
  c("arcgislayers", "arcgisgeocode", "arcgisplaces", "calcite"),
  from = "2024-01-11",
  to = Sys.Date()
) |>
  as_tibble() |>
  filter(count > 0) |>
  mutate(week_of = lubridate::floor_date(date, "week")) |>
  group_by(package, week_of) |>
  summarise(total = sum(count))


pkgs <- c(
  "arcgislayers",
  "arcgisgeocode",
  "arcgisplaces",
  "calcite",
  "arcpbf",
  "arcgisutils"
)

weekly <- cranlogs::cran_downloads(
  pkgs,
  from = "2024-01-11",
  to = Sys.Date()
) |>
  as_tibble() |>
  filter(count > 0) |>
  mutate(week_of = lubridate::floor_date(date, "week")) |>
  group_by(week_of) |>
  summarise(total = sum(count)) |>
  mutate(total_downloads = cumsum(total))

ggplot(weekly, aes(week_of, total_downloads)) +
  geom_line(lwd = 0.8, color = "white") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(
    x = "",
    y = "Cumulative downloads",
    title = "CRAN package downloads"
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "#ffffff20"),
    panel.grid.minor = element_line(color = "#ffffff50"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    axis.text = element_text(color = "white"),
    title = element_text(color = "white")
  )

ggsave(
  filename = "~/downloads/pkg-dls.png",
  width = 1920,
  height = 1080,
  units = "px"
)

sum(weekly$total)

announce_day <- as.Date("2024-03-13")
as.integer(Sys.Date() - announce_day) / 365
