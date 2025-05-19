library(dplyr)
library(ggplot2)

downloads <- cranlogs::cran_downloads(
  c("arcgislayers", "arcgisutils", "arcpbf", "arcgisgeocode", "arcgisplaces"),
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
  facet_wrap("package", scales = "free", ncol = 2)

downloads |>
  summarise(downloads = sum(downloads), .by = package) |>
  arrange(desc(downloads))
