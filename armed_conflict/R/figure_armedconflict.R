finaldata <- read.csv(here("original", "primary_data.csv"), header = TRUE)

forfigure <- finaldata |>
  dplyr::select(country_name, ISO, year, MatMor) |>
  dplyr::filter(year < 2018) |>
  group_by(ISO) |>
  mutate(diffmatmor = MatMor - MatMor[1L]) |>
  arrange(ISO, desc(year)) |>
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0 , 1, 0)) |>
  arrange(ISO, year) |>
  ungroup() |>
  dplyr::filter(incmatmor == 1)
length(unique(forfigure$ISO))

fig1 <- forfigure |>
  ggplot(aes(x = year, y = MatMor, group = ISO)) +
  geom_line(aes(color = country_name), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 sclae for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 10 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)

# save the gplot as a png file
ggsave(fig1, file = here("figures", "fig1_incmatmor.png"), width = 8, height = 5)