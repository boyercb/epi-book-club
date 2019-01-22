library(tidyverse)
library(ggthemes)

df <- tribble(
  ~country, ~category, ~value,
  "USA", "no change", 0.49,
  "USA", "less certain", 0.14,
  "USA", "more certain", 0.37,
  "Canada", "no change", 0.46,
  "Canada", "less certain", 0.12,
  "Canada", "more certain", 0.42,
  "Brazil", "no change", 0.17,
  "Brazil", "less certain", 0.20,
  "Brazil", "more certain", 0.63,
  "Spain", "no change", 0.53,
  "Spain", "less certain", 0.11,
  "Spain", "more certain", 0.36,
  "France", "no change", 0.53,
  "France", "less certain", 0.18,
  "France", "more certain", 0.29,
  "UK", "no change", 0.60,
  "UK", "less certain", 0.13,
  "UK", "more certain", 0.27,
  "Germany", "no change", 0.59,
  "Germany", "less certain", 0.10,
  "Germany", "more certain", 0.31,
  "Italy", "no change", 0.42,
  "Italy", "less certain", 0.12,
  "Italy", "more certain", 0.46,
  "China", "no change", 0.23,
  "China", "less certain", 0.16,
  "China", "more certain", 0.61,
  "Japan", "no change", 0.48,
  "Japan", "less certain", 0.22,
  "Japan", "more certain", 0.30,
  "Australia", "no change", 0.49,
  "Australia", "less certain", 0.14,
  "Australia", "more certain", 0.37,
  "World", "no change", 0.46,
  "World", "less certain", 0.14,
  "World", "more certain", 0.40,
)


df <- df %>%
  spread(category, value) %>%
  mutate(
    rank = `more certain`,
    country = fct_reorder(country, rank),
  ) %>%
  gather(category, value, `less certain`:`no change`) %>%
  mutate(
    category = fct_relevel(category, c("less certain", "no change", "more certain")),
    highlight = country == "World"
  )

ggplot(df, aes(x = country, y = value, fill = category, alpha = highlight)) + 
  geom_bar(stat = "identity", color = "white", width = 1) +
  geom_text(
    aes(label = scales::percent(value, accuracy = 1)), 
    size = 3, 
    color = "black", 
    position = position_stack(vjust = 0.5)
  ) +
  labs(title = "Over the past year, have your views about climate altered\nin any way?") + 
  theme_fivethirtyeight() +
  scale_fill_viridis_d() + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = FALSE) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
  ) +
  coord_flip()


ggsave("twitter_country_plot.png", device = "png", dpi = "retina")