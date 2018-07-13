library('tidyverse')
library('gganimate')

deaths <- read_csv('deaths.csv') # http://fridaythe13th.wikia.com/wiki/List_of_deaths_in_the_Friday_the_13th_films

deaths.totals <- deaths %>% filter(`On-Screen Death?` == 'Yes') %>% select(Film, Name) %>%
  group_by(Film) %>%
  mutate(n = row_number(), total.deaths = max(n)) %>%
  select(Film, total.deaths) %>% ungroup() %>%
  distinct() %>%
  mutate(cumulative.deaths = cumsum(total.deaths)) %>% ungroup()

deaths.totals$year <- str_extract_all(deaths.totals$Film, "\\([^()]+\\)")
deaths.totals$year <- as.numeric(substring(deaths.totals$year, 2, nchar(deaths.totals$year)-1))


ggplot(deaths.totals, aes(x = year, y = cumulative.deaths)) +
  geom_step(color = 'darkred', size = 1) +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1984, 1985, 1988, 1989, 1993, 2009)) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 20)) +
  labs(title = "Tracking the body count of the Friday the 13th films", subtitle = "cumulative on-screen deaths, from Friday the 13th (1980) to Friday the 13th (2009)", y = "", x = "", 
       caption = "Source: fandom.wikia.com") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        plot.subtitle = element_text(size = 12), 
        axis.text = element_text(size = 10),
        plot.caption = element_text(hjust = -.01, color = 'grey30', size = 9))




