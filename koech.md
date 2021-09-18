``` r
library(tidyverse)
library(ggtext)
library(patchwork)
library(showtext)
library(rnaturalearth)
library(sf)
library(cowplot)
library(rgeos)
library(here)
library(glue)

circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')
```

``` r
mcLarenId = filter(constructors, str_detect(constructors$name, "McLaren")) %>% 
  pull(constructorId)
wins_ferrari_mclaren_raw <- results %>% 
  filter(constructorId %in% mcLarenId | constructorId == "6") %>% 
  filter(positionText == "1")
sf_world <- ne_countries(returnclass = "sf")
wins_ferrari_mclaren <- wins_ferrari_mclaren_raw %>% 
  select(raceId, constructorId)
wins_ferrari_mclaren_with_circuit <- wins_ferrari_mclaren %>% 
  left_join(races, by = "raceId") %>%
  left_join(circuits, by = "circuitId") %>%
  select(raceId, constructorId, year, name.x, country) %>% 
  mutate(winning_constructor = case_when(
          constructorId == "1" ~ "McLaren",
          constructorId == "6" ~ "Ferrari"
  ),
        country = case_when(
          country == "UK" ~ "United Kingdom",
          country == "USA" ~ "United States of America",
          country == "Korea" ~ "South Korea", 
          country == "UAE" ~ "United Arab Emirates",
          TRUE ~ country
        ))
```

``` r
world_ferrari <- sf_world %>% full_join(wins_ferrari_mclaren_with_circuit, by = c("admin" = "country")) %>%
  filter(iso_a3 != "ATA") %>% 
  mutate(have_ferrari_won = case_when(
            constructorId == "6" ~ TRUE,
            TRUE ~ FALSE
  ))
(map_ferrari <- world_ferrari %>% 
  ggplot() +
  geom_sf(aes(fill = have_ferrari_won)) +
  coord_sf(crs = st_crs("ESRI:54030")) + # robinson projection
  scale_fill_manual(values = c("grey90", "#ED1C24")) + 
  labs(subtitle = "Where did <b style='color:#ED1C24'>Ferrari</b> won F1 races?",
       caption = "Scuderia Ferrari is the racing division of luxury Italian auto manufacturer Ferrari and the racing team that competes in Formula One racing. As a constructor, Ferrari has a <b style='color:#ED1C24'>record 16 Constructors' Championships</b>, the last of which was won in 2008. With its drivers, Ferrari has won <b style='color:#ED1C24'>15 Drivers' Championships</b>, a record in this sport. Their current drivers are <b style='color:#ED1C24'>Charles Leclerc</b> and <b style='color:#ED1C24'>Carlos Sainz</b>.") +   
  theme_void() +
  theme(plot.margin = margin(0, 50, 0, 50),
        legend.position = "none",
        plot.subtitle = element_textbox_simple(family = "formula1", face = "bold", halign = .5, size = rel(1.5), margin = margin(b = -350)),
        plot.caption.position = "plot",
        plot.caption = element_textbox_simple(family = "formula1", hjust = 0, lineheight = 1.5, margin = margin(t = -350))))
```

![](koech_files/figure-markdown_github/map-ferrari-1.png)

``` r
world_mclaren <- sf_world %>% full_join(wins_ferrari_mclaren_with_circuit, by = c("admin" = "country")) %>%
  filter(iso_a3 != "ATA") %>% 
  mutate(have_mclaren_won = case_when(
            constructorId == "1" ~ TRUE,
            TRUE ~ FALSE
  ))
(map_mclaren <- world_mclaren %>% 
  ggplot() +
  geom_sf(aes(fill = have_mclaren_won)) +
  coord_sf(crs = st_crs("ESRI:54030")) + # robinson projection
  scale_fill_manual(values = c("grey90", "#E0610E")) + 
  labs(subtitle = "**Where did <b style='color:#E0610E'>McLaren</b> won F1 races?**",
       caption = "McLaren Racing Limited is a British motor racing team based at the McLaren Technology Centre in Woking, Surrey, England. McLaren is best known as a Formula One constructor, and is the second oldest active team, and second most successful Formula One team after Ferrari, having won <b style='color:#E0610E'>182 races</b>, <b style='color:#E0610E'>12 Drivers' Championships</b> and <b style='color:#E0610E'>8 Constructors' Championships</b>. Their current drivers are <b style='color:#E0610E'>Lando Norris</b> and <b style='color:#E0610E'>Daniel Ricciardo</b>.") +
  theme_void() +
  theme(plot.margin = margin(0, 50, 0, 50),
        legend.position = "none",
        plot.subtitle = element_textbox_simple(family = "formula1", face = "bold", halign = .5, size = rel(1.5), margin = margin(b = -350)),
        plot.caption.position = "plot",
        plot.caption = element_textbox_simple(family = "formula1", hjust = 0, lineheight = 1.5, margin = margin(t = -350))))
```

![](koech_files/figure-markdown_github/map-mclaren-1.png)

``` r
win_per_decade <- wins_ferrari_mclaren_raw %>% 
  left_join(races, by = "raceId") %>% 
  select(resultId, raceId, constructorId, name, year, date) %>% 
  mutate(decade = paste0((year - year %% 10), "s")) %>% 
  group_by(decade, constructorId) %>% 
  count()
win_per_decade_ferrari <- win_per_decade %>% 
  filter(constructorId == "6")
(bar_ferrari <- win_per_decade_ferrari %>% 
  ggplot(aes(decade, n)) +
  geom_col(fill = "#ED1C24") +
  scale_x_discrete() +
  scale_y_continuous(
                     breaks = seq(0, 80, by = 20),
                     labels = seq(0, 80, by = 20)) +
  geom_text(data = win_per_decade %>% 
              filter(constructorId == "6"),
            aes(x = decade, y = n + 3, label = n),
            color = "#ED1C24", 
            fontface = "bold") + 
  labs(x = NULL,
       y = NULL, 
       title = "<span style='color:#ED1C24'>Ferrari</span> v <span style='color:#E0610E'>McLaren                </span><br><span style=''>The history of</span> <span style='font-size:1pt;color:#E5E5E5'>some placeholder textsome placeholder textsome placeholder textsome placeholder textsome placeholder textsome placeholder text</span>",
       subtitle = "<span style='color:#ED1C24'>Ferrari</span>'s wins") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(family = "formula1", face = "bold", size = rel(3), lineheight = 1.25, halign = .5, margin = margin(t = 20, b = 20)),
    plot.subtitle = element_textbox_simple(family = "formula1", face = "bold", size = rel(1.25), halign = .5, margin = margin(t = 10)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = rel(1.5))
  ))
```

![](koech_files/figure-markdown_github/bar-chart-1.png)

``` r
bar_ferrari_with_logo <- ggdraw(bar_ferrari) + 
  draw_image(
    here::here("img", "f1logo.png"), 
    x = .875, y = .785, 
    width = .27, 
    hjust = .5, vjust = .5
  )
win_per_decade_mclaren <- win_per_decade %>% 
  filter(constructorId != "6")
win_per_decade_mclaren <- tibble(win_per_decade_mclaren) %>% 
  add_row(decade = "1950s", constructorId = 1, n = 0, .before = 1)
(bar_mclaren <- win_per_decade_mclaren %>% 
  ggplot(aes(decade, -n)) +
  geom_col(fill = "#E0610E") +
  scale_x_discrete() +
  scale_y_continuous(limits = c(-150, NA),
                     breaks = seq(0, -80, by = -20),
                     labels = seq(0, 80, by = 20)) +
  geom_text(data = win_per_decade %>% 
              filter(constructorId != "6"),
            aes(x = decade, y = -n - 3, label = n),
            color = "#E0610E", 
            fontface = "bold") + 
  labs(x = NULL,
       y = NULL, 
       caption = "<span style='color:#E0610E'>McLaren</span>'s wins") +
  theme_minimal() + 
  theme(
    plot.margin = margin(b = -200),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_textbox_simple(family = "formula1", face = "bold", size = rel(1.25), halign = .5, margin = margin(t = -400)),
    axis.text = element_blank()
  ))
```

![](koech_files/figure-markdown_github/bar-chart-2.png)

``` r
(map_ferrari | (bar_ferrari_with_logo / bar_mclaren) | map_mclaren) + plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(caption = "Data: Ergast &bull; Text and logo: Wikipedia &bull; TidyTuesday", 
                  theme = theme(
                                plot.background = element_rect(fill = "grey90"),
                                plot.caption = element_textbox_simple(family = "Avenir", halign = 1, margin = margin(b = 20), size = rel(1.25))))
```

![](koech_files/figure-markdown_github/bar-chart-3.png)

``` r
# Saving and exporting 
path <- here::here("plots", "2021_37", "2021_Formula1")
```

``` r
ggsave(filename =glue::glue("{path}.pdf"), width = 19, height = 11,device=cairo_pdf)
```

``` r
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 300)
```

    ## Converting page 1 to C:/Users/USER/Desktop/pandas/F1-WINNING/plots/2021_37/2021_Formula1.png... done!

    ## [1] "C:/Users/USER/Desktop/pandas/F1-WINNING/plots/2021_37/2021_Formula1.png"
