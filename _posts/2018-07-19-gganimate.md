---
layout: post
title: "Tutorial: gganimate"
date: "2018-07-19"
comments: true
---
``` r
library(ggplot2)
library(gapminder)
library(gganimate)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


animate(p,device="png")
```

<video controls loop>
<source src="animate_files/figure-markdown_github/unnamed-chunk-1.webm" />
</video>
