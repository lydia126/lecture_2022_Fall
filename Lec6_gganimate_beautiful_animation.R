# Source from https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# Load required packages and set the default ggplot2 theme to theme_bw():

library(ggplot2)
library(gganimate)
library(gifski)
theme_set(theme_bw())

# Demo dataset
library(gapminder)
head(gapminder)

# Static plot

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

# Transition through distinct states in time
# Basics
pa1 <- p + transition_time(year) +
  labs(title = "Year: {frame_time}")
# save as a GIF
animate(pa1, fps = 10, width = 750, height = 450, renderer=gifski_renderer("pa1.gif"))

# save as a video
#library(ffmpeg)
#animate(pp, renderer = ffmpeg_renderer(), width = 800, height = 450)
#anim_save("pp.mp4")

# Create facets by continent:
pa2 <- p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

animate(pa2, fps = 10, width = 750, height = 450, renderer=gifski_renderer("pa2.gif"))

# Let the view follow the data in each frame
p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)
#Show preceding frames with gradual falloff
p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
#Show the original data as background marks
p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)

#Reveal data along a given dimension
sp <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
sp

#Let data gradually appear
sp + transition_reveal(Day)

p + 
  geom_point() +
  transition_reveal(Day)

p + 
  geom_point(aes(group = seq_along(Day))) +
  transition_reveal(Day)

# Transition between several distinct stages of the data
library(dplyr)
mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp))
mean.temp

bp <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
bp

bp + transition_states(Month, wrap = FALSE) +
  shadow_mark()