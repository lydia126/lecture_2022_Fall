# Source from https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# Load required packages and set the default ggplot2 theme to theme_bw():

library(ggplot2)
library(gganimate)
theme_set(theme_bw())

# Demo dataset
df <- read.csv("DC_BOD_COD.csv", na = "-", fileEncoding = "CP949", encoding = "UTF-8")
head(df)

# Static plot

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18),
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18),
  legend.key = element_rect(size = 20))

p0 <- ggplot(
  df, 
  aes(x = BOD, y=COD, size = CODBOD, colour = Site)) +
  geom_point(show.legend = TRUE, alpha = 0.7, position = "jitter") +
  scale_color_viridis_d() +
  scale_size(range = c(2, 8)) +
  labs(x = "BOD(mg/L)", y = "COD(mg/L)")+
  My_Theme
p0

# Transition through distinct states in time
# Basics
pa0 <- p0 + transition_time(Year) +
  labs(title = "Year: {frame_time}", size = 20)
# save as a GIF
animate(pa0, fps = 5, width = 600, height = 500, renderer=gifski_renderer("pa0.gif"))

p1 <- ggplot(
  df, 
  aes(x = BOD, y=COD, size = CODBOD, colour = Waterbody)) +
  geom_point(show.legend = TRUE, alpha = 0.7, position = "jitter") +
  scale_color_viridis_d() +
  scale_size(range = c(2, 8)) +
  labs(x = "BOD(mg/L)", y = "COD(mg/L)")+
  My_Theme
p1

# Transition through distinct states in time
# Basics
pa1 <- p1 + transition_time(Year) +
  labs(title = "Year: {frame_time}", size = 20)
# save as a GIF
animate(pa1, fps = 5, width = 600, height = 500, renderer=gifski_renderer("pa1.gif"))


pl <- ggplot(
  df,
  aes(Year, CODBOD, group = Site, color = Site)) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Year", y = "COD to BOD Ratio") +
  theme(legend.position = "top")+
  My_Theme
pl

# pla <- pl + 
#   geom_point() +
#   transition_reveal(Year)
# 
# animate(pla, fps = 5, width = 1000, height = 500, renderer=gifski_renderer("pla.gif"))
# 

pla <- pl + 
  geom_point(aes(group = seq_along(Year))) +
  transition_reveal(Year)
animate(pla, fps = 5, width = 1200, height = 500, renderer=gifski_renderer("pla.gif"))
