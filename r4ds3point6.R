library(tidyverse)
library(gridExtra)

str(mpg)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ .)

if (FALSE) {"
 geom_line will make a line chart, geom_boxplot will make a boxplot, 
 geom_histogram will make a histogram, and geom_area will make an area chart.
"}

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

if (FALSE) {"
  show.legend = FALSE removes the legend from the chart if one is created by default.
  If you remove it, then there's more space for the chart in the *grob*.
  I assume that Hadley removed it since there's limited space in r4ds with so many charts next to one another.
"}

if (FALSE) {"
  The se argument adds error bars to the lines produced by geom_smooth and stat_smooth geoms.
"}

if (FALSE) {"
  These should produce the same plot since the aesthetics mapping remain the same.
The only difference is that one plot defines the mapping in `ggplot()` and the other defines it in the
individual graphical layers.
"}

g1 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

g2 <- ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

gridExtra::grid.arrange(g1, g2, nrow = 2)

g1 <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE)

g2 <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 2) +
  geom_smooth(data = filter(mpg, drv == "4"), se = FALSE) +
  geom_smooth(data = filter(mpg, drv == "f"), se = FALSE) +
  geom_smooth(data = filter(mpg, drv == "r"), se = FALSE)
              
g3 <- ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

g4 <- ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(aes(x = displ, y = hwy), 
              se = FALSE)

g5 <- ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(aes(x = displ, y = hwy, linetype = drv), 
              se = FALSE)

# g6 <- ggplot(data = mpg) +
#   geom_point(aes(x = displ, y = hwy, fill = "white", size = 2), show.legend = FALSE) +
#   geom_point(aes(x = displ, y = hwy, color = drv))
g6 <- ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(color = "white", size = 4) +
  geom_point() 

grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 3)