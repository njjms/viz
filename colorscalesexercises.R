library(ggplot2)
iah <- read.csv("http://vis.cwick.co.nz/data/iah-summary.csv")

# make sure days of week are displayed in the right order
iah$DayOfWeek <- factor(iah$DayOfWeek, 
                        levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

p <- ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = prop_over_15)) 
p + scale_fill_gradient("Proportion", high="white", low="springgreen4")

RColorBrewer::display.brewer.all()

install.packages("viridis")
library(viridis)

(unique(iah$avg_delay_cut))

iah$avg_delay_cut <- with(iah, 
                          cut(avg_delay,
                              breaks = c(-5, 0, 15, 30, 60, 1000),
                              labels = c("early", "on time", "kinda late", "late", "what the actual heck"))) # discretize average delay

pp <- ggplot(iah, aes(DepHour, DayOfWeek)) +
  geom_tile(aes(fill = avg_delay_cut))
pp + scale_fill_manual(values = c("#fcc8b6","#f9926c","#f76d3b","#f5490a","#d73f09"))

p + scale_fill_viridis()
p + scale_fill_distiller(palette = "Blues")
