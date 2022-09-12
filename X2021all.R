library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtern)
data <-MIDS
df <- filter(data,Pos == "MF" & Min>=1400)
#view(df)
tern_limits(x=530,y=420,z=30)
plot <- ggtern(data = df, aes(x=ProgAct, y=DefAct, z=GAT)) + geom_point()
plot
plot + stat_density_tern(geom = 'polygon',
                         n         = 200,
                         aes(fill  = ..level..,
                             alpha = ..level..)) +
  geom_point() +
  theme_rgbw() +
  labs(title = "Example Density/Contour Plot")    +
  scale_fill_gradient(low = "blue",high = "red")  +
  guides(color = "none", fill = "none", alpha = "none") + geom_text(label=Name)
