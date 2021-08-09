library(raster)
library(rasterVis)
library(ggplot2)

minlin_pred <- raster("./Res/bear_minlin_pred.tif")
minlin_limit <- raster("./Res/bear_minlin_limiting.tif")

mapstack <- stack(minlin_pred, minlin_limit)
names(mapstack) <- c("prediction","limiting factor")

p1 <- gplot(minlin_pred) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "#0072B2",
                      high =  "#E7B800",
                      midpoint = 0.5,
                      #low="gray",
                      na.value="transparent") +
  labs(fill = "Probability", x = "", y="") + 
  coord_equal()
p1

p2 <- gplot(minlin_limit) + 
  geom_tile(aes(fill = factor(value, labels = c("Conservation","Topology", "Human", "Forest"))))+
  scale_fill_manual(values = c("gold", "grey", "red", "forestgreen"),na.value="transparent",na.translate = F)+
  labs(fill = "Limiting Factor", x = "", y="") + 
  coord_equal()
p2

library(ggpubr)
ggarrange(p1,p2, labels = "AUTO", align = "hv")

ggsave("./Res/sichuan_maps.pdf", width = 12, height = 5)
