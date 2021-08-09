library(raster)
library(rasterVis)
library(ggplot2)

minlin_pred <- raster("./Res/bear_minlin_pred.tif")
minlin_limit <- raster("./Res/bear_minlin_limiting.tif")
rf_pred <- raster("./Res/bear_rf_pred.tif")
glm_pred <- raster("./Res/bear_glm_pred.tif")

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
  ggsn::scalebar(x.min=125000, x.max=250000, y.min = 2.75e6, y.max = 3e6,
                 dist = 200,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .15, st.dist = .15,
                 #family = "Times New Roman"
                 )+
  ggsn::north(x.min=125000, x.max=250000, y.min = 3.5e6, y.max = 3.65e6,
              location = "topleft",scale = 1, symbol = 15)+
  coord_equal()
p1

p2 <- gplot(minlin_limit) + 
  geom_tile(aes(fill = factor(value, labels = c("Conservation","Topology", "Human", "Forest"))))+
  scale_fill_manual(values = c("gold", "grey", "red", "forestgreen"),na.value="transparent",na.translate = F)+
  labs(fill = "Limiting Factor", x = "", y="")  + 
  ggsn::scalebar(x.min=125000, x.max=250000, y.min = 2.75e6, y.max = 3e6,
                 dist = 200,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .15, st.dist = .15,
                 #family = "Times New Roman"
                 )+
  ggsn::north(x.min=125000, x.max=250000, y.min = 3.5e6, y.max = 3.65e6,
              location = "topleft",scale = 1, symbol = 15) +
  coord_equal()
p2

p3 <- gplot(rf_pred) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "#0072B2",
                       high =  "#E7B800",
                       midpoint = 0.5,
                       #low="gray",
                       na.value="transparent") +
  labs(fill = "Probability", x = "", y="") + 
  ggsn::scalebar(x.min=125000, x.max=250000, y.min = 2.75e6, y.max = 3e6,
                 dist = 200,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .15, st.dist = .15,
                 #family = "Times New Roman"
                 )+
  ggsn::north(x.min=125000, x.max=250000, y.min = 3.5e6, y.max = 3.65e6,
              location = "topleft",scale = 1, symbol = 15)+
  coord_equal()
p3

p4 <- gplot(glm_pred) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "#0072B2",
                       high =  "#E7B800",
                       midpoint = 0.5,
                       #low="gray",
                       na.value="transparent"
                       ) +
  labs(fill = "Probability", x = "", y="") + 
  ggsn::scalebar(x.min=125000, x.max=250000, y.min = 2.75e6, y.max = 3e6,
                 dist = 200,dist_unit = "km", transform = F,
                 location = "bottomright",border.size = .15, st.dist = .15,
                 #family = "Times New Roman"
                 )+
  ggsn::north(x.min=125000, x.max=250000, y.min = 3.5e6, y.max = 3.65e6,
              location = "topleft",scale = 1, symbol = 15)+
  coord_equal()
p4


library(ggpubr)
ggarrange(p1,p2, p3,p4, nrow = 2, ncol = 2,labels = "AUTO", align = "hv")

ggsave("./Res/sichuan_maps.pdf", width = 12, height = 10)
