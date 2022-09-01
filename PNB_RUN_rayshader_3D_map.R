library(terra)
library(rgdal)
library(sf)
library(raster)
library(osmdata)
library(dplyr)
library(ggplot2)
library(rayshader)
library(rayrender)

# run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION","Reunion_2015_region")
run <- terra::rast("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/RUN_3D/litto3D_5m.ers")
run <- terra::rast(run)
run
plot(run)
r <- run

apb <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/APB_PNB/APB_PNB.shp")
apb
plot(apb)

crs(r) <- crs(apb)

# lf <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/3_SUPPLEMENTS_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",
#                  full.names = TRUE,
#                  pattern = ".shp")
# source <- st_read(lf[1])
# lf <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/1_DONNEES_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",
#                  full.names=TRUE)
# l <- lapply(lf,
#             rast)
# r <- do.call("merge",
#              l)
# crs(r) <- "+proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # proj of source

r[r <= 0] <- NA

r_aggr <- terra::aggregate(r,
                            fact = 10)

r_mat <- raster_to_matrix(raster(r_aggr))

mouse <- rgl::par3d("mouseMode")
mouse[5] <- "zoom"

r_mat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(generate_polygon_overlay(geometry = apb,
                                       extent = extent(raster(r_aggr)),
                                       heightmap = r_mat,
                                       palette = "red",
                                       linecolor = NA),
              alphalayer = 0.85)%>%
  plot_3d(r_mat,
          water = T,
          zscale = res(r_aggr)[1],
          mouseMode = mouse)

render_compass(position = "E",
               compass_radius = 30,
               clear_compass = F)

# render_scalebar(limits = c(0, 1),
#                 label_unit = "km",
#                 position = "W",
#                 y = 50,
#                 scale_length = 0.1,
#                 clear_scalebar = T)
render_snapshot()

#### ------ Zoom in APB ------- ####
# r_aggr_crop <- crop(r, vect(st_buffer(apb, 3000)))
# r_aggr_crop <- mask(r_aggr_crop, vect(st_buffer(apb, 3000)))


crop2 <- crop(r, vect(st_as_sfc(st_bbox(st_buffer(apb, 3000)))))



r_crop <- raster_to_matrix(raster(crop2))

r_crop %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(generate_polygon_overlay(geometry = apb,
                                       extent = extent(raster(crop2)),
                                       heightmap = r_crop,
                                       palette = "red",
                                       linecolor = NA),
              alphalayer = 0.75) %>%
  plot_3d(r_crop,
          zscale = res(crop2)[1],
          mouseMode = mouse)
render_snapshot()
