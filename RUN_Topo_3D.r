library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)
str(localtif)

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)
str(elmat)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()


test <- raster::raster("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/mnt_relief.tif")

test_mat <- raster_to_matrix(test)

str(test_mat)

test_mat %>%
    sphere_shade(texture = "desert") %>%
    plot_map()


# other file #
library(terra)
library(sf)
library(rayshader)

run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION","Reunion_2015_region")
run <- terra::rast("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/RUN_3D/litto3D_5m.ers")
run
apb <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/APB_PNB/APB_PNB.shp")
apb
plot(apb)

# run <- st_buffer (st_buffer(run, 100), -100)
lf <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/3_SUPPLEMENTS_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",
                 full.names = TRUE,
                 pattern = ".shp")
source <- st_read(lf[1])
lf <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/TOPO_REUNION/BDALTI974/BDALTIV2_2-0_25M_ASC_RGR92UTM40S-REUN89_D974_2016-03-11/BDALTIV2/1_DONNEES_LIVRAISON_2020-06-00408/BDALTIV2_MNT_25M_ASC_RGR92UTM40S_REUN89_D974",
                 full.names=TRUE)
l <- lapply(lf,
            rast)
r <- do.call("merge",
             l)
crs(r) <- "+proj=utm +zone=40 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # proj of source
r <- focal(r,
           7,
           mean) # smooth pixels and look
r[r < 0] <- 0
r_mat <- raster_to_matrix(r)

x11()
r_mat %>%
    sphere_shade(texture = "desert") %>% # 'imhof1','imhof2','imhof3','imhof4','desert', 'bw', and 'unicorn'
    # add_shadow(ray_shade(r_mat), 0.5) %>% # extra long - autour de 25 min
    plot_3d(r_mat,
            zscale = 10,
            fov = 0,
            theta = 0,
            zoom = 0.5,
            phi = 35,
            background = "white")

render_scalebar(limits = c(0, 5, 10),
                label_unit = "km",
                position = "W",
                y = 50,
                scale_length = c(0.33, 1))
render_compass(position = "E")
    render_snapshot()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 ##### Correction de la couche litto3d   
    
run <- terra::rast("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/RUN_3D/litto3D_5m.ers")
run
summary(raster::values(run))

run2 <- run
run2[run2 < 0] <- 0
summary(raster::values(run2))
raster::writeRaster(run2,
             "C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/RUN_3D/litto3D_5m_V2.tif")
