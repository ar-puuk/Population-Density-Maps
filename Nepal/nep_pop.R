####################################################
#' Project Name       - Nepal Population
#' Dataset            - kontur.io
#' 
#' Mapping Population Density of Nepal using Rayshader
####################################################

# ==================================================
# 1. Setting up the environment
# ==================================================

options(rgl.useNULL = FALSE)

# -------------------------------------
# Load packages
# -------------------------------------

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("tmap")
# install.packages("ggplot2")
# install.packages("mapview")
# install.packages("stars")
# install.packages("rayshader")
# install.packages("MetBrewer")
# install.packages("colorspace")
# install.packages("rayrender")
# install.packages("magick")

# library(remotes)
# remove.packages("Rttf2pt1")
# remotes::install_version("Rttf2pt1", version = "1.3.8")

# installing dev version of the following packages due to texture = TRUE error
# remotes::install_github("tylermorganwall/rayshader")
# remotes::install_github("tylermorganwall/rayrender")

require(tidyverse)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(MetBrewer)
require(colorspace)
require(rayrender)
require(magick)
require(extrafont)

# Load Fonts
# font_import()
# loadfonts(device = "win")
# loadfonts(device = "all")

# -------------------------------------
# Set Working Directory
# -------------------------------------

# For Windows
path = "[Project Folder Path]"
setwd(path)

# load existing environment files
# load(file.path(path, "nep_pop.RData"))

# ==================================================
# 2. Read the data
# ==================================================

# load population 400m H3 hexagon
nep_hex <- st_read(file.path(path, "Data/kontur_population_NP_20220630.gpkg")) %>% st_transform(6207)
# source: https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_NP_20220630.gpkg.gz
# or download from: https://data.humdata.org/dataset/kontur-population-nepal
# nep_hex %>% st_geometry() %>% plot()

# load population by administrative boundary
nep_admin <- st_read(file.path(path, "Data/kontur_boundaries_NP_20220407.gpkg")) %>% st_transform(6207)
# source: https://geodata-eu-central-1-kontur-public.s3.eu-central-1.amazonaws.com/kontur_datasets/kontur_boundaries_NP_20220407.gpkg.gz
# or download from: https://data.humdata.org/dataset/kontur-boundaries-nepal
# nep_admin %>% st_geometry() %>% plot()

# creating Nepal boundary
nepal_boundary <- nep_admin%>% st_geometry %>% st_union %>% st_sf %>% st_make_valid()
# nepal_boundary %>% st_geometry() %>% plot()

# ==================================================
# 3. Clean the data
# ==================================================

# check the plot
ggplot(nep_hex) + geom_sf(aes(fill = population), color = "gray66", linewidth = 0) +
  geom_sf(data = nepal_boundary, fill = NA, color = "black", linetype = "dashed", linewidth = 1)

# setting the nepal boundary as a bounding box
bbox <- st_bbox(nepal_boundary)

# finding the aspect ratio
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>% st_sfc(crs = 6207)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>% st_sfc(crs = 6207)
top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>% st_sfc(crs = 6207)

width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if(width > height) {
  w_ratio = 1
  h_ratio = height / width
} else {
  h_ratio = 1
  w_ratio = width / height
}

# convert to raster to convert to matrix

size = 1000 * 2.5

pop_raster <- st_rasterize(nep_hex,
                           nx = floor(size * w_ratio),
                           ny = floor(size * h_ratio))

pop_matrix <- matrix(pop_raster$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))

# create a color palette
color <- met.brewer("OKeeffe2")
swatchplot(color)

texture <- grDevices::colorRampPalette(color, bias = 3)(256)
# grDevices::colorRampPalette(c("#fff7bc", "#cc4c02", "#662506"))
swatchplot(texture)

# plotting 3D

rgl::rgl.close()

pop_matrix %>%
  height_shade(texture = texture) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 250 / 2.5,
          solid = F,
          shadowdepth = 0)

render_camera(theta = 0,
              phi = 30,
              zoom = 0.4,
              fov = 90
              )

outfile <- "Plots/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }

  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 225,
    lightaltitude = c(20, 80),
    lightcolor = c(color[2], "white"),
    lightintensity = c(600, 100),
    width = 1980,
    height = 1080,
    samples = 300
  )

end_time <- Sys.time()
diff <- end_time - start_time
cat(crayon::cyan(diff), "\n")
}

# ==================================================
# 5. Save Environment (.Rdata) file
# ==================================================

save.image(file.path(path, "nep_pop.RData"))

# ==================================================
# 6. Edit and Annotate Image
# ==================================================

pop_raster <- image_read("Plots/final_plot.png")

text_color <- darken(color[7], .5)
swatchplot(text_color)

pop_raster %>%
  # image_crop(gravity = "center", geometry = "") %>%
  image_annotate("NEPAL",
                 gravity = "northeast",
                 location = "+50+50",
                 color = text_color,
                 size = 150,
                 font = "Ananda Namaste",
                 weight = 700,
                 # degrees = 0,
                 ) %>%
  image_annotate("POPULATION DENSITY MAP",
                 gravity = "northeast",
                 location = "+50+200",
                 color = text_color,
                 size = 36.5,
                 font = "FuturaBT-Medium",
                 weight = 500,
                 # degrees = 0,
                ) %>%
  image_annotate("Kathmandu",
                 gravity = "center",
                 location = "+250-50",
                 color = alpha(text_color, .8),
                 size = 30,
                 font = "FuturaBT-Medium",
                 # degrees = -85,
                 ) %>%
  image_annotate("Biratnagar",
                 gravity = "east",
                 location = "+125+100",
                 color = alpha(text_color, .8),
                 size = 28,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Itahari",
                 gravity = "east",
                 location = "+230+150",
                 color = alpha(text_color, .8),
                 size = 24,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Janakpur",
                 gravity = "east",
                 location = "+500+140",
                 color = alpha(text_color, .8),
                 size = 22,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Pokhara",
                 gravity = "center",
                 location = "-30+35",
                 color = alpha(text_color, .8),
                 size = 25,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Nepalgunj",
                 gravity = "center",
                 location = "-450+0",
                 color = alpha(text_color, .8),
                 size = 24,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Surkhet",
                 gravity = "center",
                 location = "-450-75",
                 color = alpha(text_color, .8),
                 size = 20,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Tikapur",
                 gravity = "west",
                 location = "+400-125",
                 color = alpha(text_color, .8),
                 size = 22,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Dhangadhi",
                 gravity = "west",
                 location = "+275-135",
                 color = alpha(text_color, .8),
                 size = 18,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Birgunj",
                 gravity = "center",
                 location = "+130+100",
                 color = alpha(text_color, .8),
                 size = 25,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Bharatpur",
                 gravity = "center",
                 location = "+60+0",
                 color = alpha(text_color, .8),
                 size = 20,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Butwal",
                 gravity = "center",
                 location = "-125+5",
                 color = alpha(text_color, .8),
                 size = 20,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Ghorahi\nTulsipur",
                 gravity = "center",
                 location = "-350-100",
                 color = alpha(text_color, .8),
                 size = 22,
                 font = "FuturaBT-Medium",
                 # degrees = -75,
                 ) %>%
  image_annotate("Visualization by: Pukar Bhandari with Rayshader(@tylermorganwall) | Data: Kontur Population (Released 2022-06-30)",
                 gravity = "southwest",
                 location = "+20+20",
                 color = alpha(text_color, .6),
                 font = "FuturaBT-Medium",
                 size = 20,
                 # degrees = 0,
                 ) %>%
  image_write("Plots/final_plot_edited.png", format = "png", quality = 100)
