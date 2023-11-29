library(magick)

img <- image_read(outfile)

img %>% 
  image_annotate("Population density of Georgia (people/km2)",
                 gravity = "northwest",
                 location = "+5+20",
                 color = "#013c58",
                 size = 25,
                 font = "Avenir")

image_write(img, "images/final.png", format = "png")
