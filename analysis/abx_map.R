### Plotting data on a map 
# from: https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html

## libraries 
library(eurostat)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## locations
home <- "~/Documents/flusurvey/"
plots <- "~/Documents/flusurvey/plots/"
data <- "~/Documents/flusurvey/data/"

### data
# Bouts of illness data - for antibiotics, cleaned in antibiotics_univariate
setwd(data)
bt <- readRDS("btt_abx.rds")
bt <- data.table(bt)

### Manipulating 
sp_data <- tgs00026 %>% 
  # subsetting to year 2014 and NUTS-3 level
  dplyr::filter(time == 2014, nchar(as.character(geo)) == 4, grepl("PL",geo)) %>% 
  # label the single geo column
  mutate(label = paste0(label_eurostat(.)[["geo"]], "\n", values, "€"),
         income = cut_to_classes(values)) %>% 
  # merge with geodata
  merge_eurostat_geodata(data=.,geocolumn="geo",resolution = "01", all_regions = FALSE, output_class="spdf")

# plot map
map2 <- tm_shape(Europe) +
  tm_fill("lightgrey") +
  tm_shape(sp_data, is.master = TRUE) +
  tm_polygons("income", title = "Disposable household incomes in 2014",
              palette = "Oranges", border.col = "white") + 
  tm_text("label", just = "center") + 
  tm_scale_bar() +
  tm_format_Europe(legend.outside = TRUE, attr.outside = TRUE)
map2