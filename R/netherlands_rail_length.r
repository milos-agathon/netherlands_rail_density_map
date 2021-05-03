# RAILWAY LENGTH FOR EVERY DUTCH MUNICIPALITY USING OSM DATA
# Milos Popovic 05/03/2021

library(plyr, quietly=T)
library(tidyverse, quietly=T) 
library(sf, quietly=T)
library(ggplot2, quietly=T) 
library(dplyr, quietly=T)
library(rgdal, quietly=T)
library(classInt, quietly=T)
library(zoo, quietly=T)
library(geofabrik, quietly=T)
library(raster, quietly=T)

#download the official shapefile of Dutch munis
tm <- tempfile(fileext = ".zip")
download.file("https://opendata.arcgis.com/datasets/e1f0dd70abcb4fceabbc43412e43ad4b_0.zip", 
    tm)
unzip(tm)

#load muni shapefile
nl <- readOGR(getwd(),
				"Gemeentegrenzen__voorlopig____kustlijn", 
				 verbose = TRUE, 
				 stringsAsFactors = FALSE) %>%
      spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# get OSM data for the Netherlands (size 1816.0 MB)
outDir <- paste0(getwd(), "/netherlands")
get_osm("Netherlands", 
  type = "shp", 
  file = outDir, 
  quiet = FALSE)
unzip(paste0(outDir, ".shp.zip"), exdir=getwd())

#load railways and subset rail type
rail <- readOGR(getwd(),
        "gis_osm_railways_free_1", 
         verbose = TRUE, 
         stringsAsFactors = FALSE) %>%
      subset(fclass=="rail") %>%
      spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

#calculate the length of railway for every muni
# first, turn both shp files into sf class
n <- nl %>% st_as_sf
r <- rail %>% st_as_sf 

ints <- st_intersection(r, n) %>% 
        dplyr::mutate(len_m = sf::st_length(geometry)) %>% # returns length in meters
        dplyr::group_by(Code)

int <- as.data.frame(as.matrix(ints))
int$len_m <- as.numeric(as.character(int$len_m))
int$Code <- as.character(int$Code)
ii <- ddply(int, "Code", summarise, lr_m = sum(len_m, na.rm = TRUE))

# join be and int
df <- merge(nl, ii, by='Code', all.x=T)

# we already have area var but let's see how to calculate on our own
df$area_sqkm <- area(df) / 1000000

# rail density: km or rail / 10 square km
df$rail_dens <- (df$lr_m/1000) / df$area_sqkm * 100

# place back in sf object for mapping
d <- df %>% st_as_sf

# let's find a natural interval with quantile breaks
ni = classIntervals(d$rail_dens, 
           n = 8, 
           style = 'quantile')$brks

# this function uses above intervals to create categories
labels <- c()
for(i in 1:length(ni)){
    labels <- c(labels, paste0(round(ni[i], 0), 
                             "–", 
                             round(ni[i + 1], 0)))
}
labels <- labels[1:length(labels)-1]

# finally, carve out the categorical variable based on the breaks and labels above
d$cat <- cut(d$rail_dens, 
              breaks = ni, 
              labels = labels, 
              include.lowest = T)
levels(d$cat) # let's check how many levels it has (8)

# label NAs, too
lvl <- levels(d$cat)
lvl[length(lvl) + 1] <- "No rail"
d$cat <- factor(d$cat, levels = lvl)
d$cat[is.na(d$cat)] <- "No rail"
levels(d$cat)

# plot
p <- ggplot() +
geom_sf(data=d, aes(fill = cat), color="white", size=0.1) +
    coord_sf(crs = 4326, datum = NA) +
    theme_minimal() +
scale_fill_manual(name= expression(paste("1 km of rail per 100", km^{2}, "of land area")), 
  values = rev(c("grey80", '#513b56', '#51556f', '#4c6e88', '#44889e', '#51a1a3', '#75b99c', '#99d091', '#bce784')),
  labels = c("~0–8",     "8–14",    "14–20",   "20–26",   "26–41",   "41–55",   "55–78",  
">78",  "No rail"),
  drop = F)+
  guides(color=F, fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            # also the guide needs to be reversed
            reverse = F,
            label.position = "bottom"
          )
    ) +
  labs(x = "©2021 Milos Popovic (https://milosp.info)\n Data: OSM Geofabrik",
         title = "Railway density in the Netherlands", 
         subtitle = "municipality level", 
         caption = "") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=9, color="grey60", hjust=0, vjust=25),
    axis.title.y = element_blank(),
    legend.position = c(.5, -.01),
    legend.text = element_text(size=11, color="grey20"),
    legend.title = element_text(size=12, color="grey20"),
    # panel.grid.minor = element_line(color = "grey20", size = 0.2),
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin     =   unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
  plot.title = element_text(face="bold", size=20, color="#513b56", hjust=.5, vjust=-2),
plot.subtitle = element_text(size=16, color="#457795", hjust=.5, vjust=-2),
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank())
    
ggsave(filename="nld_rail_density.png", width= 7, height= 9.15, dpi = 600, device='png', p)