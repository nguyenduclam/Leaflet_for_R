library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(webshot)

HCM_shp <- readOGR("data/HCMC-Districts.shp")
HCM_kcn <- read.csv("data/HCM_KCN.csv")
HCM_cho <- read.csv("data/HCM_Cho.csv")

label_KCN <- paste("<span>", HCM_kcn$TenKCN, "</span>", "<br>",
                  "<span>", "Longitude", HCM_kcn$Lng, "</span>", "<br>",
                  "<span>", "Latitude", HCM_kcn$Lat, "</span>")
label_Cho <- paste("<span>", HCM_cho$TEN_CHO, "</span>", "<br>",
                  "<span>", "Longitude", HCM_cho$Lng, "</span>", "<br>",
                  "<span>", "Latitude", HCM_cho$Lat, "</span>")
label_HCMshp <- paste("<span>", HCM_shp$VARNAME_3, "</span>", "<br>",
                  "<span>", "Longitude", HCM_shp$X, "</span>", "<br>",
                  "<span>", "Latitude", HCM_shp$Y, "</span>")

bins <- c(0.08, 0.1, 0.12, 0.2, 0.24, 0.28, 0.3, 0.4, 0.6, 1, 3)
pal <- colorBin("RdYlBu", domain = HCM_shp$Shape_Leng, bins = bins)
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "white",
              weight = 1,
              fillOpacity = 0.8,
              fillColor = pal(HCM_shp$Shape_Leng),
              label = lapply(label_HCMshp, HTML)) %>%
  addCircleMarkers(lng = HCM_kcn$Lng,
             lat = HCM_kcn$Lat,
             color = "green",
             weight = 2,
             radius = 5,
             group = "KhuCongNghiep",
             label = lapply(label_KCN, HTML)) %>%
  addCircleMarkers(lng = HCM_cho$Lng,
             lat = HCM_cho$Lat,
             color = "blue",
             weight = 2,
             radius = 5,
             group = "Cho",
             label = lapply(label_Cho, HTML)) %>%
  addLegend(pal = pal,
            values = HCM_shp$Shape_Leng,
            opacity = 0.7,
            position = "topright") %>%
  addLayersControl(overlayGroups = c("KhuCongNghiep", "Cho"),
                   options = layersControlOptions(collapsed = FALSE))

install_phantomjs()
saveWidget(widget = m, file = "index.html")