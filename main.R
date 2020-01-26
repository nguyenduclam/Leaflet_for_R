library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(raster)
#library(dplyr)
library(DT)

### Introduction ###
# Cách 1
m <- leaflet()
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11)

#Cách 2
m <- setView(addTiles(leaflet()), lng = 106.660172, lat = 10.762622, zoom = 11)

#Cách 3
m1 <- leaflet()
m2 <- addTiles(m1)
m3 <- setView(m2, lng = 106.660172, lat = 10.762622, zoom = 11)

#Custom Base Map (Lớp sau sẽ hiển thị và đè lớp trước)
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%

#Adding Shapefiles
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "#000000",
              weight = 1)
HCM_shp <- readOGR("data/HCMC-Districts.shp")

#Address and Coordinates (Cần có key register của Google)
library(ggmap)
register_google("AIzaSyAw3QZ5hhReTIJLSWz7o_582Wm3p6PV-EM")
str = "10 - 12 Dinh Tien Hoang Street, Ho Chi Minh City"
gc = geocode(str, source = "google")

#Circle Marker and Coordinates
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "#000000",
              weight = 1) %>%
  addCircleMarkers(lng = HCM_kcn$Lng,
                   lat = HCM_kcn$Lat,
                   color = "red",
                   weight = 2,
                   radius = 5)
HCM_kcn <- read.csv("data/HCM_KCN.csv")

#Adding Labels to Markets and Clustering(Can Custom HTML)
library(htmltools)
label_KCN <- paste("<span>", HCM_kcn$TenKCN, "</span>", "<br>",
                  "<span>", "Kinh độ", HCM_kcn$Lng, "</span>", "<br>",
                  "<span>", "Vĩ độ", HCM_kcn$Lat, "</span>")
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "#000000",
              weight = 1) %>%
  addCircleMarkers(lng = HCM_kcn$Lng,
                   lat = HCM_kcn$Lat,
                   color = "green",
                   weight = 2,
                   radius = 5,
                   label = lapply(label_KCN, HTML),
                   labelOptions = labelOptions(noHide = T),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
HCM_shp <- readOGR("data/HCMC-Districts.shp")
HCM_kcn <- read.csv("data/HCM_KCN.csv")

#Interactive Controls (Không thể fix tiếng Việt, kể cả khi xuất file HTML)
library(htmltools)
label_KCN <- paste("<span>", HCM_kcn$TenKCN, "</span>", "<br>",
                  "<span>", "Longitude", HCM_kcn$Lng, "</span>", "<br>",
                  "<span>", "Latitude", HCM_kcn$Lat, "</span>")
label_Cho <- paste("<span>", HCM_cho$TEN_CHO, "</span>", "<br>",
                  "<span>", "Longitude", HCM_cho$Lng, "</span>", "<br>",
                  "<span>", "Latitude", HCM_cho$Lat, "</span>")
m <- leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri) %>%
  #addProviderTiles(providers$OpenTopoMap) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "#000000",
              weight = 1) %>%
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
  addLayersControl(overlayGroups = c("KhuCongNghiep", "Cho"),
                   options = layersControlOptions(collapsed = FALSE))
HCM_shp <- readOGR("data/HCMC-Districts.shp")
HCM_kcn <- read.csv("data/HCM_KCN.csv")
HCM_cho <- read.csv("data/HCM_Cho.csv")

#Creating a Choropleth (Lỗi ở phần Highlight)
library(htmltools)
HCM_shp <- readOGR("data/HCMC-Districts.shp")
label_HCMshp <- paste("<span>", HCM_shp$VARNAME_3, "</span>", "<br>",
                  "<span>", "Longitude", HCM_shp$X, "</span>", "<br>",
                  "<span>", "Latitude", HCM_shp$Y, "</span>")

bins <- c(0.08, 0.1, 0.12, 0.2, 0.24, 0.28, 0.3, 0.4, 0.6, 1, 3)
pal <- colorBin("RdYlBu", domain = HCM_shp$Shape_Leng, bins = bins)
m <- leaflet() %>%
  addProviderTiles(providers$Esri) %>%
  setView(lng = 106.660172, lat = 10.762622, zoom = 11) %>%
  addPolygons(data = HCM_shp,
              color = "white",
              weight = 1,
              fillOpacity = 0.8,
              fillColor = pal(HCM_shp$Shape_Leng),
              label = lapply(label_HCMshp, HTML)) %>%
              #highlightOptions = highlightOptions(weight = 5,
              #                                    color = "#666666",
              #                                    dashArray = "",
              #                                    fillOpacity = 0.7,
              #                                    bringToFront = TRUE)
  addLegend(pal = pal,
            values = HCM_shp$Shape_Leng,
            opacity = 0.7,
            position = "topright")


#Total
library(htmltools)
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

### Raster Images (R Studio hiển thị bình thường, không bị hiển thị lỗi)
NTB_shp <- readOGR("data/NTB.shp")
r <- raster("data/NDVI-Q4-2018.tif")
crs(r) <- CRS("+init=epsg:4326")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"),
                    values(r),
                    na.color = "transparent")

m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = 108.384, lat = 13.794, zoom = 6) %>%
  # addPolygons(data = NTB_shp,
  #             color = "blue",
  #             weight = 1,
  #             fillOpacity = 0.2,
  #             label = NTB_shp$VARNAME_1) %>%
  addRasterImage(r,
                 opacity = 0.8,
                 project = T) %>%
  addLegend(pal = pal,
            values = values(r),
            title = "Surface temp")