# install.packages("spData")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("dplyr")
# install.packages("spdep")
# install.packages("scales")
# install.packages("rmarkdown")
# install.packages("spatialreg")
# install.packages("leaflet")
# install.packages ("purrr")
# install.packages("markdown")
# install.packages("readxl")
# install.packages("DBI")
# install.packages("RSQLite")

library(spData)
library(shiny)
library(shinythemes)
library(dplyr)
library(spdep)
library(scales)
library(rmarkdown)
library(spatialreg)
library(leaflet)
library(purrr)
library(readxl)
library(DBI)
library(RSQLite)

sem_model <- readRDS("sem_model11.rds")

gdf_halte <- st_read("HALTE_1K.gdb.zip")
common_crs <- st_crs(gdf_halte)
gdf_apotek <- st_read("APOTEK_1K.gdb.zip")
gdf_saluran <- st_read("SALURAN_1K.gdb.zip")
gdf_sungai <- st_read("SUNGAI_1K.gdb.zip")
gdf_danau_embu <- st_read("DANAU_EMBUNG_WADUK_1K.gdb.zip")
gdf_cagar_budaya <- st_read("CAGAR_BUDAYA_1K.gdb.zip")
gdf_daya_tarik_pariwisata <- st_read("DAYATARIKPARIWISATA_1K.gdb.zip")
gdf_bandara <- st_read("BANDARA_1K.gdb.zip")
gdf_SPBG <- st_read("SPBG_1K.gdb.zip")
gdf_kantor_pemerintah <- st_read("PEMERINTAHAN_1K.gdb.zip")

shade_color <- "#FFD700"
mark_color <- "purple"

# Import data
data1 <- read.csv("geometry_solved_attribute_data_after_preprocessing7.csv")
head(data1)

data2 <- st_read("geometry_solved_index_geometry_add_attractions7.shp")
head(data2)

# Merge data1 and data2
dataset <- inner_join(data1, data2, by = "no")
nrow(dataset)

# Filter dataset to include only 'Rumah'
dataset_rumah <- subset(dataset, tipe_properti == 'Rumah')

# Convert dataset_rumah to a spatial data frame
dataset_rumah <- st_as_sf(dataset_rumah, crs=common_crs)

# Check for invalid geometries and remove them
dataset_rumah <- dataset_rumah[sf::st_is_valid(dataset_rumah), ]

# Group by kecamatan and calculate the average harga
average_harga_per_m_kubik_per_kecamatan <- dataset_rumah %>%
  group_by(kecamatan) %>%
  summarise(harga_asli = mean(harga / (luas_tanah + luas_gedung), na.rm = TRUE))

# Calculates the average housing prices per kecamatan based on various features
average_harga_per_kecamatan <- dataset_rumah %>%
  select(kecamatan, halte, apotek, saluran, sungai, cagar_buda, danau_embu, daya_tarik, bandara, spbg, pemerintah, geometry, harga) %>%
  group_by(kecamatan, halte, apotek, saluran, sungai, cagar_buda, danau_embu, daya_tarik, bandara, spbg, pemerintah, geometry) %>%
  summarise(harga_asli = mean(harga, na.rm = TRUE))

# Create a list called attractions_list
attractions_list <- sort(c("Halte", "Apotek", "Saluran Air", "Sungai", "Cagar Budaya", "Danau/Embung/Waduk","Daya Tarik Pariwisata", "Bandara", "SPBG", "Kantor Pemerintahan"))

# Read and transform NJOP column to numeric
njop_per_kecamatan <- st_read("kecamatan_njop_geometry1.shp")
njop_per_kecamatan$NJOP <- as.numeric(njop_per_kecamatan$NJOP)

# Identify unique kecamatan values
unique_kecamatan <- unique(dataset_rumah$kecamatan)

# Order average_harga_per_kecamatan dataframe by kecamatan column
relevant_attraction_data <- average_harga_per_kecamatan %>% arrange(kecamatan)

# Create a list called kecamatan_list based on kecamatan column
# in relevant_attraction_data dataframe
kecamatan_list <- as.list(relevant_attraction_data$kecamatan)

# Convert the first alphabet of each value and the first alphabet after ' ' to uppercase
kecamatan_list <- lapply(kecamatan_list, function(x) {
  words <- strsplit(x, " ")[[1]]
  words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
  return(paste(words, collapse = " "))
})

real_values <- data.frame(
  x1 = as.numeric(dataset_rumah$luas_tanah),
  x2 = as.numeric(dataset_rumah$luas_gedung),
  x3 = as.numeric(dataset_rumah$daya_listrik),
  x5 = as.numeric(dataset_rumah$banyak_kamar_tidur),
  x6 = as.numeric(dataset_rumah$banyak_toilet),
  x4 = as.numeric(dataset_rumah$lebar_jalan),
  x8 = as.numeric(dataset_rumah$halte),
  x9 = as.numeric(dataset_rumah$apotek),
  x10 = as.numeric(dataset_rumah$saluran),
  x11 = as.numeric(dataset_rumah$sungai),
  x12 = as.numeric(dataset_rumah$cagar_buda),
  x13 = as.numeric(dataset_rumah$danau_embu),
  x14 = as.numeric(dataset_rumah$daya_tarik),
  x15 = as.numeric(dataset_rumah$bandara),
  x16 = as.numeric(dataset_rumah$spbg),
  x17 = as.numeric(dataset_rumah$pemerintah)
)

mean_train <- colMeans(real_values)
sd_train <- apply(real_values, 2, sd)

mean_y <- mean(dataset_rumah$harga)
sd_y <- sd(dataset_rumah$harga)

# Connect to the SQLite database (create if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), "predictions.sqlite")

# Create table predictions
dbExecute(con, "
CREATE TABLE IF NOT EXISTS predictions (
  Luas_Tanah REAL,
  Luas_Gedung REAL,
  Daya_Listrik REAL,
  Banyak_Kamar_Tidur REAL,
  Banyak_Toilet REAL,
  Lebar_Jalan REAL,
  Kecamatan TEXT,
  Predicted_Price REAL
)")


shinyServer(function(input, output, session){
  
  # Read data from SQLite table predictions to db_data
  db_data <- reactive({
    dbGetQuery(con, "SELECT * FROM predictions") %>%
      rename(
        "Luas Tanah" = "Luas_Tanah",
        "Luas Gedung" = "Luas_Gedung",
        "Daya Listrik" = "Daya_Listrik",
        "Banyak Kamar Tidur" = "Banyak_Kamar_Tidur",
        "Banyak Toilet" = "Banyak_Toilet",
        "Lebar Jalan" = "Lebar_Jalan",
        "Kecamatan" = "Kecamatan",
        "Predicted Price" = "Predicted_Price"
      ) %>%
      mutate(`Predicted Price` = paste0("Rp ", comma(`Predicted Price`)))
  })
  
  output$map <- renderLeaflet({
    
    # Create the color palette function for avg_harga
    warnaPallet_avg_harga <- colorNumeric(c("red", "pink", "purple", "blue", "cyan", "darkgreen", "green", "lightgreen", "yellow", "orange"), domain = average_harga_per_m_kubik_per_kecamatan$harga_asli)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = average_harga_per_m_kubik_per_kecamatan,
        fillColor = ~warnaPallet_avg_harga(harga_asli),
        fillOpacity = 0.5,
        weight = 1,
        color = "black",
        label = ~paste("Harga Properti: Rp", scales::comma(harga_asli)),
        group = "Harga Properti"
      ) %>%
      addPolygons(
        data = njop_per_kecamatan,
        fillColor = ~warnaPallet_avg_harga(NJOP),
        fillOpacity = 0.5,
        weight = 1,
        color = "black",
        label = ~paste("Harga NJOP: Rp", scales::comma(NJOP)),
        group = "NJOP"
      ) %>%
      addLegend(
        "bottomright",
        pal = warnaPallet_avg_harga,
        values = average_harga_per_m_kubik_per_kecamatan$harga_asli,
        title = "Harga",
        opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("Harga Properti", "NJOP"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$dropdown_kecamatan, {
    selected_kecamatan <- input$dropdown_kecamatan
    
    # Filter dataset_rumah based on selected kecamatan
    selected_data <- average_harga_per_kecamatan %>%
      filter(tolower(kecamatan) == tolower(selected_kecamatan))
    
    # Create the color palette function
    warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
    
    # Render the map
    output$peta <- renderLeaflet({
      leaflet(data = selected_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~warnaPallet(harga_asli),
          fillOpacity = 0.5,
          weight = 1,
          color = "black",
          label = ~paste("Harga: Rp", scales::comma(harga_asli))
        )
    })
  })
  
  observeEvent(input$dropdown_attractions, {
    if (input$dropdown_attractions == "Halte") {
      
      # Filter gdf_halte based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_halte[st_intersects(gdf_halte, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Apotek") {
      
      # Filter gdf_apotek based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_apotek[st_intersects(gdf_apotek, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Saluran Air") {
      
      # Filter gdf_saluran based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_lines <- gdf_saluran[st_intersects(gdf_saluran, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_lines)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addPolylines(
              data = st_intersection(filtered_lines, selected_data),
              color = "blue",
              weight = 2,
              label = ~paste(filtered_lines$NAMOBJ)
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Sungai") {
      
      # Filter gdf_sungai based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_lines <- gdf_sungai[st_intersects(gdf_sungai, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_lines)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addPolylines(
              data = st_intersection(filtered_lines, selected_data),
              color = "#0000FF",
              weight = 2,
              label = ~paste(filtered_lines$NAMOBJ)
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Danau/Embung/Waduk") {
      
      # Filter gdf_danau_embu based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_lines <- gdf_danau_embu[st_intersects(gdf_danau_embu, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_lines)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addPolygons(
              data = st_intersection(filtered_lines, selected_data),
              fillColor = "cyan",
              fillOpacity = 1,
              weight = 2,
              label = ~paste(filtered_lines$NAMOBJ)
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Cagar Budaya") {
      
      # Filter gdf_cagar_budaya based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_cagar_budaya[st_intersects(gdf_cagar_budaya, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)  # Assuming 'ID'
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Daya Tarik Pariwisata") {
      
      # Filter gdf_daya_tarik_pariwisata based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_daya_tarik_pariwisata[st_intersects(gdf_daya_tarik_pariwisata, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)  # Assuming 'ID'
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Bandara") {
      
      # Filter gdf_bandara based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_bandara[st_intersects(gdf_bandara, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)  # Assuming 'ID'
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "SPBG") {
      
      # Filter gdf_bandara based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_SPBG[st_intersects(gdf_SPBG, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)  # Assuming 'ID'
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
    else if (input$dropdown_attractions == "Kantor Pemerintahan") {
      
      # Filter gdf_kantor_pemerintah based on selected kecamatan and polygons in average_harga_per_kecamatan
      selected_kecamatan <- input$dropdown_kecamatan
      selected_data <- average_harga_per_kecamatan %>%
        filter(tolower(kecamatan) == tolower(selected_kecamatan))
      
      print(selected_data)
      
      filtered_points <- gdf_kantor_pemerintah[st_intersects(gdf_kantor_pemerintah, selected_data, sparse = FALSE), ]
      
      warnaPallet <- colorNumeric(c(shade_color), domain = selected_data$harga_asli)
      
      # Render the map with filtered points
      output$peta <- renderLeaflet({
        if (nrow(filtered_points)!=0) {
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            ) %>%
            addCircleMarkers(
              data = filtered_points,
              radius = 5,
              color = mark_color,
              label = ~paste(filtered_points$NAMOBJ)  # Assuming 'ID'
            )
        }
        else{
          leaflet() %>%
            addTiles() %>%
            addPolygons(
              data = selected_data,
              fillColor = ~warnaPallet(harga_asli),
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = ~paste("Harga: Rp", scales::comma(harga_asli))
            )
        }
      })
    }
  })
  
  output$conclusion <- renderUI({
    HTML("
    <h2>Conclusion</h2>
    <p>By April 4th, 2024, an analysis of the variables affecting house prices has revealed both favorable and unfavorable impacts. Specifically, the standardized coefficients for each variable indicate their relative effect on house prices.</p>
    <p>Factors that contribute to higher house prices include:</p>
    <ul>
      <li>Land area (x1, coefficient: 0.641)</li>
      <li>Electrical power (x3, coefficient: 0.165)</li>
      <li>Road width (x4, coefficient: 0.036)</li>
      <li>Number of toilets (x6, coefficient: 0.287)</li>
      <li>Number of bus stops (x8, coefficient: 0.572)</li>
      <li>Number of rivers (x11, coefficient: 0.359)</li>
      <li>Number of tourist attractions (x14, coefficient: 0.216)</li>
      <li>Number of gas stations (x16, coefficient: 0.273)</li>
      
    </ul>
    <p>These positive coefficients suggest that larger land area, more toilets, closer bus stops, and nearby public gas stations are associated with increased house prices.</p>
    <p>Conversely, several variables are linked to lower house prices:</p>
    <ul>
      <li>Size of the building (x2, coefficient: -0.122)</li>
      <li>Number of bedrooms (x5, coefficient: -0.277)</li>
      <li>Number of pharmacies (x9, coefficient: -0.395)</li>
      <li>Number of water channels (x10, coefficient: -0.201)</li>
      <li>Number of cultural heritages (x12, coefficient: -0.135)</li>
      <li>Number of lakes/reservouirs (x13, coefficient: -0.273)</li>
      <li>Number of airports (x15, coefficient: -0.254)</li>
      <li>Number of government buildings (x17, coefficient: -0.163)</li>
    </ul>
    <p>This implies that these factors are associated with decreased house prices.</p>
    <p>These findings highlight the complex interplay of various factors in determining house prices. While amenities and infrastructure such as land area, toilets, and public transportation contribute to higher house prices, other factors such as proximity to pharmacies, water channels, cultural heritage sites, and government buildings appear to reduce property values.</p>
    <p>This comprehensive analysis provides valuable insights for potential homeowners and real estate developers in making informed decisions regarding property investments.</p>
  ")
  })
  
  options(scipen = 10)
  
  # Input Data
  datasetInput <- reactive({ 
    
    # Assuming input$kecamatan holds the value for kecamatan
    selected_kecamatan <- tolower(input$kecamatan)
    
    # Filter relevant_attraction_data based on kecamatan
    filtered_data <- relevant_attraction_data %>%
      filter(kecamatan == selected_kecamatan) %>%
      select(halte,
             apotek,
             saluran,
             sungai,
             cagar_buda,
             danau_embu,
             daya_tarik,
             bandara,
             spbg,
             pemerintah)
    
    # Render the map with filtered points
    output$peta_prediksi <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = filtered_data,
          fillColor = "green",
          fillOpacity = 0.5,
          weight = 1,
          color = "black"
        )
    })
    
    # Ensure all input values are numeric
    input_values <- data.frame(
      x1 = as.numeric(input$luas_tanah),
      x2 = as.numeric(input$luas_gedung),
      x3 = as.numeric(input$daya_listrik),
      x5 = as.numeric(input$banyak_kamar_tidur),
      x6 = as.numeric(input$banyak_toilet),
      x4 = as.numeric(input$lebar_jalan)
    )

    # Check for NA values and replace them with 0
    input_values[is.na(input_values)] <- 0
    
    filtered_data_values <- data.frame(
      x8 = as.numeric(filtered_data$halte),
      x9 = as.numeric(filtered_data$apotek),
      x10 = as.numeric(filtered_data$saluran),
      x11 = as.numeric(filtered_data$sungai),
      x12 = as.numeric(filtered_data$cagar_buda),
      x13 = as.numeric(filtered_data$danau_embu),
      x14 = as.numeric(filtered_data$daya_tarik),
      x15 = as.numeric(filtered_data$bandara),
      x16 = as.numeric(filtered_data$spbg),
      x17 = as.numeric(filtered_data$pemerintah)
    )
    
    
    
    # Check for NA values and replace them with 0
    filtered_data_values[is.na(filtered_data_values)] <- 0
    
    # Combine input values and filtered data values
    combined_data <- cbind(input_values, filtered_data_values)
    
    # Ensure combined_data has the correct number of rows and columns
    combined_data <- combined_data[1, , drop = FALSE]

    # Convert lag_features to a dataframe
    combined_data_df <- as.data.frame(combined_data, stringsAsFactors = FALSE)
    
    # Standardize the real data
    real_input <- as.matrix(combined_data_df)
    standardized_input <- (real_input - mean_train) / sd_train
    
    # Multiply each element in coef_matrix with each column in combined_data_df
    spatial_error <- as.numeric(sem_model$lambda) * sum(as.matrix(sem_model$weights) * residuals(sem_model))
    result <- pmax((sum(coef(sem_model)[colnames(standardized_input)] * standardized_input)+spatial_error),-0.35)
    
    # Sum the resulting values
    predicted_value <- result * sd_y + mean_y
    
    # Save data to SQLite database
    new_data <- data.frame(
      Luas_Tanah = input_values$x1,
      Luas_Gedung = input_values$x2,
      Daya_Listrik = input_values$x3,
      Banyak_Kamar_Tidur = input_values$x5,
      Banyak_Toilet = input_values$x6,
      Lebar_Jalan = input_values$x4,
      Kecamatan = selected_kecamatan,
      Predicted_Price = predicted_value
    )
    
    dbWriteTable(con, "predictions", new_data, append = TRUE, row.names = FALSE)
    
    return(predicted_value)
  })
  
  # Read data from SQLite database and render it in the History tab
  output$history_table <- renderTable({
    db_data()
  })
  
  # Download handler for history data
  output$download_history <- downloadHandler(
    filename = function() {
      paste("history-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(db_data(), file, row.names = FALSE)
    }
  )
  
  # Event handler for the Delete History button
  observeEvent(input$clear_history, {
    dbExecute(con, "DELETE FROM predictions")
    output$history_table <- renderTable({
      db_data()
    })
  })
  
  # Status/Output Text Box
  output$calculation_result <- renderPrint({
    if (input$submitbutton > 0) {
      isolate({
        prediction <- datasetInput()
        formatted_prediction <- paste("Rp ", comma(prediction))
        cat(formatted_prediction)
      })
    } else {
      return("Server is ready for calculation")
    }
  })
})