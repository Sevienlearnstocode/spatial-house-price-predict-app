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

fluidPage(
  tags$head(
    tags$style(HTML("
      body, html, .content, .container-fluid {
        height: 100%;
      }
      .content {
        display: flex;
        flex-direction: column;
      }
      .main-content {
        flex: 1;
      }
      footer {
        text-align: center;
        background-color: #E95420;
        color: white;
        padding: 10px;
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    "))
  ),
  theme = shinytheme("united"),
  navbarPage("Spatial House Price Prediction App",
             # Page header
             tabPanel("Home",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h2("Welcome to Sevien’s Spatial House Price Modeling Web Application!"),
                            tags$p(
                              style = "text-align: justify;",
                              "This is your go-to web application for estimating house prices based on some variables our research found relevant. As owning a home is a primary necessity and a dream for many, our app assists making this dream achievable by providing accurate price predictions. Our application leverages advanced spatial regression models, the Spatial Error Model (SEM), to account for both physical characteristics and geographic location of the house.")
                          ),
                          mainPanel(
                            leafletOutput("map", width = "100%", height = 600)
                          ),
                        )
                      )
             ),
             tabPanel("Geocomputing",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h2("Geocomputing"),
                            tags$p(
                              style = "text-align: justify;",
                              "This Geocomputing page serves the purpose to provide transparency to users regarding the geocomputing process carried out using Geopandas. This page is designed to demonstrate that the data has been properly processed and engineered before being used in training a spatial regression model."),
                            selectInput("dropdown_kecamatan", h3("District (Kecamatan): "), choices = kecamatan_list),
                            selectInput("dropdown_attractions", h3("Attraction: "), choices = attractions_list)
                          ),
                          mainPanel(
                            leafletOutput("peta", width = "100%", height = 600)
                          )
                        )
                      )
             ),
             tabPanel("Conclusion",
                      fluidPage(
                        uiOutput("conclusion")
                      )
             ),
             tabPanel("Analysis",
                      
                      # Input values
                      sidebarPanel(
                        HTML("<h3>Input Values</h3>"),
                        
                        textInput("luas_tanah", "Luas Tanah (m^2):", "60"),
                        textInput("luas_gedung", "Luas Gedung (m^2): ", "50"),
                        textInput("daya_listrik", "Daya Listrik (Watt): ","1300"),
                        textInput("banyak_kamar_tidur", "Banyak Kamar Tidur: ","2"),
                        textInput("banyak_toilet", "Banyak Toilet: ","1"),
                        textInput("lebar_jalan", "Lebar Jalan (X cars): ","2"),
                        
                        # Input Kelurahan/District
                        selectInput(
                          "kecamatan", 
                          label="Kecamatan:",
                          choices = kecamatan_list,
                          selected = "Ampera"
                        ),
                        
                        actionButton("submitbutton", "Predict", class = "btn btn-primary")
                      ),
                      
                      mainPanel(
                        leafletOutput("peta_prediksi", width = "100%", height = 600),
                        tags$label(h3('Harga Prediksi')),
                        verbatimTextOutput('calculation_result')
                      )
             ),
             tabPanel("History",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h2("Prediction History"),
                            tags$p(
                              style = "text-align: justify;",
                              "The Prediction History Page in your web application serves as a comprehensive record of the house price predictions generated by our advanced spatial regression models, specifically the Spatial Error Model (SEM). This page enables users to track and review historical predictions made by the app, providing valuable insights into the accuracy and performance of the predictive models over time. By maintaining a detailed history of predictions, users can assess the reliability of the model outputs, identify trends or patterns in predicted prices, and make informed decisions regarding property investments or transactions. The Prediction History Page enhances the transparency and trustworthiness of our application, empowering users with actionable information for their real estate endeavors."),
                          ),
                          mainPanel(
                            tags$style(".dataTables_wrapper { width: 100% !important; }"),
                            tableOutput("history_table"),
                            div(
                              downloadButton("download_history", "Download History", class = "btn-success"),
                              actionButton("clear_history", "Clear History", class = "btn-danger")
                            )
                          )
                        ))
             ),
             tabPanel("Instructions",
                      div(includeMarkdown("instructions.md"),
                          align="justify")
             ),
             tabPanel("About",
                      div(includeMarkdown("about.md"),
                          align="justify")
             ),
             tags$footer("Sevien - sevienlearnstocode@gmail.com © 2024")
  )
)