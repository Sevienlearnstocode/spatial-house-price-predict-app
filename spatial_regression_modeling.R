# Load necessary libraries
# install.packages("spdep")
# install.packages("spData")
# install.packages("dplyr")
# install.packages("sf")
# install.packages("corrplot")
# install.packages("tmap")
# install.packages("sp")
# install.packages("raster")
# install.packages("dplyr")
# install.packages("spatialreg")

library(spData)
library(spdep)
library(dplyr)
library(sf)
library(corrplot)
library(tmap)
library(sp)
library(raster)
library(dplyr)

# Import data
data1 <- read.csv("geometry_solved_attribute_data_after_preprocessing7.csv")
nrow(data1)

#data2 sudah di-replace dengan versi 7, karena ada kesalahan geometry
data2 <- st_read("geometry_solved_index_geometry_add_attractions7.shp")
nrow(data2)

# Only keep rows that are in both X and Y
dataset <- inner_join(data1, data2, by = "no")
nrow(dataset)

# Assuming 'dataset_rumah' is already loaded as a data frame
# Filter 'dataset_rumah' to only include rows with 'tipe_properti' equal to 'Rumah'
dataset_rumah <- subset(dataset, tipe_properti == 'Rumah')
nrow(dataset_rumah)

# Assuming dataset_rumah is a data frame with geometry column
# Convert dataset_rumah to an sf object and set CRS during conversion
dataset_rumah <- st_as_sf(dataset_rumah, crs = 3395)

# Check for invalid geometries and remove them
dataset_rumah <- dataset_rumah[sf::st_is_valid(dataset_rumah), ]
nrow(dataset_rumah)

# Count the unique geometries
length(unique(st_geometry(dataset_rumah)))

attribute.data <- data.frame(dataset_rumah$harga, dataset_rumah$luas_tanah, dataset_rumah$luas_gedung,
                             dataset_rumah$daya_listrik, dataset_rumah$lebar_jalan, dataset_rumah$banyak_kamar_tidur, 
                             dataset_rumah$banyak_toilet, dataset_rumah$jumlah_populasi, dataset_rumah$halte,
                             dataset_rumah$apotek, dataset_rumah$saluran, dataset_rumah$sungai, 
                             dataset_rumah$cagar_buda, dataset_rumah$danau_embu, dataset_rumah$daya_tarik,
                             dataset_rumah$bandara, dataset_rumah$spbg, dataset_rumah$pemerintah)
attribute.data

# Centering and Scaling
standarisasi <- as.data.frame(scale(attribute.data))
standarisasi

# Save standardized dataset to excel
install.packages("writexl")
library(writexl)

# Save the DataFrame to an Excel file
write_xlsx(as.data.frame(apply(standarisasi, 2, round, 3)), "standarisasi_attribute_data.xlsx")

# Inserting the standardized results of each feature into new variables
Y <- standarisasi$dataset_rumah.harga
x1 <- standarisasi$dataset_rumah.luas_tanah
x2 <- standarisasi$dataset_rumah.luas_gedung
x3 <- standarisasi$dataset_rumah.daya_listrik
x4 <- standarisasi$dataset_rumah.lebar_jalan
x5 <- standarisasi$dataset_rumah.banyak_kamar_tidur
x6 <- standarisasi$dataset_rumah.banyak_toilet
x7 <- standarisasi$dataset_rumah.jumlah_populasi
x8 <- standarisasi$dataset_rumah.halte
x9 <- standarisasi$dataset_rumah.apotek
x10 <- standarisasi$dataset_rumah.saluran
x11 <- standarisasi$dataset_rumah.sungai
x12 <- standarisasi$dataset_rumah.cagar_buda
x13 <- standarisasi$dataset_rumah.danau_embu
x14 <- standarisasi$dataset_rumah.daya_tarik
x15 <- standarisasi$dataset_rumah.bandara
x16 <- standarisasi$dataset_rumah.spbg
x17 <- standarisasi$dataset_rumah.pemerintah

# Default: Pearson's Correlation Coefficient
# correlation coefficients will be displayed as numbers
# in the plot instead of using other visual representations 
# such as colors, circles, or squares
corrplot(cor(standarisasi),method="number")

# Mean
(mean_variabel_harga = mean(dataset_rumah$harga))
(mean_variabel_luas_tanah = mean(dataset_rumah$luas_tanah))
(mean_variabel_luas_gedung = mean(dataset_rumah$luas_gedung))
(mean_variabel_daya_listrik = mean(dataset_rumah$daya_listrik))
(mean_variabel_lebar_jalan = mean(dataset_rumah$lebar_jalan))
(mean_variabel_banyak_kamar_tidur = mean(dataset_rumah$banyak_kamar_tidur))
(mean_variabel_toilet = mean(dataset_rumah$banyak_toilet))
(mean_variabel_jumlah_populasi = mean(dataset_rumah$jumlah_populasi))
(mean_variabel_halte = mean(dataset_rumah$halte))
(mean_variabel_apotek = mean(dataset_rumah$apotek))
(mean_variabel_saluran = mean(dataset_rumah$saluran))
(mean_variabel_sungai = mean(dataset_rumah$sungai))
(mean_variabel_cagar_budaya = mean(dataset_rumah$cagar_buda))
(mean_variabel_danau_embung = mean(dataset_rumah$danau_embu))
(mean_variabel_daya_tarik = mean(dataset_rumah$daya_tarik))
(mean_variabel_gdf_bandara = mean(dataset_rumah$bandar))
(mean_variabel_spbg = mean(dataset_rumah$spbg))
(mean_variabel_kantor_pemerintahan = mean(dataset_rumah$pemerintah))

# Min
(min_variabel_harga = min(dataset_rumah$harga))
(min_variabel_luas_tanah = min(dataset_rumah$luas_tanah))
(min_variabel_luas_gedung = min(dataset_rumah$luas_gedung))
(min_variabel_daya_listrik = min(dataset_rumah$daya_listrik))
(min_variabel_lebar_jalan = min(dataset_rumah$lebar_jalan))
(min_variabel_banyak_kamar_tidur = min(dataset_rumah$banyak_kamar_tidur))
(min_variabel_toilet = min(dataset_rumah$banyak_toilet))
(min_variabel_jumlah_populasi = min(dataset_rumah$jumlah_populasi))
(min_variabel_halte = min(dataset_rumah$halte))
(min_variabel_apotek = min(dataset_rumah$apotek))
(min_variabel_saluran = min(dataset_rumah$saluran))
(min_variabel_sungai = min(dataset_rumah$sungai))
(min_variabel_cagar_budaya = min(dataset_rumah$cagar_buda))
(min_variabel_danau_embung = min(dataset_rumah$danau_embu))
(min_variabel_daya_tarik = min(dataset_rumah$daya_tarik))
(min_variabel_gdf_bandara = min(dataset_rumah$bandar))
(min_variabel_spbg = min(dataset_rumah$spbg))
(min_variabel_kantor_pemerintahan = min(dataset_rumah$pemerintah))

# Max
(max_variabel_harga = max(dataset_rumah$harga))
(max_variabel_luas_tanah = max(dataset_rumah$luas_tanah))
(max_variabel_luas_gedung = max(dataset_rumah$luas_gedung))
(max_variabel_daya_listrik = max(dataset_rumah$daya_listrik))
(max_variabel_lebar_jalan = max(dataset_rumah$lebar_jalan))
(max_variabel_banyak_kamar_tidur = max(dataset_rumah$banyak_kamar_tidur))
(max_variabel_toilet = max(dataset_rumah$banyak_toilet))
(max_variabel_jumlah_populasi = max(dataset_rumah$jumlah_populasi))
(max_variabel_halte = max(dataset_rumah$halte))
(max_variabel_apotek = max(dataset_rumah$apotek))
(max_variabel_saluran = max(dataset_rumah$saluran))
(max_variabel_sungai = max(dataset_rumah$sungai))
(max_variabel_cagar_budaya = max(dataset_rumah$cagar_buda))
(max_variabel_danau_embung = max(dataset_rumah$danau_embu))
(max_variabel_daya_tarik = max(dataset_rumah$daya_tarik))
(max_variabel_gdf_bandara = max(dataset_rumah$bandar))
(max_variabel_spbg = max(dataset_rumah$spbg))
(max_variabel_kantor_pemerintahan = max(dataset_rumah$pemerintah))

# Q1, Q2, and Q3
(quantile_variabel_harga = quantile(dataset_rumah$harga))
(quantile_variabel_luas_tanah = quantile(dataset_rumah$luas_tanah))
(quantile_variabel_luas_gedung = quantile(dataset_rumah$luas_gedung))
(quantile_variabel_daya_listrik = quantile(dataset_rumah$daya_listrik))
(quantile_variabel_lebar_jalan = quantile(dataset_rumah$lebar_jalan))
(quantile_variabel_banyak_kamar_tidur = quantile(dataset_rumah$banyak_kamar_tidur))
(quantile_variabel_toilet = quantile(dataset_rumah$banyak_toilet))
(quantile_variabel_jumlah_populasi = quantile(dataset_rumah$jumlah_populasi))
(quantile_variabel_halte = quantile(dataset_rumah$halte))
(quantile_variabel_apotek = quantile(dataset_rumah$apotek))
(quantile_variabel_saluran = quantile(dataset_rumah$saluran))
(quantile_variabel_sungai = quantile(dataset_rumah$sungai))
(quantile_variabel_cagar_budaya = quantile(dataset_rumah$cagar_buda))
(quantile_variabel_danau_embung = quantile(dataset_rumah$danau_embu))
(quantile_variabel_daya_tarik = quantile(dataset_rumah$daya_tarik))
(quantile_variabel_gdf_bandara = quantile(dataset_rumah$bandar))
(quantile_variabel_spbg = quantile(dataset_rumah$spbg))
(quantile_variabel_kantor_pemerintahan = quantile(dataset_rumah$pemerintah))

# Boxplot
car::Boxplot(Y, col="white")
car::Boxplot(x1, col="white")
car::Boxplot(x2, col="white")
car::Boxplot(x3, col="white")
car::Boxplot(x4, col="white")
car::Boxplot(x5, col="white")
car::Boxplot(x6, col="white")
car::Boxplot(x7, col="white")
car::Boxplot(x8, col="white")
car::Boxplot(x9, col="white")
car::Boxplot(x10, col="white")
car::Boxplot(x11, col="white")
car::Boxplot(x12, col="white")
car::Boxplot(x13, col="white")
car::Boxplot(x14, col="white")
car::Boxplot(x15, col="white")
car::Boxplot(x16, col="white")
car::Boxplot(x17, col="white")

# Data Exploration to Identify Patterns in House Price
# Distribution in DKI Jakarta and Surrounding Areas by 
# Displaying a Map

# Display map
# Set tmap options to check and fix invalid geometries
tmap_options(check.and.fix = TRUE)
tm_shape(dataset_rumah) + tm_polygons() 

# Setting map style
current_style <- tmap_style("white")

# Create a color scale with 4 levels (green, yellow, orange, red)
color_scale <- c("green", "yellow", "red")

# Create a map with a legend based on values and color scale

# Assuming dataset_rumah is your data frame
# Calculate the average harga per kecamatan
average_harga_per_kecamatan <- dataset_rumah %>%
  group_by(kecamatan) %>%
  summarize(avg_harga = mean(harga, na.rm = TRUE))

# Closed on the Right by default [0,3], (3,6], (6,262)
tm_shape(average_harga_per_kecamatan) +
  tm_fill(col = "avg_harga", style = "quantile", n = 3, palette = color_scale, title = "Legend") +
  tm_borders() +
  tm_layout(legend.show = TRUE, legend.position = c("right", "bottom"))

# Assuming dataset_rumah is your data frame
# Group by the 'kota' column and calculate the mean of the 'harga' column
average_harga_per_kota <- dataset_rumah %>%
  group_by(kota) %>%
  summarise(average_harga = mean(harga, na.rm = TRUE))

average_harga_per_kota

# Detailed correlation of each variable
X1 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.luas_tanah)
X2 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.luas_gedung)
X3 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.daya_listrik)
X4 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.lebar_jalan)
X5 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.banyak_kamar_tidur)
X6 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.banyak_toilet)
X7 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.jumlah_populasi)
X8 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.halte)
X9 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.apotek)
X10 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.saluran)
X11 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.sungai)
X12 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.cagar_buda)
X13 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.danau_embu)
X14 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.daya_tarik)
X15 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.bandara)
X16 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.spbg)
X17 = cor.test(standarisasi$dataset_rumah.harga,standarisasi$dataset_rumah.pemerintah)

X1
X2
X3
X4
X5
X6
X7
X8
X9
X10
X11
X12
X13
X14
X15
X16
X17

# Multiple Linear Regression Model
lm_price <- lm(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17)
summary(lm_price)

# OLS assumption test
# 1. Error Independency test
# Durbin watson test
# install.packages("lmtest")
library(lmtest)
dwtest(lm_price, alternative = "two.sided")
# p-value<alpha, reject H0
# Conclusion:
# There is significant evidence to suggest that the residuals are autocorrelated.

# 2. Homoscedasticity (Constant Variance of Errors/Errors Identical)
# Perform the Breusch-Pagan test
bptest(lm_price)
# p-value<alpha, reject H0, (ada heteroskedastisitas)
# Conclusion: 
# house prices varies across different spatial locations

# 3. Errors are normally distributed
# Kolmogorov Smirnov test (type: Lilliefors test)
# install.packages("nortest")
library(nortest)
lillie.test(resid(lm_price))
# p-value<alpha, reject H0, 
# Conclusion:
# error (residual) are not normally distributed

# 4. Multicollinearity test
# install.packages("car")
library(car)
vif(lm_price)

# Spatial Effect Test 
# 1. Spatial Heterogeneity test
bptest(lm_price)
# p-value<alpha, reject H0
# Conclusion:
# Errors are not identical (Heteroscedasticity)
# There is heteroskedasticity across regions

# 2. Spatial Autocorrelation test (Moran's I test)
# Step1: Define neighboring polygons
nb <- poly2nb(dataset_rumah, queen=TRUE)
# Step2: Assign weights to the neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# Step3: Computing the Moran's I statistic
I <- moran(dataset_rumah$harga, lw, length(nb), Szero(lw))[1]
# Step4: Performing a hypothesis test
moran.test(dataset_rumah$harga, lw, alternative="two.sided", zero.policy = TRUE)
# p-value<alpha, reject H0
# Conclusion:
# Spatial Autocorrelation exists

# 3. Spatial Dependency Test (Lagrange Multiplier Test)
# Perform LM tests for spatial lag and spatial error dependence
lm_tests <- lm.RStests(lm_price, lw, test = c("LMlag", "LMerr"))
lm_tests
# Test                         Nilai     p-value
# Lagrange Multiplier (lag)   650.03    <2.2e-16
# Lagrange Multiplier (error) 1185.1    <2.2e-16
#
# Conclusion:
# Both lag and error dependency have p-value < alpha
# Spatial Lag Dependency and Spatial Error Dependency exist
# Need to model with both SAR and SEM.

# 4. Determining the Weight Matrix
centroids <- st_centroid(dataset_rumah)
longlat <- st_coordinates(centroids)

# 4.1. Fixed Distance Matrix
fixed.nb <- spdep::dnearneigh(longlat, d1 = 0, d2 = 200)
fixed.lw <- nb2listw(fixed.nb, style = "W", zero.policy = TRUE)

# 4.2. K-Nearest Neighbor Weight (k=3)
W.knn<-knn2nb(knearneigh(longlat,k=3,longlat=TRUE)) 
W.knn1 <- nb2listw(W.knn,style='W')

# 4.3. Radial Distance Weight (dmax=70)
W.dmax<-dnearneigh(longlat,0,70,longlat=TRUE)
W.dmax.s <- nb2listw(W.dmax,style='W')

# 4.4. Power Distance Weight/Invers Distance Weight
alpha1=1
# Calculating distance matrix
ddist<-dist(longlat) 
m.ddist<-as.matrix(ddist)
# Handling zero or very small values in the distance matrix
m.ddist[m.ddist == 0] <- min(m.ddist[m.ddist > 0]) / 10
W.idw <-1/(m.ddist^alpha1) 
# Normalization 
diag(W.idw)<-0 
rtot<-rowSums(W.idw,na.rm=TRUE)
W.idw.sd<-W.idw/rtot #row-normalized 
W.idw.1 = mat2listw(W.idw.sd,style='W')

alpha2=2
W.idw2 <-2/(m.ddist^alpha2)
# Normalization
diag(W.idw2)<-0 
rtot<-rowSums(W.idw2,na.rm=TRUE)
W.idw.sd2<-W.idw2/rtot #row-normalized
W.idw.2 = mat2listw(W.idw.sd2,style='W') 

# 4.5. Exponential Distance Weight
alpha1=1 
W.exp1<-exp((-alpha1)*m.ddist) #dinormalisasi 
diag(W.exp1)<-0 
rtot<-rowSums(W.exp1,na.rm=TRUE) 
W.exp.sd1<-W.exp1/rtot #row-normalized 
W.ed1 = mat2listw(W.exp.sd1,style='W') 

alpha2=2
W.exp2<-exp((-alpha2)*m.ddist) #dinormalisasi 
diag(W.exp2)<-0 
rtot<-rowSums(W.exp2,na.rm=TRUE) 
W.exp.sd2<-W.exp2/rtot #row-normalized
W.ed2 = mat2listw(W.exp.sd2,style='W') 

# 4.6. Contiguity-Based Weights
# Rook
cont.nb.rook <- poly2nb(dataset_rumah, queen=FALSE)
W.rook.s <- nb2listw(cont.nb.rook, style="W", zero.policy=TRUE)
# Queen
cont.nb.queen <- poly2nb(dataset_rumah, queen=TRUE)
W.queen.s <- nb2listw(cont.nb.queen, style="W", zero.policy=TRUE)


# Pemilihan Matriks Bobot
MI.fixed <- moran(dataset_rumah$harga, fixed.lw, n=length(fixed.lw$neighbours), S0=Szero(fixed.lw))
MI.knn <- moran(dataset_rumah$harga, W.knn1, n=length(W.knn1$neighbours), S0=Szero(W.knn1))
MI.radial <- moran(dataset_rumah$harga, W.dmax.s, n=length(W.dmax.s$neighbours), S0=Szero(W.dmax.s))
MI.power1 <- moran(dataset_rumah$harga, W.idw.1, n=length(W.idw.1$neighbours), S0=Szero(W.idw.1))
MI.power2 <- moran(dataset_rumah$harga, W.idw.2, n=length(W.idw.2$neighbours), S0=Szero(W.idw.2))
MI.exp1 <- moran(dataset_rumah$harga, W.ed1, n=length(W.ed1$neighbours), S0=Szero(W.ed1))
MI.exp2 <- moran(dataset_rumah$harga, W.ed2, n=length(W.ed2$neighbours), S0=Szero(W.ed2))
MI.rook <- moran.test(dataset_rumah$harga, W.rook.s, randomisation = TRUE, zero.policy = TRUE)
MI.queen <- moran.test(dataset_rumah$harga, W.queen.s, randomisation = TRUE, zero.policy = TRUE)

moranindeks<-data.frame(
  "Matriks Bobot"=c("Fixed distance weight","KNN (k=5)", "Radial distance weight", "Power distance weight (alpha=1)", "Power distance weight (alpha=2)", "Exponential distance weight (alpha=1)", "Exponential distance weight (alpha=2)", "Rook Contiguity", "Queen Contiguity"),
  "Nilai Indeks Moran"=c(MI.fixed$I, MI.knn$I, MI.radial$I, MI.power1$I, MI.power2$I, MI.exp1$I, MI.exp2$I, 0.2504779, 0.2499633))

moranindeks

# Export Optimum Weight Matrix
Woptimum <- W.idw.2
WoptimumMatrix <- matrix(unlist(Woptimum$weights, use.names = FALSE), 
                         nrow=2195, ncol=2195, byrow=TRUE)
WoptimumMatrix <- as.data.frame(WoptimumMatrix)

install.packages("writexl")
library(writexl)
# Save weights to an Excel file
write_xlsx(WoptimumMatrix, "WOptinum_weights.xlsx")

# 5. Spatial Autoregressive (SAR) and Spatial Error Model (SEM) Modeling
library(spatialreg)
# Fit SAR
sar_model <- lagsarlm(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17,
                      listw = Woptimum)
summary(sar_model)

sar_model2 <- lagsarlm(Y~x1+x2+x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17,
                       listw = Woptimum)
summary(sar_model2)

# Fit SEM
sem_model <- errorsarlm(Y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17,
                        listw = Woptimum)
summary(sem_model)

sem_model2 <- errorsarlm(Y~x1+x2+x3+x4+x5+x6+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17,
                         listw = Woptimum)
summary(sem_model2)

# 6. Determining the best model (R^2, AIC dan BIC)
# 6.1. R^2 for SAR model
# Extract residuals
residuals <- residuals(sar_model2)
# Compute RSS
RSS <- sum(residuals^2)

# Compute TSS
observed_values <- Y
mean_observed_values <- mean(observed_values)
TSS <- sum((observed_values - mean_observed_values)^2)

# Compute pseudo R-squared
pseudo_R2_sar <- 1 - (RSS / TSS)
pseudo_R2_sar

# 6.2. R^2 for SEM model
# Extract residuals
residuals <- residuals(sem_model2)

# Compute RSS
RSS <- sum(residuals^2)

# Compute pseudo R-squared
pseudo_R2_sem <- 1 - (RSS / TSS)
pseudo_R2_sem

# 6.3. AIC of SAR and SEM
AIC(sar_model2, sem_model2)

# 6.4. BIC of SAR and SEM
BIC(sar_model2, sem_model2)