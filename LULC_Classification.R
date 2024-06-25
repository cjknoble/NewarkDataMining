### LULC 
# Load the raster packages:

library(raster)
library(sf)
library(RStoolbox)

################################################################################
### SENTINEL 2017-2022 ###
################################################################################

# Set the working directory
setwd("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Sentinel 2")  # Replace "path_to_folder_containing_rasters" with the actual folder path

### TRAINING SAMPLE

# Read the land cover tif file that has already classified
SentClsfd <- brick("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/QGIS Project - Paper #3 - Sys Dyn/LULC Training Inputs/LULC_Classification4.tif")

# 0 - Unclassified 
# 1 - Rivers
# 2 - Ponds
# 3 - Buildings 
# 4 - Roads/Pavement 
# 5 - Residence 
# 6 - Trees/Greenery
# 7 - Marsh
# 8 - Grass
# 9 - Open Space

# Set the class names and class numbers correspondingly, with different colors:
SentClsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

SentClsfdclassdf <- data.frame(classvalue1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), classnames1 = SentClsfdclass)
SentClsfdclasscolors <- rainbow(10)  # use the rainbow palette color with 14 different colors.

# The 0 value of the dataset are no-data. I need to set them to NA
SentClsfd[SentClsfd==0]<-NA
SentClsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

# Sample from the layer:
# You cannot sample from a rasterbrick object directly, you have to extract the raster layer out from the brick, even if there is only one layer in the brick:
SentClsfd<-SentClsfd[[1]]

# Ratify this raster layer:
SentClsfd <-ratify(SentClsfd)
SentClsfdrat <-levels(SentClsfd)[[1]]
SentClsfdrat$landcover <- SentClsfdclass
levels(SentClsfd) <-SentClsfdrat
#SentClsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

set.seed(99)
samp <- sampleStratified(SentClsfd, size = 200, na.rm = TRUE, sp = TRUE)

# See the samples:
table(samp$LULC_Classification2)

# Plot
library(rasterVis)
plt <- levelplot(SentClsfd, col.regions = SentClsfdclasscolors, main = 'Distribution of TrainingSites')
print(plt + latticeExtra::layer(sp.points(samp, pch = 3, cex = 0.5, col = 1)))

### CLASSIFYING
bndry <- st_read("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Newark Boundary Shapefile/Newark Boundary.shp")

snt20221007 <- stack("Sentinel2_2022-10-07.tif")
snt20210902 <- stack("Sentinel2_2021-09-02.tif")
snt20200825 <- stack("Sentinel2_2020-08-25.tif")
snt20190903 <- stack("Sentinel2_2019-09-03.tif")
snt20180824 <- stack("Sentinel2_2018-08-24.tif")
snt20170826 <- stack("Sentinel2_2017-08-26.tif")

RstrsList <- list("snt20221007", "snt20210902","snt20200825","snt20190903","snt20180824","snt20170826")
#RstrsList <- list("snt20221029", "snt20211106", "snt20201106", "snt20191102", "snt20181030", "snt20171018") #Be sure that list is in temporal order from top (2022) to bottom (2010 for example) - otherwise naming convention at the end of for loop is wrong

for (i in 1:length(RstrsList)){
  sntRstr <- get(paste0(RstrsList[i]))
  sntRstr <- crop(sntRstr, bndry)
  sntRstr <- mask(sntRstr, bndry)
  
  # Give its bands appropriate names:
  names(sntRstr) <- c('Ublue', 'blue', 'green', 'red', 'VNIR1', 'VNIR2',  'VNIR3', 'VNIR4', 'VNIR5', 'SWIR1','SWIR2','SWIR3', 'SWIR4')
  
  # The two rasters have different projection, they must be aligned:
  # I will also keep the original projected dataset
  SentClsfd.prj<-projectRaster(SentClsfd, sntRstr, method = "ngb")
  
  # Sample it again:
  samp.prj <- sampleStratified(SentClsfd.prj, size = 200, na.rm = TRUE, sp = TRUE)
  
  # Extract the layer values for the locations
  
  sampvals <- extract(sntRstr, samp.prj, df = TRUE)
  sampvals.sp <- extract(sntRstr, samp.prj, df = TRUE, sp = TRUE)
  sampvals <- sampvals[, -1]
  sampvals.sp <- sampvals.sp[, -1]
  
  # combine the class information with extracted values
  sampdata <- data.frame(classvalue = samp.prj@data, sampvals) 
  
  # The superClass requires a spatialPointDataframe, so when extract the training sites, setting "sp = TRUE" is critical:
  # Random Forest Classification
  wv <- superClass(sntRstr, sampvals.sp, verbose = T, model = "rf", responseCol = "LULC_Classification4", filename = paste0("Classified_", RstrsList[i])) 
  name <- paste0("wv", 23-i, ".rf")
  assign(name, wv)
  }

# Create empy master pixel freq table 
wvdf.master <- data.frame(NA)
#SentClsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")
wvdf.master[1:9,] <- SentClsfdclass
colnames(wvdf.master) <- c("Value")

# Create df counting pixel by type and calculating perc of total
for (i in 17:22){
  dfname <- paste0("wv", i, ".rf") #Create superClass object Name
  wvdf <- data.frame(freq(get(dfname)$map)) #Create df with pixel freq by type
  wvdf[3] <- NA #Create empty third column for perc 
  colnames(wvdf) <- c("Value", paste0("Pixel Count - ", i), paste0("Perc of Total Pixels - ", i)) #Rename cols based on year
  wvdf <- wvdf[-nrow(wvdf), ] #Remove NA row count
  
  for (i in 1:nrow(wvdf)){
    wvdf[i,3] <- wvdf[i,2]/sum(wvdf[2]) #Calculate percs
  }
  
  wvdf.master <- cbind(wvdf.master, wvdf[, 2], wvdf[, 3]) #Add data to master df
  colnames(wvdf.master)[ncol(wvdf.master) - 1] <- c(colnames(wvdf[ncol(wvdf) -1])) #Rename 
  colnames(wvdf.master)[ncol(wvdf.master)] <- c(colnames(wvdf[ncol(wvdf)]))
}

wvdf.master.sntl <- wvdf.master


################################################################################
### LANDSAT 8/9 2013-2016 ###
################################################################################

# Set the working directory
setwd("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Landsat/LANDSAT 8-9 - 2013-2017")  # Replace "path_to_folder_containing_rasters" with the actual folder path

### TRAINING SAMPLE

# Read the land cover tif file that has already classified
Lndst89Clsfd <- brick("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/QGIS Project - Paper #3 - Sys Dyn/LULC Training Inputs/LULC_Landsat_8_9_Classification2.tif")

# 0 - Unclassified 
# 1 - Rivers
# 2 - Ponds
# 3 - Buildings 
# 4 - Roads/Pavement 
# 5 - Residence 
# 6 - Trees/Greenery
# 7 - Marsh
# 8 - Grass
# 9 - Open Space

# Set the class names and class numbers correspondingly, with different colors:
Lndst89Clsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

Lndst89Clsfdclassdf <- data.frame(classvalue1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), classnames1 = Lndst89Clsfdclass)
Lndst89Clsfdclasscolors <- rainbow(10)  # use the rainbow palette color with 14 different colors.

# The 0 value of the dataset are no-data. I need to set them to NA
Lndst89Clsfd[Lndst89Clsfd==0]<-NA
Lndst89Clsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

# Sample from the layer:
# You cannot sample from a rasterbrick object directly, you have to extract the raster layer out from the brick, even if there is only one layer in the brick:
Lndst89Clsfd<-Lndst89Clsfd[[1]]

# Ratify this raster layer:
Lndst89Clsfd <-ratify(Lndst89Clsfd)
Lndst89Clsfdrat <-levels(Lndst89Clsfd)[[1]]
Lndst89Clsfdrat$landcover <- Lndst89Clsfdclass
levels(Lndst89Clsfd) <-Lndst89Clsfdrat
#Lndst89Clsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

set.seed(99)
samp <- sampleStratified(Lndst89Clsfd, size = 200, na.rm = TRUE, sp = TRUE)

# See the samples:
table(samp$LULC_Landsat_8_9_Classification2)

# Plot
library(rasterVis)
plt <- levelplot(Lndst89Clsfd, col.regions = Lndst89Clsfdclasscolors, main = 'Distribution of TrainingSites')
print(plt + latticeExtra::layer(sp.points(samp, pch = 3, cex = 0.5, col = 1)))

### CLASSIFYING
bndry <- st_read("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Newark Boundary Shapefile/Newark Boundary.shp")

lndst89_20130820 <- stack("Landsat8_9_2013-08-20.tif")
lndst89_20140917 <- stack("Landsat8_9_2014-09-17.tif")
lndst89_20151006 <- stack("Landsat8_9_2015-10-06.tif")
lndst89_20161015 <- stack("Landsat8_9_2016-10-15.tif")

RstrsList <- list("lndst89_20161015", "lndst89_20151006", "lndst89_20140917", "lndst89_20130820") #Be sure that list is in temporal order from top (2022) to bottom (2010 for example) - otherwise naming convention at the end of for loop is wrong

for (i in 1:length(RstrsList)){
  lndstRstr <- get(paste0(RstrsList[i]))
  lndstRstr <- crop(lndstRstr, bndry)
  lndstRstr <- mask(lndstRstr, bndry)
  
  # Give its bands appropriate names:
  names(lndstRstr) <- c('CstlArsl', 'blue', 'green', 'red', 'NIR', 'SWIR1',  'SWIR2')
  
  # The two rasters have different projection, they must be aligned:
  # I will also keep the original projected dataset
  Lndst89Clsfd.prj<-projectRaster(Lndst89Clsfd, lndstRstr, method = "ngb")
  
  # Sample it again:
  samp.prj <- sampleStratified(Lndst89Clsfd.prj, size = 200, na.rm = TRUE, sp = TRUE)
  
  # Extract the layer values for the locations
  
  sampvals <- extract(lndstRstr, samp.prj, df = TRUE)
  sampvals.sp <- extract(lndstRstr, samp.prj, df = TRUE, sp = TRUE)
  sampvals <- sampvals[, -1]
  sampvals.sp <- sampvals.sp[, -1]
  
  # combine the class information with extracted values
  sampdata <- data.frame(classvalue = samp.prj@data, sampvals) 
  
  # The superClass requires a spatialPointDataframe, so when extract the training sites, setting "sp = TRUE" is critical:
  # Random Forest Classification
  wv <- superClass(lndstRstr, sampvals.sp, verbose = T, model = "rf", responseCol = "LULC_Landsat_8_9_Classification2", filename = paste0("Classified_", RstrsList[i])) 
  name <- paste0("wv", 17-i, ".rf")
  assign(name, wv)
}


# Create empy master pixel freq table 
wvdf.master <- data.frame(NA)
#Lndst89Clsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")
wvdf.master[1:9,] <- Lndst89Clsfdclass
colnames(wvdf.master) <- c("Value")

# Create df counting pixel by type and calculating perc of total
for (i in 13:16){
  dfname <- paste0("wv", i, ".rf") #Create superClass object Name
  wvdf <- data.frame(freq(get(dfname)$map)) #Create df with pixel freq by type
  wvdf[3] <- NA #Create empty third column for perc 
  colnames(wvdf) <- c("Value", paste0("Pixel Count - ", i), paste0("Perc of Total Pixels - ", i)) #Rename cols based on year
  wvdf <- wvdf[-nrow(wvdf), ] #Remove NA row count
  
  for (i in 1:nrow(wvdf)){
    wvdf[i,3] <- wvdf[i,2]/sum(wvdf[2]) #Calculate percs
  }
  
  wvdf.master <- cbind(wvdf.master, wvdf[, 2], wvdf[, 3]) #Add data to master df
  colnames(wvdf.master)[ncol(wvdf.master) - 1] <- c(colnames(wvdf[ncol(wvdf) -1])) #Rename 
  colnames(wvdf.master)[ncol(wvdf.master)] <- c(colnames(wvdf[ncol(wvdf)]))
}

wvdf.master.lndst89 <- wvdf.master

################################################################################
### LANDSAT 5 2000-2011 ###
################################################################################

# Set the working directory
setwd("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Landsat/Landsat 5 2000-2012")  # Replace "path_to_folder_containing_rasters" with the actual folder path

### TRAINING SAMPLE

# Read the land cover tif file that has already classified
Lndst5Clsfd <- brick("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/QGIS Project - Paper #3 - Sys Dyn/LULC Training Inputs/LULC_Landsat_5_Classification1.tif")

# 0 - Unclassified 
# 1 - Rivers
# 2 - Ponds
# 3 - Buildings 
# 4 - Roads/Pavement 
# 5 - Residence 
# 6 - Trees/Greenery
# 7 - Marsh
# 8 - Grass
# 9 - Open Space

# Set the class names and class numbers correspondingly, with different colors:
Lndst5Clsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

Lndst5Clsfdclassdf <- data.frame(classvalue1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), classnames1 = Lndst5Clsfdclass)
Lndst5Clsfdclasscolors <- rainbow(10)  # use the rainbow palette color with 14 different colors.

# The 0 value of the dataset are no-data. I need to set them to NA
Lndst5Clsfd[Lndst5Clsfd==0]<-NA
Lndst5Clsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

# Sample from the layer:
# You cannot sample from a rasterbrick object directly, you have to extract the raster layer out from the brick, even if there is only one layer in the brick:
Lndst5Clsfd<-Lndst5Clsfd[[1]]

# Ratify this raster layer:
Lndst5Clsfd <-ratify(Lndst5Clsfd)
Lndst5Clsfdrat <-levels(Lndst5Clsfd)[[1]]
Lndst5Clsfdrat$landcover <- Lndst5Clsfdclass
levels(Lndst5Clsfd) <-Lndst5Clsfdrat
#Lndst5Clsfdclass<- c("Unclassified","Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")

set.seed(99)
samp <- sampleStratified(Lndst5Clsfd, size = 200, na.rm = TRUE, sp = TRUE)

# See the samples:
table(samp$LULC_Landsat_5_Classification1)

# Plot
library(rasterVis)
plt <- levelplot(Lndst5Clsfd, col.regions = Lndst5Clsfdclasscolors, main = 'Distribution of TrainingSites')
print(plt + latticeExtra::layer(sp.points(samp, pch = 3, cex = 0.5, col = 1)))

### CLASSIFYING
bndry <- st_read("C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/Newark Boundary Shapefile/Newark Boundary.shp")

lndst5_20110831 <- stack("Landsat5_2011-08-31.tif")
lndst5_20100828 <- stack("Landsat5_2010-08-28.tif")
lndst5_20090818 <- stack("Landsat5_2009-08-18.tif")
lndst5_20080822 <- stack("Landsat5_2008-08-22.tif")
lndst5_20070617 <- stack("Landsat5_2007-06-17.tif")
lndst5_20060801 <- stack("Landsat5_2006-08-01.tif")
lndst5_20050908 <- stack("Landsat5_2005-09-08.tif")
lndst5_20040703 <- stack("Landsat5_2004-07-03.tif")
lndst5_20030825 <- stack("Landsat5_2003-08-25.tif")
lndst5_20020907 <- stack("Landsat5_2002-09-07.tif")
lndst5_20010913 <- stack("Landsat5_2001-09-13.tif")
lndst5_20000917 <- stack("Landsat5_2000-09-17.tif")


RstrsList <- list("lndst5_20110831","lndst5_20100828","lndst5_20090818","lndst5_20080822","lndst5_20070617","lndst5_20060801","lndst5_20050908","lndst5_20040703","lndst5_20030825","lndst5_20020907","lndst5_20010913","lndst5_20000917") #Be sure that list is in temporal order from top (2022) to bottom (2010 for example) - otherwise naming convention at the end of for loop is wrong

for (i in 1:length(RstrsList)){
  lndstRstr <- get(paste0(RstrsList[i]))
  lndstRstr <- crop(lndstRstr, bndry)
  lndstRstr <- mask(lndstRstr, bndry)
  
  # Give its bands appropriate names:
  names(lndstRstr) <- c('blue', 'green', 'red', 'NIR1', 'NIR2',  'THRM')
  
  # The two rasters have different projection, they must be aligned:
  # I will also keep the original projected dataset
  Lndst5Clsfd.prj<-projectRaster(Lndst5Clsfd, lndstRstr, method = "ngb")
  
  # Sample it again:
  samp.prj <- sampleStratified(Lndst5Clsfd.prj, size = 200, na.rm = TRUE, sp = TRUE)
  
  # Extract the layer values for the locations
  
  sampvals <- extract(lndstRstr, samp.prj, df = TRUE)
  sampvals.sp <- extract(lndstRstr, samp.prj, df = TRUE, sp = TRUE)
  sampvals <- sampvals[, -1]
  sampvals.sp <- sampvals.sp[, -1]
  
  # combine the class information with extracted values
  sampdata <- data.frame(classvalue = samp.prj@data, sampvals) 
  
  # The superClass requires a spatialPointDataframe, so when extract the training sites, setting "sp = TRUE" is critical:
  # Random Forest Classification
  wv <- superClass(lndstRstr, sampvals.sp, verbose = T, model = "rf", responseCol = "LULC_Landsat_5_Classification1", filename = paste0("Classified_", RstrsList[i])) 
  name <- paste0("wv", 12-i, ".rf")
  assign(name, wv)
}


# Create empy master pixel freq table 
wvdf.master <- data.frame(NA)
#Lndst5Clsfdclass<- c("Rivers", "Ponds", "Buildings", "Roads/Pavement", "Residence", "Trees/Greenery", "Marsh", "Grass", "Open Space")
wvdf.master[1:9,] <- Lndst5Clsfdclass
colnames(wvdf.master) <- c("Value")

# Create df counting pixel by type and calculating perc of total
for (i in 0:11){
  dfname <- paste0("wv", i, ".rf") #Create superClass object Name
  wvdf <- data.frame(freq(get(dfname)$map)) #Create df with pixel freq by type
  wvdf[3] <- NA #Create empty third column for perc 
  colnames(wvdf) <- c("Value", paste0("Pixel Count - ", i), paste0("Perc of Total Pixels - ", i)) #Rename cols based on year
  wvdf <- wvdf[-nrow(wvdf), ] #Remove NA row count
  
  for (i in 1:nrow(wvdf)){
    wvdf[i,3] <- wvdf[i,2]/sum(wvdf[2]) #Calculate percs
  }
  
  wvdf.master <- cbind(wvdf.master, wvdf[, 2], wvdf[, 3]) #Add data to master df
  colnames(wvdf.master)[ncol(wvdf.master) - 1] <- c(colnames(wvdf[ncol(wvdf) -1])) #Rename 
  colnames(wvdf.master)[ncol(wvdf.master)] <- c(colnames(wvdf[ncol(wvdf)]))
}

wvdf.master.lndst5 <- wvdf.master

### COMBINE DATA FRAMES AND EXPORT AS CSV
wvdf.master.sntl <- data.frame(wvdf.master.sntl, row.names = TRUE)
wvdf.master.lndst89 <- data.frame(wvdf.master.lndst89, row.names = TRUE)
wvdf.master.lndst5 <- data.frame(wvdf.master.lndst5, row.names = TRUE)

wvdf.master <- cbind.data.frame(wvdf.master.lndst5, wvdf.master.lndst89, wvdf.master.sntl)

write.csv(wvdf.master, file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/LULC Master.csv")

# write.csv(wvdf.master.lndst5, file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/LULC Landsat 5 (2000-2011).csv")
# write.csv(wvdf.master.lndst89, file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/LULC Landsat 78 (2012-2014).csv")
# write.csv(wvdf.master.sntl, file = "C:/Users/cjkno/Documents/My Documents/Classes - '23 Spring/Research/Paper #3_Systems Dynamics/Data Collected/LULC/LULC Sentinel (2014-2022).csv")
