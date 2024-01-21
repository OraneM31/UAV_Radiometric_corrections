#For this code, I began by organizing my images. I created a folder for each flight, then in each folder, I created a calibration file in which I manually took images containing photos of my calibration targets before and after each flight. During this code, I go through several "useless" data extraction steps that I don't use afterwards. However, these steps could be useful for other analyses, which is why I extract more files than necessary.
######################################### PACKAGE #######################################
library(exifr) #On windows it is needed to install strawberry http://strawberryperl.com/
library(raster)
library(sp)
library(reshape2)
library(Rmisc)
library(ggplot2)
library(sf)

#Exposition compensation
######################################### NOTE ###################################
#I use this script to calculate the correction factor for the exposure compensation from #the EXIF data of the images. I apply these corrections later in the script.

######################################### ANALYSE ################################
# Get a list of folders containing RGB or NIR images
tempfolder <- list.dirs(path = "*", recursive = TRUE) #* with the direction of your UAV data
tempfolder <- tempfolder[grep("/RGB|/NIR", tempfolder, value = FALSE)]

# Filter out folders with corrected photos
tempfolder <- tempfolder[!grepl("/Corrected_Photo$", tempfolder)]

# Process each flight folder

for (folder in tempfolder) {
  setwd(folder) # Set the working directory
  # Skip if already processed
  if (length(list.files(pattern = "ExposureCompensation.csv$")) > 0 ||
      length(list.files(pattern = ".jpg$")) == 0) {
    print("Already done")
    print(folder)
    next
  }
  
  # Create a dataset to store the correction factors
  Dataset <- data.frame(Photos = character(),
                        Facteur_Correction = numeric(),
                        stringsAsFactors = FALSE)
  
  # Get a list of all image files
  image_files <- list.files(pattern = "*.jpg$", recursive = TRUE)
  
  for (file in image_files) {
    # Extract EXIF data
    exif_data <- read_exif(file)
    # Calculate the correction factor for exposure compensation
    Correction_factor <- exif_data$FNumber^2 / (exif_data$ExposureTime * exif_data$ISO * 10)
    # Save the correction factor in the dataset
    Dataset[nrow(Dataset) + 1, ] <- c(file, Correction_factor)
    # Clean up variables
    rm(Correction_factor, exif_data)
  }  
  # Save the correction factors to a CSV file
  write.csv(Dataset, "ExposureCompensation.csv", row.names = FALSE)  
  print("Processing completed")
}
Calibration
#This script is used to draw the shape on my MAPIR Panels, on raw image (without any light #or exposition correction). I extract my correction factor for 3 panels of my calibration #target (light grey, dark gray and black) for few images (for RGB and NIR/RE sensor) take
#before and after each flight (already selected and placed in a separate folder).

######################################### NOTE ##########################################
# Calibration data from manufacturer
# Red: 650nm -- W 0.87196 -- LG 0.26290 -- DG 0.19854 -- BT 0.01937
# Green: 548nm -- W 0.86478 -- LG 0.26273 -- DG 0.19274 -- BT 0.01970
# Blue: 446nm-W 0.78671 -- LG 0.24728 -- DG 0.18221-BT 0.02015
# Red Edge: 720nm-W 0.87012 -- LG 0.26248 -- DG 0.21095-BT 0.01946
# Near Infrared: 840nm -- W 0.86252 -- LG 0.27549 -- DG 0.22837 -- BT 0.02146
# LG is Light Grey; DG is Dark Grey and BT is black

# This script is used to draw the shape on my MAPIR Panels, on raw image (without any light or exposition correction).
# I extract my correction factor for 3 panels of my calibration target (light grey, dark gray, and black) for a few images
# taken before and after each flight (already selected and placed in a separate folder).

######################################### FUNCTION #######################################

# Function to calculate correction factor
calculateCorrectionFactor <- function(panelName, trueValue, DNData) {
  factor <- trueValue / mean(DNData$DN[grep(panelName, DNData$Var2, value = FALSE)])
  return(factor)
}

######################################### ANALYZE #######################################
# Get a list of folders containing RGB or NIR calibration images
tempFolders <- list.dirs(path = "*/", recursive = TRUE) #* with the direction of your UAV data
tempFolders <- tempFolders[grep("RGB/Calibration$|/NIR/Calibration$", tempFolders, value = FALSE)]

for (folder in tempFolders) {
  setwd(folder)
  
  if (length(list.files(pattern = "*.jpg.csv$")) == 0) {
    temp <- list.files(pattern = "*.jpg$", recursive = TRUE)
    data_names <- gsub(".jpg", "", temp) # Remove the file extension
    
    ######################################## Light Grey #######################################
    # Loop through each image
    for (image in temp) {
      Calibration <- stack(image)
      plotRGB(Calibration)
      Calibration <- raster::select(Calibration)
      plotRGB(Calibration)
      cut <- drawPoly()
      shapefile(cut, file = paste("ShapeLG", image, ".shp", sep = ""), overwrite = TRUE)
      DNData <- extract(Calibration, cut)
      write.csv(DNData, file = paste("DNDataLG", image, ".csv", sep = ""), row.names = FALSE)
      DNData <- melt(DNData, variable.name = "Band", value.name = "DN")
      DNData <- summarySE(DNData, measurevar = "DN", groupvars = "Var2")
      write.csv(DNData, file = paste("SummaryDNDataLG", image, ".csv", sep = ""), row.names = FALSE)
      SumDNData <- rbind(SumDNData, DNData)
    }
    
    SumDNData <- SumDNData[-1,]
    
    # Calculate correction factors
    Facteur_Correction_Red <- calculateCorrectionFactor(".1$", 0.26290, SumDNData) # Red
    Facteur_Correction_Green <- calculateCorrectionFactor(".2$", 0.26273, SumDNData) # Green
    Facteur_Correction_Blue <- calculateCorrectionFactor(".3$", 0.24728, SumDNData) # Blue
    #Value for light grey panel, change it for the other panel with manufacturer values.
    
    # Store correction factors in a data frame
    Facteur_CorrectionLG <- data.frame(
      name = c("Facteur_Correction_Red", "Facteur_Correction_Green", "Facteur_Correction_Blue"),
      value = c(Facteur_Correction_Red, Facteur_Correction_Green, Facteur_Correction_Blue)
    )
    
    # Save correction factors to a CSV file
    write.csv(Facteur_CorrectionLG, file = "Facteur_CorrectionLG.csv", row.names = FALSE)
  } else {
    print("Already done")
  }
}

# Clean up variables
rm(list = c("temp", "data_names", "SumDNData", "Facteur_Correction_Red", "Facteur_Correction_Green", "Facteur_Correction_Blue"))

############ DONE --- Change the value, file name, and draw polygon for the next panel


Re-do Calibration on with exposure compensation.
######################################### ANALYSE RGB ####################################
## Start with RGB image sensor, NIR after
tempfolder <- list.dirs(path ="*/",recursive = TRUE) #* with the direction of your UAV data
tempfolder <- tempfolder[grep("RGB/Calibration$",tempfolder,value=FALSE)] 

for (m in 1: length(tempfolder)) {
  setwd(tempfolder[m])
  
  temp <- list.files(pattern = "*.jpg$",recursive = TRUE)
  data_names <- vector("list",length(temp))
  
  for (n in 1: length(temp)) {data_names[n] <- strsplit(temp[n], split=".jpg")}
  
  Exposure_Compensation <- read.csv("ExposureCompensation.csv")
  #Importe exposure compensation already calculated    
  
  SumDNDataLG <- NA
  SumDNDataDG <- NA
  SumDNDataD <- NA
  
  for (i in 1 : length(temp)) {
    OriginalPhoto <- stack(temp[i])
    OriginalPhoto <- OriginalPhoto*Exposure_Compensation$Facteur_Correction[Exposure_Compensation$Photos == temp[i]]
    
    #Importe image and apply exposure compensation already calculated for this image.
    
    cut <- st_read(paste("ShapeLG", temp[i],".shp", sep = ""))
    #Importe the draw shape to extract DN for light grey panel (same for others panels after)
    DNData <- extract(OriginalPhoto,cut) #Extract DN value with exposure compensation for the light grey panel
    DNData <- melt(DNData, variable.name="Band", value.name = "DN") #Puts all the values in one column
    DNData<- summarySE(DNData, measurevar="DN", groupvars="Var2") #
    SumDNDataLG <- rbind(SumDNDataLG,DNData) #I store in a dataset all the DN extracted for the light grey panel for this folder
    
    #Re do the same for others panels
  }
  
  write.csv(SumDNDataLG,file="Calibrated_SummaryDNDataLG.csv",row.names = FALSE) #Save it
  write.csv(SumDNDataDG,file="Calibrated_SummaryDNDataDG.csv",row.names = FALSE)
  write.csv(SumDNDataD,file="Calibrated_SummaryDNDataD.csv",row.names = FALSE)
  
  Facteur_Correction_Red <- 0.26290/mean(SumDNDataLG$DN[grep(".1$",SumDNDataLG$Var2,value=FALSE)]) #Apply factor correction for each band (RGB here/ Light Grey)
  Facteur_Correction_Green <- 0.26273/mean(SumDNDataLG$DN[grep(".2$",SumDNDataLG$Var2,value=FALSE)]) 
  Facteur_Correction_Blue <- 0.24728/mean(SumDNDataLG$DN[grep(".3$",SumDNDataLG$Var2,value=FALSE)])
  
  name<-c("Facteur_Correction_Red","Facteur_Correction_Green","Facteur_Correction_Blue")
  value<-c(Facteur_Correction_Red,Facteur_Correction_Green,Facteur_Correction_Blue)
  Facteur_CorrectionLG <- data.frame(name, value)
  write.csv(Facteur_CorrectionLG,file="Calibrated_Facteur_CorrectionLG.csv",row.names = FALSE) 
  
  remove(Facteur_CorrectionLG, SumDNDataLG)
}
remove(i, n, m, Exposure_Compensation, tempfolder,Solar_Elevaion, data_names, OriginalPhoto,Solar_corrected_Photo,Exposure_corrected_Photo)
rm(list=ls())


Application of corrections 
# Exposure compensation and calibration using previously extracted calibration data
# Get a list of folders containing RGB images
tempfolder <- list.dirs(path = "*", recursive = TRUE) #* with the direction of your UAV data
tempfolder <- tempfolder[grep("/RGB$|", tempfolder, value = FALSE)]

# Process each RGB folder
for (folder in tempfolder) {
  setwd(folder) # Set the working directory
  
  if (file.exists(paste0(folder, "/Corrected_Photo"))) {
    print("Already done")
    next
  }
  
  # Read the exposure compensation data
  Exposure_Compensation <- read.csv("ExposureCompensation.csv", stringsAsFactors = FALSE)
  
  # Get the calibration folder for this flight
  tempfolderCalib <- list.dirs(path = "/Fall_UAV/", recursive = TRUE)
  tempfolderCalib <- tempfolderCalib[grep(folder, tempfolderCalib, value = FALSE)]
  
  setwd(tempfolderCalib[2]) # Go to the calibration folder
  
  # Read the calibration correction files
  Facteur_CorrectionLG <- read.csv("Calibrated_Facteur_CorrectionLG.csv")
  Facteur_CorrectionDG <- read.csv("Calibrated_Facteur_CorrectionDG.csv")
  Facteur_CorrectionD <- read.csv("Calibrated_Facteur_CorrectionD.csv")
  
  # Calculate the average corrections by wavelength
  Correction_Blue <- mean(Facteur_CorrectionD$value[Facteur_CorrectionD$name == "Facteur_Correction_Blue"],
                          Facteur_CorrectionDG$value[Facteur_CorrectionDG$name == "Facteur_Correction_Blue"],
                          Facteur_CorrectionLG$value[Facteur_CorrectionLG$name == "Facteur_Correction_Blue"])
  Correction_Red <- mean(Facteur_CorrectionD$value[Facteur_CorrectionD$name == "Facteur_Correction_Red"],
                         Facteur_CorrectionDG$value[Facteur_CorrectionDG$name == "Facteur_Correction_Red"],
                         Facteur_CorrectionLG$value[Facteur_CorrectionLG$name == "Facteur_Correction_Red"])
  Correction_Green <- mean(Facteur_CorrectionD$value[Facteur_CorrectionD$name == "Facteur_Correction_Green"],
                           Facteur_CorrectionDG$value[Facteur_CorrectionDG$name == "Facteur_Correction_Green"],
                           Facteur_CorrectionLG$value[Facteur_CorrectionLG$name == "Facteur_Correction_Green"])
  
  setwd('..') # Go back to the main flight folder, which contains all the flight images.
  
  # Get the list of image files
  image_files <- list.files(pattern = "*.jpg$")
  
  # Process each image
  for (image_file in image_files) {
    # Read the original image
    OriginalPhoto <- brick(image_file)
    
    # Apply exposure compensation if available
    if (image_file %in% Exposure_Compensation$Photos) {
      OriginalPhoto <- OriginalPhoto * as.numeric(Exposure_Compensation$Facteur_Correction[Exposure_Compensation$Photos == image_file])
    } else {
      OriginalPhoto <- OriginalPhoto * mean(Exposure_Compensation$Facteur_Correction)
    }
    
    # Apply correction factors
    OriginalPhoto[[1]] <- OriginalPhoto[[1]] * Correction_Red
    OriginalPhoto[[2]] <- OriginalPhoto[[2]] * Correction_Green
    OriginalPhoto[[3]] <- OriginalPhoto[[3]] * Correction_Blue
    
    # Create a directory for corrected images
    dir.create(file.path(folder, "Corrected_Photo"), showWarnings = FALSE)
    setwd(file.path(folder, "Corrected_Photo"))
    
    # Save the new corrected image
    jpeg(paste(image_file), height = 3000, width = 4000, units = "px", quality = 100)
    plotRGB(OriginalPhoto, scale = max(maxValue(OriginalPhoto)), maxpixels = 12000000)
    dev.off()
    
    setwd('..')
  }
  
  print(folder)
}
EXIF application
#After correction, I need to reaply the old EXIF image on my new corrected image
######################################### ANALYSE #######################################

tempfolder <- list.dirs(path ="*/",recursive = TRUE) #* with the direction of your UAV data
tempfolder < - tempfolder[grep("RGB/Corrected_Photo$| NIR/Corrected_Photo$",tempfolder,value=FALSE)] 

#For each corrected folder
for (folder in tempfolder) {
  
  setwd(folder) # Set the working directory
  
  # Get the list of corrected images
  corrected_images <- list.files(pattern = "*IMG", recursive = TRUE)
  
  # Read EXIF from the first corrected image
  exif_data <- read_exif(corrected_images[1])
  setwd("..") # Go back to the parent directory
  
  # Check if it's an EXIF of a corrected image
  
  if (ncol(exif_data) < 30) {    
    for (image in corrected_images) {      
      
      # Build the exiftool command
      T.img.H <- (paste0("-tagsfromfile ",'"',getwd(),"/",temp[n], '"',
                         " -all:all-makernotes -make -model -P-overwrite_original ", '"', folder,"/",image, '"')) 
      
      # Run exiftool to copy the EXIF data to the new images
      exiftool_call(T.img.H)    
    }
  } else {
    print(folder) # Indicates folders that have already been processed
  }
}
  