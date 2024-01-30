#Produce by Orane Mordacq
#2022


##### Radiometric correction

# Some steps detailed below were not used in the paper but could be useful for other analyses.
# We begin by organizing the images. 
# We create a folder for each flight, 
# then in each folder, we add photos of the calibration targets before and after each flight.
######################################### PACKAGE #################################
# Load required libraries
library(exifr) # On windows it is needed to install strawberry http://strawberryperl.com/
library(raster)
library(sp)
library(reshape2)
library(Rmisc)
library(ggplot2)
library(sf)

######################################### EXPOSURE COMPENSATION ##################
# NOTE: This script calculates the correction factor for exposure compensation
# from the EXIF data of the images, to be applied later in the script.

######################################### ANALYZE ################################
# Organize images by creating folders for each flight and adding calibration 
# target photos before and after each flight.
tempfolder <- list.dirs(path = "*", recursive = TRUE) # with the direction of UAV data
tempfolder <- tempfolder[grep("/RGB|/NIR", tempfolder, value = FALSE)] # for each sensor

# Filter out folders with already corrected photos
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


######################################### CALIBRATION ###################################
# NOTE: Calibration data from the manufacturer (MAPIR Panel)
# Red: 650nm -- W 0.87196 -- LG 0.26290 -- DG 0.19854 -- BT 0.01937
# Green: 548nm -- W 0.86478 -- LG 0.26273 -- DG 0.19274 -- BT 0.01970
# Blue: 446nm-W 0.78671 -- LG 0.24728 -- DG 0.18221-BT 0.02015
# Red Edge: 720nm-W 0.87012 -- LG 0.26248 -- DG 0.21095-BT 0.01946
# Near Infrared: 840nm -- W 0.86252 -- LG 0.27549 -- DG 0.22837 -- BT 0.02146
# LG is Light Grey; DG is Dark Grey and BT is black

# NOTE: This script draws shapes on MAPIR Panels
# We calculated the correction factor for 3 panels of the calibration 
# target (light grey, dark gray and black) for few images (6 for RGB and 6 for the NIR/RE sensor) take
# before and after each flight (already selected and placed in a separate folder).

######################################### FUNCTION #######################################
# Function to calculate correction factor
calculateCorrectionFactor <- function(panelName, trueValue, DNData) {
  factor <- trueValue / mean(DNData$DN[grep(panelName, DNData$Var2, value = FALSE)])
  return(factor)
}

######################################### ANALYZE #######################################
# Get a list of folders containing RGB or NIR calibration images
tempFolders <- list.dirs(path = "*/", recursive = TRUE) # with the direction of your UAV data
tempFolders <- tempFolders[grep("RGB/Calibration$|/NIR/Calibration$", tempFolders, value = FALSE)]


for (folder in tempFolders) {
  setwd(folder)
  
  #Create an empty dataset to collect calibration values
  SumDNData <- NA
  
  if (length(list.files(pattern = "*.jpg.csv$")) == 0) {
    temp <- list.files(pattern = "*.jpg$", recursive = TRUE)
    data_names <- gsub(".jpg", "", temp) # Remove the file extension
    
    Exposure_Compensation <- read.csv("ExposureCompensation.csv")
    #Importe exposure compensation already calculated    
    
    ######################################## Light Grey #######################################
    # Start with one panel, next change value, dataset name etc. for the next panel
    
    for (image in temp) {
      
      Calibration <- stack(image)
      Calibration <- Calibration*Exposure_Compensation$Facteur_Correction[Exposure_Compensation$Photos == image]
      #Importe image and apply exposure compensation already calculated for this image.
      
      plotRGB(Calibration)
      Calibration <- raster::select(Calibration)
      plotRGB(Calibration)
      cut <- drawPoly()
      #Write the polygone in a shapefile, if needed later
      shapefile(cut, file = paste("ShapeLG", image, ".shp", sep = ""), overwrite = TRUE) #LG is Light Grey
      
      
      #Extract DN value with exposure compensation for the light grey panel
      DNData <- extract(Calibration, cut) #DN is Digital Number
      #Write a csv with the DN extracted from the part of the image with the Light Grey panel
      DNData <- melt(DNData, variable.name = "Band", value.name = "DN")
      DNData <- summarySE(DNData, measurevar = "DN", groupvars = "Band")
      #Collect DN for all the LG panel of this folder (by flight/ by sensor)
      SumDNData <- rbind(SumDNData, DNData)
    }
    
    SumDNData <- SumDNData[-1,]
    #From values defined above
    
    # Calculate correction factors
    Facteur_Correction_Red <- 0.26290/mean(SumDNDataLG$DN[grep(".1$",SumDNDataLG$Var2,value=FALSE)]) #Apply factor correction for each band (RGB here/ Light Grey)
    Facteur_Correction_Green <- 0.26273/mean(SumDNDataLG$DN[grep(".2$",SumDNDataLG$Var2,value=FALSE)]) 
    Facteur_Correction_Blue <- 0.24728/mean(SumDNDataLG$DN[grep(".3$",SumDNDataLG$Var2,value=FALSE)])
    #Value for light grey panel, change it for the other panel with manufacturer values.
    
    Facteur_CorrectionLG <- data.frame(
      name = c("Facteur_Correction_Red", "Facteur_Correction_Green", "Facteur_Correction_Blue"),
      value = c(Facteur_Correction_Red, Facteur_Correction_Green, Facteur_Correction_Blue)
    )
    
    write.csv(Facteur_CorrectionLG, file = "Calibrated_Facteur_CorrectionLG.csv", row.names = FALSE)
    remove(Facteur_CorrectionLG, SumDNDataLG)
    
  } else {
    print("Already done")
  }
}
# Clean up variables
rm(list=ls())

############ DONE --- Change the value, file name, and draw polygon for the next panel


######################################### Application of corrections 
# Exposure compensation and calibration using previously extracted calibration data
# Get a list of folders containing for example RGB images
tempfolder <- list.dirs(path = "*", recursive = TRUE) # with the direction of your UAV data
tempfolder <- tempfolder[grep("/RGB$|", tempfolder, value = FALSE)]

# Process each RGB folder
for (folder in tempfolder) {
  setwd(folder) # Set the working directory
  
  if (file.exists(paste0(folder, "/Corrected_Photo"))) {
    print("Already done")
    next
  }
  
  # Read the exposure compensation file 
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
    
    # Apply correction factors for each wavelenght (change it if you also have NIR photo or other wavelenghts)
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


#EXIF application
#Last step, after correction, apply the old EXIF image on the new corrected image
######################################### ANALYZE #######################################

tempfolder <- list.dirs(path ="*/",recursive = TRUE) # with the direction of your UAV data
tempfolder <- tempfolder[grep("RGB/Corrected_Photo$| NIR/Corrected_Photo$",tempfolder,value=FALSE)] #already corrected photo 

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

rm(list=ls())
