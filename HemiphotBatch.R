##########          Hemiphot.R         ##########
# a script to calculate light indeces and more from hemispherical images
# read the manual and helfile to use this script.
# HemiphotTest.R provides examples of all single calculations
# HemiphotBatch.R provides examples of batch calculations for a 
# number of images in one directory
#
# to be able to use Hemophot.R yo will need to install the functions
# with the command >source("Hemiphot.R")
# provided the script is foudn in our working directory
#######END          Hemiphot.R         ##########





##########          How to cite the use of Hemiphot         ##########
#If you use Hemiphot.R for your research or work please cite as:
#Hans ter Steege (2018) Hemiphot.R: Free R scripts to analyse hemispherical 
#photographs for canopy openness, leaf area index and photosynthetic 
#active radiation under forest canopies.  
#Unpublished report. Naturalis Biodiversity Center, Leiden, The Netherlands
#https://github.com/Hans-ter-Steege/Hemiphot
#######END          How to cite the use of Hemiphot         ##########





### This is the batch script for Hemiphot.R
### Here you can run all functions and store 
### all data by file


### clear memory and set working directory
rm(list = ls())
setwd("...R/Hemiphot")  ## set your working directory





##########          load libraries and source files          ##########

library(jpeg)      			     # reading and writing jpg's
source("Hemiphot.R")                     # functions to calculate all results

days = seq(15,360,30)   # roughly each mid of the 12 months

#######END          load libraries and source files          ##########



##########          initialize site and image data          ##########

### Location parameters
location.latitude   = 1.487
location.altitude   = 0
location.day        = 150
location.days       = seq(15,360,30)   # roughly each mid of the 12 months

### Image parameters
## determine in Hemiphot.R and fill in here for batch processing
location.cx         = 1504             # x coordinate of center
location.cy         =  975             # y coordinate of center
location.cr         =  900             # radius of circle
location.threshold  = 0.65

### atmospheric parameters
location.tau        = 0.6
location.uoc        = 0.15

#######END          initialize site and image data          ##########






##########          load image namess          ##########

### We assume colour images
### in a subdirectory images
### load all JPG file names in a list

all.images = list.files("images/",pattern = ".JPG")
nr.images = length(all.images); nr.images

## Create data frame to hold all results
all.data = data.frame(matrix(0, nr.images, 7))
names(all.data) = c("File", "CanOpen", "LAI",
                    "DirectAbove", "DiffAbove",
                    "DirectBelow", "DiffBelow")
all.data[,1] = all.images

## now the batch can start
t1 = Sys.time()
for(i in 1:nr.images){    
  ## read file
  image = readJPEG(paste("images/",all.images[i],sep = ""), native = F)     #if native = T creates a raster, else an array

  ## conver to Hemi image
  image = Image2Hemiphot(image)

  ## set cirlce parameters
  image = SetCircle(image, cx = location.cx, cy = location.cy, cr = location.cr)

  ## select blue channel
  image = SelectRGB(image, "B")

  #threshold
  image = ThresholdImage(im = image, th = location.threshold, draw.image = F)

  # canopy openness
  gap.fractions = CalcGapFractions(image)
  all.data[i,2] = CalcOpenness(fractions = gap.fractions)
   
  ## calculate LAI according to Licor's LAI Analyzer 
  all.data[i,3] = CalcLAI(fractions = gap.fractions)

  ## Photosynthetic Photon Flux Density (PPDF, umol m-1 s-1) P
  rad = CalcPAR.Day(im = image,
                    lat = location.latitude, d = days,
                    tau = location.tau, uoc = location.uoc, 
                    draw.tracks = F, full.day = F)
  all.data[i,4] = rad[1]
  all.data[i,5] = rad[2]
  all.data[i,6] = rad[3]
  all.data[i,7] = rad[4]
}
t2 = Sys.time()

##time per image
(t2 - t1)/nr.images

head(All.Data)

** save data
write.table(All.Data, "HemiphotOutput.csv", sep = ",")


