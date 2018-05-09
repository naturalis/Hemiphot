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





### This is the test script for Hemiphot.R
### Here you can test the functions, set the parameters
### you will need to install the library jpeg 
### or any other library that can read images





### clear memory and 
rm(list = ls())


### set working directory
### need to change to your own settings!
setwd("") # set your working directory





##########          load libraries and source files          ##########

library(jpeg)      			#library for reading and writing jpg's
source("Hemiphot.R")                #functions to carry out Hemiphot analysis
days = seq(15,360,30)               #roughly each mid of the 12 months

#######END          load libraries and source files          ##########





##########          load image and plot         ##########

### We assume colour images: FlevoPlot1.1.jpg is provided at Github
im = readJPEG("FlevoPlot1.1.jpg", native = F)     #if native = T creates a raster, else an array


## convert to class HemiphotImage - adding a circle
im.hemi = Image2Hemiphot(im)
PlotHemiImage(image = im.hemi, draw.circle = T)                         #note that east and west are reversed as the image looks upward


##adjust circle, if necessary
im.hemi = SetCircle(im.hemi, cx = 1136, cy = 852, cr = 102)
PlotHemiImage(im.hemi)


## for now use default taken from Hemiphotclass
im.hemi = Image2Hemiphot(im)
PlotHemiImage(im.hemi, draw.circle = T, channel = "")


#######END          load image and plot         ##########





##########          check RGB channels and choose         ##########

## We check the RGB channels 
## R (red) and B (blue) both have high absorption by leaves - leaves are black
## G (green) has high tranmission of light through leaves
## B gives high values for both white and blue sky
## B thus gives best contrast between sky (1) and leaves (0)

PlotHemiImage(im.hemi, draw.circle = T, channel = "R")
PlotHemiImage(im.hemi, draw.circle = T, channel = "G")
PlotHemiImage(im.hemi, draw.circle = T, channel = "B")


## We select the blue channel , as it has strong absorbance with leaves
## leaves will be almost black and sky will be almost white 
## under varying conditions (white and blue sky)

im.blue = SelectRGB(im.hemi, "B")
PlotHemiImage(im.blue, draw.circle = T)

#######END          check RGB channels and choose         ##########





##########          Threshold the image of B channel for analysis         ##########

## We need to threshold the image
## a new image (image.th) is created that will be used for the subsequent analysis
## because the original image is kept (image), you can try various options

threshold = 0.55
image.th = ThresholdImage(image = im.blue, th = threshold, draw.image = T)

#######END          Threshold the image of B channel for analysis         ##########





##########          Main Calculations of Hemiphot.R         ##########


## canopy openess is calculated over 89 circles, each 360 points
## these points can be visualized with draw.circles()

PlotHemiImage(image.th, draw.circle = T) 
DrawCircles(image.th[[2]], image.th[[3]], image.th[[4]])

PlotHemiImage(image.th, draw.circle = T) 


## calculate canopy cover based on canopy openess of the 89 circles
## the openess by circle is stored in gap.fractions
gap.fractions = CalcGapFractions(image.th)
canopy.openness = CalcOpenness(fractions = gap.fractions); canopy.openness


## calculate LAI according to Licor's LAI Analyzer 
canopy.LAI = CalcLAI(fractions = gap.fractions, width = 6); canopy.LAI


## Photosynthetic Photon Flux Density (PPDF, umol m-1 s-1) P
## is calculated for the variable day. 
## Day can either contain 1 or more days
## If one day is selected the PPFD for the full day can be returned
## for graphical purposes, or just the total for 1 day


## show solar tracks for which data is calculated
## see the effect of magnetic correction 
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = 0)
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = 1)
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = 5)
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = 35)
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = -35)
DrawSolarTracks(image.th, lat = 52, d = 320, magn.corr = 55)

Flevo.lat = 52.467086
Flevo.lon = 5.426731

## or 12 days mid month (Jan - Dec)
PlotHemiImage(image.th)
DrawSolarTracks(image.th, lat = 0, d = days, magn.corr = 0)
DrawSolarTracks(image.th, lat = Flevo.lat, lon = Flevo.lon, 
                d = 97, magn.corr = 0)


## show sunlocation at specified hour
PlotHemiImage(image.th)
DrawSolarTracks(image.th, lat = 0, lon = 0, time.zone = 0, 
                d = 300, magn.corr = 0, sun.location = T, h = 12.00)
PlotHemiImage(image.th)
DrawSolarTracks(image.th, lat = 5, lon = -58, time.zone = -4, 
                d = 300, magn.corr = 0, sun.location = T, h = 12.00)


PlotHemiImage(image.th)
for(i in 3:21){
  DrawSolarTracks(image.th, lat = 63, lon = 4, time.zone = 0, 
                  d = 150, magn.corr = 0, sun.location = T, h = i)
}

## calculate light. If for 1 day, the dayly track is returned or the total
## for many days, only the average light (w/m2 or PPFD) is stored

# first for 1 day, returning PPFD in 2 minute values
# and graphing this result

PlotHemiImage(image.th)
Rad = CalcPAR.Day(image.th, lat = 53.36, lon = 5.17, time.zone = +1,
                  d = 174, tau = 0.6, uoc = 0.15,
                  magn.corr = 0, draw.tracks = T, full.day = T)


## plot the dayly values of PPFD
## Works only if day is 1 and full.day is true
PlotPAR.Day(radiation = Rad, real.time = F)
PlotPAR.Day(radiation = Rad, real.time = T)

# second for 12 days, returning PPFD in 4 fractions
# which is the dayly average of the days sampled
# direct above canopy, diffuse above canopy
# direct below canopy, diffuse below canopy
# when days > 1, FullDay will be ignored

Rad = CalcPAR.Day(image.th, lat = 5, d = days, 
                 tau = 0.65, uoc = 0.15,
                 draw.tracks = F, full.day = F)

## show result
Rad

## PAR for 365 days
## may take a few minutes to complete

par(mfrow = c(1,2))

Rad = CalcPAR.Year(image.th, lat = 52, tau = 0.6, uoc = 0.15, magn.corr = 0)
PlotPAR.Year(radiation = Rad)

Rad = CalcPAR.Year(image.th, lat = 0, tau = 0.6, uoc = 0.15, magn.corr = 0)
PlotPAR.Year(radiation = Rad)

par(mfrow = c(1,1))


