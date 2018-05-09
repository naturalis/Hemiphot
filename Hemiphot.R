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





##########          set classes for this script         ##########


#HemiphotImage = setClass("HemiphotImage",    ## this function is discontinued
#                         representation(image = "numeric", 
#                                        cx    = "numeric", 
#                                        cy    = "numeric", 
#                                        cr    = "numeric"))

#######END          set classes for this script         ##########





##########          Image2Hemiphot         ##########

Image2Hemiphot = function(image = ""){
  if (length(dim(image)) < 2){ 
    cat("Cannot convert image. Need at least 2D image to convert","\n")
    return(im)
  }  
  fisheye.cx = dim(image)[2]/2                    # x coordinate of center
  fisheye.cy = dim(image)[1]/2                    # y coordinate of center
  fisheye.cr  = (dim(image)[1]/2) - 2             # radius of hemiphot
  cat("centre of circle (", fisheye.cx, ", ", fisheye.cy, ")", "\n", sep = "")
  cat("radius of circle ", fisheye.cr, "\n", sep = "")
  HI = list(image, fisheye.cx, fisheye.cy, fisheye.cr)
#  class(HI) = "HemiphotImage"
  return(HI)
}

#######END          Image2HemiphotImage         ##########





##########          SetCircle         ##########

SetCircle = function(image = "", cx = 0, cy = 0, cr = 0){
  if(cx > 0) image[[2]] = cx
  if(cy > 0) image[[3]] = cy
  if(cr > 0) image[[4]] = cr
  return(image)
}

#######END          SetCircle         ##########





##########          Plot image         ##########

PlotHemiImage = function(image = "", draw.circle = T, channel = ""){
  # can plot only in R 2.11.0 and higher  
  if (exists("rasterImage")){ 
    if(channel %in% c("R", "G", "B")){
      plot(c(0,dim(image[[1]])[2]), c(0,dim(image[[1]])[1]), asp = 1,
           xlab = "columns", ylab = "rows", type = "n" )
      if(channel == "R") rasterImage(image[[1]][,,1], xleft = 0, ybottom = 0, xright = dim(image[[1]])[2], ytop = dim(image[[1]])[1], main = "R")
      if(channel == "G") rasterImage(image[[1]][,,2], xleft = 0, ybottom = 0, xright = dim(image[[1]])[2], ytop = dim(image[[1]])[1], main = "G")
      if(channel == "B") rasterImage(image[[1]][,,3], xleft = 0, ybottom = 0, xright = dim(image[[1]])[2], ytop = dim(image[[1]])[1], main = "B")
      if(draw.circle == T) DrawCircle(image[[2]], image[[3]], image[[4]])
    }else{
      plot(c(0,dim(image[[1]])[2]), c(0,dim(image[[1]])[1]), asp = 1,
           xlab = "columns", ylab = "rows", type = "n" )
      rasterImage(image[[1]], xleft = 0, ybottom = 0, xright = dim(image[[1]])[2], ytop = dim(image[[1]])[1])         ## all colors
      if(draw.circle == T) DrawCircle(image[[2]], image[[3]], image[[4]])
    }
  }else{ 
    cat("need R 2.11.0 and higher to support plotting of images","\n")
  }  
}

#######END          Plot image         ##########





##########          select a RGB channel         ##########

SelectRGB = function(image = "", channel = ""){
  if(channel %in% c("R", "G", "B")){
    if(channel == "R") layer = image[[1]][,,1]
    if(channel == "G") layer = image[[1]][,,2]
    if(channel == "B") layer = image[[1]][,,3]
    HI = list(layer, image[[2]], image[[3]], image[[4]])
#    class(HI) = "HemiphotImage"
    return(HI)
  } else {
    cat("need to select a color channel (R, G, or B)","\n")
  }
}

#######END          select an RGB channel         ##########





##########          threshold image         ##########

ThresholdImage = function(image = "", th = 0.5, draw.image = F){
  on = image[[1]] > th
  image[[1]][on] = 1; image[[1]][!on] = 0
  if(draw.image == T) PlotHemiImage(image)
  return(image)
}

#######END          threshold image         ##########





##########          draw a red circle on image          ##########

DrawCircle = function(cx = 100, cy = 100, radius = 50){
  x = 0:radius
  y = sin(acos(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  y = x
  x = cos(asin(x/radius)) * radius
  points(cx + x, cy + y, col = "red", cex = 0.1)
  points(cx + x, cy - y, col = "red", cex = 0.1)
  points(cx - x, cy + y, col = "red", cex = 0.1)
  points(cx - x, cy - y, col = "red", cex = 0.1)
  text(cx+radius-25, cy, "W", col = "red")
  text(cx-radius+25, cy, "E", col = "red")
  text(cx, cy+radius-25, "N", col = "red")
  text(cx, cy-radius+25, "S", col = "red")
}

#######END          draw a red circle on image          ##########





##########          draw circles         ##########

DrawCircles = function(fisheye.cx, fisheye.cy, fisheye.cr){
  deg2rad = pi / 180
  steps = 1:360
  for (i in 1:89){
    x = round(fisheye.cx + cos(steps*deg2rad)*i*fisheye.cr/90,0)
    y = round(fisheye.cy + sin(steps*deg2rad)*i*fisheye.cr/90,0)
    points(x, y, col = "red", cex = 0.1)
  }
}

#######END          draw circles         ##########





##########          calculate gap fractions on 89 circles         ##########

CalcGapFractions = function(image = ""){
  deg2rad = pi / 180
  gap.fractions = matrix(0,89)
  steps         = 1:360
  fisheye.cx    = image[[2]]
  fisheye.cy    = image[[3]]
  fisheye.cr    = image[[4]]

  for (i in 1:89){
    x = round(fisheye.cx + cos(steps*deg2rad)*i*fisheye.cr/90,0)
    y = round(fisheye.cy + sin(steps*deg2rad)*i*fisheye.cr/90,0)
    for(j in 1:360) gap.fractions[i] = gap.fractions[i] + image[[1]][y[j],x[j]]
  }
  return(gap.fractions/360)
}

#######END          calculate gap fractions on 89 circles         ##########





##########          calculate canopy openness         ##########

CalcOpenness = function(fractions){
  deg2rad        = pi / 180
  a              = deg2rad * 1:89
  d05            = deg2rad * 0.5
  Atot           = sin(a[89] + d05) - sin(a[1] - d05)
  Aa             = sin(a + d05) - sin(a - d05)
  canopy.openess = sum(fractions*Aa/Atot)
  return(canopy.openess)
}

#######END          calculate canopy openess         ##########





##########          calculate Leaf Area Index         ##########
## note that calc.LAI can only be executed if calc.gap.fractions has been called

CalcLAI = function(fractions, width = 6){
  if(dim(fractions)[1] != 89) return("incorrect dimensions for fractions") 
  deg2rad = pi/180
  angle    = c(7, 23, 38, 53, 68)                  # angles of LAI2000
  wi       = c(0.034, 0.104, 0.160, 0.218, 0.494)  # weights given by Licor canopy analyzer manual

  if(width < 0 | width > 6) width = 6
  T = rep(0,5)
  for(i in -width:width) T = T + gap.fractions[angle + i]
  T = T/(2*width +1)

  LAI      = 2 * sum(-log(T) * wi * cos(angle*deg2rad))
  return(LAI)
}

#######END          calculate Leaf Area Index         ##########





##########          ShowSolarTracks          ##########

DrawSolarTracks = function(image ="", lat = 0, lon = 0, time.zone = 0, 
                           d = 150, magn.corr = 0, sun.location = F, h = 0){
  fisheye.cx   = image[[2]]
  fisheye.cy   = image[[3]]
  fisheye.cr   = image[[4]]

  deg2rad      = pi / 180
  rad2deg      = 180 / pi
  day2angle    = (pi / 180) * (360 / 365)
 
  ## latitude angle (psi)
  if (lat >  89.99) lat =  89.99
  if (lat < -89.99) lat = -89.99
  psi     = lat * deg2rad
  sin.psi = sin(psi)
  cos.psi = cos(psi)

  for(k in 1:length(d)){
    ## solar declination (delta)
    day.angle    = d[k] * day2angle
    sin.delta   = 0.3985 * sin(4.869 + day.angle + 0.03345 * sin(6.224 + day.angle))
    cos.delta   = sqrt(1 - sin.delta*sin.delta)

    ## Solar day length
    ## hour angle (eta) for sun.up and sun.down
    cos.eta = -(sin.psi/cos.psi)*(sin.delta/cos.delta)
    if (cos.eta > 1) {
      cos.eta = 1
      eta = pi
    } else if(cos.eta < -1){
      cos.eta = -1
      eta = pi
    } else if(cos.eta == 0){
      eta = 0.5 * pi
    } else {
      eta = atan(sqrt(1 - cos.eta*cos.eta)/cos.eta)
    }
    if(eta < 0) eta = eta + pi
    sun.up   = (round(12 - (eta*12/pi)))*30 - 10 #{add a safety 20 min}
    sun.down = (round(12 + (eta*12/pi)))*30 + 10

    for(i in sun.up:sun.down){
      hour = i/30                    # 2 minute steps - size of solar disc in sky

      ## hour angle (eta)
      sin.eta  = sin(pi*(hour-12)/12)  #pi/12 = 2pi/24!
      cos.eta  = cos(pi*(hour-12)/12)  #pi/12 = 2pi/24!

      ## Solar angle(alfa)
      sin.alfa = sin.psi*sin.delta + cos.psi*cos.delta*cos.eta
      cos.alfa = sqrt(1-(sin.alfa*sin.alfa))
      alfa     = atan(sin.alfa/cos.alfa);

      ## solar azimuth (beta)
      sin.beta = -cos.delta*sin.eta/cos.alfa
      cos.beta = -(sin.delta-sin.psi*sin.alfa)/(cos.psi*cos.alfa)

      sin.angle = sin(magn.corr*deg2rad)
      cos.angle = cos(magn.corr*deg2rad)
      sin.beta_c = (sin.beta*cos.angle + cos.beta*sin.angle)   ##sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)
      cos.beta_c = (cos.beta*cos.angle - sin.beta*sin.angle)   ##cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)

      ## draw track points only if alfa > 0 (sun above horizon)
      if(alfa > 0){
        xp   = fisheye.cx - (sin.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        yp   = fisheye.cy - (cos.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        points(xp, yp, cex = 0.1, col = "red")
      }
    }
    if(sun.location == T){
    ## Equation of time
      EoT = 229.18*(0.000075+0.001868*cos(day.angle)-0.032077*sin(day.angle)-0.014615*cos(2*day.angle)-0.040849*sin(2*day.angle))
      hour = h + (EoT + 15*(lon - (15 * time.zone)))/60  # in hours

      ## hour angle (eta)
      sin.eta  = sin(pi*(hour-12)/12)  #pi/12 = 2pi/24!
      cos.eta  = cos(pi*(hour-12)/12)  #pi/12 = 2pi/24!

      ## Solar angle(alfa)
      sin.alfa = sin.psi*sin.delta + cos.psi*cos.delta*cos.eta
      cos.alfa = sqrt(1-(sin.alfa*sin.alfa))
      alfa     = atan(sin.alfa/cos.alfa);

      ## solar azimuth (beta)
      sin.beta = -cos.delta*sin.eta/cos.alfa
      cos.beta = -(sin.delta-sin.psi*sin.alfa)/(cos.psi*cos.alfa)

      sin.angle = sin(magn.corr*deg2rad)
      cos.angle = cos(magn.corr*deg2rad)
      sin.beta_c = (sin.beta*cos.angle + cos.beta*sin.angle)   ##sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)
      cos.beta_c = (cos.beta*cos.angle - sin.beta*sin.angle)   ##cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)
      if(alfa > 0){
        xp   = fisheye.cx - (sin.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        yp   = fisheye.cy - (cos.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        points(xp, yp, cex = 1, pch = 21, col = "yellow", bg = "yellow")
      } 
    }
  } 
}

#######END          ShowSolarTracks          ##########




##########          Calculate PAR by Day         ##########

## this is returned as PPFD in umol m-2 s-1 or mol m-2 day-1

CalcPAR.Day = function(image = "", lat = 0, lon = 0, time.zone = 0, 
                       d = 150, tau = 0.6, uoc = 0.15, 
                       magn.corr = 0, draw.tracks = F, full.day = T){
  force(image)
  fisheye.cx    = image[[2]]
  fisheye.cy    = image[[3]]
  fisheye.cr    = image[[4]]

  deg2rad        = pi / 180
  rad2deg        = 180 / pi
  solar.constant = 1360 * 0.51 * 4.6            ## PPFD (umol m-2 s-1)
  uMol2Mol       = 2 * 60 / 1000000             ## PAR calculated every 2 minutes + conversion from umol to mol
  day2angle      = (pi / 180) * (360 / 365)

  ## diffuse factor
  fractions = CalcGapFractions(image = image)
  a         = deg2rad*1:89
  d05       = deg2rad*0.5
  Atot      = sin(a[89] + d05) - sin(a[1] - d05)
  Aa        = (sin(a + d05) - sin(a - d05))*sin(a)
  ISF       = sum(fractions*Aa/Atot)

  ## latitude angle (psi)
  if (lat >  89.99) lat =  89.99
  if (lat < -89.99) lat = -89.99
  psi     = lat * deg2rad
  sin.psi = sin(psi)
  cos.psi = cos(psi)

  rad.day  = matrix(0, length(d) , 6)     

  for(k in 1:length(d)){
 
    ## solar declination (delta)
    day.angle    = d[k] * day2angle
    sin.delta   = 0.3985 * sin(4.869 + day.angle + 0.03345 * sin(6.224 + day.angle))
    cos.delta   = sqrt(1 - sin.delta*sin.delta)
    R_out       = solar.constant + 0.034 * cos(day.angle)   #Radiation at outer atmosphere

    ## Equation of time
    if(length(d) == 1){
      EoT = 229.18*(0.000075+0.001868*cos(day.angle)-0.032077*sin(day.angle)-0.014615*cos(2*day.angle)-0.040849*sin(2*day.angle))
      time.offset = (EoT + 15*(lon - (15 * time.zone)))/60  # in hours
    }

    ## Solar day length
    ## hour angle (eta) for sun.up and sun.down
    cos.eta = -(sin.psi/cos.psi)*(sin.delta/cos.delta)
    if (cos.eta > 1) {
      cos.eta = 1
      eta = pi
    } else if(cos.eta < -1){
      cos.eta = -1
      eta = pi
    } else if(cos.eta == 0){
      eta = 0.5 * pi
    } else {
      eta = atan(sqrt(1 - cos.eta*cos.eta)/cos.eta)
    }
    if(eta < 0) eta = eta + pi
    sun.up   = (round(12 - (eta*12/pi)))*30 #- 5 #{add a safety 10 min}
    sun.down = (round(12 + (eta*12/pi)))*30 #+ 5 #{add a safety 10 min}

    nrows = 1+sun.down-sun.up
    radiation = matrix(0,nrows,6) 

    for(i in sun.up:sun.down){
      hour = i/30                    # 2 minute steps - size of solar disc in sky
      ## hour angle (eta)
      sin.eta  = sin(pi*(hour-12)/12)
      cos.eta  = cos(pi*(hour-12)/12)

      ## Solar angle(alfa) - method see SunLocation
      sin.alfa = sin.psi*sin.delta + cos.psi*cos.delta*cos.eta
      cos.alfa = sqrt(1-(sin.alfa*sin.alfa))
      alfa     = atan(sin.alfa/cos.alfa);

      ## solar azimuth (beta)
      sin.beta = -cos.delta*sin.eta/cos.alfa
      cos.beta = -(sin.delta-sin.psi*sin.alfa)/(cos.psi*cos.alfa)

      sin.angle = sin(magn.corr * deg2rad)
      cos.angle = cos(magn.corr * deg2rad)
      sin.beta_c = (sin.beta*cos.angle + cos.beta*sin.angle)   ##sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)
      cos.beta_c = (cos.beta*cos.angle - sin.beta*sin.angle)   ##cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)

      ## use track points only if alfa > 0 (sun above horizon)
      if(alfa > 0){
        ## airmasses
        M        = sqrt(1229 + (614 * sin.alfa)^2) - 614 * sin.alfa 

        ## Transmission of direct light through airmasses
        trans    = tau^M

        ## direct light
        direct   = solar.constant * trans * sin.alfa

        ## diffuse light as Uniform Overcast Sky (UOC)
        # in which 15% of direct light becomes diffuse
        diffuse  = direct * uoc                            #Uniform Overcast Sky (15% of direct light becomes diffuse

        ## other options not yet implemented
        #diffuse = R_out * (0.271 - 0.294*Trans)*sin.alfa  #Clear not dust free sky        
        #diffuse = direct * sin.alfa * (1 + 2*sin.alfa)/3) #SOC 

        ## pixel location
        xp   = fisheye.cx - round(sin.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        yp   = fisheye.cy + round(cos.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)  # on image
        yp2  = fisheye.cy - round(cos.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)  # on plot!

        if(draw.tracks == T) points(xp, yp2, cex = 0.1, col = "red")

        i2 = i-sun.up+1
        if(length(d) == 1){
          radiation[i2,1] = hour
          radiation[i2,2] = hour + time.offset
        }
        radiation[i2,3] = radiation[i2,3] + direct
        radiation[i2,4] = radiation[i2,4] + diffuse
        radiation[i2,5] = radiation[i2,5] + direct * image[[1]][yp,xp]
        radiation[i2,6] = radiation[i2,6] + diffuse * ISF
      }
    }
    rad.day[k,1] = sum(radiation[,3]) * uMol2Mol / length(d)
    rad.day[k,2] = sum(radiation[,4]) * uMol2Mol / length(d)
    rad.day[k,3] = sum(radiation[,5]) * uMol2Mol / length(d)
    rad.day[k,4] = sum(radiation[,6]) * uMol2Mol / length(d)
  } 
  if(length(d) == 1 & full.day == T){ 
    colnames(radiation) = c("Solar Time","Location Time","DirectAbove", "DiffuseAbove", "DirectUnder", "DiffuseUnder")
    return(radiation)
  } else {
    rad.out = c(sum(rad.day[,1]), sum(rad.day[,2]), sum(rad.day[,3]), sum(rad.day[,4])) 
    names(rad.out) = c("DirectAbove", "DiffuseAbove", "DirectUnder", "DiffuseUnder")
    return(rad.out) 
  }
}

#######END          Calculate PAR by day          ##########




##########          Plot PAR by Day         ##########

## plot the dayly values of PPFD
## Works only if day = 1 and FullDay = True

PlotPAR.Day = function(radiation = "", real.time = F){
  if(dim(radiation)[2] == 6 & dim(radiation)[1] > 1){
    radiation.above = radiation[,3] + radiation[,4]
    radiation.below = radiation[,5] + radiation[,6]
    if(real.time == F){
      plot(radiation[,1],   radiation.above, cex = 0.1, xlab = "solar time", ylab = "PPFD (umol m-2 s-1)")
      points(radiation[,1], radiation.below, cex = 0.1, xlab = "solar time", ylab = "PPFD umol m-2 s-1")
      lines(radiation[,1],  radiation.below, col = "red")
    } else {
      plot(radiation[,2],   radiation.above, cex = 0.1, xlab = "location time", ylab = "PPFD (umol m-2 s-1)")
      points(radiation[,2], radiation.below, cex = 0.1, xlab = "solar time", ylab = "PPFD umol m-2 s-1")
      lines(radiation[,2],  radiation.below, col = "red")
    }      
  } else {
  cat("Radiation matrix not of correct dimensions (6,1..)","\n")
  }
}

#######END          Plot PAR by Day         ##########





##########          Calculate PAR by Year         ##########

## this is returned as PPFD in umol m-2 s-1

CalcPAR.Year = function(image = "", lat = 0, tau = 0.6, 
                        uoc = 0.15, magn.corr = 0){
  force(image)
  fisheye.cx     = image[[2]]
  fisheye.cy     = image[[3]]
  fisheye.cr     = image[[4]]

  deg2rad        = pi / 180
  rad2deg        = 180 / pi
  solar.constant = 1360 * 0.51 * 4.6           ## PPFD (umol m-2 s-1)
  uMol2Mol       = 2 * 60 / 1000000
  day2angle      = (pi / 180) * (360 / 365)
  d              = 1:365 
  rad.year        = matrix(0, 365, 5)
  rad.year[,1]    = d 

  ## diffuse factor
  fractions = CalcGapFractions(image = image)
  a         = deg2rad * 1:89
  d05       = deg2rad * 0.5
  Atot      = sin(a[89] + d05) - sin(a[1] - d05)
  Aa        = (sin(a + d05) - sin(a - d05)) * sin(a)
  ISF       = sum(fractions * Aa / Atot)

  ## latitude angle (psi)
  if (lat >  89.99) lat =  89.99
  if (lat < -89.99) lat = -89.99
  psi     = lat * deg2rad
  sin.psi = sin(psi)
  cos.psi = cos(psi)

  for(k in 1:365){
 
    ## solar declination (delta) - method see solar.declination()
    day.angle    = d[k] * day2angle
    sin.delta   = 0.3985 * sin(4.869 + day.angle + 0.03345 * sin(6.224 + day.angle))
    cos.delta   = sqrt(1 - sin.delta*sin.delta)
    R_out       = solar.constant + 0.034 * cos(day.angle)   #Radiation at outer atmosphere

    ## Solar day length
    ## hour angle (eta) for sun.up and sun.down
    cos.eta = -(sin.psi/cos.psi)*(sin.delta/cos.delta)
    if (cos.eta > 1) {
      cos.eta = 1
      eta = pi
    } else if(cos.eta < -1){
      cos.eta = -1
      eta = pi
    } else if(cos.eta == 0){
      eta = 0.5 * pi
    } else {
      eta = atan(sqrt(1 - cos.eta*cos.eta)/cos.eta)
    }
    if(eta < 0) eta = eta + pi
    sun.up   = (round(12 - (eta*12/pi)))*30 #- 5 #{add a safety 10 min}
    sun.down = (round(12 + (eta*12/pi)))*30 #+ 5 #{add a safety 10 min}

    nrows = 1+sun.down-sun.up
    radiation = matrix(0,nrows,6) 

    for(i in sun.up:sun.down){
      hour = i/30                    # 2 minute steps - size of solar disc in sky
      ## hour angle (eta)
      sin.eta  = sin(pi*(hour-12)/12)
      cos.eta  = cos(pi*(hour-12)/12)

      ## Solar angle(alfa) - method see SunLocation
      sin.alfa = sin.psi*sin.delta + cos.psi*cos.delta*cos.eta
      cos.alfa = sqrt(1-(sin.alfa*sin.alfa))
      alfa     = atan(sin.alfa/cos.alfa);

      ## solar azimuth (beta)
      sin.beta = -cos.delta*sin.eta/cos.alfa
      cos.beta = -(sin.delta-sin.psi*sin.alfa)/(cos.psi*cos.alfa)

      sin.angle = sin(magn.corr*deg2rad)
      cos.angle = cos(magn.corr*deg2rad)
      sin.beta_c = (sin.beta*cos.angle + cos.beta*sin.angle)   ##sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)
      cos.beta_c = (cos.beta*cos.angle - sin.beta*sin.angle)   ##cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)

      ## use track points only if alfa > 0 (sun above horizon)
      if(alfa > 0){
        ## nr of airmasses direct light has to pass through
        M        = sqrt(1229 + (614 * sin.alfa)^2) - 614 * sin.alfa 

        ## Transmission of direct light through airmasses
        trans    = tau^M

        ## direct light
        direct  = solar.constant * trans * sin.alfa

        ## diffuse light
        ## used is the Uniform Overcast Sky (UOC)
        diffuse = direct * uoc                             #Uniform Overcast Sky (15% of direct light becomes diffuse

        ## other methods not yet implemented
        #diffuse = R_out * (0.271 - 0.294*Trans)*sin.alfa  #Clear not dust free sky        
        #diffuse = direct * sin.alfa * (1 + 2*sin.alfa)/3) #SOC 

        ## pixel location
        xp   = fisheye.cx - round(sin.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)
        yp   = fisheye.cy + round(cos.beta_c * (90-rad2deg*alfa) * fisheye.cr/90)

        i2 = i-sun.up+1
        if(length(d) == 1){
          Radiation[i2,1] = hour
          Radiation[i2,2] = hour + TimeOffset
        }
        radiation[i2,3] = radiation[i2,3] + direct
        radiation[i2,4] = radiation[i2,4] + diffuse
        radiation[i2,5] = radiation[i2,5] + direct * image[[1]][yp,xp]
        radiation[i2,6] = radiation[i2,6] + diffuse * ISF
      }
    }
    rad.year[k,2] = sum(radiation[,3]) * uMol2Mol
    rad.year[k,3] = sum(radiation[,4]) * uMol2Mol
    rad.year[k,4] = sum(radiation[,5]) * uMol2Mol
    rad.year[k,5] = sum(radiation[,6]) * uMol2Mol
  } 
  colnames(rad.year) = c("Day", "DirectAbove", "DiffuseAbove", "DirectUnder", "DiffuseUnder")
  return(rad.year)
}


#######END          Calculate PAR by day          ##########





##########          Plot PAR by Year         ##########

## plot the dayly values of PPFD of a full year
## Works only if Radiation is in 5 by 365 matrix

PlotPAR.Year = function(radiation = ""){
  if(dim(radiation)[2] == 5 & dim(radiation)[1] == 365){
    above = radiation[,2] + radiation[,3]
    below = radiation[,3] + radiation[,4]
    plot(radiation[,1],   above, cex = 0.1, xlab = "day", ylab = "PPFD (mol m-2 day-1)",
         ylim = c(0,max(above)))
    points(radiation[,1], below, cex = 0.1)
  } else {
  cat("Radiation matrix not of correct dimensions (365x2)","\n")
  }
}

