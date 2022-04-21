# US Point Pattern Analysis ----
library(spatstat)
library(maptools)
library(readr)
library(rgdal)

# CRREATING A PPP OBJECT FOR FAULTS AND LOCATION
## 1. Create a window of observation ----

## a) Importing the shape file ----
# upload the us shape file

US2020 = readOGR("cb_2020_us_nation_20m", "cb_2020_us_nation_20m")

# find the  shape fro the 49 states
# us_poligons <- c(1:300) 
# for(i in us_poligons) {
#   print(us2020@polygons[[1]]@Polygons[[i]]@area)
# }

# https://blog.valdosta.edu/andersonlab/2018/04/29/spatiallinesdataframe-to-owin-in-spatstat/

## b) Extracting the coordinates from the SpatialObject ----

US_coordX <- US2020@polygons[[1]]@Polygons[[36]]@coords[,1] 
US_coordY <- US2020@polygons[[1]]@Polygons[[36]]@coords[,2]

US_coordX <- US_coordX[-1] 
US_coordY <- US_coordY[-1] 


## c) Creating the Observation Window : Z ----
# Z <- owin(poly=list(x=US_coordX, y=US_coordY)) Error in owin(poly = list(x = US_coordX, y = US_coordY)) : 
# Area of polygon is negative - maybe traversed in wrong direction?

Z <- owin(poly=list(x=rev(US_coordX), y=rev(US_coordY )))  #Bingo!; 
class(Z)
plot(Z)

## 2. PPP of Faults for US ----
### a) Import the csv file ----
US_fault <- read_csv("vfault.csv", col_types = cols(CITY = col_skip(), VEHICLE_ID = col_skip(), WEEK = col_skip()))


US_fault$DATE <-  as.Date(US_fault$DATE,  format="%Y-%m-%d")
### b) Creating x an y ----
x <- US_fault$LONGITUDE
y <- US_fault$LATITUDE

### c) Adding the marks ----
mark = subset(US_fault, select = c(4:7))

### d) Joining the data into a  PPP object for faults ----
US_ppp = ppp(x, y, window= Z, marks = mark)
#summary(US_ppp)

## 3. PPP object for the vehicle location file ----
### a) Import the csv file ----

US_location <- read_csv("vlocation.csv", col_types = cols(STATE = col_skip(), CITY = col_skip(), 
                                                          VEHICLE_ID = col_skip(), WEEK = col_skip()))
US_location$DATE <-  as.Date(US_location$DATE,  format="%Y-%m-%d")

### b) Creating x an y ----
x1 <- US_location$LONGITUDE
y1 <- US_location$LATITUDE

### c) Adding the marks ----
mark1 = subset(US_location, select = c(3:5))

### d) Joining the data into a  PPP object for location ----
USL_ppp = ppp(x1, y1, window= Z, marks = mark1)
# summary(USL_ppp)

# VISUALIZING THE FAULTS DENSITY  ----
## For the entire year of 2020  ----
Fraction = density(unmark(US_ppp))/density(unmark(USL_ppp))
plot(Fraction, main=NULL)

## by month  ----
## Split on the month mark
USfaults_month <- split(US_ppp, "MONTH", un = TRUE)
USlocat_month <- split(USL_ppp, "MONTH", un = TRUE)

## USlculate the density by month
USF_m_den <-density(USfaults_month)
USL_m_den <-density(USlocat_month)
Density_month <- USF_m_den/USL_m_den 
plot(Density_month)


  
