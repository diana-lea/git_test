
### Install packages if not installed before

if(!require("install.load")){install.packages("install.load") 
  library("install.load")}

install_load("tidyr", "dplyr", "bigmemory", "maps", "readxl", "reshape", 
             "rgeos", "plyr", "ggplot2", "maptools", "scales", "ggmap",
             "RColorBrewer", "gtools", "foreign", "ggrepel", "rgdal",
             "installr", "stringr", "corrplot", "Hmisc", "lubridate", "spatstat", "reshape2")

### Ordering variables function (https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame)

arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

options(scipen=999)



##############################################
### Regional estimates prevalence - 2018 Data

#setwd("G:\\docs\\World Drug Report\\WDR_2020\\Pre-pub\\Working Files\\Prevalence\\Regional Estimates\\Cocaine")
getwd()
setwd("C:/Users/Diana Camerini/OneDrive - United Nations")

dir()


########################
### ### Cocaine ### ###
#####################

### read data

CANNABIS_data <- read.csv("Prevalence_cannabis_working_2020.csv", header = T, sep = ";")


###clean age groups
levels(CANNABIS_data$Age)
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Oct-69"] <- "10-69"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Dec-35"] <- "12-35"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Dec-64"] <- "12-64"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Dec-65"] <- "12-65"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Dec-70"] <- "12-70"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Oct-60"] <- "10-60"
levels(CANNABIS_data$Age)[levels(CANNABIS_data$Age)== "Oct-75"] <- "10-75"
levels(CANNABIS_data$Age)



#remove column to use (temporarily)

CANNABIS_data <- CANNABIS_data[CANNABIS_data$Use_regional_estimates != "0", ] 


### crop non-last available year

#sort first and then remove duplicates
CANNABIS_data <- CANNABIS_data[order(CANNABIS_data$Country, -abs(CANNABIS_data$Year) ), ] ### sort first
data <- CANNABIS_data[ !duplicated(CANNABIS_data$Country), ]  ### Keep highest
data[] <- lapply(data, function(x) if(is.factor(x)) factor(x) else x)



### read population data
pop_data <- read.csv("Population_2017_2018.csv", header = T, sep = ";")


### add in database countries for which we don't have any data
setdiff(pop_data$Country.or.region, data$Country)
country_no_data <- setdiff(pop_data$Country.or.region, data$Country)
country_no_data <- as.data.frame(country_no_data)
names(country_no_data)[names(country_no_data)=="country_no_data"] <- "Country"

country_no_data$Drug <- "Cannabis"
country_no_data[setdiff(names(data), names(country_no_data))] <- NA

data <- rbind(data, country_no_data)


#update country names
levels(data$Country)
levels(data$Country)[levels(data$Country)== "The former Yugoslav Republic of Macedonia"] <- "North Macedonia"



### Add regions and subregions

#REGION_SUBREGION <- read_excel("G:\\RAB\\DDDU\\04_Data CrossCutting\\Standardization of names and classification\\UNODC Standard - Version 4\\MASTER file.xlsx", sheet = "MainTable")
REGION_SUBREGION <- read_excel("MASTER file.xlsx", sheet = "MainTable")


REGION_SUBREGION$Country <- REGION_SUBREGION$`UNODC Name`
data <- left_join(x=data, y=REGION_SUBREGION[,c("Country", "Proposed drugs data (WDR) subregion, PRIOR to spelling and name changes", "Proposed region")], by = "Country")

names(data)[names(data)=="Proposed drugs data (WDR) subregion, PRIOR to spelling and name changes"] <- "Sub.Region2"
names(data)[names(data)=="Proposed region"] <- "Region2"



data$Region <- NULL
names(data)[names(data)=="Region2"] <- "Region"

unique(data$Sub.region)
data$Sub.region <- NULL
names(data)[names(data)=="Sub.Region2"] <- "Sub.Region"

data <- arrange.vars(data, c("Region"= 2, "Sub.Region"=3))



###clean region names
class(data$Sub.Region)
data$Sub.Region <- as.factor(data$Sub.Region)

levels(data$Sub.Region)
levels(data$Sub.Region)[levels(data$Sub.Region)== "Central America"] <- "Central America (excluding Mexico)"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Northern America"] <- "Northern America (including Mexico)"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Southern Africa"] <- "Southern and South-Eastern Africa"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Central Asia and Transcaucasian countries"] <- "Central Asia and Transcaucasia"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Eastern Europe"] <- "Eastern and South-Eastern Europe (including Turkey)"
levels(data$Sub.Region)[levels(data$Sub.Region)== "South-Eastern Europe"] <- "Eastern and South-Eastern Europe (including Turkey)"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Western & Central Europe"] <- "Western and Central Europe"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Western and Central Africa"] <- "West and Central Africa"
levels(data$Sub.Region)[levels(data$Sub.Region)== "Northern America (including Mexico)"] <- "North America (including Mexico)"
levels(data$Sub.Region)

#change Turkey region
data$Region[data$Country== "Turkey"] <- "Europe"
data$Region[data$Sub.Region== "Western & Central Europe"] <- "Europe"


#remove UK
unique(data$Country)
data <- data[data$Country != "United Kingdom", ]



### Merge population with data

pop_data$Country <- pop_data$Country.or.region
pop_data$X2017_15_64 <- NULL
pop_data$X2017_TOT <- NULL
data <- left_join(x = data, y = pop_data, by = "Country")

data <- arrange.vars(data, c("Country.or.region"=5))
data$Country.or.region <- NULL


names(data)[names(data)=="X2018_15_64"] <- "POP_15_64"
names(data)[names(data)=="X2018_TOT"] <- "POP_TOT"


data$Code <- NULL


################### Start calculations



### calculate sum regional and subregional population

data$pop_sum_15_64_subr <- ave(data$POP_15_64, data$Sub.Region, FUN=sum)
data$pop_sum_15_64_reg <- ave(data$POP_15_64, data$Region, FUN=sum)

data <- arrange.vars(data, c("pop_sum_15_64_subr"=5, "pop_sum_15_64_reg" = 6))


data$pop_sum_subr <- ave(data$POP_TOT, data$Sub.Region, FUN=sum)
data$pop_sum_reg <- ave(data$POP_TOT, data$Region, FUN=sum)

data <- arrange.vars(data, c("pop_sum_subr"=7, "pop_sum_reg" = 8))



#### Make the calculations

## for countries with available data


#last year prevalence - Best, low and high
data$LYP <- data$Best
data$LYP_low <- ifelse(!is.na(data$Low), data$Low, data$Best)
data$LYP_high <- ifelse(!is.na(data$High), data$High, data$Best)

data <- arrange.vars(data, c("LYP"=12, "LYP_low" = 13, "LYP_high" = 14))


#Number of users

data$Nusers_best <- data$LYP * 1000 * (data$POP_15_64/100)
data$Nusers_low <- data$LYP_low * 1000 * (data$POP_15_64/100)
data$Nusers_high <- data$LYP_high * 1000 * (data$POP_15_64/100)



#count available datapoints by sub-region
data$frequency_subr <- ave(data$Country, data$Sub.Region, FUN=length)
data$av_data <- ifelse(!is.na(data$LYP), "YES", "NO")

data$count_avail <- ave(data$LYP, data$av_data, data$Sub.Region, FUN=length)
data$count_avail2 <- ifelse(data$av_data == "YES", data$count_avail, NA)

c<- table(data$Sub.Region, data$count_avail2)
c2 <- melt(c)
c2 <- c2[c2$value != 0, ]
c2 <- as.data.frame(c2)
c2$value <- NULL
names(c2) <- c("Sub.Region", "count")
data <- left_join(x=data, y= c2, by="Sub.Region")

data$count_avail2 <- NULL


### sum population by sub-region and by data availability
data$sum_pop_avail_1564_subr <- ave(data$POP_15_64, data$av_data, data$Sub.Region, FUN=sum)
data$sum_pop_avail_TOT_subr <- ave(data$POP_TOT, data$av_data, data$Sub.Region, FUN=sum)

data <- arrange.vars(data, c("sum_pop_avail_1564_subr"=7, "sum_pop_avail_TOT_subr"=8))



#indicator to produce subreginal estimates -> if at leat 2 country in the subregion and if they represent at least 20% population

data$pop_rep <- data$sum_pop_avail_TOT_subr / data$pop_sum_subr
data$pop_rep <- ifelse(data$av_data == "YES", data$pop_rep, NA)


d<- table(data$Sub.Region, data$pop_rep)
d2 <- melt(d)
d2 <- d2[d2$value != 0, ]
d2 <- as.data.frame(d2)
#View(d2)
d2$value <- NULL
names(d2) <- c("Sub.Region", "rep_pop")
data <- left_join(x=data, y= d2, by="Sub.Region")


data$reg_est_allowed <- ifelse(data$rep_pop >= 0.2 & data$count >1, "yes", "no")
data$reg_est_allowed[is.na(data$reg_est_allowed)] <- "no"
Z<- table(data$Sub.Region, data$reg_est_allowed)
#View(Z)
Z


#write.csv(data, "data_test.csv", row.names = F)



##########################
### Construct subregional summary table

Sub.Region <- NA
nr_best <- NA
nr_low <- NA
nr_high <- NA
prev_best <- NA
prev_low <- NA
prev_high <- NA

regional_estimates <- cbind.data.frame(Sub.Region, nr_best, nr_low, nr_high, prev_best, prev_low, prev_high)





#############################################
### Calculate estimates - subregion by subregion


###### Regions with available data


#############
#### Africa
######


### ### Northern Africa

data_n_africa <- subset.data.frame(data, data$Sub.Region == "Northern Africa")
data_n_africa[] <- lapply(data_n_africa, function(x) if(is.factor(x)) factor(x) else x)


### Total available users + prev

  ## best
  users_nr_best <- sum(data_n_africa$Nusers_best, na.rm = TRUE)
  pop <- unique(data_n_africa$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_n_africa$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_n_africa$Nusers_low, na.rm = TRUE)
  prev_reg_low <- 0.685586735872627   ### possibly find out the methodology to calculate this
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_n_africa$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_n_africa$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 5.26079976074022  ### possibly find out the methodology to calculate this
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_n_africa$pop_sum_15_64_subr)*10)
  prev_high_sreg
  


  ## Add data to the reg estimates tables
  reg_est_sum <- c("Northern Africa", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)




### ### West and Central Africa

data_w_africa <- subset.data.frame(data, data$Sub.Region == "West and Central Africa")
data_w_africa[] <- lapply(data_w_africa, function(x) if(is.factor(x)) factor(x) else x)


### Total available users + prev

  ## best
  users_nr_best <- sum(data_w_africa$Nusers_best, na.rm = TRUE)
  pop <- unique(data_w_africa$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_w_africa$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_w_africa$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  1.95138453551124  
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_w_africa$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_w_africa$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 11.2371132103328
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_w_africa$pop_sum_15_64_subr)*10)
  prev_high_sreg

  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("West and Central Africa", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  

  
  

### ### Eastern Africa -> calculate starting from the regional estimate
  
data_e_africa <- subset.data.frame(data, data$Sub.Region == "Eastern Africa")
data_e_africa[] <- lapply(data_e_africa, function(x) if(is.factor(x)) factor(x) else x)
  
  
### Total available users + prev

  ## best
  users_nr_best <- sum(data_e_africa$Nusers_best, na.rm = TRUE)
  pop <- unique(data_e_africa$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_e_africa$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_e_africa$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  1.19727879223248
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_e_africa$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_e_africa$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 9.09640828562871
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_e_africa$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
    
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Eastern Africa", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  

  
### ### Southern and South-Eastern Africa -> calculate starting from the regional estimate
  
data_s_africa <- subset.data.frame(data, data$Sub.Region == "Southern and South-Eastern Africa")
data_s_africa[] <- lapply(data_s_africa, function(x) if(is.factor(x)) factor(x) else x)
  
  

  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_s_africa$Nusers_best, na.rm = TRUE)
  pop <- unique(data_s_africa$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_s_africa$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_s_africa$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  2.90714876926787
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_s_africa$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_s_africa$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 10.9877835579025
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_s_africa$pop_sum_15_64_subr)*10)
  prev_high_sreg


  ## Add data to the reg estimates tables
  reg_est_sum <- c("Southern and South-Eastern Africa", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  

    
  
#############
#### America
######
  
  
### ### Central America (excluding Mexico)
  
data_c_america <- subset.data.frame(data, data$Sub.Region == "Central America (excluding Mexico)")
data_c_america[] <- lapply(data_c_america, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_c_america$Nusers_best, na.rm = TRUE)
  pop <- unique(data_c_america$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- 0
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_c_america$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_c_america$Nusers_low, na.rm = TRUE)
  prev_reg_low <- 0.466887554599014   
  
  users_nr_low_NA <- 0
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_c_america$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_c_america$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 4.97847614698886  
  
  users_nr_high_NA <- 0
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_c_america$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Central America (excluding Mexico)", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  
### ### North America (including Mexico)
  
data_n_america <- subset.data.frame(data, data$Sub.Region == "North America (including Mexico)")
data_n_america[] <- lapply(data_n_america, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_n_america$Nusers_best, na.rm = TRUE)
  pop <- unique(data_n_america$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_n_america$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_n_america$Nusers_low, na.rm = TRUE)
  prev_reg_low <- 1.90169581043227  
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_n_america$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_n_america$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 19.3281027161114  ### possibly find out the methodology to calculate this
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_n_america$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("North America (including Mexico)", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  

  
### ### South America
  
data_s_america <- subset.data.frame(data, data$Sub.Region == "South America")
data_s_america[] <- lapply(data_s_america, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_s_america$Nusers_best, na.rm = TRUE)
  pop <- unique(data_s_america$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_s_america$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_s_america$Nusers_low, na.rm = TRUE)
  prev_reg_low <- 1.20958583666449
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_s_america$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_s_america$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 8.35383306831592
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_s_america$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("South America", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  

### ### Caribbean
  
data_caribbean <- subset.data.frame(data, data$Sub.Region == "Caribbean")
data_caribbean[] <- lapply(data_caribbean, function(x) if(is.factor(x)) factor(x) else x)
  
  
### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_caribbean$Nusers_best, na.rm = TRUE)
  pop <- unique(data_caribbean$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_caribbean$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_caribbean$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  0.319869253855553
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_caribbean$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_caribbean$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 17.8922644232789
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_caribbean$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Caribbean", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  
  
  
#############
#### Asia
######
  
  
### ### East and South-East Asia
  
data_e_asia <- subset.data.frame(data, data$Sub.Region == "East and South-East Asia")
data_e_asia[] <- lapply(data_e_asia, function(x) if(is.factor(x)) factor(x) else x)
  
  
### Total available users + prev
  
  
  ## best
  users_nr_best <- sum(data_e_asia$Nusers_best, na.rm = TRUE)
  pop <- unique(data_e_asia$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_e_asia$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_e_asia$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  0.0379142125418219
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_e_asia$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_e_asia$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 1.54038359109714
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_e_asia$pop_sum_15_64_subr)*10)
  prev_high_sreg



  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("East and South-East Asia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  
  
### ### Near and Middle East / South-West Asia
  
data_nm_asia <- subset.data.frame(data, data$Sub.Region == "Near and Middle East /South-West Asia")
data_nm_asia[] <- lapply(data_nm_asia, function(x) if(is.factor(x)) factor(x) else x)
  
  
### Total available users + prev

  ## best
  users_nr_best <- sum(data_nm_asia$Nusers_best, na.rm = TRUE)
  pop <- unique(data_nm_asia$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_nm_asia$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_nm_asia$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  0.377774567258421
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_nm_asia$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_nm_asia$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 4.60122714009289   ### possibly find out the methodology to calculate this
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_nm_asia$pop_sum_15_64_subr)*10)
  prev_high_sreg
  


  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Near and Middle East /South-West Asia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  

### ### Southern Asia
  
data_s_asia <- subset.data.frame(data, data$Sub.Region == "South Asia")
data_s_asia[] <- lapply(data_s_asia, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_s_asia$Nusers_best, na.rm = TRUE)
  pop <- unique(data_s_asia$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_s_asia$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_s_asia$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  2.83478700339348
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_s_asia$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_s_asia$Nusers_high, na.rm = TRUE)
  prev_reg_high <-  2.90535576422813
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_s_asia$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("South Asia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  

  
### ### Central Asia and Transcaucasia -> calculate starting from the regional estimate
  
data_c_asia <- subset.data.frame(data, data$Sub.Region == "Central Asia and Transcaucasia")
data_c_asia[] <- lapply(data_c_asia, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_c_asia$Nusers_best, na.rm = TRUE)
  pop <- unique(data_c_asia$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_c_asia$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_c_asia$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  0.662318359732166
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_c_asia$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_c_asia$Nusers_high, na.rm = TRUE)
  prev_reg_high <-  4.27996909865752
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_c_asia$pop_sum_15_64_subr)*10)
  prev_high_sreg


  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Central Asia and Transcaucasia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  

#############
#### Europe
######
  
  
  
### ### Eastern/ South-East Europe (including Turkey)
  
data_se_europe <- subset.data.frame(data, data$Sub.Region == "Eastern and South-Eastern Europe (including Turkey)")
data_se_europe[] <- lapply(data_se_europe, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_se_europe$Nusers_best, na.rm = TRUE)
  pop <- unique(data_se_europe$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_se_europe$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_se_europe$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  1.10453226250297  ### possibly find out the methodology to calculate this
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_se_europe$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_se_europe$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 3.53277877285894   ### possibly find out the methodology to calculate this
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_se_europe$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Eastern and South-Eastern Europe (including Turkey)", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  
### ### Western and Central Europe
  
data_w_europe <- subset.data.frame(data, data$Sub.Region == "Western and Central Europe")
data_w_europe[] <- lapply(data_w_europe, function(x) if(is.factor(x)) factor(x) else x)
  
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_w_europe$Nusers_best, na.rm = TRUE)
  pop <- unique(data_w_europe$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- (pop[2]*1000)*(prev_reg/100)
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_w_europe$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_w_europe$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  3.81190800283294
  
  users_nr_low_NA <- (pop[2]*1000)*(prev_reg_low/100)
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_w_europe$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_w_europe$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 11.0071012910807
  
  users_nr_high_NA <- (pop[2]*1000)*(prev_reg_high/100)
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_w_europe$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Western and Central Europe", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)



  
#############
#### Oceania
######
  
  
### ### Australia and New Zealand
  
data_au_nz <- subset.data.frame(data, data$Sub.Region == "Australia and New Zealand")
data_au_nz[] <- lapply(data_au_nz, function(x) if(is.factor(x)) factor(x) else x)
  
  
  
  ### Total available users + prev
  
  ## best
  users_nr_best <- sum(data_au_nz$Nusers_best, na.rm = TRUE)
  pop <- unique(data_au_nz$sum_pop_avail_1564_subr)
  tpop_av <- pop[1]
  prev_reg <- 100*users_nr_best/(1000*tpop_av)
  prev_reg
  
  users_nr_best_NA <- 0
  
  #number of users best
  nusers_best_sreg <- users_nr_best+users_nr_best_NA
  nusers_best_sreg
  
  #prevalence best
  prev_best_sreg <- nusers_best_sreg/(unique(data_au_nz$pop_sum_15_64_subr)*10)
  prev_best_sreg
  
  
  
  ## low
  users_nr_low <- sum(data_au_nz$Nusers_low, na.rm = TRUE)
  prev_reg_low <-  1.46303487634295  ### possibly find out the methodology to calculate this
  
  users_nr_low_NA <- 0
  
  #number of users low
  nusers_low_sreg <- users_nr_low+users_nr_low_NA
  nusers_low_sreg
  
  #prevalence low
  prev_low_sreg <- nusers_low_sreg/(unique(data_au_nz$pop_sum_15_64_subr)*10)
  prev_low_sreg
  
  
  
  ## high
  users_nr_high <- sum(data_au_nz$Nusers_high, na.rm = TRUE)
  prev_reg_high <- 2.46784275281787   ### possibly find out the methodology to calculate this
  
  users_nr_high_NA <- 0
  users_nr_high_NA
  
  #number of users high
  nusers_high_sreg <- users_nr_high+users_nr_high_NA
  nusers_high_sreg
  
  #prevalence high
  prev_high_sreg <- nusers_high_sreg/(unique(data_au_nz$pop_sum_15_64_subr)*10)
  prev_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Australia and New Zealand", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  

### ### Polynesia
  
data_polynesia <- subset.data.frame(data, data$Sub.Region == "Polynesia")
data_polynesia[] <- lapply(data_polynesia, function(x) if(is.factor(x)) factor(x) else x)
  
  
### Total available users + prev

  ## best
  pop <- unique(data_polynesia$sum_pop_avail_1564_subr)
  
  prev_best_sreg <- 0.14892416649285
  prev_best_sreg
  
  nusers_best_sreg <- (prev_best_sreg * pop)*10
  nusers_best_sreg


  ## low
  prev_low_sreg <- 0.0000
  prev_low_sreg
  
  nusers_low_sreg <- (prev_low_sreg * pop)*10
  nusers_low_sreg



  ## high
  prev_high_sreg <- 0.14892416649285
  prev_high_sreg
  
  nusers_high_sreg <- (prev_high_sreg * pop)*10
  nusers_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Polynesia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  

  
  
  
### ### Melanesia
  
data_melanesia <- subset.data.frame(data, data$Sub.Region == "Melanesia")
data_melanesia[] <- lapply(data_melanesia, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  pop <- unique(data_melanesia$sum_pop_avail_1564_subr)
  
  prev_best_sreg <- 0.14892416649285
  prev_best_sreg
  
  nusers_best_sreg <- (prev_best_sreg * pop)*10
  nusers_best_sreg
  
  
  ## low
  prev_low_sreg <- 0.0000
  prev_low_sreg
  
  nusers_low_sreg <- (prev_low_sreg * pop)*10
  nusers_low_sreg
  
  
  
  ## high
  prev_high_sreg <- 0.14892416649285
  prev_high_sreg
  
  nusers_high_sreg <- (prev_high_sreg * pop)*10
  nusers_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Melanesia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
### ### Micronesia
  
  data_micronesia <- subset.data.frame(data, data$Sub.Region == "Micronesia")
  data_micronesia[] <- lapply(data_micronesia, function(x) if(is.factor(x)) factor(x) else x)
  
  
  ### Total available users + prev
  
  ## best
  pop <- unique(data_micronesia$sum_pop_avail_1564_subr)
  
  prev_best_sreg <- 0.14892416649285
  prev_best_sreg
  
  nusers_best_sreg <- (prev_best_sreg * pop)*10
  nusers_best_sreg
  
  
  ## low
  prev_low_sreg <- 0.0000
  prev_low_sreg
  
  nusers_low_sreg <- (prev_low_sreg * pop)*10
  nusers_low_sreg
  
  
  
  ## high
  prev_high_sreg <- 0.14892416649285
  prev_high_sreg
  
  nusers_high_sreg <- (prev_high_sreg * pop)*10
  nusers_high_sreg
  
  
  ## Add data to the reg estimates tables
  reg_est_sum <- c("Micronesia", nusers_best_sreg, nusers_low_sreg, nusers_high_sreg, prev_best_sreg, prev_low_sreg, prev_high_sreg)
  regional_estimates <- rbind(regional_estimates, reg_est_sum)
  
  
  
  
  
### Clean summary table
  
  
regional_estimates <- regional_estimates[!is.na(regional_estimates$Sub.Region), ]


reg_est_working <- regional_estimates



# round number of users
regional_estimates[,2] <- round(as.numeric(regional_estimates[,2]), digits = 0)
regional_estimates[,3] <- round(as.numeric(regional_estimates[,3]), digits = 0)
regional_estimates[,4] <- round(as.numeric(regional_estimates[,4]), digits = 0)

# round number of users
regional_estimates[,2] <- round((regional_estimates[,2]/1000), digits = 0)
regional_estimates[,3] <- round((regional_estimates[,3]/1000), digits = 0)
regional_estimates[,4] <- round((regional_estimates[,4]/1000), digits = 0)


# round prevalence
regional_estimates[,5] <- round(as.numeric(regional_estimates[,5]), digits = 2)
regional_estimates[,6] <- round(as.numeric(regional_estimates[,6]), digits = 2)
regional_estimates[,7] <- round(as.numeric(regional_estimates[,7]), digits = 2)


#merge region name
#reg_sub_list <- cbind.data.frame(data$Region, data$Sub.Region)
#reg_sub_list <- reg_sub_list[!duplicated(reg_sub_list), ]
#names(reg_sub_list) <- c("Region", "Sub.Region")

#regional_estimates <- left_join(x=regional_estimates, y=reg_sub_list, by="Sub.Region")
#regional_estimates <- arrange.vars(regional_estimates, c("Region"= 1, "Sub.Region"=3))

#View(regional_estimates)
#View(reg_est_working)



### Merge regional estimates with countries with missing data

setdiff(reg_est_working$Sub.Region, data$Sub.Region)
setdiff(data$Sub.Region, reg_est_working$Sub.Region)

unique(reg_est_working$Sub.Region)




### Export results

write.csv(reg_est_working, "Cannabis_sreg_estimates_23Jan.csv", row.names = F, na= "")
write.csv(regional_estimates, "Cannabis_sreg_estimates_clean_23Jan.csv", row.names = F, na= "")





############################### End of regional estimates tables ######################################



