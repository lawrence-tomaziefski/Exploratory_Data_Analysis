###Week 1 project another way
#Exploratory Project 1
#prepdata.R
#by Ray Lugo, Jr.
#2016-12-03

#The purpose of "prepdata.R" is to:
#1. Isolate records for analysis from a large dataset.
#2. Extract the rows that fall within the date range of 7/1/2016 to 7/2/2016
#3. Validate the variable data types.
#4. Develop scripts to generate plots per project specifications.
#5. Save each resulting plot to a PNG file with the corresponding plot name.

#_______________________________________________________________________________

#Script Begins

#clear workspace of prior objects to free memory.
rm(list = ls())

#function to install and load libraries that are not already installed or loaded
#using very cool approach found here https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}
print(paste("started at :", Sys.time()))

print("loading libraries.")
#create vector of libraries and pass into the above function.
libraries <- c("tibble","data.table","dtplyr","dplyr","readr","lubridate")
ipak(libraries)

#remove the objects since they will not be used again this session
rm("libraries","ipak")

#check for data folder.  Create one if none exists
if (!file.exists("./data")) { dir.create("./data")}

#get "UCI data" archive by using libcurl, which allows for OS independence.
#set the url value.
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

#create a sourcefile variable for reference when downloading and unzipping
sourceFile <- "./data/hpc.zip"

print("downloading data archive.")
#check if file has already been downloaded.  If it does not exist, then download it.
if (!file.exists(sourceFile)) {
        download.file(url, destfile = "./data/hpc.zip", method = "libcurl")
}

loadfile <- "household_power_consumption.txt"

print("unzipping data files")
#using "unzip", extract only those file.  using "jumkpaths" removes any internal folder structure
#to allow all files to be placed as designated, in this case, all in the root of the "./data" folder.
unzip(sourceFile, loadfile, list = FALSE, junkpaths = TRUE, exdir = "./data", unzip = "internal")


print("loading file into data tables.")
#use readr::read_delim, set date and time fields to character and default others to decimal.
hpc <- read_delim(paste0("./data/",loadfile), ";", col_names = TRUE, na = c("?",NA), col_types = cols(
        Date = col_character(),
        Time = col_character(),
        .default = col_double()
))

print("Isolating rows according to required dates.")
#subset hpc to isolate the rows with the determined dates and save them in a hpc_set
hpc_set <- hpc[(hpc$Date == "1/2/2007" | hpc$Date == "2/2/2007"),]

#remove large dataset from memory
rm("hpc")

print("creating date-time variable.")
#create concatenated date-time field to simplify datetime operations.
obsDateTime <- paste(hpc_set$Date,hpc_set$Time)

print("parsing dates and times for each pertinent table column.")
#Convert Date column values into true R date values.
hpc_set$Date <- parse_date(hpc_set$Date,"%d/%m/%Y")

#Convert Time column values into true R time values
hpc_set$Time <- parse_time(hpc_set$Time,"%H:%M:%S")

#Convert new obsDateTime column values into true R datetime values
obsDateTime <- parse_date_time2(obsDateTime, "%d/%m/%Y %H:%M:%S")

print("cbinding new datetime column to existing dataset.")
#bind the new column to the existing dataset
hpc_set <- cbind(obsDateTime,hpc_set)

#cleanup
rm("obsDateTime")

#check for images folder.  Create one if none exists
if (!file.exists("./images")) { dir.create("./images")}

print("plotting graphs and saving each to PNG file.")
#load plot scripts to generate and save each to their own PNG files

print("generating plot1")
source("plot1.R")

print("generating plot2")
source("plot2.R")

print("generating plot3")
source("plot3.R")

print("generating plot4")
source("plot4.R")

#clear workspace of prior objects to free memory.
rm(list = ls())

print("Processing completed!")
#Script ends