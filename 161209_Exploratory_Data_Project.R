#Exploratory Data Analysis Project
#
#by Lawrence Tomaziefski
#2016-12-09

#_______________________________________________________________________________

#Script Begins

#clear workspace of prior objects to free memory.
rm(list = ls())

#set working directory
setwd('/Users/lawrence_tomaziefski/R_Working_Directory/161130_Exploratory_Data_Analysis')

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
libraries <- c("tibble","data.table","dtplyr","dplyr","readr","lubridate","ggplot2","RColorBrewer","gridExtra","devtools")
ipak(libraries)
install_github("plotflow", "trinker")
#remove the objects since they will not be used again this session
rm("libraries","ipak")

#check for data folder.  Create one if none exists
if (!file.exists("./data")) { dir.create("./data")}

#get "emmisions data" archive by using libcurl, which allows for OS independence.
#set the url value.
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

#create a sourcefile variable for reference when downloading and unzipping
sourceFile <- "./data/emmisions.zip"

print("downloading data archive.")
#check if file has already been downloaded.  If it does not exist, then download it.
if (!file.exists(sourceFile)) {
        download.file(url, destfile = "./data/emmisions.zip", method = "libcurl")
}

loadfile = c("Source_Classification_Code.rds","summarySCC_PM25.rds")

print("unzipping data files")
#using "unzip", extract only those file.  using "jumkpaths" removes any internal folder structure
#to allow all files to be placed as designated, in this case, all in the root of the "./data" folder.
unzip(sourceFile, loadfile, list = FALSE, junkpaths = TRUE, exdir = "./data", unzip = "internal")

#create a path to the data
path = ("./data/")

print("loading file into data tables.")

#loading the required files using readRDS
nei_summary = readRDS(paste0(path,"summarySCC_PM25.rds"))
scc_source =  readRDS(paste0(path,"Source_Classification_Code.rds"))
pic_width = 480
pic_height = 480  
#take out some extraneous data from the scc_source data frame to keep the SCC identifier and source categories
scc_source = select(scc_source, -2, -(5:6), -(11:15))

#merge the nei_summary and the scc_source data frames on the SCC code
combined = inner_join(nei_summary, scc_source, by = "SCC")
#-------------------------------------------------------------------------
###total US PM25 emissions
total_emissions = combined %>%
                group_by(year) %>%
                summarize(total = sum(Emissions)) %>%
                mutate(total = total/1000000) 
        
        
with(total_emissions, barplot(height = total,
                             width = .5,
                             names.arg = year,
                             col = "red",
                             xlab = "Year",
                             ylab = "Total PM 2.5 Emissions Tonnage (in millions)",
                             main = "U.S. PM 2.5 Emissions",
                             font = 2))
                               
dev.copy(png, file = paste0(path,"plot1.png"), width = pic_width, height = pic_height)
dev.off()    
#-------------------------------------------------------------------------
##total emissions in Baltimore City
baltimore_emissions = combined %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(total = sum(Emissions)) %>%
        mutate(total = total/1000) 
                        
with(baltimore_emissions, barplot(height = total,
                                      width = .5,
                                      names.arg = year,
                                      col = "red",
                                      xlab = "Year",
                                      ylab = "Total PM 2.5 Emissions Tonnage (in thousands)",
                                      main = "Baltimore City PM 2.5 Emissions",
                                      font = 2))

dev.copy(png, file = paste0(path,"plot2.png"), width = pic_width, height = pic_height)
dev.off()  

baltimore_type = combined %>%
        filter(fips == "24510") %>%
        group_by(type,year) %>%
        summarize(total = sum(Emissions)) %>%
        mutate(total = total/1000) 
        
balt_type = ggplot(baltimore_type, aes(x = year, y = total, group = type,color = type)) +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black"),
              plot.caption = element_text(hjust = 0.5)) + 
        geom_point() +    
        geom_line () +
        stat_smooth(method = "lm",se = FALSE, lwd = .5, lty = "dashed") + 
        labs(y = "Total PM 2.5 Emissions Tonnage (in thousands)",
                x = "Year",
                title = "Baltimore City PM 2.5 Emissions By Source Type",
                caption = "Dashed lines are the linear trend lines. \nPoint source emissions increase from 1999-2008") +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3)) + 
        scale_color_stata() 
balt_type
dev.copy(png, file = paste0(path,"plot3.png"), width = pic_width, height = pic_height)
dev.off()  
#-------------------------------------------------------------------------
#grep all "coal" references from Short.Name, and then find all combustion sources in SCC.Level.One
coal_short = grep("Coal",combined$Short.Name,value =TRUE)
coal_short_data = filter(combined,Short.Name %in% coal_short)
combustion_sources = grep("Combustion",coal_short_data$SCC.Level.One, value =TRUE)
combustion_source_data = filter(coal_short_data,SCC.Level.One %in% combustion_sources)
combustion_source_summary = combustion_source_data %>%
                                mutate(SCC.Level.Two = factor(SCC.Level.Two,SCC.Level.Two)) %>%                        
                                group_by(SCC.Level.Two,year) %>%
                                summarize(Average = mean(Emissions),
                                           Median = median(Emissions),
                                           total = sum(Emissions)/10000, 
                                           Max = max(Emissions), 
                                           Min = min(Emissions)) %>%
                                arrange(desc(total),SCC.Level.Two)
                                
combustion_type = ggplot(combustion_source_summary, aes(x = year, y = total)) +
        theme(panel.background = element_rect(fill = NA),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "grey95"),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black", face = "bold"),
              plot.caption = element_text(hjust = 0.5)) + 
        geom_point() +    
        geom_line () +
        stat_smooth(method = "lm",se = FALSE, lwd = .5, lty = "dashed") + 
        labs(y = "Total PM 2.5 Emissions Tonnage (in thousands)",
             x = "Year",
             title = "Coal Combustion PM 2.5 Emissions By Source",
             caption = "Most coal combustion sources have a sharp decrease in emissions from 2005-2008.
             However, industrial emissions see an overall increase from 1999-2008") +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3)) + 
        scale_linetype_stata() +
        facet_wrap(~SCC.Level.Two, scales = "free_y")

combustion_area = ggplot(combustion_source_summary,aes(x = year,
                             y = total,
                             fill = SCC.Level.Two)) +
        geom_area(color = "grey", size = 1, alpha = .6) +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
        scale_y_continuous(breaks = seq(0, 60, by = 10)) +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "black",linetype = "dashed"),
              panel.border = element_rect(color = "black",fill = NA,size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black", face = "bold"),
              plot.caption = element_text(hjust = 0.5)) +
        labs(y = "Total PM 2.5 Emissions Tonnage (in thousands)",
             x = "Year",
             title = " U.S. PM 2.5 Emissions from Coal Combustion Related Sources (1999-2008)",
             caption = "Overall PM 2.5 Emissions decrease in the U.S. from 1999-2000.
             The majority of emissions come from electrical generation and industry.") +
        scale_fill_gdocs("Source")
combustion = grid.arrange(combustion_area,combustion_type, ncol = 1)
combustion
dev.copy(png, file = paste0(path,"plot4.png"), width = 1000, height = 1000)
dev.off() 

#-------------------------------------------------------------------------
#make a data set with just motor vehicles for Baltimore City, split the combined data frame on type == On Road
baltimore_motor_data = combined %>%
                filter(type == "ON-ROAD" & fips == "24510") 
                  
vehicles = c(rep("Heavy Duty Diesel Vehicles",5),
             "Heavy Duty Gasoline Vehicles",
             rep("Light Duty Diesel Vehicles",2),
             rep("Light Duty Gasoline Vehicles",4))
vehicle_types = data.frame(SCC.Level.Three = unique(baltimore_motor_data$SCC.Level.Three),Vehicle = vehicles)
baltimore_motor_data = inner_join(baltimore_motor_data,vehicle_types, by = "SCC.Level.Three") 
baltimore_motor_data = baltimore_motor_data %>%
        select(Emissions,year,Vehicle) %>%
        group_by(Vehicle,year) %>%
        summarize(total = sum(Emissions))


motor_area = ggplot(baltimore_motor_data,aes(x = year,
                                                  y = total,
                                                  fill = Vehicle)) +
        geom_area(color = "black", size = .1, alpha = .6) +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3)) +
        scale_y_continuous(breaks = seq(0, 350, by = 50)) +
        theme(panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "black",linetype = "dashed"),
              panel.border = element_rect(color = "black",fill = NA,size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black", face = "bold"),
              plot.caption = element_text(hjust = 0.5)) +
        labs(y = "Total PM 2.5 Emissions in Tons",
             x = "Year",
             title = "Baltimore City PM 2.5 Emmisions from Motor Vehicle Sources 1999-2008") +
        scale_fill_gdocs("Vehicle Types")

motor_type = ggplot(baltimore_motor_data, aes(x = year, y = total)) +
        theme(panel.background = element_rect(fill = NA),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "grey95"),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black", face = "bold"),
              plot.caption = element_text(hjust = 0.5)) + 
        geom_point() +    
        geom_line () +
        stat_smooth(method = "lm",se = FALSE, lwd = .5, lty = "dashed") + 
        labs(y = "Total PM 2.5 Emissions Tonnage (in thousands)",
             x = "Year",
             title = "Baltimore City PM 2.5 Emmisions from Motor Vehicle Sources 1999-2008",
             caption = "There was a spike in PM 2.5 emissions from heavy duty gasoline vehicles
             from 2002-2005, However, all emission sources decreased sharply between 1999-2000,
        and decreased overall.") +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3)) + 
        scale_linetype_stata() +
        facet_wrap(~Vehicle, scales = "free_y")

baltimore_motor_plot = grid.arrange(motor_area, motor_type, ncol = 1)
dev.copy(png, file = paste0(path,"plot5.png"), width = 1000, height = 1000)
dev.off() 

#-------------------------------------------------------------------------
#make a data set with just motor vehicles for Baltimore City and Los Angeles, 
#split the combined data frame on type == On Road
combined_motor_data = combined %>%
        filter(type == "ON-ROAD" & (fips == "24510" | fips == "06037")) 
com_vehicle_types = data.frame(SCC.Level.Three = unique(combined_motor_data$SCC.Level.Three))
com_vehicle_types = arrange(com_vehicle_types,SCC.Level.Three)
com_vehicle_types$Vehicle = vehicles
city = data.frame(fips = c("24510", "06037"), City = c("Baltimore", "Los Angeles"))
combined_motor_data = inner_join(combined_motor_data, com_vehicle_types, by = "SCC.Level.Three") 
combined_motor_data = inner_join(combined_motor_data, city, by = "fips")
combined_motor_data = select(combined_motor_data,2,4,6,13,14)

baltimore_summary = combined_motor_data %>%
        select(Emissions,year,City) %>%
        filter(City == "Baltimore") %>%
        group_by(year) %>%
        summarize(total = sum(Emissions))
la_summary = combined_motor_data %>%
        select(Emissions,year,City) %>%
        filter(City == "Los Angeles") %>%
        group_by(year) %>%
        summarize(total = sum(Emissions))

percent_change = data.frame(City = c(rep(c("Baltimore"),4), rep(c("Los Angeles"),4)), 
                            Year = rep(c("1999", "2002", "2005", "2008"),2))
for (i in 1:4){
        percent_change$Change[i]= 
                ((baltimore_summary$total[1] - baltimore_summary$total[i])/baltimore_summary$total[1])*-100
}
for (j in 5:8){
        percent_change$Change[j]= 
                ((la_summary$total[1] - la_summary$total[j-4])/la_summary$total[1])*-100
}
percent_change$Year=as.numeric(levels(percent_change$Year))[percent_change$Year]
percent_change_graph = ggplot(percent_change, aes(x = Year, y = Change, fill = City, group = City, color = City)) +
        theme(panel.background = element_rect(fill = NA),
              panel.grid.minor = element_line(color = NA),
              panel.grid.major.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = "grey95"),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.title = element_text(hjust = 0.5),
              text = element_text(size = 10, color = "black", face = "bold"),
              plot.caption = element_text(hjust = 0.5)) + 
        geom_point() +    
        geom_line () +
        labs(y = "% Change in Motor Vehicle PM 2.5 Emissions",
             x = "Year",
             title = "Comparison of Baltimore and Los Angeles Motor Vehilce PM 2.5 Emissions
             Changes from 1999-2008") +
        scale_x_continuous(breaks = seq(1999, 2008, by = 3))  +
        scale_color_stata()

percent_change_graph
        dev.copy(png, file = paste0(path,"plot6.png"), width = 600, height = pic_height)
        dev.off() 
