
### Caroline Sabin
### 94-880 Section A3
### R Shiny for Operations Management
### HW 2: Create a Dashboard 

#### PROJECT SET-UP ####

# Load libraries ----------------------------------------------------------
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)


# Load data from directory -------------------------------------------------
ghg_inventory <- read.csv("Greenhouse_Gas_Inventory.csv")


#### DATA MANIPULATION ####

# Change column names
colnames(ghg_inventory) <- c("inventory", "sector", "source", "year", 
                             "consumption_units","consumption", "emissions", "ID")

# Change to df
ghg_inventory <- data.frame(ghg_inventory)

# Change variables to factors
ghg_inventory$inventory <- as.factor(ghg_inventory$inventory)
ghg_inventory$sector <- as.factor(ghg_inventory$sector)
ghg_inventory$source <- as.factor(ghg_inventory$source)
ghg_inventory$year <- as.factor(ghg_inventory$year)


# Create a subset of citywide emissions
dcemissions <- ghg_inventory %>%
                  select("inventory", "sector", "source", "year", "emissions") %>%
                  filter(inventory == "Citywide")

# Combine similar sectors into roll-up categories
dcemissions$sector <- recode_factor(dcemissions$sector, 
                          "Buildings & Energy: Non-Residential" = "Buildings & Facilities", 
                          "Buildings & Energy: Residential" = "Buildings & Facilities", 
                          "Fleet: DC Circulator" = "Fleet", 
                          "Fleet: DC Circulator & DC Streetcar Electricity" = "Fleet", 
                          "Fleet: On-Road" = "Fleet", 
                          "Grid Loss: Water & Wastewater" = "Water & Wastewater", 
                          "Solid Waste" = "Waste", 
                          "Transportation: Transit" = "Transportation")

# Combine similar sources into roll-up categories
dcemissions$source <- recode_factor(dcemissions$source, 
                          "Biodiesel" = "Diesel",
                          "CNG" = "Natural Gas",
                          "Fossil Gas Distribution" = "Natural Gas",
                          "Gas" = "Gasoline",
                          "Process Emissions" = "Fugitive Emissions")
                          
                          

##### SHINY APP CHUNKS #####

# User Interface

ui <- fluidPage(
                
    # Title
    title <- titlePanel("Washington D.C. Citywide Greenhouse Gas Emissions"),
                
    # Sidebar
    sidebarLayout(
        sidebarPanel(
                    
            # Input: allow user to choose a range of years
            radioButtons(inputId = "years_selected", label = "Year: ",
                         choices = c("2006", "2009", "2010", "2011", 
                                     "2012", "2013", "2014", "2015",
                                     "2016", "2017", "2018", "2019", "2020"),
                              selected = "2020"),
            
            # Input: allow user to select sectors to look at
            selectInput(inputId = "sectors",
                        label = "Selected sector(s): ",
                        choices = c(
                               "Buildings & Facilities",
                               "Employee Commute",
                               "Fleet",
                               "Fugitive Emissions",
                               "Streetlights & Traffic Signals",
                               "Transportation",
                               "Waste",
                               "Water & Wastewater"),
                        selected = "Transportation"),
            
            # Input: allow user to select source(s) to look at
            selectInput(inputId = "sectors",
                        label = "Selected sector(s): ",
                        choices = c("Compost", "Diesel", 
                                    "Electricity", "Ethanol",
                                    "Fuel Oil", "Fugitive Emissions",
                                    "Gasoline", "Grid Loss", 
                                    "Incineration", "Kerosene", "Landfill", 
                                    "Natural Gas", "Steam"),
                        selected = "Electricity")
        ),
                  
    # Main Panel
        mainPanel(
            # Outputs
                  )
    )
)




# Server

server <- function(input, output) {}
    
    


# Call ShinyApp
    
shinyApp(ui = ui, server = server)

