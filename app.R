
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
ghg_inventory$year <- as.factor(ghg_inventory$year)






##### SHINY APP CHUNKS #####

# User Interface

ui <- fluidPage(theme = shinytheme("cosmo"),
                
    # Title
    titlePanel("Washington D.C. Citywide Greenhouse Gas Emissions: 2006-2020")
                
    # Sidebar
    sidebarLayout(
        sidebarPanel(
                    
            # Inputs
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

