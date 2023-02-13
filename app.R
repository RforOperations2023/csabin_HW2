
### Caroline Sabin
### 94-880 Section A3
### R Shiny for Operations Management
### HW 2: Create a Dashboard 

#### PROJECT SET-UP ####

# Load libraries ----------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinydashboard)
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

# Filter out sources and sectors with no related emissions data
dcemissions$source <- droplevels(dcemissions$source)
dcemissions$sector <- droplevels(dcemissions$sector)
  
                          
                          

##### SHINY APP CHUNKS #####

# User Interface

ui <- fluidPage(
                
    # Title
    title <- titlePanel("Washington D.C. Citywide Greenhouse Gas Emissions"),
                
    # Sidebar
    sidebarLayout(
        sidebarPanel(
                    
            # Input: allow user to choose a range of years
            radioButtons(inputId = "selected_year", label = "Year: ",
                         choices = c("2006", "2009", "2010", "2011", 
                                     "2012", "2013", "2014", "2015",
                                     "2016", "2017", "2018", "2019", "2020"),
                              selected = "2020"),
            
            # Input: allow user to select sectors to look at
            selectInput(inputId = "selected_sector",
                        label = "Selected sector(s): ",
                        choices = c(
                               "Buildings & Facilities",
                               "Fugitive Emissions",
                               "Transportation",
                               "Waste",
                               "Water & Wastewater"),
                        selected = "Transportation"),
            
            # Input: allow user to select source(s) to look at
            selectInput(inputId = "selected_source",
                        label = "Selected source(s): ",
                        choices = c("Compost", "Diesel", 
                                    "Electricity",
                                    "Fuel Oil", "Fugitive Emissions",
                                    "Gasoline", "Grid Loss", 
                                    "Incineration", "Kerosene", 
                                    "Landfill", "Natural Gas"),
                        selected = "Electricity")
        ),
                  
    # Main Panel
        dashboardBody(
            valueBoxOutput("totalannualemissions"),
             infoBoxOutput("useryear")
        )
    )
)




# Server

server <- function(input, output) {
    
    # Create a reactive subset containing data from the user-selected year
    # Add three columns: total emissions by source, by sector, and by year
    emissions_chosenyear <- reactive({
        req(input$selected_year)
      
        sector_chosenyear <- dcemissions %>% 
            filter(year == input$selected_year) %>%
            group_by(sector, year) %>%
            mutate(sector_emissions = sum(emissions, na.rm = TRUE)) %>% 
            ungroup() 
      
        source_chosenyear <- sector_chosenyear %>%
            filter(year == input$selected_year) %>%
            group_by(source, year) %>%
            mutate(source_emissions = sum(emissions, na.rm = TRUE)) %>%
            ungroup()
        
        total_chosenyear <- source_chosenyear %>%
            filter(year == input$selected_year) %>%
            group_by(year) %>%
            mutate(annual_emissions = sum(emissions, na.rm = TRUE))
    })
    
    # Create a reactive Value to retrieve the annual emissions for selected year
#    annualemissions_chosenyear <- reactiveValues({
#        req(input$selected_year)
#      
#        totalemissions <- emissions_chosenyear()$annual_emissions[1]
#      
#    })
    
    # Output: Value box showing the total emissions in user-selected year
    output$totalannualemissions <- renderValueBox({
          valueBox(emissions_chosenyear()$annualemissions[5], 
                 "Total Annual Emissions",
                 icon = icon("bolt", lib = "font-awesome"),
                 color = "green")
    })  

    
    # Output: Info box showing the user-selected year
    output$useryear <- renderInfoBox({
        infoBox("Year", value = input$selected_year,
                subtitle = "selected by the user",
                icon = icon("leaf"), 
                color = "green", fill = TRUE)
    })
    
}
    
# Call ShinyApp
    
shinyApp(ui = ui, server = server)

