
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
  

# Create subset showing total emissions for each year
dcemissions_year <- dcemissions %>%
                        group_by(year) %>%
                        mutate(totalemissions = sum(emissions, na.rm = TRUE)) %>%
                        select(year, totalemissions) %>%
                        unique()

# Create subset showing total emissions for each source in each year
dcemissions_sources <- dcemissions %>%
                       group_by(source, year) %>%
                       mutate(source_emissions = sum(emissions, na.rm = TRUE)) %>%
                       select(source, year, source_emissions, emissions) %>%
                       unique()
dcemissions_sources <- data.frame(dcemissions_sources)

# Create subset showing total emissions for each source in each year
dcemissions_sectors <- dcemissions %>%
                       group_by(sector, year) %>%
                       mutate(sector_emissions = sum(emissions, na.rm = TRUE)) %>%
                       select(sector, year, sector_emissions, emissions) %>%
                       unique()                          
dcemissions_sectors <- data.frame(dcemissions_sectors)
                          

##### SHINY APP CHUNKS #####

# User Interface

ui <- dashboardPage(skin = "green",
                
    # Title
    dashboardHeader(title = "Washington D.C. Citywide Greenhouse Gas Emissions",
                    titleWidth = 650),
                
    # Sidebar
    dashboardSidebar(
        sidebarMenu(id = "tabs",
          
            # Menu Items ----------------------------------------------
            menuItem("Annual Emissions", icon = icon("leaf"), tabName = "Emissions"),
            menuItem("Emission Sources", icon = icon("gas-pump"), tabName = "Sources"),
            menuItem("Emission Sectors", icon = icon("building"), tabName = "Sectors"),
                    
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
        )
    ),
                  
    # Body
    dashboardBody(
      tabItems(  
        
        # Group 1: Total Emissions
        tabItem(tabName = "Emissions",
                fluidRow(
                  box(
                    title = h4(strong("Total Greenhouse Gas Emissions (tons)"), align = "center"),
                    width = 12, 
                    br(),
                    infoBoxOutput("useryear", width = 6),
                    valueBoxOutput("totalannualemissions", width = 6)
                  )
                ),
                fluidRow(
                  box(width = 12, plotOutput("annualemissions"))
                )),
        
        # Group 2: Total Emissions by Source
        tabItem(tabName = "Sources",
                fluidRow(
                  box(
                    title = h4(strong("Greenhouse Gas Emissions from Various Sources (in tons)"), align = "center"),
                    width = 12,
                    br(), 
                    valueBoxOutput("emissions_sourceyear", width = 6),
                    infoBoxOutput("emissions_sourcetotal", width = 6)
                  )
                ),
                fluidRow(
                  box(width = 12, plotOutput("sourceemissions"))
                ),
                fluidRow(
                  box(
                    width = 12,
                    h4(strong("Data: Greenhouse Gas Emissions (tons) from Each Source, Across Years"),align = "center"),
                    br(),
                    br(),
                    DT::dataTableOutput("sourceemissions_table"),
                    collapsible = TRUE
                  )
                )
          ),

        # Group 3: Total Emissions by Sector
        tabItem(tabName = "Sectors",
                fluidRow(
                  box(
                    title = h4(strong("Greenhouse Gasses Emitted by Various Sectors (in tons)"), align = "center"),
                    width = 12, 
                    br(),
                    valueBoxOutput("emissions_sectoryear", width = 6),
                    infoBoxOutput("emissions_sectortotal", width = 6)
                  )
                ),
                fluidRow(
                  box(width = 12, plotOutput("sectoremissions"))
                ),
                fluidRow(
                  box(
                    width = 12, 
                    h4(strong("Data: Greenhouse Gas Emissions (tons) from Each Sector, Across Years"), align = "center"),
                    br(),
                    br(),
                    DT::dataTableOutput("sectoremissions_table"),
                    collapsible = TRUE
                  )
                )
          )
      )
    )
)


# Server

server <- function(input, output) {
    
    
##### TAB 1: TOTAL EMISSIONS BY YEAR #####
    
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
      annualemissions_chosenyear <- reactive({
        req(input$selected_year)
        
        totalemissions <- emissions_chosenyear()
        totalemissions$annual_emissions[1]
      })

    # Output: Value box showing the total emissions in user-selected year
    output$totalannualemissions <- renderValueBox({
        valueBox(comma(round(annualemissions_chosenyear(), 0)), 
                 "Total Annual Emissions",
                 icon = icon("leaf", lib = "font-awesome"),
                 color = "green")
    })  

    
    # Output: Info box showing the user-selected year
    output$useryear <- renderInfoBox({
        infoBox(title = "Year",
                value = h5(strong(input$selected_year), style = "font-size:40px;"),
                icon = icon("calendar"), 
                color = "green", fill = TRUE)
    })

    # Output: bar graph of total emissions by year
    output$annualemissions <- renderPlot({
        ggplot(dcemissions_year, aes(x = year, y = totalemissions, 
                                     fill = factor(
                                       ifelse(year == input$selected_year, 
                                              "Highlighted", "Normal")
                                     )
               )) + 
        geom_col() + 
        labs(title = "Annual Greenhouse Gas Emissions in Washington D.C.") +
        xlab("Year") + ylab("Emissions (millions of tons)") + 
        scale_y_continuous(labels = label_number(scale = 0.000001)) + 
        scale_fill_manual(values = c("#00a951","grey80")) + 
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
        theme(legend.position = "none")
    })
    

##### TAB 2: TOTAL EMISSIONS BY SOURCE AND YEAR #####     
    
    # Output: Info box showing emissions by selected source 
    
        # Create a reactive Value to retrieve the total emissions for source across all years
        emissions_chosensource <- reactive({
          req(input$selected_source)
          
          source_allyears <- dcemissions %>%
            group_by(source) %>%
            mutate(source_emissions = sum(emissions, na.rm = TRUE)) %>%
            select(source, source_emissions) %>%
            filter(source == input$selected_source) %>%
            unique()
          
          source_allyears[2]
        })  
        
        # Create info box showing total emissions for selected source and year
        output$emissions_sourcetotal <- renderInfoBox({
          infoBox(value = h5(strong(prettyNum(round(emissions_chosensource(),0),
                                     big.mark = ",")), style = "font-size:35px;"),
                   title = "Total Emissions",
                   subtitle = paste0(toupper(input$selected_source), ":  2006 to 2020"),
                   icon = icon("leaf", lib = "font-awesome"),
                   color = "yellow", fill = TRUE)
        })
        
        
    # Output: Value box showing emissions by selected source in selected year
    
        # Create a reactive Value to retrieve the total emissions for source in selected year
        sourceemissions_chosenyear <- reactive({
          req(input$selected_year, input$selected_source)
        
          sourcetotals_chosenyear <- emissions_chosenyear() %>%
              group_by(source) %>%
              filter(source == input$selected_source) %>%
              select(source, source_emissions) %>%
              unique()
             
        sourcetotals_chosenyear[2]
        })    
      
        # Create value box showing total emissions for selected source and year
        output$emissions_sourceyear <- renderValueBox({
          valueBox(value = prettyNum(round(sourceemissions_chosenyear(),0),
                                     big.mark = ","),
                   subtitle = paste0("Emissions from ", input$selected_source,
                                     " in ", input$selected_year),
                   icon = icon("gas-pump", lib = "font-awesome"),
                   color = "yellow")
          
        })
        

        
    # Output: bar graph of total emissions by year within user-selected source
      
        read_source <- reactive({
            req(input$selected_source)
          
            dcemissions_sources %>%
            filter(source == input$selected_source)
        })
        
       output$sourceemissions <- renderPlot({
          ggplot(data = read_source(), aes(x = year, y = emissions)) + 
              geom_col(aes(fill = factor(
                              ifelse(year == input$selected_year, 
                              "Highlighted", "Normal"))
                           )
                      ) + 
              labs(title = paste0("Greenhouse Gas Emissions from ", input$selected_source)) +
              xlab("Year") + ylab("Emissions (tons)") +
              scale_y_continuous(labels = comma) +
              scale_fill_manual(values = c("#fb9504","grey80")) + 
              theme_classic() + 
              theme(plot.title = element_text(hjust = 0.5, size = 18)) +
              theme(legend.position = "none")
      })

       
    # Output: data table showing total emissions by year within user-selected source
       totalsourceemissions <- reactive({
         req(input$selected_source)
         
         dcsources <- dcemissions_sources %>%
           select(source, year, source_emissions) %>%
           filter(source == input$selected_source) %>%
           unique()
         
         dcsources$source_emissions <- prettyNum(dcsources$source_emissions,
                                                 big.mark = ",")
         colnames(dcsources) <- c("Energy Source", "Year", "Total Source Emissions")
         
         dcsources
       })
       
       
       
       output$sourceemissions_table <- DT::renderDataTable({
        DT::datatable(data = totalsourceemissions())
      })
       
         
##### TAB 3: TOTAL EMISSIONS BY SECTOR AND YEAR #####     
        
    # Output: Info box showing emissions by selected sector 
        
        # Create a reactive Value to retrieve the total emissions for sector across all years
        emissions_chosensector <- reactive({
          req(input$selected_sector)
          
          sector_allyears <- dcemissions %>%
            group_by(sector) %>%
            mutate(sector_emissions = sum(emissions, na.rm = TRUE)) %>%
            select(sector, sector_emissions) %>%
            filter(sector == input$selected_sector) %>%
            unique()
          
          sector_allyears[2]
        })  
        
        # Create info box showing total emissions for selected sector and year
        output$emissions_sectortotal <- renderInfoBox({
          infoBox(value = h5(strong(prettyNum(round(emissions_chosensector(),0),
                                     big.mark = ",")), style = "font-size:35px;"),
                  title = "Total Emissions",
                  subtitle = paste0(toupper(input$selected_sector), ":  2006 to 2020"),
                   icon = icon("leaf", lib = "font-awesome"),
                   color = "blue", fill = TRUE)
        })
        
    
    # Output: Value box showing emissions by selected sector in selected year
        
        # Create a reactive Value to retrieve the total emissions for sector in selected year
        sectoremissions_chosenyear <- reactive({
          req(input$selected_year, input$selected_sector)
          
          sectortotals_chosenyear <- dcemissions %>%
            filter(year == input$selected_year) %>%
            group_by(sector, year) %>%
            mutate(sector_emissions = sum(emissions, na.rm = TRUE)) %>%
            ungroup() %>%
            filter(sector == input$selected_sector) %>%
            select(sector, sector_emissions) %>%
            unique()
          
          sectortotals_chosenyear[2]

        })
        
        
        # Create value box showing total emissions for selected sector and year
        output$emissions_sectoryear <- renderValueBox({
          valueBox(value = prettyNum(round(sectoremissions_chosenyear(),0),
                                     big.mark = ","),
                   subtitle = paste0("Emissions from ", input$selected_sector,
                                     " Sector in ", input$selected_year),
                   icon = icon("building", lib = "font-awesome"),
                   color = "blue")
        })
        
    # Output: bar graph of total emissions by year within user-selected sector
        
        read_sector <- reactive({
          req(input$selected_sector)
          
          dcemissions_sectors %>%
            filter(sector == input$selected_sector)
        })
        
        output$sectoremissions <- renderPlot({
          ggplot(data = read_sector(), 
                 aes(x = year, y = emissions)) + 
            geom_col(aes(fill = factor(
                            ifelse(year == input$selected_year,
                                   "Highlighted", "Normal"))
                        )
            ) + 
          labs(title = paste0("Greenhouse Gas Emissions in the ", input$selected_sector,
                              " Sector")) + 
          xlab("Year") + ylab("Emissions (tons)") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = c("#0075bd", "grey80")) + 
          theme_classic() + 
          theme(plot.title = element_text(hjust = 0.5, size = 18)) + 
          theme(legend.position = "none")
        })

        
    # Output: data table showing total emissions by year within user-selected sector
        totalsectoremissions <- reactive({
          req(input$selected_sector)
          
          dcsectors <- dcemissions_sectors %>%
            select(sector, year, sector_emissions) %>%
            filter(sector == input$selected_sector) %>%
            unique()
          
          dcsectors$sector_emissions <- prettyNum(dcsectors$sector_emissions,
                                                  big.mark = ",")
          colnames(dcsectors) <- c("Sector", "Year", "Total Sector Emissions")
          
          dcsectors
        })

        output$sectoremissions_table <- DT::renderDataTable({
          DT::datatable(data = totalsectoremissions())
        })
    
}
    
# Call ShinyApp
    
shinyApp(ui = ui, server = server)

