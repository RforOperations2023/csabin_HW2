# Homework 2: Create a Shiny Dashboard


## Assignment

* Include at least three (3) input/filters
* Create three (3) single numeric based boxes/gauges
* Create one (1) data table
* Create three (3) interactive and reactively responsive charts
* Place elements throughout dashboard, with at least three (3) pages or tabs <br> with an analytical theme or question about the data on each page
* On the server side, plots and tables must utilize the reactive function for any and all datasets
* Final app must work when deployed to shinyapps.io

## Data Source

The data used for this project represents the results of the annual Greenhouse Gas Inventory conducted in Washington D.C. It represents the citywide and government-specific emissions between 2006 and 2020. 

The collection of this data was the result of an initiative by the Department of Energy & Environment (DOEE) of Washington D.C. The DOEE has two specific environmental goals that they have set for the District: 
* reducing emissions 60% by 2030 
* reaching net-zero emissions by 2045
<br> These data represent the Department's efforts to monitor the city's progress toward these two important goals. 

The raw data can be found here: [Open Data DC: Greenhouse Gas Inventory](https://opendata.dc.gov/datasets/DCGIS::greenhouse-gas-inventory/about)

Learn more about the DOEE's emission goals here: [DOEE Greenhouse Gas Inventories](https://doee.dc.gov/service/greenhouse-gas-inventories)

## R Shiny App

I created a web application using R Shiny to depict certain facets of this greenhouse gas inventory data. 

#### Annual Greenhouse Gas Emissions 
The first page of this dashboard, titled "Annual Emissions," represents the total greenhouse gas emissions in Washington D.C. for each year between 2006 and 2020. As data from 2007 and 2008 were null across the dataset, these two years have been excluded from all visualizations and analyses. The first page focuses on total emissions to give the user a "baseline" against which the sector and source breakdowns on following pages can be compared.

The interface gives the user options to change the figures presented in the Value Boxes by selecting the year they want to focus on. When the user selects a new year, the value boxes and the fill color on the bar chart change so as to reflect the emissions data specifically from the user-chosen year.

#### Greenhouse Gas Emissions from Various Sources
The second page of this dashboard is titled "Emission Sources," and focuses on the various sources that the DOEE identified as causing greenhouse gas emissions. The relevant emissions sources include: Compost, Diesel, Electricity, Fuel Oil, 

# FIX FROM HERE DOWN.
panel expands the scope of the data analysis, focusing not only on emissions from waste but also on those emissions from the full range of sectors. The interface gives the user the ability to change the year for which these data depict. There are two bar graphs between the tabs, showing the top five sectors with the most emissions and consumption values, respectively, in the user-selected year. The annual focus aims to allow the user to "zoom in" on a single year and dive into the sectors with the highest Greenhouse Gas numbers during that year.


#### Annual Greenhouse Gas Emissions and Consumption Originating from Any Sector
The lower panel expands the scope of the data analysis, focusing not only on emissions from waste but also on those emissions from the full range of sectors. The interface gives the user the ability to change the year for which these data depict. There are two bar graphs between the tabs, showing the top five sectors with the most emissions and consumption values, respectively, in the user-selected year. The annual focus aims to allow the user to "zoom in" on a single year and dive into the sectors with the highest Greenhouse Gas numbers during that year.

#### Application
The web application can be viewed on <ins>shinyapps.io</ins>, at the following link: [Washington D.C. Citywide Greenhouse Gas Emissions](https://7bewzr-caroline-sabin.shinyapps.io/CSabin_GHGEmissions_Sources_Sectors/?_ga=2.76396565.147871873.1676476961-153993422.1674242848)


