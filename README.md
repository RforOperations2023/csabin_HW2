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
The second page of this dashboard is titled "Emission Sources," and focuses on the various sources that the DOEE identified as causing greenhouse gas emissions. The relevant emissions sources include: Compost, Diesel, Electricity, Fuel Oil, Fugitive Emissions, Gasoline, Grid Loss, Incineration, Kerosene, Landfill, and Natural Gas. 

The value box on the left displays the user-selected source's emissions in a single year, while the info box on the right displays the total emissions from the source across all years (2006-2020). Additionally, there is a bar graph displaying the emissions for the selected source across all years (2006-2020), and a data table that reflects the same data subset in tabular form.

The interface gives the user options to change the figures presented by selecting both the year they want to focus on, *and* the emission source they are interested in exploring. When the user selects a new year, the value boxes on the left and the bar chart's fill change so as to reflect the emissions data specifically from the user-chosen year. When the user selects a different emission source, the value boxes, bar graph, and data table change to reflect a new subset of the data based on the selected source. These features should give the user the ability to "drill down" into the total emissions figures from the first page, based on what energy source and year they are interested in.

#### Greenhouse Gas Emissions by Different Sectors
The third page of this dashboard is titled "Emission Sectors," and focuses on the various sectors that the DOEE identified as produce and emit greenhouse gasses. The sectors included in this analaysis are: Buildings & Facilities, Fugitive Emissions, Transportation, Waste, and Water & Wastewater.

The value box on the left displays the total emissions in the user-selected sector for a single year, while the info box on the right displays the total emissions within that same sector across all years (2006-2020). Additionally, there is a bar graph displaying the emissions in the selected sector across all years (2006-2020), and a data table that reflects the same sector data in tabular form.

The interface gives the user options to change the figures presented by selecting both the year they want to focus on, *and* the sector of emissions they are interested in exploring. When the user selects a new year, the value boxes on the left and the bar chart's fill change so as to reflect the emissions data specifically from the user-chosen year. When the user selects a different sector, the value boxes, bar graph, and data table change to reflect a new subset of the data based on the selected sector. These features should give the user the ability to "drill down" into the total emissions figures from the first page, based on what Washington D.C. sector and year they are interested in.


#### Application
The web application can be viewed on <ins>shinyapps.io</ins>, at the following link: [Washington D.C. Citywide Greenhouse Gas Emissions](https://7bewzr-caroline-sabin.shinyapps.io/CSabin_GHGEmissions_Sources_Sectors/?_ga=2.76396565.147871873.1676476961-153993422.1674242848)


