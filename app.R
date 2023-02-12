
### Caroline Sabin
### 94-880 Section A3
### R Shiny for Operations Management
### HW 2: 

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