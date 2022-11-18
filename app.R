# Load Packages ------
library(devtools)
library(dplyr)
library(ggplot2)
library(glue)
library(htmltools)
library(janitor)
library(maps)
library(mapproj)
library(mapview)
library(packrat)
library(readr)
library(rsconnect)
library(sf)
library(stringr)
library(shiny)
library(shinythemes)
library(tigris)

# Check that you're in an interactive R session
interactive()

# Set working directory 
setwd("C:/Users/wonasch/Desktop/app-project2")

# helpers.R facilitates creation of percent maps
source("helpers.R")

# Process Source Data-----
chr <- read.csv("data/chr2022.csv", skip = 1) %>% 
  select(
    statecode,
    countycode,
    fipscode,
    county,
    state,
    v009_rawvalue,
    v011_rawvalue,
    v070_rawvalue,
    v049_rawvalue,
    v139_rawvalue,	 
    v166_rawvalue) %>%
  mutate(
    pct_smokers = round(v009_rawvalue*100,2),
    pct_obese = round(v011_rawvalue*100,2),
    pct_inactive = round(v070_rawvalue*100,2),
    pct_drinkers = round(v049_rawvalue*100,2),
    pct_foodinsec = round(v139_rawvalue*100,2),
    pct_broadband = round(v166_rawvalue*100,2))
head(chr)


# Convert FIPS to character and Add Leading Zeros
chr <- chr %>% 
  mutate(fipschar = as.character(chr$fipscode))
chr$fipschar <- str_pad(chr$fipschar, width = 5, side = "left", pad = "0")
head(chr)

# Download Geography Shape Files ------
us_geo <- tigris::counties(cb=TRUE, year = 2020)

# Object Class Conversion + Join ----- 
# Joined table must be converted into an sf object 
join <- st_as_sf(inner_join(chr, us_geo, by = c("fipschar" = "GEOID")))
class(join) # check class 

# Check that counts match
n_distinct(chr$fipschar)    
n_distinct(join$fipschar)  

 

# User interface ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Community Health Measures by County, 2022"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create interactive demographic maps with 
        information from the 2022 County Health Rankings and Roadmaps."),
      
      selectInput(inputId = "var", 
                  label = "Choose a topic of interest:",
                  choices = c("Adult Obesity Rate", "Percentage of Adult Smokers",
                              "Physical Inactivity (Adults)", "Excessive or Binge Drinking",
                              "Food Insecure Population","Households with Highspeed Internet"),
                  selected = "Adult Obesity Rate"),
      selectInput(inputId = "color",
                  label = "Choose a color gradient for the map:",
                  choices = c("Purples", "Reds",
                              "Oranges", "Greens",
                              "Blues","Greys"),
                  selected = "Purples"),
    ),
    mainPanel(plotOutput("map"))
  )
)



# Server logic ----
server <- function(input, output) {
    output$map <- renderPlot({
    zcol <- switch(input$var, 
                   "Adult Obesity Rate" = join$pct_obese,
                   "Percentage of Adult Smokers" = join$pct_smokers,
                   "Physical Inactivity (Adults)" = join$pct_inactive,
                   "Excessive or Binge Drinking" = join$pct_drinkers,
                   "Food Insecure Population" = join$pct_foodinsec,
                   "Households with Highspeed Internet" = join$pct_broadband)
    color <- switch(input$color, 
                    "Greens"  = RColorBrewer::brewer.pal(9, "Greens"),
                    "Blues"  = RColorBrewer::brewer.pal(9, "Blues"),
                    "Purples"  = RColorBrewer::brewer.pal(9, "Purples"),
                    "Reds"  = RColorBrewer::brewer.pal(9, "Reds"),
                    "Oranges"  = RColorBrewer::brewer.pal(9, "Oranges"),
                    "Greys"  = RColorBrewer::brewer.pal(9, "Greys"))
    layer.name <- switch(input$var, 
                     "Adult Obesity Rate"  = "% Obese",
                     "Percentage of Adult Smokers" = "% Adult Smokers",
                     "Physical Inactivity (Adults)" = "% Inactive",
                     "Excessive or Binge Drinking" = "% Drinkers",
                     "Food Insecure Population" = "% Food Insecure",
                     "Households with Highspeed Internet" = "% Connected")


# Make a Map  
    percent_map(zcol, color, layer.name, max = max(zcol), min = min(zcol))
    })
}

# Run app ----
shinyApp(ui, server)
























 