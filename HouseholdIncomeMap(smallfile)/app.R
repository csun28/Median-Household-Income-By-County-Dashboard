library(shiny)
library(RColorBrewer)
library(tidycensus)
library(censusapi)
library(tidyverse)
library(tmap)
library(tmaptools)
library(leaflet)
library(stringr)

census_api_key("fe3afe59181e64009b1eae3990201d7c0a3ec5aa")

#create a list of all state names 
StateList = c("Alabama (AL)",  "Alaska (AK)", "Arizona (AZ)", "Arkansas (AR)",  "California (CA)", "Colorado (CO)", 
              "Connecticut (CT)", "Delaware (DE)", "District of Columbia (DC)",  "Florida (FL)", "Georgia (GA)", "Hawaii (HI)", 
              "Idaho (ID)", "Illinois (IL)", "Indiana (IN)","Iowa (IA)", "Kansas (KS)", "Kentucky (KY)", "Louisiana LA", 
              "Maine (ME)", "Massachusetts (MA)", "Maryland (MD)", "Michigan (MI)", "Minnesota (MN)", "Mississippi (MS)", 
              "Missouri (MO)",  "Montana (MT)", "Nebraska (NE)", "Nevada (NV)", "New Hampshire (NH)", "New Jersey (NJ)", 
              "New Mexico (NM)", "New York (NY)", "North Carolina (NC)", "North Dakota (ND)", "Oklahoma (OK)", "Ohio (OH)", 
              "Oregon (OR)", "Pennsylvania (PA)", "Puerto Rico (PR)", "Rhode Island (RI)", "South Carolina (SC)",  "South Dakota (SD)", "Tennessee (TN)", 
              "Texas (TX)", "Utah (UT)", "Vermont (VT)",  "Virginia (VA)",  "Washington (WA)", "West Virginia (WV)",  
              "Wisconsin (WI)", "Wyoming (WY)")
    
    
# define UI for application that draws a histogram
ui <- fluidPage(
    
    #define title of dashboard 
    titlePanel("State Map Showing Median Household Income by County"),
    
    
    #define sidebar panel UI (2 selection inputs and a text output)
    sidebarPanel(selectizeInput(inputId = "state",
                                 label = "Select a State",
                                 choices = StateList,
                                 selected = NULL,
                                 multiple = FALSE),
                 
                 selectizeInput(inputId = "year",
                                label = "Select a Year",
                                choices = c("2014-2018", "2009-2013", "2019", "2018", "2017", "2016", "2015", 
                                            "2014"), 
                                selected = "2015-2018",
                                multiple = FALSE),
                 
                 textOutput("acsinfo")),
    
    
    #define map for main panel             
    mainPanel(leafletOutput("my_tmap"))
)



#define server logic required to draw a histogram
server <- function(input, output) {
    
    #specify text output 
    output$acsinfo <- renderText({
        "Data was retrived from the US Census Bureau's American Community Survey. Annual American Community Surveys only 
        include counties with a population of 65000 or more."
    })
    
    
    #specify map output
    output$my_tmap <- renderLeaflet({
        
        #use census API from censusapi library to retrive county shapefiles by state
        geodata <- get_acs(geography = "county", 
                           year = ifelse(input$year=="2019", 2018, as.numeric(gsub(".*-","", input$year))),
                           table = "B19013", 
                           survey = "acs5",
                           state = gsub("[\\(\\)]", "", regmatches(input$state, gregexpr("\\(.*?\\)", input$state))[[1]]),
                           geometry = TRUE)
        
        #use census API from tidycensus library to retrieve median household income data by county
        incomedata <- if(input$year=="2014-2018") {
                            getCensus(name="acs/acs5", vintage=2018, 
                                      key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                                      vars=c("NAME", "B19013_001E"),
                                      region = "county:*")}
        else if (input$year=="2009-2013") {
                    getCensus(name="acs/acs5", vintage=2013, 
                              key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                              vars=c("NAME", "B19013_001E"),
                              region = "county:*")}
        else {getCensus(name="acs/acs1", vintage=input$year, 
                         key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                         vars=c("NAME", "B19013_001E"),
                         region = "county:*")}
        
        #merge datasets and add columns 
        mapdata <- full_join(geodata, incomedata, by="NAME") 
        mapdata$CountyName <- gsub("(.*),.*", "\\1", mapdata$NAME)
        
        
        #create map
        tm <- tm_shape(mapdata) +
              tm_polygons(col="B19013_001E",
                          palette = "YlGn",
                          legend.show = TRUE,
                          title = "Median Household Income",
                          id= "CountyName",
                          popup.vars=c("County Code"="county", "Income"="B19013_001E"))
        tmap_mode("view")
        #convert map to leadlet to view as html
        tmap_leaflet(tm,
                     mode = "view",
                     show = FALSE,
                     add.titles = FALSE,
                     in.shiny = TRUE)
        })}

        

# Run the application 
shinyApp(ui = ui, server = server)
