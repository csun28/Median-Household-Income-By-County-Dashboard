#load packages
library(shiny)
library(RColorBrewer)
library(tidycensus)
library(censusapi)
library(tidyverse)
library(tmap)
library(tmaptools)
library(leaflet)
library(stringi)
library(stringr)

#set census API key
census_api_key("fe3afe59181e64009b1eae3990201d7c0a3ec5aa")

#use census API from censusapi library to retrieve state shapefiles (same regardless of year)
statedata <- get_acs(geography = "state", 
                     year = 2018,
                     table = "B19013", 
                     survey = "acs5",
                     geometry = TRUE)

#create a list of all state names available for selection
StateList = c("Alabama (AL)",  "Alaska (AK)", "Arizona (AZ)", "Arkansas (AR)",  "California (CA)", "Colorado (CO)", 
              "Connecticut (CT)", "Delaware (DE)", "District of Columbia (DC)",  "Florida (FL)", "Georgia (GA)", "Hawaii (HI)", 
              "Idaho (ID)", "Illinois (IL)", "Indiana (IN)","Iowa (IA)", "Kansas (KS)", "Kentucky (KY)", "Louisiana LA", 
              "Maine (ME)", "Massachusetts (MA)", "Maryland (MD)", "Michigan (MI)", "Minnesota (MN)", "Mississippi (MS)", 
              "Missouri (MO)",  "Montana (MT)", "Nebraska (NE)", "Nevada (NV)", "New Hampshire (NH)", "New Jersey (NJ)", 
              "New Mexico (NM)", "New York (NY)", "North Carolina (NC)", "North Dakota (ND)", "Oklahoma (OK)", "Ohio (OH)", 
              "Oregon (OR)", "Pennsylvania (PA)", "Puerto Rico (PR)", "Rhode Island (RI)", "South Carolina (SC)",  "South Dakota (SD)", 
              "Tennessee (TN)", "Texas (TX)", "Utah (UT)", "Vermont (VT)",  "Virginia (VA)",  "Washington (WA)", "West Virginia (WV)",  
              "Wisconsin (WI)", "Wyoming (WY)", "United States (US)")


# define UI for application 
ui <- fluidPage(
    
    #define title of dashboard 
    titlePanel("Map Showing Median Household Income by County"),
    
    #define sidebar panel UI (2 selection inputs and a text output)
    sidebarPanel(selectizeInput(inputId = "state",
                                label = "Select a State",
                                choices = StateList,
                                selected = "United States (US)",
                                multiple = FALSE),
                 
                 selectizeInput(inputId = "year",
                                label = "Select a Range of Years or Year",
                                choices = c("2014-2018", "2009-2013", "2022", "2021", "2019", "2018", "2017", 
                                            "2016", "2015", "2014"), 
                                selected = "2015-2018",
                                multiple = FALSE),
                 
                 htmlOutput("acsinfo")),
    
    
    #define map for main panel UI             
    mainPanel(leafletOutput("my_tmap"))
)


#define server logic 
server <- function(input, output) {
    
    #specify text output 
    output$acsinfo <- renderUI({
      str1 <- "Data was retrived from the US Census Bureau's American Community Survey (ACS). Annual ACS data only 
          includes counties with a population of 65000 or more."
      str2 <- "*ACS data was not released for the year 2020 due to challenges from the COVID-19 pandemic."
      HTML(paste(str1, str2, sep = '</p>'))
    })
    
    #use census API from tidycensus library to retrieve median household income data by inputted year
    incomedata <- reactive({if(input$year=="2014-2018") {
        getCensus(name="acs/acs5", vintage=2018, 
                  key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                  vars=c("NAME", "B19013_001E"),
                  region = "county:*")}
        else if (input$year=="2009-2013") {
            getCensus(name="acs/acs5", vintage=2013, 
                      key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                      vars=c("NAME", "B19013_001E"),
                      region = "county:*")}
        else {
            getCensus(name="acs/acs1", vintage=input$year, 
                      key="fe3afe59181e64009b1eae3990201d7c0a3ec5aa", 
                      vars=c("NAME", "B19013_001E"),
                      region = "county:*")}})
    
    #use census API from censusapi library to retrieve county shapefiles by inputted year
    geodata <- reactive({get_acs(geography = "county", 
                                 year = if (input$year=="2009-2013") {2013}
                                        else if (input$year=="2014-2018") {2018}
                                        else as.numeric(gsub(".*-","", input$year)),
                                 table = "B19013", 
                                 survey = "acs5",
                                 geometry = TRUE)})
    
    
    #specify map output
    output$my_tmap <- renderLeaflet({
        
        #merge datasets and create new columns which separate out county name and state name  
        fulldata <- full_join(geodata(), incomedata(), by="NAME") 
        fulldata$CountyName <- gsub("(.*),.*", "\\1", fulldata$NAME)
        fulldata$StateName <- gsub(".*, ", "" , fulldata$NAME)
        
        #use all data in full data set to create map of United States 
        mapdata <- if (input$state=="United States (US)") {
            fulldata}
        #subset data by selected state to create map of selected state
        else {fulldata %>% filter(StateName==str_replace(input$state, " \\(.*\\)", ""))}
        
        
        #create map
        tm <- tm_shape(mapdata) +
            #only set zoom if map is of United States and not of states
            {if (input$state=="United States (US)") tm_view(set.view = c(-90, 40, 3.5))} + 
            tm_polygons(col="B19013_001E",
                        palette = "YlGn",
                        legend.show = TRUE,
                        title = "Median Household Income",
                        id= "CountyName",
                        popup.vars=c("County Code"="county", "Income"="B19013_001E")) +
            tm_borders(col="#636363",
                       lwd=0.2) +
            #add darker boundaries for state lines 
            tm_shape(statedata) +
            tm_borders(col="black",
                       lwd=0.5) 
        
        #convert map to leaflet to view as html
        tmap_mode("view")
        tmap_leaflet(tm,
                     mode = "view",
                     show = FALSE,
                     add.titles = FALSE,
                     in.shiny = TRUE)
    }) 
    
}


# Run the application 
shinyApp(ui = ui, server = server)
