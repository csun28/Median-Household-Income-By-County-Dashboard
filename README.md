# Medican Household Income by County 

### Overview 
This repository creates an interactive R Shiny applications that shows median household income by country. The default visual is a choropleth map of the continetal US, but users can choose to view a any of the 50 states or Puerto Rico through a drop down selection menu. Users can also select the year (ranging from 2014 to 2022) in which the data is pertaining to through a separate drop down selection menu. When hovering over the county will provide users with the county name, and clicking on the county will reveal the county name, county code, and median household income. 

### Data Source
Median household income data was taken from the US Census Bureau's American Community Survey (ACS). An API, along with the packages `tidycensus` and `censusapi` were used to pull data from ACS. Users can select to view data from the 5 year ACS or the 1 year ACS. Users are given the option to view either the 2009-2013 5 year ACS data or the 2014-2018 5 year ACS data. For the 1 year ACS data, data from 2014 through to 2022 are avaliable for visulization.

Additionally, county shapefiles are pulled from the UC Census, and correspond to the year of the selected median household income data. State shapefiles are pulled once using data from 2018.

### Application Deployment 
The application can be runned locally using the `RShiny` package or be published onto a web-based platform. Please note that this application cannot be published on the free version of shinyapps.oi because the shapefiles are too big.
