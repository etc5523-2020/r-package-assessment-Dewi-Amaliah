# loading the package

# if you have not had tidycovid19, install it first using this code:
# devtools::install_github("joachim-gassen/tidycovid19")

library(tidyverse)
library(tidycovid19)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
#library(readxl)
library(lubridate)
#library(ggthemes)
#library(rworldmap)
#library(grid)
library(maps)
library(viridis)
library(viridisLite)
library(mapproj)
library(DT)
library(c19euxplorer)


##### DATA #######

### PREPARE THE DATA ###

# Load daily data from ecdc website
covid_data_daily <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                             na.strings = "", fileEncoding = "UTF-8-BOM")

# Load cumulative data from tidycovid19 package
covid_data_cumulative <- download_merged_data(cached = TRUE, silent = TRUE) #download the data from tidycovid19 package

# Load weekly testing data from ecdc web 
#test_data <- read_excel(here::here("data-raw/weekly_testing_2020_10_01.xlsx"))
test_data <- c19euxplorer::testing_weekly_eu

# Load weekly transmission data from ecdc COVID dashboard
#transmission_data <- read_csv(here::here("data-raw/subnational level of transmission-2.csv"))
transmission_data <- c19euxplorer::transmission_subnational_level

# Load world map data
world_map <- map_data("world")


# Load word map centroid 

#world_map_centroid <- read_csv(here::here("data-raw/country_centroids.csv")) 
world_map_centroid <- c19euxplorer::country_centroids

# Load country measure response

country_response <- read_csv("https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_2020-10-02.csv")


### WRANGLE COVID DAILY DATA ###

covid_eu_daily <- covid_data_daily %>%
  filter(continentExp == "Europe") %>%
  rename(date = dateRep,
         country = countriesAndTerritories,
         population = popData2019,
         daily_cases = cases,
         daily_deaths = deaths) %>%
  mutate(date = ymd(dmy(date))) %>%
  arrange(date) %>%
  # calculate rate per 10,000 people
  mutate(daily_cases_rate = daily_cases/population*10000,
         daily_deaths_rate = daily_deaths/population*10000) %>%
  mutate(country = gsub("_", " ", country))


### WRANGLE COVID CUMULATIVE DATA ###

# wrangle cases and deaths data


# first step is to extract the country name from transmission_data

countries_trans <- as_tibble(unique(transmission_data$Country)) %>%
  rename(country = value) 

countries_test <- as_tibble(unique(test_data$country)) %>%
  rename(country = value)

# population data is different between the one in tidycovid19 and the one in ecdc website.
# it is because the difference of year
# in this case I used the population data based on ecdc who get the data from the World Bank 
# based on population data in 2019

pop2019 <- covid_eu_daily %>%
  group_by(country) %>%
  count(pop = mean(population)) %>%
  dplyr::ungroup() %>%
  select(c(1:2)) %>%
  filter(country != "Bosnia and Herzegovina",
         country != "Kosovo",
         country != "Holy See")


covid_eu_wrangled <- left_join(pop2019, covid_data_cumulative, by = "country") %>%
  # select variables that we want to use
  select(country, 
         date, 
         ecdc_cases, 
         ecdc_deaths,
         pop, 
         gcmr_retail_recreation, 
         gcmr_grocery_pharmacy,
         gcmr_parks,
         gcmr_transit_stations,
         gcmr_workplaces,
         gcmr_residential) %>%
  filter(!is.na(ecdc_cases),
         !is.na(ecdc_deaths)) %>%
  rename(population = pop)

  

covid_eu_cumulative <- covid_eu_wrangled %>%
  rename(cumulative_cases = ecdc_cases,
         cumulative_deaths = ecdc_deaths) %>%
  mutate(cumulative_cases_rate = cumulative_cases/population*10000,
         cumulative_deaths_rate = cumulative_deaths/population*10000)
  
  
# WRANGLE TESTING DATA

test_data_clean <- test_data %>%
  mutate(week_only = as.numeric(sub(".*W", "", year_week))) %>%
  #get the date of the week
  mutate(date = as.Date(paste(2020, week_only, 1, sep="-"), "%Y-%U-%u")) %>%
  mutate(week = week(date),
         testing_rate_percent = round(tests_done/population*100,2),
         positivity_rate = round(positivity_rate, 2))


# WRANGLE TRACING DATA

transmission_wrangled <- transmission_data %>%
  group_by(Country, Week) %>%
  count(`Level of transmission`) %>%
  mutate(week_only = as.numeric(sub(".*W", "", Week))) %>%
  mutate(date = as.Date(paste(2020, week_only, 1, sep="-"), "%Y-%U-%u"),
    week = week(date)) %>%
  rename(Transmission = `Level of transmission`)



# WRANGLE EU MAP DATA 

# Get only the country in transmission data
eu_map <- left_join(countries_trans, world_map, by = c("country" = "region")) 

# Get the centroid of the country in transmission data

#rename the country name, so that it has the same country with countries_trans
world_map_centroid$name_long[world_map_centroid$name_long == "Czech Republic"] = "Czechia" 

centroid_wrangled <- left_join(countries_trans, world_map_centroid,
                               by = c("country" = "name_long")) %>%
  select(c("country",
           "Longitude",
           "Latitude"))

# get the lates rate of cumulative cases for maximum week displayed
latest_cumulative_case <- filter(covid_eu_cumulative,
                                 date == max(transmission_wrangled$date)) %>%
  select(c(country,
           date,
           cumulative_cases_rate))

# get the long and lat for the map
cum_cases_rate_for_map<- left_join(centroid_wrangled, latest_cumulative_case, by = "country")

cum_cases_rate_map <- cum_cases_rate_for_map %>%
  mutate(mytext = paste(
    "Country: ", cum_cases_rate_for_map$country, "\n", 
    "Cases: ", round(cum_cases_rate_for_map$cumulative_cases_rate, 3), sep="")
  )




# WRANGLE MOBILITY INDEX

mobility <- covid_eu_cumulative %>%
  select(date, country,
         gcmr_grocery_pharmacy,
         gcmr_retail_recreation,
         gcmr_parks,
         gcmr_residential,
         gcmr_transit_stations,
         gcmr_workplaces) %>%
  rename(`Grocery and Pharmacy` = gcmr_grocery_pharmacy,
         `Retail and Recreation` = gcmr_retail_recreation,
         `Parks` = gcmr_parks,
         `Residential` = gcmr_residential,
         `Transit Station` = gcmr_transit_stations,
         `Workplaces` = gcmr_workplaces) %>%
  pivot_longer(c(3:8), names_to = "Location", values_to = "index") 


# WRANGLE COUNTRY RESPONSE DATA

response_wrangled <- country_response %>%
  mutate(response = case_when(
    str_detect(Response_measure, "ClosDaycare") ~ "Day care or nursery closure",
    str_detect(Response_measure, "ClosHigh") ~ "Higher education closure",
    str_detect(Response_measure, "ClosPrim") ~ "Primary school closure",
    str_detect(Response_measure, "ClosPubAny") ~ "Public spaces of any kind closure",
    str_detect(Response_measure, "ClosSec") ~ "Secondary school closure",
    str_detect(Response_measure, "ClosSecHigh") ~ "High secondary school closure",
    str_detect(Response_measure, "MasksMandatory") ~ "Law enforced use of mask",
    str_detect(Response_measure, "MasksVoluntary") ~ "Voluntary use of mask",
    str_detect(Response_measure, "MassGather50") ~ "Mass gatherings limited to 50 people",
    str_detect(Response_measure, "MassGatherAll") ~ "Mass gatherings limited to 1000 people or less",
    str_detect(Response_measure, "StayHomeGen") ~ "Voluntary stay at home for general population",
    str_detect(Response_measure, "StayHomeOrder") ~ "Lockdown",
    str_detect(Response_measure, "TeleworkingClosures") ~ "Workplace closure",
    str_detect(Response_measure, "StayHomeRiskG") ~ "Stay at home recommendations for risk groups"
  )) %>%
  rename(measure = Response_measure,
         meaning = response) %>%
  mutate(Country = gsub("_", " ", Country)) %>%
  #these countries don't have information of mobility index
  filter(Country != "Iceland",
         Country != "Cyprus")

col_order <- c("Country",
               "measure",
               "meaning",
               "date_start",
               "date_end")

response_wrangled <- response_wrangled[, col_order]



##### FOR DATA TABLE #####

daily_cumulative_table <- left_join(pop2019, covid_data_cumulative, by = "country") %>%
  # select variables that we want to use
  select(country, 
         date, 
         ecdc_cases, 
         ecdc_deaths,
         pop, 
         gcmr_retail_recreation, 
         gcmr_grocery_pharmacy,
         gcmr_parks,
         gcmr_transit_stations,
         gcmr_workplaces,
         gcmr_residential) %>%
  rename(population = pop) %>%
  group_by(country) %>%
  filter(!is.na(ecdc_cases),
         !is.na(ecdc_deaths)) %>%
  mutate(lag_cases = lag(ecdc_cases, 1),
         lag_deaths = lag(ecdc_deaths, 1)) %>%
  mutate(daily_cases = ecdc_cases - lag_cases,
         daily_deaths = ecdc_deaths - lag_deaths) %>%
  dplyr::ungroup()
  
daily_cumulative_df <- daily_cumulative_table %>%
  mutate(daily_cases = ifelse(is.na(daily_cumulative_table$daily_cases),
                              daily_cumulative_table$ecdc_cases, 
                              daily_cumulative_table$daily_cases),
         daily_deaths = ifelse(is.na(daily_cumulative_table$daily_deaths), 
                               daily_cumulative_table$ecdc_deaths, 
                               daily_cumulative_table$daily_deaths)) %>%
  # calculate rate per 10,000 people
  mutate(daily_cases_rate = round(daily_cases/population*10000, 2),
         daily_deaths_rate = round(daily_deaths/population*10000, 2),
         cumulative_cases_rate = round(ecdc_cases/population*10000, 2),
         cumulative_deaths_rate = round(ecdc_deaths/population*10000, 2)) %>%
  select(-lag_cases,
         -lag_deaths)


# reorder column

col_order_cases_deaths <- c(
  "country",
  "date",
  "population",
  "daily_cases",
  "daily_cases_rate",
  "daily_deaths",
  "daily_deaths_rate",
  "ecdc_cases",
  "cumulative_cases_rate",
  "ecdc_deaths",
  "cumulative_deaths_rate",
  "gcmr_retail_recreation", 
  "gcmr_grocery_pharmacy",
  "gcmr_parks",
  "gcmr_transit_stations",
  "gcmr_workplaces",
  "gcmr_residential"
)

dt_cases_deaths <- daily_cumulative_df[, col_order_cases_deaths]


###Testing data for DT

testing_for_dt <- test_data_clean %>%
  select(country,
         country_code,
         date,
         week,
         new_cases,
         tests_done,
         population,
         testing_rate_percent,
         positivity_rate,
         testing_data_source)


###Tracing for DT

tracing_for_dt<- transmission_data %>%
  mutate(week_only = as.numeric(sub(".*W", "", Week))) %>%
  mutate(date = as.Date(paste(2020, week_only, 1, sep="-"), "%Y-%U-%u"),
         week = week(date)) %>%
  select(Country,
         Region,
         date,
         week,
         `Level of transmission`) %>%
  rename(Date = date,
         Week = week,
         Transmission = `Level of transmission`)
  



##### SHINY APP ##### -----------------------------------------------------------------------------------

##### SHINY USER INTERFACE ##### ----------------------------------------------------------------------------

top_left <- "https://images.unsplash.com/photo-1586448910895-c87c079c026a?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1050&q=80"
top_right <- "https://images.unsplash.com/photo-1590611936760-eeb9bc598548?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1050&q=80"
bottom_left <- "https://images.unsplash.com/photo-1586293403445-ffa224197984?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=750&q=80"

bottom_right <- "https://images.unsplash.com/photo-1586864387564-ea6bc7ceb97c?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1050&q=80"



ui <- bootstrapPage(
  tags$head(includeCSS("www/style.css")),
  navbarPage(theme = shinytheme("cerulean"), 
             collapsible = TRUE,
             "COVID-19 in Europe", id="nav",
             
             #setBackgroundColor(
               #color = "#EAF2F8",
               #gradient = c("linear", "radial"),
               #direction = c("bottom", "top", "right", "left"),
               #shinydashboard = FALSE),
             
##-------------------------------------------HOME TAB-------------------------------------------------------             
             tabPanel("Home", icon = icon("home"),
                      tagList(
                        # head
                        tags$head(
                          tags$link(href="www/styles.css"),
                          tags$style('.well {background-color: #EAF2F8;}')
                        ),
                        
                        tags$div(class="landing-wrapper",
                                 
                                 # child element 1: images
                                 tags$div(class="landing-block background-content",
                                          
                                          # images - top -> bottom, left -> right
                                          tags$img(src=top_left),
                                          tags$img(src=top_right),
                                          tags$img(src=bottom_left), 
                                          tags$img(src=bottom_right)
                                 ),
                                 
                                 # child element 2: content
                                 tags$div(class="landing-block foreground-content",
                                          tags$div(class="foreground-text",
                                                   tags$h1("Welcome to COVID-19 in Europe Data Explorer!"),
                                                   tags$p("This is a shiny app to explore the data of COVID-19 related variables in various countries in Europe."),
                                          
                                                   tags$p("Let's go around by clicking each tab above!")
                                          )
                                 )))),

##-----------------------------------------CASES TAB-------------------------------------------------------

             tabPanel("Cases and Deaths", icon = icon("bar-chart-o"),
                      sidebarLayout(
                        sidebarPanel(
                          helpText("The aim of this tab is to create a plot of cases and deaths of COVID-19 in daily and cumulative figure.",
                                   "Follow these steps to create the plot",
                                   "Note: For comparison across countries, the rate per 10,000 people is preferred."),
                          c19euxplorer::ui_selectize("country_select", pop2019$country),
                          radioButtons("indicator_select", "Step 2: Select an indicator to display",
                                       choices = c("Cases", "Cases per 10,000 people",
                                                 "Deaths", "Deaths per 10,000 people"),
                                       selected = "Cases per 10,000 people"),
                         dateRangeInput("date_input",
                                      "Step 3: Choose range of date",
                                      start = min(covid_eu_daily$date),
                                      end = "2020-10-01",
                                      min = min(covid_eu_daily$date),
                                      max = max(covid_eu_cumulative$date)
                                      )),
                        mainPanel(h5(textOutput("plot_title")),
                                  tabsetPanel(
                                    tabPanel("Daily", plotlyOutput("covid_daily_plot"),
                                             p("Data source:", 
                                               tags$a(href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                                      "European Centre for Disease Control and Prevention (ECDC)",
                                                      class = "externallink"),
                                               style = "text-align: right;")),
                                    
                                    tabPanel("Cumulative", plotlyOutput("covid_cumulative_plot"),
                                             p("Data source: ECDC downloaded with ", 
                                               tags$a(href = "https://github.com/joachim-gassen/tidycovid19",
                                                      "tidycovid19 R package",
                                                      class = "externallink"),
                                               style = "text-align: right;")
                                             )),
                                  
                                  p(icon("info-circle", lib = "font-awesome"), em("Hover to the line to see the detailed figure")),
                                  h5(p(icon("book-reader", lib = "font-awesome"),"What does this plot convey?")),
                                  p("Plotting COVID-19 cases and death day by day is vital to track the pandemic trajectory.", 
                                    "Daily figures allow us to track the change of situation day by day.", 
                                    "Simultaneously, the cumulative statistics will enable us to see the number in total, i.e.
                                    essential to see whether the cases or deaths flatten.", 
                                    "Moreover, we can also compare the pandemic situation in many countries in Europe by using the rate per 10,000 people.", 
                                    "This indicator is apple to apple since we have omitted the effect of the population.",
                                    br(),
                                    br(),
                                    icon("info", lib = "font-awesome"),
                                    "Note that the cumulative figure can never be decreasing. 
                                    However, there is a decreasing number in some countries, such as Spain, France, and Luxembourg, 
                                    because of methodology change.",
                                    #br(),
                                    "We should also note that reported cases vary depend on country's testing performance.",
                                    style="text-align:justify;
                                          background-color:#FBEEE6;
                                          padding:15px;
                                          border-radius:10px")
                                  ))),
             
## ------------------------------------------TESTING TAB-----------------------------------------------------


             tabPanel("Testing", icon = icon("vials"),
                      sidebarLayout(
                        sidebarPanel(
                          helpText(paste("The aim of this tab is to create a plot of testing and positivity rate of COVID-19 for", 
                                         nrow(countries_test), "countries in Europe on weekly basis. 
                                         Follow these steps to create the plot.")),
                          c19euxplorer::ui_selectize("input_country_test", countries_test$country),
                          sliderInput("input_date_test",
                                      "Step 2: Slide to select week range",
                                      min = min(test_data_clean$week),
                                      max = max(test_data_clean$week),
                                      value = c(min(test_data_clean$week),
                                                max(test_data_clean$week))),
                          helpText(p(icon("book", lib = "font-awesome"),
                                     "Definition",
                                     br(),
                                     "The testing rate is the percentage of testing done divided by the population.",
                                     br(),
                                     "The positivity rate is the percentage of new confirmed cases divided by testing done."))
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Testing Rate",
                                   h5("COVID-19 Weekly Testing Rate"),
                                   plotlyOutput("covid_test_rate_plot"),
                                   
                                   p("Data source:", 
                                     tags$a(href = "https://www.ecdc.europa.eu/en/publications-data/covid-19-testing",
                                            "European Centre for Disease Control and Prevention",
                                            class = "externallink"),
                                     style = "text-align: right;"),
                                   
                                   p(
                                           icon("info-circle", lib = "font-awesome"),
                                           em("Dashed line is the threshold of recommended weekly testing rate"),
                                           tags$a(href = "https://www.kff.org/policy-watch/what-testing-capacity-do-we-need/",
                                                  em("(Gottlieb in Kates et al., 2020)."),
                                                  class = "externallink")
                                           ),
                                   
                                   p(icon("info-circle", lib = "font-awesome"), em("Hover to the line to see detailed figure")),
                                   h5(p(icon("book-reader", lib = "font-awesome"),"What does the testing rate imply?")),
                                   p("A mass COVID-19 testing is a key to quickly identify the infected people, trace, and quarantine their contacts.",
                                     "It is also important to track the spread of a virus in community.",
                                     style="text-align:justify;
                                          background-color:#FBEEE6;
                                          padding:15px;
                                          border-radius:10px")
                                   ),
                          tabPanel("Positivity Rate",
                                   h5("COVID-19 Weekly Positivity Rate"),
                                   plotlyOutput("covid_positivity_rate_plot"),
                                   
                                   p("Data source:", 
                                     tags$a(href = "https://www.ecdc.europa.eu/en/publications-data/covid-19-testing",
                                            "European Centre for Disease Control and Prevention",
                                            class = "externallink"),
                                     style = "text-align: right;"),
                                   
                                   
                                   p(icon("info-circle", lib = "font-awesome"), 
                                     em("Dashed line is a threshold that epidemic is under control"),
                                     tags$a(href = "https://ourworldindata.org/coronavirus-testing",
                                            em("(WHO in Our World in Data, 2020)."),
                                            class = "externallink")
                                     ),
                                   
                                   p(icon("info-circle", lib = "font-awesome"), em("Hover to the line to see detailed figure")),
                                   h5(p(icon("book-reader", lib = "font-awesome"),"What does the positivity rate imply?")),
                                   p(
                                     "A positivity rate less than 5 percent is an indicator that epidemic is under control.",
                                     "According to",
                                     tags$a(href = "https://www.npr.org/sections/coronavirus-live-updates/2020/03/30/824127807/if-most-of-your-coronavirus-tests-come-back-positive-youre-not-testing-enough",
                                            "WHO in Huang (2020), ",
                                            class = "externallink"),
                                     "it is also important to know the testing performance of a country.",
                                     "High positivity rates could mean that a country has not been done enough testing.",
                                     style="text-align:justify;
                                          background-color:#FBEEE6;
                                          padding:15px;
                                          border-radius:10px")
                                   
                                   )
                          
                        ))
                      )),

#-------------------------------------------TRACING TAB------------------------------------------------------

             tabPanel("Tracing", icon = icon("route"),
                      sidebarLayout(
                        sidebarPanel(
                          helpText(paste("This tab aims to visualize the transmission of COVID-19 in",
                                         nrow(countries_trans),
                                         "countries in Europe by week."),
                                   "You can choose a country to be plotted by the following alternatives."),
                          selectInput("input_country_trans", 
                                         "Choose a country using this dropdown.",
                                         choices = countries_trans$country,
                                         selected = "France",
                                         multiple = FALSE),
                          p(strong("Or choose it by clicking the buble in this map.")),
                          h5(icon("map", lib = "font-awesome"),
                             "Europe Map by COVID-19 Cumulative Cases per 10,000 Population"),
                          plotlyOutput("map"),
                          br(),
                          p(icon("info-circle", lib = "font-awesome"), 
                            em("Buble size reflects the latest rate of cumulative cases per 10,000 people.",
                               "The bigger the size, the higher the rate."))
                        ),
                        mainPanel(
                          p("In pandemic, tracing is the process of finding out the people who might be exposed by the virus and having a chance of being infected.",
                            "Contact tracing then would allow the authorities to classify the source of transmission in certain area.",
                            "Finally, if the source of transmission is known, the measure and strategy to control the spread could be adjusted.",
                            style="text-align:justify;
                                          background-color:#EAF2F8;
                                          padding:15px;
                                          border-radius:10px"), 
                          h5(textOutput("trans_plot_title")),
                          plotlyOutput("covid_transmission_plot"),
                          
                          p("Data source:", 
                            tags$a(href = "https://qap.ecdc.europa.eu/public/extensions/COVID-19/COVID-19.html#subnational-transmission-tab",
                                   "European Centre for Disease Control and Prevention",
                                   class = "externallink"),
                            style = "text-align: right;"),
                          
                          p(icon("info-circle", lib = "font-awesome"), em("Hover to the plot to see the detailed figure")),
                          h5(p(icon("book-reader", lib = "font-awesome"),"How to read this plot?")),
                          p("Transmission data from ECDC is published at a subnational level.",
                            "The source of transmission is classified using the WHO definition.",
                            "In the absence of information provided by the countries, the category is marked as 'not reported'.",
                            "Hence, the plot conveys the dominant source of transmission by the country's subnational level, 
                            for example, in the provinces in France.",
                            "In this case, the plot can be interpreted, for instance, in week 34 2020, 
                            most of the confirmed cases in about 75 percent of provinces in France came from the cluster transmission, 
                            while the rest were from community transmission.",
                            style="text-align:justify;
                                          background-color:#FBEEE6;
                                          padding:15px;
                                          border-radius:10px")
                          )
                      )),

#--------------------------------------------COMMUNITY MOBILITY---------------------------------------------
             tabPanel("Community Mobility", icon = icon("car-side"),
                      fluidRow(
                        column(width = 5,
                               style = "background-color:#EAF2F8;
                                          padding:15px;
                                          border-radius:10px",
                               p("This tab aims to visualise the community mobility to 6 types of places during the pandemic.",
                                 "The mobility measurement using the data from Google Community Mobility Index.",
                                 "Here are the steps to get the plot."
                                 ),
                               selectInput("country_response",
                                           "Step 1: Select a country",
                                           choices = unique(response_wrangled$Country),
                                           selected = "France",
                                           multiple = FALSE),
                               strong("Step 2: Select date range"),
                               br(),
                               br(),
                               em(icon("info-circle", lib = "font-awesome"),
                                  "Do you want to choose a date according to government's response?",
                                  br(),
                                  "Don't worry, I've got your back!",
                                  style = "color = blue"),
                               br(),
                               br(),
                               h5(icon("table", lib = "font-awesome"),
                                  "Country's Response Measure to COVID-19",
                                  align = "center"),
                               pickerInput("measure_input",
                                            "Select at least one measure",
                                            choices = unique(response_wrangled$meaning),
                                            options = list(`actions-box` = TRUE,
                                                           `none-selected-text` = "Please make a selection!"),
                                            selected = "Higher education closure",
                                            multiple = TRUE),
                               dataTableOutput("response_table"),
                               br(),
                               br(),
                               sliderInput("date_response",
                                           "Finally, slide here to choose date range",
                                           min = as.Date(min(covid_eu_cumulative$date),"%Y-%m-%d"),
                                           max = as.Date(max(covid_eu_cumulative$date), "%Y-%m-%d"),
                                           value = c(as.Date(min(covid_eu_cumulative$date)), 
                                                     as.Date(max(covid_eu_daily$date))),
                                           timeFormat="%d %b"
                                           )
                               
                        ),
                        column(width = 7,
                               
                               p("Google Community Mobility Index is the index that measures the visitor number of six type of locations every day and compares the change relative to baseline days before the pandemic ",
                                 tags$a(href = "https://ourworldindata.org/covid-mobility-trends",
                                        "(Our World in Data, 2020).",
                                        class = "externallink"),
                      
                                 "This index could give us an insight of people's mobility during the pandemic.",
                                 "It also could show the effect of various government response on the community ",
                                 tags$a(href = "https://ourworldindata.org/covid-mobility-trends",
                                        "(Our World in Data, 2020).",
                                        class = "externallink"),
                                 style="text-align:justify;
                                          background-color:#EAF2F8;
                                          padding:15px;
                                          border-radius:10px"
                                 ),
                               tabsetPanel(
                                 tabPanel("Plot",
                               h5(textOutput("selected_country_mob")),
                               plotlyOutput("com_mobility_plot"),
                               p("Data source: Google Community Mobility Report", 
                                 br(),
                                 "downloaded with",
                                 tags$a(href = "https://github.com/joachim-gassen/tidycovid19",
                                        "tidycovid19 R package",
                                        class = "externallink"),
                                 style = "text-align: right;"),
                               p(icon("info-circle", lib = "font-awesome"), em("Single click a location on the legend to hide it, double click it to hide the rest."))
                               ),
                               
                               tabPanel("Notes",
                               h5(icon("book-reader", lib = "font-awesome"),"Some notes to interpret the trends ",
                                  tags$a(href = "https://ourworldindata.org/covid-mobility-trends",
                                         "(Our World in Data, 2020).",
                                         class = "externallink")),
                               tags$li(class= "li-custom", "The 'Residential' category uses different measure, which is, duration, not visitors."),
                               tags$li(class= "li-custom", "Gaps in a specific time series could be due to the small data to meet the quality and anonimity standards."),
                               tags$li(class= "li-custom", "Parks visitors might be affected by season and holidays."),
                               tags$li(class= "li-custom", "Weekends and weekdays are not comparable."),
                               tags$li(class= "li-custom", "This index could not be compared by country, hence in this tab, you can only choose a country.")
                               )
                        )
                        )
                      )
                      
                      ),


#--------------------------------------------DATA-----------------------------------------------------------
             tabPanel("Data", icon = icon("table"),
                      fluidRow(
                        column(
                          width = 12,
                          p("This tab provides the data sets used for previous tabs visualisation.",
                            br(),
                            "You can choose one or more countries and date range to be displayed.")
                        ),
                        column(
                          width = 12,
                          br(),
                          tabsetPanel(tabPanel("Cases, Deaths, and Mobility",
                                              
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   pickerInput("cases_deaths_country",
                                                               "Select one or more countries",
                                                               choices = pop2019$country,
                                                               selected = c("Austria", 
                                                                            "Spain",
                                                                            "Italy"),
                                                               options = list(`actions-box` = TRUE,
                                                                              `none-selected-text` = "Please make a selection!"),
                                                               multiple = TRUE),
                                                   dateRangeInput("cases_deaths_date",
                                                                  "Step 3: Select range of date",
                                                                  start = min(covid_eu_cumulative$date),
                                                                  end = "2020-10-01",
                                                                  min = min(covid_eu_cumulative$date),
                                                                  max = max(covid_eu_cumulative$date)
                                                   )
                                                   
                                                 ),
                                                 mainPanel(
                                                   h5("COVID-19 Cases, Deaths, and People Mobility in Europe"),
                                              
                                                   dataTableOutput("cases_and_deaths_table"),
                                                   em("GMI = Google Mobility Index")
                                                 )
                                               )),
                                      tabPanel("Testing and Transmission",
                                               br(),
                                               sidebarLayout(
                                                 sidebarPanel(radioButtons("table_select", 
                                                                           "Select a table to be displayed",
                                                                           choices = c("Testing",
                                                                                       "Transmission"),
                                                                           selected = "Testing"),
                                                              pickerInput("testing_trans_select",
                                                                          "Select one or more countries",
                                                                          choices = unique(tracing_for_dt$Country),
                                                                          selected = c("Austria",
                                                                                       "Italy",
                                                                                       "Spain"),
                                                                          multiple = TRUE,
                                                                          options = list(`actions-box` = TRUE,
                                                                                         `none-selected-text` = "Please make a selection!")
                                                                            
                                                                          ),
                              
                                                              pickerInput("testing_trans_select_week",
                                                                          "Select one or more week",
                                                                          choices = sort(unique(testing_for_dt$week)),
                                                                          selected = 30,
                                                                          multiple = TRUE,
                                                                          options = list(`actions-box` = TRUE,
                                                                                         `none-selected-text` = "Please make a selection!")
                                                                          
                                                                          )
                                                                
                                                              
                                                              ),
                                                 mainPanel(
                                                   h5(textOutput("table_title")),
                                                   br(),
                                                   dataTableOutput("testing_and_trans_table")
                                                 )
                                               ))
                                      
                                      )
                          
                        )
                      )),




#--------------------------------------------ABOUT---------------------------------------------------------
             navbarMenu("More", icon = icon("info-circle"),
                      tabPanel("About",
                               fluidRow(
                                 column(width = 3,
                                        
                                        h4("About This App",
                                           align = "center"),
                                        
                                        p(br(),
                                          tags$img(src = "https://www.flaticon.com/svg/static/icons/svg/2303/2303502.svg",
                                                   height = 175,
                                                   width = 175),
                                          style="text-align: center;")
                                        ),
                                 
                                 column(width = 8,
                                        style = "text-align: justify;",
                                        br(),
                                        p("Data is a vital component in the fight against COVID-19.",
                                          "It helps governments and health sector authorities to measure policy.",
                                          "It can even help us, as part of the community,",
                                          "understand the current situation, and how we can play a role, hand in hand, to prevent the wider transmission."
                                          ),
                                        br(),
                                        p("This app aims to be a tool to explore the data of COVID-19 related variables in various countries in Europe.",
                                          "This app covers:"),
                                        tags$li(class= "li-custom", "Daily and cumulative cases and deaths of COVID-19 in", nrow(pop2019), "countries in Europe."),
                                        tags$li(class= "li-custom", "Weekly testing rate, positivity rate, and source of transmission of the EU/EEA member countries and the UK."),
                                        tags$li(class= "li-custom", "Community mobility in six types of location."),
                                        br(),
                                        p("This app built with",
                                          icon("r-project",
                                               lib = "font-awesome"),
                                          ", mainly with", 
                                          tags$a(href = "https://shiny.rstudio.com",
                                                 "shiny",
                                                 class = "externallink"), 
                                          "with the help of several other packages: ",
                                          tags$a(href = "https://www.tidyverse.org",
                                                 "tidyverse,",
                                                 class = "externallink"),
                                          tags$a(href = "https://rstudio.github.io/shinythemes/",
                                                 "shinythemes,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=shinyWidgets",
                                                 "shinyWidgets,",
                                                 class = "externallink"),
                                          tags$a(href = "https://plotly-r.com",
                                                 "plotly,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=readxl",
                                                 "readxl,",
                                                 class = "externallink"),
                                          tags$a(href = "http://www.jstatsoft.org/v40/i03/",
                                                 "lubridate,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=maps",
                                                 "maps,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=viridis",
                                                 "viridis,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=viridisLite",
                                                 "viridisLite,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=mapproj",
                                                 "mapproj,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=DT",
                                                 "DT,",
                                                 class = "externallink"),
                                          tags$a(href = "https://CRAN.R-project.org/package=here",
                                                 "here,",
                                                 class = "externallink"),
                                          "and",
                                          tags$a(href = "https://ggplot2.tidyverse.org",
                                                 "ggplot2.",
                                                 class = "externallink")),
                                        
                                        p("This app's source of data is the ",
                                          tags$a(href = "https://www.ecdc.europa.eu/en",
                                                 "European Centre for Disease Control and Prevention",
                                                 class = "externallink"),
                                          "and",
                                          tags$a(href = "https://www.google.com/covid19/mobility/",
                                                 "Google Community Mobility Report (GCMR).",
                                                 class = "externallink"),
                                          "The cumulative figures and GCMR are downloaded with the",
                                          tags$a(href = "https://github.com/joachim-gassen/tidycovid19",
                                                 "tidycovid19",
                                                 class = "externallink"),
                                          "R package.",
                                          "Further, the daily data and the data which are downloaded with the package are automatically updated.",
                                          "However, the testing and transmission data should be downloaded and updated manually."),
                                        br(),
                                        p("Finally, I hope this app could give you some insights into the COVID-19 situation in Europe.")
                                        
                                   
                                 ),
                                 
                                 column(width = 1),
                                 
                                 column(width = 2,
                                        br(),
                                        br()),
                                 
                                 column(width = 10,
                                        br(),
                                        br()),
                                 
                                 column(width = 3,
                                        h4("About Me",
                                           align = "center"),
                                        p(br(),
                                          tags$img(src = "https://avatars1.githubusercontent.com/u/62932031?s=400&u=b797f843cc7724fc598a0498418e8316243d38db&v=4",
                                                 width = 175,
                                                 height = 175),
                                          style="text-align: center;")
                                        
                                        ),
                                 
                                 column(width = 8,
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        #br(),
                                        p("My name is Dewi Lestari Amaliah.",
                                          "Currently, I am a graduate student of", 
                                          tags$a(href = "https://www.monash.edu/study/courses/find-a-course/2021/business-analytics-b6022?gclid=EAIaIQobChMI5bKC_Y-g7AIVzn8rCh3aQABbEAAYASAAEgK09PD_BwE&international=true#overview-1",
                                                 "Master of Business Analytics",
                                                 class = "externallink"),
                                          "in Monash University.",
                                          "I believe that a data-driven insight can lead to a better decision.",
                                          br(),
                                          "I mainly use R in R studio in wrangling, visualizing, modeling, and analyzing the data.",
                                          "I also used GitHub for the reproducibility of my analysis."),
                                        #br(),
                                        tags$a(href = "https://twitter.com/dlamaliah",
                                               icon("twitter", lib = "font-awesome"),
                                               class = "externallink"),
                                        tags$a(href = "https://github.com/Dewi-Amaliah",
                                               icon("github", lib = "font-awesome"),
                                               class = "externallink"),
                                        tags$a(href = "https://www.linkedin.com/in/dewi-lestari-amaliah-579b231a1/",
                                               icon("linkedin", lib = "font-awesome"),
                                               class = "externallink"),
                                        tags$a(href = "https://datain360.com",
                                               icon("pen-square", lib = "font-awesome"),
                                               class = "externallink")
                                        ),
                                 
                                 column(width = 1)
                                 
                               )
                               
                               
                               ),
                      tabPanel("References",
                               h4("References"),
                               fluidRow(
                                 column(width = 2,
                                        h4("Data",
                                           align = "left")
                                        ),
                                 column(width = 9,
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                               "Daily COVID-19 Cases and Deaths"),
                                        getref("European Centre for Disease Control and Prevention in Joachim Gassen",
                                               "2020",
                                               "https://github.com/joachim-gassen/tidycovid19",
                                               "Cumulative COVID-19 Cases and Deaths"),
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://www.ecdc.europa.eu/en/publications-data/covid-19-testing",
                                               "Testing and Positivity Rate"),
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://qap.ecdc.europa.eu/public/extensions/COVID-19/COVID-19.html#subnational-transmission-tab",
                                               "Subnational COVID-19 Transmission Level"),
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19",
                                               "Country Response Measure to COVID-19"),
                                        getref("Google inc. in Joachim Gassen",
                                               "2020",
                                               "https://github.com/joachim-gassen/tidycovid19",
                                               "Google Community Mobility Report"),
                                        getref("Harvard University",
                                               "2020",
                                               "https://worldmap.harvard.edu/data/geonode:country_centroids_az8",
                                               "Harvard World Map: Country Centroid Coordinates")
                                        ),
                                 
                                 column(width = 1),
                                 column(width = 10),
                                 column(width = 2),
                                 column(width = 2,
                                        h4("Readings",
                                           align = "left")
                                        ),
                                 column(width = 8,
                                        br(),
                                        getref("David Druvolo",
                                               "2019",
                                               "hhttps://davidruvolo51.github.io/shinytutorials/tutorials/landing-page/",
                                               "Custom home pages in shiny"),
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://www.ecdc.europa.eu/sites/default/files/documents/2020-08-12_Variable_Dictionary_and_Disclaimer_weekly_testing_data_EUEEAUK_0.pdf",
                                               "COVID-19 Testing Data Dictionary"),
                                        getref("European Centre for Disease Control and Prevention",
                                               "2020",
                                               "https://www.ecdc.europa.eu/sites/default/files/documents/Variable_Dictionary_and_Disclaimer.pdf",
                                               "Response Measure Data Dictionary"),
                                        getref("Our World in Data",
                                               "2020",
                                               "https://ourworldindata.org/covid-mobility-trends",
                                               "Google Mobility Trends: How has the pandemic movement of people around the world?"),
                                        getref("Scott Gottlieb in Jennifer Kates, et.al.",
                                               "2020",
                                               "https://www.kff.org/policy-watch/what-testing-capacity-do-we-need/",
                                               "What Testing Capacity Do We Need?"),
                                        getref("World Health Organization in Pien Huang",
                                               "2020",
                                               "https://www.npr.org/sections/coronavirus-live-updates/2020/03/30/824127807/if-most-of-your-coronavirus-tests-come-back-positive-youre-not-testing-enough",
                                               "If Most Of Your Coronavirus Tests Come Back Positive, You're Not Testing Enough"),
                                        getref("World Health Organization in Our World in Data",
                                               "2020",
                                               "https://ourworldindata.org/coronavirus-testing",
                                               "Coronavirus (COVID-19) Testing")
                                        ),
                                 
                                 column(width = 2),
                                 column(width = 10),
                                 column(width = 2),
                                 column(width = 2,
                                        h4("R Packages",
                                           align = "left")
                                        ),
                                 column(width = 8,
                                        br(),
                                        #br(),
                                        getref("Carson Sievert", "2020", 
                                               "https://plotly-r.com", 
                                               "Interactive Web-Based Data Visualization with R, plotly, and shiny"),
                                        getref("Garrett Grolemund and Hadley Wickham",
                                               "2011",
                                               "http://www.jstatsoft.org/v40/i03/",
                                               "Dates and Times Made Easy with {lubridate}"),
                                        getref("Hadley Wickham",
                                               "2016",
                                               "https://ggplot2.tidyverse.org",
                                               "ggplot2: Elegant Graphics for Data Analysis"),
                                        getref("Hadley Wickham and Jennifer Bryan",
                                               "2019",
                                               "https://CRAN.R-project.org/package=readxl",
                                               "readxl: Read Excel Files"),
                                        getref("Hadley Wickham, et.al.",
                                               "2019",
                                               "https://www.tidyverse.org",
                                               "Welcome to the {tidyverse}"),
                                        getref("Joachim Gassen",
                                               "2020",
                                               "https://github.com/joachim-gassen/tidycovid19",
                                               "tidycovid19: Download, Tidy and Visualize Covid-19 Related Data"),
                                        getref("Kirill Müller",
                                               "2017",
                                               "https://CRAN.R-project.org/package=here",
                                               "here: A Simpler Way to Find Your Files"),
                                        getref("Ray Brownrigg and Thomas P Minka",
                                               "2020",
                                               "https://CRAN.R-project.org/package=mapproj",
                                               "mapproj: Map Projections"),
                                        getref("Ray Brownrigg, enhanced by Thomas P Minka and Alex Deckmyn",
                                               "2018",
                                               "https://CRAN.R-project.org/package=maps",
                                               "maps: Draw Geographical Maps"),
                                        getref("Simon Garnier",
                                               "2018",
                                               "https://CRAN.R-project.org/package=viridis",
                                               "viridis: Default Color Maps from 'matplotlib'"),
                                        getref("Simon Garnier",
                                               "2018",
                                               "https://CRAN.R-project.org/package=viridisLite",
                                               "viridisLite: Default Color Maps from 'matplotlib' (Lite Version)"),
                                        getref("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan McPherson",
                                               "2020",
                                               "https://shiny.rstudio.com",
                                               "shiny: Web Application Framework for R"),
                                        getref("Winston Chang",
                                               "2018",
                                               "https://CRAN.R-project.org/package=shinythemes",
                                               "shinythemes: Themes for Shiny"),
                                        getref("Victor Perrier, Fanny Meyer, David Granjon",
                                               "2020",
                                               "https://CRAN.R-project.org/package=shinyWidgets",
                                               "shinyWidgets: Custom Inputs Widgets for Shiny"),
                                        getref("Yihui Xie, Joe Cheng, and Xianying Tan",
                                               "2020",
                                               "https://CRAN.R-project.org/package=DT",
                                               "DT: A Wrapper of the JavaScript Library 'DataTables'")
                                        ),
                                 
                                 column(width = 2),
                                 column(width = 10),
                                 column(width = 2),
                                 column(width = 2,
                                        h4("Images",
                                           align = "left")
                                 ),
                                 column(width = 8,
                                        br(),
                                        getref("freepik",
                                               "2020",
                                               "https://www.flaticon.com/free-icon/dashboard_2303502?term=dashboard&page=1&position=86",
                                               "Dashboard"),
                                        getref("Julian Wan",
                                               "2020",
                                               "https://unsplash.com/photos/DWaC44FUV5o",
                                               "A couple enjoying sunshine during New York City's #CoronavirusQuarantine"),
                                        getref("iMattSmart",
                                               "2020",
                                               "https://unsplash.com/photos/8vTKvTdenyc",
                                               "Coronavirus stay at home"),
                                        getref("KOBU Agency",
                                               "2020",
                                               "https://unsplash.com/photos/Q0cm4L_Mvws",
                                               "Coronavirus / Covid-19 cases in the world"),
                                        getref("SJ Obijo",
                                               "2020",
                                               "https://unsplash.com/photos/8hHxO3iYuU0",
                                               "Covid-19 Face Masks")
                                        ),
                                 column(width = 2)
                               )
                               
                               )
                      )
             
             
             ))

##### SHINY SERVER ##### ------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  ### CASES TAB ### --------------------------------------------------------------------------------------
  #observe({
    #updateSelectizeInput(session = session, inputId = "country_select", 
                      #choices = pop2019$country, 
                      #selected = c("Italy", "Austria", "Spain"))
  #})
  
  filtered_daily <- reactive({
    dat <- c19euxplorer::daily_indicator(covid_eu_daily, input$indicator_select)
    
    # if (input$indicator_select=="Cases") {
    #   
    #   dat <- covid_eu_daily %>%
    #     select(country, date, daily_cases) %>%
    #     rename(count = daily_cases) 
    # }
    # 
    # if (input$indicator_select=="Cases per 10,000 people") {
    #   dat <- covid_eu_daily %>%
    #     select(country, date, daily_cases_rate) %>%
    #     rename(count = daily_cases_rate) 
    # }
    # 
    # if (input$indicator_select=="Deaths") {
    #   dat <- covid_eu_daily %>%
    #     select(country, date, daily_deaths) %>%
    #     rename(count = daily_deaths) 
    # }
    # 
    # if (input$indicator_select=="Deaths per 10,000 people") {
    #   dat <- covid_eu_daily %>%
    #     select(country, date, daily_deaths_rate) %>%
    #     rename(count = daily_deaths_rate) 
    # }
    
    # there is some negative value in daily cases and deaths as methodology changed, 
    # for example in Luxembourg in 2020/08/28
    # refer to Our World in Data website, I filtered out these dates, where the cases or deaths are negative
    
    dat %>% filter(date >= input$date_input[1],
           date <= input$date_input[2],
           country %in% input$country_select,
           count >= 0)
    
    
  })
  
  filtered_cumulative <- reactive({
    
    dat2 <- c19euxplorer::cumulative_indicator(covid_eu_cumulative, input$indicator_select)
    # if (input$indicator_select=="Cases") {
    #   dat2 <- covid_eu_cumulative %>%
    #     select(country, date, cumulative_cases) %>%
    #     rename(count = cumulative_cases) 
    # }
    # 
    # if (input$indicator_select=="Cases per 10,000 people") {
    #   dat2 <- covid_eu_cumulative %>%
    #     select(country, date, cumulative_cases_rate) %>%
    #     rename(count = cumulative_cases_rate) 
    # }
    # 
    # if (input$indicator_select=="Deaths") {
    #   dat2 <- covid_eu_cumulative %>%
    #     select(country, date, cumulative_deaths) %>%
    #     rename(count = cumulative_deaths) 
    # }
    # 
    # if (input$indicator_select=="Deaths per 10,000 people") {
    #   dat2 <- covid_eu_cumulative %>%
    #     select(country, date, cumulative_deaths_rate) %>%
    #     rename(count = cumulative_deaths_rate) 
    # }
    
    # there is some negative value in daily cases and deaths as methodology changed, 
    # for example in Luxembourg in 2020/08/28
    # refer to Our World in Data website, I filtered out these dates, where the cases or deaths are negative
    
    dat2 %>% filter(date >= input$date_input[1],
                   date <= input$date_input[2],
                   country %in% input$country_select)
    
    
  })
  
  
  output$covid_daily_plot <- renderPlotly({
    g1 <- ggplot(filtered_daily(), aes(date, count, colour = country)) +
      geom_line(size = 0.3) + labs(x = "Date", y = "Count",
                                   caption = "Data source: European Centre for Disease Prevention and Control") +
      theme_classic() 
    ggplotly(g1) 
  })
  
  output$covid_cumulative_plot <- renderPlotly({
    g2 <- ggplot(filtered_cumulative(), aes(date, count, colour = country)) +
      geom_line(size = 0.3) + labs(x = "Date", y = "Count",
                                   caption = "Data source: European Centre for Disease Prevention and Control")+
      theme_classic() 
    ggplotly(g2)
  })
  
  output$plot_title <- renderText({ 
    paste("COVID-19", input$indicator_select)
  })
  
  ### TESTING TAB ### ---------------------------------------------------------------------------------------
  
  
  filtered_testing_positivity_rate <- reactive({
    
    dat_testing_rate<- test_data_clean %>% filter(week >= input$input_date_test[1],
                   week <= input$input_date_test[2],
                   country %in% input$input_country_test)
    
  })
  
  output$covid_test_rate_plot <- renderPlotly({
    g3 <- ggplot(filtered_testing_positivity_rate(), aes(week, testing_rate_percent, colour = country)) +
      geom_line(size = 0.3) + labs(x = "Week", y = "Testing Rate (%)",
                                   caption = "Data source: European Centre for Disease Prevention and Control") +
      geom_hline(yintercept = 1, colour = "steelblue", linetype = 2) +
      theme_classic() 
    ggplotly(g3) 
  })
  
  output$covid_positivity_rate_plot <- renderPlotly({
    g3 <- ggplot(filtered_testing_positivity_rate(), aes(week, positivity_rate, colour = country)) +
      geom_line(size = 0.3) + labs(x = "Week", y = "Positivity Rate (%)",
                                   caption = "Data source: European Centre for Disease Prevention and Control") +
      geom_hline(yintercept = 5, colour = "steelblue", linetype = 2) +
      theme_classic() 
    ggplotly(g3) 
  })
  
  
  ### TRACING TAB ###-------------------------------------------------------------------------------------
  
  observeEvent(event_data("plotly_click"), {
    
    click_trans <- event_data("plotly_click")
    input_country_trans <- filter(cum_cases_rate_map, 
                      Longitude == click_trans$x,
                      Latitude == click_trans$y) %>%
      pull(country)
    
    updateSelectInput(session, "input_country_trans", selected = input_country_trans)
  })
  
  
  output$map <- renderPlotly({
    
    tmap <- ggplot() +
      geom_polygon(data = eu_map, 
                   aes(x = long,
                       y = lat,
                       group = group),
                   fill = "#73C6B6",
                   colour = "white",
                   alpha = 0.7
      ) +
      geom_point(data = cum_cases_rate_map,
                 aes(x = Longitude,
                     y = Latitude,
                     colour = cumulative_cases_rate,
                     size = cumulative_cases_rate,
                     text = mytext,
                     alpha = cumulative_cases_rate)) +
      scale_size_continuous(range = c(1, 6)) +
      scale_color_viridis(option = "inferno", trans = "log") +
      scale_alpha_continuous(trans = "log") +
      guides(colour = guide_legend()) + 
      xlab("") +
      ylab("") +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")) 
      
        
    ggplotly(tmap, tooltip =  "text")
    
  })
  
  
  output$covid_transmission_plot <- renderPlotly({
    
    color_category <- c(
      Clusters = "#D2B4DE",
      Community = "#EC7063",
      `No cases` = "#52BE80",
      `Not reported` = "#5DADE2",
      Sporadic = "#F9E79F"
    )
    
    trans_plot <- ggplot(filter(transmission_wrangled, Country == input$input_country_trans)) +
      geom_col(aes(x = week,
                   y = n,
                   fill = Transmission),
               position = "fill",
               width = 1) +
      theme_classic() +
      xlab("Week") +
      ylab("Transmission (%)") +
      scale_fill_manual(values = color_category)
    
    ggplotly(trans_plot)
    
  })
  
  output$trans_plot_title <- renderText({ 
    paste("Transmission of COVID-19 in", input$input_country_trans, "by Week")
  })
  
  
  ## COMMUNITY MOBILITY TAB ---------------------------------------------------------------------------------
  
  # table output
  filtered_response<- reactive({
    
    dat_response <- response_wrangled %>%
      filter(Country %in% input$country_response,
             meaning %in% input$measure_input)
    
  })
  
  output$response_table <- renderDataTable({
    datatable(filtered_response(),
              colnames = c("Country",
                           "Measure Code",
                           "Meaning",
                           "Date Start",
                           "Date End"),
              class = "cell-border stripe",
              options = list(
                pageLength = 5,
                searching = FALSE)
              )
    
    })
  
  
  # plot output
  
  filtered_mobility <- reactive({
    
    dat_mobility <- mobility %>%
      filter(date >= input$date_response[1],
             date <= input$date_response[2],
             country %in% input$country_response)
    
  })
  
  output$com_mobility_plot <- renderPlotly({
    
    color_index <- c(
      `Grocery and Pharmacy` = "#5CA8BC",
      Parks = "#27AE60",
      Residential = "#8E44AD",
      `Retail and Recreation` = "#2E86C1",
      `Transit Station` = "#E34DA4",
      Workplaces = "#F2752A"
    )
    
    mob_plot <- ggplot(filtered_mobility()) +
      geom_line(aes(x = date,
                    y = index,
                    colour = Location
                    ),
                size = 0.3) +
      geom_hline(yintercept = 0, 
                 colour = "grey50") +
      xlab("Date")+
      ylab("Index")+
      theme_classic()+
      scale_color_manual(values = color_index) 
    
    ggplotly(mob_plot)
    
  })
  
  output$selected_country_mob <- renderText({ 
    paste("Mobility Index for 6 Types of Locations in", input$country_response, "During the Pandemic")
  })
  
  
  ####DATA --------------------------------------------------------------------------------------------
  
  
  ### CASES AND DEATHS
  filtered_table1 <- reactive({
    dat_dt1 <- dt_cases_deaths %>%
      filter(date >= input$cases_deaths_date[1],
             date <= input$cases_deaths_date[2],
             country %in% input$cases_deaths_country)
  })
  
  output$cases_and_deaths_table <- renderDataTable({
    datatable(filtered_table1(),
              colnames = c(
                "Country",
                "Date",
                "Population",
                "Daily Cases",
                "Daily Cases Rate",
                "Daily Deaths",
                "Daily Deaths Rate",
                "Cumulative Cases",
                "Cumulative Cases Rate",
                "Cumulative Deaths",
                "Cumulative Deaths Rate",
                "GMI Retail Recreation",
                "GMI Grocery Pharmacy",
                "GMI Parks",
                "GMI Transit Stations",
                "GMI Workplaces",
                "GMI Residential"
              ),
              options = list(
                pageLength = 7,
                searching = FALSE,
                width = "600px",
                scrollX = T)
             
    )
    
  })
  
  ### TESTING AND TRANSMISSION
  
  filtered_table2 <- reactive({
    
    if (input$table_select=="Testing") {
      dat_dt <- testing_for_dt
    }
    
    if (input$table_select=="Transmission") {
      dat_dt <- tracing_for_dt %>%
        rename(country = Country,
               week = Week,
               subnational = Region,
               date = Date,
               transmission = Transmission)
    }
    
    dat_dt %>%
      filter(week %in% input$testing_trans_select_week,
             country %in% input$testing_trans_select)
    
  })
  
  output$testing_and_trans_table <- renderDataTable({
    datatable(filtered_table2(),
              options = list(
                pageLength = 8,
                searching = FALSE,
                width = "600px",
                scrollX = T)
    )
  })
  
  output$table_title <- renderText({ 
    paste(input$table_select, "Figure of COVID-19 in Europe")
  })
  
  
}


##### Shiny App #####
shinyApp(ui = ui, server = server)




  


