
library(tidyverse)
library(openintro)
library(albersusa)
library(readxl)
library(plotly)

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

covid19 <- read_csv("C:/RLibrary/Final Project/Final Project Map/covid-ny-times.txt",
                             col_types = cols(date = col_date(format = "%Y-%m-%d")))
  
populationReport <- read_csv("C:/RLibrary/Final Project/Final Project Map/PopulationReportCSV.csv")

covid19 <- left_join(covid19, populationReport,
                     c("state"="Name"))

us_states <- usa_sf("laea")

covidmap <- function(mydate, var){
  var1 <- enquo(var)
  
  covid19_display <- covid19 %>%
    filter(date == as.Date(mydate)) %>%
    mutate(casesText = paste("<b>",state,"</b>\nCases:",cases)) %>%
    mutate(deathsText = paste("<b>",state,"</b>\nDeaths:",deaths)) %>%
    mutate(casesThousands = cases / 1000) %>%
    mutate(casesPerMillion = round(
      (cases / Population)*1000000, digits=2)) %>%
    mutate(deathsPerMillion = round(
      (deaths / Population)*1000000, digits=2)) %>%
    mutate(casesPerMillionText = paste("<b>",state,"</b>\nCases Per Million:",casesPerMillion)) %>%
    mutate(deathsPerMillionText = paste("<b>",state,"</b>\nDeaths Per Million:",deathsPerMillion))
      
    
  
  us_states_covid19_display <- left_join(us_states,
                                         covid19_display,
                                         c("fips_state"="fips")) %>%
    select(state, my_stat=!!var1, cases, deaths, geometry,
           casesText, deathsText, casesThousands, 
           casesPerMillion, deathsPerMillion,
           casesPerMillionText, deathsPerMillionText) 
  
  if(toString(var)=="cases"){
    plot_us_states_covid19_display <-
      ggplot(us_states_covid19_display) +
      geom_sf(aes(fill=casesThousands+
                    runif(nrow(
                      us_states_covid19_display)),
        text=casesText,
        geometry=geometry)) +
      labs(title="Covid-19 Cases in the U.S.", 
           fill="Cases (thousands)") +
      scale_fill_continuous(low="yellow", high="red") +
      my_map_theme()    
  }else if(toString(var)=="deaths"){
    plot_us_states_covid19_display <-
      ggplot(us_states_covid19_display) +
      geom_sf(aes(fill=my_stat+
                    runif(nrow(
                      us_states_covid19_display)),
                  text=deathsText, 
                  geometry=geometry)) +
      labs(title="Covid-19 Deaths in the U.S.", 
           fill="Deaths") +
      scale_fill_continuous(low="yellow", high="red") +
      my_map_theme()  
  }else if(toString(var)=="casesPerMillion"){
    plot_us_states_covid19_display <-
      ggplot(us_states_covid19_display) +
      geom_sf(aes(fill=my_stat+
                    runif(nrow(
                      us_states_covid19_display)),
                  text=casesPerMillionText, 
                  geometry=geometry)) +
      labs(title="Covid-19 Cases Per Million in the U.S.", 
           fill="Cases Per\n Million") +
      scale_fill_continuous(low="yellow", high="red") +
      my_map_theme()      
  }else{
    plot_us_states_covid19_display <-
      ggplot(us_states_covid19_display) +
      geom_sf(aes(fill=my_stat+
                    runif(nrow(
                      us_states_covid19_display)),
                  text=deathsPerMillionText, 
                  geometry=geometry)) +
      labs(title="Covid-19 Deaths Per Million in the U.S.", 
           fill="Deaths Per\n Million") +
      scale_fill_continuous(low="yellow", high="red") +
      my_map_theme()    
  }
  
   ggplotly(plot_us_states_covid19_display,
           tooltip="text") %>%
          style(hoveron="fills")
}


# Define UI for generating our map
ui <- fluidPage(
  
  # Application title
  titlePanel("U.S. COVID-19 Information by State"),
  
  # Sidebar with a slider input for selecting a date for the map 
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  "Statistic to Plot:",
                  choices = list("Cases" = "cases",
                                 "Deaths" = "deaths",
                                 "Cases Per Million"=
                                   "casesPerMillion",
                                 "Deaths Per Million"=
                                   "deathsPerMillion"), 
                  selected = "cases"),
      sliderInput("mydate",
                  "Select a date to display:",
                  min = as.Date("2020-01-22","%Y-%m-%d"),
                  max = as.Date("2020-11-16","%Y-%m-%d"),
                  value = as.Date("2020-11-21"),
                  timeFormat="%m-%d")
    ),
    
    # Show our map
    mainPanel(
      plotlyOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderPlotly({
    covidmap(input$mydate, input$var)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)