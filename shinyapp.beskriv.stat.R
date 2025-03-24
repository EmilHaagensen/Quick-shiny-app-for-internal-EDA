library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(countrycode)
library(shinythemes)
library(bslib)
#### Shiny app med beskrivende stats ####

#### Klargøring af data ####

shiny.df = readRDS("newlife-bmi.rds")

shiny.df = shiny.df[, 1:74]


shiny.df <- shiny.df %>%
  rename(OriginalYear = Year)

shiny.long = shiny.df %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "LifeExpectancy") %>% 
  mutate(Year = as.numeric(sub("X", "", Year)))

#### UI ####

ui = fluidPage(
  theme = bs_theme(
    bg = "#343a40",   
    fg = "#f8f9fa",    
    primary = "#d4af37", 
    bootswatch = "lux"),
  tags$style(HTML("
  #life_expectancy_plot {
    margin-top: 1cm;
  }
")),
  titlePanel("Udforskning af de forskellige lande"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("Søg", "Søg efter et land", ""),
      selectInput("Country", "vælg et land", choices = unique(shiny.df$Country), selected = NULL),
      sliderInput("år", "Vælg hvilke år der skal kigges på",
                  min = min(shiny.long$Year), max = max(shiny.long$Year), 
                  value = c(min(shiny.long$Year), max(shiny.long$Year)), step = 1)
    ),
    
    mainPanel(uiOutput("country_flag"))
  ),
  fluidRow(
    column(6,plotOutput("life_expectancy_plot")),
    column(6, tableOutput("land_data"))
      )
  )

#### Server ####



server <- function(input, output, session) {
  
  observeEvent(input$Søg, {
    filt_countries = unique(shiny.df$Country[grep(paste0("^", input$Søg, ".*"), shiny.df$Country, ignore.case = TRUE)])
    updateSelectInput(session, "Country", choices = filt_countries)
    
  })
  
  selected_data = reactive({
    req(input$Country)
    shiny.long %>%  filter(Country == input$Country & Year >= input$år[1] & Year <= input$år[2])
  })
  

  output$land_data = renderTable({
    req(input$Country)
    shiny.df %>% filter (Country == input$Country) %>% 
      select(-starts_with("X")) %>% 
      t() %>% 
      as.data.frame()
  }, rownames = TRUE, colnames = FALSE)
  
  output$life_expectancy_plot = renderPlot({
    req(input$Country)
    ggplot(selected_data(), aes(x = Year, y = LifeExpectancy)) +
      geom_line(color = "#d4af37") + 
      geom_point(color = "darkblue") +
      labs(title = paste("Udvikling af life-expectancy i ", input$Country),
           x = "Year", y = "Life expectancy") +
      theme_minimal()
  })
  
  output$country_flag = renderUI({ 
    req(input$Country)
    
    country_code_3 = shiny.df %>%  filter(Country == input$Country) %>%  pull(Country.Code) %>% unique()
    
    country_code_2 = countrycode(country_code_3, "iso3c", "iso2c")
    
    if (!is.na(country_code_2)) {
      img_url = paste0("https://flagcdn.com/w320/", tolower(country_code_2), ".png")
      tags$img(src = img_url, width = "400px", style = "display: block; margin: auto;")
    } else {
      "Flag not available"
    }
    })

}

shinyApp(ui, server)

