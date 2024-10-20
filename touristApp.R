library(shiny)
library(leaflet)
library(readr)
library(readxl)
library(bslib)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(shinyjs)
library(ggiraph)


source('tableau-in-shiny-v1.0.R')

low_price <- 'https://public.tableau.com/views/AirbnbMap_16977273209510/lowprice?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
high_price <- 'https://public.tableau.com/views/AirbnbMap_16977273209510/highprice?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
most_reviewed <- 'https://public.tableau.com/views/AirbnbMap_16977273209510/mostreview?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
minimum_nights <- 'https://public.tableau.com/views/AirbnbMap_16977273209510/minnights?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link'
train_station <- 'https://public.tableau.com/shared/YGSD3BRWS?:display_count=n&:origin=viz_share_link'

# Create a bslib theme




lowPrice <- tabPanel(
  title='Low Price',

  fluidRow(
    column(4, HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
    column(6, tableauPublicViz(
      id='tableauViz2',
      url=low_price,
      height='600px',
    ),), 


  )
)

highPrice <- tabPanel(
  title='High Price',
  fluidRow(
    column(4, HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
    column(6, tableauPublicViz(
      id='tableauViz2',
      url=high_price,
      height='600px',
    ),), 

  )
)

mostReviewed <- tabPanel(
  title='Most Reviewed',

  fluidRow(
    column(4, HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
    column(6, tableauPublicViz(
      id='tableauViz2',
      url=most_reviewed,
      height='600px',
    ),), 
    
  )
)

minimumNights <- tabPanel(
  title='Minimum Nights',

  fluidRow(
    column(4, HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")),
    column(6, tableauPublicViz(
      id='tableauViz2',
      url=minimum_nights,
      height='600px',
    ),), 
    
  )
)

tramStation <- tabPanel(
  title='Tram Station',
  verticalLayout(
    splitLayout(
      tableauPublicViz(
        id='tableauViz5',
        url='https://public.tableau.com/views/trainandtram/3?:language=zh-CN&:display_count=n&:origin=viz_share_link',
        height='600px'
      ),
    ),
  )
)

trainStation <- tabPanel(
  title='Train Station',
  verticalLayout(
    splitLayout(
      tableauPublicViz(
       id='tableauViz6',
        url='https://public.tableau.com/views/trainandtram/2?:language=zh-CN&:display_count=n&:origin=viz_share_link',
        height='600px'
      ),
    ),
  )
)


# UI
ui <- navbarPage(
  header=setUpTableauInShiny(),
  useShinyjs(),
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  tags$style("
  .zoomable-image {
    transition: transform .2s; /* Animation */
    width: 100%;
    height: auto;
    margin: 0 auto;
  }
  
  .zoomable-image:hover {
    transform: scale(1.5); /* (150% zoom - Note: if the zoom is too large, it will go outside of the viewport) */
  }
  "),
  tags$style(HTML(
    "
    body {
      background-color: lightyellow; 
    }
    "
  )),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "weather-styles.css"),
    tags$script(src = "custom.js"),
    tags$style(HTML("
                .shiny-table {
                background-color: rgba(240, 240, 240, 0.7);
                }
                #forecastWeather {
                margin-left: 50px;
                margin-top: 20px;
                }
                #weather_plot {
                margin-left: 50px;
                margin-top: -70px;
                }
                .hidden-tab {
                display: none !important;
                }
                
             "))
  ),
  tags$head(
    tags$style(
      HTML(
        "
        .melbourne-image {
          position: absolute;
          top: 80px; /* Adjust this value to move the image up or down */
          right: 30px; /* Adjust this value to move the image left or right */
          z-index: 1000; /* Ensures the image is on top of other elements */
        }
        "
      )
    )
  ),
  tags$style(HTML("
    .custom-title {
      margin-left: 30px;
      margin-top: 30px;
      font-size: 25px;
    }
  ")),
  
  tags$style(HTML("
    .custom-navtitle {
      font-size: 18px;
    }
  ")),

  
  
    title = div("Melbourne Travel Guide", class = "custom-title"),
    tabPanel(div("Melbourne Map Guide", class = "custom-navtitle"),
             tags$img(src = "image/melbourne.png", class = "melbourne-image", width = "45%", height = "40%"),  # Adjust width and height as needed
             div(
               style = "height: 40vh; width: 50%; overflow-y: auto; padding: 20px; border: 0px solid #ddd; border-radius: 5px;",
               selectInput("dataset", "Choose a dataset:",
                           choices = c("Landmarks" = "landmarks",
                                       "Toilets" = "toilets",
                                       "Top 50 Restaurants" = "restaurants"
                           )), # Add landmarks option

               # Conditional controls for Toilets
               conditionalPanel(
                 condition = "input.dataset === 'toilets'",
                 checkboxInput("female", "Female", value = TRUE),
                 checkboxInput("male", "Male", value = TRUE),
                 checkboxInput("wheelchair", "Wheelchair Accessible", value = TRUE),
               ),

               # Conditional controls for Landmarks
               conditionalPanel(
                 condition = "input.dataset === 'landmarks'",
                 selectInput("theme", "Choose a theme:", choices = c("All", unique(read_csv("dataset/landmark.csv")$Theme)))
               )
             ), # End of div
             div(style = "position: absolute; bottom: 0; height: 50%; width: 100%;",
                 leafletOutput("map", height = "100%")
             )

    ),

    tabPanel("Hidden Tab",
           uiOutput("googleMapIframe")),
    tabPanel(div("Airbnb", class = "custom-navtitle"), 
             fluidRow(
               column(10,style = "padding-left: 50px;",
                      tags$div("Tableau Airbnb Map Integration with Shiny", style = "font-size: 20px; font-weight: bold;"),
                      tags$div("If some graphs didn't show after waiting, it's the internet issue, just refresh the page", style = "font-size: 16px; font-weight: bold;"),
                      tags$br(),
                      tags$br(),
                      tags$br(),
                      
                      
                )
             ),
            
             div(
               style = "height: 40vh; width: 50%; overflow-y: auto; padding: 20px; border: 0px solid #ddd; border-radius: 5px;",
               selectInput("sheetSelector",
                           "Choose a Tableau worksheet:",
                           choices = c("price low to high",
                                       "price high to low",
                                       "most reviewed",
                                       "minimum nights")),
               tags$br(),
               tags$br(),
               tags$div(
                 "This page will help you find the top 10 airbnb for each states,",
                 tags$br(),
                 "sorted by price, number of reviews and the minimum nights to stay.",
                 tags$br(),
                 "Every point represents an airbnb, with popup message of corresponding information",
                 style = "font-size: 16px; font-weight: bold;"
               )
               
             ), 
             div(style = "position: absolute; bottom: 0; height: 70%; width: 100%; text-align: center;",
                 conditionalPanel(
                   condition = "input.sheetSelector === 'price low to high'",
                   lowPrice
                 ),
                 conditionalPanel(
                   condition = "input.sheetSelector === 'price high to low'",
                   highPrice
                 ),
                 conditionalPanel(
                   condition = "input.sheetSelector === 'most reviewed'",
                   mostReviewed
                 ),
                 conditionalPanel(
                   condition = "input.sheetSelector === 'minimum nights'",
                   minimumNights
                 )
             )
             
    ),

    tabPanel(div("travel suggestion", class = "custom-navtitle"), 

             fluidRow(
               column(10,style = "padding-left: 50px;",
                      

                      tags$div("Travel Recommendation Engine", style = "font-size: 20px; font-weight: bold;"),
                      tags$div("Integrated With ChatGPT, Needs to wait for the answer", style = "font-size: 16px; font-weight: bold;"),
                      tags$br(),
                      tags$br(),
                      textInput("interests", "Interests", value = "Art, Music"),
                      tags$br(),
                      numericInput("budget", "Budget (in AUD)", value = 300),
                      tags$br(),
                      tags$br(),
                      dateRangeInput("dates", "Travel Dates", start = Sys.Date(), end = Sys.Date() + 7),
                      tags$br(),
                      tags$br(),
                      actionButton("plan", "Plan My Trip"),
                      tags$br(),
                      tags$br(),
                      h4("Your Travel Recommendations"),

               ),
               
             ),

             verbatimTextOutput("recommendations")
    ),

  tabPanel(div("weather", class = "custom-navtitle"), 
           tags$head(
             tags$style(HTML("
                #current-time {
                  position: relative;
                  top: 60px;
                  left: 70px;
                  font-size: 25px;
                  font-weight: bold;
                  font-family: Arial, sans-serif; 
                  color: #FFA07A;
                  display: inline-block;
                }
                 #current-weather-icon {
                  position: absolute;
                  top: 0px;
                  left: 50px; 
                  width: 70px; 
                  height: 70px; 
                }
              "))
           ),
           
           div(id = "weatherBackground",
               div(class = "raindrop"),
               div(class = "raindrop"),
               div(class = "raindrop"),
               div(class = "raindrop"),
               div(class = "raindrop"),
               div(class = "raindrop"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               div(class = "snowflake"),
               uiOutput("currentTime"),
               uiOutput("currentWeatherIcon", style = "current-weather-icon"),
               fluidRow(
                 column(3, style = "padding-left: 50px;",
                        h4("Current weather:"),
                        div(class = "data-font", verbatimTextOutput("currentWeather")),
                        h4("Feels like:"),
                        div(class = "data-font", verbatimTextOutput("feelsLike")),
                        h4("Wind speed:"),
                        div(class = "data-font", verbatimTextOutput("windSpeed"))
                 ),
                 column(8, style = "padding-left: 50px;",
                        plotlyOutput("weather_plot"))
                 
               ),
               
               fluidRow(
                 tableOutput("forecastWeather")
               )
           ),
           actionButton("showTip", "Little Tips", style = "position:fixed; bottom:100px; right:200px;", class = "custom-btn"),
           verbatimTextOutput("weatherAdvice")
  ),

  tabPanel(div("transport", class = "custom-navtitle"),
           
           # Section: Instructions and information
           fluidRow(
             column(12, style = "padding-left: 50px;",
                    tags$div("Tableau with train map and tram map", style = "font-size: 20px; font-weight: bold;"),
                    tags$div("If some graphs didn't show after waiting, it's the internet issue, just refresh the page", style = "font-size: 16px; font-weight: bold;"),
                    tags$div("You can find the corresponding station based on the street name on the map.", style = "font-size: 12px; font-weight: bold;")
             )
           ),
           
           # Section: Map selection
           fluidRow(
             column(6, style = "padding-left: 50px;",
                    column(6,tags$div(style = "margin-top: 20px; text-align: left;", 
                             checkboxInput("showTrain", "Show Train Station", value = FALSE),
                             checkboxInput("showTram", "Show Tram Station", value = FALSE),
                             tags$br(),
                             tags$div("How to use the map：", style = "font-size: 16px; font-weight: bold;"),
                             tags$ul(
                               tags$li("Select 'Show Train Station' or 'Show Tram Station' to display the corresponding map."),
                               tags$li("After the Tableau visualization loads, you can interact with it to see details about each site."),
                               tags$li("For a better experience please use the street name map provided at right side. Easily locate and plan your trip by comparing street names with Tableau maps below.")
                             )
                      )
                    ),
                    column(12,
                           conditionalPanel(
                             condition = "input.showTrain",
                             trainStation
                           ),
                           conditionalPanel(
                             condition = "input.showTram",
                             tramStation
                           )
                    )
             ),
             column(6, 
                    conditionalPanel(
                      condition = "input.showTrain",
                      column(10, tags$img(src = "image/train.png", width = "100%",class = "zoomable-image"))
                    ),
                    conditionalPanel(
                      condition = "input.showTram",
                      column(10, tags$img(src = "image/tram.png", width = "100%",class = "zoomable-image"))
                    )
             )
           )


           
           
  )
  
  

)

  



# Server
server <- function(input, output, session) {
  
  # Custom icons
  toiletIcon <- makeIcon(
    iconUrl = "/icons/toilt.png",
    iconWidth = 40, iconHeight = 40
  )
  
  restaurantIcon <- makeIcon(
    iconUrl = "/icons/restaurant.png",
    iconWidth = 40, iconHeight = 40
  )
  
  landmarkIcon <- makeIcon(
    iconUrl = "/icons/landmark.png",
    iconWidth = 40, iconHeight = 40
  )
  
  observe({
    map_data <- leaflet() %>%
      addTiles() %>%
      setView(144.9631, -37.8136, zoom = 12)
    
    if (input$dataset == "toilets") {
      toilets <- read_csv("dataset/public_toilets.csv")
      
      # Define filters
      filter_female <- if(input$female) toilets$female == "yes" else FALSE
      filter_male <- if(input$male) toilets$male == "yes" else FALSE
      filter_wheelchair <- if(input$wheelchair) toilets$wheelchair == "yes" else FALSE
      
      # Apply "or" logic
      filter_combined <- filter_female | filter_male | filter_wheelchair
      
      # Filter data
      toilets <- toilets[filter_combined, ]
      
      # Render map
      output$map <- renderLeaflet({
        leaflet(data = toilets) %>%
          setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
          addTiles() %>%
          addMarkers(~longitude, ~latitude, icon = toiletIcon, popup = ~paste0(
            "<strong>Name: </strong>", name, "<br>",
            "<strong>Female: </strong>", ifelse(female == "yes", "Available", "Not available"), "<br>",
            "<strong>Male: </strong>", ifelse(male == "yes", "Available", "Not available"), "<br>",
            "<strong>Wheelchair: </strong>", ifelse(wheelchair == "yes", "Accessible", "Not accessible"), "<br>",
            "<strong>Baby Facilities: </strong>", ifelse(baby_facil == "yes", "Available", "Not available"),
            "<br><button onclick='setDestination(", latitude, ",", longitude, "); getUserLocation(); switchToGoogleMapTab();'>Go there</button>"
          ))
      })
    } else if (input$dataset == "restaurants") {
      restaurants <- read_excel("dataset/restaurant.xlsx") # Ensure this Excel file is in your working directory
      
      output$map <- renderLeaflet({
        leaflet(data = restaurants) %>%
          setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
          addTiles() %>%
          addMarkers(~y_cor, ~x_cor, icon = restaurantIcon, popup = ~paste0(
            "<strong>Name: </strong>", name, "<br>",
            "<strong>Location: </strong>", location, "<br>",
            "<strong>Opening Hours: </strong>", opening_hours,
            "<br><button onclick='setDestination(", x_cor, ",", y_cor, "); getUserLocation(); switchToGoogleMapTab();'>Go there</button>"
          ))
      })
    } else if (input$dataset == "landmarks") {
      landmarks <- read_csv("dataset/landmark.csv", show_col_types = FALSE)
      
      # Filter landmarks by theme if not "All"
      if (input$theme != "All" && !is.null(input$theme)) {
        landmarks <- subset(landmarks, Theme == input$theme)
      }
      
      # For rows with NA values in certain columns, set a default value.
      landmarks$`Feature Name`[is.na(landmarks$`Feature Name`)] <- "Unknown"
      landmarks$des[is.na(landmarks$des)] <- "Description not available"
      landmarks$picture_url[is.na(landmarks$picture_url)] <- "Image not available"
      
      output$map <- renderLeaflet({
        leaflet(data = landmarks) %>%
          setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
          addTiles() %>%
          addMarkers(
            ~y_cor, ~x_cor,
            icon = landmarkIcon,
            popup = ~paste0("<strong>Feature Name: </strong>", `Feature Name`, "<br>",
                            ifelse(landmarks$picture_url != "Image not available", paste0("<img src='", picture_url, "' alt='Image' width='200' height='150'>"), "Image not available"),
                            "<br><button onclick='setDestination(", x_cor, ",", y_cor, "); getUserLocation(); switchToGoogleMapTab();'>Go there</button>"
            )
            
          )
      })
      observe({
        # 检查是否选择了地点
        if (!is.null(input$selected_lat) && !is.null(input$selected_lng)) {
          print(input$selected_lat)
          print(input$selected_lng)
          print(input$user_coords$lat)
          print(input$user_coords$lng)
          
          output$googleMapIframe <- renderUI({
            tags$iframe(
              src = paste0(
                "https://www.google.com/maps/embed/v1/directions?key=", "AIzaSyCFfPVfLwb8yldAlkmgoGC09CECg6F2Z_U",
                "&origin=", input$user_coords$lat,",", input$user_coords$lng,
                "&destination=",input$selected_lat,",", input$selected_lng,
                "&mode=walking"
              ),
              width = "100%", height = "1200vh"
            )
          })
        }
      })
    }
  })
  
  # below is all the code for the weather
  # current date and time update every time
  observe({
    invalidateLater(1000) # 1000 milliseconds = 1 second
    current_time <- Sys.time()
    formatted_time <- format(current_time, "%Y-%m-%d %H:%M:%S")
    output$currentTime <- renderUI({
      tags$div(id = "current-time", formatted_time)
    })
  })
  
  api_key <- "f8840183ef6fbd0cd4f3be24f0b2e2f2"
  
  get_current_weather <- function() {
    url <- sprintf("http://api.openweathermap.org/data/2.5/weather?q=Melbourne,au&units=metric&appid=%s", api_key)
    response <- GET(url)
    if (status_code(response) != 200) {
      stop("Failed to fetch current weather data!")
    }
    content(response, as = "parsed")
    print(content(response, as = "parsed"))
  }
  
  get_forecast_weather <- function() {
    url <- sprintf("http://api.openweathermap.org/data/2.5/forecast?q=Melbourne,au&units=metric&appid=%s", api_key)
    response <- GET(url)
    if (status_code(response) != 200) {
      stop("Failed to fetch forecast weather data!")
    }
    content(response, as = "parsed")
  }
  
  # current weather
  output$currentWeather <- renderText({
    weather_data <- get_current_weather()
    paste(weather_data$weather[[1]]$description)
  })
  
  # "Feels like"the temprature.
  output$feelsLike <- renderText({
    weather_data <- get_current_weather()
    paste(weather_data$main$feels_like, "°C")
  })
  
  # show the wind speed
  output$windSpeed <- renderText({
    weather_data <- get_current_weather()
    paste(weather_data$wind$speed, "m/s")
  })
  
  # get the picture for each weather description
  get_icon_url <- function(description) {
    
    url <- switch(description,
                  "clear sky" = "https://openweathermap.org/img/wn/01d@2x.png",
                  "few clouds" = "https://openweathermap.org/img/wn/02d@2x.png",
                  "scattered clouds" = "https://openweathermap.org/img/wn/03d@2x.png",
                  "broken clouds" = "https://openweathermap.org/img/wn/04d@2x.png",
                  "overcast clouds" = "https://openweathermap.org/img/wn/04d@2x.png",
                  "moderate rain" = "https://openweathermap.org/img/wn/09d@2x.png",
                  "light rain" = "https://openweathermap.org/img/wn/10d@2x.png",
                  "light intensity shower rain" = "https://openweathermap.org/img/wn/10d@2x.png",
                  "light intensity drizzle" = "https://openweathermap.org/img/wn/10d@2x.png",
                  "thunderstorm" = "https://openweathermap.org/img/wn/11d@2x.png",
                  "snow" = "https://openweathermap.org/img/wn/13d@2x.png",
                  "mist" = "https://openweathermap.org/img/wn/50d@2x.png",
                  default = "https://path.to.default.icon"
    )
  }
  
  current_weather_data1 <- reactive({
    get_current_weather()
  })
  output$currentWeatherIcon <- renderUI({
    
    # Safely get the description from current_weather_data1
    if(!is.null(current_weather_data1()$weather) && length(current_weather_data1()$weather) >= 1) {
      description <<- current_weather_data1()$weather[[1]]$description
      if(description == "clear sky") {
        session$sendCustomMessage(type = 'setWeatherBackground', message = "clear-sky")
      } else if (description == "few clouds"){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "few-clouds")
      } else if (description  %in% c("overcast clouds", "broken clouds", "scattered clouds")){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "overcast-clouds")
      } else if (description %in% c("light rain", "light intensity drizzle", "moderate rain", "light intensity shower rain")){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "light-rain")
      } else if (description == "thunderstorm"){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "thunderstorm")
      } else if (description == "snow"){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "snow")
      } else if (description == "mist"){
        session$sendCustomMessage(type = 'setWeatherBackground', message = "mist")
      } 
    } else {
      description <- NULL
    }
    
    # If description is not NULL, get the icon URL
    if(!is.null(description)) {
      current_icon_url <- get_icon_url(description)
    } else {
      current_icon_url <- NULL  # or set to a default icon URL
    }
    
    ## Render the icon on the UI
    if(!is.null(current_icon_url)) {
      tags$img(src = current_icon_url, height = "80px", width = "80px")
    } else {
      # Display an alternative content or nothing if the icon URL is NULL
      NULL
    }
  })
  
  observeEvent(input$showTip, {
    advice <- switch(description,
                     "clear sky" = "The sun is quite strong, please wear sunscreen when going out.",
                     "few clouds" = "There are only a few clouds outside, it's a great day to go out！",
                     "scattered clouds" = "The sky is overcast with dark clouds outside, please take an umbrella with you in case it rains.",
                     "broken clouds" = "The sky is overcast with dark clouds outside, please take an umbrella with you in case it rains.",
                     "overcast clouds" = "The sky is overcast with dark clouds outside, please take an umbrella with you in case it rains.",
                     "light rain" = "It's raining outside, please be careful as the roads may be slippery.",
                     "light intensity shower rain" = "It's raining outside, please be careful as the roads may be slippery.",
                     "moderate rain" = "t's pouring rain outside. Please take necessary precautions against the rain and be cautious about traffic safety when heading out.",
                     "thunderstorm" = "It's stormy with thunder and lightning outside. It's not advisable to go out.",
                     "snow" = "It's snowing outside. Wanna to build a snowman together?⛄️",
                     "mist" = "It's smoggy outside. Please wear a mask when going out and pay attention to traffic safety.",
    )
    showModal(modalDialog(
      title = "Weather Advice",
      advice,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # forecast weather table
  output$forecastWeather <- renderTable({
    data <- get_forecast_weather()
    if (is.null(data) || is.null(data$list)) return(NULL)
    
    # get min and max temperature
    get_daily_temps <- function(list_data) {
      dates <- sapply(list_data, function(x) substr(x$dt_txt, 1, 10))
      temp_mins <- sapply(list_data, function(x) x$main$temp_min)
      temp_maxs <- sapply(list_data, function(x) x$main$temp_max)
      
      min_temps <- tapply(temp_mins, dates, min)
      max_temps <- tapply(temp_maxs, dates, max)
      
      return(list(min = min_temps, max = max_temps))
    }
    
    daily_temps <- get_daily_temps(data$list)
    
    forecast_table_data <- lapply(data$list[seq(1, length(data$list), by = 8)], function(x) {
      description_text = x$weather[[1]]$description
      icon_url = get_icon_url(description_text)
      date_only = format(as.POSIXct(x$dt_txt, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d")
      
      temperature_text = sprintf("%s°C - %s°C", daily_temps$min[date_only], daily_temps$max[date_only])
      print(temperature_text)
      
      list(
        Date = date_only,
        Description = paste0(description_text, 
                             '<img src="', icon_url, '" width="40px" height="40px"/>'),
        Temperature = temperature_text,
        Humidity = paste(x$main$humidity, "%"),
        Wind = paste(x$wind$speed, "m/s")
      )
    })
    
    do.call(rbind, forecast_table_data)
  }, sanitize.text.function = identity)
  
  # forecast line plot：
  output$weather_plot <- renderPlotly({
    
    data <- get_forecast_weather()
    
    N <- 8
    df <- data.frame(
      hour = sapply(1:N, function(i) format(as.POSIXct(data$list[[i]]$dt, origin="1970-01-01"), "%H:%M")),
      temp = sapply(1:N, function(i) data$list[[i]]$main$temp),
      rain_pct = sapply(1:N, function(i) paste(data$list[[i]]$main$humidity, "%")),
      weather_desc = sapply(1:N, function(i) data$list[[i]]$weather[[1]]$description),
      wind_speed = sapply(1:N, function(i) paste(data$list[[i]]$wind$speed, "m/s"))
    )
    
    p <- ggplot(df, aes(x = hour, y = temp)) + 
      geom_line(aes(group = 1), color = "#ADD8E6", size = 1.5) + 
      geom_point(aes(text = paste("Time: ", df$hour,
                                  "<br>Temperature: ", temp, "°C",
                                  "<br>Rain: ", rain_pct, 
                                  "<br>Weather: ", weather_desc, 
                                  "<br>Wind speed: ", wind_speed)), 
                 size = 3, color = "#003366") +
      scale_x_discrete(breaks = df$hour) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) +
      labs(title = "Hourly forecast", y = NULL, x = NULL)
    
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  })
  # For the travel suggestion tab
  observeEvent(input$plan, {
    notif_id <- showNotification(
      id = "waitingNotif",  # Set an ID for the notification so we can remove it later
      ui = tags$div(
        icon("spinner", class = "fa-pulse"),  # This creates a spinning icon
        " Waiting for recommendations..."
      ),
      duration = NULL,  # This makes the notification stay until we remove it
      closeButton = FALSE,
      type = "message"  # Set the type of the notification
    )
    message <- paste(
      "I'm planning a trip to Melbourne with an interest in", input$interests, 
      "and a budget of", input$budget, 
      "AUD, from", input$dates[1], "to", input$dates[2], 
      ". Do you have any recommendations?"
    )
    
    # Define the URL and your actual API key
    api_url <- "https://api.openai.com/v1/chat/completions"
    api_key <- "123456" # Replace this with your actual API key
    
    # Set up the request
    response <- POST(api_url,
                     add_headers("Content-Type" = "application/json", 
                                 "Authorization" = paste("Bearer", api_key)),
                     body = toJSON(list(
                       model = "gpt-3.5-turbo",
                       messages = list(list(role = "user", content = message)),
                       temperature = 0.7
                     ), auto_unbox = TRUE),
                     encode = "json")
    
    gpt_response <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    # Check if the request was successful
    if (status_code(response) == 200) {
      removeNotification(id = notif_id)
      message_as_list <- as.list(gpt_response$choices$message)
      recommendations <- message_as_list$content[[1]]
      
    } else {
      # If the request failed, display an error message
      print("no")
      recommendations <- paste("Error:", status_code(response), content(response, "text", encoding = "UTF-8"))
    }
    
    # Output recommendations to UI
    output$recommendations <- renderText({
      recommendations
    })
  })
  
  
  
  output$tableauIframe <- renderUI({
    
    tableau_url <- switch(input$sheetSelector,
                          "price low to high" ='show the low price graph',
                          "price high to low" = "show the high price graph",
                          "most reviewed" = "...",
                          "minimum nights" = "..."
    )
    tags$iframe(src=tableau_url, height=600, width=800)
  })
  
  observeEvent(input$showTrain, {
    if(input$showTrain) {
      updateCheckboxInput(session, "showTram", value = FALSE)
    }
  })
  
  observeEvent(input$showTram, {
    if(input$showTram) {
      updateCheckboxInput(session, "showTrain", value = FALSE)
    }
  })
  output$tableauIframe1 <- renderUI({
    if (input$showTrain) {
      tableau_url1 <- 'https://public.tableau.com/views/trainandtram/2?:language=zh-CN&:display_count=n&:origin=viz_share_link'
    } else if (input$showTram) {
      tableau_url1 <- 'https://public.tableau.com/views/trainandtram/3?:language=zh-CN&:display_count=n&:origin=viz_share_link'
    } else {
      return(NULL) 
    }
    tags$iframe(src=tableau_url1, height=600, width=800)
  })
  
}

# Run the Shiny app
shinyApp(ui, server, options=list(launch.browser=TRUE))



