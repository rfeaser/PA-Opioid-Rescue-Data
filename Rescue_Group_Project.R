#Loading in libraries
library(shiny)
library(zoo)
library(shinydashboard)
library(readr)
library(leaflet)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)
library(DT)

#Reading in csv files
help_hotline <- read_csv("Help_Hotline.csv",
                         col_types = cols(`Week Begin Date` = col_date()))
ems_naloxone <- read_csv("EMS_Naloxone.csv")
sp_od_info <- read_csv("OD_Info_State_Police.csv")
prescr_boxes <- read_csv("Take_Back_Locations.csv") %>%
  arrange(City)
cce <- read_csv("PA_CCE.csv")
ed_visits <- read_csv("ED_Visits.csv",
                      col_types = cols(`Quarter Date Start` = col_date()))

#Help Hotline Month Creation
help_hotline$month <-
  as.Date(as.yearmon(help_hotline$`Week Begin Date`))

#Pennsylvania Help Hotline
pa_hotline <- help_hotline %>%
  filter(`County Name` == "Commonwealth") %>%
  gather(gender, total, `Male Callers`:`Female Callers`) %>%
  group_by(month)

#PA Help Hotline Recommended Services for Caller
recommended_service <- help_hotline %>%
  filter(`County Name` == "Commonwealth") %>%
  gather(treatment, total, `Callers Referred for Assessment`:`Callers Referred to their SCA`)

#EMS Naloxone Dosage Dataset
naloxone_ems <- ems_naloxone %>%
  group_by(`Incident County - Name`) %>%
  summarise(naloxone_administration = n()) %>%
  mutate(subregion = tolower(`Incident County - Name`))

naloxone_sp <- sp_od_info %>%
  filter(`Naloxone Administered` == "Y") %>%
  group_by(`Incident County Name`) %>%
  summarise(naloxone_admin = n()) %>%
  mutate(subregion = tolower(`Incident County Name`))

#Creation of County Map
states <- map_data("state")
pa_df <- subset(states, region == "pennsylvania")
counties <- map_data("county")
pa_county <- subset(counties, region == "pennsylvania")

#Creation of County Maps for EMS, State Police, and CCE datasets
pa_naloxone_ems <-
  inner_join(pa_county, naloxone_ems, by = "subregion")
pa_naloxone_sp <-
  inner_join(pa_county, naloxone_sp, by = "subregion")
pa_cce <- inner_join(cce, pa_county, by = "subregion")

#Gets rid of axes in mapping
no_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#Box Icon creation for Drug Take Back map
boxicons <- iconList(box = makeIcon("Shipment_DropOff.png", "Shipment_DropOff.png", 35, 35))

#Shiny App
sidebar <- dashboardSidebar (sidebarMenu(
  #First tab
  menuItem(
    "First Responders/Police",
    tabName = "firstres",
    icon = icon("dashboard")
  ),
  
  #Second tab
  menuItem("PA Help Hotline",
           tabName = "help",
           icon = icon("th")),
  
  #Third tab
  menuItem(
    "Drug Take Back Locations",
    tabName = "maps",
    icon = icon("bar-chart-o")
  )
))

body <- dashboardBody(tabItems(
  
  # First tab content
  tabItem(tabName = "firstres",
          fluidRow(
            tabBox(
              title = "2018 Naloxone Administration",
              id = "tabset1",
              tabPanel("EMS", plotOutput("naloxone_ems")),
              tabPanel("SPD", plotOutput("naloxone_sp"))
            ),
            box(
              title = "Emergency Department Visit Rates",
              plotOutput("ed_visits"),
              selectInput("overdose", "Overdose Type",
                          unique(ed_visits$`Overdose Type`)),
              selectInput("ctynm", "County Name",
                          unique(ed_visits$`County Name`))
            )
          ),
          fluidRow(dataTableOutput("ccetable"))),
  
  # Second tab content
  tabItem(tabName = "help",
          fluidRow(
            box(
              title = "Call Intakes per County (Nov. 2016 - Present)",
              width = 5,
              background = "black",
              plotOutput("help_hotline"),
              selectInput(
                "county",
                "Choose a county:",
                unique(help_hotline$`County Name`)
              )
            ),
            box(
              title = "% of Male/Female Hotline Callers",
              background = "black",
              width = 5,
              plotOutput("gender_hotline")
            )
          ),
          fluidRow(
            box(
              title = "Recommended Treatments Across PA",
              background = "black",
              width = 50,
              plotOutput("service_hotline")
            )
          )),
  
  #Third tab content
  tabItem(
    tabName = "maps",
    h2("Drug Take-Back Boxes"),
    fluidPage(
      box(leafletOutput("dropoff"), width = 50),
      selectInput("city", "Choose a city:",
                  unique(prescr_boxes$City), selected = "Select All")
    )
  )
))

#Customization of dashboard
rescue <- dashboardPage(skin = "red",
                        dashboardHeader(title = "PA Rescue Data"),
                        sidebar,
                        body)

server <- function(input, output, session) {
  
  #Reactive filter for drug take-back boxes icons based on user input
  cityInput <- reactive({
    if (input$city != "Select All") {
      prescr_boxes %>%
        filter(City == input$city)
    } else {
      prescr_boxes
    }
  })
  
  #Drug Take-Back Boxes Interactive Map
  output$dropoff <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-77.77727, 40.80968, zoom = 6) %>%
      addMarkers(
        lng = prescr_boxes$Longitude,
        lat = prescr_boxes$Latitude,
        popup = prescr_boxes$Address,
        icon = boxicons
      )
  })
  
  #Reactive zooming for Drug Take-Back Box Map that also clears and adds icons based on user input
  observe({
    zoom <-  if (input$city != "Select All") {
      12
    } else {
      6
    }
    
    leafletProxy("dropoff") %>%
      clearMarkers() %>%
      addMarkers(
        lng = cityInput()$Longitude,
        lat = cityInput()$Latitude,
        popup = cityInput()$Address,
        icon = boxicons
      ) %>%
      setView(
        lng = cityInput()$Longitude[1],
        lat = cityInput()$Latitude[1],
        zoom = zoom
      )
  })
  
  #EMS Naloxone 2018 Map
  output$naloxone_ems <- renderPlot ({
    ggplot(data = pa_df,
           mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(color = "black", fill = "gray") +
      geom_polygon(data = pa_naloxone_ems,
                   aes(fill = naloxone_administration),
                   color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      labs(title = "2018 EMS Naloxone Administrations", fill = "# of People") +
      scale_fill_viridis(option = "rainbow") +
      no_axes
  })
  
  #State Police Naloxone 2018 Map
  output$naloxone_sp <-renderPlot ({
      ggplot(data = pa_df, mapping = aes(x = long, y = lat, group = group)) +
        coord_fixed(1.3) +
        geom_polygon(color = "black", fill = "gray") +
        geom_polygon(data = pa_naloxone_sp,
                     aes(fill = naloxone_admin),
                     color = "white") +
        geom_polygon(color = "black", fill = NA) +
        theme_bw() +
        labs(title = "2018 State Police Naloxone Administrations", fill = "# of People") +
        scale_fill_viridis(option = "rainbow") +
        no_axes
    })
  
  #Help hotline reactive county filter based on user input
  hotlineInput <- reactive({
    help_hotline %>%
      filter(`County Name` == input$county)
  })
  
  #Help hotline call intakes line graph
  output$help_hotline <- renderPlot({
    ggplot(data = hotlineInput(), aes(x = `Week Begin Date`, y = `Total Intakes`)) +
      geom_smooth()
  })
  
  #Help hotline gender percentage graph
  output$gender_hotline <- renderPlot({
    pa_hotline %>%
      ggplot(aes(x = month, y = total, fill = gender)) +
      geom_bar(stat = "identity", position = "fill")
  })
  
  #Help hotline recommended services bar graph
  output$service_hotline <- renderPlot ({
    recommended_service %>%
      ggplot(aes(x = month, y = total, fill = treatment)) +
      geom_col() +
      coord_flip()
  })
  
  #ED visits reactive user input
  edInput <- reactive({
    ed_visits %>%
      filter(`Overdose Type` == input$overdose & `Quarterly Rate` > 0
             & `County Name` == input$ctynm) %>%
      group_by(`Quarter Date Start`) %>%
      summarise(ED_Visit_Rate = mean(`Quarterly Rate`))
  })
  
  
  #ED visits graph
  output$ed_visits <- renderPlot({
    ggplot(data = edInput(), aes(x = `Quarter Date Start`, y = ED_Visit_Rate)) +
      geom_smooth() +
      labs(y = "Rate of ED Visits per overdose for 1000 residents")
  })
  
  #Data Table of CCE Information
  output$ccetable <- renderDataTable({
    datatable(cce, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}

shinyApp(rescue, server)
