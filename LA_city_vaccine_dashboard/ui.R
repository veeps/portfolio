library(shiny)
library(tidyverse)
library(DT)
library(plotly)
# define some credentials



ui <- fluidPage(
  tags$head(HTML("<script type='text/javascript' src='keep-alive.js'></script>")),
  titlePanel(h2("City of LA COVID Vaccination Dashboard")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(style="height:100px", imageOutput("logo")),
      fluidRow(h5("Internal COVID Vaccination Dashboard. Data source from Carbon Health.")),
      fluidRow(p("All filter selections will be carried throughout the entire dashboard.")),
      fluidRow(selectInput(inputId="site_type", #references the input to server
                           label = h4("Select Site Type"), # text that appears on UI
                           choices=c("Overall", "Mobile Sites" = "Mobile Site", "City Sites" = "City Site","Private Sites" = "Private Site", "Harbor" = "Harbor" , "Kiosk" = "Kiosk" , "Homebound" = "Homebound",  "SNF" = "SNF", "UHRC" = "UHRC"))),
      fluidRow(dateRangeInput("dates", label = h4("Select Date range"), 
                              min = min(vax$appointment_date), max = max(vax$appointment_date),
                              start = min(vax$appointment_date), end = max(vax$appointment_date))),
      fluidRow(selectInput("locations", "Select Location", choices = NULL)),
      fluidRow(uiOutput("check_peh"))
    ), #SidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("Main ", fluid = TRUE,
                 fluidRow(style="padding-top:20px; text-align:center",
                          column(3,
                                 div(h2(textOutput("total_booked")),
                                     h3("Appointments Booked")
                                 )),
                          column(3,
                                 div(h2(textOutput("total_completed")),
                                     h3("Appointments Completed")
                                 )),
                          column(3,
                                 div(h2(textOutput("total_px")),
                                     h3("Patients")
                                 )),
                          column(3,
                                 div(h2(textOutput("median_age")),
                                     h3("Median Age")
                                 ))
                 ),
                 hr(),
                 fluidRow(style="padding-top:20px; text-align:left",
                          h4("Demographic Breakdown")),
                 fluidRow(plotly::plotlyOutput("demo")),
                 fluidRow(textOutput("missing_race")),
                 hr(),
                 fluidRow(
                   column(6,h4("Age Breakdown"), DTOutput("age_table")),
                   column(6,h4("Gender Breakdown"), DTOutput("gender_table")),
                 ),
                 fluidRow(downloadButton("demo_dl", "Download Demographics Data"))
                 
        ),#Tab panel
        
        # tabPanel("Vaccine Coverage", fluid = TRUE,
        #          fluidPage(
        #            fluidRow(h3("Percent of Population Vaccinated by Neighborhood")),
        #            fluidRow(p("The data on this map is provided by LA County Department of Public Health, which includes  vaccination coverage beyond just City site locations. Click on layers to view COVID case rates.")),
        #            fluidRow(p("")),
        #            fluidRow(tags$b("Green = Neighborhoods with under 60% of its population vaccinated.")),
        #            fluidRow(tags$b("Blue = Neighborhoods withover 60% of its population vaccinated.")),
        #            fluidRow(p("")),
        #            fluidRow(leafletOutput(outputId = "non_reactive_map", height="600px")))),
      
            tabPanel("Zipcode Breakdown", fluid = TRUE,
                            fluidPage(
                              fluidRow(h3("Vaccination Coverage by Zipcode")),
                              #fluidRow(p("Click on layers to view test positivity rates, and LA city outline.")),
                              #fluidRow(leafletOutput(outputId = "reactive_map", height="600px")),
                              fluidRow(
                                column(6,
                                       div(h2(textOutput("perc_la")),
                                           h3("in City of LA")
                                       )),
                                column(6,
                                       div(h2(textOutput("perc_county")),
                                           h3("in LA County")
                                       ))
                              ),
                  # hr(),
                  # fluidRow(h3("COVID Maps")),
                  # fluidRow(leafletOutput(outputId = "non_reactive_map", height="600px")),
                   hr(),
                   fluidRow(DTOutput("zip_table")),
                   fluidRow(textOutput("missing_zip")),
                   fluidRow(downloadButton("zips_dl", "Download"))
                 ) # fluidPage
        ),# tabpanel
        tabPanel("Hourly Breakdown", fluid = TRUE,
                 fluidRow(style="padding-top:20px; text-align:center",
                          h4("First Appointments Booked by the Hour")),
                 fluidRow(plotlyOutput("bar_chart")),
                 fluidRow(DTOutput("race_table")),
                 fluidRow(downloadButton("race_dl", "Download"))
        ),
        tabPanel("Weekly Trends", fluid = TRUE,
                 fluidRow(style="padding-top:20px, text-align:left",
                          h2("Weekly Trends for First Appointments Booked Over Time")),
                 fluidRow(style="padding-top:20px; text-align:left",
                          h4("Race/Ethnicity Breakdown")),
                 fluidRow(plotlyOutput("race_lines")),
                 fluidRow(style="padding-top:20px; text-align:left",
                          h4("Brand Breakdown")),
                 fluidRow(plotlyOutput("brand_lines")),
                 fluidRow(style="padding-top:20px; text-align:left",
                          h4("Healthy Places Index (HPI) Breakdown")),
                 fluidRow(p("A low HPI value indicates that the underlying population is, on average, less healthy and has less access to healthcare according to the state's Healthy Places Index.")),
                 fluidRow(plotlyOutput("hpi_lines"))
                 
                 ),
        tabPanel("Time Series", fluid = TRUE,
                 fluidRow(style="padding-top:20px, text-align:left",
                          h2("Daily Breakdown of Appointments by Site, Brand, and Dose")),
                 fluidRow(DTOutput("daily_table")),
                 fluidRow(downloadButton("daily_dl", "Download"))
             
        )
        # tabPanel("HPI", fluid = TRUE,
        #          fluidPage(
        #            fluidRow(
        #              column(style="height:150px", 3, h3("HPI x Vaccines Administered Map"),
        #                     imageOutput("legend")
        #              ),
        #              column(9, leafletOutput(outputId = "bivar_map"))),
        #            fluidRow(style="padding-top:0px; ",
        #                     p("A low Healthy Places Index (HPI) value indicates that the underlying population is, on average, unhealthy according to the state's Healthy Places Index. This is represented in the color white on the map. Visualizing the HPI value against our total % vaccinated is an effective tool to help prioritize areas. Here's how to read the map:")
        #            ),
        #            fluidRow("Green: High vaccine coverage and unhealthy HPI."),
        #            fluidRow("Purple: High vaccine coverage and healthy HPI."),
        #            fluidRow("White: Low vaccine coverage and unhealthy HPI."),
        #            fluidRow( "We can prioritize our efforts by working to improve vaccination coverage at the white and light pink areas.")
        #          )# fluidPage
        # )
      )# tabsetpanel
    ) # main panel
  ), #sidebar panel
  hr(),
  print("© Dashboard created by Vivian Peng, Elmer Camargo, and the Mayor's Innovation Team.")
  
)

#ui <- secure_app(ui)