server <- function(input, output, session) {
  observe(input$alive_count)
  session$allowReconnect("force")
  
  
#  res_auth <- secure_server(
#    check_credentials = check_credentials(credentials)
#  )
#  
#  output$auth_output <- renderPrint({
#    reactiveValuesToList(res_auth)
#  })
#  
  
  # dynamic dropdown
  site_type <- reactive({
    req(input$site_type)
    filter(vax, if(input$site_type != 'Overall')  (site_type == input$site_type) else TRUE)
  })
  
  observeEvent(site_type(), {
    choices <- unique(site_type()$main_site)
    updateSelectInput(session, inputId = "locations", choices = c("None", choices), selected = "None") 
  })
  
  
  # dynamic checkbox
  peh <- reactive({
    req(input$site_type, input$site_type == "UHRC")
  })
  
  observeEvent(peh(), {
   output$check_peh <- renderUI({
     checkboxInput("checkbox", label = "PEH only", value = FALSE)
   })
     })
  
  

  # create reactive dataframe
  df <- reactive({
    if (input$site_type != "UHRC") {
    vax %>%
    mutate(appointment_date = as.Date(appointment_booked_time)) %>%
    mutate(apptdate = as.Date(apptdate)) %>%
    filter(if(input$site_type != 'Overall')  (site_type == input$site_type) else TRUE) %>%
    filter(appointment_date >= input$dates[1] & appointment_date <= input$dates[2]) %>%
    filter(apptdate >= input$dates[1] & apptdate <= input$dates[2]) %>%
    filter(if(input$locations != "None") (main_site == input$locations) else TRUE) 
    } else {
      vax %>%
        mutate(appointment_date = as.Date(appointment_booked_time)) %>%
        filter(appointment_date >= input$dates[1] & appointment_date <= input$dates[2]) %>%
        filter(if(!isTRUE(input$checkbox)) (peh == TRUE | main_site == "LA UHRC") else (peh == TRUE))
    }
  })
  
  # summary table by race
  summary_race <- reactive({
    df() %>%
    filter(shot_number== "First Shot") %>%
    filter(appt_status == "Discharged") %>%
    filter(!is.na(race_ethnicity)) %>%
    filter(race_ethnicity != "Not Indicated") %>%
    group_by(race_ethnicity) %>%
    summarise(total = n(),
              percent = round((total/nrow(full_race_ethnicity()))*100, 1)) %>%
    left_join(demo)
  })
  
  # summary table by age
  summary_age <-  reactive({
    df() %>%
    filter(shot_number== "First Shot") %>%
    filter(appt_status == "Discharged") %>%
    filter(!is.na(agegroup)) %>%
    group_by(agegroup) %>%
    summarise(total = n()) %>%
    mutate(percent = round((total/sum(total))*100,2)) %>%
    select(!total)
  })
  # output summary age table
  output$age_table <- renderDT(summary_age(), 
                               colnames = c('Age Group', 'Percent'), 
                               rownames = FALSE,
                               options=list( info = FALSE, paging = F, searching = F, columnDefs = list(list(className = 'dt-left', targets = "_all"))))
  
  # summary table by gender
  summary_gender <-  reactive({
    df() %>%
      filter(shot_number== "First Shot") %>%
      filter(appt_status == "Discharged") %>%
      filter(!is.na(patient_gender)) %>%
      group_by(patient_gender) %>%
      summarise(total = n()) %>%
      mutate(percent = round((total/sum(total))*100,2)) %>%
      select(!total)
  })
  
  # combine all demographics
  summary_demo <- reactive({
    bind_rows({summary_race() %>% select(race_ethnicity, percent) %>% rename(demo = race_ethnicity)}, 
              {summary_age() %>% rename(demo = agegroup)}, 
              {summary_gender() %>% rename(demo = patient_gender)})
    })
  
  
  # download demographics data
  output$demo_dl<- downloadHandler(
    filename = function() {
      paste("demographics_vaccine_breakdown_", input$dates[1] ,"_to_", input$dates[2], "_", input$locations, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summary_demo(), file, row.names = FALSE)
    })
  
  # output summary gender table
  output$gender_table <- renderDT(summary_gender(), 
                                  rownames = FALSE,
                                  colnames = c('Gender', 'Percent'),
                                  options=list( info = FALSE, paging = F, searching = F, columnDefs = list(list(className = 'dt-left', targets = "_all"))))
  
  # create race table
  race <- reactive({
    df() %>%
    filter(!is.na(race_ethnicity)) %>%
    dcast(appointment_booked_time ~ race_ethnicity) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))  %>%
    mutate(appointments = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
    #mutate(across(where(is.numeric), ~round((./total_tests)*100,2)))
    mutate(across(where(is.numeric), ~round((./appointments)*100,2), .names= 'percent_{col}' )) %>%
    mutate(Date = as.Date(appointment_booked_time),
             Hour = hour(appointment_booked_time)) %>%
    select(Date, Hour, contains("percent"), appointments) %>%
    select(!percent_appointments)
  }) 
  # output summary race table
  output$race_table <- renderDT(race(),
                                colnames = c('Appointment Booked Date', 'Hour', '% AIAN', '% Asian', '% Black', '% Hispanic_Latino', '% Other', '% White', 'Total Appointments' ), 
                                rownames = FALSE,
                                options=list(columnDefs = list(list(className = 'dt-left', targets = "_all")))
                                )
  
  # download race data
  output$race_dl<- downloadHandler(
    filename = function() {
      paste("race_ethnicity_vaccine_breakdown_", input$dates[1] ,"_to_", input$dates[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(race(), file, row.names = FALSE)
    })
  

  # render demographics bar chart
  output$demo <- renderPlotly({
   if(input$site_type == "Overall") {
     plot_ly(data = summary_race()) %>%
       add_trace(
         type='bar',
         x=~race_ethnicity,
         y=~percent_pop,
         name = "Percent of LA City Population",
         width = 0.5,
         marker=list(color="#eeeeee"),
         hovertemplate = paste('%{y}% of LA City Population'),
         showlegend = T) %>% add_trace(
           type='bar',
           x=~race_ethnicity,
           y=~percent,
           width = 0.1, 
           name = "Percent of Vaccines",
           marker=list(color='#0b5394'),
           hovertext=~total,
           hovertemplate = paste('%{y}% of Vaccines<br>Total: %{hovertext}'),
           showlegend = T) %>% layout(
             barmode="overlay",
             bargap=0.1,
             yaxis =list(title="Percent"),
             xaxis = list(title = "Race/Ethnicity"),
             legend = list(orientation = 'h', y = -0.04, x = 0.25),
             font = list(size=10)
           )
   } else {
     plot_ly(data = summary_race()) %>%
     add_trace(
           type='bar',
           x=~race_ethnicity,
           y=~percent,
           width = 0.1, 
           name = "Percent of Vaccines",
           marker=list(color='#0b5394'),
           hovertext=~total,
           hovertemplate = paste('%{y}% of Vaccines<br>Total: %{hovertext}'),
           showlegend = T) %>% layout(
             barmode="overlay",
             bargap=0.1,
             yaxis =list(title="Percent"),
             xaxis = list(title = "Race/Ethnicity"),
             legend = list(orientation = 'h', y = -0.04, x = 0.25),
             font = list(size=10)
           )
   }
  })
  
  # constant values to use
  full_race_ethnicity <- reactive({
    df() %>%
      filter(shot_number== "First Shot") %>%
      filter(appt_status == "Discharged") %>%
      filter(!is.na(race_ethnicity)) %>%
      filter(race_ethnicity != "Not Indicated")
  }) 
  
  na_zip <- reactive({
    df() %>%
      filter(shot_number== "First Shot") %>%
      filter(appt_status == "Discharged") %>%
      filter(is.na(patient_zipcode)) 
  }) 
  
  output$total_booked <- renderText(
    formatC(nrow(df()), format="d", big.mark=',')
  )
 
  output$total_completed <- renderText(
   formatC(
   nrow(df()%>% filter(appt_status == "Discharged")), format="d", big.mark=','))
  
  output$total_px <- renderText(
    formatC(length(unique(df()$patient_mrn)), format="d", big.mark=','))

  output$median_age <- renderText(median(df()$patient_age, na.rm = T))
  

 
  # print mising values
  output$missing_race <- renderText(
    paste(round((1-(nrow(full_race_ethnicity())/nrow(df())))*100), 
          "% of first appointment data is missing race/ethnicity info.")
    )
  
  output$missing_zip <- renderText(
    paste(round((nrow(na_zip())/nrow(df()))*100), 
          "% of data is missing zip code info.")
  )
  
  

  # summary table by hour
  summary_hour <- reactive({
    df() %>%
      filter(shot_number== "First Shot") %>%
      group_by(appointment_booked_time) %>%
      summarise(total = n())
  })
  
  #  hourly chart
  output$bar_chart <- renderPlotly({
      plot_ly(data = summary_hour(), type = "bar", x = ~appointment_booked_time, y = ~total, 
          text = ~total,
          #text = ~paste("Date": appointment_booked_time, "\n",
              #          "Appointments": total),
          name = "",
          hovertemplate = paste('%{x}', '<br>Total: %{text:.2s}<br>'),
          texttemplate = '%{y:.2s}', textposition = 'outside'
          )   %>%
      layout(
            xaxis = list(
              type = 'date',
              tickformat = "%b %d %Y %H",
              title = "Date"
            ),
            yaxis = list(
              title = "Appointments Booked"
            ))
    })
  
  # output$non_reactive_map <- renderLeaflet({
  #     layerd_map %>% 
  #     tmap_leaflet() %>%
  #     leaflet::showGroup("Vaccination Coverage") %>%
  #     leaflet::hideGroup("Adjusted Case Rates")
  #   })
  
  # output$bivar_map <- renderLeaflet({
  #   risk_vax_map %>% 
  #     tmap_leaflet()})

  
##################################[REACTIVE MAPPING]############################################
  # Pre-Processing
  
  zcta_vaccine <-reactive({
                df() %>%
                 filter(shot_number== "First Shot") %>%
                 filter(!is.na(patient_zipcode)) %>%
                 group_by(patient_zipcode) %>%
                 summarise(total = n()) %>%
                 mutate(vax_percentage = round((total/sum(total))*100,2)) 
      
    })
  
  summary_zcta <- reactive({
    merge(features, zcta_vaccine(),by.x="zipcode",by.y='patient_zipcode',all.x=TRUE) %>% 
      mutate(pop_vax = round((total/pop)*100,2)) %>%
      #vax_percentage = round((total/total_vax)*100,2)) %>%
      clean_names()
  })
  
  
  # Updates and Renders
  
 zcta_popup = c("Zipcode"="zipcode", 
               "Total Population" = "pop",
                "Total People Vaccinated" = "total",
                "% of Population Vaccinated" = "pop_vax",
                "% of Total Vaccinations" = "vax_percentage")
output$reactive_map <- renderLeaflet({
zcta_map <- tm_basemap(leaflet::providers$CartoDB.Positron) +
     tm_shape(summary_zcta())+
      tm_view(view.legend.position=c("top","right"))+
      tm_polygons(col='vax_percentage', palette = "Blues",alpha=.5, border.col="#174A85",border.alpha=.5, lwd=1,
                  style = "pretty",
                  textNA = "No Available Data",title="Percent of Total Vaccinations",
                  id='zipcode',popup.vars=zcta_popup,group="Vaccination Map")
    (zcta_map + test_pos_map + outline) %>% tmap_leaflet() %>%
      leaflet::showGroup("Percent of Total Vaccinations") %>%
      leaflet::hideGroup("Test Positivity Percentage") %>%
      leaflet::hideGroup("LA City Outline")
 })
 
  

  # summary zip table
  zips <- reactive({ summary_zcta() %>%
    mutate(zipcode = as.numeric(zipcode)) %>%
    #filter(shot_number== "First Shot") %>%
    #filter(!is.na(patient_zipcode)) %>%
    #group_by(patient_zipcode) %>%
    #summarise(appointments = n()) %>%
    #mutate(percent = round((appointments/sum(appointments))*100,2)) %>%
    select(zipcode, vax_percentage, total, pop_vax, pop, zip_hpi_quartile) %>%
    arrange(desc(vax_percentage))
  })
  
  # display zip table
  output$zip_table <- renderDT(zips(), #%>% st_drop_geometry()
                               colnames = c('Zip Code', '% of Appointments', 'Total Appointments', '% of Population Vaccinated', 'Population', 'HPI Quartile'),
                               rownames = FALSE,
                               options=list(columnDefs = list(list(className = 'dt-left', targets = "_all"))))
  
  # download zip data
  output$zips_dl<- downloadHandler(
    filename = function() {
      paste("zipcode_vaccine_breakdown_", input$dates[1] ,"_to_", input$dates[2], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(zips(), file, row.names = FALSE)
    })

  # zip code LA City v County analysis
  city_vax <- reactive({
    left_join(zips(), la, by.x = "zipcode", by.y="patient_zipcode")
  })
  
  county_vax <- reactive({
    full_join(city_vax(), county, by.x = "zipcode", by.y="patient_zipcode")
  })
  
  # create output variables
  la_zips <- reactive({
    county_vax() %>% filter(city == "City") %>% 
      summarize(percent = sum(vax_percentage, na.rm = T))
  })
  
  output$perc_la <- renderText({
    paste0(round(la_zips()[[1]],1), "%")
  })
  
  county_zips <- reactive({
    county_vax() %>% filter(county == "County") %>% 
      filter(is.na(city)) %>%
      summarize(percent = sum(vax_percentage, na.rm=T))
  })
  
  output$perc_county <- renderText({
    paste0(round(county_zips()[[1]],1), "%")
  })
    

  
### Images
  output$legend <- renderImage({
    list(
      src = file.path("www/temp_legend.png"),
      contentType = "image/png",
      width = 150,
      height = 150
    )
  }, deleteFile = FALSE)
  

output$logo <- renderImage({
  list(
    src = file.path("www/logo.png"),
    contentType = "image/png",
    width = 100,
    height = 100
  )
}, deleteFile = FALSE)



###### weekly stuff




output$race_lines <- renderPlotly({
  plot_ly(data = weekly_race, x= ~date, y = ~AIAN, mode = "line", type = "scatter", name = "AIAN", line=list(color="#99bde1")) %>%
  add_trace(y = ~Asian, name = "Asian", line=list(color="#144fbe")) %>%
  add_trace(y = ~Black, name = "Black", line=list(color="#fcd36c")) %>%
  add_trace(y = ~Hispanic_Latino, name = "Hispanic or Latino", line=list(color="#63213d")) %>%
  add_trace(y = ~White, name = "White", line=list(color="#66c092"))  %>%
  add_trace(y = ~Other, name = "Other", line=list(color="#636363")) %>%
  layout(yaxis= list(title= "# of Appointments Booked"),
         xaxis = list(title= "Date"))
})



output$brand_lines <- renderPlotly({
  plot_ly(data = weekly_brands, x= ~date, y = ~Pfizer, mode = "line", type = "scatter", name = "Pfizer", line=list(color="#99bde1")) %>%
    add_trace(y = ~Moderna, name = "Moderna", line=list(color="#66c092")) %>%
    add_trace(y = ~Janssen, name = "Janssen", line=list(color="#63213d")) %>%
    layout(yaxis= list(title= "# of Appointments Booked"),
           xaxis = list(title= "Date"))
})



output$hpi_lines <- renderPlotly({
  plot_ly(data = weekly_hpi, x= ~date, y = ~HPI_1, mode = "line", type = "scatter", name = "HPI 1", line=list(color="#99bde1")) %>%
    add_trace(y = ~HPI_2, name = "HPI 2", line=list(color="#66c092")) %>%
    add_trace(y = ~HPI_3, name = "HPI 3", line=list(color="#63213d")) %>%
    add_trace(y = ~HPI_4, name = "HPI 4", line=list(color="#144fbe")) %>%
    layout(yaxis= list(title= "# of Appointments Booked"),
           xaxis = list(title= "Date"))
})



### Time series
daily_shots <- reactive({
  df() %>%
  filter(!is.na(brand)) %>%
  filter(apptdate > "2020-12-01") %>%
  dcast(apptdate ~main_site +  brand + shot_number , value.var="apptdate")  %>%
  mutate(across(where(is.character), ~cumsum(.x))) %>%
  arrange(apptdate)
})


# output summary race table
output$daily_table <- renderDT(daily_shots(),
                              rownames = FALSE,
                              options=list(columnDefs = list(list(className = 'dt-left', targets = "_all")))
)

# download race data
output$daily_dl<- downloadHandler(
  filename = function() {
    paste("daily_vaccine_breakdown_", input$dates[1] ,"_to_", input$dates[2], ".csv", sep = "")
  },
  content = function(file) {
    write.csv(daily_shots(), file, row.names = FALSE)
  })

}

