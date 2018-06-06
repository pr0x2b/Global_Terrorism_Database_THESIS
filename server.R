
shinyServer(function(input, output, session) {

  # Collapse sidebar by default
  # addClass(selector = "body", class = "sidebar-collapse")

  #-------------------------------------
  # globe chart (About page) 
  #-------------------------------------
  output$world_hchart <- renderHighchart({
    
    data(worldgeojson)
    readRDS("countries.rds")
    dshmstops <- data.frame(q = c(0, exp(1:7)/exp(7)), c = substring(viridis(7 + 1, option = "D", direction = -1), 0, 7)) %>%  list_parse2()
    
    highchart() %>% 
      hc_add_series_map(worldgeojson, countries, value = "total", joinBy = "iso3") %>% 
      hc_colorAxis(stops = dshmstops) %>% 
      hc_legend(enabled = TRUE) %>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text = "Terrorist Attacks Around the World between 1970-2016") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_credits(enabled = TRUE, 
                 text = "Sources: Global Terrorism Database (START Consortium)",
                 style = list(fontSize = "12px"))
    
  })

  #-------------------------------------
  # Data for sidebar selection
  #-------------------------------------

  sidebar_data <- reactive({

      req(input$radioBtn_ldata)

      if(input$radioBtn_ldata == "T10 Groups") {
        sidebar_data <- df_leaflet_t10 # filter data for top 10 deadliest groups
      } else {
        sidebar_data <- df_leaflet # select everything
      }

      return(sidebar_data)

    })


  #-----------------------------------------------
  # EDA p1 leaflet
  #------------------------------------------------
    # reactive data for leaflet
    leaflet_plot_data <- reactive({

      data <- sidebar_data()
      data <- data[data$year >= input$leaflet_year[1] & data$year <= input$leaflet_year[2], ]
      data <- data[data$region %in% input$leaflet_region, ]
      data <- data[data$group_name %in% input$leaflet_group, ]
      data <- data[data$attack_type %in% input$leaflet_attack, ]
      data <- data[data$target_type %in% input$leaflet_target, ]
      data <- data[data$weapon_type %in% input$leaflet_weapon, ]
      data <- data[data$suicide_attack %in% input$checkBtn1_suicide, ]
      data <- data[data$part_of_multiple_attacks %in% input$checkBtn2_multiple, ]
      data <- data[data$intl_ideological_attack %in% input$checkBtn3_log_int, ]
      data <- data[data$intl_ideological_attack %in% input$checkBtn4_ido_int, ]
      data <- data[data$crit1_pol_eco_rel_soc %in% input$checkBtn5_crit1, ]
      data <- data[data$crit2_publicize %in% input$checkBtn6_crit2, ]
      data <- data[data$crit3_os_intl_hmn_law %in% input$checkBtn7_crit3, ]
      data$reg_lng <- ifelse(length(unique(data$region)) < 10, mean(data$longitude), 0 )
      data$reg_lat <- ifelse(length(unique(data$region)) < 10, mean(data$latitude), 0 )

      return(data)
    })

    # reactive data for leaflet + major attacks
    major_attacks <- reactive({

      # to quanify actual nkills
      data <- leaflet_plot_data() %>% 
          replace_na(list(nkill = 0, nwound = 0)) %>% 
          filter(nkill > 50) %>%
          select(group_name, region, year, month, nkill, part_of_multiple_attacks, longitude, latitude, intl_logistical_attack, intl_ideological_attack,
                 date, country, city, nwound, attack_type, target_type, weapon_type, extended, attack_success, suicide_attack, part_of_multiple_attacks) %>% 
          group_by(group_name, region, year, month) %>% 
          filter(if_else(part_of_multiple_attacks == 1, nkill == max(nkill) & nwound == max(nwound), nkill == nkill & nwound == nwound)) %>%
          distinct(group_name, region, year, month, nkill, part_of_multiple_attacks, longitude, latitude, intl_logistical_attack, intl_ideological_attack,
                 date, country, city, nwound, attack_type, target_type, weapon_type, extended, attack_success, suicide_attack, part_of_multiple_attacks)

      return(data)
    })

    #-------------------------------------
    # EDA p1 (Leaflet sidebar menu)
    #-------------------------------------

    output$radioBtn_ldata <- renderUI ({
          radioButtons(inputId = "radioBtn_ldata", label = "Select data :", choices = c("T10 Groups", "All"), selected = "T10 Groups", inline = TRUE)
      })

    output$radioBtn_major_attacks <- renderUI ({
          radioGroupButtons(inputId = "radioBtn_major_attacks", label = "Major attacks only?", c("Yes", "No"), selected = "Yes",
                            size = "xs", justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))
      })

    output$leaflet_year <- renderUI({ 
          sliderInput("leaflet_year", label = "Year range", min = 1970, max = 2016, value = c("2010", "2016"))
        })

    output$leaflet_region <- renderUI({  
   
          pickerInput(inputId = "leaflet_region", label = "Region", choices = levels(factor(sidebar_data()$region)), 
            selected = levels(factor(sidebar_data()$region)), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
        })

    output$leaflet_group <- renderUI({   
  
          pickerInput(inputId = "leaflet_group", label = "Terrorist group", choices = levels(factor(sidebar_data()$group_name)), 
            selected = levels(factor(sidebar_data()$group_name)), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
        })

    output$leaflet_attack <- renderUI({  

          pickerInput(inputId = "leaflet_attack", label = "Attack type", choices = levels(factor(sidebar_data()$attack_type)), 
            selected = levels(factor(sidebar_data()$attack_type)), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
        })

    output$leaflet_target <- renderUI({   

          pickerInput(inputId = "leaflet_target", label = "Target type", choices = levels(factor(sidebar_data()$target_type)), 
            selected = levels(factor(sidebar_data()$target_type)), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
        })

    output$leaflet_weapon <- renderUI({   

          pickerInput(inputId = "leaflet_weapon", label = "Weapon type", choices = levels(factor(sidebar_data()$weapon_type)), 
            selected = levels(factor(sidebar_data()$weapon_type)), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
        })

    output$checkBtn1_suicide <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn1_suicide", label = "Suicide attack? ", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn2_multiple <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn2_multiple", label = "Part of multiple attacks?", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn3_log_int <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn3_log_int", label = "Logistically intl?", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn4_ido_int <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn4_ido_int", label = "Ideologically intl?", choices = c("Yes", "No", "Unknown"), selected = c("Yes", "No", "Unknown"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn5_crit1 <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn5_crit1", label = "Political/ Social goal?", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn6_crit2 <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn6_crit2", label = "Intention to coerce?", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })

    output$checkBtn7_crit3 <- renderUI({ 
          checkboxGroupButtons(inputId = "checkBtn7_crit3", label = "O/s Geneva Conventions?", choices = c("Yes", "No"), selected = c("Yes", "No"),
                               size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
        })


  # Dynamically update pickerInput value
  # observeEvent(input$leaflet_region, {
  #   data <- leaflet_plot_data()
  #   updatePickerInput(session = session, inputId = "leaflet_region", choices = unique(data$region))
  # }, ignoreInit = TRUE)

  # observeEvent(input$leaflet_group, {
  #   region <- input$leaflet_region
  #   data <- leaflet_plot_data()
  #   updatePickerInput(session = session, inputId = "leaflet_group", choices = unique(data$group_name), selected = unique(data$group_name))
  # })

  # observeEvent(input$leaflet_attack, {
  #   data <- leaflet_plot_data()
  #   updatePickerInput(session = session, inputId = "leaflet_attack", choices = unique(data$attack_type))
  # }, ignoreInit = TRUE)

  # observeEvent(input$leaflet_target, {
  #   data <- leaflet_plot_data()
  #   updatePickerInput(session = session, inputId = "leaflet_target", choices = unique(data$target_type))
  # }, ignoreInit = TRUE)

  # observeEvent(input$leaflet_weapon, {
  #   data <- leaflet_plot_data()
  #   updatePickerInput(session = session, inputId = "leaflet_weapon", choices = unique(data$weapon_type))
  # }, ignoreInit = TRUE)

  #-------------------------------------
  # EDA p2 (Leaflet plot output)
  #-------------------------------------
  output$top_10_dg_leaflet <- renderLeaflet({

    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3, input$radioBtn_major_attacks)

    data <- leaflet_plot_data()
    default_lng <- data$reg_lng[1]
    default_lat <- data$reg_lat[1]
    default_zoom <- ifelse(default_lng == 0 & default_lng == 0, 2, 3)


    leaf_plot <- leaflet() %>%
        setView(lng = default_lng, lat = default_lat, zoom= default_zoom) %>%
        addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>,
          <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>
          &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        addScaleBar() %>%
        addProviderTiles(providers$CartoDB.DarkMatter, group = "Black") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Standard") %>%
        addLayersControl(baseGroups = c("Black", "Standard"), options = layersControlOptions(collapsed = FALSE)) 
        # %>%
        # addFullscreenControl()


        if(input$radioBtn_major_attacks == "No") {

          leaf_plot %>%
            addCircles(data = data, lat= ~latitude, lng = ~longitude, 
                  popup=paste(
                    "<strong>Date: </strong>", data$date,
                    "<br><strong>Group: </strong>", data$group_name,
                    "<br><strong>Region : </strong>", data$region,
                    "<br><strong>Country : </strong>", data$country,
                    "<br><strong>City : </strong>", data$city,
                    "<br><strong>Number of people killed : </strong>", data$nkill,
                    "<br><strong>Number of people wounded : </strong>", data$nwound,
                    "<br><strong>Attack type: </strong>", data$attack_type,
                    "<br><strong>Weapon type: </strong>", data$weapon_type,
                    "<br><strong>Target type: </strong>", data$target_type,
                    "<br><strong>Attack > 24 hours?: </strong>", data$extended,
                    "<br><strong>Suicide attack?: </strong>", data$suicide_attack,
                    "<br><strong>Part of multiple attacks?: </strong>", data$part_of_multiple_attacks),
                  weight = 1, 
                  radius = ~(nkill + 1), 
                  color= ifelse(data$nkill == 0, "#8bd354", "#8B1A1A"),
                  stroke = FALSE, 
                  fillOpacity = 0.7)

        } else {

          data <- major_attacks()

          leaf_plot %>%
            addPulseMarkers(lng= data$longitude, lat= data$latitude,
                            label = ifelse(data$nkill < 100, "50-100 people killed", 
                                    ifelse(data$nkill >= 100 & data$nkill < 300, "100-300 people killed", 
                                    ifelse(data$nkill >= 300 & data$nkill < 500, "300-500 people killed", 
                                    ifelse(data$nkill >= 500 & data$nkill < 1000, "500-1000 people killed", "Major attack: > 1000 people killed")))),
                            popup=paste( 
                              "<br><strong>Date: </strong>", data$date,
                              "<br><strong>Group: </strong>", data$group_name,
                              "<br><strong>Region : </strong>", data$region,
                              "<br><strong>Country : </strong>", data$country,
                              "<br><strong>City : </strong>", data$city,
                              "<br><strong>Number of people killed : </strong>", data$nkill,
                              "<br><strong>Number of people wounded : </strong>", data$nwound,
                              "<br><strong>Attack type: </strong>", data$attack_type,
                              "<br><strong>Weapon type: </strong>", data$weapon_type,
                              "<br><strong>Target type: </strong>", data$target_type,
                              "<br><strong>Attack > 24 hours?: </strong>", data$extended,
                              "<br><strong>Suicide attack?: </strong>", data$suicide_attack,
                              "<br><strong>Part of multiple attacks?: </strong>", data$part_of_multiple_attacks),
                            icon = makePulseIcon(
                              color = ifelse(data$nkill < 100, "#8bd354", 
                                      ifelse(data$nkill >= 100 & data$nkill < 300, "#ffbf00", 
                                      ifelse(data$nkill >= 300 & data$nkill < 500, "#FF8000", 
                                      ifelse(data$nkill >= 500 & data$nkill < 1000, "#FF4000", "#ff0000")))),
                              iconSize = ifelse(data$nkill < 100, 2, 
                                         ifelse(data$nkill >= 100 & data$nkill < 300, 5, 
                                         ifelse(data$nkill >= 300 & data$nkill < 500, 9, 
                                         ifelse(data$nkill >= 500 & data$nkill < 1000, 13, 25)))),
                              animate = TRUE, 
                              heartbeat = ifelse(data$nkill < 100, 0.99, 
                                          ifelse(data$nkill >= 100 & data$nkill < 300, 0.90, 
                                          ifelse(data$nkill >= 300 & data$nkill < 500, 0.80, 
                                          ifelse(data$nkill >= 500 & data$nkill < 1000, 0.70, 0.5))))
                              ))
            
        }


    # addMarkers(data = data, lat= ~latitude, lng = ~longitude, 
    #             popup=paste(
    #               "<strong>Date: </strong>", data$date,
    #               "<br><strong>Group: </strong>", data$group_name,
    #               "<br><strong>Region : </strong>", data$region,
    #               "<br><strong>Country : </strong>", data$country,
    #               "<br><strong>City : </strong>", data$city,
    #               "<br><strong>Number of people killed : </strong>", data$nkill,
    #               "<br><strong>Number of people wounded : </strong>", data$nwound,
    #               "<br><strong>Attack type: </strong>", data$attack_type,
    #               "<br><strong>Weapon type: </strong>", data$weapon_type,
    #               "<br><strong>Target type: </strong>", data$target_type,
    #               "<br><strong>Attack > 24 hours?: </strong>", data$extended,
    #               "<br><strong>Suicide attack?: </strong>", data$suicide_attack,
    #               "<br><strong>Part of multiple attacks?: </strong>", data$part_of_multiple_attacks),
    #             clusterOptions = markerClusterOptions()) 



    # addEasyButton(easyButton(
    #     icon="fa-globe", title="Zoom to Level 2",
    #     onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
    #   addEasyButton(easyButton(
    #     icon="fa-crosshairs", title="My location",
    #     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  
    
  })

  #-------------------------------------
  # Value boxes (GUI leaflet) 
  #-------------------------------------

   output$countries_affected <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }

    valueBox(data %>% group_by(country) %>% summarise(count = n()) %>% nrow(), "Nations affected", icon = icon("globe"), color = 'red') 
    })

   output$suicide_attack <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }

    data <- data %>% group_by(suicide_attack) %>% summarise(total = n())
    val_suc <- round(data %>% filter(suicide_attack == "Yes") %>% select(total) / sum(data$total) * 100, 1)
    val_suc <- ifelse(is.null(val_suc), paste0("Nill", "%"), paste0(val_suc, "%"))
    valueBox(val_suc, "Suicide attacks", icon = icon("bomb"), color = 'orange') 
    })

   output$attack_success <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }

    data <- data %>% group_by(attack_success) %>% summarise(total = n())
    val_attack_success <- round(data %>% filter(attack_success == "Yes") %>% select(total) / sum(data$total) * 100, 1)
    val_attack_success <- ifelse(is.null(val_attack_success), paste0("Nill", "%"), paste0(val_attack_success, "%"))
    valueBox(val_attack_success, "Attack success", icon = icon("check"), color = 'blue') 
    })

   output$extended <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }
    data <- data %>% group_by(extended) %>% summarise(total = n())
    val_extended <- round(data %>% filter(extended == "Yes") %>% select(total) / sum(data$total) * 100, 1)
    val_extended <- ifelse(is.null(val_extended), paste0("Nill", "%"), paste0(val_extended, "%"))
    valueBox(val_extended, "Attacks > 24 hrs", icon = icon("glyphicon glyphicon-hourglass"), color = 'blue') 
    })

   output$attack_log_intl <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }

    data <- data %>% group_by(intl_logistical_attack) %>% summarise(total = n())
    val_lia1 <- round(data %>% filter(intl_logistical_attack == "Yes") %>% select(total) / sum(data$total) * 100, 1)
    val_lia1 <- ifelse(is.null(val_lia1), paste0("Nill", "%"), paste0(val_lia1, "%"))
    valueBox(val_lia1, "Logistically intl",icon = icon("globe"), color = 'olive') 
    })
  
   output$attack_ideo_intl <- renderValueBox({
    
    req(input$radioBtn_ldata, input$leaflet_year, input$leaflet_region, input$leaflet_group, input$leaflet_attack, input$leaflet_target, input$leaflet_weapon, 
        input$checkBtn1_suicide, input$checkBtn2_multiple, input$checkBtn3_log_int, input$checkBtn4_ido_int, input$checkBtn5_crit1, input$checkBtn6_crit2, 
        input$checkBtn7_crit3)

    if(input$radioBtn_major_attacks == "Yes") { data <- major_attacks() } else { data <- leaflet_plot_data() }

    data <- data %>% group_by(intl_ideological_attack) %>% summarise(total = n())
    val_id1 <- round(data %>% filter(intl_ideological_attack == "Yes") %>% select(total) / sum(data$total) * 100, 1)
    val_id1 <- ifelse(is.null(val_id1), paste0("Nill", "%"), paste0(val_id1, "%"))
    valueBox(val_id1, "Ideologically intl",icon = icon("globe"), color = 'olive') 
    })


    #-------------------------------------
    # Animations tab
    #-------------------------------------
    # output$animation_1 <- renderPlotly({
    #   
    # df %>% filter(nkill > 50) %>%
    #   plot_ly(x = ~attack_type, y = ~nkill, color = ~target_type,
    #           frame = ~year, hoverinfo = 'text', 
    #           text = ~paste('Year:', year,
    #                         '<br>Number of people killed:', nkill,
    #                         '<br>Target type:', target_type,
    #                         '<br>Attack type:', attack_type, 
    #                         '<br>Weapon type:', weapon_type,
    #                         '<br>Country:', country),
    #           type = 'scatter', mode = 'markers') %>% 
    #   animation_opts(frame = 2000) %>% 
    #   animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="#104e8b"))) %>%
    #   layout(title = "Terroris attacks by year, casualities (>50), attack and target type")
    # 
    # 
    #   })


  #-----------------------------------------------
  # Section 1.2: Global Attack Patterns/ Heatmaps
  #-----------------------------------------------

  output$pattern_global_hmap1 <- renderPlotly({
    tmp <- df_leaflet %>% group_by(target_type, year) %>% summarise(total_attacks = n()) 
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$target_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target type: ", target_type,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Target Type", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$pattern_global_hmap2 <- renderPlotly({
    tmp <- df_leaflet %>% group_by(attack_type, year) %>% summarise(total_attacks = n()) 
    tmp %>%
    plot_ly(x=tmp$year, y=tmp$attack_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
            hoverinfo = 'text',text = ~paste("Year: ", year,
                                             "<br>Attack type: ", attack_type,
                                             "<br>Total attacks : ", total_attacks)) %>%
      layout(title = "By Attack Type",
             xaxis = list(autorange = "reversed"), 
             yaxis = list(autorange = "reversed"), 
             paper_bgcolor= "black", plot_bgcolor = "black")

    })


  output$pattern_global_hmap3 <- renderPlotly({
    tmp <- df_leaflet %>% group_by(weapon_type, year) %>% summarise(total_attacks = n())
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$weapon_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Weapon type: ", weapon_type,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Weapon Type", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$slider_filter_year <- renderUI({ 
        sliderInput("slider_filter_year", label = "Year range", min = 1970, max = 2016, value = c("2010", "2016"))
    })

  output$slider_filter_total_attacks <- renderUI({ 

      sliderInput("slider_filter_total_attacks", label = "Total number of attacks", min = 0, max = 1500, , value = c("0", "1500"))

    })

  output$radioBtn_filter_tgroup <- renderUI ({
        radioGroupButtons(inputId = "radioBtn_filter_tgroup", label = "Exclude Unknown Groups?", c("Yes", "No"), selected = "Yes",
                          checkIcon = list(yes = icon("ok", lib = "glyphicon")))
    })

  # reactive data for leaflet + major attacks
  df_tgroup_heatmap <- reactive({
    req(input$slider_filter_year, input$slider_filter_total_attacks, input$radioBtn_filter_tgroup)

    data <- df_leaflet[df_leaflet$year >= input$slider_filter_year[1] & df_leaflet$year <= input$slider_filter_year[2], ]

    if(input$radioBtn_filter_tgroup == "Yes") { 
      data <- data %>% filter(group_name != "Unknown") %>% group_by(group_name, year) %>% summarise(total_attacks = n()) 
    } else { 
      data <- data %>% group_by(group_name, year) %>% summarise(total_attacks = n()) 
    }

    data <- data[data$total_attacks >= input$slider_filter_total_attacks[1] & data$total_attacks <= input$slider_filter_total_attacks[2], ]

    return(data)

  })

  observe({

    data <- df_tgroup_heatmap()
    min_attacks <- min(data$total_attacks)
    max_attacks <- max(data$total_attacks)
    updateSliderInput(session, "slider_filter_total_attacks", min = 0, max = 1500, value = c(min_attacks, max_attacks))
    
  })

  output$pattern_global_hmap4 <- renderPlotly({
    tmp <- df_tgroup_heatmap()
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$group_name, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Group: ", group_name,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Terrorist Groups", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$pattern_global_hmap_countries <- renderPlotly({
    tmp <- df_leaflet %>% group_by(country, year) %>% summarise(total_attacks = n())
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$country, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target Countries: ", country,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Country", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")
       
    })


  output$pattern_global_hmap_tnats <- renderPlotly({
    tmp <- df_leaflet %>% group_by(target_nalty, year) %>% summarise(total_attacks = n())
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$target_nalty, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target Nationality: ", target_nalty,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Target Nationality<br><br><br>", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")
       
    })


  #-------------------------------------
  # EDA p2: Deadlist group top10_hc1
  #-------------------------------------

    # by_groups <- df %>% group_by(group_name) %>% summarise(total = n()) %>% filter(group_name != "Unknown") %>% arrange(desc(total)) %>% head(10)
    # top10_groups <- as.vector(by_groups$group_name)
    group_weapon_type <- df_leaflet_t10 %>% group_by(weapon_type) %>% summarize(count = n()) %>% arrange(desc(count))
    group_target_country <- df_leaflet_t10 %>% group_by(country) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)


  output$top10_hc1 <- renderHighchart({
    
    highchart() %>% 
      hc_title(text = "Top 10 Deadliest Terrorist Groups") %>%
      hc_subtitle(text = "By total number of fatalities (nkill + nwound) and from year 2010 onward") %>%
      hc_add_theme(hc_theme_sandsignika()) %>%
      hc_add_series_labels_values(by_groups$group_name, by_groups$total, name = "Show/ hide bar chart", showInLegend=F,
                                  dataLabels = list(align = "center", enabled = TRUE),
                                  colors = substr(heat.colors(10), 0 , 7),
                                  colorByPoint = TRUE, type = "column") %>% 
      # hc_add_series_labels_values(group_attack_type$attack_type, group_attack_type$count, name = "Pie chart- Attack types", 
      #                             colors = substr(heat.colors(9), 0 , 7),
      #                             type = "pie", innerSize= '40%', size= "30%", showInLegend=F,
      #                             colorByPoint = TRUE, center = c('75%', '25%'),
      #                             size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
      # hc_add_series_labels_values(group_target_type$target_type, group_target_type$count, name = "Pie chart- Target types", 
      #                             colors = substr(heat.colors(10), 0 , 7),
      #                             type = "pie", innerSize= '40%', size= "25%", showInLegend=F,
      #                             colorByPoint = TRUE, center = c('85%', '20%'),
      #                             size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
      hc_yAxis(title = list(text = "Total number of fatalities (nkill + nwound)"),
               labels = list(format = "{value}"), max = 40000) %>% 
      hc_xAxis(categories = by_groups$group_name, title = list(text = "Name of the terrorist group")) %>% 
      hc_legend(enabled = T, align= "left", verticalAlign = "bottom") %>% 
      hc_tooltip(pointFormat = "{point.y}") 

    })

  #-------------------------------------
  # hc1 ~ pie chart 1, 2 (Attack + target type)
  #-------------------------------------
  output$top10_hc1_attack_type <- renderHighchart({

    group_attack_type <- df_leaflet_t10 %>% group_by(attack_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10) 
    group_target_type <- df_leaflet_t10 %>% group_by(target_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

    highchart() %>% 
      hc_title(text = "Frequent Attack Types and Target Types") %>%
      hc_add_theme(hc_theme_sandsignika()) %>%
      hc_add_series_labels_values(group_attack_type$attack_type, group_attack_type$count, 
                                  colors = substr(heat.colors(9), 0 , 7),
                                  type = "pie", innerSize= '40%', size= "50%", showInLegend=F,
                                  colorByPoint = TRUE, center = c('29%', '48%'),
                                  size = 100, dataLabels = list(align = "left", enabled = TRUE)) %>%
      hc_add_series_labels_values(group_target_type$target_type, group_target_type$count, 
                                  colors = substr(heat.colors(10), 0 , 7),
                                  type = "pie", innerSize= '40%', size= "50%", showInLegend=F,
                                  colorByPoint = TRUE, center = c('78%', '52%'),
                                  size = 100, dataLabels = list(align = "right", enabled = TRUE)) %>%
      hc_tooltip(pointFormat = "{point.y}")%>%
      hc_credits(enabled = TRUE, 
               text = "Sources: Global Terrorism Database (START Consortium)",
               style = list(fontSize = "12px"))

    })

  #-------------------------------------
  # hc1 ~ pie chart 3 (target nationality)
  #-------------------------------------
  output$top10_hc1_target_naltly <- renderHighchart({

    group_target_nalty <- df_leaflet_t10 %>% group_by(target_nalty) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

    highchart() %>% 
      hc_title(text = "Frequent Target Nationality") %>%
      hc_add_theme(hc_theme_sandsignika()) %>%
      hc_add_series_labels_values(group_target_nalty$target_nalty, group_target_nalty$count, 
                                  colors = substr(heat.colors(10), 0 , 7),
                                  type = "bar", showInLegend=F,
                                  colorByPoint = TRUE, 
                                  size = 100, dataLabels = list(align = "right", enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Total number of terror attacks"), labels = list(format = "{value}"), max = 6000) %>% 
      hc_xAxis(categories = group_target_nalty$target_nalty, title = list(text = "Target nationality")) %>%
      hc_tooltip(pointFormat = "{point.y}")

    })

  #-------------------------------------
  # hc1 ~ chart 4 (Yearwise activity)
  #-------------------------------------
  output$top10_hc1_year <- renderHighchart({

    group_year <- df_leaflet_t10 %>% group_by(year) %>% summarize(count = n())
    
    hchart(group_year, "line", hcaes(year, count)) %>%
      hc_yAxis(title = list(text = "Total attacks"), labels = list(format = "{value}"), max = 5000) %>% 
      hc_title(text = "Yearwise Activity") %>%
      hc_add_theme(hc_theme_sandsignika()) %>%
      hc_tooltip(pointFormat = "{point.y}")

    })


  #-------------------------------------
  # Analysis 2: Data description
  #-------------------------------------
  
  # output$radioBtn_ldata2 <- renderUI ({
  #       radioGroupButtons(inputId = "radioBtn_ldata2", label = "Select data :", c("T10 Groups", "All"), selected = "T10 Groups",
  #                         size = "xs", justified = TRUE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  # })

  # output$plotly_year <- renderUI({ 
  #       sliderInput("plotly_year", label = "Year range", min = 1970, max = 2016, value = c("2010", "2016"))
  #     })


  #-------------------------------------
  # Reactive data for sidebar selection
  #-------------------------------------

  # init_data <- reactive({

  #     if(input$radioBtn_ldata2 == "T10 Groups") {
  #       data <- df_leaflet_t10 %>% select(-c(eventid, day, ISO)) # filter data for top 10 deadliest groups
  #     } else {
  #       data <- df_leaflet %>% select(-c(eventid, day, ISO)) # select everything
  #     }

  #     data <- data[data$year >= input$plotly_year[1] & data$year <= input$plotly_year[2], ]

  #     return(data)

  #   })

  # output$plotly_country <- renderUI({ 
  #   data <- df_leaflet_t10 %>% select(-c(eventid, day, ISO))
  #   cont_options <- levels(factor(data$country))
  #   selectizeInput(inputId = "plotly_country", label = "Country", multiple = T, choices = cont_options, selected = cont_options[1:2])  
  # })


  # plotly_3d_data <- reactive({

  #     data <- df_leaflet_t10 %>% 
  #       select(-c(date, year, region, country, city, 
  #                 nkill, nwound, latitude, longitude,
  #                 group_name, attack_type, target_type, weapon_type, target_nalty,
  #                 suicide_attack, intl_logistical_attack, intl_ideological_attack, crit1_pol_eco_rel_soc, crit2_publicize))
  #     # data <- data[data$country %in% input$plotly_country, ]

  #     return(data)

  #   })

  output$select1_xvar <- renderUI({ 
    
    selectizeInput(inputId = "select1_xvar", label = "X var", multiple = F, 
                   choices =  list(
                                Categorical = c("group_name", "attack_type", "target_type", "weapon_type", "target_nalty"),
                                Geographical = c("region", "country", "city"),
                                Numeric = c("nkill", "nwound", "latitude", "longitude", "date", "year"),
                                Numeric_ext = c("arms_export", "arms_import", "population", "gdp_per_capita", "refugee_origin", "refugee_asylum", "net_migration", "n_peace_keepers", "conflict_index"),
                                Binary = c("suicide_attack", "intl_logistical_attack", "intl_ideological_attack", "crit1_pol_eco_rel_soc", "crit2_publicize")),
                   selected = "nwound")
  })

  output$select2_yvar <- renderUI({
    
    selectizeInput(inputId = "select2_yvar", label = "Y var", multiple = F, 
                   choices =  list(
                                Categorical = c("group_name", "attack_type", "target_type", "weapon_type", "target_nalty"),
                                Geographical = c("region", "country", "city"),
                                Numeric = c("nkill", "nwound", "latitude", "longitude", "date", "year"),
                                Numeric_ext = c("arms_export", "arms_import", "population", "gdp_per_capita", "refugee_origin", "refugee_asylum", "net_migration", "n_peace_keepers", "conflict_index"),
                                Binary = c("suicide_attack", "intl_logistical_attack", "intl_ideological_attack", "crit1_pol_eco_rel_soc", "crit2_publicize")),
                   selected = "nkill")
  })

  output$select3_zvar <- renderUI({
    
    selectizeInput(inputId = "select3_zvar", label = "Z var", multiple = F, 
                   choices =  list(
                                Categorical = c("group_name", "attack_type", "target_type", "weapon_type", "target_nalty"),
                                Geographical = c("region", "country", "city"),
                                Numeric = c("nkill", "nwound", "latitude", "longitude", "date", "year"),
                                Numeric_ext = c("arms_export", "arms_import", "population", "gdp_per_capita", "refugee_origin", "refugee_asylum", "net_migration", "n_peace_keepers", "conflict_index"),
                                Binary = c("suicide_attack", "intl_logistical_attack", "intl_ideological_attack", "crit1_pol_eco_rel_soc", "crit2_publicize")),
                   selected = "date")
  })

  output$select4_colvar <- renderUI({
    
    selectizeInput(inputId = "select4_colvar", label = "Color var", multiple = F, 
                   choices =  list(
                                Categorical = c("group_name", "attack_type", "target_type", "weapon_type", "target_nalty"),
                                Geographical = c("region", "country", "city"),
                                Numeric = c("nkill", "nwound", "latitude", "longitude", "date", "year"),
                                Numeric_ext = c("arms_export", "arms_import", "population", "gdp_per_capita", "refugee_origin", "refugee_asylum", "net_migration", "n_peace_keepers", "conflict_index"),
                                Binary = c("suicide_attack", "intl_logistical_attack", "intl_ideological_attack", "crit1_pol_eco_rel_soc", "crit2_publicize")),
                   selected = "target_type") 
  })

   output$show_legend <- renderUI ({
        radioGroupButtons(inputId = "show_legend", label = "Show legend?", c("Yes", "No"), selected = "Yes",
                          size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })

  output$radioBtn_log_tr <- renderUI ({
        radioGroupButtons(inputId = "radioBtn_log_tr", label = "Logarithmic vars?", c("Yes", "No"), selected = "Yes",
                          size = "xs", checkIcon = list(yes = icon("ok", lib = "glyphicon")))
  })

  output$plotly_1 <- renderPlotly({

    req(input$select1_xvar, input$select2_yvar, input$select3_zvar, input$select4_colvar, input$show_legend, input$radioBtn_log_tr, input$goButton)

    input$goButton

    data <- df_leaflet_t10 %>% 
        select(date, year, region, country, city, 
                nkill, nwound, latitude, longitude,
                group_name, attack_type, target_type, weapon_type, target_nalty,
                suicide_attack, intl_logistical_attack, intl_ideological_attack, crit1_pol_eco_rel_soc, crit2_publicize,
                arms_export, arms_import, population, gdp_per_capita, refugee_origin, refugee_asylum, net_migration, n_peace_keepers, conflict_index) %>% 
        replace_na(list(nkill = 0, nwound = 0)) %>%
        na.omit()

      if(input$radioBtn_log_tr == "No") {
        data <- data 
      } else {
        data <- data %>% mutate(nkill = log1p(nkill), 
                                nwound = log1p(nwound),
                                arms_export = log1p(arms_export), 
                                arms_import = log1p(arms_import), 
                                population = log1p(population + 1), 
                                gdp_per_capita = log1p(gdp_per_capita), 
                                refugee_origin = log1p(refugee_origin), 
                                refugee_asylum = log1p(refugee_asylum))
      }
        
    data %>%
      plot_ly(x = ~get(input$select1_xvar), 
              y = ~get(input$select2_yvar), 
              z = ~get(input$select3_zvar), 
              color = ~get(input$select4_colvar), 
              colors = ~rev(viridis::plasma(length(unique(get(input$select4_colvar))))),
              hoverinfo = 'text',
              text = ~paste("Date: ", date,
                            "<br>Group: ", group_name,
                            "<br>Population: ", population,
                            "<br>Region : ", region,
                            "<br>Country : ", country,
                            "<br>City : ", city,
                            "<br># killed : ", nkill,
                            "<br># wounded : ", nwound,
                            "<br>Attack type: ", attack_type,
                            "<br>Weapon type: ", weapon_type,
                            "<br>Target type: ", target_type,
                            "<br>Suicide attack?: ", suicide_attack,
                            "<br>Arms export: ", arms_export,
                            "<br>Arms import: ", arms_import,
                            "<br>refugee_origin: ", refugee_origin,
                            "<br>refugee_asylum: ", refugee_asylum)) %>%
      add_markers(opacity = 0.8) %>%
      layout( 
           # paper_bgcolor= "#f7f7f7",
           showlegend = ifelse(input$show_legend == "Yes", TRUE, FALSE),
           legend = list(orientation = "h", xanchor = "center", x = 0.5),
           paper_bgcolor= "black", plot_bgcolor = "black",
           annotations=list(yref='paper',xref="paper", text = "", showarrow=F), 
           scene = list(xaxis = list(title = input$select1_xvar),
                        yaxis = list(title = input$select2_yvar),
                        zaxis = list(title = input$select3_zvar)))
    
   })


  #-------------------------------------
  # Tab 3: Patterns/ Heatmaps
  #-------------------------------------

  output$pattern_hmap1 <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(target_type, year) %>% summarise(total_attacks = n()) 
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$target_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target type: ", target_type,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Target Type", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$pattern_hmap2 <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(attack_type, year) %>% summarise(total_attacks = n()) %>% ungroup() %>%
      mutate(attack_type = ifelse(attack_type == "Hostage Taking (Kidnapping)", "Hostage Taking (Kidnap.)", 
                           ifelse(attack_type == "Hostage Taking (Barricade Incident)", "Hostage Taking (Barricade)", 
                           ifelse(attack_type == "Facility/Infrastructure Attack", "Facility/Infra. Attack", 
                                  attack_type)))) #shorten long names (for better visualization)
    tmp %>%
    plot_ly(x=tmp$year, y=tmp$attack_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
            hoverinfo = 'text',text = ~paste("Year: ", year,
                                             "<br>Attack type: ", attack_type,
                                             "<br>Total attacks : ", total_attacks)) %>%
      layout(title = "By Attack Type",
             xaxis = list(autorange = "reversed"), 
             yaxis = list(autorange = "reversed"), 
             paper_bgcolor= "black", plot_bgcolor = "black")

    })


  output$pattern_hmap3 <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(weapon_type, year) %>% summarise(total_attacks = n()) %>% ungroup() %>%
      mutate(weapon_type = ifelse(weapon_type == "Explosives/Bombs/Dynamite", "Explosives", weapon_type)) #shorten long names (for better visualization)
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$weapon_type, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Weapon type: ", weapon_type,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Weapon Type", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$pattern_hmap4 <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(group_name, year) %>% summarise(total_attacks = n()) 
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$group_name, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Group: ", group_name,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Top 10 Terror Groups", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")

    })

  output$pattern_hmap_countries <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(country, year) %>% summarise(total_attacks = n())
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$country, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target Countries: ", country,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Country", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")
       
    })


  output$pattern_hmap_tnats <- renderPlotly({
    tmp <- df_leaflet_t10 %>% group_by(target_nalty, year) %>% summarise(total_attacks = n())
    tmp %>%
      plot_ly(x=tmp$year, y=tmp$target_nalty, z = tmp$total_attacks, type = "heatmap", colors = viridis::plasma(100),
              hoverinfo = 'text',text = ~paste("Year: ", year,
                                               "<br>Target Nationality: ", target_nalty,
                                               "<br>Total attacks : ", total_attacks)) %>%
        layout(title = "By Target Nationality<br><br><br>", 
               xaxis = list(autorange = "reversed"),
               yaxis = list(autorange = "reversed"), 
               paper_bgcolor= "black", plot_bgcolor = "black")
       
    })


    #-------------------------------------
    # Section 3: GUI (plots)
    #-------------------------------------

    output$radioBtn_data_gui <- renderUI ({
          radioButtons(inputId = "radioBtn_data_gui", label = "Select data :", choices = c("T10 Groups", "All"), selected = "T10 Groups", inline = TRUE)
      })

    output$slider_year_gui <- renderUI({ 
          sliderInput("slider_year_gui", label = "Year range", min = 1970, max = 2016, value = c("2015", "2016"))
        })

    observe({
      nms <- names(df_shiny())

      # Make list of variables that are not factors
      nms_cont <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), df_shiny()))

      # Make list of variables that are not factors
      nms_fact <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), df_shiny()))

      avail_all <- c("No groups" = ".", nms)
      avail_con <-
        if (identical(nms_cont, character(0)))
          c("No continuous vars available" = ".")
        else c(nms_cont)
      avail_fac <-
        if (identical(nms_fact, character(0)))
          c("No factors available" = ".")
        else c("No groups" = ".", nms_fact)

      updateSelectInput(session, "y_var", choices = avail_con)
      updateSelectInput(session, "x_var", choices = c("No x-var" = "' '", nms))
      updateSelectInput(session, "group", choices = avail_all)
      updateSelectInput(session, "facet_row",  choices = avail_fac)
      updateSelectInput(session, "facet_col",  choices = avail_fac)

      min_year <- min(df_shiny()$year)
      max_year <- max(df_shiny()$year)
      updateSliderInput(session, "slider_year_gui", min = 1970, max = 2016, value = c(min_year, max_year))

    })


    #-------------------------------------
    ###### READ IN / GET DATA ###########
    #-------------------------------------

     df_shiny <- reactive({

        req(input$radioBtn_data_gui, input$slider_year_gui)

        if(input$radioBtn_data_gui == "T10 Groups") {
          data <- df_leaflet_t10 # filter data for top 10 deadliest groups
        } else {
          data <- df_leaflet # select everything
        }

        data <- data[data$year >= input$slider_year_gui[1] & data$year <= input$slider_year_gui[2], ]

        data <- data %>% 
          mutate(nkill_log = log1p(nkill), nwound_log = log1p(nwound)) %>%          
          select(nkill_log, nwound_log, nkill, nwound, date, year, region, country, group_name, attack_type, target_type, weapon_type, 
                 suicide_attack, intl_logistical_attack, intl_ideological_attack, crit1_pol_eco_rel_soc, crit2_publicize,
                 arms_export, arms_import, population, gdp_per_capita, refugee_origin, refugee_asylum, net_migration) 
        return(data)

      })

    #-------------------------------------
    # CREATE GRAPH-CODE 
    #-------------------------------------

    string_code <- reactive({

      # Variable used for how to deal with x/y in ggplot
      gg_x_y <- input$Type == "Histogram" || input$Type == "Density"
      # Variable used for how to deal with colour/fill
      gg_fil <- input$Type == "Histogram" || input$Type == "Density"

      # Only plot jitter when graphs allow them
      if (gg_fil || input$Type == "Scatter")
        jitt <- FALSE else jitt <- input$jitter

      p <- paste(
        "ggplot(df_plotly_data, aes(",
          if (gg_x_y) {
            "x = input$y_var"
          } else {
            "x = input$x_var, y = input$y_var"
          },
          if (input$group != "." && gg_fil) {
            ", fill = input$group"
          } else if (input$group != "." && !gg_fil) {
            ", colour = input$group"
          },
          ")) + ",
          if (input$Type == "Histogram")
            paste("geom_histogram(position = 'identity', alpha = input$alpha, ", "binwidth = input$binwidth)", sep = ""),
          if (input$Type == "Density")
            paste("geom_density(position = 'identity', alpha = input$alpha, ", "adjust = input$adj_bw)", sep = ""),
          if (input$Type == "Boxplot")
            "geom_boxplot(notch = input$notch)",
          if (input$Type == "Violin")
            "geom_violin(adjust = input$adj_bw)",
          if (input$Type == "Scatter")
            "geom_point()",
          if (input$Type == "Scatter" && input$line)
            "+ geom_smooth(se = input$se, method = 'input$smooth')",
          if (jitt)
            paste(" + geom_jitter(size = input$size_jitter, ", "alpha = input$opac_jitter, width = input$width_jitter, ", "colour = 'input$col_jitter')", sep = ""), sep = "")

      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, "~", input$facet_col)
      if (facets != ". ~ .")
        p <- paste(p, "+ facet_grid(", facets, ")")

      # if labels specified
      if (input$label_axes)
        p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")

      # if title specified
      if (input$add_title)
        p <- paste(p, "+ ggtitle('input$title')")

      # if legend specified
      if (input$adj_leg == "Change legend")
        p <- paste(p, "+ labs(",
                   if (gg_fil) "fill" else "colour",
                   " = 'input$leg_ttl')",
                   sep = "")

      # if colour legend specified
      # if (input$adj_col)
      #   p <- paste(p, "+ scale_",
      #              if (gg_fil) "fill" else "colour",
      #              "_brewer(palette = 'input$palet')",
      #              sep = "")

      # If a theme specified
      p <- paste(p, "+", input$theme)

      # If theme features are specified
      if (input$adj_fnt_sz || input$adj_fnt || input$rot_txt || input$adj_leg != "Keep legend as it is" || input$adj_grd) {
        p <- paste(p,
          paste(
            " + theme(\n    ",
            if (input$adj_fnt_sz)
              "axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
            if (input$adj_fnt_sz)
              "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
            if (input$adj_fnt)
              "text = element_text(family = 'input$font'),\n    ",
            if (input$rot_txt)
              "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
            if (input$adj_leg == "Remove legend")
              "legend.position = 'none',\n    ",
            if (input$adj_leg == "Change legend")
              "legend.position = 'input$pos_leg',\n    ",
            if (input$grd_maj)
              "panel.grid.major = element_blank(),\n    ",
            if (input$grd_min)
              "panel.grid.minor = element_blank(),\n    ", ")",
            sep = ""
          ),
          sep = ""
        )
      }

      # Replace name of variables by values
      p <- str_replace_all(
             p,
             c("input\\$y_var" = input$y_var,
               "input\\$x_var" = input$x_var,
               "input\\$group" = input$group,
               "input\\$notch" = as.character(input$notch),
               "input\\$binwidth" = as.character(input$binwidth),
               "input\\$adj_bw" = as.character(input$adj_bw),
               # "input\\$dot_dir" = as.character(input$dot_dir),
               "input\\$alpha" = as.character(input$alpha),
               "input\\$se" = as.character(input$se),
               "input\\$smooth" = as.character(input$smooth),
               "input\\$CI" = as.character(input$CI),
               "input\\$size_jitter" = as.character(input$size_jitter),
               "input\\$width_jitter" = as.character(input$width_jitter),
               "input\\$opac_jitter" = as.character(input$opac_jitter),
               "input\\$col_jitter" = as.character(input$col_jitter),
               "input\\$lab_x" = as.character(input$lab_x),
               "input\\$lab_y" = as.character(input$lab_y),
               "input\\$title" = as.character(input$title),
               "input\\$palet" = as.character(input$palet),
               "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
               "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
               "input\\$font" = as.character(input$font),
               "input\\$leg_ttl" = as.character(input$leg_ttl),
               "input\\$pos_leg" = as.character(input$pos_leg))
      )

    })

    #-------------------------------------
    # GRAPHICAL/TABLE OUTPUT 
    #-------------------------------------

    # output$out_table <- renderDataTable({
    #   df_shiny() %>% 
    #     select(date, year, region, country, target_nalty, city, nkill, nwound, group_name, attack_type, target_type, weapon_type,
    #            suicide_attack, intl_logistical_attack, intl_ideological_attack, crit1_pol_eco_rel_soc, crit2_publicize) %>% 
    #     datatable(rownames = FALSE, style="cell-border", options = list(scrollX = TRUE, pageLength = 5))
    # })

    width <- reactive ({ input$fig_width })
    height <- reactive ({ input$fig_height })    

    output$out_ggplot <- renderPlot(width = width, height = height, {

      df_plotly_data <- df_shiny()
      p <- eval(parse(text = string_code())) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
      p
    })

    output$out_plotly <- renderPlotly({
      # evaluate the string RCode as code
      df_plotly_data <- df_shiny()
      p <- eval(parse(text = string_code())) + 
            scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
      ggplotly(p) 
      # %>% layout(legend = list(orientation = "h", y = -10, x = 0))
    })


  }) # End of shinyServer logic



