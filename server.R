
shinyServer(function(input, output, session) {

  options(warn = -1, digits = 5, scipen = 999)
  set.seed(84)

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
          checkboxGroupButtons(inputId = "checkBtn3_log_int", label = "Logistically intl?", choices = c("Yes", "No", "Unknown"), selected = c("Yes", "No", "Unknown"),
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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
             paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

    })

  output$slider_filter_year <- renderUI({ 
        sliderInput("slider_filter_year", label = "Year range", min = 1970, max = 2016, value = c("2010", "2016"))
    })

  output$slider_filter_total_attacks <- renderUI({ 

      sliderInput("slider_filter_total_attacks", label = "Total number of attacks", min = 0, max = 1500, value = c("0", "1500"))

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")
       
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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")
       
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
      # hc_add_theme(hc_theme_sandsignika()) %>%
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
      # hc_add_theme(hc_theme_sandsignika()) %>%
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
      # hc_add_theme(hc_theme_sandsignika()) %>%
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
      # hc_add_theme(hc_theme_sandsignika()) %>%
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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
             paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")

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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")
       
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
               paper_bgcolor= "#0f1011", plot_bgcolor = "#0f1011")
       
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
      updateSelectInput(session, "x_var", choices = c("No x-var" = "' '", nms), selected = "region")
      updateSelectInput(session, "group", choices = avail_all, selected = "region")
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


    #------------------------------------------------
    # Sidebar : time-series analysis
    #------------------------------------------------
    output$ts_fc_goal <- renderUI ({
          radioButtons(inputId = "ts_fc_goal", label = "Forecasting goal", 
                       choices = c("Number of attacks", 
                                   "Fatalities (nkill)", 
                                   "Fatalities (nwound)"), 
                       selected = "Number of attacks", inline = FALSE)
      })

    output$ts_filter_country <- renderUI({    
          selectizeInput(inputId = "ts_filter_country", label = "Select country", choices = sort(unique(df$country)), 
            selected = sort(unique(df$country))[1], multiple = TRUE)
        })

    output$ts_filter_year <- renderUI({ 
          sliderInput("ts_filter_year", label = "Select year range", min = 1970, max = 2016, value = c("2000", "2016"))
        })

    output$ts_attack_freq <- renderUI ({
          radioButtons(inputId = "ts_attack_freq", label = "Time-series frequency", 
                       choices = c("Monthly", "Quarterly"), selected = "Monthly", inline = FALSE)
      })

    output$ts_slider_horizon <- renderUI({ 
      req(input$ts_attack_freq)
      sliderInput("ts_slider_horizon", label = "Horizon (months)", min = 6, max = 24, value = 12, step = 2)
      })

   observe({
      input$ts_attack_freq
      min   <- ifelse(input$ts_attack_freq == "Quarterly", 4, 6)
      max   <- ifelse(input$ts_attack_freq == "Quarterly", 12, 24)
      value <- ifelse(input$ts_attack_freq == "Quarterly", 8, 12)
      label <- ifelse(input$ts_attack_freq == "Quarterly", "Horizon (quarters)", "Horizon (months)")
      updateSliderInput(session, "ts_slider_horizon", label = label, min = min, max = max, value = value, step = 2)
      })

    output$ts_slider_nn_repeats <- renderUI({ 
      sliderInput("ts_slider_nn_repeats", label = "nnet (repeats)", min = 1, max = 15, value = 10, step = 1)
      })

    #---------------------------------------
    # Reactive time-series data
    #---------------------------------------
    ts_data <- reactive({

      # filtered data based on year and country selected
      data <- df
      data <- data[data$year >= input$ts_filter_year[1] & data$year <= input$ts_filter_year[2], ]
      data <- data[data$country %in% input$ts_filter_country, ]

      #----------------------------------------------
      # Choice 1: Extract data for number of attacks
      #----------------------------------------------
      if(input$ts_fc_goal == "Number of attacks") {

        # Extract data by user selected attack frequency
        if(input$ts_attack_freq == "Monthly") {

            #---------------------------------------
            # Monthly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(month = month(date),
                     month_year = paste(year, month,sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              group_by(year, month) %>%
              summarise(total_count = n()) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(month = full_seq(seq(1:12), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(month_year = paste(year, month, sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              select(month_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$month_year)), frequency = 12) # 1=annual, 4=quartly, 12=monthly
            data <- na.kalman(data)

        }

        if(input$ts_attack_freq == "Quarterly") {
      
            #---------------------------------------
            # Quarterly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(quarter = Quarter(date),
                     quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = as.yearqtr(quarter_year, "%Y-%q")) %>%
              group_by(year, quarter) %>%
              summarise(total_count = n()) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(quarter = full_seq(seq(1:4), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = zoo::as.yearqtr(quarter_year)) %>%
              select(quarter_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$quarter_year)), frequency = 4) # 1=annual, 4=quartly, 12=quarterly
            data <- na.kalman(data)

          }
      }

      #----------------------------------------------
      # Choice 2: Extract data for nkills
      #----------------------------------------------
      if(input$ts_fc_goal == "Fatalities (nkill)") {

        # if multiple channels report different count for same attack then select the one which is maximumn
        data <- data %>% 
          replace_na(list(nkill = 0)) %>% 
          group_by(group_name, region, year, month) %>% 
          filter(if_else(part_of_multiple_attacks == 1, nkill == max(nkill), nkill == nkill)) %>%
          ungroup()

        # Extract data by user selected attack frequency
        if(input$ts_attack_freq == "Monthly") {

            #---------------------------------------
            # Monthly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(month = month(date),
                     month_year = paste(year, month,sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              group_by(year, month) %>%
              summarise(total_count = sum(nkill)) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(month = full_seq(seq(1:12), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(month_year = paste(year, month, sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              select(month_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$month_year)), frequency = 12) # 1=annual, 4=quartly, 12=monthly
            data <- na.kalman(data)

        }

        if(input$ts_attack_freq == "Quarterly") {
      
            #---------------------------------------
            # Quarterly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(quarter = Quarter(date),
                     quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = as.yearqtr(quarter_year, "%Y-%q")) %>%
              group_by(year, quarter) %>%
              summarise(total_count = sum(nkill)) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(quarter = full_seq(seq(1:4), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = zoo::as.yearqtr(quarter_year)) %>%
              select(quarter_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$quarter_year)), frequency = 4) # 1=annual, 4=quartly, 12=quarterly
            data <- na.kalman(data)

          }

      }


      #----------------------------------------------
      # Choice 3: Extract data for nwounds
      #----------------------------------------------
      if(input$ts_fc_goal == "Fatalities (nwound)") {

        # if multiple channels report different count for same attack then select the one which is maximumn
        data <- data %>% 
          replace_na(list(nwound = 0)) %>% 
          group_by(group_name, region, year, month) %>% 
          filter(if_else(part_of_multiple_attacks == 1, nwound == max(nwound), nwound == nwound)) %>%
          ungroup()

        # Extract data by user selected attack frequency
        if(input$ts_attack_freq == "Monthly") {

            #---------------------------------------
            # Monthly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(month = month(date),
                     month_year = paste(year, month,sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              group_by(year, month) %>%
              summarise(total_count = sum(nwound)) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(month = full_seq(seq(1:12), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(month_year = paste(year, month, sep="-"),
                     month_year = zoo::as.yearmon(month_year)) %>%
              select(month_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$month_year)), frequency = 12) # 1=annual, 4=quartly, 12=monthly
            data <- na.kalman(data)

        }

        if(input$ts_attack_freq == "Quarterly") {
      
            #---------------------------------------
            # Quarterly time-series data
            #---------------------------------------
            data <- data %>%
              mutate(quarter = Quarter(date),
                     quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = as.yearqtr(quarter_year, "%Y-%q")) %>%
              group_by(year, quarter) %>%
              summarise(total_count = sum(nwound)) %>%
              ungroup() %>%
              group_by(year) %>%
              tidyr::complete(quarter = full_seq(seq(1:4), 1L), fill = list(total_count = 0)) %>%
              ungroup()

            data <- data %>%
              mutate(quarter_year = paste(year, quarter, sep="-"),
                     quarter_year = zoo::as.yearqtr(quarter_year)) %>%
              select(quarter_year, total_count)

            # Create a ts object
            data <- ts(data[, 2], start = Year(min(data$quarter_year)), frequency = 4) # 1=annual, 4=quartly, 12=quarterly
            data <- na.kalman(data)

          }
      }

      # return data based on choice (by attack counts/ nkills/ nwounds)
      return(data)
    })


    #-------------------------------------
    # Trends by attack counts
    #-------------------------------------

    output$ts_line <- renderPlotly({
      total_counts <- ts_data()
      ts_plot(total_counts, line.mode = "lines+markers")
    })

    output$ts_cycle <- renderPlotly({
      total_counts <- ts_data()
      ts_seasonal(total_counts, type = "cycle")      
    })

    output$ts_normal <- renderPlotly({
      total_counts <- ts_data()
      ts_seasonal(total_counts, type = "normal")      
    })

    output$ts_box <- renderPlotly({
      total_counts <- ts_data()
      ts_seasonal(total_counts, type = "box")      
    })

    output$ts_heatmap <- renderPlotly({
      total_counts <- ts_data()
      ts_heatmap(total_counts)      
    })

    output$ts_surface <- renderPlotly({
      total_counts <- ts_data()
      ts_surface(total_counts)      
    })

    output$ts_polar <- renderPlotly({
      total_counts <- ts_data()
      ts_polar(total_counts, width = 470, height = 500)      
    })

    output$ts_decompose <- renderPlotly({
      total_counts <- ts_data()
      ts_decompose(total_counts, type = "both")     
    })
  
    output$ts_acf <- renderPlotly({
      total_counts <- ts_data()
      ts_acf(total_counts, lag.max = 36)      
    })

    output$ts_pacf <- renderPlotly({
      total_counts <- ts_data()
      ts_pacf(total_counts, lag.max = 36)      
    })
    
    output$ts_lag <- renderPlotly({
      total_counts <- ts_data()
      ts_lags(total_counts)      
    })


    #------------------------------------------------
    # Time-series train test validation
    #------------------------------------------------

    selected_horizon <- reactive({as.numeric(input$ts_slider_horizon)})
    selected_nn_repeats <- reactive({as.numeric(input$ts_slider_nn_repeats)})

    # reactive data for arima, tbats and ets
    ts_forecast_data <- reactive({

      # req(input$ts_attack_freq, input$ts_slider_horizon)

      data <- ts_data()
      
      # crete split for train and test set
      data <- ts_split(data, sample.out = selected_horizon())
      
      # Split the data into training and testing sets
      train <- data$train
      test  <- data$test

      set.seed(84)

      # Building a models on the training set
      fit_arima <- auto.arima(train, lambda = BoxCox.lambda(train))
      fit_tbats <- tbats(train, lambda = BoxCox.lambda(train))
      fit_ets   <- ets(train, lambda = BoxCox.lambda(train))

      # Accuracy check/ Forecast evaluation for each models
      fc_arima <- forecast(fit_arima, h = selected_horizon())
      fc_tbats <- forecast(fit_tbats, h = selected_horizon())
      fc_ets   <- forecast(fit_ets, h = selected_horizon())

      data <- list(train      = train, 
                   test       = test,
                   fit_arima  = fit_arima,
                   fc_arima   = fc_arima,
                   fit_tbats  = fit_tbats,
                   fc_tbats   = fc_tbats,
                   fit_ets    = fit_ets,
                   fc_ets     = fc_ets)
      return(data)

    })

    # reactive data for neural network
    ts_forecast_data_nn <- reactive({

      data <- ts_data()
      
      # crete split for train and test set
      data <- ts_split(data, sample.out = selected_horizon())
      
      # Split the data into training and testing sets
      train <- data$train
      test  <- data$test

      # Building a models on the training set
      set.seed(84)
      fit_nn    <- nnetar(train, repeats = selected_nn_repeats(), lambda = BoxCox.lambda(train))

      # Accuracy check/ Forecast evaluation for each models
      fc_nn    <- forecast(fit_nn, h = selected_horizon())

      data <- list(train      = train, 
                   test       = test,
                   fit_nn     = fit_nn,
                   fc_nn      = fc_nn)
      return(data)

    })


    output$ts_res_arima <- renderPlotly({

      forecast_data <- ts_forecast_data()
      fit_arima     <- forecast_data$fit_arima

      # plot the residuals
      check_res(fit_arima)
    })
    
    output$ts_eval_arima <- renderPlotly({

      act_data      <- ts_data()
      forecast_data <- ts_forecast_data()
      fc_arima      <- forecast_data$fc_arima
      test          <- forecast_data$test

      #plot actual vs fitted and forecasted
      test_forecast(actual = act_data, forecast.obj = fc_arima, test = test) %>% layout(legend = list(x = 0.1, y = 0.9)) 
    })


    output$ts_res_nn <- renderPlotly({

      forecast_data <- ts_forecast_data_nn()
      fit_nn        <- forecast_data$fit_nn

      # plot the residuals
      check_res(fit_nn)
    })
    
    output$ts_eval_nn <- renderPlotly({

      act_data      <- ts_data()
      forecast_data <- ts_forecast_data_nn()
      fc_nn         <- forecast_data$fc_nn
      test          <- forecast_data$test

      #plot actual vs fitted and forecasted
      test_forecast(actual = act_data, forecast.obj = fc_nn, test = test) %>% layout(legend = list(x = 0.1, y = 0.9))    
    })


    output$ts_res_tbats <- renderPlotly({

      forecast_data <- ts_forecast_data()
      fit_tbats     <- forecast_data$fit_tbats

      # can't use check_res so using autoplot
      p <- ggplot2::autoplot(fit_tbats) 
      ggplotly(p)
    })
    
    output$ts_eval_tbats <- renderPlotly({

      act_data      <- ts_data()
      forecast_data <- ts_forecast_data()
      fc_tbats      <- forecast_data$fc_tbats
      test          <- forecast_data$test

      #plot actual vs fitted and forecasted
      test_forecast(actual = act_data, forecast.obj = fc_tbats, test = test) %>% layout(legend = list(x = 0.1, y = 0.9))   
    })


    output$ts_res_ets <- renderPlotly({

      forecast_data <- ts_forecast_data()
      fit_ets       <- forecast_data$fit_ets

      # plot the residuals
      check_res(fit_ets)
    })
    
    output$ts_eval_ets <- renderPlotly({

      act_data      <- ts_data()
      forecast_data <- ts_forecast_data()
      fc_ets        <- forecast_data$fc_ets
      test          <- forecast_data$test

      #plot actual vs fitted and forecasted
      test_forecast(actual = act_data, forecast.obj = fc_ets, test = test)  %>% layout(legend = list(x = 0.1, y = 0.9))   
    })

    output$acc_arima <- function() {

      forecast_data <- ts_forecast_data()
      fc_arima      <- forecast_data$fc_arima
      test          <- forecast_data$test

      knitr::kable(t(round(accuracy(fc_arima$mean, test), 3)), caption = "Validation result: ") %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "black", background = "#dee2ed")

    }

    output$acc_nn <- function() {

      forecast_data <- ts_forecast_data_nn()
      fc_nn         <- forecast_data$fc_nn
      test          <- forecast_data$test

      knitr::kable(t(round(accuracy(fc_nn$mean, test), 3)), caption = "Validation result: ") %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "black", background = "#dee2ed")

    }

    output$acc_tbats <- function() {

      forecast_data <- ts_forecast_data()
      fc_tbats      <- forecast_data$fc_tbats
      test          <- forecast_data$test

      knitr::kable(t(round(accuracy(fc_tbats$mean, test), 3)), caption = "Validation result: ") %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "black", background = "#dee2ed")

    }

    output$acc_ets <- function() {

      forecast_data <- ts_forecast_data()
      fc_ets        <- forecast_data$fc_ets
      test          <- forecast_data$test

      knitr::kable(t(round(accuracy(fc_ets$mean, test), 3)), caption = "Validation result: ") %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "black", background = "#dee2ed")

    }

    # reactive data for model comparison
    data_model_compare <- reactive({

      fc_arima  <- ts_forecast_data()$fc_arima
      fc_nn     <- ts_forecast_data_nn()$fc_nn
      fc_tbats  <- ts_forecast_data()$fc_tbats
      fc_ets    <- ts_forecast_data()$fc_ets
      test      <- ts_forecast_data()$test

      metrics  <- rbind(as.data.frame(round(accuracy(fc_arima$mean, test), 3)),
                        as.data.frame(round(accuracy(fc_nn$mean, test), 3)),
                        as.data.frame(round(accuracy(fc_tbats$mean, test), 3)),
                        as.data.frame(round(accuracy(fc_ets$mean, test), 3))) %>% 
                  add_column(models = c("Auto Arima", "NeuralNet", "TBATS", "ETS"), .before = "ME") %>% 
                  arrange(MAPE)

      return(metrics)

    })

    # Performance evaluation (accurace comparison)
    output$tbl_eval_compare <- function() {

      metrics <- data_model_compare()

      knitr::kable(metrics) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = T, position = "left") %>%
        column_spec(1, bold = T, color = "black") %>%
        column_spec(2:8, color = "black", background = "#dee2ed") %>%
        column_spec(6, color = "black", background = "#c7cfe5") %>%
        row_spec(1, background = "#c7cfe5")

    }

    output$eval_theilu <- renderHighchart({

      metrics <- data_model_compare()

      colnames(metrics)[8] <- "theils_U"
      metrics <- metrics %>% arrange(theils_U)
      highchart() %>% 
        hc_subtitle(text = "Model evaluation by Theil's U statistic score") %>%
        hc_xAxis(categories = metrics$models, 
                 title = list(text = "Name of the model")) %>% 
        hc_yAxis(title = list(text = "Theil's U Score"),
                              plotLines = list(list(
                                          color = "#FF0000",
                                          width = 2,
                                          value = 1)),
                              plotBands = list(list(
                                          # label = list(text = "Any model in this region is worst than random guess as per Theil's U"),
                                          from = 1, 
                                          to = JS("Infinity"), 
                                          color = "rgba(100, 0, 0, 0.1)"))) %>%
        hc_add_series(data = metrics$theils_U, colorByPoint = TRUE, type = "column", showInLegend = F)

    })

    output$tbl_eval_text <- function() {

      text_tbl <- data.frame(
                    Metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "ACF1", "Theil’s U"),
                    Interpretation = c(
                      "Mean Error: Scale-dependent error, lower better",
                      "Root Mean Squared Error: Scale-dependent error, lower better", 
                      "Mean Absolute Error: Scale-dependent error, lower better",
                      "Mean Percentage Error: Percentage error, lower better", 
                      "Mean Absolute Percent Error: Percentage error, lower better", 
                      "First-order autocorrelation", 
                      "Relative accuracy. Less than one means better than guessing"
                       )
                  )
      knitr::kable(text_tbl) %>%
        kable_styling(full_width = F) %>%
        column_spec(1, bold = T, border_right = F) %>%
        column_spec(2, background = "#dee2ed")%>%
        row_spec(5, background = "#dee2ed")

    }

    output$tbl_eval_text_theilu <- function() {

      text_tbl <- data.frame(
                    Term = c("Theil’s U statistic: ", "score < 1", "score = 1", "score > 1"),
                    Interpretation = c(
                      "Theil’s U statistic is a relative accuracy measure that compares the forecasted results with the results of forecasting 
                       with minimal historical data. It also squares the deviations to give more weight to large errors and to exaggerate errors, 
                       which can help eliminate methods with large errors.",
                      "The forecasting technique is better than guessing", 
                      "The forecasting technique is about as good as guessing",
                      "The forecasting technique is worse than guessing"
                       )
                  )
      knitr::kable(text_tbl) %>%
        kable_styling(full_width = F) %>%
        column_spec(1, bold = T, border_right = F) %>%
        column_spec(2, background = "#dee2ed") %>%
        row_spec(2, background = "#dee2ed")

    }

    #--------------------------------------------------------
    # Time-series Forecast from each models
    #--------------------------------------------------------
    
    # reactive data
    arima_preds <- reactive({

      set.seed(84)
      act_data  <- ts_data()
      fit       <- auto.arima(act_data)
      fore      <- forecast(fit, h = selected_horizon(), level = c(80, 95))
      return(fore)

    })

    output$fc_prediction_arima <- renderPlotly({

      act_data  <- ts_data()
      fore      <- arima_preds()

      act_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Actual number of attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Actual number of people killed", "Actual number of people wounded"))

      fore_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted number of attacks", 
                       ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      plot_ly() %>%
        add_lines(x = time(act_data), y = act_data,
                  color = I("#487caf"), name = act_label) %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                    color = I("gray90"), name = "95% confidence") %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                    color = I("gray85"), name = "80% confidence") %>%
        add_lines(x = time(fore$mean), y = fore$mean, color = I("orange"), name = fore_label) %>% 
        layout(legend = list(x = 0.1, y = 0.9)) 

    })

    output$tbl_fc_prediction_arima <- function() {

      fore          <- arima_preds()
      tbl           <- timetk::tk_tbl(fore$mean) 
      names(tbl)    <- c("time_period", "forecast")
      tbl$forecast  <- round(tbl$forecast)

      cap_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      knitr::kable(tbl, caption = cap_label) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "white", background = "#a05050") 

    }

    # reactive data
    nnetar_preds <- reactive({

      set.seed(84)
      act_data  <- ts_data()
      fit       <- nnetar(act_data, repeats = selected_nn_repeats())
      fore      <- forecast(fit, h = selected_horizon(), level = c(80, 95), PI = TRUE)
      return(fore)

    })

    output$fc_prediction_nn <- renderPlotly({

      act_data  <- ts_data()
      fore      <- nnetar_preds()

      act_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Actual number of attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Actual number of people killed", "Actual number of people wounded"))

      fore_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted number of attacks", 
                       ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      plot_ly() %>%
        add_lines(x = time(act_data), y = act_data,
                  color = I("#487caf"), name = act_label) %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                    color = I("gray90"), name = "95% confidence") %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                    color = I("gray85"), name = "80% confidence") %>%
        add_lines(x = time(fore$mean), y = fore$mean, color = I("orange"), name = fore_label) %>% 
        layout(legend = list(x = 0.1, y = 0.9)) 

    })

    output$tbl_fc_prediction_nn <- function() {

      fore          <- nnetar_preds()
      tbl           <- timetk::tk_tbl(fore$mean) 
      names(tbl)    <- c("time_period", "forecast")
      tbl$forecast  <- round(tbl$forecast)

      cap_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      knitr::kable(tbl, caption = cap_label) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "white", background = "#a05050") 

    }

    # reactive data
    tbats_preds <- reactive({
      set.seed(84)
      act_data  <- ts_data()
      fit       <- tbats(act_data)
      fore      <- forecast(fit, h = selected_horizon(), level = c(80, 95))
      return(fore)

    })

    output$fc_prediction_tbats <- renderPlotly({

      act_data  <- ts_data()
      fore      <- tbats_preds()

      act_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Actual number of attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Actual number of people killed", "Actual number of people wounded"))

      fore_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted number of attacks", 
                       ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      plot_ly() %>%
        add_lines(x = time(act_data), y = act_data,
                  color = I("#487caf"), name = act_label) %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                    color = I("gray90"), name = "95% confidence") %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                    color = I("gray85"), name = "80% confidence") %>%
        add_lines(x = time(fore$mean), y = fore$mean, color = I("orange"), name = fore_label) %>% 
        layout(legend = list(x = 0.1, y = 0.9)) 

    })

    output$tbl_fc_prediction_tbats <- function() {

      fore          <- tbats_preds()
      tbl           <- timetk::tk_tbl(fore$mean) 
      names(tbl)    <- c("time_period", "forecast")
      tbl$forecast  <- round(tbl$forecast)

      cap_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      knitr::kable(tbl, caption = cap_label) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "white", background = "#a05050") 

    }

    # reactive data
    ets_preds <- reactive({
      set.seed(84)
      act_data  <- ts_data()
      fit       <- ets(act_data)
      fore      <- forecast(fit, h = selected_horizon(), level = c(80, 95))
      return(fore)

    })

    output$fc_prediction_ets <- renderPlotly({

      act_data  <- ts_data()
      fore      <- ets_preds()

      act_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Actual number of attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Actual number of people killed", "Actual number of people wounded"))

      fore_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted number of attacks", 
                       ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      plot_ly() %>%
        add_lines(x = time(act_data), y = act_data,
                  color = I("#487caf"), name = act_label) %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                    color = I("gray90"), name = "95% confidence") %>%
        add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                    color = I("gray85"), name = "80% confidence") %>%
        add_lines(x = time(fore$mean), y = fore$mean, color = I("orange"), name = fore_label) %>% 
        layout(legend = list(x = 0.1, y = 0.9)) 

    })

    output$tbl_fc_prediction_ets <- function() {

      fore          <- ets_preds()
      tbl           <- timetk::tk_tbl(fore$mean) 
      names(tbl)    <- c("time_period", "forecast")
      tbl$forecast  <- round(tbl$forecast)

      cap_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Forecasted attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Forecasted fatalities (nkill)", "Forecasted fatalities (nwound)"))

      knitr::kable(tbl, caption = cap_label) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(2, bold = T, color = "white", background = "#a05050") 

    }

    # reactive data for ensemble
    ensemble_data <- reactive({

      tbl_arima   <- timetk::tk_tbl(round(arima_preds()$mean)) 
      tbl_nn      <- timetk::tk_tbl(round(nnetar_preds()$mean))
      tbl_tbats   <- timetk::tk_tbl(round(tbats_preds()$mean))
      tbl_ets     <- timetk::tk_tbl(round(ets_preds()$mean))

      tbl <- tbl_arima %>% 
          left_join(tbl_nn, by = "index") %>% 
          left_join(tbl_tbats, by = "index") %>% 
          left_join(tbl_ets, by = "index")

      names(tbl) <- c("Time period", "Arima", "NN", "TBATS", "ETS")
      tbl$Ensemble <- round(rowMeans(tbl[,2:5]))

      return(tbl)

    })

    output$tbl_fc_prediction_ensemble <- function() {

      tbl <- ensemble_data()
      cap_label <- ifelse(input$ts_fc_goal == "Number of attacks", "Ensembled forecast for future attacks", 
                      ifelse(input$ts_fc_goal == "Fatalities (nkill)", "Ensembled forecast for future fatalities (nkill)", 
                             "Ensembled forecast for future fatalities (nwound)"))

      knitr::kable(tbl, caption = cap_label) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(1:5, color = "black", background = "#dee2ed") %>%
        column_spec(6, bold = T, color = "black", background = "#c7cfe5") 

    }

    output$plot_ensemble <- renderHighchart({

      tbl <- ensemble_data()

      highchart() %>% 
        hc_subtitle(text = "Ensembled forecast") %>%
        hc_xAxis(categories = as.character(tbl$"Time period"), 
                 title = list(text = "Time period")) %>% 
        hc_yAxis(title = list(text = "Ensembled forcast")) %>%
        hc_add_series(data = tbl$Ensemble, type = "line", showInLegend = F) %>%
        hc_add_theme(hc_theme_ffx()) %>%
        hc_tooltip(pointFormat = "{point.y}") 

    })

    #--------------------------------------------------------
    # Part 5: Classification with LightGBM
    #--------------------------------------------------------

    output$lgb_filter_country <- renderUI({    
          selectizeInput(inputId = "lgb_filter_country", label = "Select country", choices = sort(unique(df_class$country)), 
                         selected = sort(unique(df_class$country))[1], multiple = TRUE)
        })

    # output$lgb_target_var <- renderUI({    
    #       pickerInput(inputId = "lgb_target_var", label = "Target variable", 
    #                   choices = list(Binary = c("suicide_attack", "attack_success", "extended", "part_of_multiple_attacks", 
    #                                              "crit1_pol_eco_rel_soc", "crit2_publicize", "crit3_os_intl_hmn_law")), 
    #                   selected = "suicide_attack", options = list(`actions-box` = TRUE), inline = FALSE, multiple = FALSE)
    #     })


    output$lgb_target_var <- renderUI({    
          prettyRadioButtons(inputId = "lgb_target_var", 
                             label = "Target variable", 
                             status = "success",
                             choiceNames =  c("Suicide attack", 
                                              "Attack success", 
                                              "Extended attack ", 
                                              "Part of multiple attacks", 
                                              "Pol/Eco goal", 
                                              "Intention to publicize", 
                                              "Outside humanitarian law"), 
                             choiceValues = c("suicide_attack", 
                                              "attack_success", 
                                              "extended", 
                                              "part_of_multiple_attacks", 
                                              "crit1_pol_eco_rel_soc", 
                                              "crit2_publicize", 
                                              "crit3_os_intl_hmn_law"),
                             selected = "suicide_attack",
                             animation = "pulse",
                             shape = "round")
        })

    # reactive data for target var
    target_var_data <- reactive({
      req(input$lgb_target_var)

      if(input$lgb_target_var == "suicide_attack"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(suicide_attack) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "attack_success"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(attack_success) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "extended"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(extended) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "part_of_multiple_attacks"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(part_of_multiple_attacks) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "crit1_pol_eco_rel_soc"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(crit1_pol_eco_rel_soc) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "crit2_publicize"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(crit2_publicize) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }
      if(input$lgb_target_var == "crit3_os_intl_hmn_law"){
        req(input$lgb_filter_country)
        data <- df_class %>% filter(country %in% input$lgb_filter_country) %>% group_by(crit3_os_intl_hmn_law) %>% summarize(count = n())
        names(data) <- c("target_var", "count")
      }

      return(data)

    })

    output$plot_target_var <- renderHighchart({

      data  <- target_var_data() 
      label <- ifelse(input$lgb_target_var == "suicide_attack", "Suicide attack",
               ifelse(input$lgb_target_var == "attack_success", "Attack success",
               ifelse(input$lgb_target_var == "extended", "Extended attack (>24 hrs)",
               ifelse(input$lgb_target_var == "part_of_multiple_attacks", "Part of multiple attacks",
               ifelse(input$lgb_target_var == "crit1_pol_eco_rel_soc", "Political/ Eco/ Rel/ Social goal",
               ifelse(input$lgb_target_var == "crit2_publicize", "Intention to publicize",
               ifelse(input$lgb_target_var == "crit3_os_intl_hmn_law", "Outside intl humanitarian law", "")))))))

      highchart() %>% 
        hc_title(text = paste0(label)) %>%
        hc_xAxis(categories = data$target_var, title = list(text = label)) %>% 
        hc_yAxis(title = list(text = "Total count")) %>%
        hc_add_series(data = data$count, type = "column", showInLegend = F, colorByPoint = TRUE, dataLabels = list(enabled = TRUE, format='{point.y}')) %>%
        hc_add_theme(hc_theme_ffx()) %>%
        hc_tooltip(pointFormat = "{point.y}") 

    })

   output$vbox_spw <- renderValueBox({
    req(input$lgb_filter_country, input$lgb_target_var)
    data <- df_class %>% filter(country %in% input$lgb_filter_country)
    data <- data[, colnames(data) == input$lgb_target_var]
    spw <- as.data.frame(table(data[,1])) 
    spw <- round(spw$Freq[1]/spw$Freq[2], 1)
    valueBox(spw, "scale_pos_weight", icon = icon("plus"), color = 'orange') 

    })

   output$vbox_tot_obs <- renderValueBox({

    req(input$lgb_filter_country)
    valueBox(df_class %>% filter(country %in% input$lgb_filter_country) %>% nrow(), "Total observations", icon = icon("database"), color = 'blue') 

    })

   output$vbox_year_range <- renderValueBox({

    req(input$lgb_filter_country)
    data <- df_class %>% filter(country %in% input$lgb_filter_country)
    min_year <- min(data$year)
    max_year <- max(data$year)
    valueBox(paste0(min_year, "-", max_year), "Data availability by years", icon = icon("calendar"), color = 'olive') 

    })

    output$dt_out_1 <- DT::renderDataTable({

      df_class %>% 
        filter(country %in% input$lgb_filter_country) %>% 
        tail(20) %>% 
        datatable(class="display", options = list(scrollX = TRUE, pageLength = 5, dom = "tp"))
    })

    #-----------------------------------------------------------------------------------------
    # reactive data (feature engineering + split) 
    #-----------------------------------------------------------------------------------------
    data_lgb_model <- reactive({

      data <- df_class %>% 
        filter(country %in% input$lgb_filter_country) %>%
        mutate(suicide_attack = if_else(suicide_attack == "Yes", 1, 0),
               attack_success = if_else(attack_success == "Yes", 1, 0),
               extended = if_else(extended == "Yes", 1, 0),
               individual_attack = if_else(individual_attack == "Yes", 1, 0),
               part_of_multiple_attacks = if_else(part_of_multiple_attacks == "Yes", 1, 0),
               crit1_pol_eco_rel_soc = if_else(crit1_pol_eco_rel_soc == "Yes", 1, 0),
               crit2_publicize = if_else(crit2_publicize == "Yes", 1, 0),
               crit3_os_intl_hmn_law = if_else(crit3_os_intl_hmn_law == "Yes", 1, 0),
               intl_logistical_attack = if_else(intl_logistical_attack == "Yes", 1, 
                                        if_else(intl_logistical_attack == "No", 0, 9)),
               intl_ideological_attack = if_else(intl_ideological_attack == "Yes", 1, 
                                         if_else(intl_ideological_attack == "No", 0, 9)))  


      #-------------------------------------------------------------
      # Step 1: log transformation
      #-------------------------------------------------------------
      data <- data %>%  
        mutate(nkill = log1p(nkill), 
               nwound= log1p(nwound),
               arms_export = log1p(arms_export + 0.01),
               arms_import = log1p(arms_import + 0.01),
               population = log1p(population + 0.01))

      #--------------------------------------------------------------
      # Step 2: Add frequency count features
      #--------------------------------------------------------------
      data <- as.data.table(data)
      data[, n_group_year:=.N,            by=list(group_name, year)]
      data[, n_region_year:=.N,           by=list(region, year)]
      data[, n_city_year:=.N,             by=list(city, year)]
      data[, n_attack_year:=.N,           by=list(attack_type, year)]
      data[, n_target_year:=.N,           by=list(target_type, year)]
      data[, n_weapon_year:=.N,           by=list(weapon_type, year)]
      data[, n_group_region_year:=.N,     by=list(group_name, region, year)]
      data[, n_group:=.N,                 by=list(group_name)]
      data[, n_provstate:=.N,             by=list(provstate)]
      data[, n_city:=.N,                  by=list(city)]
      data <- as.data.frame(data)

      #--------------------------------------------------------------
      # save raw test for model interpretation part
      raw_test_data  <- data %>% filter(year == 2016)   

      #--------------------------------------------------------------
      # Step 3: label encode categorical data (lightgbm requirement)
      #--------------------------------------------------------------

      features= names(data)
      for (f in features) {
        if (class(data[[f]])=="character") {
          levels <- unique(c(data[[f]]))
          data[[f]] <- as.integer(factor(data[[f]], levels=levels))
        }
      }

      data[] <- lapply(data, as.numeric)
      
      #-------------------------------------------------------------
      # Step 4: drop columns with near zero variance
      #-------------------------------------------------------------

      #Ensure that target variable is not removed in nonZeroVars
      # near_zero_vars <- nearZeroVar(data)
      # near_zero_vars <- near_zero_vars[!near_zero_vars %in% input$lgb_target_var]
      # data <- data[ , -near_zero_vars]
      
      #--------------------------------------------------------------
      # Step 5: Create train, test and validation split
      #--------------------------------------------------------------

      train <- data %>% filter(year <= 2014)
      valid <- data %>% filter(year == 2015)
      test  <- data %>% filter(year == 2016) 
     
      data <- list(dfall = data,
                   training_data = train, 
                   validation_data = valid,
                   test_data  = test,
                   raw_test_data = raw_test_data)

      return(data)

    })

    # output$lgb_independent_vars <- renderUI({  
    #   data <- data_lgb_model()$dfall
    #   independent_vars <- names(data)[!(names(data) %in% input$lgb_target_var)]
    #   pickerInput(inputId = "lgb_independent_vars", label = "Independent variables", 
    #               choices = independent_vars, 
    #               selected = independent_vars, options = list(`actions-box` = TRUE), inline = FALSE, multiple = TRUE)
    # })



   output$vbox_train <- renderValueBox({
    data <- data_lgb_model()$training_data
    min_year <- min(data$year)
    max_year <- max(data$year)
    valueBox(paste0(min_year, " - ", max_year), "Training data", icon = icon("database"), color = 'olive') 

    })

   output$vbox_valid <- renderValueBox({
    data <- data_lgb_model()$validation_data
    valueBox(unique(data$year), "Validation data", icon = icon("database"), color = 'olive') 

    })  

   output$vbox_test <- renderValueBox({
    data <- data_lgb_model()$test_data
    valueBox(unique(data$year), "Test data", icon = icon("database"), color = 'olive') 

    })

    output$dt_out_all_split <- DT::renderDataTable({

      data_lgb_model()$dfall %>% 
        head(20) %>% 
        datatable(class="display", options = list(scrollX = TRUE, pageLength = 5, dom = "tp"))
    })

   output$lgb_split_str_train <- renderPrint({ 
    str(data_lgb_model()$training_data)
    })

   output$lgb_split_str_valid <- renderPrint({ 
    str(data_lgb_model()$validation_data)
    })

   output$lgb_split_str_test <- renderPrint({ 
    str(data_lgb_model()$test_data)
    })

  #--------------------------------------------------------------
  # Classification: Lightgbm model tab
  #--------------------------------------------------------------

  #Sidebar Parameters tuning

  output$lgb_learning_rate <- renderUI({ 
        sliderTextInput(inputId = "lgb_learning_rate", label = "Learning rate", choices = c(0.001, 0.01, 0.05, 0.1, 0.15), selected = 0.01, grid = TRUE)
    })

  output$lgb_num_leaves <- renderUI({ 
    sliderInput("lgb_num_leaves", label = "Number of leaves", min = 1, max = 500, value = 7, step = 1)
    })

  output$lgb_max_depth <- renderUI({ 
    sliderInput("lgb_max_depth", label = "Max depth", min = -1, max = 12, value = 6, step = 1)
    })

  output$lgb_bagging_fraction <- renderUI({ 
    sliderInput("lgb_bagging_fraction", label = "Bagging fraction", min = 0.4, max = 1, value = 0.8, step = 0.1)
    })

  output$lgb_bagging_freq <- renderUI({ 
    sliderInput("lgb_bagging_freq", label = "Bagging freq", min = 0, max = 5, value = 1, step = 1)
    })

  output$lgb_feature_fraction <- renderUI({ 
    sliderInput("lgb_feature_fraction", label = "Feature fraction", min = 0.4, max = 1, value = 0.9, step = 0.1)
    })

  output$lgb_scale_pos_weight <- renderUI({ 
    searchInput(inputId = "lgb_scale_pos_weight", 
                label = "scale_pos_weight", 
                placeholder = "Enter numeric value...", 
                value = "1",
                btnSearch = icon("plus-square"), 
                # btnReset = icon("remove"), 
                width = "100%"
              )
    })

  spw_input <- reactive({as.numeric(input$lgb_scale_pos_weight)})

  output$lgb_nrounds <- renderUI({ 
    sliderInput("lgb_nrounds", label = "nrounds", min = 500, max = 5000, value = 3000, step = 500)
    })

  output$lgb_early_stopping_rounds <- renderUI({ 
    sliderInput("lgb_early_stopping_rounds", label = "Early stopping rounds", min = 10, max = 100, value = 50, step = 10)
    })

  output$lgb_eval_freq <- renderUI({ 
    sliderInput("lgb_eval_freq", label = "Evaluation freq", min = 1, max = 100, value = 50, step = 10)
    })


    #-----------------------------------------------------------------------------------------
    # reactive data (model, evaluation, predictions) 
    #-----------------------------------------------------------------------------------------
    model_data <- eventReactive(c(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country),{

      if(is.null(c(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country))){
        return()
      }

      dfall <- data_lgb_model()$dfall
      train <- data_lgb_model()$training_data
      valid <- data_lgb_model()$validation_data
      test  <- data_lgb_model()$test_data

      dtest <- as.matrix(test[, colnames(test) != input$lgb_target_var])
      
      # define all categorical features
      all_cat_vars <- df %>% select(year, month, day, conflict_index, region, country, provstate, city, 
                                    attack_type, target_type, weapon_type, target_nalty, group_name,
                                    crit1_pol_eco_rel_soc, crit2_publicize, crit3_os_intl_hmn_law,
                                    part_of_multiple_attacks, individual_attack, attack_success, intl_logistical_attack,
                                    intl_ideological_attack) %>% names()
      
      # Select features that are present in prepared data
      categorical_features <- names(dfall)[names(dfall) %in% all_cat_vars]
      categorical_features <- categorical_features[!categorical_features %in% input$lgb_target_var]

      train_label <- train[, input$lgb_target_var]
      valid_label <- valid[, input$lgb_target_var]

      dtrain = lgb.Dataset(data = as.matrix(train[, colnames(train) != input$lgb_target_var]), 
                           label = train_label, categorical_feature = categorical_features)
      dvalid = lgb.Dataset(data = as.matrix(valid[, colnames(valid) != input$lgb_target_var]), 
                           label = valid_label, categorical_feature = categorical_features)

      params <- list(objective = "binary", 
                    metric = "auc", 
                    num_leaves = input$lgb_num_leaves,
                    max_depth = input$lgb_max_depth,
                    bagging_fraction = input$lgb_bagging_fraction,
                    bagging_freq = input$lgb_bagging_freq,
                    feature_fraction = input$lgb_feature_fraction,
                    learning_rate = input$lgb_learning_rate,
                    scale_pos_weight = spw_input()
                    ) 


      cat("--------------------------------------", "\n")
      cat("Model performance on validation data: ", "\n")
      cat("--------------------------------------", "\n")

      tic("Total time for modeling: ")
      model <- try(
                lgb.train(params, 
                  dtrain, 
                  valids = list(validation = dvalid), 
                  nrounds = input$lgb_nrounds, 
                  early_stopping_rounds = input$lgb_early_stopping_rounds,
                  eval_freq = input$lgb_eval_freq)
                )
      toc()

      cat("\n")
      cat("--------------------------------------", "\n")
      cat("Validation AUC @ best iter: ", max(unlist(model$record_evals[["validation"]][["auc"]][["eval"]])), "\n")
      cat("--------------------------------------", "\n", "\n")

      # get predictions on validation data (for model interpretation)
      test_preds <- predict(model, data = dtest, n = model$best_iter)

      # get feature importance
      fi <- lgb.importance(model, percentage = TRUE)
      fi <- as.data.frame(fi, rownames = FALSE)

      cat("--------------------------------------", "\n")
      cat("Feature Importance Matrix (Top 5): ", "\n")
      cat("--------------------------------------")
      print(knitr::kable(head(fi, 5), format = "markdown"))

      data <- list(model = model,
                   test = test,
                   dtest = dtest,
                   dtrain = dtrain,
                   dvalid = dvalid,
                   test_preds = test_preds,
                   fi = fi)

      return(data)

    })


  observe({
      if (input$lgb_model_output %in% c("tab_fi", "tab_fid", "tab_tbl")) {
        disable("lgb_filter_country")
        disable("lgb_target_var")
      } else {
        enable("lgb_filter_country")
        enable("lgb_target_var")
      }
    })

  # output$vbox_nthread <- renderValueBox({
  #   valueBox(detectCores(), "CPU cores", icon = icon("microchip"), color = 'green') 
  #   })

  output$vbox_avil_mem <- renderValueBox({
    valueBox(paste0(round(memory.limit()/1000,0), " GB"), "RAM capacity", icon = icon("microchip"), color = 'olive') 
    })

  output$vbox_used_mem <- renderValueBox({
    req(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country)
    valueBox(paste0(round(mem_used()/1000000000,1), " GB"), "RAM in use", icon = icon("microchip"), color = 'blue') 
    })


  output$lgb_console_out <- renderPrint({

      req(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country)

      cat("Target country : ", input$lgb_filter_country, "\n")
      cat("Target variable: ", input$lgb_target_var, "\n", "\n")

      model <- model_data()$model
    })


  output$tbl_lgb_fi <-  function() {

      req(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country)

      tbl <- model_data()$fi 

      knitr::kable(tbl) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(1:4, color = "black", background = "#dee2ed") %>%
        column_spec(1, bold = T, color = "black", background = "#dee2ed") %>%
        column_spec(2, color = "black", background = "#c7cfe5") 

    }

    output$plot_lgb_fi <- renderHighchart({

      req(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country)

      fi <- model_data()$fi %>% head(15)

      label <- ifelse(input$lgb_target_var == "suicide_attack", "Suicide attack",
               ifelse(input$lgb_target_var == "attack_success", "Attack success",
               ifelse(input$lgb_target_var == "extended", "Extended attack (>24 hrs)",
               ifelse(input$lgb_target_var == "part_of_multiple_attacks", "Part of multiple attacks",
               ifelse(input$lgb_target_var == "crit1_pol_eco_rel_soc", "Political/ Eco/ Rel/ Social goal",
               ifelse(input$lgb_target_var == "crit2_publicize", "Intention to publicize",
               ifelse(input$lgb_target_var == "crit3_os_intl_hmn_law", "Outside intl humanitarian law", "")))))))

      highchart() %>% 
        hc_title(text = "Top 15 Most Important Features") %>%
        hc_subtitle(text = paste0("Target variable : ", label)) %>%
        hc_xAxis(categories = fi$Feature) %>%
        hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y = Gain)) %>%
        hc_add_theme(hc_theme_ffx())

    })

    output$plot_lgb_fi_all <- renderHighchart({

      req(input$btn_lgb_model, input$lgb_target_var, input$lgb_filter_country)

      fi <- model_data()$fi %>% head(10)

      label <- ifelse(input$lgb_target_var == "suicide_attack", "Suicide attack",
               ifelse(input$lgb_target_var == "attack_success", "Attack success",
               ifelse(input$lgb_target_var == "extended", "Extended attack (>24 hrs)",
               ifelse(input$lgb_target_var == "part_of_multiple_attacks", "Part of multiple attacks",
               ifelse(input$lgb_target_var == "crit1_pol_eco_rel_soc", "Political/ Eco/ Rel/ Social goal",
               ifelse(input$lgb_target_var == "crit2_publicize", "Intention to publicize",
               ifelse(input$lgb_target_var == "crit3_os_intl_hmn_law", "Outside intl humanitarian law", "")))))))

      highchart() %>%
        hc_title(text = "Feature importance by Cover, Gain and Frequency") %>%
        hc_subtitle(text = paste0("Target variable : ", label)) %>%
        hc_xAxis(categories = fi$Feature) %>%
        hc_add_series(name = "Cover", data = fi, type = "bar", hcaes(x = Feature, y = Cover)) %>%
        hc_add_series(name = "Gain", data = fi, type = "bar", hcaes(x = Feature, y= Gain)) %>%
        hc_add_series(name = "Frequency", data = fi, type = "bar", hcaes(x = Feature, y = Frequency)) %>%
        hc_add_theme(hc_theme_ffx()) 

    })

    output$dt_out_lgb_test <- DT::renderDataTable({

      raw_test_data <- data_lgb_model()$raw_test_data

      raw_test_data %>% 
        datatable(class="display", options = list(scrollX = TRUE, pageLength = 5, dom = "tp"))
    })

    output$lgb_select_test_obs <- renderUI({  

      test <- model_data()$test

      selectInput(inputId = "lgb_select_test_obs", 
                  label = "Select an observation from test set", 
                  choices = seq(1:nrow(test)), 
                  selected = 1) 
    })

    test_pred_index <- reactive({as.numeric(input$lgb_select_test_obs)})

    output$tbl_lgb_test_obs_predicted <- renderPrint({

      test_preds <- model_data()$test_preds
      cat(paste("Predicted value from the model: ", round(test_preds[[test_pred_index()]], 3)))

    })

    output$tbl_lgb_test_obs <- function() {

      test <- model_data()$test
      observation <- t(test[test_pred_index(), ])

      knitr::kable(observation) %>% 
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
        column_spec(1, bold = T, color = "black", background = "#dee2ed")

    }

    output$plot_lgb_explainer <- renderHighchart({

      #extract interpretation for 1st observation in validation data
      test_matrix <- model_data()$dtest
      model <- model_data()$model
      tree_interpretation <- lgb.interprete(model, data = test_matrix, idxset = test_pred_index())
      
      tree_interpretation <- as.data.frame(rbindlist(tree_interpretation)) %>% head(10)
      tree_interpretation$Contribution <- round(tree_interpretation$Contribution, 2)

      highchart() %>% 
        hc_title(text = "Model/ Tree Interpretation by Features Contribution") %>%
        hc_subtitle(text = "Red/ Positive value means support | Blue/ Negative vlaue means contradict") %>%
        hc_add_series_labels_values(tree_interpretation$Feature, tree_interpretation$Contribution, showInLegend=F,
                                    dataLabels = list(enabled = TRUE),
                                    colors = ifelse(tree_interpretation$Contribution >= 0, "#ce1e36", "#0d6bc6"),
                                    type = "bar") %>% 
        hc_yAxis(title = list(text = "Contribution"), labels = list(format = "{value}")) %>% 
        hc_xAxis(categories = tree_interpretation$Feature, title = list(text = "Feature")) %>% 
        hc_add_theme(hc_theme_ffx()) %>%
        hc_tooltip(pointFormat = "{point.y}")


    })

    output$gtd_codebook <- renderUI({

      tags$iframe(style="height:650px; width:100%", src="Codebook.pdf")


    })


  }) # End of shinyServer logic