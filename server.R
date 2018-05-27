
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
        sidebar_data <- df_leaflet_t10 %>% filter(group_name %in% top10_groups) # filter data for top 10 deadliest groups
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
                                         ifelse(data$nkill >= 100 & data$nkill < 300, 4, 
                                         ifelse(data$nkill >= 300 & data$nkill < 500, 8, 
                                         ifelse(data$nkill >= 500 & data$nkill < 1000, 12, 20)))),
                              animate = TRUE, 
                              heartbeat = ifelse(data$nkill < 100, 0.99, 
                                          ifelse(data$nkill >= 100 & data$nkill < 300, 0.87, 
                                          ifelse(data$nkill >= 300 & data$nkill < 500, 0.77, 
                                          ifelse(data$nkill >= 500 & data$nkill < 1000, 0.67, 0.4))))
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
  
  output$radioBtn_ldata2 <- renderUI ({
      radioButtons(inputId = "radioBtn_ldata2", label = "Select data :", choices = c("T10 Groups", "All"), selected = "T10 Groups", inline = TRUE)
  })

  #-------------------------------------
  # Reactive data for sidebar selection
  #-------------------------------------

  plotly_3d_data <- reactive({

      if(input$radioBtn_ldata2 == "T10 Groups") {
        data <- df_leaflet_t10 %>% filter(group_name %in% top10_groups) # filter data for top 10 deadliest groups
      } else {
        data <- df_leaflet # select everything
      }

      return(data)

    })


  output$select1_xvar <- renderUI({ 
    vars <- plotly_3d_data() %>% names()
    selectizeInput(inputId = "select1_xvar", label = "X var", multiple = F, choices = vars, selected = NULL)  
  })


  output$select2_yvar <- renderUI({
    vars <- plotly_3d_data() %>% names()
    selectizeInput(inputId = "select2_yvar", label = "Y var", multiple = F, choices = vars, selected = NULL) 
  })

  output$select3_zvar <- renderUI({
    vars <- plotly_3d_data() %>% names()
    selectizeInput(inputId = "select3_zvar", label = "Z var", multiple = F, choices = vars, selected = NULL) 
  })

  output$select4_colvar <- renderUI({
    vars <- plotly_3d_data() %>% names()
    selectizeInput(inputId = "select4_colvar", label = "Color var", multiple = F, choices = vars, selected = NULL)  
  })

  # observeEvent(c(input$picker2_yvar, input$picker3_zvars, input$picker4_colvar),{
  #   vars <- sidebar_data() %>% select(-c(input$picker2_yvar, input$picker3_zvars, input$picker4_colvar)) %>% names()
  #   updatePickerInput(session = session, inputId = "picker1_xvar", choices = vars)
  # }) 


  output$plotly_1 <- renderPlotly({
    req(input$select1_xvar, input$select2_yvar, input$select3_zvar, input$select4_colvar)
              
    # df_leaflet_t10 %>%
    #   replace_na(list(nkill = 0, nwound = 0)) %>%
    #   mutate(group_name = as.factor(group_name), year = as.factor(as.character(year))) %>%
    #   plot_ly(x = ~nkill, y = ~nwound, z = ~year, color = ~group_name, colors = ~viridis(length(unique(group_name)))) %>%
    #   add_markers(opacity = 0.6) %>%
    # layout(title = "Terror Attacks from Top 10 Groups", 
    #        paper_bgcolor= "#f7f7f7",
    #        legend = list(orientation = 'h', xanchor = "center", x = 0.5),
    #        annotations=list(yref='paper',xref="paper", text = "Terror attacks", showarrow=F), 
    #        scene = list(xaxis = list(title = 'nkill'),
    #                     yaxis = list(title = 'nwound'),
    #                     zaxis = list(title = 'year')))
    
    data <- plotly_3d_data()
    data$xvar <- input$select1_xvar
    data$yvar <- input$select2_yvar
    data$zvar <- input$select3_zvar
    data$colvar <- input$select4_colvar

    data %>%
      plot_ly(x = ~get(input$select1_xvar), 
              y = ~get(input$select2_yvar), 
              z = ~get(input$select3_zvar), 
              color = ~get(input$select4_colvar), colors = ~viridis(length(unique(get(input$select4_colvar))))) %>%
      add_markers(opacity = 0.6) %>%
    layout(title = "Terror Attacks from Top 10 Groups", 
           paper_bgcolor= "#f7f7f7",
           legend = list(orientation = 'h', xanchor = "center", x = 0.5),
           annotations=list(yref='paper',xref="paper", text = "Terror attacks", showarrow=F), 
           scene = list(xaxis = list(title = 'xvar'),
                        yaxis = list(title = 'yvar'),
                        zaxis = list(title = 'zvar')))

      
   })


  }) # End of shinyServer logic



