
shinyServer(function(input, output, session) {


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
    # Value boxes (p1 global overview) 
    #-------------------------------------
      tmp <- df %>% group_by(part_of_multiple_attacks) %>% summarise(total = n())
      val_mult <- round(tmp %>% filter(part_of_multiple_attacks == 1) %>% select(total) / sum(tmp$total) * 100)
      tmp <- df %>% group_by(intl_logistical_attack) %>% summarise(total = n())
      val_lia1 <- round(tmp %>% filter(intl_logistical_attack == 1) %>% select(total) / sum(tmp$total) * 100)
      tmp <- df %>% group_by(intl_logistical_attack) %>% summarise(total = n())
      val_lia0 <- round(tmp %>% filter(intl_logistical_attack == 0) %>% select(total) / sum(tmp$total) * 100)
      tmp <- df %>% group_by(intl_ideological_attack) %>% summarise(total = n())
      val_id1 <- round(tmp %>% filter(intl_ideological_attack == 1) %>% select(total) / sum(tmp$total) * 100)
      tmp <- df %>% group_by(intl_ideological_attack) %>% summarise(total = n())
      val_id0 <- round(tmp %>% filter(intl_ideological_attack == 0) %>% select(total) / sum(tmp$total) * 100)

     output$countries_affected <- renderValueBox({
        valueBox(df %>% group_by(country) %>% summarise(count = n()) %>% nrow(),
                 "Affected countries", icon = icon("flag-o"), color = 'blue') })
    
    output$mult_attacks <- renderValueBox({
      valueBox(paste0(val_mult, "%"), "Part of multiple", icon = icon("users"), color = 'blue') })
  
     output$attack_log_intl <- renderValueBox({
        valueBox(paste0(val_lia1, "%"), "Logistically intl",icon = icon("globe"), color = 'olive') })

     output$attack_log_domestic <- renderValueBox({
       valueBox(paste0(val_lia0, "%"), "Logistically domestic",icon = icon("home"), color = 'olive') })
     
     output$attack_ideo_intl <- renderValueBox({
       valueBox(paste0(val_id1, "%"), "Ideologically intl",icon = icon("globe"), color = 'orange') })
     
     output$attack_ideo_domestic <- renderValueBox({
       valueBox(paste0(val_id0, "%"), "Ideologic. domestic",icon = icon("home"), color = 'orange') })


    #-------------------------------------
    # Deadlist group top10_hc1
    #-------------------------------------

      # by_groups <- df %>% group_by(group_name) %>% summarise(total = n()) %>% filter(group_name != "Unknown") %>% arrange(desc(total)) %>% head(10)
      # top10_groups <- as.vector(by_groups$group_name)
      group_attack_type <- df %>% filter(group_name %in% top10_groups) %>% group_by(attack_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10) 
      group_target_type <- df %>% filter(group_name %in% top10_groups) %>% group_by(target_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)
      group_target_nalty <- df %>% filter(group_name %in% top10_groups) %>% group_by(target_nalty) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)
      group_year <- df %>% filter(group_name %in% top10_groups) %>% group_by(year) %>% summarize(count = n())
      group_weapon_type <- df %>% filter(group_name %in% top10_groups) %>% group_by(weapon_type) %>% summarize(count = n()) %>% arrange(desc(count))
      group_target_country <- df %>% filter(group_name %in% top10_groups) %>% group_by(country) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)


    output$top10_hc1 <- renderHighchart({
      
      highchart(height = "400px") %>% 
        hc_title(text = "Characteristics of Top 10 Deadliest Terrorist Groups") %>%
        hc_subtitle(text = "By total number of fatalities (nkill + nwound) and from year 2000 onward") %>%
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
                 labels = list(format = "{value}"), max = 45000) %>% 
        hc_xAxis(categories = by_groups$group_name, title = list(text = "Name of the terrorist group")) %>% 
        hc_legend(enabled = T, align= "left", verticalAlign = "bottom") %>% 
        hc_tooltip(pointFormat = "{point.y}") %>%
        hc_credits(enabled = TRUE, 
                 text = "Sources: Global Terrorism Database (START Consortium)",
                 style = list(fontSize = "12px"))

      })

    #-------------------------------------
    # hc1 ~ pie chart 1, 2 (Attack + target type)
    #-------------------------------------
    output$top10_hc1_attack_type <- renderHighchart({

      highchart(height = "250px") %>% 
        hc_title(text = "Frequent Attack Types and Target Types") %>%
        hc_add_theme(hc_theme_sandsignika()) %>%
        hc_add_series_labels_values(group_attack_type$attack_type, group_attack_type$count, 
                                    colors = substr(heat.colors(9), 0 , 7),
                                    type = "pie", innerSize= '40%', size= "50%", showInLegend=F,
                                    colorByPoint = TRUE, center = c('31%', '50%'),
                                    size = 100, dataLabels = list(align = "left", enabled = TRUE)) %>%
        hc_add_series_labels_values(group_target_type$target_type, group_target_type$count, 
                                    colors = substr(heat.colors(10), 0 , 7),
                                    type = "pie", innerSize= '40%', size= "50%", showInLegend=F,
                                    colorByPoint = TRUE, center = c('77%', '50%'),
                                    size = 100, dataLabels = list(align = "right", enabled = TRUE)) %>%
        hc_tooltip(pointFormat = "{point.y}")

      })

    #-------------------------------------
    # hc1 ~ pie chart 3 (target nationality)
    #-------------------------------------
    output$top10_hc1_target_naltly <- renderHighchart({

      highchart(height = "400px") %>% 
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
      
      hchart(group_year, "line", hcaes(year, count)) %>%
        hc_yAxis(title = list(text = "Total attacks"), labels = list(format = "{value}"), max = 5000) %>% 
        hc_title(text = "Yearwise Activity") %>%
        hc_add_theme(hc_theme_sandsignika()) %>%
        hc_tooltip(pointFormat = "{point.y}")

      })

  #-------------------------------------
  # EDA p2 (Leaflet sidebar menu)
  #-------------------------------------

  output$leaflet_year <- renderUI({ 
        sliderInput("leaflet_year", label = "Year range", min = 1970, max = 2016, value = c(2000,2016)) 
    })

  output$leaflet_country <- renderUI({ 
        pickerInput(inputId = "leaflet_country", label = "Country", choices = unique(df_leaflet$country), 
          selected = unique(df_leaflet$country), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
    })

  output$leaflet_group <- renderUI({ 
        pickerInput(inputId = "leaflet_group", label = "Terrorist group", choices = unique(df_leaflet$group_name), 
          selected = unique(df_leaflet$group_name), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
    })

  output$leaflet_attack <- renderUI({ 
        pickerInput(inputId = "leaflet_attack", label = "Attack type", choices = unique(df_leaflet$attack_type), 
          selected = unique(df_leaflet$attack_type), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
    })

  output$leaflet_target <- renderUI({ 
        pickerInput(inputId = "leaflet_target", label = "Target type", choices = unique(df_leaflet$target_type), 
          selected = unique(df_leaflet$target_type), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
    })

  output$leaflet_weapon <- renderUI({ 
        pickerInput(inputId = "leaflet_weapon", label = "Weapon type", choices = unique(df_leaflet$weapon_type), 
          selected = unique(df_leaflet$weapon_type), options = list(`actions-box` = TRUE), multiple = T, inline = FALSE) 
    })

  output$checkBtn1_suicide <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn1_suicide", label = "Suicide attack? ", choices = c("Yes", "No"), selected = "No",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn2_multiple <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn2_multiple", label = "Multiple attacks?", choices = c("Yes", "No"), selected = "Yes",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn3_log_int <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn3_log_int", label = "Logistically intl?", choices = c("Yes", "No"), selected = "Yes",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn4_ido_int <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn4_ido_int", label = "Ideologically intl?", choices = c("Yes", "No", "Unknown"), selected = "Yes",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn5_crit1 <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn5_crit1", label = "Political/ Soc. goal?", choices = c("Yes", "No"), selected = "Yes",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn6_crit2 <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn6_crit2", label = "Intention to coerce?", choices = c("Yes", "No"), selected = "Yes",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })

  output$checkBtn7_crit3 <- renderUI({ 
        checkboxGroupButtons(inputId = "checkBtn7_crit3", label = "O/s Geneva Convtns?", choices = c("Yes", "No"), selected = "No",
                             size = "sm", checkIcon = list(yes = icon("ok", lib = "glyphicon"))) 
    })


  observe({ 
    print(input$leaflet_weapon) 

    })

  # reactive data
  leaflet_plot_data <- reactive({

    data <- df_leaflet[df_leaflet$year >= input$leaflet_year[1] & df_leaflet$year <= input$leaflet_year[2], ]
    data <- data[data$country %in% input$leaflet_country, ]
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
    })

  # dfl_data <- reactive({

    # if(input$leaflet_region != "All"){ df_leaflet$region = leaflet_region %in% input$leaflet_region }
    #   else{ df_leaflet$region = df_leaflet$region }

    # if(input$leaflet_group != "All"){ df_leaflet$group_name = leaflet_group %in% input$leaflet_group }
    #   else{ df_leaflet$group_name = df_leaflet$group_name }

    # if(input$leaflet_attack != "All"){ df_leaflet$attack_type = leaflet_attack %in% input$leaflet_attack }
    #   else{ df_leaflet$attack_type = df_leaflet$attack_type }

    # if(input$leaflet_target != "All"){ df_leaflet$target_type = leaflet_target %in% input$leaflet_target }
    #   else{ df_leaflet$target_type = df_leaflet$target_type }

    # if(input$leaflet_weapon != "All"){ df_leaflet$weapon_type = leaflet_weapon %in% input$leaflet_weapon }
    #   else{ df_leaflet$weapon_type = df_leaflet$weapon_type }

    # if(input$checkBtn1_suicide != "Yes" & input$checkBtn1_suicide != "No"){ df_leaflet$suicide_attack = df_leaflet$suicide_attack }
    #   else{ df_leaflet$suicide_attack = input$checkBtn1_suicide}

  #   data <- df_leaflet %>%
  #     filter(year >= input$leaflet_year[1] & year <= input$leaflet_year[2]) %>%
  #     filter(ifelse(input$leaflet_region == "All", region == region, region == input$leaflet_region))

  #   return(data)

  # })

  #-------------------------------------
  # EDA p2 (Leaflet plot output)
  #-------------------------------------
  output$top_10_dg_leaflet <- renderLeaflet({

    data <- leaflet_plot_data()

    leaflet() %>%
    setView(40, 20, zoom= 2) %>% 
    addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
    attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>,
      <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>
      &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
    addScaleBar() %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Black") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Standard") %>%
    addLayersControl(baseGroups = c("Black", "Standard"), options = layersControlOptions(collapsed = FALSE)) %>%
    addCircles(data = data, lat= ~latitude, lng = ~longitude, 
                popup=paste(
                  "<strong>Date: </strong>", data$date,
                  "<br><strong>Country : </strong>", data$country,
                  "<br><strong>City : </strong>", data$city,
                  "<br><strong>Number of people killed : </strong>", data$nkill,
                  "<br><strong>Number of people wounded : </strong>", data$nwound,
                  "<br><strong>Attack type: </strong>", data$attack_type,
                  "<br><strong>Weapon type: </strong>", data$weapon_type,
                  "<br><strong>Target type: </strong>", data$target_type,
                  "<br><strong>Group: </strong>", data$group_name,
                  "<br><strong>Attack > 24 hours?: </strong>", data$extended,
                  "<br><strong>Suicide attack?: </strong>", data$suicide_attack,
                  "<br><strong>Part of multiple attacks?: </strong>", data$part_of_multiple_attacks),
                weight = 1.3, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)
  
    
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





  }) # End of shinyServer logic



