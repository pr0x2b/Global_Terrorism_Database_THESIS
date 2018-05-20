

# load clean data (GTD)
df <- readRDS("gtd_clean.rds")
countries <- readRDS("countries.rds") # countries mapped with iso3c codes for worldmap


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
    output$top10_hc1 <- renderHighchart({

      by_groups <- df %>% group_by(group_name) %>% summarise(total = n()) %>% filter(group_name != "Unknown") %>% arrange(desc(total)) %>% head(10)
      top10_groups <- as.vector(by_groups$group_name)
      group_attack_type <- df %>% filter(group_name %in% top10_groups) %>% group_by(attack_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)
      group_target_type <- df %>% filter(group_name %in% top10_groups) %>% group_by(target_type) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(10)

      highchart(height = "500px") %>% 
        hc_title(text = "Top 10 Deadliest Terrorist Groups and Their Priority Attack and Target Types") %>%
        hc_subtitle(text = "Piechart 1: Attack type | Piechart 2: Target types") %>%
        hc_add_theme(hc_theme_sandsignika()) %>%
        hc_add_series_labels_values(by_groups$group_name, by_groups$total, name = "Show/ hide bar chart", showInLegend=F,
                                    dataLabels = list(align = "center", enabled = TRUE),
                                    colors = substr(heat.colors(10), 0 , 7),
                                    colorByPoint = TRUE, type = "column") %>% 
        hc_add_series_labels_values(group_attack_type$attack_type, group_attack_type$count, name = "Pie chart- Attack types", 
                                    colors = substr(heat.colors(9), 0 , 7),
                                    type = "pie", innerSize= '40%', size= "25%", showInLegend=F,
                                    colorByPoint = TRUE, center = c('50%', '20%'),
                                    size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
        hc_add_series_labels_values(group_target_type$target_type, group_target_type$count, name = "Pie chart- Target types", 
                                    colors = substr(heat.colors(10), 0 , 7),
                                    type = "pie", innerSize= '40%', size= "25%", showInLegend=F,
                                    colorByPoint = TRUE, center = c('85%', '20%'),
                                    size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
        hc_yAxis(title = list(text = "Total number of terror attacks"),
                 labels = list(format = "{value}"), max = 6580) %>% 
        hc_xAxis(categories = by_groups$group_name, title = list(text = "Name of terrorist group")) %>% 
        hc_legend(enabled = T, align= "left", verticalAlign = "bottom") %>% 
        hc_tooltip(pointFormat = "{point.y}") %>%
        hc_credits(enabled = TRUE, 
                 text = "Sources: Global Terrorism Database (START Consortium)",
                 style = list(fontSize = "12px"))

      })



  }) # End of shinyServer logic



