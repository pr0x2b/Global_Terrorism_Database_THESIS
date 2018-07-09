

#---------------------------------------------------------------------------------------------------------------------  
# Dashboard header
#---------------------------------------------------------------------------------------------------------------------  
header <- dashboardHeader(
    title = "Data-driven Counter Terrorism Support",
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 55px}"),
            tags$style(".main-header .logo {height: 55px; }"),
            tags$style(".navbar {min-height:55px !important}")),
    titleWidth = 400,
    # links to my social and professional profiles
    tags$li(class = "dropdown", tags$a(href = "https://www.kaggle.com/pranav84", target = "_blank", tags$img(height = "25px", src = "kaggle.png"))),
    tags$li(class = "dropdown", tags$a(href = "https://twitter.com/pranavpandya84", target = "_blank", tags$img(height = "25px", src = "twitter.png"))),
    tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/pranav84/", target = "_blank", tags$img(height = "25px", src = "linkedin.png")))) 

#---------------------------------------------------------------------------------------------------------------------  
# Dashboard sidebar
#---------------------------------------------------------------------------------------------------------------------  
sidebar <- dashboardSidebar(
    # width = 280,
    sidebarMenu(id = "gtd_pranav",
        # tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})'))),
      menuItem("Master Thesis", tabName = "master_thesis", icon = icon("home")),
      menuItem("About GTD", tabName = "about_gtd", icon = icon("home")),
      menuItem("Part 1: Impact Analysis", icon = icon("globe"),tabName = "part_1",
        menuSubItem("GUI: Geographical", icon = icon("globe"),tabName = "eda_p1_geographic"),
        menuSubItem("Global Attack Patterns", icon = icon("globe"),tabName = "eda_p1_2_heatmaps")),
      menuItem("Part 2: Active Groups", tabName = "eda_p2", icon = icon("users")),
      menuItem("Part 3: Statistical Analysis", tabName = "gui", icon = icon("bar-chart-o")),
      menuItem("Part 4: Time-series Analysis", tabName = "ts_analysis_01_season", icon = icon("calendar")),
      menuItem("Part 5: classification", tabName = "classification", icon = icon("gears")),
      menuItem("Part 6: Insights", tabName = "predictions", icon = icon("globe")),
      br(), br(), br(), 
      menuItem("Author: Pranav Pandya", tabName = "author", icon = icon("user"))
      # h5("Pranav Pandya", style="text-align:center;"),
      # h5("Master BIPM 2016-18", style="text-align:center;"),
      # h5("Berlin School of Economics & Law", style="text-align:center;")
      #img(src = "hwr.png", width = 230, style="display: block; margin-left: auto; margin-right: auto;")
    )

    )

#---------------------------------------------------------------------------------------------------------------------  
# Dashboard body
#---------------------------------------------------------------------------------------------------------------------  

body <- dashboardBody(
  # shinyDashboardThemes(theme = "grey_dark"),
  useShinyjs(), 
  tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"),
  extendShinyjs(text = "shinyjs.activateTab = function(name){
                        setTimeout(function(){
                        $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
                        }, 200);}"),
  tabItems(
    #---------------------------------------
    # Home page: Quick overview about GTD
    #---------------------------------------

    tabItem(tabName = "master_thesis",
      navbarPage("Master Thesis", id = "master_thesis", 
        tabPanel("pdf version", 
          fluidPage(title = "thesis_pdf", 
            fluidRow(
              column(width = 12,
                uiOutput("thesis_pdf"))
              )
            )
          ), 
        tabPanel("gitbook version", 
          fluidPage(title = "thesis_gitbook", 
            fluidRow(
              # column(width = 12,
              #   uiOutput("thesis_gitbook"))
              )
            )
          )
        )
      ),

    tabItem(tabName = "about_gtd",
      navbarPage("Global Terrorism Database", id = "about_gtd", 

        tabPanel("App Overview", 
          fluidPage(title = "about_gtd", 

            fluidRow(
              column(width = 4,
                box(
                  h3("App summary"),
                  style = "font-size: 110%; ", background = "blue", width = 15, solidHeader = FALSE,
                  tags$ul(
                    tags$li("Part 1: Global impact analysis"), 
                    tags$li("Part 2: Determining most active groups"), 
                    tags$li("Part 3: Statistical analysis"), 
                    tags$li("Part 4: Algorithmic decision support"), 
                    tags$li("Part 5: Interpretations/ Insights")), 
                  p("Note: Algorithmic decision support will include various binary and multiclass classification models.")),

                box(
                  style = "font-size: 110%; ", background = "blue", width = 15, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                  h3("About data source: "),
                  tags$ul(
                    tags$li("Time period: 1970-2016, except 1993"), 
                    tags$li("Variables: based on location, tactics, perpetrators, targets and outcomes"), 
                    tags$li("Sources: Unclassified media articles")), p(""), 
                  p("The Global Terrorism Database (GTD) is an open-source database including information on over 170,000 terrorist events around the world 
                     from 1970 through 2016. It is the most comprehensive unclassified database on terrorist events in the world."), 
                  p("GTD is maintained by researchers at the National Consortium for the Study of Terrorism and Responses to Terrorism (START), 
                      headquartered at the University of Maryland."), 
                  p("Note: Some of the variable names have been renamed to keep the analysis informative for audience."))
                ),

              column(width = 8,
                img(src = "munich_image.jpg",height = 140, width = 950, style="display: block; margin-left: auto; margin-right: auto;"), 
                withSpinner(highchartOutput("world_hchart",height = 520)))
              ),

            fluidRow(
              column(width = 12,
                # actionButton("info_box", " Detailed information", icon = icon("info-circle")),
                # bsModal("modalExample", "info_box", size = "large", 
                  box(
                    h3("Definition of Terrorism: "),
                    style = "font-size: 109%; ", width = 16, solidHeader = TRUE,
                    p("The GTD defines a terrorist attack as the threatened or actual use of illegal force and violence by a non-state actor to attain a 
                        political, economic, religious, or social goal through fear, coercion, or intimidation."),
                    p("In practice this means in order to consider an incident for inclusion in the GTD, all three of the following attributes must be present:"),
                    tags$ul(
                      tags$li("The incident must be intentional – the result of a conscious calculation on the part of a perpetrator."), p(""),
                      tags$li("The incident must entail some level of violence or immediate threat of violence including property violence, as well as violence against people."),  p(""),
                      tags$li("The perpetrators of the incidents must be sub-national actors. GTD does not include acts of state terrorism.")),
                    p("Additionally, at least two of the following three criteria must be present for an incident to be included in the GTD:"),
                    tags$ul(
                      tags$li("Criterion 1: The act must be aimed at attaining a political, economic, religious, or 
                           social goal. In terms of economic goals, the exclusive pursuit of profit does not satisfy this 
                           criterion. It must involve the pursuit of more profound, systemic economic change."), p(""),
                      tags$li("Criterion 2: There must be evidence of an intention to coerce, intimidate, or convey some other message 
                           to a larger audience (or audiences) than the immediate victims. It is the act taken as a totality that is 
                           considered, irrespective if every individual involved in carrying out the act was aware of this intention. 
                           As long as any of the planners or decision-makers behind the attack intended to coerce, intimidate or publicize, 
                           the intentionality criterion is met."),  p(""),
                      tags$li("Criterion 3: The action must be outside the context of legitimate warfare activities. That is, the act must be 
                             outside the parameters permitted by international humanitarian law (particularly the prohibition against deliberately 
                             targeting civilians or non-combatants)."))
                      ))
                  )
            )), # End of overview tab panel


        tabPanel("Codebook/ Reference Manual", 
          fluidPage(title = "GTD Reference Manual", 
            fluidRow(
              column(width = 12,
                uiOutput("gtd_codebook"))
              )
            )
          )


      ) 
    ),


    #-------------------------------------------- 
    # section 1.1: Geographic Visualization (Leaflet)
    #-------------------------------------------- 
    tabItem(tabName = "eda_p1_geographic",
      fluidPage(title = "tdg_eda_p1_geographic",
        fluidRow(h3("GUI: Exploration of Attack Characteristics"),
          # tags$style(HTML(".tab-panel{ background-color: black; color: white}")),
            tabBox(width = 2, 
              tabPanel("Plot configurations:",  
                uiOutput("radioBtn_ldata"),
                uiOutput("radioBtn_major_attacks"),
                uiOutput("leaflet_year"),
                uiOutput("leaflet_region"),
                uiOutput("leaflet_group"),
                uiOutput("leaflet_attack"),
                uiOutput("leaflet_weapon"),
                uiOutput("leaflet_target"),
              # tabPanel("configs 2", 
                uiOutput("checkBtn1_suicide"),
                uiOutput("checkBtn2_multiple"),
                uiOutput("checkBtn3_log_int"), 
                uiOutput("checkBtn4_ido_int"), 
                uiOutput("checkBtn5_crit1"),
                uiOutput("checkBtn6_crit2"),
                uiOutput("checkBtn7_crit3"))),
              column(width = 10, 
                  title = "Summarized figures:", 
                  tags$head(tags$style(HTML(".small-box {height: 95px}"))),
                  valueBoxOutput("countries_affected", width = 2), 
                  valueBoxOutput("suicide_attack", width = 2), 
                  valueBoxOutput("attack_success", width = 2), 
                  valueBoxOutput("extended", width = 2), 
                  valueBoxOutput("attack_log_intl", width = 2), 
                  # valueBoxOutput("attack_log_domestic", width = 2), 
                  valueBoxOutput("attack_ideo_intl", width = 2)),
                  # valueBoxOutput("attack_ideo_domestic", width = 2)),
              column(width = 10, 
                withSpinner(
                  leafletOutput("top_10_dg_leaflet", height = 750)))
              )

        )),


    #-----------------------------------------------------
    # section 1.2: Identifying Patterns in Global attacks
    #-----------------------------------------------------
    tabItem(tabName = "eda_p1_2_heatmaps",
      fluidPage(title = "eda_p1_2_heatmaps",
        column(width = 12, 
          navbarPage("Global Attack Patterns", id = "global_patterns", 

              tabPanel("By Terrorist Groups", 
                fluidRow(
                  box(title = "By Terrorist Groups",status = "primary", width = 12, solidHeader = TRUE, background = "black",
                      fluidRow(
                          column(width = 4, uiOutput("slider_filter_year")),
                          column(width = 3, uiOutput("slider_filter_total_attacks")),
                          column(width = 3, uiOutput("radioBtn_filter_tgroup"))
                          ),
                      fluidRow(
                        column(width = 12, withSpinner(plotlyOutput("pattern_global_hmap4", width = "100%", height = 600))))
                      ))),

              tabPanel("By Target, Attack and Weapon Types", 
                fluidRow(

                  box(title = "By Attack and Weapon Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, background = "black",
                    column(width = 12,
                      withSpinner(plotlyOutput("pattern_global_hmap2", width = "100%", height = 250)),
                      withSpinner(plotlyOutput("pattern_global_hmap3", width = "100%", height = 250))
                      )),
                  box(title = "By Target Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, background = "black",
                    column(width = 12,
                      withSpinner(plotlyOutput("pattern_global_hmap1", width = "100%", height = 500))
                      ))
                  )),

              tabPanel("By Geographic Location", 
                fluidRow(
                  # tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),
                  box(title = "Geographic Patterns",status = "primary", width = 12, solidHeader = TRUE, background = "black",
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_global_hmap_countries", width = "100%", height = 4000))
                      ),
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_global_hmap_tnats", width = "100%", height = 4000))
                      )
                    )))
              ))
        )),

    #-------------------------------------------- 
    # section 2: Deadliest groups EDA part 1
    #-------------------------------------------- 
    tabItem(tabName = "eda_p2",
      fluidPage(title = "tdg_eda_p2",
            # tabBox(width = 12, title = "Characteristics of Top 10 Deadliest Groups", id = "t10_char", side = "left", 

        column(width = 12,  
          navbarPage("Most active groups", id = "t10_char", 
              tabPanel("Snapshot", 
                fluidRow(
                  column(width = 9, withSpinner(highchartOutput("top10_hc1",height = 350))),
                  column(width = 3, withSpinner(highchartOutput("top10_hc1_target_naltly", height = 350)))
                  ),
                fluidRow(            
                  column(width = 9, withSpinner(highchartOutput("top10_hc1_attack_type", height = 350))),
                  column(width = 3, withSpinner(highchartOutput("top10_hc1_year",height = 350)))
                  ) 
                ),

              tabPanel("By Fatalities (killed and/or wounded)", 
                fluidRow(
                    sidebarPanel(width = 2,
                      h4("Plot configs"),
                            # uiOutput("radioBtn_ldata2"),
                            # uiOutput("plotly_year"),
                            # uiOutput("plotly_country"),
                            uiOutput("select1_xvar"),
                            uiOutput("select2_yvar"),
                            uiOutput("select3_zvar"),
                            uiOutput("select4_colvar"),
                            uiOutput("radioBtn_log_tr"),
                            uiOutput("show_legend"),
                            actionButton("goButton", "Initialize Plot!", 
                              style = "color: white; background-color: #104E8B; width: 125px; height: 40px;")
                            ),
                    column(width = 10, 
                      withSpinner(plotlyOutput(outputId = "plotly_1", width = "100%", height = "750px"))
                          ))),

              tabPanel("Patterns by Number of Attcks Over Years", 
                fluidRow(
                  # tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),
                  box(title = "By Target, Attack and Weapon Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, background = "black",
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap1", width = "100%", height = 450))
                      ),
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap2", width = "100%", height = 225)),
                      withSpinner(plotlyOutput("pattern_hmap3", width = "100%", height = 225))
                      ),
                    column(width = 12,
                      withSpinner(plotlyOutput("pattern_hmap4", width = "100%", height = 250))
                      ))),
                fluidRow(
                  # tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),
                  box(title = "Geographic Patterns",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, background = "black",
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap_countries", width = "100%", height = 1400))
                      ),
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap_tnats", width = "100%", height = 1400))
                      )
                    ))
                )
              )
          )
        )),

    #-----------------------------------------------------
    # section 3: GUI data exploration
    #-----------------------------------------------------
    tabItem(tabName = "gui",
      fluidPage(
        sidebarPanel(width = 3,
          tabsetPanel(type = "tabs",

            tabPanel("Plot configs",
              uiOutput("radioBtn_data_gui"),
              uiOutput("slider_year_gui"),
              pickerInput(inputId = "Type", label = "Type of graph:", choices = c("Boxplot", "Density", "Histogram", "Scatter", "Violin"), selected = "Boxplot"),
              pickerInput("y_var", "Y-variable", choices = ""),
              conditionalPanel(
                condition = "input.Type!='Density' && input.Type!='Histogram'",
                pickerInput("x_var", "X-variable", choices = "")),
              pickerInput("group", "Group (or colour)", choices = ""),
              pickerInput("facet_row", "Facet Row", choices = ""),
              pickerInput("facet_col", "Facet Column", choices = ""),
              conditionalPanel(
                condition = "input.Type == 'Boxplot' || input.Type == 'Violin'",
                checkboxInput(inputId = "jitter", label = strong("Show data points"), value = FALSE)),
              conditionalPanel(
                condition = "input.Type == 'Boxplot'",
                checkboxInput(inputId = "notch", label = strong("Notched box plot"), value = FALSE)),
              conditionalPanel(
                condition = "input.Type == 'Density' || input.Type == 'Histogram'",
                sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)),
              conditionalPanel(
                condition = "input.Type == 'Histogram'",
                numericInput("binwidth", "Binwidth:", value = 1)),
              conditionalPanel(
                condition = "input.Type == 'Density' || input.Type == 'Violin'",
                sliderInput(inputId = "adj_bw", label = "Bandwidth adjustment:", min = 0.01, max = 2, value = 1, step = 0.1)),
              conditionalPanel(
                condition = "input.Type == 'Scatter'",
                checkboxInput(inputId = "line", label = strong("Show regression line"), value = FALSE),
                conditionalPanel(
                  condition = "input.line == true",
                  pickerInput("smooth", "Smoothening function", choices = c("lm", "loess", "gam"))),
                conditionalPanel(
                  condition = "input.line == true",
                  checkboxInput(inputId = "se", label = strong("Show confidence interval"), value = FALSE))
                  )
              ), # End of plot config tab panel

            tabPanel("Aesthetics",
              tabsetPanel(type = "tabs",

                tabPanel("Text",
                    checkboxInput(inputId = "label_axes", label = strong("Change labels axes"), value = FALSE),
                    conditionalPanel(
                      condition = "input.label_axes == true",
                      textInput("lab_x", "X-axis:", value = "label x-axis")),
                    conditionalPanel(
                      condition = "input.label_axes == true",
                      textInput("lab_y", "Y-axis:", value = "label y-axis")),
                    checkboxInput(inputId = "add_title", label = strong("Add title"), value = FALSE),
                    conditionalPanel(
                      condition = "input.add_title == true",
                      textInput("title", "Title:", value = "Title")),
                    checkboxInput(inputId = "adj_fnt_sz", label = strong("Change font size"), value = FALSE),
                    conditionalPanel(
                      condition = "input.adj_fnt_sz == true",
                      numericInput("fnt_sz_ttl", "Size axis titles:", value = 12),
                      numericInput("fnt_sz_ax", "Size axis labels:", value = 10)),
                    checkboxInput(inputId = "rot_txt", label = strong("Rotate text x-axis"), value = FALSE),
                    checkboxInput(inputId = "adj_fnt", label = strong("Change font"), value = FALSE),
                    conditionalPanel(
                      condition = "input.adj_fnt == true",
                      pickerInput("font", "Font", choices = c("Courier", "Helvetica", "Times"), selected = "Helvetica"))
                    ),

                  tabPanel("Theme",
                    # conditionalPanel(
                    #   condition = "input.group != '.'",
                    #   checkboxInput(inputId = "adj_col", label = strong("Change colours"), value = FALSE),
                    #   conditionalPanel(
                    #     condition = "input.adj_col",
                    #     pickerInput(inputId = "palet", label = strong("Select palette"),
                    #                 choices = list(
                    #                   "Qualitative" = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                    #                   "Diverging" = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                    #                   "Sequential" = c("Blues",  "BuGn",  "BuPu",  "GnBu",  "Greens",  "Greys",  "Oranges",  "OrRd",  "PuBu",  "PuBuGn",  "PuRd",  "Purples",  "RdPu",  "Reds",  "YlGn",  "YlGnBu",  "YlOrBr",  "YlOrRd")), selected = "Spectral"))),
                    conditionalPanel(
                      condition = "input.jitter",
                      checkboxInput("adj_jitter", strong("Change look jitter"), FALSE),
                      conditionalPanel(
                        condition = "input.adj_jitter",
                        textInput("col_jitter", "Colour (name or RGB):", value = "#cfd1cf"),
                        numericInput("size_jitter", "Size:", value = 1),
                        sliderInput("opac_jitter", "Opacity:", min = 0, max = 1, value = 0.5, step = 0.01),
                        sliderInput("width_jitter", "Width jitter:", min = 0, max = 0.5, value = 0.25, step = 0.01))),
                    checkboxInput("adj_grd", strong("Remove gridlines"), FALSE),
                    conditionalPanel(
                      condition = "input.adj_grd",
                      checkboxInput("grd_maj", strong("Remove major gridlines"), FALSE),
                      checkboxInput("grd_min", strong("Remove minor gridlines"), FALSE)),
                    pickerInput("theme", "Theme",
                                choices = c("grey" = "theme_grey()", "light" = "theme_light()", "minimal" = "theme_minimal()"), 
                                selected = "theme_grey()")
                    ),
                  
                  tabPanel("Legend",
                    conditionalPanel(
                      condition = "input.group != '.'",
                      radioButtons(inputId = "adj_leg", label = NULL,
                                   choices = c("Keep legend as it is", "Remove legend", "Change legend"),
                                   selected = "Keep legend as it is"),
                      conditionalPanel(
                        condition = "input.adj_leg=='Change legend'",
                        textInput("leg_ttl", "Title legend:", value = "title legend"),
                        pickerInput("pos_leg", "Position legend", choices = c("right", "left", "top", "bottom"))))
                    ),

                  tabPanel("Size",
                    checkboxInput("fig_size", strong("Adjust plot size on screen"), FALSE),
                    conditionalPanel(
                      condition = "input.fig_size", 
                      numericInput("fig_height", "Plot height (# pixels): ", value = 600),
                      numericInput("fig_width", "Plot width (# pixels):", value = 900)
                    ),
                    checkboxInput("fig_size_download", strong("Adjust plot size for download"), FALSE),
                    conditionalPanel(
                      condition = "input.fig_size_download",
                      numericInput("fig_height_download", "Plot height (in cm):", value = 14),
                      numericInput("fig_width_download", "Plot width (in cm):", value = 14)
                      )
                    )
                  )
              ) # End of Aesthetics tab panel

            ) # End main tabset panel
        ), # End sidebar panel


        column(width = 9, 
          navbarPage("Statistical Analysis", id = "stats_plots", 
              tabPanel("Interactive",
                withSpinner(plotlyOutput("out_plotly", width = "100%", height = 600))),
              tabPanel("Static",
                withSpinner(plotOutput("out_ggplot", width = "100%", height = 600)))
              ))

        ) # End fluid page
    ),


    #-------------------------------- 
    # Part 4: time-series analysis
    #------------------------------- 
    tabItem(tabName = "ts_analysis_01_season",
      fluidPage(title = "Time-series forecasting",
        fluidRow(
          # column(width = 2,
            sidebarPanel(width = 2,
              uiOutput("ts_filter_country"),
              uiOutput("ts_filter_year"),
              # p("Observe trend on the plots and select accordingly for forecasting!"),
              uiOutput("ts_fc_goal"), 
              uiOutput("ts_attack_freq"), 
              conditionalPanel(
                condition = " input.ts_tabs ==  'Forecasts (Predictions)' || input.ts_tabs ==  'Model Evaluation' ",
                uiOutput("ts_slider_horizon")),
              conditionalPanel(
                condition = " input.ts_tabs ==  'Model Evaluation' "),
              conditionalPanel(
                condition = " input.ts_tabs ==  'Forecasts (Predictions)' "),
              conditionalPanel(
                condition = " input.ts_mod_eval ==  'Neural Network' || input.ts_mod_preds ==  'Neural Network' ",
                h4("NeuralNet configs:"), 
                uiOutput("ts_slider_nn_repeats"),
                p("Number of networks to fit with different random starting weights"))
              # )
            ),

        column(width = 10,  
          navbarPage("Time-series Analysis", id = "ts_tabs", 

            tabPanel("Seasonality analysis", 
              tabsetPanel(type = "tabs", id = "ts_seas_analysis",
                tabPanel("Seasonal patterns", 
                  fluidRow(
                    box(title = "Seasonal pattern within the year",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      column(width = 12,
                        withSpinner(plotlyOutput("ts_line", width = "100%", height = 250)),
                        withSpinner(plotlyOutput("ts_cycle", width = "100%", height = 300)))
                      ),
                    box(title = "Seasonal pattern between cycles",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      column(width = 12, 
                        withSpinner(plotlyOutput("ts_box", width = "100%", height = 350)),
                        withSpinner(plotlyOutput("ts_normal", width = "100%", height = 400)))
                      ))),

                  tabPanel("3-Dimensional view", 
                    fluidRow(
                      box(title = "Polar, Surface plot and Heatmap",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 6,
                        withSpinner(plotlyOutput("ts_polar", width = 470, height = 500))),
                        column(width = 6, 
                          withSpinner(plotlyOutput("ts_surface", width = "100%", height = 500))),
                        column(width = 12,
                          withSpinner(plotlyOutput("ts_heatmap", width = "100%", height = 200)))
                        )
                      # box(title = "Surface plot",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      #   column(width = 12,
                      #     withSpinner(plotlyOutput("ts_heatmap", width = "100%", height = 300)))
                      #     )
                      )
                    )
                  )
              ),



            tabPanel("Statistical Analysis", 
                tabsetPanel(type = "tabs", id = "ts_stat_analysis",

                  tabPanel("Time-series Decomposition", 
                    fluidRow(
                      box(title = "Time-series Decomposition",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_decompose", width = "100%", height = 600)))))
                    ), 

                  tabPanel("Correlation Analysis I", 
                    fluidRow(
                      box(title = "ACF (Autocorrelation Function)",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_acf", width = "100%", height = 275)))),
                      box(title = "PACF (Partial Autocorrelation Function)",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_pacf", width = "100%", height = 275)))))
                    ), 

                  tabPanel("Correlation Analysis II", 
                    fluidRow(
                      box(title = "Lag plot",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_lag", width = "100%", height = 600)))))))
                ),


            tabPanel("Model Evaluation", 
              tabsetPanel(type = "tabs", id = "ts_mod_eval",

                  tabPanel("Auto-Arima", 
                    fluidRow(
                      box(title = "Evaluation: Forecast and Fitted vs Actual",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("ts_eval_arima", width = "100%", height = 500))),
                        column(width = 2, br(), br(),
                          withSpinner(tableOutput("acc_arima")))
                        ),
                      box(title = "Residuals analysis",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_res_arima", width = "100%", height = 500)))
                        )
                    )), 

                  tabPanel("Neural Network", 
                    fluidRow(
                      box(title = "Evaluation: Forecast and Fitted vs Actual",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("ts_eval_nn", width = "100%", height = 500))),
                        column(width = 2, br(), br(),
                          withSpinner(tableOutput("acc_nn")))
                        ),
                      box(title = "Residuals analysis",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_res_nn", width = "100%", height = 500)))
                        )
                    )),  

                  tabPanel("TBATS", 
                    fluidRow(
                      # box(title = "Residuals analysis",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      #   column(width = 12, 
                      #     withSpinner(plotlyOutput("ts_res_tbats", width = "100%", height = 500)))
                      #   ),
                      box(title = "Evaluation: Forecast and Fitted vs Actual",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("ts_eval_tbats", width = "100%", height = 500))),
                        column(width = 2, br(), br(),
                          withSpinner(tableOutput("acc_tbats")))
                        )
                    )), 

                  tabPanel("ETS", 
                    fluidRow(
                      box(title = "Evaluation: Forecast and Fitted vs Actual",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("ts_eval_ets", width = "100%", height = 500))),
                        column(width = 2, br(), br(),
                          withSpinner(tableOutput("acc_ets")))
                        ),
                      box(title = "Residuals analysis",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 12, 
                          withSpinner(plotlyOutput("ts_res_ets", width = "100%", height = 500)))
                        )
                    )),

                  tabPanel("Comparison", 
                    fluidRow(
                      box(title = "Finding best model based on evaluation metrics",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 7, withSpinner(tableOutput("tbl_eval_compare")), hr(),
                          p("Note: MAPE metric has been chosen to highlight the best model. Model with lowest MAPE score is on the top.")),
                        column(width = 5, withSpinner(tableOutput("tbl_eval_text")))
                        ),
                    
                      box(title = "Check if model is better than random guess or not",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 7, withSpinner(highchartOutput("eval_theilu", width = "100%", height = 350))),
                        column(width = 5, withSpinner(tableOutput("tbl_eval_text_theilu")))
                        )
                      )
                    )
                  )
                ),


            tabPanel("Forecasts (Predictions)", 
              tabsetPanel(type = "tabs", id = "ts_mod_preds",

                  tabPanel("Auto-Arima", 
                    fluidRow(
                      box(title = "Forecast from Auto Arima model",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("fc_prediction_arima", width = "100%", height = 550))),
                        column(width = 2, 
                          withSpinner(tableOutput("tbl_fc_prediction_arima")))
                        )
                    )), 

                  tabPanel("Neural Network", 
                    fluidRow(
                      box(title = "Forecast from Neural Network model",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("fc_prediction_nn", width = "100%", height = 550))),
                        column(width = 2, 
                          withSpinner(tableOutput("tbl_fc_prediction_nn")))
                        )
                    )),  

                  tabPanel("TBATS", 
                    fluidRow(
                      box(title = "Forecast from TBATS model",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("fc_prediction_tbats", width = "100%", height = 550))),
                        column(width = 2, 
                          withSpinner(tableOutput("tbl_fc_prediction_tbats")))
                        )
                    )), 

                  tabPanel("ETS", 
                    fluidRow(
                      box(title = "Forecast from ETS model",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        column(width = 10, 
                          withSpinner(plotlyOutput("fc_prediction_ets", width = "100%", height = 550))),
                        column(width = 2, 
                          withSpinner(tableOutput("tbl_fc_prediction_ets")))
                        )
                    )), 

                  tabPanel("Ensemble", 
                    fluidRow(
                      box(title = "Ensemble of all models",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        fluidRow(
                          column(width = 12, 
                            withSpinner(highchartOutput("plot_ensemble", width = "100%", height = 350)))),
                        fluidRow(
                          column(width = 6, 
                          withSpinner(tableOutput("tbl_fc_prediction_ensemble"))))
                        )
                    ))
                  )
                ) # end of tab panels
            ) # end of navbarpage
          ) 
        ))),



    #-------------------------------- 
    # Part 5: Classification Models
    #------------------------------- 
    tabItem(tabName = "classification",
      fluidPage(title = "classification",
        fluidRow(
            sidebarPanel(width = 2,
              uiOutput("lgb_filter_country"),
              uiOutput("lgb_target_var"),
              conditionalPanel(
                condition = " input.classification_lgb ==  'Data preparation' ", 
                hr(),
                h4("Help text:"),
                p("Goal of this part is to predict class probabilities (Yes/No) for attacks in selected region")
                ),
              conditionalPanel(
                condition = " input.classification_lgb ==  'Model Interpretation' ", 
                hr(),
                h4("Model Explainer:"),
                uiOutput("lgb_select_test_obs")
                )     
              ), #end of sidebar panel

        column(width = 10,  
          navbarPage("Classification", id = "classification_lgb", 

            tabPanel("Data preparation", 
                  fluidRow(
                    box(title = "Overview of target variable and ASIS data",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      column(width = 4,
                        withSpinner(highchartOutput("plot_target_var", width = "100%", height = 400))),
                      column(width = 8,
                        fluidRow(
                          tags$div(title= paste("A value to consider, in order to control the balance of positive and negative weights."),
                            withSpinner(valueBoxOutput("vbox_spw", width = 3))), 
                          valueBoxOutput("vbox_tot_obs", width = 4), 
                          valueBoxOutput("vbox_year_range", width = 5)), 
                        h4("Observations (last 20)"),
                        tags$head(tags$style("#dt_out_1  {white-space: nowrap;  }")),
                          withSpinner(dataTableOutput("dt_out_1", width = "100%", height = 275)))
                      ),

                    box(title = "Overview of data after feature engineering and transformations",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      fluidRow(
                        column(width = 4,
                          fluidRow(
                            box(width = 12, status = "primary", title = "Data transformation overview:", style = "font-size: 115%; ",
                              h4("Feature Engineering:"), 
                              tags$ol(
                                tags$li("log transformation"), 
                                tags$li("categorical encoding"), 
                                tags$li("added frequency count features")), 
                                # tags$li("dropped features with near zero variance")),
                              br(),
                              h4("Validation strategy:"), 
                              tags$ul(
                                tags$li("create time based split"),
                                tags$li("train model on training data"),
                                tags$li("evaluate model performance on validation data"),
                                tags$li("predict on test data")), br(),
                              h4("Split strategy:"), 
                              tags$ul(
                                tags$li("training data: upto year 2014 only"),
                                tags$li("validation data: year 2015"),
                                tags$li("test data: year 2016"))
                              )
                            )
                          ),

                        column(width = 8,
                          fluidRow(
                            box(width = 12, status = "warning",
                              title = "Time based split for modelling", 
                              withSpinner(valueBoxOutput("vbox_train", width = 6)), 
                              tags$head(tags$style(HTML("#vbox_valid .fa { font-size: 55px; }"))),
                              withSpinner(valueBoxOutput("vbox_valid", width = 3)), 
                              tags$head(tags$style(HTML("#vbox_test .fa { font-size: 55px; }"))),
                              withSpinner(valueBoxOutput("vbox_test", width = 3))
                            )),
                          h4("Glimpse of prepared data"), 
                          tabsetPanel(type = "tabs", id = "lgb_split_str",
                              tabPanel("All data (glimpse)", 
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(dataTableOutput("dt_out_all_split", width = "100%", height = 275))
                                  ))),
                              tabPanel("Training data", 
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(verbatimTextOutput("lgb_split_str_train"))
                                  ))),
                              tabPanel("Validation data", 
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(verbatimTextOutput("lgb_split_str_valid"))
                                  ))),
                              tabPanel("Test data", 
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(verbatimTextOutput("lgb_split_str_test"))
                                  )))
                              )
                            )
                        ))
                    )),

            tabPanel("Modeling", 
              fluidRow(
                box(title = "LightGBM Classifier", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                      fluidRow(
                        column(width = 4,
                          fluidRow(
                            box(width = 12, status = "primary", title = "Parameters tuning:",
                            column(width = 6, 
                              uiOutput("lgb_num_leaves"),
                              uiOutput("lgb_max_depth"),
                              uiOutput("lgb_bagging_fraction"), # subsample
                              uiOutput("lgb_bagging_freq"),     # subsample_freq
                              uiOutput("lgb_feature_fraction") # colsample_bytree
                              ),
                            column(width = 6, 
                              uiOutput("lgb_learning_rate"),
                              uiOutput("lgb_nrounds"),
                              uiOutput("lgb_early_stopping_rounds"),
                              uiOutput("lgb_eval_freq"), 
                              uiOutput("lgb_scale_pos_weight")),
                            column(width = 12, 
                              tags$head(tags$style(HTML('#btn_lgb_model{background-color:orange;}'))),
                              actionButton(inputId = "btn_lgb_model", label = "Initialize Model", icon = icon("cogs"), width = "95%")
                              )
                            ),
                            box(width = 12, status = "success", title = "Machine Capacity: ", 
                              # tags$head(tags$style(HTML("#vbox_nthread .fa { font-size: 60px; }"))),
                              # withSpinner(valueBoxOutput("vbox_nthread", width = 4)), 
                              tags$head(tags$style(HTML("#vbox_avil_mem .fa { font-size: 50px; }"))),
                              withSpinner(valueBoxOutput("vbox_avil_mem", width = 6)),
                              tags$head(tags$style(HTML("#vbox_used_mem .fa { font-size: 50px; }"))),
                              withSpinner(valueBoxOutput("vbox_used_mem", width = 6))
                            )
                            )
                          ),

                        column(width = 8,
                          box(width = 12, status = "warning", title = "Model output:",
                          # fluidRow(
                          #   box(width = 12, status = "success",
                          #     title = "Model output", 
                          #     tags$head(tags$style(HTML("#vbox_nthread .fa { font-size: 45px; }"))),
                          #     withSpinner(valueBoxOutput("vbox_nthread", width = 4)), 
                          #     tags$head(tags$style(HTML("#vbox_avil_mem .fa { font-size: 45px; }"))),
                          #     withSpinner(valueBoxOutput("vbox_avil_mem", width = 4)), 
                          #     tags$head(tags$style(HTML("#vbox_used_mem .fa { font-size: 45px; }"))),
                          #     withSpinner(valueBoxOutput("vbox_used_mem", width = 4))
                          #   )),
                          tabsetPanel(type = "tabs", id = "lgb_model_output",
                              tabPanel("log", value = "tab_log",
                                fluidRow(
                                  column(width = 12, 
                                    tags$style(type='text/css', '#lgb_console_out {background-color: black; color: rgb(113, 214, 55); font-size: 14px;}'), 
                                    withSpinner(verbatimTextOutput("lgb_console_out"))
                                  ))),
                              tabPanel("Feature importance (by gain)", value = "tab_fi",
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(highchartOutput("plot_lgb_fi", width = "100%", height = 650)))
                                    )
                                ),
                              tabPanel("Feature importance (detailed)", value = "tab_fid",
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(highchartOutput("plot_lgb_fi_all", width = "100%", height = 650)))
                                  )
                                ),
                              tabPanel("table", value = "tab_tbl",
                                fluidRow(
                                  column(width = 12,
                                    withSpinner(tableOutput("tbl_lgb_fi")))
                                  )
                                  )
                              ))
                            )
                        ))

                  )
              ), # end of tab panel Modeling

            tabPanel("Model Interpretation", 
              fluidRow(
                box(title = "Model Interpretation", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  fluidRow(
                    column(width = 8,
                      box(width = 12, status = "success", title = "Test data:",
                        tags$head(tags$style("#dt_out_lgb_test  {white-space: nowrap;  }")),
                          withSpinner(dataTableOutput("dt_out_lgb_test", width = "100%", height = 275))),
                      box(width = 12, status = "warning", title = "Model Explainer:",
                        wellPanel(
                          withSpinner(verbatimTextOutput("tbl_lgb_test_obs_predicted"))
                          ),
                        withSpinner(highchartOutput("plot_lgb_explainer", width = "100%", height = 550)))
                      ),
                    column(width = 4,
                      box(width = 12, status = "primary", title = "Selected observation from Test set",
                        withSpinner(tableOutput("tbl_lgb_test_obs")))
                      )
                    )
                  ))
              ) # end of tab panel Model interpretation

            ))))) # end of classification menu



    ) # End tab items
  ) # End dashboard body

#---------------------------------------------------------------------------------------------------------------------  
# Compile dashboard
#---------------------------------------------------------------------------------------------------------------------  
dashboardPage(header, sidebar, body, skin = "blue")