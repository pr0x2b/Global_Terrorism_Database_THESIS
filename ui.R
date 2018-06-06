

#---------------------------------------------------------------------------------------------------------------------  
# Dashboard header
#---------------------------------------------------------------------------------------------------------------------  
header <- dashboardHeader(
    title = "Global Terrorism Database",
    # links to my social and professional profiles
    tags$li(class = "dropdown", tags$a(href = "https://www.kaggle.com/pranav84", target = "_blank", tags$img(height = "25px", src = "kaggle.png"))),
    tags$li(class = "dropdown", tags$a(href = "https://twitter.com/pranavpandya84", target = "_blank", tags$img(height = "25px", src = "twitter.png"))),
    tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/pranav84/", target = "_blank", tags$img(height = "25px", src = "linkedin.png")))) 

#---------------------------------------------------------------------------------------------------------------------  
# Dashboard sidebar
#---------------------------------------------------------------------------------------------------------------------  
sidebar <- dashboardSidebar(
    sidebarMenu(id = "gtd_pranav",
        # tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})'))),
      menuItem("About GTD", tabName = "about_gtd", icon = icon("home")),
      menuItem("Part 1: Impact Analysis", icon = icon("globe"),tabName = "part_1",
        menuSubItem("GUI: Geographical", icon = icon("globe"),tabName = "eda_p1_geographic"),
        menuSubItem("Global Attack Patterns", icon = icon("globe"),tabName = "eda_p1_2_heatmaps")),
      menuItem("Part 2: Active Groups", tabName = "part_2", icon = icon("user"),
        menuSubItem("Characteristics", icon = icon("user"),tabName = "eda_p2"),
        menuSubItem("Animations", icon = icon("play"), tabName = "p1_animations")),
      menuItem("Part 3: Statistical Analysis", tabName = "part_3", icon = icon("gears"),
        menuSubItem("GUI", tabName = "gui", icon = icon("gears"))),
      menuItem("Part 4: Modeling", tabName = "modelling", icon = icon("gears"),
        menuSubItem("Modelling_1", tabName = "m1", icon = icon("gears")),
        menuSubItem("Modelling_2", tabName = "m2", icon = icon("gears")),
        menuSubItem("Modelling_3", tabName = "m3", icon = icon("gears"))),
      menuItem("Part 5: Insights", tabName = "predictions", icon = icon("globe")),
      menuItem("Next steps", tabName = "predictions6", icon = icon("globe")),
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
    tabItem(tabName = "about_gtd",
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

            ) # End of fluid page
        ),


    #-------------------------------------------- 
    # section 1.1: Geographic Visualization (Leaflet)
    #-------------------------------------------- 
    tabItem(tabName = "eda_p1_geographic",
      fluidPage(title = "tdg_eda_p1_geographic",
        fluidRow(h3("GUI: Exploration of Attack Characteristics"),
          tags$style(HTML(".tab-panel{ background-color: black; color: white}")),
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
            tabBox(width = 12, title = "Identifying Patterns in All Global Attacks", id = "t10_char", side = "left", 

              tabPanel("By Terrorist Groups", 
                fluidRow(
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),
                  box(title = "By Terrorist Groups",status = "primary", width = 12, solidHeader = TRUE, 
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
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),

                  box(title = "By Attack and Weapon Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    column(width = 12,
                      withSpinner(plotlyOutput("pattern_global_hmap2", width = "100%", height = 250)),
                      withSpinner(plotlyOutput("pattern_global_hmap3", width = "100%", height = 250))
                      )),
                  box(title = "By Target Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    column(width = 12,
                      withSpinner(plotlyOutput("pattern_global_hmap1", width = "100%", height = 500))
                      ))
                  )),

              tabPanel("By Geographic Location", 
                fluidRow(
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{ background:black }")),
                  box(title = "Geographic Patterns",status = "primary", width = 12, solidHeader = TRUE, 
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_global_hmap_countries", width = "100%", height = 4000))
                      ),
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_global_hmap_tnats", width = "100%", height = 4000))
                      )
                    )))
              )
        )),

    #-------------------------------------------- 
    # section 2: Deadliest groups EDA part 1
    #-------------------------------------------- 
    tabItem(tabName = "eda_p2",
      fluidPage(title = "tdg_eda_p2",
            tabBox(width = 12, title = "Characteristics of Top 10 Deadliest Groups", id = "t10_char", side = "left", 
              tabPanel("Snapshot", 
                fluidRow(
                  column(width = 9, withSpinner(highchartOutput("top10_hc1",height = 340))),
                  column(width = 3, withSpinner(highchartOutput("top10_hc1_target_naltly", height = 340)))
                  ),
                fluidRow(            
                  column(width = 9, withSpinner(highchartOutput("top10_hc1_attack_type", height = 250))),
                  column(width = 3, withSpinner(highchartOutput("top10_hc1_year",height = 250)))
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
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {}
                                   .box.box-solid.box-primary{ background:black }")),
                  box(title = "By Target, Attack and Weapon Types",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
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
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {}
                                   .box.box-solid.box-primary{ background:black }")),
                  box(title = "Geographic Patterns",status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap_countries", width = "100%", height = 1400))
                      ),
                    column(width = 6,
                      withSpinner(plotlyOutput("pattern_hmap_tnats", width = "100%", height = 1400))
                      )
                    ))
              )
        ))),


    tabItem(tabName = "p1_animations",
      fluidPage(title = "Pattern Visualization",

          tabBox(width = 9, title = "4 D animation", id = "animation_tab", side = "left", 
            tabPanel("animation_1"
              # highchartOutput("top10_hc1",height = 400)
              ),
            tabPanel("animation_1", "")
              #highchartOutput("top10_hc2",height = 600))
              ),

          box(width = 3, title = "Frequent Targets",status = "primary", solidHeader = TRUE
              # highchartOutput("top10_hc1_target_type", height = 300),
              # highchartOutput("top10_hc1_target_naltly", height = 300)
              )

        )
      ),

    #-----------------------------------------------------
    # section 3: GUI data exploration
    #-----------------------------------------------------
    tabItem(tabName = "gui",
      fluidPage(
        sidebarPanel(width = 3,
          tabsetPanel(

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
              tabsetPanel(

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
                      numericInput("fig_width", "Plot width (# pixels):", value = 600)
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



        mainPanel(width = 9, 
          h3("Statistical Analysis"),
          tabsetPanel(
            tabPanel("Interactive",
              withSpinner(plotlyOutput("out_plotly", width = "95%", height = 600))),
            tabPanel("static plots",
              withSpinner(plotOutput("out_ggplot", width = "95%", height = 600)))
            ))

        ) # End fluid page
    ),


    tabItem(tabName = "p1_3",
      fluidPage(title = "Market Explorer",
        fluidPage(
          column(width = 12,height = 300,
            box(title = "Top 10 Countries By",status = "primary",width = 12,solidHeader = TRUE,collapsible = TRUE,
              fluidRow(
                box(title = "Fatalities",status = "primary",width = 4,solidHeader = FALSE,collapsible = FALSE,plotlyOutput("plot3",height = 250)),
                box(title = "Damages",status = "primary",width = 4,solidHeader = FALSE,collapsible = FALSE,plotlyOutput("plot4",height = 250)),
                box(title = "Attacks",status = "primary",width = 4,solidHeader = FALSE,collapsible = FALSE,plotlyOutput("plot2",height = 250))))),
          column(width = 12,
            box(title = "Time-series of Attacks",status = "primary",width = 6,solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot5")),
            box(title = "Time-series of Fatalities",status = "primary",width = 6,solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot6")))))),

    #-------------------------------- 
    # section 3: Interactive plots
    #------------------------------- 
    tabItem(tabName = "modelling",

      fluidPage(title = "Terrorism by Country",
        column(width = 2,
          box(title = "Query Builder", status = "primary", width = 12, solidHeader = TRUE, background = "navy",
            box(width = 12, status = "primary", solidHeader = FALSE, background = "navy", uiOutput("levelQueryUi")),
            conditionalPanel(
              condition = "input.analysisLevel == 1",
              box(status = "primary", solidHeader = FALSE, width = 12, background = "navy", uiOutput("regionlist"))),
            conditionalPanel(condition = "input.analysisLevel == 2",
              box(status = "primary", solidHeader = FALSE, width = 12, background = "navy", uiOutput("countrylist"))), 
            box(status = "primary", solidHeader = FALSE, width = 12, background = "navy",
              sliderInput("hviQuery", label = "Year Range", min = 1970, max = 2016, value = c(1970,2016))),
            box(width = 12, status = "primary", solidHeader = FALSE, background = "navy", uiOutput("timeplot")),
            actionButton("query", label = "Go"))),

        conditionalPanel(
          condition = "input.query", 
          column(width = 10,
            box(title = textOutput("Design"), status = "primary", width = 12, height = 1500, solidHeader = TRUE,collapsible = TRUE,
              fluidRow(
                box(status = "primary", width = 12, solidHeader = FALSE, collapsible = TRUE, 
                  valueBoxOutput("totcity_country", width = 3),
                  valueBoxOutput("totAttacks_country", width = 3),
                  valueBoxOutput("totlife_country", width = 3),
                  valueBoxOutput("totloss_country", width = 3))), 
              fluidRow(
                column(width = 12,
                  box(title = "Attacks", status = "primary", width = 4, solidHeader = FALSE, collapsible = TRUE, plotlyOutput("plot7",height = 250)),
                  box(title = "Targets", status = "primary", width = 4, solidHeader = FALSE, collapsible = TRUE, plotlyOutput("plot8",height = 250)),
                  box(title = "Weapons", status = "primary", width = 4, solidHeader = FALSE, collapsible = TRUE, plotlyOutput("plot9",height = 250)))),
              fluidRow(
                column(width = 12,
                  box(title = "Major Attacks", status = "primary", width = 6, height = 475, solidHeader = FALSE, collapsible = TRUE, style = "color: #444",
                    leafletOutput("country_map")),
                  box(title = "Major Terrorist Groups", status = "primary", width = 6, solidHeader = FALSE, collapsible = TRUE, style = "color: #444", 
                    DT::dataTableOutput("groupnameTbl")))),
              fluidRow(
                column(width = 12,
                  box(title = textOutput("tseries"), status = "primary", width = 12, solidHeader = FALSE, collapsible = TRUE, 
                    plotlyOutput("plot10",height = 350)))))))
            ) # Closing fluidpage
        ), # Closing tabName = modelling


    #------------------------------- 
    # section 4: Predictions tab
    #------------------------------- 
    tabItem(tabName = "predictions",
      fluidPage(title = "To dos....",
        column(width = 12,
          box(title = "To dos...", status = "primary", width = 12, collapsible = TRUE, solidHeader = TRUE,
            box(width = 6, status = "warning", title = "To dos...")),
          box(width = 12, title = "To dos part 2...", status = "primary", solidHeader = TRUE, collapsible = TRUE,
            box(width = 6, style = "font-size: 120%;", style = "color: #444", status = "primary", withMathJax())),
          box(width = 6, status = "warning", solidHeader = F, p("")))))

    ) # End tab items
  ) # End dashboard body

#---------------------------------------------------------------------------------------------------------------------  
# Compile dashboard
#---------------------------------------------------------------------------------------------------------------------  
dashboardPage(header, sidebar, body, skin = "blue")