

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
      menuItem("About GTD", tabName = "about_gtd", icon = icon("home")),
      menuItem("The Deadliest Groups", tabName = "p1_1", icon = icon("globe"),
        menuSubItem("EDA Part 1", icon = icon("user"),tabName = "eda_p1"),
        menuSubItem("EDA Part 2", icon = icon("user"),tabName = "eda_p2"),
        menuSubItem("Animations", icon = icon("play"), tabName = "p1_animations"),
        menuSubItem("Tab 3", icon = icon("check-circle"), tabName = "p1_3")),
      menuItem("Terrorism by Country", tabName = "bycountry", icon = icon("gears")),
      menuItem("Tab 3", tabName = "predictions", icon = icon("globe")),
      menuItem("Tab 4", tabName = "predictions1", icon = icon("globe")),
      menuItem("Tab 5", tabName = "predictions2", icon = icon("globe")),
      menuItem("Tab 6", tabName = "predictions3", icon = icon("globe")),
      menuItem("Tab 7", tabName = "predictions4", icon = icon("globe")),
      menuItem("Tab 8", tabName = "predictions5", icon = icon("globe")),
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
              style = "font-size: 110%; ", background = "blue", width = 15, solidHeader = TRUE,
              tags$ul(
                tags$li("Part 1: Global overview"), 
                tags$li("Part 2: EDA on conflicted regions"), 
                tags$li("Part 3: Algorithmic decision support")), 
              p("Note: Some of the variable names have been renamed to keep the analysis informative for target audience.")),

            box(
              style = "font-size: 110%; ", background = "blue", width = 15, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
              h3("About data source: "),
              p("The Global Terrorism Database (GTD) is an open-source database including information on over 170,000 terrorist events around the world 
                 from 1970 through 2016. It is the most comprehensive unclassified database on terrorist events in the world."), 
              p("GTD is maintained by researchers at the National Consortium for the Study of Terrorism and Responses to Terrorism (START), 
                  headquartered at the University of Maryland."), p(""), 
              p("Data description: "), 
              tags$ul(
                tags$li("Time period: 1970-2016, except 1993"), 
                tags$li("Variables: based on location, tactics, perpetrators, targets and outcomes"), 
                tags$li("Sources: Unclassified media articles")))

            ),

          column(width = 8, 
            fluidRow( 
              # box(status = "primary", width = 12, solidHeader=T, 
                img(src = "munich_image.jpg",height = 130, width = 900, style="display: block; margin-left: auto; margin-right: auto;"), 
                highchartOutput("world_hchart",height = 500)
                # )
              ))
            ), # End of fluid row
        hr(),

        fluidRow(

          column(width = 6,
            # actionButton("info_box", " Detailed information", icon = icon("info-circle")),
            # bsModal("modalExample", "info_box", size = "large", 
              box(
                h3("Definition of Terrorism: "),
                style = "font-size: 109%; ", background = "olive", width = 15, solidHeader = TRUE,
                p("The GTD defines a terrorist attack as the threatened or actual use of illegal force and violence by a non-state actor to attain a 
                    political, economic, religious, or social goal through fear, coercion, or intimidation."),
                p("In practice this means in order to consider an incident for inclusion in the GTD, all three of the following attributes must be present:"),
                tags$ul(
                  tags$li("The incident must be intentional – the result of a conscious calculation on the part of a perpetrator."), p(""),
                  tags$li("The incident must entail some level of violence or immediate threat of violence including property violence, as well as violence against people."),  p(""),
                  tags$li("The perpetrators of the incidents must be sub-national actors. GTD does not include acts of state terrorism.")
                      # p("Additionally, at least two of the following three criteria must be present for an incident to be included in the GTD:"),
                      # tags$ul(
                      #   tags$li("Criterion 1: The act must be aimed at attaining a political, economic, religious, or 
                      #        social goal. In terms of economic goals, the exclusive pursuit of profit does not satisfy this 
                      #        criterion. It must involve the pursuit of more profound, systemic economic change."), p(""),
                      #   tags$li("Criterion 2: There must be evidence of an intention to coerce, intimidate, or convey some other message 
                      #        to a larger audience (or audiences) than the immediate victims. It is the act taken as a totality that is 
                      #        considered, irrespective if every individual involved in carrying out the act was aware of this intention. 
                      #        As long as any of the planners or decision-makers behind the attack intended to coerce, intimidate or publicize, 
                      #        the intentionality criterion is met."),  p(""),
                      #   tags$li("Criterion 3: The action must be outside the context of legitimate warfare activities. That is, the act must be 
                      #          outside the parameters permitted by international humanitarian law (particularly the prohibition against deliberately 
                      #          targeting civilians or non-combatants)."))
                  ))
              ), 

          column(width = 6,
            fluidRow(valueBoxOutput("countries_affected"), valueBoxOutput("mult_attacks")),
            fluidRow(valueBoxOutput("attack_log_intl"), valueBoxOutput("attack_log_domestic")), 
            fluidRow(valueBoxOutput("attack_ideo_intl"), valueBoxOutput("attack_ideo_domestic"))
            )

          ) # End of fluid row

        )),

    #-------------------------------------------- 
    # section 1.1: Deadliest groups EDA part 1
    #-------------------------------------------- 
    tabItem(tabName = "eda_p1",
      fluidPage(title = "tdg_eda_p1",

        fluidRow(            
          column(width = 9, highchartOutput("top10_hc1_attack_type", height = 250)),
          column(width = 3, highchartOutput("top10_hc1_year",height = 250))
          ),


        fluidRow(
          column(width = 9, highchartOutput("top10_hc1",height = 400)),
          column(width = 3, highchartOutput("top10_hc1_target_naltly", height = 410))


            )

          # tabBox(width = 9,
          #   title = "Characteristics Deadliest Terrorist Groups", id = "tabset2", side = "left", 
          #   tabPanel("Plot_bar", "", 
          #     highchartOutput("top10_hc1",height = 300)),
          #   tabPanel("Plot_2", "")
          #     #highchartOutput("top10_hc2",height = 600))
          # ),

          # box(width = 3, title = "Frequent Targets",status = "primary", solidHeader = TRUE,
          #     highchartOutput("top10_hc1_target_type", height = 300),
          #     highchartOutput("top10_hc1_target_naltly", height = 300)          
          # ),


          # plotlyOutput("plot1",height='auto', width = 'auto')
          
          # )
        )),

    #-------------------------------------------- 
    # section 1.2: Deadliest groups EDA part 2
    #-------------------------------------------- 
    tabItem(tabName = "eda_p2",
      fluidPage(title = "tdg_eda_p2",
                          
          tabBox(width = 12, title = "Characteristics of the Top 10 Deadliest Groups: Part 2", id = "eda_p2_leaflet", side = "left", 

            tabPanel("Target locations", "", 
                      fluidRow(
                          sidebarPanel(width = 2, 
                            uiOutput("leaflet_year"),
                            uiOutput("leaflet_country"),
                            uiOutput("leaflet_group"),
                            uiOutput("leaflet_attack"),
                            uiOutput("leaflet_weapon"),
                            uiOutput("leaflet_target"),
                            uiOutput("checkBtn1_suicide"),
                            uiOutput("checkBtn2_multiple")),
                          box(width = 10,
                            column(width = 2, uiOutput("checkBtn3_log_int")),  
                            column(width = 3, uiOutput("checkBtn4_ido_int")),  
                            column(width = 2, uiOutput("checkBtn5_crit1")),  
                            column(width = 2, uiOutput("checkBtn6_crit2")),  
                            column(width = 3, uiOutput("checkBtn7_crit3"))
                            ),
                          column(width = 10, leafletOutput("top_10_dg_leaflet", height = 510))
                          )
                      ),


            tabPanel("Intentions", "")
              #highchartOutput("top10_hc2",height = 600))
              )

        )),

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
    tabItem(tabName = "bycountry",

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
        ), # Closing tabName = bycountry


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