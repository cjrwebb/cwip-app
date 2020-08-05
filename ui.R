# UI - Mapping Overlaps Gadget Education


ui <- dashboardPage(title = paste("CWIP App", sep = ""), 
                    skin = "blue", 
                    
                    

# Header ------------------------------------------------------------------

  
  dashboardHeader(
    title = span("CWIP APP", style = "color: #ffffff", titleWidth = 400)
  ),
  

# Sidebar -----------------------------------------------------------------

  dashboardSidebar(#width = 300,
    sidebarMenu(
      menuItem("Welcome to the CWIP App", tabName = "welc", icon = icon('info'), selected = TRUE),
      menuItem("Data Dashboard", tabName = "data_dash", icon = icon('digital-tachograph')),
      menuItem("Spending Waffle Plots", tabName = "spending", icon = icon('money-bill-wave')),
      menuItem("Side-by-side Trends", tabName = "sbs_trends", icon = icon('chart-line')),
      menuItem("Side-by-side Maps", tabName = "sbs_maps", icon = icon('map-signs')),
      menuItem("CLA Rate by Ethnic Group", tabName = "cla_ethnicity", icon = icon('users')),
      menuItem(HTML("&nbsp;&nbsp;&nbsp;LA Similarity Clusters"), tabName = "hca", icon = icon("pagelines")),
      menuItem("Neighbourhood Maps", tabName = "uni_maps", icon = icon('map-marked-alt')),
      menuItem("Neighbourhood Overlap Maps", tabName = "biv_maps", icon = icon('layer-group')),
      menuItem("Neighbourhood Correlations", tabName = "corrs", icon = icon('chart-line')),
      menuItem("LA ANOVA", tabName = "anova", icon = icon('equals')),
      menuItem("Neighbourhood Regression", tabName = "regress", icon = icon('project-diagram')),
      menuItem("Discussion Board", tabName = "disc", icon = icon("comments"))
    )
  ),
  

# Body --------------------------------------------------------------------


  dashboardBody(
    use_waiter(),
    waiter_show_on_load(html = "", color = "#00AD92"),

    
    tabItems(
      

# Welcome/info tab --------------------------------------------------------
      
      tabItem(tabName = "welc", # Welcome/Info tab 
              
              fluidRow(style = "padding: 0px 0px; margin-top:-2em", 
                column(
                  12,
                  includeHTML("intro.html"), offset = 0, style='padding:0px;'
                )
              )
              
      ), # Close tab



# Data Dashboard tab ------------------------------------------------------

tabItem(tabName = "data_dash",
        setSliderColor(color = rep("#00947c", 7), sliderId = 1:7),
        chooseSliderSkin("Flat"),
        br(), br(),
        
        fluidRow(
          column(
            12,
            selectizeInput("la_names_data_dash", 
                           "Select one or more local authorities:",
                           choices = NULL,
                           options = list(multiple = TRUE, maxOptions = 1000),
                           multiple = TRUE, 
                           width = "95%"
            )
          )
          
        ),
        
        fluidRow(
          column(12, selectizeInput("var_data_dash", "Select something to visualise:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "95%"))
        ), 
        
        fluidRow(
          column(12, sliderInput("year_range_data_dash", "Select a year range:", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "95%")),
        ), 
        
        br(),
        
        fluidRow(
          column(3, align = "center", radioGroupButtons("trend_switch", label = "Make trends relative to English LA Average?", choices = c("Yes", "No"), selected = "No")),
          column(2, align = "center", radioGroupButtons("inc_avg_data_dash", label = "Show English LA Average?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = TRUE)),
          column(2, align = "center", radioGroupButtons("adj_dep_data_dash", "Adjust for deprivation?", choices = c("Yes", "No"), selected = "No")),
          column(5, align = "center", radioGroupButtons("data_dash_plot_switch", label = "Change accompanying plot:", choices = c("Map", "Correlation with deprivation", "Cluster", "Trend Cluster"), selected = "Map"))
        ),
        
        br(), br(), # spacer
        
        fluidRow(
          column(7, plotOutput("data_dash_trend", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8)),
          column(5, plotOutput("data_dash_map", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8))
        ),
        
        br(), br(), br(), br(), br(), br() # Spacer
        
        ),


# Spending Waffles Tab ----------------------------------------------------

  tabItem(tabName = "spending",
          br(), br(),
          
          fluidRow(column(12, selectizeInput("la_spending", "Select a local authority:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = FALSE,  width = "95%"))),
          fluidRow(column(12, plotOutput("waffle_plot", width = "100%", height = "800px") %>% withSpinner(color="#0dc5c1", type = 8)))
          
          ), 



# Side by side trends tab -------------------------------------------------

tabItem(tabName = "sbs_trends",
        br(), br(),
        
        fluidRow(column(6, selectizeInput("var_sbs_trend_left", "Select something to visualise:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "100%")),
                 column(6, selectizeInput("var_sbs_trend_right", "Select something to visualise:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "100%"))
                 ),
        fluidRow(column(6, selectizeInput("la_select_sbs_trend_left", "Select one or more local authorities:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%")),
                 column(6, selectizeInput("la_select_sbs_trend_right", "Select one or more local authorities:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%"))
                 ),
        fluidRow(
          column(4, sliderInput("year_range_sbs_trend_left", "Select a year range:", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "100%")),
          column(1, align = "center", radioGroupButtons("adj_dep_sbs_trend_left", "Adjust for deprivation?", choices = c("Yes", "No"), selected = "No")),
          column(1, align = "center", radioGroupButtons("inc_avg_sbs_trend_left", label = "Show Average?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = TRUE)),
          column(4, sliderInput("year_range_sbs_trend_right", "Select a year range:", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "100%")),
          column(1, align = "center", radioGroupButtons("adj_dep_sbs_trend_right", "Adjust for deprivation?", choices = c("Yes", "No"), selected = "No")),
          column(1, align = "center", radioGroupButtons("inc_avg_sbs_trend_right", label = "Show Average?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = TRUE))
        ),
        fluidRow(
          column(6, plotOutput("sbs_trend_plot_left", width = "100%", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8)),
          column(6, plotOutput("sbs_trend_plot_right", width = "100%", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8))
        )
        
        ),



# Side-by-side Maps -------------------------------------------------------

tabItem(tabName = "sbs_maps",
        br(), br(),
        
        fluidRow(column(6, selectizeInput("var_sbs_map_left", "Select something to visualise:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "100%")),
                 column(6, selectizeInput("var_sbs_map_right", "Select something to visualise:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "100%"))
        ),
        fluidRow(column(6, selectizeInput("la_select_sbs_map_left", "Select one or more local authorities:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%")),
                 column(6, selectizeInput("la_select_sbs_map_right", "Select one or more local authorities:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%"))
        ),
        fluidRow(
          column(4, sliderInput("year_range_sbs_map_left", "Select a year range:", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "100%")),
          column(1, align = "center", radioGroupButtons("adj_dep_sbs_map_left", "Adjust for deprivation?", choices = c("Yes", "No"), selected = "No")),
          column(1, align = "center", radioGroupButtons("cart_sbs_map_left", label = "Make Cartogram?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = FALSE)),
          column(4, sliderInput("year_range_sbs_map_right", "Select a year range:", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "100%")),
          column(1, align = "center", radioGroupButtons("adj_dep_sbs_map_right", "Adjust for deprivation?", choices = c("Yes", "No"), selected = "No")),
          column(1, align = "center", radioGroupButtons("cart_sbs_map_right", label = "Make Cartogram?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = FALSE))
        ),
        fluidRow(
          column(6, plotOutput("sbs_map_plot_left", width = "100%", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8)),
          column(6, plotOutput("sbs_map_plot_right", width = "100%", height = "600px") %>% withSpinner(color="#0dc5c1", type = 8))
        )
        
        
        
        ),



# CLA by Ethnic Group Tab -------------------------------------------------

tabItem(tabName = "cla_ethnicity",
        br(), br(),
        fluidRow(
        column(3, 
               selectizeInput("la_select_cla_ethnicity", "Select one or more local authorities to highlight:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%"),
               selectizeInput("var_cla_ethnicity", "Select a variable:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = FALSE, width = "100%")),
        column(9, plotOutput("plot_cla_ethnicity", width = "100%", height = "1700px") %>% withSpinner(color="#0dc5c1", type = 8))
        )
        ),



# HCA Clustering tab ------------------------------------------------------

tabItem(tabName = "hca",
        br(), br(),
        
        fluidRow(
        column(3, 
               selectizeInput("la_select_hca", "Select one or more local authorities to highlight:", selected = NULL, choices = NULL, options = list(multiple = TRUE, maxOptions = 1000), multiple = TRUE,  width = "100%"),
               selectizeInput("var_hca", "Select variables to cluster on:", choices = NULL, selected = NULL, options = list(multiple = FALSE, maxOptions = 1000), multiple = TRUE, width = "100%"),
               sliderInput("year_range_hca", "Select a year range to cluster within (if not a trend-HCA, the variable will be averaged across these years):", value = c(2011, 2019), min = 2011, max = 2019, sep = "", width = "100%"),
               radioGroupButtons("trend_hca", label = "Cluster on trends? (If yes, clusters will be based on the average change per year in each variable):", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = FALSE)
               ),
        column(9, plotOutput("plot_hca", width = "100%", height = "900px") %>% withSpinner(color="#0dc5c1", type = 8))
        )
        
        
        ),

      

# Univariate Maps tab -----------------------------------------------------


      tabItem(tabName = "uni_maps",
              br(), br(),
               fluidRow(
                 column(
                   12,
                   titlePanel("Univariate Maps")
                 )
               ),
              
              fluidRow( # Intro text
                column(
                  12,
                  helpText(div(HTML("This page allows you to look at the levels of certain variables across neighbourhoods in a local authority, or multiple local authorities. <br>
                           Choose your local authority, or list of local authorities; a variable; and the number of quantiles you would like to split the variable into below (quantiles are ranges on the variable's score with equal numbers of neighbourhoods in). 
                           For example, if I selected Index of Multiple Deprivation Score split into 10 quantiles I would get a plot back where there are an equal number of neighbourhoods in each colour break in the legend (for example, if there were 100 neighbourhoods in total there would be 10 neighbourhoods in each quantile). The quantiles are ordered from low scores to high scores. Higher scores, which ususally mean worse social, economic, or health outcomes (but not always, depending on the variable) are closer to dark green, lower scores are closer to light green.<br>
                           If the plot returns fewer quantiles than you requested this is because there are not enough neighbourhoods, or not enough variation in the scores, to create that number of quantiles. In this case it will return the highest number of quantiles possible. You can try adding additional surrounding local authorities to increase the number of quantiles it is possible to create.")))
                )
              ),
              
              fluidRow(
                column(
                  12,
                  selectizeInput("la_names_univ", 
                              "Select one or more local authorities:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              multiple = TRUE, 
                              width = "95%"
                  )
                )
              ),
              
              fluidRow( # New row for variable selection and bins slider
                # Select a variable for univariate map
                column(
                  8,
                  selectizeInput("x_univ", 
                              "Select neighbourhood variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                ),
                column(
                  4,
                  sliderInput("bins_slider",
                              "Select the number of quantiles (default = 5)",
                              min = 3, 
                              max = 10,
                              value = 5,
                              round = TRUE,
                              step = 1,
                              animate = FALSE,
                              width = "80%")
                )
              ), # End of Row
              
              fluidRow( # Legend row
                column(11,
                       plotOutput("univ_legend",
                                  height = "60px"),
                       align = "center"
                       ) 
              ),
              
              fluidRow( # Univariate map render
                column(
                  11, align = "center",
                  plotlyOutput("univ_map", height = "700px") %>% withSpinner(color="#0dc5c1")
                )
                
              )
              
              
              ), # Close tab


# Bivariate maps tab ------------------------------------------------------


      
      tabItem(tabName = "biv_maps", # Open tab - bivariate maps
              br(), br(),
              fluidRow(
                # Title
                column(
                  12,
                  titlePanel("Overlap (Bivariate) Maps")
                ),
                # Description Placeholder
                column(
                  12,
                  helpText(div(HTML("Pick any combination of two variables to break them into tertiles and look at their relationship on the map when their colour scales are overlapping. 
                            You can pick as many or as few local authorities as you like, but do keep in mind that the more local authorities you select the slower the map will load. 
                            The left hand side variable shows up in shades of red, and the right hand side shows up in variations of blue. 
                            The key for difference combinations is shown in the bottom-right corner of the page.<br>
                           If a map doesn't appear, that usually means that the data for this variable is not available for that specific local authority. This is the case for many of the Welsh authorities.")))
                ),
                column(
                  12, 
                  verbatimTextOutput("click")
                ),
                # Select LAs: Users can enter one or more LAs to map
                # their administrative data
                column(
                  12,
                  selectizeInput("la_names", 
                              "Select one or more local authorities:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              multiple = TRUE, 
                              width = "100%"
                              # selected = sample(lsoa_data_spatial$LA_name, 1))
                  )
                )
              ), 
            # End of row
              fluidRow( # New row for variable selection
                # Selectize two variables to use to plot onto the map
                column(
                  6,
                  selectizeInput("x", 
                              "Select Red Variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                ),
                column(
                  6,
                  selectizeInput("y", 
                              "Select Blue Variable:",
                              choices = NULL,
                              options = list(multiple = TRUE, maxOptions = 1000),
                              width = "100%")
                )
              ), # End of Row
            fluidRow( # New row for tertile legends
              column(
                6,
                plotOutput("red_scale", height = "60px")
              ),
              column(
                6,
                plotOutput("blue_scale", height = "60px")
              )
            ), 
            fluidRow( # New row
              column(9,
                     plotlyOutput("bivar_map", height = "700px") %>% withSpinner(color="#0dc5c1")
                     ),
              column(3,
                     helpText(HTML("<br><br>Use the key below to show only areas with that combination of scores. Click once on the legend to activate it, then left click on one of the circles inside the key to redraw the map for just the combination, click on a different circle to change the selected colour. You can add more than one combination by holding down the <em>Shift</em> key and left clicking on additional circles. To reset the map, double-click either on the map itself or anywhere outside of the circles on the key.")),
                     plotlyOutput("legend")
                     )
              
            )
          ),
            

# Correlations tab --------------------------------------------------------


            
        tabItem(tabName = "corrs", # Open tab - correlations
                br(), br(),
                fluidRow(
                  # Title
                  column(
                    12,
                    titlePanel("Correlations")
                  ),
                  # Description Placeholder
                  column(
                    12,
                    helpText(div(HTML("This page allows you to correlate two variables and shows their relationship on a scatterplot.")))
                  ),
                  # Select LAs: Users can enter one or more LAs to map
                  # their administrative data
                  column(
                    12,
                    selectizeInput("la_names_corrs", 
                                   "Select one or more local authorities:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   multiple = TRUE, 
                                   width = "100%"
                                   # selected = sample(lsoa_data_spatial$LA_name, 1))
                    )
                  )
                ), 
                # End of row
                fluidRow( # New row for variable selection
                  # Selectize two variables to use to plot onto the map
                  column(
                    6,
                    selectizeInput("x_corrs", 
                                   "Select X variable:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   width = "100%")
                  ),
                  column(
                    6,
                    selectizeInput("y_corrs", 
                                   "Select Y Variable:",
                                   choices = NULL,
                                   options = list(multiple = TRUE, maxOptions = 1000),
                                   width = "100%")
                  )
                ), # End of Row
                fluidRow( # New row
                  column(8,
                         plotOutput("corrplot", height = "700px", width = "100%") 
                         %>% withSpinner(color="#0dc5c1")
                  ),
                  column(4,
                         h4("Pearson's R Correlation Results"),
                         htmlOutput(HTML("corr_result"))
                         )
                  
                )
                ),
      

# ANOVA tab ---------------------------------------------------------------


      tabItem(tabName = "anova", # Open tab - correlations
              br(), br(),
              fluidRow(
                # Title
                column(
                  12,
                  titlePanel("ANOVA")
                ),
                # Description Placeholder
                column(
                  12,
                  helpText(div(HTML("To compare whether two local authorities have significantly different mean values of a chosen variable add all local authorities you wish to compare to the box below.")))
                ),
                # Select LAs: Users can enter one or more LAs to map
                # their administrative data
                column(
                  12,
                  selectizeInput("la_names_anova", 
                                 "Select one or more local authorities:",
                                 choices = NULL,
                                 options = list(multiple = TRUE, maxOptions = 1000),
                                 multiple = TRUE, 
                                 width = "100%"
                  )
                )
              ), 
              # End of row
              fluidRow( # New row for variable selection
                # Selectize two variables to use to plot onto the map
                column(
                  6,
                  selectizeInput("x_anova", 
                                 "Select variable:",
                                 choices = NULL,
                                 options = list(multiple = TRUE, maxOptions = 1000),
                                 width = "100%")
                )
              ), # End of Row
              fluidRow( # New row
                column(6,
                       plotOutput("anovaplot", height = "700px", width = "100%") 
                       %>% withSpinner(color="#0dc5c1")
                ),
                column(6,
                       h4("Means for each Local Authority"),
                       dataTableOutput("meantable"),
                       h4("Comparison of means and Tukey HSD Significance"),
                       dataTableOutput("tukeytable")
                       )
                
              )
      ),


# Regression tab ----------------------------------------------------------

                
                tabItem(tabName = "regress", # Open tab - regress
                        br(), br(),
                        fluidRow(
                          # Title
                          column(
                            12,
                            titlePanel("Regression & Multiple Regression")
                          ),
                          # Description Placeholder
                          column(
                            12,
                            helpText(div(HTML("This page allows you to build multiple linear regression models using LSOA-level data. You can add multiple independent variables using the left drop down box, and a dependent variable on the right hand drop down box.<br>
                                              ")))
                          ),
                          # Select LAs: Users can enter one or more LAs to map
                          # their administrative data
                          column(
                            12,
                            selectizeInput("la_names_regress", 
                                           "Select one or more local authorities:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           multiple = TRUE, 
                                           width = "100%"
                            )
                          )
                        ), 
                        # End of row
                        fluidRow( # New row for variable selection
                          # Selectize two variables to use to plot onto the map
                          column(
                            6,
                            selectizeInput("x_regress", 
                                           "Select X variables:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           multiple = TRUE,
                                           width = "100%")
                          ),
                          column(
                            6,
                            selectizeInput("y_regress", 
                                           "Select Y Variable:",
                                           choices = NULL,
                                           options = list(multiple = TRUE, maxOptions = 1000),
                                           width = "100%")
                          )
                        ), # End of Row
                        fluidRow( # New row
                          column(12,
                                 h4("Regression Results"),
                                 dataTableOutput("regress_table")
                                 %>% withSpinner(color="#0dc5c1")
                          )

                        )
                        , # End of Row
                        fluidRow( # New row
                          column(12,
                                 h4("Predictions"),
                                 plotOutput("partial_plots", height = "800px")
                                 %>% withSpinner(color="#0dc5c1")
                          )
                          
                        )
                ),


# Discussion board tab ----------------------------------------------------


                tabItem(tabName = "disc",
                        fluidRow(
                          HTML("<div style = 'padding: 0px 20%; font-size: 16px'>
                               <h1>Discussion Board</h1>
                               This discussion board is <b>moderated</b>. Your post will not appear until it has been checked. If abused, this feature will be removed. It can be used to discuss how you have used the CWIP App, to share examples, ask questions, or to speak to other people interested in children's social services more generally.<br><br>"),
                          tags$iframe(seamless = "seamless", src = "https://padlet.com/9e4keezwnh/aypa6rqzy8kbrxpr", height = 800, width = "100%"),
                          HTML("</div>")
                         )
                        )
        
              
      ), # Close tabs
 # Close tab items
    

# Style tags --------------------------------------------------------------


    # Style
    tags$head(tags$style(HTML('
                          
                          /* body */
                          .content-wrapper, .right-side {
                          background-color: #ffffff;
                          }
                          
                          '))),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),


tags$head(tags$style(HTML('

        .content {
          margin-top: 50px;
        }
        
        .sidebar {
          position: fixed;
          width: 220px;
          white-space: nowrap;
          overflow: visible;
        }
        
        .main-header {
          position: fixed;
          width:100%;
        }


        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #00AD92;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #00947c;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #00AD92;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #00AD92;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #00AD92;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00AD92;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #00947c;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #00947c;
                              }
                              ')))


    
    

  ) # Close dash body
  
  
)