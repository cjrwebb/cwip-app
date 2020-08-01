

server <- shinyServer(function(input, output, session){


# Waiter ------------------------------------------------------------------



  # give time for wait screen to show
  Sys.sleep(1)
  waiter_hide()
  waiter_show(html = spin_rotate(), color = "#00AD92")
  Sys.sleep(2) # give time for wait screen to show
  waiter_hide()


# Quick loading reactive select inputs ------------------------------------
  
  
  # Inputs for Data Dashboard
  updateSelectizeInput(session, 
                       "la_names_data_dash",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  updateSliderInput(session,
                    "year_range_data_dash",
                    value = c(2011, 2019),
                    min = 2011,
                    max = 2019,
                    step = 1)
  
  # updateRadioGroupButtons(session,
  #                    "adj_dep_data_dash",
  #                    choices = c("Yes", "No"),
  #                    selected = "No", inline = TRUE)
  
  updateSelectizeInput(session, 
                       "var_data_dash",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  # updateRadioButtons(session,
  #                    "inc_avg_data_dash",
  #                    choiceNames = c("Yes", "No"),
  #                    choiceValues = c(TRUE, FALSE),
  #                    selected = TRUE, inline = TRUE)
  
  
  # Outputs for Data Dashboard
  
  output$data_dash_trend <- renderPlot({
    
    if (is.null(input$var_data_dash)) {
      
      
    } else {
    
    if (input$trend_switch == "No") {
    
    plot_csctrend(csc_data, var = input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1), 
                  adj_dep = input$adj_dep_data_dash, la_select = input$la_names_data_dash, 
                  include_avg = input$inc_avg_data_dash) } 
    
    else {

    plot_csctrend_diff(csc_data, var = input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1),
                      adj_dep = input$adj_dep_data_dash, la_select = input$la_names_data_dash,
                      include_avg = input$inc_avg_data_dash)
                    
    }
      
    }
    
  })
  
  output$data_dash_map <- renderPlot({
    
    if (is.null(input$var_data_dash)) { 
      
      } else {
    
    if (input$data_dash_plot_switch == "Map") {
    
    plot_csc_map(geo_data = la_sf, centroid_data = la_sf_centroids, data = csc_data, 
                 var = input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1),
                 adj_dep = input$adj_dep_data_dash, la_select = input$la_names_data_dash)
      
    } else if (input$data_dash_plot_switch == "Correlation with deprivation") {
      
      plot_csc_points(csc_data, input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1), 
                      la_select = input$la_names_data_dash)
      
    } else if (input$data_dash_plot_switch == "Cluster") {
      
      plot_csc_hca(csc_data, var_list = input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1), 
                   la_select = input$la_names_data_dash, trend_hca = FALSE)
      
    } else if (input$data_dash_plot_switch == "Trend Cluster") {
     
      plot_csc_hca(csc_data, var_list = input$var_data_dash, year_range = seq(input$year_range_data_dash[1], input$year_range_data_dash[2], 1), 
                   la_select = input$la_names_data_dash, trend_hca = TRUE)
       
    }
        
      }
    
  })
  
  
  # Inputs/outputs for Spending Waffles
  
  updateSelectizeInput(session, 
                       "la_spending",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  output$waffle_plot <- renderPlot({
    
    if (input$la_spending == "") {
      
      spending_waffles20s(csc_data, la_select = NULL)
      
    } else {
    
    spending_waffles20s(csc_data, la_select = input$la_spending)
      
    }
    
    
  })
  
  

# Inputs for side by side trends ------------------------------------------


  
  updateSelectizeInput(session, 
                       "var_sbs_trend_left",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "var_sbs_trend_right",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)

  
  updateSelectizeInput(session, 
                       "la_select_sbs_trend_left",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "la_select_sbs_trend_right",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  # Outputs
  
  output$sbs_trend_plot_left <- renderPlot({
    
    plot_csctrend(data = csc_data, var = input$var_sbs_trend_left, year_range = seq(input$year_range_sbs_trend_left[1], input$year_range_sbs_trend_left[2], 1), adj_dep = input$adj_dep_sbs_trend_left, la_select = input$la_select_sbs_trend_left, include_avg = input$inc_avg_sbs_trend_left)
    
  })
  
  output$sbs_trend_plot_right <- renderPlot({
    
    plot_csctrend(data = csc_data, var = input$var_sbs_trend_right, year_range = seq(input$year_range_sbs_trend_right[1], input$year_range_sbs_trend_right[2], 1), adj_dep = input$adj_dep_sbs_trend_right, la_select = input$la_select_sbs_trend_right, include_avg = input$inc_avg_sbs_trend_right)
    
  })
  
  

# Side by side maps -------------------------------------------------------

  
  
  updateSelectizeInput(session, 
                       "var_sbs_map_left",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "var_sbs_map_right",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "la_select_sbs_map_left",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "la_select_sbs_map_right",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  # Outputs
  
  output$sbs_map_plot_left <- renderPlot({
    
    
    if (seq(input$year_range_sbs_map_left[1], input$year_range_sbs_map_left[2], 1) == seq(input$year_range_sbs_map_right[1], input$year_range_sbs_map_right[2], 1)) 
        {set_global_scales_left <- FALSE} else {set_global_scales_left <- TRUE}
    
    
    plot_csc_map(geo_data = la_sf, centroid_data = la_sf_centroids, data = csc_data, 
                 var = input$var_sbs_map_left, 
                 year_range = seq(input$year_range_sbs_map_left[1], input$year_range_sbs_map_left[2], 1), 
                 adj_dep = input$adj_dep_sbs_map_left, la_select = input$la_select_sbs_map_left, 
                 make_cart = input$cart_sbs_map_left,
                 global_scales = set_global_scales_left)
    
  })
  
  output$sbs_map_plot_right <- renderPlot({
    
    if (seq(input$year_range_sbs_map_left[1], input$year_range_sbs_map_left[2], 1) == seq(input$year_range_sbs_map_right[1], input$year_range_sbs_map_right[2], 1)) 
    {set_global_scales_right <- FALSE} else {set_global_scales_right <- TRUE}
    
    plot_csc_map(geo_data = la_sf, centroid_data = la_sf_centroids, data = csc_data, var = input$var_sbs_map_right, 
                 year_range = seq(input$year_range_sbs_map_right[1], input$year_range_sbs_map_right[2], 1), 
                 adj_dep = input$adj_dep_sbs_map_right, la_select = input$la_select_sbs_map_right, 
                 make_cart = input$cart_sbs_map_right, 
                 global_scales = set_global_scales_right)
    
  })
  
  
  

# cla ethnicity rates inputs/outputs --------------------------------------

  updateSelectizeInput(session, 
                       "var_cla_ethnicity",
                       choices = csc_ethnicity_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "la_select_cla_ethnicity",
                       choices = sort(unique(csc_ethnicity_data$LA)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  output$plot_cla_ethnicity <- renderPlot({
    
    if (input$var_cla_ethnicity %in% csc_ethnicity_vars[6:9]) {relative_arg <- TRUE} else {relative_arg <- FALSE}
    
    plot_ethnic_inequalities(data = csc_ethnicity_data, var = input$var_cla_ethnicity, la_select = input$la_select_cla_ethnicity, relative = relative_arg)
    
  })
  
  

# Inputs/Outputs for HCA --------------------------------------------------

  updateSelectizeInput(session, 
                       "var_hca",
                       choices = csc_vars,
                       options = list(maxOptions = 1000),
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "la_select_hca",
                       choices = sort(unique(csc_data$la_name)),
                       options = list(maxOptions = 1000),
                       selected = "",
                       server = TRUE)
  
  
  output$plot_hca <- renderPlot({
    
    if (input$trend_hca == FALSE) {make_trendhca <- FALSE} else {make_trendhca <- TRUE}
    
    plot_csc_hca(data = csc_data, var_list = input$var_hca, la_select = input$la_select_hca, year_range = seq(input$year_range_hca[1], input$year_range_hca[2], 1), trend_hca = make_trendhca)
    
    
  })
  

  
# Reactive select inputs
  updateSelectizeInput(session, 
                    "la_names_univ",
                    choices = sort(unique(lsoa_data_spatial$LA_name)),
                    options = list(maxOptions = 1000),
                    selected = "Sheffield",
                    server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_univ",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data_univ)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_corrs",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_anova",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "la_names_regress",
                       choices = sort(unique(lsoa_data_spatial$LA_name)),
                       options = list(maxOptions = 1000),
                       selected = "Sheffield",
                       server = TRUE)
  
  
  updateSelectizeInput(session, 
                       "x",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_corrs",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y_corrs",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_regress",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "IncmDpACIS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "y_regress",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  
  updateSelectizeInput(session, 
                       "x_anova",
                       choices = labels_lsoa_data$label[4:nrow(labels_lsoa_data)],
                       options = list(maxOptions = 1000),
                       # Select a random variable to plot from a list of variables shared between England and Wales
                       selected = labels_lsoa_data$label[labels_lsoa_data$var_name == "ChldAYPSDS"],
                       server = TRUE)
  

# Univariate maps  --------------------------------------------------------
  
  output$univ_map <- renderPlotly({
    
    xvar <- sym(labels_lsoa_data_univ$var_name[match(input$x_univ, labels_lsoa_data_univ$label)])
    
    draw_univariate_map(lsoa_data_spatial, !!xvar, input$bins_slider, input$la_names_univ)
    
  })
  
  output$univ_legend <- renderPlot({
    
    xvar <- sym(labels_lsoa_data_univ$var_name[match(input$x_univ, labels_lsoa_data_univ$label)])
    
    draw_univariate_legend(lsoa_data_spatial, !!xvar, nbins = input$bins_slider, input$la_names_univ)
    
  })
  
  output$legend <- renderPlotly({
    
    # Look up variable name string from pretty names in lookup table
    # Paste the matching variable name (ugly variable name)
    # Convert lookups from selectInput from strings to symbols that can be
    # passed using !!
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    
    draw_bivar_legend_plotly_coupled(!!xvar, !!yvar) %>%
      event_register('plotly_selected') %>%
      event_register('plotly_deselect')
    
  })
  

# Bivariate Maps ----------------------------------------------------------
  
  combinations <- reactiveVal()
  
  # On click, the key field of the event data contains the fill
  # Add that name to the set of all "selected" cars
  observeEvent(event_data("plotly_selected"), {
    combinations(NULL)
    fill_selected <- event_data("plotly_selected")$key
    combinations_old_new <- c(combinations(), fill_selected)
    combinations(unique(combinations_old_new))
  })
  
  #clear the set of combinations when a double-click occurs
  observeEvent(event_data("plotly_doubleclick"), {
    combinations(NULL)
  })
  
  observeEvent(event_data("plotly_deselect"), {
    combinations(NULL)
  })
  
  plotdata <- reactive({
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    plotdata <- bivar_cuts(lsoa_data_spatial, !!xvar, !!yvar, input$la_names)
    plotdata %>% filter(fill %in% combinations())
  })
  
  
  output$bivar_map <- renderPlotly({
    
    
    # Look up variable name string from pretty names in lookup table
    # Paste the matching variable name (ugly variable name)
    # Convert lookups from selectInput from strings to symbols that can be
    # passed using !!
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y, labels_lsoa_data_2$label)])
    

    
    plotdata_static <- plotdata()

    if (nrow(plotdata_static) == 0) {
    draw_bivariate_map(lsoa_data_spatial,
                               !!xvar,
                               !!yvar,
                               input$la_names)
    }
    else {
    draw_bivariate_map_coupled(plotdata_static,
                       !!xvar,
                       !!yvar,
                       input$la_names)
    }
      
     
})
  

  
  output$red_scale <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x, labels_lsoa_data$label)])
    
    draw_red_scale(lsoa_data_spatial, !!xvar, input$la_names) 
    
  })
  
  output$blue_scale <- renderPlot({
    
    yvar <- sym(labels_lsoa_data$var_name[match(input$y, labels_lsoa_data$label)])
    
    draw_blue_scale(lsoa_data_spatial, !!yvar, input$la_names) 
    
  })
  

# Correlations  -----------------------------------------------------------
  
  ### CORRELATIONS
  
  output$corrplot <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_corrs, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_corrs, labels_lsoa_data_2$label)])
    
    plot_corr(lsoa_data_spatial, !!xvar, !!yvar, input$la_names_corrs)
    
  })
  
  output$corr_result <- renderText({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_corrs, labels_lsoa_data$label)])
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_corrs, labels_lsoa_data_2$label)])
    
    corr_test(lsoa_data_spatial, !!xvar, !!yvar, input$la_names_corrs)
    
  })
  

# Anova -------------------------------------------------------------------

  
  output$anovaplot <- renderPlot({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    plot_anova(lsoa_data_spatial, !!xvar, input$la_names_anova)
    
  })
  
  output$meantable <- renderDataTable({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    datatable(means_table(lsoa_data_spatial, !!xvar, input$la_names_anova))
    
  })
  
  output$tukeytable <- renderDataTable({
    
    xvar <- sym(labels_lsoa_data$var_name[match(input$x_anova, labels_lsoa_data$label)])
    
    datatable(anova_tidy(lsoa_data_spatial, !!xvar, input$la_names_anova))
    
  })
  
  

# Regression --------------------------------------------------------------


  
  output$regress_table <- renderDataTable({
    
    xvars <- labels_lsoa_data %>% filter(label %in% input$x_regress)
    
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_regress, labels_lsoa_data_2$label)])
    
    datatable(
      regress_tidy(lsoa_data_spatial, y = !!yvar, x = xvars$var_name, input$la_names_regress) %>% 
        add_column(R.Squared = "") %>% 
        mutate(R.Squared = ifelse(Term == "(Intercept)", 
                                  tidy_rsq(lsoa_data_spatial, 
                                           y = !!yvar, 
                                           x = xvars$var_name, 
                                           input$la_names_regress), R.Squared))
    )
                                           
    
    
  })
  
  
  output$partial_plots <- renderPlot({
    
    xvars <- labels_lsoa_data %>% filter(label %in% input$x_regress)
    
    yvar <- sym(labels_lsoa_data_2$var_name[match(input$y_regress, labels_lsoa_data_2$label)])
    
    partial_regress(lsoa_data_spatial, y = !!yvar, x = xvars$var_name, input$la_names_regress)
    
    
  })
  

})
