
# Mapping Overlaps Gadget: Education GLOBAL FUNCTIONS ---------------------

# Packages
# There are probably some functions and packages 
# here that are no longer used and could be removed

library(shiny)
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(sf)
library(lazyeval)
library(rlang)
library(mltools)
library(RColorBrewer)
library(gsubfn)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(psych)
library(broom)
library(DT)
library(ggeffects)
library(ggeasy)
library(ggrepel)
library(tidymodels)
library(patchwork)
library(shadowtext)
library(waffle)
library(egg)
library(cowplot)
library(ggdendro)
library(janitor)
library(shinyWidgets)

dir.create('~/.fonts')
# file.copy("www/fa-brands-400.ttf", "~/.fonts")
file.copy("www/fa-solid-900.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')




install_fa_fonts() # For some reason FA5 glyphs still do not show on deployment

# This tool is designed to map the data from many administrative sources
# into univariate and bivariate maps, as well as to provide an interface 
# for various basic statistical tests and linear models


# Setup ---------------------------------------------------------------

# Loading page function:

spinner <- tagList(
  spin_pixel(),
  span("Loading the MOG...", style="color:white;")
)

# You will need to register for a mapbox token and save it as "mapbox_token.txt" run parts of the app
# https://www.mapbox.com 
mapbox_access_token = read_file("mapbox_token.txt")
Sys.setenv('MAPBOX_TOKEN' = mapbox_access_token)


# Load Pre-prepared data -------------------------------------------------------
### IMPORTANT! 
lsoa_data_spatial <- read_rds("data/mog_datav4.Rds")
labels_lsoa_data <- read_csv("data/lsoa_dataset_labelsv4.csv")
csc_data <- read_rds("data/csc_data.RDS") %>%
  mutate(value = ifelse(is.infinite(value), NA, value))
la_sf <- read_rds("data/la_sf.RDS")
la_sf_centroids <- read_rds("data/la_sf_centroids.RDS")
la_sf_cart <- read_rds("data/la_sf_cart.RDS")
la_sf_centroids_cart <- read_rds("data/la_sf_centroids_cart.RDS")
csc_ethnicity_data <- read_rds("data/csc_ethnicity_data.RDS")

csc_vars <- unique(csc_data$description)
csc_ethnicity_vars <- unique(csc_ethnicity_data$description)

# Tidy labels
labels_lsoa_data <- labels_lsoa_data %>% mutate(
  label = paste(label, " - (", var_name, ")", sep = "")
)

# Make some copies of labels for dropdowns
labels_lsoa_data_univ <- labels_lsoa_data
labels_lsoa_data_2 <- labels_lsoa_data

eng_wales_checker <- labels_lsoa_data$label[4:nrow(labels_lsoa_data_univ)] %>% str_detect("England & Wales")


# Functions ------------------------------------------------

### Create functions for automating bivariate maps

# Function for calculating tertiles.
tertile_values <- function(data, variable, la) {
  require("dplyr")
  require("lazyeval")
  variable = enquo(variable)
  
  filtered_data <- data %>% filter(LA_name %in% la)
  var_quantile <- filtered_data %>% pull(!!variable) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)
  
  if (sum(duplicated(as.numeric(var_quantile))) == 0) {
    print("Unique quantiles detected - Returning regular tertiles")
    return(var_quantile)
  } else {
    print("Non-unique quantiles detected - Setting tertile 1 as 0, tertile 2 as less than median, tertile 3 as greater than median.")
    
    rare_quantile <- filtered_data %>% filter(!!variable > 0) %>% 
                        pull(!!variable) %>% 
                        quantile(probs = seq(0, 1, length.out = 3))
    
    rare_quantile <- c(0, rare_quantile)
    
    names(rare_quantile) <- c("0%", "33.3333%", "66.66667%", "100%")
    
    return(rare_quantile)
  }
  
}


# Calculate cuts and join to colour scale
bivar_cuts <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  # enquote the variables
  x <- enquo(x)
  y <- enquo(y)
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  bivariate_colour_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>%
    gather("group", "fill")
  
  # Colour scale
  
  bivariate_colour_scale$bivar_key <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  
  # Filter data
  fil_data <- data %>% filter(LA_name %in% la)


# Return data with tertile combinations linked to colour scale  
mutate(fil_data,
      X_tertile = cut(
          # remember bang bangs for evaluation
          !!x,
          breaks = tertile_values(data, !!x, la),
          include.lowest = TRUE
          ),
       Y_tertile = cut(
         # remember bang bangs for evaluation
         !!y,
         breaks = tertile_values(data, !!y, la),
         include.lowest = TRUE
       ),
      bivar_group = paste(
        as.numeric(X_tertile), "-",
        as.numeric(Y_tertile)
      )
) %>%
  left_join(bivariate_colour_scale, by = c("bivar_group" = "group")
  )  

  
}


# Function for drawing bivariate maps:
draw_bivariate_map <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)

  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Colours lookup
  
  bivariate_scale <- c(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  )
  
  names(bivariate_scale) <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  

  # Drop unnecessary variables
  data_lite <- select(data, LSOA11CD, msoa11hclnm, !!x, !!y, LA_name)
  
  # Data transformations
  map_data <- bivar_cuts(data_lite, !!x, !!y, la)
  
  
  plot_mapbox(map_data,
              alpha = 0.6,
              alpha_stroke = 0.1,
              color = ~bivar_key,
              colors = bivariate_scale,
              text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                            y_st, ": ", round(eval(parse(text = y_st)), 2), "<br>",
                            "<b>", msoa11hclnm, "</b>", sep = ""),
              hovertemplate = paste("%{text}<extra></extra>"),
              showlegend = FALSE
  ) %>%
    layout(
      mapbox = list(zoom = 8,
                    style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                    accesstoken = mapbox_access_token),
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5)
    )
  

  
}

 
# Create function for key for Shiny App
draw_bivar_legend <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x), y_st = as.integer(!!y))
  
  legend <- ggplot() +
    geom_tile(data = bivariate_scale2, mapping = aes(x = !!x, y = !!y, fill = fill, alpha = 0.75)) +
    scale_fill_identity() +
    labs(x = paste("Higher", as_string(x_st), "\u2192"), y = paste("Higher", as_string(y_st), "\u2192")) +
    theme_minimal() +
    theme(    
      axis.line = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
      ) +
    coord_fixed()
  
  legend
  
}

# Create quantiles key
# for red
draw_red_scale <- function(data, x, la) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Scale values as table
    red_scale <- tibble(
      "3 - 1" = "#AE3A4E", # high IMD, low LAC
      "2 - 1" = "#BC7C8F",
      "1 - 1" = "#CABED0" # low IMD, low LAC
    ) %>% 
      gather(group, fill)
    
    # Name columns name of variable x
    red_scale <- red_scale %>%
      separate(group, into = c(as_string(x_st), "y", sep = " - ")) %>%
      mutate(x_st = as.integer(!!x),
             y = as.integer(y))
    
    first_tertile <- paste(round(tertile_values(data, !!x, la)[1], 1), 
                            "-", 
                            round(tertile_values(data, !!x, la)[2], 1))
    second_tertile <- paste(round(tertile_values(data, !!x, la)[2], 1), 
                           "-", 
                           round(tertile_values(data, !!x, la)[3], 1))
    third_tertile <- paste(round(tertile_values(data, !!x, la)[3], 1), 
                            "-", 
                            round(tertile_values(data, !!x, la)[4], 1))
    
    red_scale <- red_scale %>% arrange(x_st)
    
    red_scale$tertile_vals <- c(first_tertile, 
                                second_tertile, 
                                third_tertile)
    
    ggplot() +
      geom_tile(data = red_scale, mapping = aes(x = !!x, y = y, fill = fill, alpha = 0.75)) +
      geom_text(data = red_scale, aes(x = !!x, y = y, label = tertile_vals)) +
      scale_fill_identity() +
      labs(x = paste("Higher", as_string(x_st), "\u2192"), y = "") +
      theme_minimal() +
      theme(    
        axis.line = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = rel(1)),
        legend.position = "none"
      ) +
      coord_fixed(1/6)
}



# for blue
draw_blue_scale <- function(data, x, la) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Scale values as table
  red_scale <- tibble(
    "3 - 1" = "#4885C1", # high IMD, low LAC
    "2 - 1" = "#89A1C8",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  # Name columns name of variable x
  red_scale <- red_scale %>%
    separate(group, into = c(as_string(x_st), "y", sep = " - ")) %>%
    mutate(x_st = as.integer(!!x),
           y = as.integer(y))
  
  first_tertile <- paste(round(tertile_values(data, !!x, la)[1], 1), 
                         "-", 
                         round(tertile_values(data, !!x, la)[2], 1))
  second_tertile <- paste(round(tertile_values(data, !!x, la)[2], 1), 
                          "-", 
                          round(tertile_values(data, !!x, la)[3], 1))
  third_tertile <- paste(round(tertile_values(data, !!x, la)[3], 1), 
                         "-", 
                         round(tertile_values(data, !!x, la)[4], 1))
  
  red_scale <- red_scale %>% arrange(x_st)
  
  red_scale$tertile_vals <- c(first_tertile, 
                              second_tertile, 
                              third_tertile)
  
  ggplot() +
    geom_tile(data = red_scale,
              mapping = aes(x = !!x, y = y, fill = fill, alpha = 0.75)) +
    geom_text(data = red_scale, aes(x = !!x, y = y,label = tertile_vals)) +
    scale_fill_identity() +
    labs(x = paste("Higher", as_string(x_st), "\u2192"), y = "") +
    theme_minimal() +
    theme(    
      axis.line = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
    ) +
    coord_fixed(1/6)
}



# Univariate maps ---------------------------------------------------------

# Function to convert into ntiles and assign custom colour scale called fill
# This is still not working perfectly - the way bins are split - needs to be changed, maybe using cut_interval. Low priority

bin_variable <- function(data, variable, la, nbins) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  
  variable = enquo(variable)
  variable_st = get_expr(variable)
  nbins = nbins
  
  # filter data
  filtered_data <- data %>% filter(LA_name %in% la)
  
  var_binned <- filtered_data %>% mutate(
    binned_var = bin_data(round(!!variable, 3), bins = nbins, binType = "quantile")
  ) %>%
    mutate(
      binned_var = str_replace_all(binned_var, "\\,", " -")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\[")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\]")
    ) %>%
    mutate(
      binned_var = str_remove(binned_var, "\\(|\\)")
    ) %>%
    mutate(
      binned_var = paste0(variable_st, ": ", binned_var)
    )
  
  # Write 10 bins version of bins
  var_binned  <- var_binned %>% mutate(
    bin_upper = as.numeric(str_extract(binned_var, "(?<=- )[0-9]*.*"))
  )

  # Join numerically ordered bins (for sorting)
  
  bin_lth_lookup <- tibble("bin_upper" = unique(sort(var_binned$bin_upper))) %>%
    arrange(bin_upper)
  
  bin_lth_lookup <- bin_lth_lookup %>% mutate(bin_low_high = as.numeric(rownames(bin_lth_lookup)))

  var_binned <- var_binned %>% left_join(bin_lth_lookup, by = "bin_upper")

  # Create dynamic colour lookup with rcolorbrewer and ramp
  bin_col_lookup <- tibble(
    "bin_low_high" = seq(1, length(unique(var_binned$bin_upper)), 1),
    "bin_colour" = colorRampPalette(brewer.pal(8, "BuGn"))(length(unique(var_binned$bin_upper)))
  )

  var_binned <- var_binned %>% left_join(bin_col_lookup,
                                         by = "bin_low_high") %>%
    mutate(
      binned_var = paste0("<br>Bin ", bin_low_high, ": <br>", binned_var, "<br>")
    )

 return(var_binned)

}







# Custom univariate colour scale (remember needs number of bins)
univariate_scale <- function(nbins) {
  nbins = nbins
  
  univariate_scale <- tibble(
    bin_low_high = seq(1, nbins, 1),
    bin_col_scale = colorRampPalette(brewer.pal(8, "BuGn"))(nbins+3)[3:(nbins+2)]
  )
  
  return(univariate_scale)
  
}


# TROUBLESHOOTING
# bin_variable(lsoa_data_spatial, variable = WhtBrtsPOP, nbins = 3, la = "Sheffield") %>% .$bin_colour
# bin_variable(lsoa_data_spatial, variable = WhtBrtsPOP, nbins = 3, la = "Sheffield") %>% .$bin_low_high
# bin_variable(lsoa_data_spatial, variable = WhtBrtsPOP, nbins = 3, la = "Sheffield") %>% .$binned_var
#

#univariate_scale(nbins = 3)
#univariate_scale(nbins = 5)


# Function for drawing univariate plot ------
draw_univariate_map  <- function(data, x, nbins, la) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  require("plotly")
  
  x <- enquo(x)
  x_st <- get_expr(x)
    
  # Create bins
  data <- bin_variable(data, !!x, la, nbins)
    
    
    # Create colour scale for bins
    univariate_col_lookup_dt <- left_join(univariate_scale(nbins), 
                                        data %>%
                                          group_by(binned_var) %>%
                                          summarise(
                                            bin_low_high = first(bin_low_high)
                                          ),
                                        by = "bin_low_high"
                                      ) %>%
                                drop_na()
    
    
    # Create lookup vector for colour scale that can be read by Plotly
    univariate_col_lookup <- setNames(c(univariate_col_lookup_dt$bin_col_scale), 
                                      univariate_col_lookup_dt$binned_var)
    
    # Create plot - could do quantile version but wouldn't work with some data
    plot_mapbox(data,
                alpha = 0.5,
                alpha_stroke = 0.1,
                color = ~binned_var,
                colors = univariate_col_lookup,
                text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                              "<b>", msoa11hclnm, "</b>", sep = ""),
                hovertemplate = paste("%{text}<extra></extra>"),
                showlegend = FALSE
    ) %>%
      layout(
        mapbox = list(zoom = 8,
                      style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                      accesstoken = mapbox_access_token)
      )

}
    


# TROUBLESHOOTING
# univariate_scale(nbins = 3)
# 
# left_join(
#   univariate_scale(nbins = 3),
#   bin_variable(lsoa_data_spatial, variable = WhtBrtsPOP, nbins = 3, la = "Sheffield") %>%
#     group_by(bin_low_high) %>%
#     summarise(
#       bin_low_high = first(bin_low_high),
#       bin_col_scale = first(bin_col_scale), 
#       range = paste(round(min(WhtBrtsPOP), 3), "-", round(max(WhtBrtsPOP),3))
#     ), 
#   by = "bin_low_high")
# 

    
# Function for ggplot univariate map legend
draw_univariate_legend  <- function(data, x, nbins, la) {
  require("dplyr")
  require("lazyeval")
  require("mltools")
  require("plotly")
  
  x <- enquo(x)
  x_st <- get_expr(x)
  
  # Create bins
  data <- bin_variable(data, !!x, la, nbins)
  
  
  # Create colour scale for bins
  univariate_col_lookup_dt <- left_join(
    
    univariate_scale(nbins = nbins),
    
    data %>%
      group_by(bin_low_high) %>%
      summarise(
        bin_low_high = first(bin_low_high),
        range = paste(round(min(!!x), 3), "-", round(max(!!x),3))
      ), 
    
    by = "bin_low_high")
  
  univariate_col_lookup_dt$y <- rep(1, times = nrow(univariate_col_lookup_dt))
  
  # univariate_col_lookup_dt <- univariate_col_lookup_dt %>% arrange(bin_low_high) %>%
  #   mutate(bin_low_high_valid = seq(1, nrow(univariate_col_lookup_dt), 1)
  #          )

  ggplot() +
    scale_fill_identity() +
    geom_tile(
      data = univariate_col_lookup_dt,
      mapping = aes(
        x = bin_low_high,
        y = y,
        fill = bin_col_scale,
        alpha = 0.75)
    ) +
    geom_text(
      data = univariate_col_lookup_dt,
      aes(
        x = bin_low_high,
        y = y,
        label = range
      )) +
    labs(x = paste("Higher", as_string(x_st), "\u2192"),
         y = "") +
    theme_minimal(
    ) +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = rel(1)),
      legend.position = "none"
    ) +
    coord_fixed(1/8)

  
}


# Plotly bivariate legend

draw_bivar_legend_plotly <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x),
           y_st = as.integer(!!y))
  
  # Add interpretable names
   bivariate_scale2$tilenames <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
   
   # Need to create numeric codes for fills and then can use colors argument linked with names
   bivariate_scale2 <- bivariate_scale2 %>%
     mutate(x_st = as.numeric(x_st),
            y_st = as.numeric(y_st),
            colkey = paste(x_st, " - ", y_st, sep = ""),
            colkey_numeric = seq(1, nrow(.), 1))
   
   colkey = bivariate_scale2$fill
   
   colkey <- setNames(colkey, bivariate_scale2$colkey_numeric)
   
   
   plot_ly(bivariate_scale2,
           x = ~x_st,
           y = ~y_st,
           z = ~colkey_numeric,
           text = ~tilenames,
           hoverinfo = "text",
           colors = colkey,
           type = "heatmap",
           showscale = FALSE
   ) %>% 
     layout(
       xaxis = list(title = paste0("Higher ", x_st, " \u2192"),
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticks = FALSE,
                    autotick = FALSE,
                    # Fix coords equal
                    scaleanchor = "y", 
                    scaleratio = 1),
       yaxis = list(title = paste0("Higher ", y_st, " \u2192"),
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE,
                    zeroline = FALSE,
                    autotick = FALSE,
                    scaleanchor = "x", 
                    scaleratio = 1)
     )
  
  
}


draw_bivariate_map_coupled <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Colours lookup
  bivariate_scale <- c(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  )

  names(bivariate_scale) <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )

  
  plot_mapbox(data,
              alpha = 0.6,
              alpha_stroke = 0.1,
              color = ~bivar_key,
              colors = bivariate_scale,
              text = ~paste(x_st, ": ", round(eval(parse(text = x_st)), 2), "<br>",
                            y_st, ": ", round(eval(parse(text = y_st)), 2), "<br>",
                            "<b>", msoa11hclnm, "</b>", sep = ""),
              hovertemplate = paste("%{text}<extra></extra>"),
              showlegend = FALSE
  ) %>%
    layout(
      mapbox = list(zoom = 8,
                    style = "mapbox://styles/drcalumwebb/ck55r34nt00yh1dol4wfi3xi4",
                    accesstoken = mapbox_access_token),
      legend = list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5)
    )
  
  
  
}


draw_bivar_legend_plotly_coupled <- function(x, y) {
  require(rlang)
  require(dplyr)
  require(tidyverse)
  
  x <- enquo(x)
  y <- enquo(y)
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  # Bivariate colour scale
  bivariate_scale <- tibble(
    "3 - 3" = "#3F2949", # high IMD, high LAC
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low IMD, high LAC
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium IMD, medium LAC
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high IMD, low LAC
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low IMD, low LAC
  ) %>% 
    gather(group, fill)
  
  
  bivariate_scale2 <- bivariate_scale %>%
    separate(group, into = c(as_string(x_st), as_string(y_st)), sep = " - ") %>%
    mutate(x_st = as.integer(!!x),
           y_st = as.integer(!!y))
  
  # Add interpretable names
  bivariate_scale2$tilenames <- c(
    paste("<br>", "3 (High) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 3 (High) X, 3 (High) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "3 (High) ", y_st, "<br>", sep = ""), # 1 (Low) X, 3 (High) Y
    paste("<br>", "3 (High) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""), # medium X, medium Y
    paste("<br>", "1 (Low) ", x_st, " -<br>", "2 (Mid) ", y_st, "<br>", sep = ""),
    paste("<br>", "3 (High) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""), # 3 (High) X, 1 (Low) Y
    paste("<br>", "2 (Mid) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = ""),
    paste("<br>", "1 (Low) ", x_st, " -<br>", "1 (Low) ", y_st, "<br>", sep = "") # 1 (Low) X, 1 (Low) Y
  )
  
  # Need to create numeric codes for fills and then can use colors argument linked with names
  bivariate_scale2 <- bivariate_scale2 %>%
    mutate(x_st = as.numeric(x_st),
           y_st = as.numeric(y_st),
           colkey = paste(x_st, " - ", y_st, sep = ""),
           colkey_numeric = seq(1, nrow(.), 1))
  
  colkey = bivariate_scale2$fill
  
  colkey <- setNames(colkey, bivariate_scale2$colkey_numeric)
  

  plot_ly(
    bivariate_scale2,
          x = ~x_st,
          y = ~y_st,
          z = ~colkey_numeric,
          text = ~tilenames,
          hoverinfo = "none",
          colors = colkey,
          alpha = 0.6,
          type = "heatmap",
          showscale = FALSE,
          key = ~fill
  ) %>%
    add_trace(bivariate_scale2,
              type = "scatter",
                x = ~x_st,
                y = ~y_st,
                text = ~tilenames,
                hoverinfo = "text",
                color = ~colkey_numeric,
                colors = colkey,
                showlegend = FALSE,
                showscale = FALSE,
                marker = list(size = 40),
                source = "subset",
                key = ~fill) %>%
    hide_colorbar() %>%
    layout(
      xaxis = list(title = paste0("Higher ", x_st, " \u2192"),
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE,
                   showticks = FALSE,
                   autotick = FALSE,
                   # Fix coords equal
                   scaleanchor = "y", 
                   scaleratio = 1),
      yaxis = list(title = paste0("Higher ", y_st, " \u2192"),
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE,
                   zeroline = FALSE,
                   autotick = FALSE,
                   scaleanchor = "x", 
                   scaleratio = 1),
      clickmode = "select+event",
      hovermode = "closest",
      showscale = FALSE
    ) 

  
}



# Function for Scatterplot/Correlation Tests ------------------------------

# Function for Scatterplot

plot_corr <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  y_label <- lookup[lookup$var_name == y_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  data %>% ggplot() +
    geom_point(aes(x = !!x, y = !!y, col = !!y), size = 3) +
    geom_smooth(aes(x = !!x, y = !!y), method = "lm", alpha = 0.3,
                col = "black") +
    ylab(str_wrap(y_label, 60)) +
    xlab(str_wrap(x_label, 60)) +
    theme_minimal() +
    scale_color_viridis_c() +
    theme(legend.position = "none",
          text = element_text(size = 16))
  
  
}


corr_test <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  y <- enquo(y)
  
  x_st <- get_expr(x)
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  x_label <- str_remove_all(lookup[lookup$var_name == x_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  x_label <- str_remove_all(x_label, "\\- ")
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  correlation <- corr.test(data[[x_st]], data[[y_st]])
  
  paste("The correlation between ", x_label, "and", y_label, "is", round(correlation$r, 3), ".<br><br>",
        ifelse(correlation$r > 0, paste("This is a positive correlation, meaning that as", x_label, "increases", y_label, "also tends to increase."),
               paste("This is a negative correlation, meaning that as", x_label, "increases", y_label, "tends to decrease.")), "<br><br>",
        ifelse(correlation$p < 0.05, "The p-value for this correlation is less than 0.05, meaning that the relationship is statistically significant and we have evidence to reject the idea of no correlation in the wider population.",
               "The p-value for this correlation is greater than 0.05, meaning that the relationship is not statistically significant and we do not have strong enough evidence to reject the idea of no correlation in the wider population."),
        "<br><br>The exact p-value is: ", format(correlation$p, scientific = F)
               )
  

}



# ANOVA plots -------------------------------------------------------------

plot_anova <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  data %>% ggplot() +
    geom_density(aes(x = !!x, fill = LA_name, col = LA_name),
                 alpha = 0.2, size = 1.5) +
    xlab(str_wrap(x_label, 60)) +
    theme_minimal() +
    theme(text = element_text(size = 16)) 
  
  
}


# Function for table of means

means_table <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
 data <- data %>% filter(LA_name %in% la) %>%
    select(LA_name, !!x) %>%
    group_by(LA_name) %>%
    summarise(
      !!x_st := round(mean(!!x, na.rm = TRUE), 3)
    )

  st_set_geometry(data, NULL)
  
}


# Function for ANOVA test with TukeyHSD

anova_tidy <- function(data, x, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  x <- enquo(x)
  
  lookup <- labels_lsoa_data
  
  x_st <- get_expr(x)
  
  x_label <- lookup[lookup$var_name == x_st,][[2]]
  
  data <- data %>% filter(LA_name %in% la)
  
  aov_model <- aov(data = data,
      formula = eval(parse(text = paste(x_st))) ~ LA_name)
  
  
  tidy(TukeyHSD(aov_model)) %>%
    select(Comparison = contrast,
           Mean_Diff = estimate,
           P_Value = adj.p.value) %>%
    mutate(
      Mean_Diff = round(Mean_Diff, 3),
      P_Value = ifelse(round(P_Value, 3) < 0.001, "<0.001", round(P_Value, 3))
    )
  
  
}


# Function for regression and marginal effects  ---------------------------

# Function for regression model

regress <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  if (length(la) > 1) {
    
    data <- data %>%
      mutate(LA_name = relevel(factor(LA_name), ref = la[1]))
    
    model <- lm(data = data,
                formula = paste(y_st, "~", ifelse(!x == "", paste(x, collapse = "+"), paste(1, collapse = "+")), "+ I(LA_name)"))
    
    return(model)
    
  } else {
  
  model <- lm(data = data,
              formula = paste(y_st, "~", paste(x, collapse = "+")))
  
  return(model)
  }
  
}


# Standardised estimates

regress_std <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  require(broom)
  
  y <- enquo(y)
  
  y_st <- get_expr(y)
  
  lookup <- labels_lsoa_data
  
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  
  y_label <- str_remove_all(y_label, "\\- ")
  
  data <- data %>% filter(LA_name %in% la)
  
  if (length(la) > 1) {
    
    data <- data %>%
      mutate(LA_name = relevel(factor(LA_name), ref = la[1]))
    
    model <- lm(data = data,
                formula = paste("scale(", y_st, ")", "~", ifelse(!x == "", paste("scale(", paste(x), ")", collapse = " + "), paste(1, collapse = " + ")), paste0("+ I(LA_name)")))
    
    return(model)
    
  } else {
  
  model <- lm(data = data,
              formula = paste("scale(", y_st, ")", "~", paste0("scale(", x, ")", collapse = " + ")))
  
  return(model)
  
  }
  
}


# Function for creating and tidying regression models

regress_tidy <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  model_std <- regress_std(data, x, !!y, la)
  
  tidymodel <- tidy(model_unst) %>%
                 mutate_at(vars(estimate:p.value), ~round(., 3)) %>%
                 select(Term = term, Coefficient = estimate, Std.Error = std.error, t = statistic, P.Value = p.value) %>%
    mutate(Term = ifelse(str_detect(Term, "I\\(LA_name"), paste(Term, "=", row_number(Term)), Term))

  tidy_std <- tidy(model_std) %>%
    mutate_at(vars(estimate:p.value), ~round(., 3)) %>%
    select(Term = term, Beta = estimate, Std.Error = std.error, t = statistic, P.Value = p.value)

  tidymodel %>% mutate(
    Beta = tidy_std$Beta
  ) %>% select(Term, Coefficient, Beta, everything()) %>%
    left_join(lookup, by = c("Term" = "var_name")) %>%
    mutate(label = str_replace_all(str_remove_all(str_remove_all(label, "\\(.*?\\)"), "\\- "), "  ", " ")
           ) %>%
    mutate(
      Term = ifelse(is.na(label), Term, label),
      P.Value = ifelse(P.Value < 0.001, "<0.001", P.Value)
    ) %>%
    mutate(
      Term = str_remove_all(Term, "I\\(LA_name\\)")
    ) %>%
    mutate(
      Term = str_trim(Term)
    ) %>%
    select(-label)

  
}


# R squared

tidy_rsq <- function(data, x, y, la) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  
  round(summary(model_unst)$r.squared, 3)
  
}




# Plot partial regression

partial_regress <- function(data, x, y, la) {
  require(dplyr)
  require(lazyeval)
  require(rlang)
  require(plotly)
  
  y <- enquo(y)
  y_st <- get_expr(y)
  lookup <- labels_lsoa_data
  y_label <- str_remove_all(lookup[lookup$var_name == y_st,][[2]], "\\(.*?\\)")
  y_label <- str_remove_all(y_label, "\\- ")
  data <- data %>% filter(LA_name %in% la)
  
  model_unst <- regress(data = data, x = x, y = !!y, la = la)
  
  ggpredict(model_unst) %>% plot(rawdata = FALSE,
                                 jitter = FALSE,
                                 grid = TRUE,
                                 show.x.title = FALSE,
                                 colors = "eight",
                                 dot.alpha = 0.2)
  
}


# partial_regress(lsoa_data_spatial, 
#                 c("LvlOfNtrgD", "IncmDpACIS", "AccssbltGP", "AccssbltyT"), 
#                 LEAB2009_2, c("Sheffield", "York", "Barnsley", "Camden"))
# 
# 
# 
# 
# datatable(regress_tidy(lsoa_data_spatial, c("LvlOfNtrgD", "IncmDpACIS"), 
#             LEAB2009_2, "Sheffield"))



# function for trend plot -------------------------------------------------

# If LA is empty show average

plot_csctrend <- function(data, var, year_range = NULL, adj_dep = "No", la_select = NULL, include_avg = TRUE) {

  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  if (adj_dep == "No") {
    data <- data %>% filter(description == var & !la_name %in% c("City of London", "Isles of Scilly"))
  } else {
    data <- data %>% filter(description == var | description == csc_vars[18] & !la_name %in% c("City of London", "Isles of Scilly"))
  }
  
  if (include_avg == TRUE & !is.null(la_select)) {
    la_select <- ordered(c("England Average", la_select), levels = c("England Average", la_select))
  }
  
  if (include_avg == TRUE & is.null(la_select)) {
    
    la_select <- ordered(c("England Average"), levels = c("England Average"))
    
  }
  
  if (include_avg == TRUE) {
    avg_select <- "England Average"
  } else {
    avg_select <- ""
  }
  
  # Check whether year range is null and if so, limit to range in data
  # Check if asked for range is larger than data range, and if so, change
  # to data range
  # year_range <- if (is.null(year_range)) {
  #   
  #   # if no year range given then select min max range
  #   seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)
  #   
  # } else {
  #   
  #   if (length(year_range) > length(seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1))) {
  #     # If year range is given but it's longer than actual available data, select only available data  
  #     seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)
  #     
  #   } else {
  #     # otherwise accept given year range
  #    year_range 
  #     
  #   }
  #   
  # }
  
 
if (adj_dep == "No") {  
    
    if (is.null(la_select)) { # if Deprivation adjust is off
      
      print(year_range)
      
      year_range <- if (is.null(year_range)) {seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)} else {year_range}
      
      print(year_range)
      
      # No LA selected
      data <- data %>% filter(year %in% year_range)
      
      data %>% 
        add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                la_name = rep("England Average", times = length(unique(.$year))),
                year = sort(unique(.$year)),
                value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value
                ) %>%
        ggplot() +
        geom_line(aes(group = la_name, x = year, y = value), alpha = 0.1) +
        geom_line(data = . %>% filter(la_name %in% la_select),
                  aes(group = la_name, x = year, y = value, col = la_name), size = 1.5) +
        geom_label(data = . %>% filter(la_name %in% la_select), size = 3.5,
                   aes(group = la_name, x = year, y = value, col = la_name, label = round(value, 2))) +
        scale_colour_brewer(palette = "Dark2") +
        scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                        max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
        coord_cartesian(xlim = c(min(unique(data$year)), max(unique(data$year) + (max(unique(data$year) - min(unique(data$year))))/3  ))) +
        # Add legend with ggrepel
        geom_text_repel(data = . %>% filter(year == max(year_range) & la_name %in% la_select),
                        aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value, label = la_name, col = la_name),
                        segment.alpha = 0, direction = "y", fontface = "bold") +
        xlab("Year Ending") +
        ylab(str_wrap(var, 80)) +
        ggtitle(str_wrap(var, width = 80)) +
        theme_minimal() +
        theme(legend.position = "top",
              plot.title = element_text(face = "bold")) +
        ggeasy::easy_remove_legend() +
        theme(panel.grid = element_blank())
      
    } else {
      
      print(year_range)
      
      year_range <- if (is.null(year_range)) {seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)} else {year_range}
      
      print(year_range)
      
      data <- data %>% 
        filter(year %in% year_range)
      
      cst_plot <- data %>% add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                                   la_name = rep("England Average", times = length(unique(.$year))),
                                   year = sort(unique(.$year)),
                                   value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value
      ) %>%
        ggplot() +
        geom_line(aes(group = la_name, x = year, y = value), alpha = 0.1) +
        geom_line(data = . %>% filter(la_name %in% la_select & la_name != "England Average"),
                  aes(group = la_name, x = year, y = value, col = la_name), size = 1.5) +
        geom_label(data = . %>% filter(la_name %in% la_select & la_name != "England Average"), size = 3.5,
                   aes(group = la_name, x = year, y = value, col = la_name, label = round(value, 2))) +
        geom_line(data = . %>% filter(la_name == avg_select),
                  aes(group = la_name, x = year, y = value), size = 1.5, col = "#00A174") +
        geom_label(data = . %>% filter(la_name == avg_select), size = 3.5,
                   aes(group = la_name, x = year, y = value, label = round(value, 2)), col = "#00A174") +
        scale_colour_manual(values = brewer.pal(8, "Dark2")[2:9]) +
        scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                        max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
        coord_cartesian(xlim = c(min(unique(data$year)), max(unique(data$year) + (max(unique(data$year) - min(unique(data$year))))/3  ))) +
        # Add legend with ggrepel
        geom_text_repel(data = . %>% filter(year == max(year_range) & la_name %in% la_select & la_name != "England Average"),
                        aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value, label = la_name, col = la_name),
                        segment.alpha = 0, direction = "y", fontface = "bold") +
        geom_text_repel(data = . %>% filter(year == max(year_range) & la_name ==  avg_select),
                        aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value, label = la_name),
                        segment.alpha = 0, direction = "y", fontface = "bold", col = "#00A174") +
        xlab("Year Ending") +
        ylab(str_wrap(var, 80)) +
        ggtitle(str_wrap(var, width = 80)) +
        theme_minimal() +
        ggeasy::easy_remove_legend() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(face = "bold"))
      
      return(cst_plot)
      
    }} else { # If dep adjusted = Yes 
      
      
      if (is.null(la_select)) {
      
        print("adjusting for deprivation - no la select")
        
        data <- data %>% 
          filter(description == var | description == csc_vars[18]) %>%
          pivot_wider(values_from = value, names_from = description) %>%
          select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
          rename(imd = 4, value = 5) %>%
          filter(!is.na(imd) & !is.na(value)) %>%
          group_by(year) %>%
          do(., tidy(lm(value ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
          filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
          select(year, imd_adj_est = estimate) %>%
          right_join(., data %>% filter(description == var | description == csc_vars[18]), by = "year") %>%
          ungroup() %>%
          pivot_wider(values_from = value, names_from = description) %>%
          select(year:la_name, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
          rename(imd = 5, value = 6) %>%
          mutate(
            imd = imd - mean(imd, na.rm = TRUE),
            imd_adj = imd * imd_adj_est,
            value_adj = value - imd_adj
          ) %>%
          filter(!is.na(value_adj))
        
        year_range <- if (is.null(year_range)) {seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)} else {year_range}
        
        data <- data %>% filter(year %in% year_range)
        
        # change value to value_adjusted
        data %>% 
          add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                  la_name = rep("England Average", times = length(unique(.$year))),
                  year = sort(unique(.$year)),
                  value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value,
                  value_adj = data %>% group_by(year) %>% summarise(value_adj = mean(value_adj, na.rm = TRUE)) %>% .$value_adj
          ) %>%
          ggplot() +
          geom_line(aes(group = la_name, x = year, y = value_adj), alpha = 0.1) +
          geom_line(data = . %>% filter(la_name %in%  avg_select),
                    aes(group = la_name, x = year, y = value_adj, col = la_name), size = 1.5) +
          geom_label(data = . %>% filter(la_name %in%  avg_select), size = 3.5,
                     aes(group = la_name, x = year, y = value_adj, col = la_name, label = round(value_adj, 2))) +
          scale_colour_brewer(palette = "Dark2") +
          scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                          max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
          coord_cartesian(xlim = c(min(unique(data$year)), max(unique(data$year) + (max(unique(data$year) - min(unique(data$year))))/3  ))) +
          # Add legend with ggrepel
          geom_text_repel(data = . %>% filter(year == max(year_range) & la_name %in%  avg_select),
                          aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value_adj, label = la_name, col = la_name),
                          segment.alpha = 0, direction = "y", fontface = "bold") +
          xlab("Year Ending") +
          ylab(str_wrap(var, 80)) +
          ggtitle(str_wrap(paste("Predicted", var, "(If all LAs had average Deprivation)"), width = 80)) +
          theme_minimal() +
          theme(legend.position = "top") +
          ggeasy::easy_remove_legend() +
          theme(panel.grid = element_blank(),
                plot.title = element_text(face = "bold"))
        
        
      
      } else {
      
        print("adjusting for deprivation - la select")
        
        data <- data %>% 
          filter(description == var | description == csc_vars[18]) %>%
          pivot_wider(values_from = value, names_from = description) %>%
          select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
          rename(imd = 4, value = 5) %>%
          filter(!is.na(imd) & !is.na(value)) %>%
          group_by(year) %>%
          do(., tidy(lm(value ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
          filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
          select(year, imd_adj_est = estimate) %>%
          right_join(., data %>% filter(description == var | description == csc_vars[18]), by = "year") %>%
          ungroup() %>%
          pivot_wider(values_from = value, names_from = description) %>%
          # Important - reorder columns so rename is correct!
          select(year:la_name, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
          rename(imd = 5, value = 6) %>%
          mutate(
            imd = imd - mean(imd, na.rm = TRUE),
            imd_adj = imd * imd_adj_est,
            value_adj = value - imd_adj
          ) %>%
          filter(!is.na(value_adj))
        
        print(data)
        
        year_range <- if (is.null(year_range)) {seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)} else {year_range}
        
        print(year_range)
        
        data <- data %>% filter(year %in% year_range)
        
        cst_plot <- data %>% add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                                     la_name = rep("England Average", times = length(unique(.$year))),
                                     year = sort(unique(.$year)),
                                     value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value,
                                     value_adj = data %>% group_by(year) %>% summarise(value_adj = mean(value_adj, na.rm = TRUE)) %>% .$value_adj
        ) %>%
          ggplot() +
          geom_line(aes(group = la_name, x = year, y = value_adj), alpha = 0.1) +
          geom_line(data = . %>% filter(la_name %in% la_select & la_name != "England Average"),
                    aes(group = la_name, x = year, y = value_adj, col = la_name), size = 1.5) +
          geom_label(data = . %>% filter(la_name %in% la_select & la_name != "England Average"), size = 3.5,
                     aes(group = la_name, x = year, y = value_adj, col = la_name, label = round(value_adj, 2))) +
          geom_line(data = . %>% filter(la_name ==  avg_select),
                    aes(group = la_name, x = year, y = value_adj), size = 1.5, col = "#00A174") +
          geom_label(data = . %>% filter(la_name ==  avg_select), size = 3.5,
                     aes(group = la_name, x = year, y = value_adj, label = round(value_adj, 2)), col = "#00A174") +
          scale_colour_manual(values = brewer.pal(8, "Dark2")[2:9]) +
          scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                          max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
          coord_cartesian(xlim = c(min(unique(data$year)), max(unique(data$year) + (max(unique(data$year) - min(unique(data$year))))/3  ))) +
          # Add legend with ggrepel
          geom_text_repel(data = . %>% filter(year == max(year_range) & la_name %in% la_select & la_name != "England Average"),
                          aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value_adj, label = la_name, col = la_name),
                          segment.alpha = 0, direction = "y", fontface = "bold") +
          geom_text_repel(data = . %>% filter(year == max(year_range) & la_name ==  avg_select),
                          aes(x = year + (max(unique(data$year) - min(unique(data$year))))/6, y = value_adj, label = la_name),
                          segment.alpha = 0, direction = "y", fontface = "bold", col = "#00A174") +
          xlab("Year Ending") +
          ylab(str_wrap(var, 80)) +
          ggtitle(str_wrap(paste("Predicted", var, "(If all LAs had average Deprivation)"), width = 80)) +
          theme_minimal() +
          ggeasy::easy_remove_legend() +
          theme(panel.grid = element_blank(),
                plot.title = element_text(face = "bold"))
        
        return(cst_plot)
        
    }
  
    }
  }
  


# Fixing English average data not being sorted properly for CIN Census data


# 
# # Add adjustment for deprivation
# 
# plot_csctrend(data = csc_data, var = csc_vars[19])
# plot_csctrend(data = csc_data, var = csc_vars[10], year_range = 2011:2015)
# plot_csctrend(data = csc_data, var = csc_vars[2], la_select = c("Sheffield", "England Average"))
# plot_csctrend(data = csc_data, var = csc_vars[15], la_select = c("Sheffield", "Derby", "Rotherham", "Kent", "Leicester", "Nottingham", "Leeds", "Hackney"), include_avg = FALSE)
# plot_csctrend(data = csc_data, var = csc_vars[2], la_select = c("Sheffield", "Derby"), year_range = 2011:2015)
# 
# 
# plot_csctrend(data = csc_data, var = csc_vars[7], adj_dep = "Yes")
# plot_csctrend(data = csc_data, var = csc_vars[7], adj_dep = "No")
# 
# 
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "No", la_select = c("Leeds", "York", "Barnsley")) +
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "Yes", la_select = c("Leeds", "York", "Barnsley"))
# 
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "No", la_select = c("Leeds", "York", "England Average"), year_range = 2016:2019)
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "Yes", la_select = c("Leeds", "York", "England Average"), year_range = 2016:2019)
# 
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "No", year_range = 2016:2019)
# plot_csctrend(data = csc_data, var = csc_vars[12], adj_dep = "Yes", year_range = 2016:2019)
# 
# # Fix error: working, moved year range within plotting
# plot_csctrend(data = csc_data, var = csc_vars[19], adj_dep = "No", la_select = c("Leeds", "York"))
# plot_csctrend(data = csc_data, var = csc_vars[19], adj_dep = "Yes", la_select = c("Leeds", "York"))
# 
# 
# plot_csctrend(data = csc_data, var = csc_vars[20], adj_dep = "Yes")



# Function for difference from average plot -------------------------------

plot_csctrend_diff <- function(data, var, year_range = NULL, adj_dep = "No", la_select = NULL, include_avg = TRUE) {
  
  require(dplyr)
  require(lazyeval)
  require(rlang)
  
  if (adj_dep == "No") {
    data <- data %>% filter(description == var & !la_name %in% c("City of London", "Isles of Scilly"))
  } else {
    data <- data %>% filter(description == var | description == csc_vars[18] & !la_name %in% c("City of London", "Isles of Scilly"))
  }
  
  # if (include_avg == TRUE & !is.null(la_select)) {
  #   la_select <- c(la_select, "England Average")
  # }
  
  # Check whether year range is null and if so, limit to range in data
  # Check if asked for range is larger than data range, and if so, change
  # to data range
  # year_range <- if (is.null(year_range)) {
  #   
  #   # if no year range given then select min max range
  #   seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)
  #   
  # } else {
  #   
  #   if (length(year_range) > length(seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1))) {
  #     # If year range is given but it's longer than actual available data, select only available data  
  #     seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)
  #     
  #   } else {
  #     # otherwise accept given year range
  #     year_range 
  #     
  #   }
  #   
  # }

  
  
  if (adj_dep == "No") {   # if Deprivation adjust is off
      
    print(year_range)
    
    year_range <- if (is.null(year_range)) {seq(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE), 1)} else {year_range}
    
    print(year_range)
    
      # No LA selected
      data <- data %>% filter(year %in% year_range)
      
      eng_averages <- data %>% 
        add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                la_name = rep("England Average", times = length(unique(.$year))),
                year = sort(unique(.$year)),
                value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value
        ) %>%
        filter(la_name == "England Average") %>%
        select(year, eng_avg = value)

      data %>%
        filter(la_name %in% la_name) %>%
        add_row(new_la_code = rep(NA, times = length(unique(.$year))), 
                la_name = rep("England Average", times = length(unique(.$year))),
                year = sort(unique(.$year)),
                value = data %>% group_by(year) %>% summarise(value = mean(value, na.rm = TRUE)) %>% .$value
        ) %>%
        left_join(., eng_averages, by = "year") %>%
        mutate(mult_diff = value / eng_avg) %>%
        filter(la_name %in% la_select) %>%
        ggplot() + 
        geom_bar(aes(y = mult_diff, x = year, fill = la_name), 
                 stat = "identity", position = "dodge") +
        geom_shadowtext(aes(y = mult_diff - ((mult_diff-1) * 0.5), x = year, label = round(mult_diff, 2), group = la_name), 
                  position = position_dodge(width = 0.9), col = "white", size = 3.5) +
        geom_text(aes(y = mult_diff + ifelse(mult_diff > 1, 0.1, -0.05), 
                      x = year, label = la_name, group = la_name, col = la_name,
                      angle = ifelse(mult_diff > 1, 90, -90)),
                  hjust = 0,
                  position = position_dodge(width = 0.9), size = 3.5, fontface = "bold") +
        scale_colour_manual(values = brewer.pal(8, "Dark2")[2:9]) +
        scale_fill_manual(values = brewer.pal(8, "Dark2")[2:9]) +
        scale_y_continuous(trans = "log10", limits=c(0.25, 4), breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4)) +
        scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                        max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
        ggtitle(str_wrap(paste(var, "relative to English Average"), width = 80)) +
        ylab("Times difference from England LA Average") +
        xlab("Year Ending") +
        theme_minimal() +
        ggeasy::easy_remove_legend() +
        theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))



        # 
        # ggplot() +
        # geom_line(aes(group = la_name, x = year, y = value), alpha = 0.1) +
        # geom_line(data = . %>% filter(la_name %in% "England Average"),
        #           aes(group = la_name, x = year, y = value, col = la_name), size = 1.5) +
        # geom_label(data = . %>% filter(la_name %in% "England Average"), size = 2.5,
        #            aes(group = la_name, x = year, y = value, col = la_name, label = round(value, 2))) +
        # scale_colour_brewer(palette = "Dark2") +
        # scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
        #                                 max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
        # coord_cartesian(xlim = c(min(year_range), max(year_range + 3))) +
        # # Add legend with ggrepel
        # geom_text_repel(data = . %>% filter(year == max(year_range) & la_name %in% "England Average"),
        #                 aes(x = year + 2, y = value, label = la_name, col = la_name),
        #                 segment.alpha = 0, direction = "y") +
        # xlab("Year Ending") +
        # ylab(str_wrap(var, 80)) +
        # ggtitle(str_wrap(var, width = 80)) +
        # theme_minimal() +
        # theme(legend.position = "top") +
        # ggeasy::easy_remove_legend() +
        # theme(panel.grid = element_blank())
      
    } else { # If dep adjusted = Yes 
      
      print(year_range)
      
      year_range <- if (is.null(year_range)) {seq(min(data %>% filter(description == var) %>% .$year, na.rm = TRUE), 
                                                  max(data %>% filter(description == var) %>% .$year, na.rm = TRUE), 
                                                  1)} else {year_range}
      
      print(year_range)
      
      data <- data %>% filter(year %in% year_range)
      
      data <- data %>% 
        filter(description == var | description == csc_vars[18]) %>%
        pivot_wider(values_from = value, names_from = description) %>%
        select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 4, value = 5) %>%
        filter(!is.na(imd) & !is.na(value)) %>%
        group_by(year) %>%
        do(., tidy(lm(value ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
        filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
        select(year, imd_adj_est = estimate) %>%
        right_join(., data %>% filter(description == var | description == csc_vars[18]), by = "year") %>%
        ungroup() %>%
        pivot_wider(values_from = value, names_from = description) %>%
        select(year:la_name, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 5, value = 6) %>%
        mutate(
          imd = imd - mean(imd, na.rm = TRUE),
          imd_adj = imd * imd_adj_est,
          value_adj = value - imd_adj
        ) %>%
        filter(!is.na(value_adj))
      
      print(data)
      
      data <- data %>%
        group_by(year) %>%
        summarise(eng_value_adj = mean(value_adj, na.rm = TRUE)) %>%
        right_join(., data) 
      
      
      data %>%
        filter(la_name %in% la_name) %>%
        mutate(mult_diff = value_adj / eng_value_adj) %>%
        filter(la_name %in% la_select) %>%
        ggplot() + 
        geom_bar(aes(y = mult_diff, x = year, fill = la_name), 
                 stat = "identity", position = "dodge") +
        geom_shadowtext(aes(y = mult_diff - ((mult_diff-1) * 0.5), x = year, label = round(mult_diff, 2), group = la_name), 
                        position = position_dodge(width = 0.9), col = "white", size = 3.5) +
        geom_text(aes(y = mult_diff + ifelse(mult_diff > 1, 0.1, -0.05), 
                      x = year, label = la_name, group = la_name, col = la_name,
                      angle = ifelse(mult_diff > 1, 90, -90)),
                  hjust = 0,
                  position = position_dodge(width = 0.9), size = 3.5, fontface = "bold") +
        scale_colour_manual(values = brewer.pal(8, "Dark2")[2:9]) +
        scale_fill_manual(values = brewer.pal(8, "Dark2")[2:9]) +
        scale_y_continuous(trans = "log10", limits=c(0.25, 4), breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4)) +
        scale_x_continuous(breaks = seq(min(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 
                                        max(data %>% filter(year %in% year_range) %>% .$year, na.rm = TRUE), 1)) +
        ggtitle(str_wrap(paste(var, "Relative to English Average", "(Adjusted for Deprivation)"), width = 80)) +
        ylab("Times difference from England LA Average") +
        xlab("Year Ending") +
        theme_minimal() +
        ggeasy::easy_remove_legend() +
        theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))
      
      
        
      
      } 
}


# 
# plot_csctrend_diff(data = csc_data, var = csc_vars[12], la_select = c("Sheffield", "Kensington and Chelsea", "Derby"), adj_dep = TRUE)
# 
# 
# plot_csctrend_diff(data = csc_data, var = csc_vars[12], la_select = c("Sheffield", "Leeds", "Derby")) +
#   plot_csctrend_diff(data = csc_data, var = csc_vars[12], la_select = c("Sheffield", "Leeds", "Derby"), adj_dep = TRUE)



# To do: maps
# Extended data should only be available without deprivation adjustment (e.g. data dash)

# 
# p1 <- plot_csctrend(data = csc_data, var = csc_vars[50], la_select = c("Blackpool", "Leeds", "York"))
# p2 <- plot_csctrend(data = csc_data, var = csc_vars[50], la_select = c("Blackpool", "Leeds", "York"), adj_dep = "Yes")

patchwork_trendplots <- function(plot1, plot2) {
  
  p_combined <- plot1 + plot2
  
  p_ranges_y <- c(ggplot_build(p_combined[[1]])$layout$panel_scales_y[[1]]$range$range,
                  ggplot_build(p_combined[[2]])$layout$panel_scales_y[[1]]$range$range)
  
  p_combined & 
    ylim(min(p_ranges_y), max(p_ranges_y))
  
}

# patchwork_trendplots(p1, p2)
# 
# 
# p3 <- plot_csctrend_diff(data = csc_data, var = csc_vars[50], la_select = c("Sheffield", "Leeds", "Derby"))
# p4 <- plot_csctrend_diff(data = csc_data, var = csc_vars[50], la_select = c("Sheffield", "Leeds", "Derby"), adj_dep = "Yes")
# 

patchwork_trendplot_diffs <- function(plot1, plot2) {
  
 plot1 + plot2
 
  
  
}

# 
# patchwork_trendplot_diffs(p3, p4)
# 
# plot_csctrend_diff(data = csc_data, var = csc_vars[12], la_select = c("Sheffield", "Leeds", "Derby", "York", "Brent"), adj_dep = "No") /
# plot_csctrend_diff(data = csc_data, var = csc_vars[12], la_select = c("Sheffield", "Leeds", "Derby", "York", "Brent"), adj_dep = "Yes")
# 



# Scatterplot to show relationship with deprivation

plot_csc_points <- function(data, var, year_range = NULL, la_select = NULL) {
  
  if (is.null(year_range)) {
    year_range <- seq(data %>%
      filter(description == var) %>%
        .$year %>% min(.), 
      data %>%
        filter(description == var) %>%
        .$year %>% max(.), 
      1)
  } else {
    year_range
  }
  
  print(year_range)
  
  if (is.null(la_select)) {
    
    
    data %>%
      filter(description == var | description == csc_vars[18]) %>%
      pivot_wider(names_from = description, values_from = value) %>%
      select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
      rename(imd = 4, value = 5) %>%
      filter(scale(value) < 3.5 & scale(value) > -3.5) %>%
      filter(year %in% year_range) %>%
      ggplot() +
      geom_point(aes(x = imd, y = value, col = imd), alpha = 0.9) +
      ggtitle(str_wrap(paste("Relationship between", var, "and Deprivation (Large outliers removed)"), 70)) +
      ylab(str_wrap(var, 50)) +
      xlab("Indices of Multiple Deprivation Score (Higher = More Deprived)") +
      scale_color_gradient(low = "#6495ed", high = "firebrick") +
      facet_wrap(~ year) +
      theme(panel.grid = element_blank(), panel.background = element_blank(), plot.title = element_text(face = "bold")) +
      ggeasy::easy_remove_legend()
    
    
  } else {
  
  
  # Remove outliers - with la_select = not null
   data %>%
    filter(description == var | description == csc_vars[18]) %>%
    pivot_wider(names_from = description, values_from = value) %>%
    select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
    rename(imd = 4, value = 5) %>%
    filter(scale(value) < 3.5 & scale(value) > -3.5) %>%
    filter(year %in% year_range) %>%
    ggplot() +
    geom_point(aes(x = imd, y = value, col = imd), alpha = 0.2) +
    geom_point(data = . %>% filter(la_name %in% la_select), aes(x = imd, y = value, col = imd)) +
    geom_text_repel(data = . %>% filter(la_name %in% la_select), aes(x = imd, y = value, label = la_name), nudge_x = 3, nudge_y = 3, min.segment.length = 0.0001, force = 3, hjust = 0) +
    ggtitle(str_wrap(paste("Relationship between", var, "and Deprivation (Large outliers removed)"), 70)) +
    ylab(str_wrap(var, 50)) +
    xlab("Indices of Multiple Deprivation Score (Higher = More Deprived)") +
    scale_color_gradient(low = "#6495ed", high = "firebrick") +
    facet_wrap(~ year) +
    theme(panel.grid = element_blank(), panel.background = element_blank(), plot.title = element_text(face = "bold")) +
    ggeasy::easy_remove_legend()
  
  }
  
}

# plot_csc_points(csc_data, csc_vars[12])
# plot_csc_points(csc_data, csc_vars[12], la_select = c("Sheffield", "Leeds", "Coventry", "York"))
# plot_csc_points(csc_data, csc_vars[12], year = 2015:2019)
# plot_csc_points(csc_data, csc_vars[12], la_select = c("Sheffield", "Leeds", "Coventry", "York"), year = 2015:2019)


# Maps

# Maps for data dash ------------------------------------------------------

plot_csc_map <- function(geo_data, centroid_data, data, var, year_range = NULL, 
                         adj_dep = "No", la_select = NULL, global_scales = FALSE,
                         make_cart = FALSE) {
  
  if (make_cart == FALSE) {

    print("cart == FALSE")

  } else {

    geo_data <- la_sf_cart
    centroid_data <- la_sf_centroids_cart

  }
  
  raw_data <- data
  
  if (is.null(year_range)) {
    year_range <- 2011:2019
  } else {
    year_range
  }
  
  if (global_scales == FALSE) {
    
    if (adj_dep == "No") {

  scale_lim_min <- data %>%
    filter(description == var & year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
    group_by(new_la_code) %>%
    summarise(la_name = first(la_name), description = first(description),  mean_val = mean(value, na.rm = TRUE)) %>%
    .$mean_val %>%
    min(., na.rm = TRUE)

  scale_lim_max <- data %>%
    filter(description == var & year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
    group_by(new_la_code) %>%
    summarise(la_name = first(la_name), description = first(description),  mean_val = mean(value, na.rm = TRUE)) %>%
    .$mean_val %>%
    max(., na.rm = TRUE)
  
    } else {
      
      # Global scales for adjusted deprivation - Global Scales = FALSE
      
      scale_lim_min <- data %>% 
        filter(description == var | description == csc_vars[18]) %>%
        pivot_wider(values_from = value, names_from = description) %>% 
        select(new_la_code, la_name, year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 4, value = 5) %>%
        print(.) %>%
        group_by(new_la_code) %>%
        summarise(la_name = first(la_name), imd = mean(imd, na.rm = TRUE),  mean_val = mean(value, na.rm = TRUE)) %>%
        filter(!is.na(imd)) %>%
        ungroup() %>%
        do(tidy(lm(mean_val ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
        filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
        select(imd_adj_est = estimate) %>%
        add_column(.data = data %>% filter(description == var | description == csc_vars[18]), 
                   imd_adj = .$imd_adj_est) %>%
        ungroup() %>%
        pivot_wider(values_from = value, names_from = description) %>%
        select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 4, value = 7) %>%
        mutate(
          imd = imd - mean(imd, na.rm = TRUE),
          imd_adj = imd * imd_adj_est,
          mean_val = value - imd_adj
        ) %>%
        filter(!is.na(mean_val)) %>% group_by(new_la_code) %>%
        filter(year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        summarise(
          la_name = first(la_name),
          mean_val = mean(mean_val, na.rm = TRUE)
        ) %>%
        .$mean_val %>%
        min(., na.rm = TRUE)
      
      scale_lim_max <- data %>% 
        filter(description == var | description == csc_vars[18]) %>%
        pivot_wider(values_from = value, names_from = description) %>% 
        select(new_la_code, la_name, year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 4, value = 5) %>%
        print(.) %>%
        group_by(new_la_code) %>%
        summarise(la_name = first(la_name), imd = mean(imd, na.rm = TRUE),  mean_val = mean(value, na.rm = TRUE)) %>%
        filter(!is.na(imd)) %>%
        ungroup() %>%
        do(tidy(lm(mean_val ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
        filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
        select(imd_adj_est = estimate) %>%
        add_column(.data = data %>% filter(description == var | description == csc_vars[18]), 
                   imd_adj = .$imd_adj_est) %>%
        ungroup() %>%
        pivot_wider(values_from = value, names_from = description) %>%
        select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
        rename(imd = 4, value = 7) %>%
        mutate(
          imd = imd - mean(imd, na.rm = TRUE),
          imd_adj = imd * imd_adj_est,
          mean_val = value - imd_adj
        ) %>%
        filter(!is.na(mean_val)) %>% group_by(new_la_code) %>%
        filter(year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        summarise(
          la_name = first(la_name),
          mean_val = mean(mean_val, na.rm = TRUE)
        ) %>%
        .$mean_val %>%
        max(., na.rm = TRUE)
      
      
      
    }

  } else {
    
    if (adj_dep == "No") {

      scale_lim_min <- data %>%
        filter(description == var & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        group_by(new_la_code) %>%
        .$value %>%
        min(., na.rm = TRUE) 
      
      scale_lim_max <- data %>%
        filter(description == var & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        group_by(new_la_code) %>%
        .$value %>%
        max(., na.rm = TRUE) 
    
    } else {
      
      # global scales for adjusted deprivation - Global Scales = TRUE
      # Min max values +- 20%
      
      scale_lim_min <- data %>%
        filter(description == var & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        group_by(new_la_code) %>%
        .$value %>%
        min(., na.rm = TRUE) 
      
      scale_lim_max <- data %>%
        filter(description == var & !la_name %in% c("City of London", "Isles of Scilly")) %>%
        group_by(new_la_code) %>%
        .$value %>%
        max(., na.rm = TRUE)
      
      
      
    }

  }

  
  
  if (adj_dep == "No") {
    
    data <- data %>% 
      filter(description == var & year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
      group_by(new_la_code) %>%
      summarise(la_name = first(la_name), description = first(description),  mean_val = mean(value, na.rm = TRUE)) 
      
    
    
  } else {
    
    data <- data %>% 
      filter(description == var | description == csc_vars[18]) %>%
      pivot_wider(values_from = value, names_from = description) %>% 
      select(new_la_code, la_name, year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
      rename(imd = 4, value = 5) %>%
      print(.) %>%
      group_by(new_la_code) %>%
      summarise(la_name = first(la_name), imd = mean(imd, na.rm = TRUE),  mean_val = mean(value, na.rm = TRUE)) %>%
      filter(!is.na(imd)) %>%
      ungroup() %>%
      do(tidy(lm(mean_val ~ as.numeric(scale(imd, scale = FALSE)), data = .))) %>%
      print(.) %>%
      filter(term == "as.numeric(scale(imd, scale = FALSE))") %>%
      select(imd_adj_est = estimate) %>%
      add_column(.data = data %>% filter(description == var | description == csc_vars[18]), 
                 imd_adj = .$imd_adj_est) %>%
      ungroup() %>%
      pivot_wider(values_from = value, names_from = description) %>%
      select(new_la_code:year, `Indices of Multiple Deprivation Score 2015`, everything()) %>%
      rename(imd = 4, value = 7) %>%
      mutate(
            imd = imd - mean(imd, na.rm = TRUE),
            imd_adj = imd * imd_adj_est,
            mean_val = value - imd_adj
          ) %>%
      filter(!is.na(mean_val)) %>%
      print(.)
    
    data <- data %>% group_by(new_la_code) %>%
      filter(year %in% year_range & !la_name %in% c("City of London", "Isles of Scilly")) %>%
      summarise(
        la_name = first(la_name),
        mean_val = mean(mean_val, na.rm = TRUE)
      )
    
    
  }
  
  

  if (make_cart == FALSE) {
  
  roe_plot <- data %>% 
   left_join(geo_data, ., by = c("ctyua19cd" = "new_la_code")) %>%
    mutate(mean_val = ifelse(Region %in% c("Inner London", "Outer London"), NA, mean_val)) %>%
    ggplot() +
    geom_sf(aes(fill = mean_val), size = 0.1, col = "white") +
    # points south west
    geom_text_repel(data = centroid_data %>% filter(long < mean(long, na.rm = TRUE) & lat < mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select), 
                    aes(x = long, y = lat, label = ctyua19nm), nudge_x = -8, segment.size = 0.2) +
    # points south east
    geom_text_repel(data = centroid_data %>% filter(long > mean(long, na.rm = TRUE) & lat < mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select), 
                    aes(x = long, y = lat, label = ctyua19nm), nudge_x = 8, segment.size = 0.2) +
    # points north
    geom_text_repel(data = centroid_data %>% filter(lat > mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select), 
                    aes(x = long, y = lat, label = ctyua19nm), nudge_x = 8, segment.size = 0.2) +
    ggtitle(str_wrap(ifelse(adj_dep == "No", 
                            ifelse(make_cart == FALSE, var, paste(var, "- Boundaries adjusted for Child Population Size")), 
                            paste("Predicted", var, "(If all LAs had Average Deprivation)", ifelse(make_cart == FALSE, "", "- Boundaries adjusted for Child Population Size"))), 
                     50)) +
    scale_fill_distiller("", palette = "BuGn", type = "seq", limits = c(scale_lim_min, scale_lim_max), direction = 1) +
    theme_minimal() +
    ggeasy::easy_remove_axes() +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = c(.8, .95), legend.direction = "horizontal")  
  
  lon_plot <- data %>% 
    left_join(geo_data, ., by = c("ctyua19cd" = "new_la_code")) %>%
    ggplot() +
    geom_sf(data = . %>% filter(Region %in% c("Inner London", "Outer London")), 
            aes(fill = mean_val), size = 0.1, col = "white") +
    geom_text_repel(data = centroid_data %>% filter(Region %in% c("Inner London", "Outer London"))  %>% filter(ctyua19nm %in% la_select), 
                    aes(x = long, y = lat, label = ctyua19nm), nudge_y = -6, segment.size = 0.2, force = 2) +
    scale_fill_distiller("London Scale", palette = "BuGn", type = "seq", limits = c(scale_lim_min, scale_lim_max), direction = 1) +
    theme_minimal() +
    ggeasy::easy_remove_axes() +
    ggeasy::easy_remove_legend() +
    theme(plot.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))  
  
  ggdraw() +
    draw_plot(lon_plot, x = -0.001, y = 0.55, width = 0.4, height = 0.4) +
    draw_plot(roe_plot)
  
  } else {
    
    data %>% 
      left_join(geo_data, ., by = c("ctyua19cd" = "new_la_code")) %>%
      ggplot() +
      geom_sf(aes(fill = mean_val), size = 0.1, col = "white") +
      # # points south west
      # geom_text_repel(data = la_sf_centroids_cart %>% filter(long < mean(long, na.rm = TRUE) & lat < mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select),
      #                 aes(x = long, y = lat, label = ctyua19nm), nudge_x = -8, segment.size = 0.2) +
      # # points south east
      # geom_text_repel(data = la_sf_centroids_cart %>% filter(long > mean(long, na.rm = TRUE) & lat < mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select),
      #                 aes(x = long, y = lat, label = ctyua19nm), segment.size = 0.2) +
      # # points north
      # geom_text_repel(data = la_sf_centroids_cart %>% filter(lat > mean(lat, na.rm = TRUE))  %>% filter(ctyua19nm %in% la_select),
      #                 aes(x = long, y = lat, label = ctyua19nm), segment.size = 0.2) +
      geom_sf_text(data = la_sf_centroids_cart %>% filter(ctyua19nm %in% la_select),
                   aes(label = ctyua19nm), segment.size = 0.2, nudge_x = 0.1, hjust = 0) +
      geom_sf(data = la_sf_centroids_cart %>% filter(ctyua19nm %in% la_select),
                      size = 0.5) +
      ggtitle(str_wrap(ifelse(adj_dep == "No", 
                              ifelse(make_cart == FALSE, var, paste(var, "- Dorling Cartogram by Child Population")), 
                              paste("Predicted", var, "(If all LAs had Average Deprivation)", ifelse(make_cart == FALSE, "", "- Dorling Cartogram by Child Population"))), 
                       50)) +
      scale_fill_distiller("", palette = "BuGn", type = "seq", limits = c(scale_lim_min, scale_lim_max), direction = 1) +
      theme_minimal() +
      ggeasy::easy_remove_axes() +
      theme(plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = c(.8, .95), legend.direction = "horizontal")  
    
    
  }
  
  
}


# testing
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[7], adj_dep = TRUE, global_scales = TRUE)
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[7], global_scales = TRUE)
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[7], make_cart = TRUE)
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12], adj_dep = "No", global_scales = TRUE) +
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[18], global_scales = TRUE) +
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12], adj_dep = TRUE, global_scales = TRUE) 
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12], adj_dep = "No", global_scales = TRUE, la_select = c("Kirklees", "Sheffield", "Coventry", "Plymouth", "Warwickshire", "Camden", "Brent", "Birmingham", "Knowsley"))  +
#   plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[18], global_scales = TRUE, la_select = c("Kirklees", "Sheffield", "Coventry", "Plymouth", "Warwickshire", "Camden", "Brent", "Birmingham", "Knowsley"))  +
#   plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12], adj_dep = TRUE, global_scales = TRUE, la_select = c("Kirklees", "Sheffield", "Coventry", "Plymouth", "Warwickshire", "Camden", "Brent", "Birmingham", "Knowsley")) &
#   plot_annotation(caption = "Data: DfE, DCLG. Data Viz by Calum Webb @cjrwebb")
# 
# (plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[12], adj_dep = "No", global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))  +
#   plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[18], global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))  +
#   plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[12], adj_dep = TRUE, global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))) /
# (plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[12], adj_dep = "No", global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))  +
#   plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[18], global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))  +
#   plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[12], adj_dep = TRUE, global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))) &
#   plot_annotation(caption = "Data: DfE, DCLG. Data Viz by Calum Webb @cjrwebb")
# 
# 
# (plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[7], year_range = 2011:2013, adj_dep = "No", global_scales = TRUE, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))  +
#     plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[18], global_scales = TRUE, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))  +
#     plot_csc_map(la_sf, la_sf_centroids, make_cart = FALSE, csc_data, csc_vars[7], year_range = 2016:2019, adj_dep = "No", global_scales = TRUE, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))) /
#   (plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[7], year_range = 2011:2013, adj_dep = "No", global_scales = TRUE, la_select = c("Birmingham", "Liverpool"))  +
#      plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[18], global_scales = TRUE, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))  +
#      plot_csc_map(la_sf, la_sf_centroids, make_cart = TRUE, csc_data, csc_vars[7], year_range = 2016:2019, adj_dep = "No", global_scales = TRUE, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))) &
#   plot_annotation(caption = "Data: DfE, DCLG. Data Viz by Calum Webb @cjrwebb")
# 
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12], adj_dep = "No", global_scales = TRUE, la_select = c("Sheffield"))
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], adj_dep = "No", global_scales = TRUE, la_select = c("Torbay", "Sheffield", "Devon", "Kent", "Camden"), year_range = 2015)
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], adj_dep = "Yes", global_scales = TRUE, la_select = c("Torbay", "Sheffield", "Devon", "Kent", "Camden"), year_range = 2019)
# 
# 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[12])
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[7], year_range = 2011) 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[7], year_range = 2019) 
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], year_range = 2018:2019)
# plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[27], year_range = 2018:2019)
# 
# (plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19]) + plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], adj_dep = "Yes")  ) /
#   (plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], make_cart = TRUE) + plot_csc_map(la_sf, la_sf_centroids, csc_data, csc_vars[19], make_cart = TRUE, adj_dep = "Yes") )  
# 
# 
# csc_vars

# Map patchwork for comparing two years with same scale

patchwork_maps <- function(data, var, la_select = NULL, year_range1, year_range2, adj_dep = "No", make_cart = FALSE) {
  
  plot1 <- plot_csc_map(la_sf, la_sf_centroids, csc_data, var, year_range = year_range1, global_scales = TRUE, la_select = la_select, make_cart = make_cart, adj_dep = adj_dep) 
  plot2 <- plot_csc_map(la_sf, la_sf_centroids, csc_data, var, year_range = year_range2, global_scales = TRUE, la_select = la_select, make_cart = make_cart, adj_dep = adj_dep) 
  
  plot1 + plot2 + 
    plot_annotation(title = paste("Comparison between years ending", min(year_range1), " -", max(year_range1), "(left) and years ending", min(year_range2), "-", max(year_range2), "(right)")) &
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  
  
}

# patchwork_maps(csc_data, csc_vars[7], 2011:2013, 2017:2019,  la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))
# patchwork_maps(csc_data, csc_vars[15], 2011:2013, 2017:2019, la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"))

# 
# (patchwork_maps(csc_data, csc_vars[7], 2011:2013, 2017:2019,  
#                la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"),
#                make_cart = FALSE)) /
# (patchwork_maps(csc_data, csc_vars[7], 2011:2013, 2017:2019,  
#                la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"),
#                make_cart = TRUE))
# 
# patchwork_maps(csc_data, csc_vars[12], 2011:2013, 2017:2019, adj_dep =  "Yes",
#                la_select = c("Redcar and Cleveland", "Camden", "North East Lincolnshire", "Bournemouth, Christchurch and Poole"),
#                make_cart = TRUE)

# Waffle plots spending ---------------------------------------------------
# Quite slow but works


spending_waffles <- function(data, year_range = NULL, la_select = NULL) {
  
  exp_cla_pc_data <- data %>% 
    filter(description %in% c(csc_vars[2], csc_vars[17])) %>%
    pivot_wider(names_from = description, values_from = value) %>%
    rename(exp_cla = 4, pop = 5) %>%
    mutate(exp_cla_pc = (exp_cla * 100000) / pop) %>%
    pivot_longer(cols = exp_cla:exp_cla_pc, names_to = "description") %>%
    filter(description == "exp_cla_pc")
  
  print(exp_cla_pc_data)
  
  if (is.null(la_select)) {
  
    waff_plot <- data %>% 
      add_row(exp_cla_pc_data) %>%
      filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
      mutate(description = case_when(description == "exp_cla_pc" ~ "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                     description == csc_vars[7] ~ "£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                     description == csc_vars[6] ~ "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
      group_by(year, description) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      mutate(value = value/10) %>%
      ggplot() +
      geom_pictogram(aes(label = description, col = description, values = value), 
                     flip = TRUE, make_proportional = FALSE, size = 4,
                     n_rows = 5) +
      scale_color_manual(
        name = NULL,
        values = c("#1B9E77", "#D95F02", "#7570B3"),
        labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                   "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                   "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
      ) +
      scale_label_pictogram(
        name = NULL,
        values = c("money-bill-wave", "money-bill-wave", "money-bill-wave"),
        labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                   "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                   "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
      ) +
      # max is 3650 which equals 10 * 5 * 71
      # ylim(c(0, 71)) +
      facet_grid(~year) +
      coord_equal() +
      labs(title = "Spending per Child on Different Services (England LA Average)") +
      theme_minimal() +
      theme(plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.text = element_text(hjust = 0, vjust = 1)) +
      ggeasy::easy_text_size(size = 12) +
      ggeasy::easy_remove_legend_title() +
      ggeasy::easy_remove_axes() +
      theme(legend.position = "top") +
      guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
    
    
      waff_plot <- set_panel_size(waff_plot, width = unit(1.5, "in"), height = unit(3.5, "in"))
  
      grid.arrange(waff_plot)
    
  } else {
  
    if (length(la_select) == 1) {
      
      waff_plot <- data %>% 
        add_row(exp_cla_pc_data) %>%
        filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
        mutate(description = case_when(description == "exp_cla_pc" ~ "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[7] ~ "£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[6] ~ "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
        filter(la_name %in% la_select) %>%
        mutate(value = value/10) %>%
        ggplot() +
        geom_pictogram(aes(label = description, col = description, values = value), 
                       flip = TRUE, make_proportional = FALSE, size = 4,
                       n_rows = 5) +
        scale_color_manual(
          name = NULL,
          values = c("#1B9E77", "#D95F02", "#7570B3"),
          labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        scale_label_pictogram(
          name = NULL,
          values = c("money-bill-wave", "money-bill-wave", "money-bill-wave"),
          labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        facet_grid(~year) +
        coord_equal() +
        labs(title = paste0("Spending per Child on Different Services ", "(", la_select, ")")) +
        theme_minimal() +
        theme(plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.text = element_text(hjust = 0, vjust = 1)) +
        ggeasy::easy_text_size(size = 12) +
        ggeasy::easy_remove_legend_title() +
        ggeasy::easy_remove_axes() +
        theme(legend.position = "top") +
        guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
      
      
      waff_plot <- set_panel_size(waff_plot, width = unit(1.5, "in"), height = unit(5.5, "in"))
      
      grid.arrange(waff_plot)
      
    } else {
      
     data %>% 
        add_row(exp_cla_pc_data) %>%
        filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
        mutate(description = case_when(description == "exp_cla_pc" ~ "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[7] ~ "£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[6] ~ "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
        filter(la_name %in% la_select) %>%
        mutate(value = value/10) %>%
        ggplot() +
        geom_pictogram(aes(label = description, col = description, values = value), 
                       flip = TRUE, make_proportional = FALSE, size = 6/length(la_select),
                       n_rows = 5) +
        scale_color_manual(
          name = NULL,
          values = c("#1B9E77", "#D95F02", "#7570B3"),
          labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        scale_label_pictogram(
          name = NULL,
          values = c("money-bill-wave", "money-bill-wave", "money-bill-wave"),
          labels = c("£10 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£10 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        facet_grid(la_name ~ year) +
        coord_equal() +
        labs(title = "Spending per Child on Different Services ") +
        theme_minimal() +
        theme(plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.text = element_text(hjust = 0, vjust = 1)) +
        ggeasy::easy_text_size(size = 12) +
        ggeasy::easy_remove_legend_title() +
        ggeasy::easy_remove_axes() +
        theme(legend.position = "top") +
        guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
      
      
      
    }
    
    
}
  
}

spending_waffles20s <- function(data, year_range = NULL, la_select = NULL) {
  
  exp_cla_pc_data <- data %>% 
    filter(description %in% c(csc_vars[2], csc_vars[17])) %>%
    pivot_wider(names_from = description, values_from = value) %>%
    rename(exp_cla = 4, pop = 5) %>%
    mutate(exp_cla_pc = (exp_cla * 100000) / pop) %>%
    pivot_longer(cols = exp_cla:exp_cla_pc, names_to = "description") %>%
    filter(description == "exp_cla_pc")
  
  print(exp_cla_pc_data)
  
  if (is.null(la_select)) {
    
    data %>% 
      add_row(exp_cla_pc_data) %>%
      filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
      mutate(description = case_when(description == "exp_cla_pc" ~ "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                     description == csc_vars[7] ~ "£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                     description == csc_vars[6] ~ "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
      group_by(year, description) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      mutate(value = value/20) %>%
      ggplot() +
      geom_pictogram(aes(label = description, col = description, values = value), 
                     flip = TRUE, make_proportional = FALSE, size = 6,
                     n_rows = 5) +
      scale_color_manual(
        name = NULL,
        values = c("#1B9E77", "#D95F02", "#7570B3"),
        labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                   "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                   "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
      ) +
      scale_label_pictogram(
        name = NULL,
        values = c("money-bill", "money-bill", "money-bill"),
        labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                   "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                   "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
      ) +
      # max is 3650 which equals 10 * 5 * 71
      # ylim(c(0, 71)) +
      facet_grid(~year) +
     # ylim(c(0, 37)) +
      coord_equal() +
      labs(title = "Spending per Child on Different Services (England LA Average)") +
      theme_minimal() +
      theme(plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.text = element_text(hjust = 0, vjust = 1)) +
      ggeasy::easy_text_size(size = 12) +
      ggeasy::easy_remove_legend_title() +
      ggeasy::easy_remove_axes() +
      theme(legend.position = "top") +
      guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
    
    
    # waff_plot <- set_panel_size(waff_plot, width = unit(1.5, "in"), height = unit(3.5, "in"))
    # 
    # grid.arrange(waff_plot)
    
  } else {
    
    if (length(la_select) == 1) {
      
     data %>% 
        add_row(exp_cla_pc_data) %>%
        filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
        mutate(description = case_when(description == "exp_cla_pc" ~ "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[7] ~ "£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[6] ~ "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
        filter(la_name %in% la_select) %>%
        mutate(value = value/20) %>%
        ggplot() +
        geom_pictogram(aes(label = description, col = description, values = value), 
                       flip = TRUE, make_proportional = FALSE, size = 6,
                       n_rows = 5) +
        scale_color_manual(
          name = NULL,
          values = c("#1B9E77", "#D95F02", "#7570B3"),
          labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        scale_label_pictogram(
          name = NULL,
          values = c("money-bill", "money-bill", "money-bill"),
          labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        facet_grid(~year) +
       # ylim(c(0, 37)) +
        coord_equal() +
        labs(title = paste0("Spending per Child on Different Services ", "(", la_select, ")")) +
        theme_minimal() +
        theme(plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.text = element_text(hjust = 0, vjust = 1)) +
        ggeasy::easy_text_size(size = 12) +
        ggeasy::easy_remove_legend_title() +
        ggeasy::easy_remove_axes() +
        theme(legend.position = "top") +
        guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
      
      # 
      # waff_plot <- set_panel_size(waff_plot, width = unit(1.5, "in"), height = unit(5.5, "in"))
      # 
      # grid.arrange(waff_plot)
      
    } else {
      
      data %>% 
        add_row(exp_cla_pc_data) %>%
        filter(description %in% c("exp_cla_pc", csc_vars[6], csc_vars[7])) %>%
        mutate(description = case_when(description == "exp_cla_pc" ~ "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[7] ~ "£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                                       description == csc_vars[6] ~ "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")) %>%
        filter(la_name %in% la_select) %>%
        mutate(value = value/20) %>%
        ggplot() +
        geom_pictogram(aes(label = description, col = description, values = value), 
                       flip = TRUE, make_proportional = FALSE, size = 5/length(la_select),
                       n_rows = 5) +
        scale_color_manual(
          name = NULL,
          values = c("#1B9E77", "#D95F02", "#7570B3"),
          labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        scale_label_pictogram(
          name = NULL,
          values = c("money-bill-wave", "money-bill-wave", "money-bill-wave"),
          labels = c("£20 Spent on Early Help/Family Support (non-SG, non-CLA) per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Looked After per Child Aged 0-17 (in 2019 prices)",
                     "£20 Spent on Safeguarding per Child Aged 0-17 (in 2019 prices)")
        ) +
        facet_grid(la_name ~ year) +
        coord_equal() +
        labs(title = "Spending per Child on Different Services ") +
        theme_minimal() +
        theme(plot.background = element_blank(),
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.text = element_text(hjust = 0, vjust = 1)) +
        ggeasy::easy_text_size(size = 12) +
        ggeasy::easy_remove_legend_title() +
        ggeasy::easy_remove_axes() +
        theme(legend.position = "top") +
        guides(col = guide_legend(ncol=1, nrow=3, byrow=TRUE)) 
      
      
      
    }
    
    
  }
  
}

# # not recommended to compare more than 2 at a time
# spending_waffles(csc_data, la_select = c("Birmingham", "Bracknell Forest")) 
# spending_waffles20s(csc_data, la_select = c("Sheffield", "Camden")) 
# spending_waffles20s(csc_data, la_select = c("Camden")) 
# 
# patchwork_trendplots(plot_csctrend(csc_data, csc_vars[12], la_select = c("Camden")),
#   plot_csctrend(csc_data, csc_vars[12], la_select = c("Camden"), adj_dep = TRUE))
# 
# 
# # Good example of IIL
# plot_csctrend(csc_data, csc_vars[93], la_select = c("Birmingham", "Bracknell Forest")) +
#   plot_csctrend(csc_data, csc_vars[93], la_select = c("Birmingham", "Bracknell Forest"), adj_dep = TRUE)
# 
# spending_waffles(csc_data)
# spending_waffles(csc_data, la_select = "Leeds")
# spending_waffles(csc_data, la_select = "Kensington and Chelsea")
# spending_waffles(csc_data, la_select = "York")
# spending_waffles(csc_data, la_select = "Birmingham")
# spending_waffles(csc_data, la_select = "Blackpool")
# spending_waffles(csc_data, la_select = "Barnsley")
# spending_waffles(csc_data, la_select = "Slough")
# spending_waffles(csc_data, la_select = "Warwickshire")
# 
# spending_waffles(csc_data, la_select = c("Blackpool", "Warwickshire")) 



# Hierarchical Cluster Analysis Diagram -----------------------------------

# HCA 

plot_csc_hca <- function(data, var_list, la_select = NULL, year_range = 2011:2019, trend_hca = FALSE) {
  
  if (trend_hca == FALSE) {
  
  name_row_lookup <- data %>% 
    filter(description %in% var_list & year %in% year_range & !la_name %in% c("City of London", "Isles Of Scilly")) %>%
    pivot_wider(names_from = description, values_from = value) %>%
    clean_names() %>%
    group_by(la_name) %>%
    mutate_at(vars(4:length(.)), ~mean(., na.rm = TRUE)) %>%
    summarise_all(first) %>%
    select(-year) %>%
    ungroup() %>%
    mutate(row_num = row_number()) %>%
    select(la_name, row_num)
  
  
  hclust_data <- data %>% 
    filter(description %in% var_list & year %in% year_range & !la_name %in% c("City of London", "Isles Of Scilly")) %>%
    pivot_wider(names_from = description, values_from = value) %>%
    clean_names() %>%
    group_by(la_name) %>%
    mutate_at(vars(4:length(.)), ~mean(., na.rm = TRUE)) %>%
    summarise_all(first) %>%
    select(-year) %>%
    ungroup() %>%
    select(-new_la_code, -la_name) %>%
    as.data.frame(hclust_data)
  
  } else { # if trend_hca = TRUE
    
    
    name_row_lookup <- data %>% 
      filter(description %in% var_list & year %in% year_range & !la_name %in% c("City of London", "Isles Of Scilly")) %>%
      pivot_wider(names_from = description, values_from = value) %>%
      clean_names() %>%
      group_by(la_name) %>%
      drop_na() %>%
      mutate_at(vars(4:length(.)), ~cor(year, ., use = "complete.obs") * (sd(., na.rm = TRUE)) / (sd(year, na.rm = TRUE)) ) %>%
      summarise_all(first) %>%
      select(-year) %>%
      ungroup() %>%
      mutate(row_num = row_number()) %>%
      select(la_name, row_num)
    
    
    hclust_data <- data %>% 
      filter(description %in% var_list & year %in% year_range & !la_name %in% c("City of London", "Isles Of Scilly")) %>%
      pivot_wider(names_from = description, values_from = value) %>%
      clean_names() %>%
      group_by(la_name) %>%
      drop_na() %>%
      mutate_at(vars(4:length(.)), ~cor(year, ., use = "complete.obs") * (sd(., na.rm = TRUE)) / (sd(year, na.rm = TRUE)) ) %>%
      summarise_all(first) %>%
      select(-year) %>%
      ungroup() %>%
      select(-new_la_code, -la_name) %>%
      as.data.frame(hclust_data)
    
    
    # print(arrange(hclust_data %>% mutate(id = row_number()), early_help_family_support_non_sg_non_cla_expenditure_per_child_aged_0_17_per_child_in_2019_prices))
    # print(as.data.frame(name_row_lookup))
    
    
  } 
  
  
  
  
  rownames(hclust_data) <- name_row_lookup$la_name
  
  hclust_data <- drop_na(hclust_data)
  
  
  hclust_results <- hclust(dist(hclust_data), method = "complete")
  csc_dendro <- ggdendro::dendro_data(hclust_results)
  csc_dendro_segments <- segment(csc_dendro)

  
  number_of_bar <- nrow(csc_dendro$labels)
  angle <-  90 - 360 * (as.numeric(rownames(csc_dendro$labels)) - 0.5) /number_of_bar
  csc_dendro$labels$hjust <- ifelse( angle < -90, 1, 0)
  csc_dendro$labels$angle <- ifelse(angle < -90, angle+180, angle)
  
  csc_dendro$labels <- csc_dendro$labels %>% 
    add_row(label = "", x = max(csc_dendro$labels$x) + 1, y = 0, hjust = 0, angle = 90)
  
  ggplot(csc_dendro_segments) +
      geom_segment(aes(x = x, y = log(y+1), xend = xend, yend = log(yend+1)),
                   alpha = 0.6, size = 0.5, col = "#00947c") +
      # geom_point(data = as_tibble(vil_ggdendro_segments %>% filter(yend == 0)),
      #            aes(x = x, y = log(yend+1))
      #            ) +
    geom_point(data = csc_dendro$labels %>% filter(label %in% la_select),
              aes(x = x, y= log(y+1)),
              inherit.aes = FALSE, size = 1.5, col = "#00947c") +
    geom_text(data = csc_dendro$labels,
              aes(x = x, y= log(y+1), label = paste("    ", label, "     "), hjust = hjust),
              inherit.aes = FALSE, size = 3,
              angle = csc_dendro$labels$angle) +
    labs(caption = str_wrap(paste("Local Authority clustered by similarity using HCA by", paste(var_list, collapse = ", "), ". Length of line connecting two local authorities is equal to their similarity across these dimensions, where shorter equals more similar. Dots equal LAs selected by user.", 
                                  paste("Years ending:", year_range[1]), "-", paste(year_range[length(year_range)]), ifelse(trend_hca == FALSE, "", "Clustered by linear trend over year range.")), 120)) +
    coord_polar(start = 0) +
    scale_y_reverse(
                    expand = c(1, -1)
                    ) +
    scale_size(range = c(0.1, 1.5)) +
      ylab("") +
      xlab("") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # plot.margin = unit(c(-1.75, -1.75, -1.75, -2.25),"cm")
      ) 
  
  
  
  
}


# plot_csc_hca(csc_data, c(csc_vars[7]), la_select = c("Rutland", "Surrey", "Tower Hamlets", "Southwark"), year_range = 2011:2019, trend_hca = TRUE)
# plot_csctrend(csc_data, c(csc_vars[7]), la_select = c("Rutland", "Surrey", "Tower Hamlets", "Southwark"), year_range = 2011:2019, include_avg = FALSE)
# plot_csctrend(csc_data, c(csc_vars[12]), la_select = c("Rutland", "Surrey", "Tower Hamlets", "Southwark"), year_range = 2011:2019, include_avg = FALSE)
# 
# plot_csc_hca(csc_data, c(csc_vars[7], csc_vars[12]), la_select = c("Camden", "Bristol City of", "Luton"), year_range = 2011:2019, trend_hca = TRUE)
# plot_csctrend(csc_data, c(csc_vars[7]), la_select = c("Camden", "Bristol City of", "Luton"), year_range = 2011:2019, include_avg = FALSE)
# plot_csctrend(csc_data, c(csc_vars[12]), la_select = c("Camden", "Bristol City of", "Luton"), year_range = 2011:2019, include_avg = FALSE)
# 
# 
# 
# plot_csc_hca(csc_data, c(csc_vars[18], csc_vars[17], csc_vars[15], csc_vars[5]), la_select = c("Sheffield", "York", "Leeds", "Devon", "Torbay"), year_range = 2018:2019)
# plot_csc_hca(csc_data, c(csc_vars[7]), la_select = "Camden", year_range = 2018:2019)
# plot_csc_hca(csc_data, c(csc_vars[7]), la_select = "Camden", year_range = 2011:2012)
# plot_csc_hca(csc_data, c(csc_vars[12]))
# plot_csc_hca(csc_data, c(csc_vars[5]))
# 
# plot_csc_hca(csc_data, c(csc_vars[7]), la_select = "Camden", year_range = 2011:2012) 
# plot_csc_hca(csc_data, c(csc_vars[7]), la_select = "Camden", year_range = 2018:2019)



# Ethnic inequalities (CLA only) -----------------------------------------

plot_ethnic_inequalities <- function(data, var, la_select = NULL, relative = FALSE) {
  
  shift_trans = function(d = 0) {
    scales::trans_new("shift", transform = function(x) x - d, inverse = function(x) x + d)
  }
  
  data <- data %>% 
    filter(description %in% var) %>%
    filter(!is.na(value)) %>%
    add_row(LA = "England LA Average", New_geog_code = NA, description = var, value = mean(.$value, na.rm = TRUE)) 
  
  if(str_detect(var, "ratio")) {

    data %>%
      ggplot() +
      geom_bar(aes(y = value, x = reorder(as.factor(LA), -value), fill = value), stat = "identity") +
      geom_text(aes(y = value, x = reorder(as.factor(LA), -value), 
                    label = ifelse(value > 0, paste(" ", LA, round(value, 2)), paste(LA, round(value, 2), "   ")), hjust = ifelse(value-1 > 0, 0, 1), 
                    fontface = ifelse(LA %in% c("England LA Average", la_select), "bold", "plain")), 
                stat = "identity", vjust = 0.4, size = 4) +
      scale_fill_distiller("", palette = "BuGn", type = "seq", direction = 1) +
      theme_minimal() +
      easy_remove_y_axis() +
      easy_remove_legend() +
      ylab("") +
      ggtitle(str_wrap(var, 80)) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      #scale_y_continuous(trans = shift_trans(1), limits = c(-1* (1.5*max(data$value)), 1.5*max(data$value))) +
      scale_y_continuous(trans = "log10", limits = c(0.05, 2.5*max(data$value))) +
      coord_flip() &
      plot_annotation(caption = str_wrap("Based on estimated 0-17 Ethnic Group Population Size and Children in Need Statistics. Missing and zero data usually indicates censored rates (due to small numbers of interventions).", 80))

  } else {
  
  data %>%
    ggplot() +
    geom_bar(aes(y = value, x = reorder(as.factor(LA), -value), fill = value), stat = "identity") +
    geom_text(aes(y = value, x = reorder(as.factor(LA), -value), label = paste(" ", LA, round(value, 1)), fontface = ifelse(LA %in% c("England LA Average", la_select), "bold", "plain")), stat = "identity", hjust = 0, vjust = 0.4, size = 4) +
    scale_fill_distiller("", palette = "BuGn", type = "seq", direction = 1) +
    ylim(c(0, 1.2*max(data$value))) +
    theme_minimal() +
    easy_remove_y_axis() +
    easy_remove_legend() +
    ylab("") +
    ggtitle(str_wrap(var, 80)) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    coord_flip() &
    plot_annotation(caption = str_wrap("Based on estimated 0-17 Ethnic Group Population Size and Children in Need Statistics. Missing and zero data usually indicates censored rates (due to small numbers of interventions).", 80))
  
  }    
    
}



# plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[7], la_select = "Liverpool")
#   
# plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[9], la_select = "Liverpool")
# 
# 
# plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[1])
# 
# plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[6]) +
#   plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[7]) +
#   plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[8]) +
#   plot_layout(ncol = 3, nrow = 1) 
#   
# plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[2]) +
#   plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[3]) +
#   plot_ethnic_inequalities(data = csc_ethnicity_data, var = csc_ethnicity_vars[4]) +
#   plot_layout(ncol = 3, nrow = 1) 


