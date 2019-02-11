
function(input, output, session) {

  
  
  
  ## US Finfish Aquaculture Baseline Metrics ##
  callModule(summary_stats, "fish_metrics",
             number_boxes = 3,
             statistic = list("51%", "$203 M", "35%"),
             text = list("of sales in USD are from catfish, followed by trout at 15%, and bass at 7%.",
                         "in finfish aquaculture products were sold in Mississippi, the highest food fish producing state.",
                         "of food fish farms were located in Mississippi, Alabama, and North Carolina."))
  
  ## Finfish Aquaculture US Map ##
  callModule(card_choro_map, "fish_us_map",
             data = fish_us_map,
             field = "input",
             filter_field = type, # type of data to plot
             popup_value = "map_data",
             popup_units = "units",
             color_palette = ygb,
             color_palette_type = "quantile",
             legend_title = "Legend",
             popup_label = "state")
  

  
  ## US Mollusk Aquaculture Baseline Metrics ##
  callModule(summary_stats, "shell_metrics",
             number_boxes = 3,
             statistic = list("55%", "45%", "52%"),
             text = list("of US mollusk sales are oysters, 38% are clams and 4% are mussels.",
                         "of mollusk aquaculture sales were produced in Washington during 2013.",
                         "of mollusk farms are in Florida, Massachusetts, and Washington."))
  
  
  
  ## Mollusk Aquaculture US Map ##
  callModule(card_choro_map, "shell_us_map",
             data = shell_us_map,
             field = "input",
             filter_field = type, # type of data to plot
             popup_value = "map_data",
             popup_units = "units",
             color_palette = ygb,
             color_palette_type = "quantile",
             legend_title = "Legend",
             popup_label = "state")
  
  
  
  ## FDA Shrimp Import Refusal Counts ##
  callModule(card_dot_map, "shrimp_dot_map",
             data = shrimp_refuse_dot,
             field = "input",
             filter_field = YEAR, # slider data filter
             col = OrRd[150],
             lon_field = "LON",
             lat_field = "LAT",
             popup_label = "COUNTRY_NAME",
             popup_value = "REFUSAL_NUM",
             popup_units = "Refusals",
             label_font_size = "12px",
             chart_width = "REFUSAL_NUM",
             lon = 12,
             lat = 30,
             zoom = 2)
  
  ## FDA Shrimp Import Refusal Charges ##
  callModule(card_plot, "shrimp_stacked_plot",
             shrimp_stacked,
             x = "YEAR",
             y = "REFUSAL_COUNT",
             color_group = "DESCRIPTION",
             filter_field = "COUNTRY_NAME",
             colors = ygb_cols, 
             mode = NULL,
             plot_type = "bar",
             barmode = "stack",
             tooltip_text = ~paste("Charge: ", DESCRIPTION,
                                  "<br>Refused:", REFUSAL_COUNT, "Imports", sep=" "),
             xaxis_label = "Year",
             yaxis_label = "Refusal Charge Count",
             x_tickangle = 45,
             x_dtick = 1)


  
  
  
  


  }
