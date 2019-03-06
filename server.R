
function(input, output, session) {

  
  
  
  ## Finfish Aquaculture Summary Stats ##
  callModule(summary_stats, "fish_metrics",
             number_boxes = 3,
             statistic = list("51%", "$203 M", "35%"),
             text = list("of sales in USD are from catfish, followed by trout at 15%, and bass at 7%.",
                         "in finfish aquaculture products were sold in Mississippi, the highest food fish producing state.",
                         "of food fish farms were located in Mississippi, Alabama, and North Carolina."))
  
  
  ## Food Fish Aquaculture Map ##
  callModule(card_choro_map, "fish_us_map",
             data = fish_us_map,
             field = "input",
             filter_field = type, # type of data to plot
             popup_value = "map_data",
             popup_units = "units",
             color_palette = ygb[80:200],
             color_palette_type = "quantile",
             legend_title = "Legend",
             popup_label = "state")
  
  
  
  ## Food Fish Sales/Operation Timeseries ##
  callModule(card_plot, "fish_dolop_plot",
             fish_dolop_plot,
             x = "OPERATIONS",
             y = "DOLLARS",
             plot_type = "scatter", 
             filter_field = "Year", # filter for slider input
             mode = "text",
             tooltip_text = ~State,
             textposition = 'middle right', # "middle right", "top center"
             xaxis_label = "No. of Farms",
             yaxis_label = "Sales in US Dollars",
             xaxis_range = c(0,220),
             x_dtick = 50, # distance between ticks
             x_tickangle = 360, # normal angle
             yaxis_range = c(0,120000000)) 
  # Problem is that Mississippi is a huge outlier
  
  
  
  
  
  
  ## Mollusk Aquaculture Summary Stats ##
  callModule(summary_stats, "shell_metrics",
             number_boxes = 3,
             statistic = list("55%", "45%", "52%"),
             text = list("of US mollusk sales are oysters, 38% are clams and 4% are mussels.",
                         "of mollusk aquaculture sales were produced in Washington during 2013.",
                         "of mollusk farms are in Florida, Massachusetts, and Washington."))
  
  
  ## Mollusk Aquaculture Map ##
  callModule(card_choro_map, "shell_us_map",
             data = shell_us_map,
             field = "input",
             filter_field = type, # type of data to plot
             popup_value = "map_data",
             popup_units = "units",
             color_palette = ygb[80:200],
             color_palette_type = "quantile",
             legend_title = "Legend",
             popup_label = "state")
  
  
  ## Mollusk Sales/Operation Timeseries ##
  callModule(card_plot, "moll_dolop_plot",
             fish_dolop_plot,
             x = "OPERATIONS",
             y = "DOLLARS",
             plot_type = "scatter", 
             filter_field = "Year", # filter for slider input
             mode = "text",
             tooltip_text = ~State,
             textposition = 'middle right', # "middle right", "top center"
             xaxis_label = "No. of Farms",
             yaxis_label = "Sales in US Dollars",
             xaxis_range = c(0,225),
             x_dtick = 50, # distance between ticks
             x_tickangle = 360, # normal angle
             yaxis_range = c(0,150000000)) 
  
  
  
  
  ## FDA Shrimp Import Refusal Summary Stats ##
  callModule(summary_stats, "shrimp_metrics",
             number_boxes = 3,
             statistic = list("40%", "15%", "36%"),
             text = list("of shrimp import refusals over the last 10 years came from India and Malaysia equally, the top two offending countries.",
                         "of shrimp import refusals over the last 10 years came from Vietnam, followed by Indonesia at 10%.",
                         "of refusals in recent years were due to presence of Salmonella. Traces of veterinary drugs came in second at 19%."))
  
  
  ## FDA Shrimp Import Refusal Counts Map ##
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
  
  ## FDA Shrimp Import Refusal Charges Stacked Plot ##
  callModule(card_plot, "shrimp_stacked_plot",
             shrimp_stacked,
             x = "YEAR",
             y = "REFUSAL_COUNT",
             xaxis_range = c(2001.5,2018.5),
             yaxis_range = c(0,max(shrimp_stacked$REFUSAL_COUNT)),
             color_group = "DESCRIPTION",
             filter_field = "COUNTRY_NAME",
             colors = ygb_cols,
             mode = NULL,
             plot_type = "bar",
             barmode = "stack",
             tooltip_text = ~paste("Charge: ", map(DESCRIPTION, capStr),
                                  "<br>Refused:", REFUSAL_COUNT, "Imports", sep=" "),
             xaxis_label = "Year",
             yaxis_label = "Refusal Charge Count",
             x_tickangle = 45,
             x_dtick = 1)
  
  


  }
