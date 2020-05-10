# ui

# source all global variables and functions
source("global.R")


## Dashboard Page ##
# Here, dashboardPage is a modified function - see functions

dashboardPage("US Aquaculture", 
              thead = tags$head(tags$link(
                rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css"), # Place mbie_header inside a container-fluid for correct positioning
                div(class = "container-fluid", dash_header())),
              
              
              
              ## About Page ##
              
              tabPanel(title = "About",
                       frontp() # content is in front_page.R
              ), # end tab panel
              
              
              
              ## US Production ##
              
              navbarMenu("Production",
                         
                         tabPanel("Food Fish",
                                  
                                  div(class = "master",
                                      
                                      tab_title(title = "US Food Fish Production",
                                                lead = "\"The first aquaculture census was conducted in 1998, in response to the intense need for an accurate measure of the aquaculture sector. The aquaculture Census collects detailed information relating to production volume and methods, surface water acres and sources, sales, point of first sale outlets, and aquaculture distributed for restoration, conservation, enhancement, or recreational purposes.\" - USDA Census of Aquaculture",
                                                subtitle = "About the Data:",
                                                description = list("Below you will find US production data from the US Department of Aquaculture Quick Stats database for food fish. The information displayed is from the most recent, comprehensive, available source: the USDA 2018 Census Aquaculture. Farms include facilities with sales of $1,000 or more. Total sales in dollars for US food fish production had to be estimated for 15 states due to undisclosed data. These were estimated by multiplying the number of farm operations by the average US sales per operation. The 2018 USDA aquaculture census will be released in late 2019.")),
                                      
                                      
                                      ## Summary Stats
                                      summary_stats_ui(id = "fish_metrics",
                                                       number_boxes = 3),
                                      
                                      
                                      ## Food Fish US Map
                                      choro_map_ui(id = "fish_us_map",
                                             title_text = paste0("Food Fish Aquaculture Sales in 2018"),
                                             sub_title_text = "Start exploring! Select type of data to view: 1) sales in dollars 2) production in weight, no. of fish, or eggs 3) total farm operations. Click on states to see values. It may take a few seconds to load. The data in the map categorizes states into 4 quantiles with 75-100% being the top producing states. Don't forget to check out Hawaii!",
                                             select_type = "radio",
                                             select_location = "above",
                                             select_choices = c("Dollars" = "DOLLARS",
                                                                # "Pounds" = "LB",
                                                                # "Fish" = "HEAD",
                                                                # "Eggs" = "EGGS",
                                                                "Farms" = "OPERATIONS"),
                                             select_label = NULL,
                                             source_text = list(
                                               p("Sources:"),
                                               p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2018)"))
                                      ), # end of map ui
                                      
                                      
                                      ## Fish Dollars per Operation Timeseries
                                      plot_ui(id = "fish_dolop_plot",
                                              title_text = "Change in Sales and Number of Food Fish Farms",
                                              sub_title_text = "Start exploring! USDA data on aquaculture production is only available for the census years 1998, 2005, 2013, and 2018. Mississippi is an outlier and not plotted on the graph below.",
                                              select_type = "slider_discrete",
                                              select_location = "below",
                                              select_choices = unique(fish_dolop_plot$Year),
                                              animate = TRUE,
                                              select_label = "Play the Timeseries:",
                                              source_text = list(
                                                p("Sources:"),
                                                p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2018)"))

                                      ) # end fish dollars/operation timeseries
                                  ) # end div
                         ), # end food fish tab panel
                                             
                         
                         
                         tabPanel("Mollusks",
                                  
                                  div(class = "master",
                                      
                                      tab_title(title = "US Mollusk Production",
                                                lead = "\"The first aquaculture census was conducted in 1998, in response to the intense need for an accurate measure of the aquaculture sector. The aquaculture Census collects detailed information relating to production volume and methods, surface water acres and sources, sales, point of first sale outlets, and aquaculture distributed for restoration, conservation, enhancement, or recreational purposes.\" - USDA Census of Aquaculture",
                                                subtitle = "About the Data:",
                                                description = list("Below you will find US production data from the US Department of Aquaculture Quick Stats database for mollusks. The information displayed is from the most recent, comprehensive, available source: the USDA 2018 Census Aquaculture. Farms include facilities with sales of $1,000 or more. For mollusk production, total sales in dollars had to be estimated for Alaska, Georgia, Hawaii, Maine, Massachusetts, and Pennsylvania. These were estimated by multiplying the number of farm operations by the average US sales per operation. The 2018 USDA aquaculture census will be released in late 2019.")),
                                      
                                      
                                      ## Summary Stats
                                      summary_stats_ui(id = "shell_metrics",
                                                       number_boxes = 3),
                                      
                                      
                                      ## Mollusk US Map
                                      choro_map_ui(id = "shell_us_map",
                                             title_text = paste0("Mollusk Aquaculture Sales in 2018"),
                                             sub_title_text = "Start exploring! Select type of data to view: 1) sales in dollars 2) total farm operations. Click on states to see values. It may take a few seconds to load. The data in the map categorizes countries into 4 quantiles with 75-100% being the top producing states. Don't forget to check out Alaska and Hawaii!",
                                             select_type = "radio",
                                             select_location = "above",
                                             select_choices = c("Dollars" = "DOLLARS",
                                                                "Farms" = "OPERATIONS"),
                                             select_label = NULL,
                                             source_text = list(
                                               p("Sources:"),
                                               p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2018)"))
                                      ), # end of map ui
                                      
                                      
                                      ## Mollusk Dollars per Operation Timeseries
                                      plot_ui(id = "moll_dolop_plot",
                                              title_text = "Change in Sales and Number of Mollusk Farms",
                                              sub_title_text = "Start exploring! USDA data on aquaculture production is only available for the census years 1998, 2005, 2013, and 2018. Mississippi is an outlier and not plotted on the graph below. While the number of shellfish farms in Washington haven't changed significantly over the years, the total sales rose from around 30 Million USD to over 80 Million USD! The sales and number of mollusk operations in Texas also rose significantly.",
                                              select_type = "slider_discrete",
                                              select_location = "below",
                                              select_choices = unique(moll_dolop_plot$Year),
                                              animate = TRUE,
                                              select_label = "Play the Timeseries:",
                                              source_text = list(
                                                p("Sources:"),
                                                p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2018)"))
                                              
                                      ) # end mollusk dollars/operation timeseries
                                      
                                  ) # end div
                         ) # end mollusk tab panel
                                  
                         
              ), # end US Production nav bar menu
              
              
              ## Imports ##
              navbarMenu("Imports",
                         
                         
                         ## FDA Import Refusals ##
                         tabPanel("FDA Import Refusals",
                                  
                                  div(class = "master",
                                      
                                      ## Tab Title
                                      tab_title(title = "FDA Shrimp Import Refusals",
                                                lead = "\"About 94 percent of our shrimp supply comes from abroad, from countries such as India, Indonesia, and Thailand.\" - Consumer Report 2015",
                                                subtitle = "About the Data:",
                                                description = paste(" Below you will find data on US shrimp imports that were refused by the US Food and Drug Administration from 2002 to September of 2018. Data includes the number of refusals per exporting country, as well as a break down of the top 4 reasons for refusal into the US. These include presence of salmonella, filthy condition, nitrofurans (a type of drug used in antibiotics), veterinary drugs, and all other.")),
                                      
                                      ## Summary Stats
                                      summary_stats_ui(id = "shrimp_metrics",
                                                       number_boxes = 3),
                                      
                                      ## Shrimp Refusals Map
                                      dot_map_ui(id = "shrimp_dot_map",
                                                 title_text = paste0("How many Shrimp Imports were Refused by the FDA?"),
                                                 sub_title_text = "Start exploring! Hover over data points to see the number of shrimp imports that were refused by the FDA and the associated import country. Use the slider to view import refusal data from 2002 to September 2018. Click on the play button to view an animation of the time series.",
                                                 select_type = "slider",
                                                 select_location = "below",
                                                 slider_min = 2002,
                                                 slider_max = 2018,
                                                 slider_start = 2018,
                                                 slider_sep = "",
                                                 animate = TRUE,
                                                 source_text = list(
                                                   p("Sources:"),
                                                   p(tags$sup("1."), tags$a(href="https://www.accessdata.fda.gov/scripts/ImportRefusals/index.cfm", "Food and Drug Administration"), ", Import Refusal Report (2002-2018)"))
                                                 ), # end dot map ui
                                      
                                 
                                      ## Shrimp Refusal Charges Plot
                                      plot_ui(id = "shrimp_stacked_plot",
                                        title_text = "Why were the Shrimp Imports Refused?",
                                        sub_title_text = "Start exploring! The countries in the drop-down menu are ordered by highest to lowest cumulative number of shrimp import refusals over the last 5 years. Malaysia is the top, recent offender with 697 import refusals from 2014 - Sep. 2018, followed by India and Vietnam with 447 and 212 refusals respectively. Hover over the bars to view number of refusals per reason.)",
                                        select_type = "search",
                                        select_location = "above",
                                        select_choices = order_cntry,
                                        select_label = "Choose a Country:",
                                        source_text = list(
                                          p("Sources:"),
                                          p(tags$sup("1."), tags$a(href="https://www.accessdata.fda.gov/scripts/ImportRefusals/index.cfm", "Food and Drug Administration"), ", Import Refusal Report (2002-2018)"))
                                        
                                      ) # end shrimp refusal charges bar graph
                                      
                                      
                                  ) # end div
                         ) # end shrimp import refusal tab panel
                         
                         
              ) # end Imports tab
              
              
              
              # ) # end dashboard body
) # end nav page


