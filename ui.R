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
                         
                         tabPanel("USDA Census",
                                  
                                  div(class = "master",
                                      
                                      tab_title(title = "USDA Census of Aquaculture",
                                                lead = "\"The first aquaculture census was conducted in 1998, in response to the intense need for an accurate measure of the aquaculture sector. The aquaculture Census collects detailed information relating to production volume and methods, surface water acres and sources, sales, point of first sale outlets, and aquaculture distributed for restoration, conservation, enhancement, or recreational purposes.\" - USDA Census of Aquaculture",
                                                subtitle = "About the Data:",
                                                description = list("Below you will find US production data from the US Department of Aquaculture Quick Stats database for food fish and mollusks. The information displayed is from the most recent, comprehensive, available source: the USDA 2013 Census Aquaculture. Farms include facilities with sales of $1,000 or more. Total sales in dollars for US food fish production had to be estimated for 15 states due to undisclosed data. For mollusk production, total sales in dollars had to be estimated for Alaska, Georgia, Hawaii, Maine, Massachusetts, and Pennsylvania. These were estimated by multiplying the number of farm operations by the average US sales per operation for mollusks and food fish separately. The 2018 USDA aquaculture census will be released in late 2019.")),
                                      
                                      
                                      ## Baseline Metrics
                                      summary_stats_ui(id = "fish_metrics",
                                                       number_boxes = 3),
                                      
                                      
                                      ## Food Fish US Map
                                      choro_map_ui(id = "fish_us_map",
                                             title_text = paste0("Food Fish Aquaculture Sales in 2013"),
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
                                               p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2013)"))
                                      ), # end of map ui
                                      
                                      
                                      ## Baseline Metrics
                                      summary_stats_ui(id = "shell_metrics",
                                                       number_boxes = 3),
                                      
                                      
                                      ## Mollusk US Map
                                      choro_map_ui(id = "shell_us_map",
                                             title_text = paste0("Mollusk Aquaculture Sales in 2013"),
                                             sub_title_text = "Start exploring! Select type of data to view: 1) sales in dollars 2) total farm operations. Click on states to see values. It may take a few seconds to load. The data in the map categorizes countries into 4 quantiles with 75-100% being the top producing states. Don't forget to check out Alaska and Hawaii!",
                                             select_type = "radio",
                                             select_location = "above",
                                             select_choices = c("Dollars" = "DOLLARS",
                                                                "Farms" = "OPERATIONS"),
                                             select_label = NULL,
                                             source_text = list(
                                               p("Sources:"),
                                               p(tags$sup("1."), tags$a(href="https://quickstats.nass.usda.gov/", "US Department of Agriculture"), ", Quick Stats Census (2013)"))
                                      ) # end of map ui
                                      
                                  ) # end div
                                  
                         ) # end US prod tab panel
                         
              ), # end US Production nav bar menu
              
              
              ## Imports ##
              navbarMenu("Imports",
                         
                         
                         ## FDA Import Refusals ##
                         tabPanel("FDA Import Refusals",
                                  
                                  div(class = "master",
                                      
                                      tab_title(title = "FDA Shrimp Import Refusals",
                                                subtitle = "About the Data:",
                                                description = "Below you will find data on US shrimp imports that were refused from the Food and Drug Administration database. Data includes the number of refusals per country from 2002 to 2018 as well as percentage of refusals due to presence of salmonella, filth, nitrofurans (a type of drug used in antibiotics), and veterinary drugs."),
                                      
                                      ## Shrimp Refusals Map
                                      dot_map_ui(id = "shrimp_dot_map",
                                                 title_text = paste0("Import Refusals"),
                                                 sub_title_text = "Start exploring! Click on specific countries to see the number of shrimp imports that were refused by the FDA for that year. Use the slider to view import refusal data from 2002 to 2018.",
                                                 select_type = "slider",
                                                 select_location = "below",
                                                 slider_min = 2002,
                                                 slider_max = 2018,
                                                 slider_start = 2018,
                                                 slider_sep = "",
                                                 
                                                 
                                                 source_text = list(
                                                   p("Sources:"),
                                                   p(tags$sup("1."), tags$a(href="https://www.accessdata.fda.gov/scripts/ImportRefusals/index.cfm", "Food and Drug Administration"), ", Import Refusal Report (2002-2018)"))
                                                 ), # end dot map ui
                                      
                                      
                                      ## Refusal Charges Stacked Bar Graph
                                      plot_ui(id = "shrimp_stacked_plot",
                                        title_text = "Top Shrimp Refusal Charges",
                                        sub_title_text = NULL,
                                        select_type = "search",
                                        select_location = "above",
                                        select_choices = unique(shrimp_stacked$COUNTRY_NAME),
                                        selected = "India",
                                        select_label = NULL, 
                                        source_text = list(
                                          p("Sources:"),
                                          p(tags$sup("1."), tags$a(href="https://www.accessdata.fda.gov/scripts/ImportRefusals/index.cfm", "Food and Drug Administration"), ", Import Refusal Report (2002-2018)"))
                                        
                                      ) # end shrimp refusal charges bar graph
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                  ) # end div
                         ) # end shrimp import refusal tab panel
                         
                         
              ) # end Imports tab
              
              
              
              # ) # end dashboard body
) # end nav page


