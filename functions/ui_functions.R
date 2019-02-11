## User Interface Functions 
## for CSS/HTML

library(shiny)
library(shinydashboard)


# aq_header
## Custom Header for CA Aquaculture Dashboard
## Note to self: header created in Word (aq_header_shiny.docx)
dash_header =
  ## Adds a basic header, that will only display
  ##  if the page is not embedded as an iframe.
  function() {
    div(id = "dash-header",
                 div(class = "aq-brand",
                     tags$a(class = "aq-brand", href = "https://iwensu0313.github.io",
                            title = "Shiny by Iwen Su",
                            tags$img(src = "https://docs.google.com/drawings/d/e/2PACX-1vRQzA-kpGtw9ehYyOJNmfgmOk1g3H0Yal-brEmzglO5yCzisl7LVUI6NWs4PzvWYL02cAnEubra_vlP/pub?w=953&h=204",
                                     alt = "Dashboard by Iwen Su",
                                     width = "600"))
                     ))
  }




# dashboardPage
## Modified version of dashboardPage
## Original Source: New Zealand Tourism Dashboard
## Github User: nz-mbie
dashboardPage =
  ## Modified navbarPage from shiny
  ## Cuts bloat and enables use of tags$head with `thead`
  function(title, ..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title){
    pageTitle = title
    navbarClass = "navbar navbar-default"
    tabs = list(...)
    tabset = shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
    containerDiv = div(class = "container", div(class = "navbar-header", 
                                                span(class = "navbar-brand", pageTitle)), tabset$navList)
    contentDiv = div(class = "container-fluid")
    if(!is.null(header))
      contentDiv = tagAppendChild(contentDiv, div(class = "row", header))
    contentDiv = tagAppendChild(contentDiv, tabset$content)
    if(!is.null(footer)) 
      contentDiv = tagAppendChild(contentDiv, div(class = "row", footer))
    bootstrapPage(title = windowTitle, thead,
                  tags$nav(class = navbarClass, role = "navigation", containerDiv),
                  contentDiv)
  }




# tab_title
tab_title <- function(title, 
                      lead = NULL,
                      subtitle,
                      description) {
  
  fluidRow(box(
    h1(strong(title)),
    tags$div(class = "title_section", lead),
    tags$br(),
    h4(paste(subtitle)),
    p(description),
    width = 12)
  )
  
}