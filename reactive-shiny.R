library(dplyr)
library(dtplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(DT)
library(xlsx)
library(leaflet)
library(plotly)
library(ggplot2)
library(readxl)
library(lubridate)

##set working directory same as the stored csv files
##csv files are the database
#setwd("/home/snehil/Documents/Dash/dashboardcomponents")

#rm(list = ls())

##---read the database
rs <- data.table(read_excel("d.xlsx", sheet = "r"))
#print(rs)

str <- data.table(read_excel("d.xlsx", sheet = "str"))
cmp <- data.table(read_excel("d.xlsx", sheet = "comp"))
lgdb <- data.table(read_excel("d.xlsx", sheet = "login"))

print(str)

##employees not available from and to
rs$NAF <- as.Date.POSIXct(rs$NAF)
rs$NAT <- as.Date.POSIXct(rs$NAT)

bdt <- as.Date(first(str$Month))
edt <- as.Date(bdt + 90)

##----------------------UI----------------

ui <- shinyUI({
  navbarPage(
    title = "Dash!",
    id = "pages",
    inverse = FALSE,
    theme = shinytheme("flatly"),
    
    tabPanel("Tab 1", id = "tab1",
             sidebarLayout(sidebarPanel({
               div(
                 class = "panel panel-primary",
                 div(class =
                       "panel-heading", icon("users"), "User Login"),
                 div (
                   class = "panel-body",
                   div(
                     class = "form-group has-info",
                     tags$label(class = "control-label "),
                     tags$input(
                       class = "form-control",
                       id = "Username",
                       type = "text",
                       placeholder = "Enter Username"
                     )
                   ),
                   div(
                     class = "form-group has-success",
                     tags$label(class = "control-label "),
                     tags$input(
                       class = "form-control",
                       id = "Password",
                       type = "password",
                       placeholder = "Enter Password"
                     )
                   ),
                   
                   
                   hr(),
                   div(class = "btn btn-success action-button btn-block ", id = "lg", "Login"),
                   hr(),
                   uiOutput("lginstate")
                 )
               )
             }),
             mainPanel({
               dataTableOutput("table1")
               #  plotlyOutput("plot1")
             }))),
    tabPanel("Tab 2", id = "tab2",
             fluidPage(
               titlePanel("Details"),
               sidebarLayout(
                 sidebarPanel (
                   dateRangeInput(
                     inputId = "daterangeinput",
                     label = "Enter Dates",
                     format = "dd/mm/yy"
                   ),
                   textOutput(outputId = "dateoutput", inline = TRUE),
                   shiny::actionButton(inputId = "dispagl", "Display")
                 ),
                 mainPanel(dataTableOutput("table2")
                           #  ,plotlyOutput("plot2"))
                 )
               ))),
    tabPanel("Tab 3", id = "tab3")
             
    )
    
})
  
  ##---creating global variables
  
  wk <- data.table(NULL)
  server <- shinyServer(function(input, output, session) {
    ######################################ui page
    ############################################
    USER <- reactiveValues(Logged = FALSE)
    
    usrfnc <- reactive({
      if (USER$Logged == TRUE) {
        uname <- isolate(input$Username)
        f <- lgdb$Function[lgdb$CID == uname]
        
      }
    })
    observeEvent(input$lg, {
      Username <- isolate(input$Username)   ####get the username
      Password <-
        isolate(input$Password)   #### and the password entered
      
      
      Id <- lgdb[lgdb$CID == Username]
      Id <- Id[Id$Password == Password]
      print(Id)
      Id.password <- which(lgdb$Password == Password)
      
      usrfnc <<- lgdb$Function[Id$CID == lgdb$CID]
      print(usrfnc)
      
      if (nrow(Id)) {
        USER$Logged <- TRUE
        
        output$lginstate <- renderUI(
          HTML(
            '
            <div class="alert alert-dismissible alert-info">
            <button type="button" class="close" data-dismiss="alert">&times;</button>
            <strong>Login Successful!</strong>.
            </div>
            '
          )
          
          )
        
        
      }
      else  {
        USER$Logged <- FALSE
        output$lginstate <- renderUI(
          HTML(
            '
            <div class="alert alert-dismissible alert-danger">
            <button type="button" class="close" data-dismiss="alert">&times;</button>
            <strong>Wrong Username or Password!</strong>
            </div>
            '
          )
          
          
          )
      }
      
      if (USER$Logged == TRUE)
      {
        updateTabsetPanel(session, inputId = "pages", selected = "Tab 2") # ENTER THE NAME OF THE TAB, NOT THE ID!
        #  updateTabsetPanel(session, "pages", selected = "tab3")
        
        print("User Logged!")
        
      }
      
    })
    
    
    
    output$dateoutput <- renderText(paste("Date Range:- ", paste(as_date(input$daterangeinput), collapse = " to ")))
    #lubridate::as_date(input$daterangeinput[1]))
    output$table1 <- renderDataTable(lgdb)
    # output$plot1 <- renderPlotly(plot_ly())
    
    
    ##show supply and demand for the function of the employee
    
    wdata <- reactive({
      ufn <- usrfnc
      wk <- str[str$Month >= bdt & str$Month <= edt]
      wk <- wk[wk$Function %in% ufn]
      wk$Disc <- wk$Supply - wk$Demand
      return(wk)
    })
    ##---------display comp functions, date of availability, number of employees
    observeEvent(input$dispagl, {
      ##---------display comp functions, date of availability, number of employees
      w <- wdata()
      rs <- rs[rs$Organisation %in% w$Organisation]
      str(w)
      s <- data.table(unique(rs$Organisation), unique(rs$Function))
      colnames(s) <-  c("Organisation", "Function")
      
      wk <- count(rs, rs$Function)
      colnames(wk) <- c("Function", "Number")
      
      aglrs <- inner_join(s, wk, by = "Function")
      
      output$table2 <- renderDataTable(aglrs)
      
      ##______________________________
      
      
    })
    
    ##______________________________
    
    #  output$plot2 <- renderPlotly(
    #    plot_ly(data = str, type = "scatter", x = ~str$Function,y = ~str$Supply, mode = 'lines')
    #    #%>% add_trace(y =  ~str$Demand, name = "Demand", mode = 'lines')
    #    %>% #lines is inside single quotes
    #      layout(title = "Supply / Demand", yaxis = list(zeroline = FALSE),
    #             xaxis = list(zeroline = FALSE))
    #  )
  })
    
    ##---Run the application
    shinyApp(ui = ui, server = server)