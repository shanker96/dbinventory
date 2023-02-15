# Load required libraries
#install.packages("DT")
library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(sf)
library(ggplot2)
library(shinyalert)
library(tidyverse)

# Read database credentials=
source("./credentials_v4.R")


ui <- dashboardPage(
  dashboardHeader(title = "ITOM6265-Final Project", titleWidth = 250),
  skin = "green",
  #Sidebar content
  dashboardSidebar(
  #Add sidebar menus here
    sidebarMenu(
      menuItem("HOME", tabName = "HOME", icon = icon("home")),
      menuItem("PRODUCT", tabName = "PRODUCT", icon = icon("search")),
      menuItem("STATUS", tabName = "STATUS", icon = icon("list-alt")),
      menuItem("REPORT", tabName = "REPORT", icon = icon("table")),
      menuItem("STORE MAP", tabName = "MAP", icon = icon("map")),
      menuItem("ANALYTICS", tabName = "ANALYTICS", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    includeCSS("./styles.css"),
    # tags$style('.container-fluid {
    #                          background-color: #007BA7;
    #           }'),
    tabItems(
      # Add contents for first tab*
      tabItem(tabName = "HOME",
              # h1("BROWN FORMAN CORPORATION",style = 'color:black; font-weight:bold;font-family: Times New Roman;
              #    font-size:300%;text-align:center;background-color:powderblue;
              #    border-width:3px; border-style:solid; border-color:black; padding: 1em;'),
              # h2("DEVELOPED BY GROUP 11",style = 'color:green; font-weight:bold;font-family: monospace;
              #    font-size:150%;text-align:center;background-color:powderblue;
              #    border-width:3px; border-style:solid; border-color:green; padding: 1em;'),
              # h3("Saumiya, Shanker, Vera, Yash",style = 'color:green; font-weight:bold;font-family:Geneva;
              #    font-size:100%;text-align:center;background-color:powderblue;
              #    border-width:3px; border-style:dotted; border-color:orange; padding: 1em;'),
              imageOutput("plotfrontend", width = "100%", height = "800px"),
              #tags$iframe(width="560", height="315", src="\\Mac\Home\Desktop\Entertainment\Friends Season 1-10 S01-S10 (1080p BluRay 5.1)\Season 4\Friends (1994) - S04E01 - The One With The Jellyfish (1080p BluRay x265 Silence).mkv", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
              #tabPanel("Help",tags$video(src="sample2.mp4",width="500px",height="350px",type="video/mp4",controls="controls"))
          ),
      # Add contents for second tab
      tabItem(tabName = "PRODUCT",
               navbarPage(
                 "STOCK MANIPULATION",
                 id = "main_navbar",
                 
                 tabPanel(
                   "CREATE",
                sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      h3("ENTER PRODUCT DETAILS"),
                      textInput("pdt_id", label = "Product ID: ", value = ""),
                      textInput("pdt_name", label = "Product Name: ", value = ""),
                      textInput("pdt_desc", label = "Product Description: ", value = ""),
                      #textInput("pdt_expD", label = "Expiry Date('YYYY-MM-DD'): ", value = ""),
                      dateInput("pdt_expD", label = h3("Expiry Date"), value = "2022-01-01"),
                      textInput("pdt_price", label = "Price: ", value = ""),
                      #textInput("pdt_manid", label = "Manufacturer ID: ", value = ""),
                      selectInput("pdt_manname", label = h3("Manufacturer Name: "), 
                                  choices = list("Rolfson" = "Rolfson", "Crown Royal" = "Crown Royal",
                                                 "Hoeger-Heidenreich" = "Hoeger-Heidenreich",
                                                 "Titos Handmade" = "Titos Handmade"), 
                                  selected = 1),
                      textInput("pdt_thsd", label = "Product Quantity Threshold: ", value = ""),
                      textInput("pdt_qty", label = "Product Quantity Available: ", value = ""),
                      textInput("pdt_stl", label = "Stock Location: ", value = ""),
                      actionButton("create_button", label = "Create Entry"),
                     ),
                    mainPanel(
                      conditionalPanel(
                        condition = "input.pdt_id == '' || input.pdt_name == '' || input.pdt_desc == '' ||
                        input.pdt_price == '' || input.pdt_manname == ''|| input.pdt_thsd == ''|| input.pdt_qty == ''|| input.pdt_stl == ''",
                        verbatimTextOutput("create_err"),
                      ),
                      h3(textOutput("create_caption1", container = span)),
                      DT::dataTableOutput("create_table1"),
                      h3(textOutput("create_caption2", container = span)),
                      DT::dataTableOutput("create_table2"))
                   )
                 ),
                 tabPanel(
                   "READ",
                   sidebarLayout(
                     sidebarPanel(
                      width = 3,

                       textInput("read_field", label = "ENTER PRODUCT ID OR PRODUCT NAME", value = "By default brings all data"),
                       actionButton("read_button", label = "Get Data"),
                     ),
                     mainPanel(h3(textOutput("read_caption1", container = span)),
                               DT::dataTableOutput("read_table1"),
                               h3(textOutput("read_caption2", container = span)),
                               DT::dataTableOutput("read_table2"))
                   )
                 ),
                tabPanel(
                  "UPDATE",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      
                      textInput("update_pid", label = "ENTER PRODUCT ID", value = ""),
                      # Copy the line below to make a set of radio buttons
                      radioButtons("radio", label = h3("Change"),
                                   choices = list("Increase" = 1, "Decrease" = 2), 
                                   selected = 1),
                      
                      hr(),
                      #fluidRow(column(3, verbatimTextOutput("value"))),
                      
                      numericInput("update_pqty", label = "Product changed by", value = 0),
                      actionButton("update_button", label = "Update Data"),
                    ),
                    mainPanel(
                      conditionalPanel(
                        condition = "input.update_pid == ''",
                        verbatimTextOutput("update_err"),
                      ),
                      h3(textOutput("update_caption1", container = span)),
                      DT::dataTableOutput("update_table1"),
                      h3(textOutput("update_caption2", container = span)),
                      DT::dataTableOutput("update_table2"))
                  )
                ),
                tabPanel(
                  "DELETE",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      
                      textInput("delete_field", label = "ENTER PRODUCT ID", value = ""),
                      actionButton("delete_button", label = "Delete Data"),
                    ),
                    mainPanel(
                      conditionalPanel(
                        condition = "input.delete_field == ''",
                        verbatimTextOutput("delete_err"),
                      ),
                      h3(textOutput("delete_caption1", container = span)),
                      DT::dataTableOutput("delete_table1"),
                      h3(textOutput("delete_caption2", container = span)),
                      DT::dataTableOutput("delete_table2"))
                  )
                )
               )
      ),
      #  Add contents for third tab
      tabItem(tabName = "STATUS",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  
                  textInput("status_field", label = "ENTER ORDER ID", value = ""),
                  selectInput("select_dropdown", label = h3("Select status"), 
                              choices = list("All" = "All","Out For Delivery" = "Out For Delivery", "In Transit" = "In Transit",
                                             "Order Placed" = "Order Placed","Failed Delivery Attempt" = "Failed Delivery Attempt",
                                             "Delivered" = "Delivered"), 
                              selected = 0),
                  
                  hr(),
                  actionButton("status_button", label = "Get Status"),
                ),
                mainPanel(h3(textOutput("status_caption2", container = span)),
                          DT::dataTableOutput("status_table2"),
                          h3(textOutput("status_caption1", container = span)),
                          DT::dataTableOutput("status_table1"))
              )),
      
      #  Add contents for fourth tab
      tabItem(tabName = "REPORT",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
              checkboxGroupInput("checkGroup", label = h3("Data range type"), 
                                 choices = list("Date Ordered" = 1, "Date Completed" = 2),
                                 selected = 1),
              # Copy the line below to make a date range selector
              dateRangeInput("dates", label = h3("Date range")),
              hr(),
              actionButton("report_button", label = "Get Report"),
              ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.checkGroup != 1 && input.checkGroup!=2",
                   verbatimTextOutput("report_err"),
                 ),
                 h3(textOutput("report_caption1", container = span)),
                 DT::dataTableOutput("report_table1"))
              )),
      
      tabItem(tabName = "MAP",
              p(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  
                  selectInput("map_select", label = h3("Select State"), 
                              choices = list("All" = "All","Alaska" = "Alaska", "Arizona" = "Arizona", "California" = "California",
                                             "Colorado" = "Colorado", "District of Columbia" = "District of Columbia", "Florida" = "Florida",
                                             "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho",
                                             "Illinois" = "Illinois", "Indiana" = "Indiana", "Kansas" = "Kansas",
                                             "Minnesota" = "Minnesota", "North Carolina" = "North Carolina", "Ohio" = "Ohio",
                                             "Oklahoma" = "Oklahoma", "Rhode Island" = "Rhode Island", "South Dakota" = "South Dakota",
                                             "Texas" = "Texas", "Virginia" = "Virginia", "West Virginia" = "West Virginia"), 
                              selected = 1),
                  actionButton("map_button", label = "Show Map"),
                ),
                mainPanel(leafletOutput("mymap", height = 500))
              ),
              ),
      tabItem(tabName = "ANALYTICS",
              navbarPage(
                "INSIGHTS",
                id = "main_navbar",
                
                tabPanel(
                  "SALES",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput(inputId = "type",label="Select the alcohol type",
                                  list("Wine","Beer","Gin","Whiskey","Vodka","Rum","Cognac","Tequila")),
                      # Show a plot of the generated distribution
                      actionButton("analytics_button1", label = "Get graph"),
                    ),
                    mainPanel(h3(textOutput("analytics_caption1", container = span)),plotOutput("distPlot1"))
                  )
                ),
                tabPanel(
                  "STOCK",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput(inputId = "type1",label="Select the alcohol type",
                                  list("Wine","Beer","Gin","Whiskey","Vodka","Rum","Cognac","Tequila")),
                      # Show a plot of the generated distribution
                      actionButton("analytics_button2","Show graph",icon("refresh")),
                    ),
                    mainPanel(width = 8,
                              h1("STOCK"),
                              plotOutput("distPlot2", height = "600px", width = "100%"))
                  )
                )
                ),
              )
    )
  ),
)

server <- function(input, output) {
  
  #browser()
  output$plotfrontend <- renderImage({
    list(src = './ppt.png',width = "100%", height = "800")
  }, deleteFile = FALSE)

  #Develop your server side code (Model) here
  observeEvent(input$create_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    #output$value <- renderPrint({ input$read_field })
    #browser();
    if(input$pdt_id == '' || input$pdt_name == '' || input$pdt_desc == '' ||
       input$pdt_price == '' || input$pdt_manname == '' || 
       input$pdt_thsd == ''|| input$pdt_qty == ''|| input$pdt_stl == '')
    {
      output$create_err <- renderDataTable({
        validate(
          need(input$pdt_id, 'Product ID is missing.'),
          need(input$pdt_name, 'Product Name is missing.'),
          need(input$pdt_desc != '', 'Product Description is missing.'),
          #need(input$pdt_expD != '', 'Product Expire Date missing.'),
          need(input$pdt_price != '', 'Product Price missing.'),
          need(input$pdt_manname != '', 'Product Manufacturer Name is missing.'),
          need(input$pdt_thsd != '', 'Product Threshold is missing.'),
          need(input$pdt_qty != '', 'Product Price is missing.'),
          need(input$pdt_stl != '', 'Stock location is missing.')
        )
      })
      shinyalert("Oops!", "Data fields are empty", type = "warning")
    }
    else
    {
      query <- paste("select Man_ID from MANUFACTURER where Man_Name like '%",input$pdt_manname,"%';",sep="");
      print(query);
      qty <- dbGetQuery(db, query);
      manid <- as.numeric(gsub(".*?([0-9]+).*", "\\1", qty)) 
      
    query1 <- paste("SET IDENTITY_INSERT STOCK ON; insert into STOCK ([PRODUCT_ID] ,[Threshold], [Qty_Available], [Stock_Location])
                   values('",  input$pdt_id,"' , '", input$pdt_thsd,"' , '", input$pdt_qty,"' , '", input$pdt_stl,
                    "');SET IDENTITY_INSERT STOCK OFF")
    
    print(query1)
    
    data1 <- dbGetQuery(db, query1)
    
    query2 <- paste("SET IDENTITY_INSERT PRODUCT ON; insert into PRODUCT ([PRODUCT_ID] ,[Name], [PRODUCT_Description], [Expiry_Date], [Price], [Man_ID])
                   values('",  input$pdt_id,"' , '", input$pdt_name,"' , '", input$pdt_desc,"' , '", input$pdt_expD,
                   "' , '", input$pdt_price,"' , '", manid,"' );SET IDENTITY_INSERT PRODUCT OFF")
    
    print(query2)
    
    data2 <- dbGetQuery(db, query2)
    
    query1 <- paste("select * from PRODUCT where PRODUCT_ID=",input$pdt_id,";",sep="");
    query2 <- paste("select * from STOCK where PRODUCT_ID=",input$pdt_id,";",sep="");
    
    data1 <- dbGetQuery(db, query1);
    data2 <- dbGetQuery(db, query2);
    
    output$create_caption1 <- renderText({
      "PRODUCT TABLE"
    })
    
    output$create_caption2 <- renderText({
      "STOCK TABLE"
    })
    
    output$create_table1 = DT::renderDataTable({
      data1
    })
    output$create_table2 = DT::renderDataTable({
      data2
    })
    shinyalert("Yay!", "Created sucessfully", type = "success")
    }
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$read_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #output$value <- renderPrint({ input$read_field })
    
    if (input$read_field == "By default brings all data")
    {
      query1 <- paste("select * from PRODUCT;",sep="");
      query2 <- paste("select * from STOCK;",sep="");
    }
    else if (grepl("[[:digit:]]", input$read_field))
    {
      query1 <- paste("select * from PRODUCT where PRODUCT_ID=",input$read_field,";",sep="");
      query2 <- paste("select * from STOCK where PRODUCT_ID=",input$read_field,";",sep="");
      
    }
    else
    {
      query1 <- paste("select * from PRODUCT where Name like '%", input$read_field ,"%';",sep="");
      query2 <- paste("select * from STOCk where PRODUCT_ID in (select PRODUCT_ID from PRODUCT where Name like '%", input$read_field ,"%');",sep="");
    }
    print(query1);
    print(query2);
    data1 <- dbGetQuery(db, query1);
    data2 <- dbGetQuery(db, query2);
    
    output$read_caption1 <- renderText({
      "PRODUCT TABLE"
    })
    
    output$read_caption2 <- renderText({
      "STOCK TABLE"
    })
    
    output$read_table1 = DT::renderDataTable({
      data1
    })
    
    output$read_table2 = DT::renderDataTable({
      data2
    })
    
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$update_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    if(input$update_pid == '' || input$update_pqty == '')
    {
      output$update_err <- renderDataTable({
        validate(
          need(input$update_pid, 'Product ID is missing.'),
          need(input$update_pqty, 'Change quantity is missing.'),
        )
      })
    }
    else
    {
      query1 <- paste("SELECT * FROM STOCK where PRODUCT_ID =",input$update_pid,";")
      print(query1);
      data1 <- dbGetQuery(db, query1);
      
      output$update_caption1 <- renderText({
        "BEFORE UPDATE"
      })
      
      output$update_table1 = DT::renderDataTable({
        data1
      })
      query <- paste("select Qty_Available from STOCK where PRODUCT_ID =",input$update_pid,";");
      print(query);
      qty <- dbGetQuery(db, query);
      current <- as.numeric(gsub(".*?([0-9]+).*", "\\1", qty)) 
      print(current);
      
      
      
      if(input$radio == 1)
      {
        current = current + input$update_pqty;
      }
      else
      {
        current = current - input$update_pqty;
      }
      
      query <- paste("select Threshold from STOCK where PRODUCT_ID =",input$update_pid,";");
      print(query);
      qty <- dbGetQuery(db, query);
      threshold <- as.numeric(gsub(".*?([0-9]+).*", "\\1", qty)) 
      print(threshold);
      
      if(current <= threshold)
      {
        msg <- paste("STOCK WENT BELOW THRESHOLD FOR PRODUCT ID:",input$update_pid)
        shinyalert("Oops!", msg, type = "error")
      }
      else
      {
        shinyalert("Yay!", "Updated sucessfully", type = "success")
      }
        
      
      query <- paste("UPDATE STOCK SET Qty_Available=",current,"where PRODUCT_ID =",input$update_pid,";")
      print(query);
      data <- dbGetQuery(db, query);
      
      query2 <- paste("SELECT * FROM STOCK where PRODUCT_ID =",input$update_pid,";")
      print(query2);
      data2 <- dbGetQuery(db, query2);
      
      
      output$update_caption2 <- renderText({
        "AFTER UPDATE"
      })
      
      output$update_table2 = DT::renderDataTable({
        data2
      })
    }
    
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$delete_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #output$value <- renderPrint({ input$read_field })
    
      if(input$delete_field == "")
      {
       output$delete_err <- renderDataTable({
         validate(
           need(input$delete_field, 'Product ID is missing.'),
         )
       })
       shinyalert("Oops!", "Product ID is missing", type = "warning")
      }
      else
      {
        
        query0 <- paste("SELECT * FROM STOCK where PRODUCT_ID =",input$delete_field,";")
        print(query0);
        data0 <- dbGetQuery(db, query0);
        
        output$delete_caption1 <- renderText({
          "BEFORE DELETE"
        })
        
        output$delete_table1 = DT::renderDataTable({
          data0
        })
        
      query1 <- paste("delete from PRODUCT where PRODUCT_ID ='", input$delete_field ,"';",sep="");
      query2 <- paste("delete from STOCK where PRODUCT_ID ='", input$delete_field ,"';",sep="");
      
      print(query1);
      print(query2);
      
      data1 <- dbGetQuery(db, query1);
      data2 <- dbGetQuery(db, query2);
      
      query3 <- paste("SELECT * FROM STOCK where PRODUCT_ID =",input$delete_field,";")
      print(query3);
      data3 <- dbGetQuery(db, query3);
      
      
      output$delete_caption2 <- renderText({
        "AFTER DELETE"
      })
      
      output$delete_table2 = DT::renderDataTable({
        data3
      })
      shinyalert("Yay!", "Deleted sucessfully", type = "success")
    }
    
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$status_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #output$value <- renderPrint({ input$read_field })
    
    if(input$select_dropdown == "All")
    {
      if (input$status_field == "")
      {
        query1 <- paste("select * from ORDERS;",sep="");
        query2 <- paste("select * from SHIPMENT;",sep="");
      }
      else
      {
        query1 <- paste("select * from ORDERS where Order_ID=", input$status_field ,";",sep="");
        query2 <- paste("select * from SHIPMENT where Order_ID=", input$status_field ,";",sep="");
      }
    }
    else
    {
      query1 <- paste("select * from ORDERS where Order_ID IN
      (select Order_ID from SHIPMENT where Shipment_Status like '%", input$select_dropdown ,"%');",sep="");
      query2 <- paste("select * from SHIPMENT where Shipment_Status like '%", input$select_dropdown ,"%';",sep="");
    }
    
    print(query1);
    print(query2);
    data1 <- dbGetQuery(db, query1);
    data2 <- dbGetQuery(db, query2);
    
    output$status_caption1 <- renderText({
      "ORDER TABLE"
    })
    
    output$status_caption2 <- renderText({
      "SHIPMENT TABLE"
    })
    
    output$status_table1 = DT::renderDataTable({
      data1
    })
    
    output$status_table2 = DT::renderDataTable({
      data2
    })
    
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$report_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    print(length(input$checkGroup));
    print(typeof(input$checkGroup));
    #output$value <- renderPrint({ input$read_field })
    if(is.null(input$checkGroup))
    {
      output$report_err <- renderDataTable({
        validate(
          need(input$checkGroup, 'Need one selection atleast.'),
        )
      })
    }
    else
    {
      if(length(input$checkGroup) == 1)
      {
        if(input$checkGroup == 1)
        {
          query1 <- paste("select * from ORDERS where Date_Ordered between '", input$dates[1] ,"'"," and ","'",input$dates[2],"';",sep="");
        }
        else
        {
          query1 <- paste("select * from ORDERS where Date_Completed between '", input$dates[1] ,"'"," and ","'",input$dates[2],"';",sep="");
        }
      }
      else
      {
        query1 <- paste("select * from ORDERS where Date_Ordered between '", input$dates[1] ,"'"," and ","'",input$dates[2],"' and ",
        "Date_Completed between '", input$dates[1] ,"'"," and '",input$dates[2],"';",sep="");
      }
      print(query1);
      data1 <- dbGetQuery(db, query1);
      
      output$report_caption1 <- renderText({
        "REPORT"
      })
      
      output$report_table1 = DT::renderDataTable({
        data1
      })
    }
    
  })
  
  observeEvent(input$map_button, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    if(input$map_select == "All")
    {
      qdata <- "select latitude,longitude,Store_Name from STORE where latitude is not null and longitude is not null";
    }
    else
    {
      qdata <- paste(
    "select latitude,longitude,Store_Name from STORE where latitude is not null and longitude is not null and State like '%",
    input$map_select,"%';",sep="");
    }
    print(qdata);
    data <- dbGetQuery(db, qdata);
    
    output$mymap <- renderLeaflet({
      leaflet(data = data) %>%
        addTiles() %>%
        #addProviderTiles(providers$Stamen.Toner) %>% 
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addMarkers(lng = ~longitude, 
                   lat = ~latitude,
                   popup = paste("Name:", data$Store_Name, "<br>"))
    })
    
    
    
  })
  
  #Develop your server side code (Model) here
  observeEvent(input$analytics_button1, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
   
    #data <- reactive(
     q1 <- paste("select SUM(Order_Price) AS Sales, MAX(FORMAT([Date_Ordered], 'MMM', 'en-US')) as Month
                 from ORDERS where Order_Des like '%", input$type ,"%' group by MONTH(Date_Ordered);",sep="");
     print(q1);
     data1 <- data.frame(dbGetQuery(db, q1));
    #)
    
     df <- data.frame(x=data1$Month, val=data1$Sales);
     df['variable'] = input$type;
     #print(df)
     output$analytics_caption1 <- renderText({
      "SALES"
     })
     
     Sales <- c(200,400,600,800,1000);
     Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec");
    
    output$distPlot1 = renderPlot({
      ggplot(data = df, aes(x=x, y=val, group=1)) + geom_point() +geom_line(aes(colour=variable))+
        ggtitle("SALES BY MONTH") +
        xlab("MONTH") + ylab("SALES")
    })
    
  })
  
  observeEvent(input$analytics_button2, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    #data <- reactive(
    q1 <- paste("  select PRODUCT_ID, Threshold, Qty_Available from STOCK where PRODUCT_ID in (Select PRODUCT_ID from PRODUCT where Name like '%", input$type1 ,"%');",sep="");
    print(q1);
    df <- data.frame(dbGetQuery(db, q1));
    #)
    
    output$analytics_caption2 <- renderText({
      "STOCK"
    })
    
    product_id <- df$PRODUCT_ID;
    qty <- df$Qty_Available;
    thres <- df$Threshold;
    print(thres)
    print(class(thres))
    #browser()
    red_col <- "#ff2a26"
    red_lab <- "BELOW THRESHOLD"
    yel_col <- "#FFDF00"
    yel_lab <- "ABOVE THRESHOLD"
    grn_col <- "#50C878"
    grn_lab <- "IN STOCK"
    
    df <- data.frame(product_id, qty)
     df <- df %>%
       mutate(mycolor = ifelse(df$qty < thres, red_col, ifelse(df$qty < (thres*1.5), yel_col, grn_col)))
     df <- df %>%
       mutate(legend = ifelse(df$qty < thres, red_lab, ifelse(df$qty < (thres*1.5), yel_lab, grn_lab)))
     
    #print(df)
    output$distPlot2 = renderPlot({
      ggplot(df, aes(x = factor(product_id), y = qty, fill=legend)) +
        geom_col(width = 0.5, position = position_dodge(0.7))+
        geom_bar(stat = "identity", position = "identity", fill=df$mycolor, width = 0.5) +
        geom_text(aes(label = qty), position = position_dodge(1), vjust = 1.5, size = 5, colour="black") +
        ggtitle("Stocks available by product") +
        xlab("PRODUCT ID") + ylab("QUANTITY") +
        theme(legend.position="bottom") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
        scale_fill_manual(values = c(grn_col, yel_col,red_col),labels = c(grn_lab,yel_lab,red_lab))
    })
    
    #ggsave(file="stocks.pdf", width=4, height=4, dpi=300)
    
  })
}

shinyApp(ui, server)

