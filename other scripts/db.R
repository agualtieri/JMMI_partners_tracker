### Partners Tracker - Online Dashboard Code
## Yemen Team Data Unit
## 08-03-200
## V1


###########
#Build Dashboard
###########
ui <-  fluidPage(
  #theme = ".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }",
  
  titlePanel("JMMI Partner Coverage Sheet"),
  #includeCSS("AdminLTE.css"),
  #includeCSS("shinydashboard.css"),
  # Create a new Row in the UI for selectInputs
  # Create a new row for the table.
  mainPanel(
    selectInput("Geographic Area",
                "Geographic Area:",
                c("All",
                  sort(unique(as.character(dt_gov$geo))))),
    
    #https://stackoverflow.com/questions/42532879/r-shiny-layout-side-by-side-chart-and-or-table-within-a-tab
    
    id = 'data',
    tabPanel(h4("Governorate"),
             column(12,
                    h3("North Governorate"),
                    DT::dataTableOutput("North_Table")
             ),
             column(12,
                    h3("South Governorate"),
                    DT::dataTableOutput("South_Table")
             ),
             column(12,
                    h3("Contested Governorate"),
                    DT::dataTableOutput("Contested_Table")
             )
    )
    
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$partner_sum <-DT::renderDataTable(DT::datatable({
    data<-partner_summary
    data
    
    
  }))
  
  output$North_Table <- DT::renderDataTable(DT::datatable({
    data <- dt_gov
    if (dt_gov$geo == "North") {
      data <- data %>%
        group_by(Date) %>%
        select(-c("governorate_ID", "governorate_name", "geo")) %>%
        dplyr::summarise(total_obs=dplyr::n())
    }
    data
  }))
  
  output$South_Table <- DT::renderDataTable(DT::datatable({
    data <- dt_gov_south
    if (input$Governorate == "South") {
      data <- data[data$gov_name == input$Governorate,]
      data<-data%>%
        dplyr::mutate(area = gov_name)%>%
        group_by(Date,area, org)%>%
        dplyr::summarise(total_obs=dplyr::n())%>%
        dplyr::select(-org)%>%
        spread(area,total_obs)
      
    }
    data
  }))
  
  output$Contested_Table <- DT::renderDataTable(DT::datatable({
    data <- dt_gov_cnt
    if (input$Governorate == "Contested") {
      data <- data[data$gov_name == input$Governorate,]
      data<-data%>%
        dplyr::mutate(area = gov_name)%>%
        group_by(Date,area, org)%>%
        dplyr::summarise(total_obs=dplyr::n())%>%
        dplyr::select(-org)%>%
        spread(area,total_obs)
      
    }
    data
  }))
  
}
# Run the application 
shinyApp(ui = ui, server = server)


#############
#Export
#############
#setwd("C:/Users/REACH_AO_YEMEN/Dropbox/REACH/YEM/YEM Assessment/YEM Cash and Markets/02_JMMI/Partner Tracker Output")
#title_Final <- paste0("./Partner_Tracker_Update-",Sys.Date(),".xlsx")
#write_xlsx(counts_by_org_per_JMMI,title_Final)
