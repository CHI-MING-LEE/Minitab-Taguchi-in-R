library(shiny)
library(shinydashboard)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "Taguchi" ,titleWidth = 250),
  
  dashboardSidebar(collapsed=T,
    sidebarMenu(
      menuItem("Taguchi", tabName = "taguchi", icon = icon("angellist"))
    )  
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }'))),
    tags$head(tags$style(HTML(
      '.myClass { 
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: "Georgia", Times, "Times New Roman", serif;
      padding: 0 15px;
      overflow: hidden;
      color: white;
      }
      '))),
    tabItems(
      tabItem(tabName = "taguchi",
      fluidRow(
        column(width = 3,
               shinydashboard::box(width = 12, title = "Create Taguchi Design", status = "primary", 
                 solidHeader = F, collapsible = TRUE,
                 radioButtons(inputId="type_of_design",label="Type of Design",
                              choices= c("2-Level Design (2-31 factors)","3-Level Design (2-13 factors)","4-Level Design (2-5 factors)",
                                        "5-Level Design (2-6 factors)","Mixed Level Design (2 to 26 factors)")),
                 
                 uiOutput("Num_of_factors"),
                 checkboxInput("add_signal", label = "Add a signal factor for dynamic characteristics", 
                               value = FALSE),
                 div(style="display:inline-block",actionButton(
                   inputId="show_design",label = "Availabel Designs"
                   ,style="color: #fff; background-color: #DC7633; border-color:#DC7633 ;font-weight:bold"
                 )),
                 div(style="display:inline-block",actionButton(
                   inputId="designs",label = "Designs"
                   ,style="color: #fff; background-color: #DC7633; border-color:#DC7633 ;font-weight:bold"
                 )),
                 br(),
                 br(),
                 uiOutput("signal_numeric"),
                 uiOutput("select_taguchi_table"),
                 uiOutput("signal_level"),
                 div(style="display:inline-block",uiOutput("revise_fac")),
                 div(style="display:inline-block",uiOutput("generate_taguchi_table"))
               )
        ),
                
        column(width = 9,
               shinydashboard::tabBox(width='800', height = "800"
                ,tabPanel("Taguchi Table", 
                          downloadButton("savetable", "Save Table",style="color: #fff; background-color: #2C3E50; border-color: #2C3E50 ;font-weight:bold"),
                          br(),
                          br(),
                          div(dataTableOutput("taguchi_table"), style = "font-size:120%"))
                ,tabPanel("Summary", div(verbatimTextOutput("summary"), tags$head(tags$style("#summary{color: black;font-size: 20px;font-weight:bold;}"))))
               )
        )
      )
     )
    )
  )
)
