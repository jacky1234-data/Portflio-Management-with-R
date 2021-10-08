# Import the library
#install.packages('rsconnect')
#library(rsconnect)
library(shiny)


# Create UI
shinyUI(
  fluidPage(
    titlePanel('Welcome to Portfolio Management/Bienvenue dans la gestion de portefeuille/
               歡迎來到投資組合管理/欢迎来到投资组合管理/ポートフォリオ管理へようこそ'),
    verticalLayout(selectInput('language','Please Select Langauge/Choisir la langue/
                               請選擇語言/请选择语言/言語を選びなさい',c('English','Français','繁體中文','简体中文','日本語')),
                   splitLayout(
                     uiOutput("upper_graph_color"),uiOutput("lower_graph_color")
                   ),
                   sidebarLayout(
                     sidebarPanel(verticalLayout(uiOutput("stock_language_choice")),
                                  uiOutput("date"),uiOutput("initial_worth"),uiOutput('portfolio_allocation'),
                                  sliderInput("Apple","Apple",min=1,max=100,value=25,step=1),
                                  sliderInput("Activisition","Activisition",min=1,max=100,value=25,step=1),
                                  sliderInput("Microsoft","Microsoft",min=1,max=100,value=25,step=1),
                                  sliderInput("IBM","IBM",min=1,max=100,value=25,step=1)),mainPanel(
                                    plotOutput("p1"),plotOutput("p2")
                                  ),position="right"
    )
  
    )
  )
)
