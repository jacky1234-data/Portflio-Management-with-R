#install.packages('rsconnect')
#library(rsconnect)
library(shiny)
library(ggplot2)
library(tidyverse)
#install.packages("showtext")
library(showtext)
showtext_auto()

# Setting up the language translation table

stock_lang <- c("English"="Select a Stock","Français"="Choisir le stock","繁體中文"="選擇股票","简体中文"="选择股票",
                '日本語'="株式を選択してください")
date_range_lang <- c("English"="Please Select Date from November 1999 to Sepember 2021","Français"="Veuillez sélectionner le date de novembre 1999 à septembre 2021",
                     "繁體中文"="選擇從1999年11月到2021年9月的日期",
                     "简体中文"="选择1999年11月到2021年9月的日期",
                     '日本語'="１９９９年１１月から２０２１年９月までの日付を選択してください")

worth_lang <- c("English"="What is your inital worth (USD)","Français"="Quel sont votre valeur initiale (dollar américain)",
                     "繁體中文"="你的初始財富是多少等值美元","简体中文"="你的初始财富是多少等值美元",
                     '日本語'="君の最初のアメリカドルと同じお金はいくらですか")
port_allocation_lang <- c("English"="Your Portfolio Arrangement","Français"="Votre combinaison de la portefeuille",
                          "繁體中文"="你的投資組合","简体中文"="你的投资组合",
                          '日本語'="君のポートフォリオ")
top_color_lang <- c("English"="The Color of the Upper Graph","Français"="La couleur de la ligne dans le graphique supérieur",
                    "繁體中文"="上方圖像的線條的顔色","简体中文"="上方图像的线条的颜色",
                    '日本語'="上のグラフの線の色")
bot_color_lang <- c("English"="The Color of the Lower Graph","Français"="La couleur de la ligne dans le graphique inférieur",
                    "繁體中文"="下方圖像的線條的顔色","简体中文"="下方图像的线条的颜色",
                    '日本語'="下のグラフの線の色")
Apple_price <- c("English"="Stock Price of Apple in USD","Français"="Le prix de le Apple en dollar américain",
               "繁體中文"="Apple的股價（美元）","简体中文"="Apple的股价（美元）",
               '日本語'="Appleのアメリカドルで株の値段")
Activision_price <- c("English"="Stock Price of Activisition in USD","Français"="Le prix de le Activisition en dollar américain",
               "繁體中文"="Activisition的股價（美元）","简体中文"="Activisition的股价（美元）",
               '日本語'="Activisitionのアメリカドルで株の値段")
Microsoft_price <- c("English"="Stock Price of Microsoft in USD","Français"="Le prix de le Microsoft en dollar américain",
               "繁體中文"="Microsoft的股價（美元）","简体中文"="Microsoft的股价（美元）",
               '日本語'="Microsoftのアメリカドルで株の値段")
IBM_price <- c("English"="Stock Price of IBM in USD","Français"="Le prix de le IBM en dollar américain",
               "繁體中文"="IBM的股價（美元）","简体中文"="IBM的股价（美元）",
               '日本語'="IBMのアメリカドルで株の値段")
port_price_lang <- c("English"="Portfolio Total Worth in USD","Français"="Valeur totale du portefeuille en dollar américain",
                "繁體中文"="投資組合總價值（美元）","简体中文"="投资组合总价值（美元）",
                '日本語'="アメリカドルでポートフォリオの総価値")
port_price_time_lang <- c("English"="Portfolio Total Worth in USD over Time","Français"="Valeur totale du portefeuille en dollar américain dans le temps",
                     "繁體中文"="投資組合總價值（美元）隨時間的變動","简体中文"="投资组合总价值（美元）随时间的变动",
                     '日本語'="時間の経過に伴うポートフォリオの総価値(アメリカドル)")
date_lang <- c("English"="Date","Français"="Le date",
                     "繁體中文"="日期","简体中文"="日期",
                     '日本語'="日付")
stock_price_lang <- c("English"="Stock Price in USD over Time","Français"="Le prix de l'action en dollar américain dans le temps",
                     "繁體中文"="股價（美元）隨時間的變動","简体中文"="股价（美元）随时间的变动",
                     '日本語'="時間の経過に伴う株の値段(アメリカドル)")

# Setting up colors in different languages

red <- c("Red","Rouge","紅色","红色","赤色")
yellow <- c("Yellow","Jaune","黃色","黄色","黄色")
green <- c("Green","Verte","綠色","绿色","緑色")
blue <- c("Blue","Bleu","藍色","蓝色","青色")
pink <- c("Pink","Rose","粉色","粉色","ピンク色")
orange <- c("Orange","Orange","橙色","橙色","オレンジ色")
purple <- c("Purple","Violette","紫色","紫色","紫色")

color_en <- c("Red","Yellow","Green","Blue","Pink","Orange","Purple")
color_fr <- c("Rouge","Jaune","Verte","Bleu","Rose","Orange","Violette")
color_zh_TW <- c("紅色","黃色","綠色","藍色","粉色","橙色","紫色")
color_zh_CN <- c("红色","黄色","绿色","蓝色","粉色","橙色","紫色")
color_jp <- c("赤色","黄色","緑色","青色","ピンク色","オレンジ色","紫色")

color_list <- data.frame("English"=color_en,"Français"=color_fr,"繁體中文"=color_zh_TW,"简体中文"=color_zh_CN,'日本語'=color_jp)
all_color <- data.frame("red"=red,"yellow"=yellow,"green"=green,"blue"=blue,"pink"=pink,"orange"=orange,"purple"=purple)

# Reading the data

normalized_data <- read.csv("Normalized Stock Price Table.csv")
original_data <- read.csv("Arranged Stock Price Table.csv")
normalized_data$Date <- as.Date(normalized_data$Date)
original_data$Date <- as.Date(original_data$Date)

# Defining function of new worth DataFrame based on user input about portfolio, initial worth, and date range

create_portfolio_table <- function(ini_worth,start_date,end_date,apple,activision,microsoft,ibm){
  filtered_data <- filter(original_data, Date>=as.Date(start_date),Date<=as.Date(end_date))
  worth_table <- data.frame("Date"=filtered_data$Date,"Total_Worth"=rep(0,nrow(filtered_data)))
  stock_list <- c("Apple","Activision","Microsoft","IBM")
  for (item in stock_list){
    ini_price <- as.numeric(filtered_data[item][c(1),])
    filtered_data[item] <- filtered_data[item]/ini_price
  }
  portfolio <- c(apple,activision,microsoft,ibm)
  portfolio <- portfolio/sum(portfolio)
  for (i in 1:nrow(worth_table)){
    worth <- 0
    for (j in 1:4){
      worth <- worth + ini_worth*portfolio[j]*filtered_data[c(i),j+2]
    }
    worth_table[c(i),c(2)] <- worth
  }
  return(worth_table)                          
  
}

# Designing the UI

shinyServer(
  function(input, output) {
    
# Setting the user inputs
    
    output$stock_language_choice <- renderUI({
      selectInput("stocks",stock_lang[input$language],c("Apple","Activision","Microsoft","IBM"))
    })
    output$date <- renderUI({
     dateRangeInput("date",date_range_lang[input$language],start=as.Date("1999-11-02"),end=as.Date("2021-09-25"),min=as.Date("1999-11-01"),max=as.Date("2021-09-27"))
    })
    output$initial_worth <- renderUI({
      numericInput("init_worth",worth_lang[input$language],value=1000,min=1,max=500000,step=1)
    })
    output$portfolio_allocation <- renderText({port_allocation_lang[input$language]
    })
    
# Separating cases based on user's language choices
    
    observeEvent(input$language,{
      if (input$language=="English"){
        output$upper_graph_color <- renderUI({
          radioButtons(inputId="upper_graph_color",label=top_color_lang[input$language],
                       choices=color_en,selected="Red")
        })
        output$lower_graph_color <- renderUI({
          radioButtons(inputId="lower_graph_color",label=bot_color_lang[input$language],
                       choices=color_en,selected="Red")
        }) 
        
      } else if (input$language=="Français"){
        output$upper_graph_color <- renderUI({
          radioButtons(inputId="upper_graph_color",label=top_color_lang[input$language],
                       choices=color_fr,selected="Rouge")
        })
        output$lower_graph_color <- renderUI({
          radioButtons(inputId="lower_graph_color",label=bot_color_lang[input$language],
                       choices=color_fr,selected="Rouge")
        }) 
        
      } else if (input$language=="繁體中文"){
        output$upper_graph_color <- renderUI({
          radioButtons(inputId="upper_graph_color",label=top_color_lang[input$language],
                       choices=color_zh_TW,selected="紅色")
        })
        output$lower_graph_color <- renderUI({
          radioButtons(inputId="lower_graph_color",label=bot_color_lang[input$language],
                       choices=color_zh_TW,selected="紅色")
        }) 
        
      } else if (input$language=='简体中文'){
        output$upper_graph_color <- renderUI({
          radioButtons(inputId="upper_graph_color",label=top_color_lang[input$language],
                       choices=color_zh_CN,selected="红色")
        })
        output$lower_graph_color <- renderUI({
          radioButtons(inputId="lower_graph_color",label=bot_color_lang[input$language],
                       choices=color_zh_CN,selected="红色")
        }) 
        
      } else if (input$language=='日本語'){
        output$upper_graph_color <- renderUI({
          radioButtons(inputId="upper_graph_color",label=top_color_lang[input$language],
                       choices=color_jp,selected="赤色")
        })
        output$lower_graph_color <- renderUI({
          radioButtons(inputId="lower_graph_color",label=bot_color_lang[input$language],
                       choices=color_jp,selected="赤色")
        }) 
      }
    })
    filtered_original_data <- reactive({
                              filter(original_data, Date>=as.Date(input$date[1]),Date<=as.Date(input$date[2]))})
    
# Plotting the graph based on stocks and facilitating the color 
    
    observeEvent(input$upper_graph_color,{
      for (i in 1:7){
        if (input$upper_graph_color %in% all_color[,c(i)]){
          upper_color <- names(all_color[i])
          break
        }
      }
      observeEvent(input$stocks, {
        if (input$stocks=="Apple"){
          output$p1 <- renderPlot({
            ggplot(data=filtered_original_data())+
              geom_line(aes(x=Date,y=Apple),color=upper_color)+
              labs(x=date_lang[input$language],y=Apple_price[input$language],title=stock_price_lang[input$language])
          })
          
        } else if (input$stocks=="Activision"){
          output$p1 <- renderPlot({
            ggplot(data=filtered_original_data())+
              geom_line(aes(x=Date,y=Activision),color=upper_color)+
              labs(x=date_lang[input$language],y=Activision_price[input$language],title=stock_price_lang[input$language])
              
          })
          
        } else if (input$stocks=="Microsoft"){
          output$p1 <- renderPlot({
            ggplot(data=filtered_original_data())+
              geom_line(aes(x=Date,y=Microsoft),color=upper_color)+
              labs(x=date_lang[input$language],y=Microsoft_price[input$language],title=stock_price_lang[input$language])
          })
        } else if (input$stocks=="IBM"){
          output$p1 <- renderPlot({
            ggplot(data=filtered_original_data())+
              geom_line(aes(x=Date,y=IBM),color=upper_color)+
              labs(x=date_lang[input$language],y=IBM_price[input$language],title=stock_price_lang[input$language])
          })
        }
        
      })
      
    })
    
    portfolio_worth <- reactive({
      create_portfolio_table(input$init_worth,input$date[1],input$date[2],input$Apple,
                             input$Activisition,input$Microsoft,input$IBM)
    })
    observeEvent(input$lower_graph_color,{
      for (i in 1:7){
        if (input$lower_graph_color %in% all_color[,c(i)]){
          lower_color <- names(all_color[i])
          break
        } 
      }
      output$p2 <- renderPlot({
        ggplot(data=portfolio_worth())+
          geom_line(aes(x=Date,y=Total_Worth),color=lower_color)+
          labs(x=date_lang[input$language],y=port_price_lang[input$language],title=port_price_time_lang[input$language])
      })
    })
  }
)
