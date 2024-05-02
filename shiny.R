##### set up #####
library(shiny)
library(sf)
library(ggplot2)
library(writexl)
library(dplyr)
library(shadowtext)
d = -40
l = -530

##### 載入地圖資料 #####
load("Incidence_data.RData")
load("map.RData")
shp <- taiwanmap[c(3, 5)]  #臺灣地圖

###### 將結果印出來(圖片、樣站) #####
map_plot_result <- function(data, river_no) {
  
  id <- list()
  
  for(i in river_no) {
    id[[i]] <- match(data[[i]], data_l$locationID)
  }
  
  all_id <- unlist(id)
  data_loc <- data_l[all_id,]  
  data_loc <- na.omit(data_loc)
  
  data_loc$BA_NO <- substr(data_loc$locationID, 1, 4)
  merged_data <- merge(data_loc, rv3, by.x = "BA_NO", by.y = "BA_NO")
  
  return(merged_data)
}

##### UI #####
ui <- shinyUI(fluidPage(
  # set up  
    {tags$head(
        tags$meta(name="viewport", content="width = device-width, initial-scale=1.0"),
        tags$title("河川情勢調查"),
        tags$style("
        body {
          display: flex;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 100%;
        }
    
        .paper{
          display: flex
        }
    
        .main{
          flex: none;
          width : 30%
        }
    
        .result{
          flex : auto;
        }
    
        @media screen and (min-aspect-ratio: 1.9) {
          body {
            transform: scale(1.4);
          }
        }
    
    
        @media screen and (max-aspect-ratio: 1.9) {
          body {
            transform: scale(1);
          }
        }
      ")
    )}
    ,
    ##標題
    titlePanel({

      tags$div(

        style = "text-align: center; font-weight: bold; position: relative; z-index: 9999;
        text-shadow: 1px 1px 2px #000;color: Black",
        HTML("<span style='font-size: 80%; font-weight: bold;'>特有生物中心</span>"),
        br(),
        HTML("<span style='font-weight: bold; font-size: 120%;'>河川情勢調查</span>")
      )

    })
    ,
    mainPanel(
      # 輸入
      {# main
      tags$div(
        class = "main",
        
        #### 種類
        absolutePanel(
          id = "category",
          draggable = TRUE,  
          top = 30,         
          left = l,
          width = 100,       
          height = 50,  
          selectInput("category", label = h4("種類"), 
                      choices = list("魚類" = 1,"蝦蟹類" = 2,"昆蟲類" = 3,
                                     "螺貝類" = 4,"環節動物" = 5,"藻類" = 6,
                                     "植物" = 7,"哺乳類" = 8,"鳥類" = 9,
                                     "爬蟲類" = 10,"兩棲類" = 11),
                      selected = 1)
        ),
        
        #### 下載檔案
        
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 200,         
          left = l,
          width = 100,       
          height = 50,  
          actionButton("btn_download", "Download")
        ),
        
        ####臺灣地圖
        absolutePanel(
          id = "button_panel",
          draggable = F,  
          top = 62,         
          left = l + 140,        
          width = 1000,       
          height = 1000,       
          
          img(src = "taiwan.png", width = "30%", height = "60%")
          
        ),
        
        ####按鈕位置、大小
        #磺溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 55,         
          left = l+ 435 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_1", rv3$RV_NAME[1], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #淡水河
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 65,         
          left = l+ 320 +d,        
          width = 8,       
          height = 8,       
          actionButton("btn_2", rv3$RV_NAME[2], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #鳳山溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 95,         
          left = l+ 265 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_3", rv3$RV_NAME[3], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #頭前溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 127,         
          left = l+ 250 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_4", rv3$RV_NAME[4], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #中港溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 156,         
          left = l+ 225 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_5", rv3$RV_NAME[5], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #後龍溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 185,         
          left = l+ 215 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_6", rv3$RV_NAME[6], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #大安溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 214,         
          left = l+ 190 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_7", rv3$RV_NAME[7], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #大甲溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 243,         
          left = l+ 175 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_8", rv3$RV_NAME[8], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #烏溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 272,         
          left = l+ 160 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_9", rv3$RV_NAME[9], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #濁水溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 301,         
          left = l+ 140 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_10", rv3$RV_NAME[10], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #北港溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 330,         
          left = l+ 130 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_11", rv3$RV_NAME[11], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #朴子溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 359,         
          left = l+ 125 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_12", rv3$RV_NAME[12], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #八掌溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 388,         
          left = l+ 125 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_13", rv3$RV_NAME[13], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #急水溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 417,         
          left = l+ 115 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_14", rv3$RV_NAME[14], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #曾文溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 446,         
          left = l+ 115 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_15", rv3$RV_NAME[15], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #鹽水溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 475,         
          left = l+ 130 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_16", rv3$RV_NAME[16], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #二仁溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 504,         
          left = l+ 145 +d,        
          width = 10,       
          height = 8,       
          actionButton("btn_17", rv3$RV_NAME[17], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #阿公店溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 533,         
          left = l+ 150 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_18", rv3$RV_NAME[18], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #高屏溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 562,         
          left = l+ 175 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_19", rv3$RV_NAME[19], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #東港溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 591,         
          left = l+ 200 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_20", rv3$RV_NAME[20], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #四重溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 620,         
          left = l+ 220 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_21", rv3$RV_NAME[21], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #卑南溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 470,         
          left = l+ 380 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_22", rv3$RV_NAME[22], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #秀姑巒溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 380,         
          left = l+ 415 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_23", rv3$RV_NAME[23], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                     
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #花蓮溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 310,         
          left = l+ 440 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_24", rv3$RV_NAME[24], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #和平溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 230,         
          left = l+ 460 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_25", rv3$RV_NAME[25], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        ),
        #蘭陽溪
        absolutePanel(
          id = "button_panel",
          draggable = TRUE,  
          top = 170,         
          left = l+ 465 +d,        
          width = 10,       
          height = 10,       
          actionButton("btn_26", rv3$RV_NAME[26], style = "font-size: 10px; font-weight: bold; box-shadow: 1px 1px 1px lightgray;                      
                         background-color: #FF8000; color: white; border: none; text-shadow: 1px 1px 2px #000;")
        )
      )}
      ,

      # 輸出
      {tags$div(
      class = "result",
      absolutePanel(
        id = "button_panel",
        draggable = F,  
        top = 10,         
        left = l + 550,        
        width = 700,       
        height = "auto",
        tabsetPanel(type = "pills",
                    tabPanel("Plot", uiOutput("river_info")),
                    tabPanel("Summary", print("summary"))
                      ) 
                         )
  )}
  )
  )
)

##### server #####
server <- shinyServer(function(input, output, session) {
  # summary
  {
    output$category <- renderPrint({input$category})
  }
  
  # Map 當按按鈕時印出結果
  {lapply(1:26, function(i) {
    
    observeEvent(input[[paste0("btn_", i)]], {
      
      
      img <- img(src = paste0(i,".jpeg"), width = "100%", height = "100%",
                 style = "box-shadow: 4px 4px 4px #A3D1D1; border: 2px solid 		#81C0C0;")
      
      
      output$river_info <- renderUI({
        merged_data <- map_plot_result(result_fish1,i)
        station_names <- data.frame()
        river_name <- rv3$RV_NAME[which(rv3$NO == i)]
        station_names <- merged_data$locality
        info_text <- paste0(
          "<div style='font-size: 22px; text-align: center; font-weight: bold;
          color: black;text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;'>",
          "<b>",river_name,"樣站資訊","</b>", "<br>","<br>",
          
          "<div style='font-size: 18px;  font-weight: bold; text-align: left;
          color: black;text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;'>",
          "<b>挑選樣站: </b>","<br>",
          
          "<div style='text-align: left; font-size: 16px; font-weight: bold;
          color: black;text-shadow: 1px 1px 2px #000;'>",
          paste(station_names, collapse = "、 "),"<br>",
          "</div>",
          "</div>",
          "</div>",
          img 
        )
        HTML(info_text)
      })
      
      
    })
  })}
})

##### shinyApp #####
shinyApp(ui, server)
