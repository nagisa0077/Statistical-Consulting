library(shiny)
library(sf)
library(ggplot2)

load("map.RData")
d = -80
shinyUI(fluidPage(
  titlePanel(title = span(
    style = "display: flex; align-items: center; justify-content: center; font-weight: bold;",
    "Taiwan River Ecology"
  )),
  
    mainPanel(
      
      absolutePanel(
        id = "button_panel",
        draggable = F,  
        top = 50,         
        left = -20,        
        width = 300,       
        height = 1000,       
        
        img(src = "allmap.png", width = "450px", height = "550px")
        
      ),
      #磺溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 55,         
        left = 400 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_1", rv3$RV_NAME[1], style = "font-size: 8px; font-weight: bold;")
      ),
      #淡水河
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 56,         
        left = 330 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_2", rv3$RV_NAME[2], style = "font-size: 8px; font-weight: bold;")
      ),
      #鳳山溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 82,         
        left = 290 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_3", rv3$RV_NAME[3], style = "font-size: 8px; font-weight: bold;")
      ),
      #頭前溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 108,         
        left = 250 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_4", rv3$RV_NAME[4], style = "font-size: 8px; font-weight: bold;")
      ),
      #中港溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 134,         
        left = 230 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_5", rv3$RV_NAME[5], style = "font-size: 8px; font-weight: bold;")
      ),
      #後龍溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 160,         
        left = 215 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_6", rv3$RV_NAME[6], style = "font-size: 8px; font-weight: bold;")
      ),
      #大安溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 186,         
        left = 200 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_7", rv3$RV_NAME[7], style = "font-size: 8px; font-weight: bold;")
      ),
      #大甲溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 212,         
        left = 180 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_8", rv3$RV_NAME[8], style = "font-size: 8px; font-weight: bold;")
      ),
      #烏溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 238,         
        left = 170 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_9", rv3$RV_NAME[9], style = "font-size: 8px; font-weight: bold;")
      ),
      #濁水溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 264,         
        left = 150 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_10", rv3$RV_NAME[10], style = "font-size: 8px; font-weight: bold;")
      ),
      #北港溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 290,         
        left = 140 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_11", rv3$RV_NAME[11], style = "font-size: 8px; font-weight: bold;")
      ),
      #朴子溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 316,         
        left = 130 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_12", rv3$RV_NAME[12], style = "font-size: 8px; font-weight: bold;")
      ),
      #八掌溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 342,         
        left = 120 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_13", rv3$RV_NAME[13], style = "font-size: 8px; font-weight: bold;")
      ),
      #急水溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 368,         
        left = 120 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_14", rv3$RV_NAME[14], style = "font-size: 8px; font-weight: bold;")
      ),
      #曾文溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 394,         
        left = 120 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_15", rv3$RV_NAME[15], style = "font-size: 8px; font-weight: bold;")
      ),
      #鹽水溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 420,         
        left = 120 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_16", rv3$RV_NAME[16], style = "font-size: 8px; font-weight: bold;")
      ),
      #二仁溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 446,         
        left = 130 +d,        
        width = 10,       
        height = 8,       
        actionButton("btn_17", rv3$RV_NAME[17], style = "font-size: 8px; font-weight: bold;")
      ),
      #阿公店溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 472,         
        left = 140 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_18", rv3$RV_NAME[18], style = "font-size: 8px; font-weight: bold;")
      ),
      #高屏溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 498,         
        left = 160 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_19", rv3$RV_NAME[19], style = "font-size: 8px; font-weight: bold;")
      ),
      #東港溪
      absolutePanel(
        id = "button_panel",
        draggable = TRUE,  
        top = 524,         
        left = 200 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_20", rv3$RV_NAME[20], style = "font-size: 8px; font-weight: bold;")
      ),
      #四重溪
      absolutePanel(
        
        id = "button_panel",
        draggable = TRUE,  
        top = 560,         
        left = 200 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_21", rv3$RV_NAME[21], style = "font-size: 8px; font-weight: bold;")
      ),
      #卑南溪
      absolutePanel(
        
        id = "button_panel",
        draggable = TRUE,  
        top = 450,         
        left = 355 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_22", rv3$RV_NAME[22], style = "font-size: 8px; font-weight: bold;")
      ),
      #秀姑巒溪
      absolutePanel(
        
        id = "button_panel",
        draggable = TRUE,  
        top = 350,         
        left = 385 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_23", rv3$RV_NAME[23], style = "font-size: 8px; font-weight: bold;")
      ),
      #花蓮溪
      absolutePanel(
      
        id = "button_panel",
        draggable = TRUE,  
        top = 290,         
        left = 405 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_24", rv3$RV_NAME[24], style = "font-size: 8px; font-weight: bold;")
      ),
      #和平溪
      absolutePanel(
        
        id = "button_panel",
        draggable = TRUE,  
        top = 230,         
        left = 425 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_25", rv3$RV_NAME[25], style = "font-size: 8px; font-weight: bold;")
      ),
      #蘭陽溪
      absolutePanel(
        
        id = "button_panel",
        draggable = TRUE,  
        top = 170,         
        left = 445 +d,        
        width = 10,       
        height = 10,       
        actionButton("btn_26", rv3$RV_NAME[26], style = "font-size: 8px; font-weight: bold;")
      ),
      

      absolutePanel(
        
        id = "button_panel",
        draggable = F,  
        top = 50,         
        left = 430,        
        width = 500,       
        height = 1000,       
        uiOutput("river_info"),
        br(),
        plotOutput(outputId = "river_plot")
      )
      
      
      
    )
  
))


