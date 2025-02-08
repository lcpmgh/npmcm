# shiny获奖信息可视化程序
# Version:
#          v2: 2021.05.01
#          v3: 2022.01.29   Switch to REmap package (from rgdal package)

##### 1. load packages #####
library(shiny)
library(showtext)
library(data.table)
library(magrittr)
library(ECharts2Shiny)
library(shinyWidgets)
library(ggplot2)
# library(REmap)
library(DT)
library(echarts4r)
library(echarts4r.maps)
library(jsonlite)


##### 2. ui function #####
ui <- fluidPage(
  tags$head(tags$title("NPMCM")),
  tags$head(tags$link(rel = "shortcut icon", href = "pmgh.ico")),
  loadEChartsLibrary(),
  loadEChartsTheme("shine"),
  titlePanel(title = "中国研究生数学建模竞赛获奖信息可视化系统"),
  tabsetPanel(id = "mainpanel",
              type = 'pills',
              tabPanel(title = '获奖名单', sidebarLayout(sidebarPanel(uiOutput("ui_p11")), mainPanel(uiOutput("ui_p12")))),
              tabPanel(title = '按队伍', uiOutput("ui_p2")),
              tabPanel(title = '按个人', uiOutput("ui_p31"), uiOutput("ui_p32")),
              tabPanel(title = '个人连续获奖', uiOutput("ui_p41"), uiOutput("ui_p42"))
              ),
  tags$hr(),
  tags$div(align = "center", style = "margin-bottom: 10px;",
           tags$p("\ua9 2021-2024, Lcpmgh, All rights reserved.", style="height:8px"),
           tags$div(align = "center",
                    actionLink(inputId = "", label = "lcpmgh ", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh')"),
                    tags$p("  ", style = "display:inline;white-space:pre"),
                    actionLink(inputId = "", label = "lcpmgh@gmail.com", icon = icon("envelope"), onclick ="window.location.href='mailto:lcpmgh@gmail.com'"),
                    tags$p("  ", style = "display:inline;white-space:pre"),
                    actionLink(inputId = "", label = "lcpmgh.com", icon = icon("home"), onclick ="window.location.href='http://lcpmgh.com/'")
           ),
           tags$div(align = "center",
                    tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                    tags$p("  ", style = "display:inline;white-space:pre"),
                    # tags$img(src="gaba.png"),
                    tags$a("川公网安备51010702002736", target="_blank", href="http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", style="color:#06c; display:inline;")
           )
  )
)
