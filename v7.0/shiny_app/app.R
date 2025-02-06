# shiny获奖信息可视化程序
# Version:
#          v2: 2021.05.01
#          v3: 2022.01.29   Switch to REmap package (from rgdal package)
#          v4：2025.02.06   大改，简化包、更改布局、添加单位历年数据等

##### packages #####
library(shiny)
library(echarty)
library(data.table)
library(showtext)
library(magrittr)
library(htmlwidgets)
library(shinyWidgets)
library(DT)
library(jsonlite)

##### ui #####
ui <- fluidPage(
  # Header
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tags$head(tags$title("npmcm")),
  tags$head(tags$link(rel = "shortcut icon", href = "pmgh.ico")),
  # Body
  h2("中国研究生数学建模竞赛获奖统计",  style = "margin-top:15px; margin-bottom:15px; font-weight:bold; color:#337ab7"),
  tabsetPanel(id = "mainPanel",
              type = 'pills',
              tabPanel(title = "按参赛队伍",
                       wellPanel(id = "page1", uiOutput("ui_p1_main"))
              ),
              tabPanel(title = "按培养单位",
                       div(class = "flex-container",
                           wellPanel(id = "pageL2", uiOutput("ui_p2_sidebar")), 
                           wellPanel(id = "pageR2", uiOutput("ui_p2_main"))
                       )),
              tabPanel(title = "个人连续获奖",
                       div(class = "flex-container",
                           wellPanel(id = "pageL3", uiOutput("ui_p3_sidebar")),
                           wellPanel(id = "pageR3", uiOutput("ui_p3_main"))
                       )),
              tabPanel(title = "获奖名单",
                       div(class = "flex-container",
                           wellPanel(id = "pageL4", uiOutput("ui_p4_sidebar")),
                           wellPanel(id = "pageR4", uiOutput("ui_p4_main"))
                       ))
  ),
  tags$hr(),
  tags$div(align = "center", style = "margin-bottom: 10px;",
           tags$p("\ua9 2021-2025, Lcpmgh, All rights reserved.", style="height:8px"),
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


##### server ##### 
server <- function(input, output, session){
  ##### 准备部分 #####
  # font
  font_add("myFont", './data/0-typeface.ttc')    
  showtext_auto()
  # functions
  my_read_csv <- function(file_name, ...){
    # For data reading
    data <- fread(paste0('./data/', file_name), encoding = 'UTF-8', ...)
    return(data)
  }
  trans <- function(x){
    # For legends sorting
    y1 <- y2 <- y3 <- NA_character_
    if('一等奖' %in% x) y1 <- '一等奖'
    if('二等奖' %in% x) y2 <- '二等奖'
    if('三等奖' %in% x) y3 <- '三等奖'
    y <- paste(na.omit(c(y1, y2, y3)), collapse = '、')
    return(y)
  }
  to_list_data <- function(df){
    # ec.int画地图，将df转为list
    # df第一列必须为provi，第二列必须为value
    data_list <- lapply(1:nrow(df), function(i) {
      list(name = df$provi[i], value = df$value[i])
    })
    return(data_list)
  }
  # data
  data_team   <- my_read_csv("2-data_total.csv") %>% .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖','成功参与奖'), order = T)]
  data_member <- my_read_csv("5-data_member.csv") %>% .[, A_type:=factor(A_type, levels = c('一等奖','二等奖','三等奖'), order = T)]
  unit_unique <- data_member$Unit %>% unique() %>% stringi::stri_sort(locale = "zh", numeric = TRUE) 
  geojson <- read_json("./data/0-china_province_simplified.json")
  data_province <- data.frame(provi = c('安徽', '澳门', '北京', '福建', '甘肃', '广东', '广西', '贵州', '海南', '河北', 
                                        '河南', '黑龙江', '湖北', '湖南', '吉林', '江苏', '江西', '辽宁', '内蒙古', '宁夏', 
                                        '青海', '山东', '山西', '陕西', '上海', '四川', '台湾', '天津', '西藏', '香港', '新疆',
                                        '云南', '浙江', '重庆'))
  
  
  ##### p1 按队伍 #####
  # p1，UI
  output$ui_p1_main <- renderUI({
    tagList(
      h3("参赛队伍数量"),
      ecs.output("p1_plot_count"),
      h3("获奖概率"),
      ecs.output("p1_plot_rate"),
      h3("获奖数量（按奖项）"),
      ecs.output("p1_plot_count_atype"),
      h3("获奖数量（按题型）"),
      ecs.output("p1_plot_count_qtype"),
      h3("获奖构成（奖项占比）"),
      ecs.output("p1_plot_rate_atype"),
      h3("获奖构成（题型占比）"),
      ecs.output("p1_plot_rate_qtype"),
    )
  })
  
  # p1图1，参赛数量
  p1_data1 <- data_team %>%
    dcast(Year~A_type, value.var = 'Year', fun = length) %>%
    .[, count:=apply(.SD, 1, sum), .SDcols=2:5] %>% 
    set_colnames(c("Year", "award1", "award2", "award3", "award4", "count")) %>% 
    .[award4==0, count:=NA_integer_]
  output$p1_plot_count <- ecs.render({
    ec.init(
      series = list(
        list(type = "line", name = "参赛队伍量", data = p1_data1$count, 
             label = list(normal = list(show=TRUE, position="top")))
      ),
      xAxis = list(type = "category", 
                   data = p1_data1$Year,
                   name="年份",
                   boundaryGap = FALSE,   #横坐标点对应，而非中心对应
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))   
      ),
      yAxis = list(type = "value", 
                   name="参赛队伍量",
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")  
      ),
      legend = list(show = F)   
    )
  })
  
  # p1图2，获奖概率
  p1_data2 <- copy(p1_data1) %>% 
    .[, c("award1r", "award2r", "award3r", "awardr"):=
        list(round(award1/count*100, 2), 
             round(award2/count*100, 2),
             round(award3/count*100, 2),
             round((count-award4)/count*100, 2)
        )]
  output$p1_plot_rate <- ecs.render({
    ec.init(
      series = list(
        list(type = "line", stack = 'total', areaStyle = "", name = "一等奖", data = p1_data2$award1r),
        list(type = "line", stack = 'total', areaStyle = "", name = "二等奖", data = p1_data2$award2r),
        list(type = "line", stack = 'total', areaStyle = "", name = "三等奖", data = p1_data2$award3r),
        list(type = "line", name = "综合获奖率", data = p1_data2$awardr, 
             label = list(
               normal = list(
                 show = TRUE, 
                 position = "top",
                 formatter = htmlwidgets::JS("
                   function(params) {
                       if (isNaN(params.value) || params.value === null) {
                         return ''; // 如果值是 NA 或 null，返回空字符串
                       }
                       return params.value.toFixed(2) + '%'; // 直接添加百分号
                     }
                  ")
               )
             )
        )
      ),
      xAxis = list(type = "category", 
                   data = p1_data2$Year,
                   name="年份",
                   boundaryGap = FALSE,
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))   
      ),
      yAxis = list(type = "value", 
                   name = "获奖概率",
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisLabel = list(
                     formatter = '{value}%'  # 将 y 轴标签显示为百分比
                   )
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                let value = item.value.toFixed(2) + '%';
                result += item.marker + ' ' + item.seriesName + ': ' + value + '<br>';
              }
            });
            return result;
          }
       ")
      ),
      legend = list(show = T)   
    )
  })
  
  # p1图3，获奖数量（按奖项）
  p1_data3 <- copy(p1_data1) %>% 
    .[, count123:=award1+award2+award3]
  output$p1_plot_count_atype <- ecs.render({
    ec.init(
      series = list(
        list(type = "line", stack = 'total', areaStyle = "", name = "一等奖", data = p1_data3$award1),
        list(type = "line", stack = 'total', areaStyle = "", name = "二等奖", data = p1_data3$award2),
        list(type = "line", stack = 'total', areaStyle = "", name = "三等奖", data = p1_data3$award3),
        list(type = "line", name = "获奖总数", data = p1_data3$count123, 
             label = list(
               normal = list(
                 show = TRUE, 
                 position = "top",
                 formatter = htmlwidgets::JS("
                   function(params) {
                       if (isNaN(params.value) || params.value === null) {
                         return ''; // 如果值是 NA 或 null，返回空字符串
                       }
                       return params.value
                     }
                  ")
               )
             )
        )
      ),
      xAxis = list(type = "category", 
                   data = p1_data3$Year,
                   name="年份",
                   boundaryGap = FALSE,
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))   
      ),
      yAxis = list(type = "value", 
                   name = "获奖数量",
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")
      ),
      legend = list(show = T)   
    )
  })
  
  # p1图4，获奖数量（按题型）
  p1_data4 <- data_team[A_type != "成功参与奖",] %>%
    dcast(Year~Q_type, value.var = 'Year', fun = length) %>% 
    .[, countall:=apply(.SD, 1, sum), .SDcols=2:8]
  output$p1_plot_count_qtype <- ecs.render({
    ec.init(
      series = list(
        list(type = "line", stack = 'total', areaStyle = "", name = "A", data = p1_data4$A),
        list(type = "line", stack = 'total', areaStyle = "", name = "B", data = p1_data4$B),
        list(type = "line", stack = 'total', areaStyle = "", name = "C", data = p1_data4$C),
        list(type = "line", stack = 'total', areaStyle = "", name = "D", data = p1_data4$D),
        list(type = "line", stack = 'total', areaStyle = "", name = "E", data = p1_data4$E),
        list(type = "line", stack = 'total', areaStyle = "", name = "F", data = p1_data4$F),
        list(type = "line", stack = 'total', areaStyle = "", name = "N（未分题型）", data = p1_data4$N),
        list(type = "line", name = "获奖总数", data = p1_data4$countall, 
             label = list(
               normal = list(
                 show = TRUE, 
                 position = "top",
                 formatter = htmlwidgets::JS("
                   function(params) {
                       if (isNaN(params.value) || params.value === null) {
                         return ''; // 如果值是 NA 或 null，返回空字符串
                       }
                       return params.value
                     }
                  ")
               )
             )
        )
      ),
      xAxis = list(type = "category", 
                   data = p1_data3$Year,
                   name="年份",
                   boundaryGap = FALSE,
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))   
      ),
      yAxis = list(type = "value", 
                   name = "获奖数量",
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
  ")
      ),
      legend = list(show = T)   
    )
  })
  
  # p1图5，获奖构成（奖项占比）
  p1_data5 <- copy(p1_data3) %>% 
    .[, c("award1p", "award2p", "award3p"):=
        list(round(award1/count123*100,2),
             round(award2/count123*100,2), 
             round(award3/count123*100,2))]
  output$p1_plot_rate_atype <- ecs.render({
    ec.init(
      series = list(
        list(type = "bar", stack = 'total', areaStyle = "", name = "一等奖", data = p1_data5$award1p),
        list(type = "bar", stack = 'total', areaStyle = "", name = "二等奖", data = p1_data5$award2p),
        list(type = "bar", stack = 'total', areaStyle = "", name = "三等奖", data = p1_data5$award3p)
      ),
      xAxis = list(type = "category", 
                   data = p1_data3$Year,
                   name="年份",
                   boundaryGap = T,
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisTick = list(alignWithLabel = TRUE)
      ),
      yAxis = list(type = "value", 
                   name = "获奖构成",
                   max = 100,
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisLabel = list(formatter = '{value}%')
      ),
      tooltip = list(
        trigger = "axis",
        axisPointer = list(type = 'shadow'),
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                let value = item.value.toFixed(2) + '%';
                result += item.marker + ' ' + item.seriesName + ': ' + value + '<br>';
              }
            });
            return result;
          }
        ")
      ),
      legend = list(show = T)   
    )
  })
  
  # p1图6，获奖构成（题型占比）
  p1_data6 <- copy(p1_data4) %>% 
    .[, c("Ap", "Bp", "Cp", "Dp", "Ep", "Fp", "Np"):=
        list(round(A/countall*100, 2), 
             round(B/countall*100, 2), 
             round(C/countall*100, 2),
             round(D/countall*100, 2), 
             round(E/countall*100, 2), 
             round(F/countall*100, 2),
             round(N/countall*100, 2))]
  output$p1_plot_rate_qtype <- ecs.render({
    ec.init(
      series = list(
        list(type = "bar", stack = 'total', areaStyle = "", name = "A", data = p1_data6$Ap),
        list(type = "bar", stack = 'total', areaStyle = "", name = "B", data = p1_data6$Bp),
        list(type = "bar", stack = 'total', areaStyle = "", name = "C", data = p1_data6$Cp),
        list(type = "bar", stack = 'total', areaStyle = "", name = "D", data = p1_data6$Dp),
        list(type = "bar", stack = 'total', areaStyle = "", name = "E", data = p1_data6$Ep),
        list(type = "bar", stack = 'total', areaStyle = "", name = "F", data = p1_data6$Fp),
        list(type = "bar", stack = 'total', areaStyle = "", name = "N（未分题型）", data = p1_data6$Np)
      ),
      xAxis = list(type = "category",
                   data = p1_data6$Year,
                   name="年份",
                   boundaryGap = T,
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisTick = list(alignWithLabel = TRUE)
      ),
      yAxis = list(type = "value",
                   name = "获奖构成",
                   max = 100,
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisLabel = list(formatter = '{value}%')
      ),
      tooltip = list(
        trigger = "axis",
        axisPointer = list(type = 'shadow'),
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                let value = item.value.toFixed(2) + '%';
                result += item.marker + ' ' + item.seriesName + ': ' + value + '<br>';
              }
            });
            return result;
          }
        ")
      ),
      legend = list(show = T)
    )
  })
  
  ##### p2 按培养单位 #####
  # p2，UI
  output$ui_p2_sidebar <- renderUI({
    tagList(
      h4("数据筛选"),
      awesomeCheckboxGroup(inputId = "p2_input_Atype", 
                           label = "获奖类型（图1、2、3）",
                           choices = c("一等奖", "二等奖", "三等奖"),
                           selected = c("一等奖", "二等奖", "三等奖"), 
                           inline = T),
      pickerInput(inputId = "p2_input_Sunit",
                  label = "培养单位（图1）", 
                  choices = unit_unique,
                  selected = "安徽财经大学",
                  multiple = FALSE,
                  options = pickerOptions(container = "body", liveSearch = TRUE),
                  width = "100%"),
      sliderTextInput(inputId = "p2_input_Ditem", 
                      label = "显示数量（图2）", 
                      choices = 1:30, 
                      selected = 10),
      sliderTextInput(inputId = "p2_input_Iyear", 
                      label = "年份区间（图2、3）", 
                      choices = sort(unique(data_member$Year)), 
                      selected = c(min(data_member$Year), max(data_member$Year)))
    )
  })
  output$ui_p2_main <- renderUI({
    tagList(
      h3(sprintf('%s 历年获奖人次', input$p2_input_Sunit)),
      ecs.output("p2_plot_Aunitaward"),
      h3('获奖人次最多的培养单位'), 
      ecs.output("p2_plot_Atimes", height = max("400px", input$p2_input_Ditem*30)),  #动态调整图高度
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s，展示前%d项）', 
                 input$p2_input_Iyear[1], input$p2_input_Iyear[2], trans(input$p2_input_Atype), input$p2_input_Ditem)),
      h3("各省获奖人次"),
      ecs.output("p2_plot_Pmember", height = "800px", width = "100%"),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', 
                 input$p2_input_Iyear[1], input$p2_input_Iyear[2], trans(input$p2_input_Atype))),
    )
  })
  
  # p2图1，选定单位历年获奖人次
  output$p2_plot_Aunitaward <- ecs.render({
    if (length(input$p2_input_Atype) == 0) {
      sAtype <- c("一等奖", "二等奖", "三等奖")
    } else {
      sAtype <- input$p2_input_Atype
    }
    sUnit <- input$p2_input_Sunit
    tdata <- data.table(Year=2004:max(data_member$Year))
    p2_data1 <- data_member[, c('Unit', 'Year', 'A_type')] %>%
      .[Unit == sUnit & A_type %in% sAtype, ] %>%
      .[, .(Freq = .N), by = .(Year)] %>% 
      .[tdata, on = "Year"] 
    ec.init(
      series = list(
        list(type = "line", name = "获奖人次", data = p2_data1$Freq, 
             label = list(normal = list(show=TRUE, position="top")))
      ),
      xAxis = list(type = "category", 
                   data = p2_data1$Year,
                   name="年份",
                   boundaryGap = FALSE,   #横坐标点对应，而非中心对应
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)), 
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))   
      ),
      yAxis = list(type = "value", 
                   name="获奖人次",
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")  
      ),
      legend = list(show = F)   
    )
  })
  
  # p2图2，获奖人次
  output$p2_plot_Atimes <- ecs.render({
    if (length(input$p2_input_Atype) == 0) {
      sAtype <- c("一等奖", "二等奖", "三等奖")
    } else {
      sAtype <- input$p2_input_Atype
    }
    p2_data2 <- data_member[, c('Unit', 'Year', 'A_type')] %>%
      .[A_type %in% sAtype & Year>=input$p2_input_Iyear[1] & Year<=input$p2_input_Iyear[2], ] %>%
      .[, .(Freq = .N), by = .(Unit)] %>%
      setorder(-Freq) %>%
      .[1:min(nrow(.), input$p2_input_Ditem), ] %>% 
      setorder(Freq)
    if (nrow(p2_data2) == 1) {
      p2_data2 <- rbind(data.table(Unit = "(占位项)", Freq = NA), p2_data2)   #避免只有一行数据时，画图出错
    }   
    ec.init(
      series = list(
        list(type = "bar", name = "获奖人次", data = p2_data2$Freq, 
             label=list(show = TRUE, position = "right", fontSize = 13, color = "#5470C6"))
      ),
      xAxis = list(type = "value",
                   name = "获奖人次",
                   nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black"))
      ),
      yAxis = list(type = "category",
                   data = p2_data2$Unit,
                   name="培养单位",
                   # boundaryGap = FALSE,
                   # nameLocation = "middle",
                   nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                   axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                   axisTick = list(alignWithLabel = TRUE)
      ),
      tooltip = list(
        trigger = "axis",
        axisPointer = list(type = 'shadow'),
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")
      ),
      legend = list(show = F)
    )
  })
  
  # p2图3，各省获奖人次
  output$p2_plot_Pmember <- ecs.render({
    if (length(input$p2_input_Atype) == 0) {
      sAtype <- c("一等奖", "二等奖", "三等奖")
    } else {
      sAtype <- input$p2_input_Atype
    }
    p2_data3 <- copy(data_member) %>% 
      .[A_type %in% sAtype & Year>=input$p2_input_Iyear[1] & Year<=input$p2_input_Iyear[2], ] %>% 
      .[, Province] %>%                       
      table()
    tdf_plot <- if (length(p2_data3) == 0) {
      data.frame(provi = "NA", value = 0)
    } else {
      data.table(provi = names(p2_data3), value = as.vector(p2_data3))
    }
    df_plot <- tdf_plot[data_province, on="provi"] %>% .[is.na(value), value:=0]
    
    # 画图
    ecplot <- ec.init(
      tooltip = list(
        trigger = "item",
        formatter = htmlwidgets::JS("
      function(params) {
        return params.name + ': ' + params.value + '人次';
      }
    ")
      ),
      toolbox = list(
        show = TRUE,
        orient = "vertical",  # 设置为垂直
        left = "left",       # 设置图例位置在右侧
        top = "45%",          # 设置距离顶部的距离
        feature = list(
          dataView = list(readOnly = FALSE),
          restore = list(),
          saveAsImage = list()
        )
      ),
      visualMap = list(
        min = 0,
        max = max(df_plot$value),
        # inRange = list(color = c("#ECF9FF", "#5470C6")),  # 调整颜色范围
        inRange = list(color = c("lightskyblue", "yellow", "orangered")),
        orient = "vertical",  # 垂直显示
        right = "100px",  # 设置右侧的偏移
        top = "45%",  # 设置距离顶部的偏移
        itemWidth = 20,  # 设置每个项的宽度
        itemHeight = 250,  # 设置每个项的高度
        calculable = TRUE  # 可计算范围
      ),
      series = list(
        list(
          name = "china-map",
          type = "map",
          map = "china",
          label = list(
            show = TRUE,
            formatter = htmlwidgets::JS("
          function(params) {
            if (params.name == '南海诸岛' || params.name == '十段线') {
              return '';  // 返回空字符串，隐藏这些标签
            }
            return params.name;
          }
        ")
          ),
          data = to_list_data(df_plot)
        )
      )
    )
    ecplot$x$registerMap <- list(list(mapName= 'china', geoJSON= geojson))
    ecplot
  })
  
  ##### p3 成员连续获奖 #####
  # p3，UI
  output$ui_p3_sidebar <- renderUI({
    tagList(
      h4("数据筛选"),
      awesomeCheckboxGroup(inputId = "p3_input_Atype", 
                           label = "获奖类型（图1、2）",
                           choices = c("一等奖", "二等奖", "三等奖"),
                           selected = c("一等奖", "二等奖", "三等奖"), 
                           inline = T),
      sliderTextInput(inputId = "p3_input_Iyear", 
                      label = "年份区间（图1、2）", 
                      choices = sort(unique(data_member$Year)), 
                      selected = c(min(data_member$Year), max(data_member$Year))),
      sliderTextInput(inputId = "p3_Stime",
                      label = "连续获奖次数（图1、2）",
                      choices = 1:max(data_member$Series),
                      selected = c(1, max(data_member$Series))),
      sliderTextInput(inputId = "p3_input_Ditem", 
                      label = "显示数量（图1）", 
                      choices = 1:30, 
                      selected = 10) 
    )
  })
  output$ui_p3_main <- renderUI({
    tagList(
      h3(sprintf('连续%d-%d次获奖者所在培养单位', input$p3_Stime[1], input$p3_Stime[2])),
      ecs.output("p3_plot_Stime", height = max("400px", input$p3_input_Ditem*30)),  #动态调整图高度
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s，展示前%d项）', 
                 input$p3_input_Iyear[1], input$p3_input_Iyear[2], trans(input$p3_input_Atype), input$p3_input_Ditem)),
      h3(sprintf('各省连续%d-%d次获奖人数', input$p3_Stime[1], input$p3_Stime[2])),
      ecs.output("p3_plot_Pseries", height = "800px", width = "100%"),
      h6(sprintf('（统计区间：%d-%d年，统计奖项：%s）', 
                 input$p3_input_Iyear[1], input$p3_input_Iyear[2], trans(input$p3_input_Atype)))
    )
  })
  
  # p3图1，个人连续获奖
  output$p3_plot_Stime <- ecs.render({
    if (length(input$p3_input_Atype) == 0) {
      sAtype <- c("一等奖", "二等奖", "三等奖")
    } else {
      sAtype <- input$p3_input_Atype
    }
    p3_data1 <- data_member %>%
      .[A_type %in% sAtype & Series>=input$p3_Stime[1] & Series<=input$p3_Stime[2] & Year>=input$p3_input_Iyear[1] & Year<=input$p3_input_Iyear[2], ] %>%
      .[, .(Freq=length(M_unique)), by = .(Unit)] %>%
      setorder(-Freq) %>%
      .[!is.na(Unit), ] %>%
      .[1:min(nrow(.), input$p3_input_Ditem), ] %>%
      na.omit() %>% 
      setorder(Freq)
    if (nrow(p3_data1) == 1) {
      p3_data1 <- rbind(data.table(Unit = "(占位项)", Freq = NA), p3_data1)   #避免只有一行数据时，画图出错
    }                 
    if(nrow(p3_data1) == 0){
      ec.init(
        graphic = list(
          list(
            type = "text",
            left = "center",    
            top = "middle",     
            style = list(
              text = "未筛选出有效数据",
              fontSize = 24,
              fontWeight = "bold",
              fill = "red"           #文字颜色
            )
          )
        ),
        xAxis = list(show = FALSE),  #隐藏 x 轴
        yAxis = list(show = FALSE),  #隐藏 y 轴
        series = list(               #一个空的系列，防止出错
          list(type = "bar", data = list())
        )
      )
    } else{
      ec.init(
        series = list(
          list(type = "bar", name = "获奖人次", data = p3_data1$Freq, 
               label=list(show = TRUE, position = "right", fontSize = 13, color = "#5470C6"))
        ),
        xAxis = list(type = "value",
                     name = "获奖人次",
                     nameLocation = "middle",
                     nameTextStyle = list(fontSize = 14, padding = c(15, 0, 0, 0)),
                     axisLine = list(show = TRUE, lineStyle = list(color = "black"))
        ),
        yAxis = list(type = "category",
                     data = p3_data1$Unit,
                     name="培养单位",
                     # boundaryGap = FALSE,
                     # nameLocation = "middle",
                     nameTextStyle = list(fontSize = 14, padding = c(0, 50, 0, 0)),
                     axisLine = list(show = TRUE, lineStyle = list(color = "black")),
                     axisTick = list(alignWithLabel = TRUE)
        ),
        tooltip = list(
          trigger = "axis",
          axisPointer = list(type = 'shadow'),
          formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")
        ),
        legend = list(show = F)
      )
    }
  })
  
  # p3图2，各省连续获奖人数
  output$p3_plot_Pseries <- ecs.render({
    if (length(input$p3_input_Atype) == 0) {
      sAtype <- c("一等奖", "二等奖", "三等奖")
    } else {
      sAtype <- input$p3_input_Atype
    }
    p3_data2 <- data_member %>%
      .[A_type %in% sAtype & Series>=input$p3_Stime[1] & Series<=input$p3_Stime[2] & Year>=input$p3_input_Iyear[1] & Year<=input$p3_input_Iyear[2], ] %>%
      .[, Province] %>%                       
      table()
    tdf_plot <- if (length(p3_data2) == 0) {
      data.frame(provi = "NA", value = 0)
    } else {
      data.table(provi = names(p3_data2), value = as.vector(p3_data2))
    } 
    df_plot <- tdf_plot[data_province, on="provi"] %>% .[is.na(value), value:=0]
    
    # 画图
    ecplot <- ec.init(
      tooltip = list(
        trigger = "item",
        formatter = htmlwidgets::JS("
      function(params) {
        return params.name + ': ' + params.value + '人次';
      }
    ")
      ),
      toolbox = list(
        show = TRUE,
        orient = "vertical",  # 设置为垂直
        left = "left",       # 设置图例位置在右侧
        top = "45%",          # 设置距离顶部的距离
        feature = list(
          dataView = list(readOnly = FALSE),
          restore = list(),
          saveAsImage = list()
        )
      ),
      visualMap = list(
        min = 0,
        max = max(df_plot$value),
        # inRange = list(color = c("#ECF9FF", "#5470C6")),  # 调整颜色范围
        inRange = list(color = c("lightskyblue", "yellow", "orangered")),
        orient = "vertical",  # 垂直显示
        right = "100px",  # 设置右侧的偏移
        top = "45%",  # 设置距离顶部的偏移
        itemWidth = 20,  # 设置每个项的宽度
        itemHeight = 250,  # 设置每个项的高度
        calculable = TRUE  # 可计算范围
      ),
      series = list(
        list(
          name = "china-map",
          type = "map",
          map = "china",
          label = list(
            show = TRUE,
            formatter = htmlwidgets::JS("
          function(params) {
            if (params.name == '南海诸岛' || params.name == '十段线') {
              return '';  // 返回空字符串，隐藏这些标签
            }
            return params.name;
          }
        ")
          ),
          data = to_list_data(df_plot)
        )
      )
    )
    ecplot$x$registerMap <- list(list(mapName= 'china', geoJSON= geojson))
    ecplot
  })
  
  ##### p4 获奖名单 #####
  # p4，UI
  output$ui_p4_sidebar <- renderUI({
    tagList(
      h4("数据筛选"),
      pickerInput(inputId = "p4_input_Syear",
                  label = "选择年份",
                  choices = unique(data_team$Year),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      checkboxGroupButtons(inputId = "p4_input_Satype",
                           label = "选择奖项",
                           choices = c("一等奖", "二等奖", "三等奖", "成功参与奖"),
                           status = "primary",
                           size = "lg"
      ),
      checkboxGroupButtons(inputId = "p4_input_Sqtype",
                           label = "选择题型",
                           choices = c("A", "B", "C", "D", "E", "F", "N"),
                           status = "primary",
                           width = "300px", 
                           size = "lg")
    )
  })
  output$ui_p4_main <- renderUI({
    tagList(
      h3("获奖名单"),
      dataTableOutput("p4_atable")
    )
  })
  
  # p4表1，获奖名单
  output$p4_atable <- renderDT({
    if(length(input$p4_input_Syear) == 0) sYear <- unique(data_team$Year) else sYear <- input$p4_input_Syear
    if(length(input$p4_input_Satype) == 0) sA_type <- unique(data_team$A_type) else sA_type <- input$p4_input_Satype
    if(length(input$p4_input_Sqtype) == 0) sQ_type <- unique(data_team$Q_type) else sQ_type <- input$p4_input_Sqtype
    Scol <- c("Year", "Q_type", "A_type", "T_numb", "C_name", "C_unit", "F_name", "F_unit", "S_name", "S_unit")
    Scol_name <- c("年份", "题型", "奖项", "队号", 
                   "队长姓名", "队长单位", "队员一姓名", "队员一单位", "队员二姓名", "队员二单位")
    p4_data1 <- data_team[Year %in% sYear & A_type %in% sA_type & Q_type %in% sQ_type, Scol, with=F] %>% 
      set_colnames(Scol_name)
    # DT::datatable(p4_data1, options = list(pageLength = 25, scrollX = TRUE))
    DT::datatable(p4_data1, options = list(pageLength = 25, scrollX = TRUE))
  })
}

##### app ######
shinyApp(ui, server)
