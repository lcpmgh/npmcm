# 本程序内容：
#     整合获奖数据文件为一个数据，并进行培养单位名称标准化、生成培养单位信息、生成成员获奖数据，为后续可视化做准备。
# 
# 程序版本：
#     v1.0：2021-04-26
#     v2.0: 2022-01-28
#     v2.1: 2024-03-06  小修
#     v3.0：2025-02-03  大修：优化了培养单位标准化、生成成员信息等函数的执行效率


##### 1. 配置环境 #####
# 1.1 初始化
stop("aaa")
rm(list = ls())
gc()

# 1.2 加载包
# 数据处理部分
library(magrittr)
library(data.table)
library(stringr)
# 网络爬虫部分
# library(RCurl)
# library(XML)
# library(rvest)

# 1.3 环境设置
options(stringsAsFactors = F)   #指定字符读取方式和默认编码
ver <- "7.0"                    #定义正在修改数据的版本，用于存储、读取数据时，定位文件夹
setwd("D:/#R/@github/npmcm")


##### 2. 创建函数 #####
# 2.1 数据读取、写入函数
raw_data_dir <- function(name = NULL){paste0(getwd(), '/awardlist/', name)}              #原始文件路径 
data_dir <- function(name = NULL){paste0(getwd(), '/v', ver, '/shiny_app/data/', name)}  #数据文件路径
my_read_csv  <- function(file_name, raw_data = F, ...){
  #判断是否在读取获奖名单原始数据
  if(isTRUE(raw_data)){
    file_name <- raw_data_dir(file_name)
  } else{
    file_name <- data_dir(file_name)
  }
  data <- fread(file_name, encoding = 'UTF-8', ...)
  message(sprintf("已读取UTF-8文件【%s】", file_name))
  return(data)
}                              #创建读取函数（强制UTF-8）
my_write_csv <- function(data, file_name){
  file_name <- data_dir(file_name)
  write.csv(data, file_name, row.names = F, quote = F, fileEncoding = 'UTF-8')
  message(sprintf("已写入UTF-8文件【%s】", file_name))
}                                           #创建写入函数（强制UTF-8）

# 2.2 整合分年获奖数据，默认不涉及国际赛道
data_collection <- function(international=F){
  data_total <- data.table()
  files_name <- list.files(raw_data_dir()) %>% str_extract("\\d{4,4}.csv") %>% na.omit() #识别全部获奖数据
  for(file_name in files_name){
    t <- my_read_csv(file_name, raw_data = T, integer64 = 'character', select = 1:9)     #转换队号列数据类型，排除2020年后的备注列
    t[, Year:=str_extract(file_name, "\\d{4,4}")]                                        #添加year
    data_total <- rbind(data_total, t)
  }
  data_total <- dplyr::rename(data_total, 
                              Q_type = '题目类型',      T_numb = '队号',               
                              C_name = '队长姓名',      C_unit = '队长所在学校',
                              F_name = '第一队友姓名',  F_unit = '第一队友所在学校', 
                              S_name = '第二队友姓名',  S_unit = '第二队友所在学校', 
                              A_type = '所获奖项') %>% 
    .[, T_id:=seq_along(A_type)] %>%                                                      #生成唯一的队伍id
    .[!(A_type %in% c('一等奖', '二等奖', '三等奖')), A_type:='成功参与奖']               #未获奖为“成功参与奖”
  if(isFALSE(international)) data_total <- data_total[!Q_type %in% c("H", "G"),]          #未涉及国际赛道
  setkey(data_total, "T_id")
  message(sprintf('成功整合以上【%d份】获奖名单文件。', length(files_name)))
  return(data_total)
}

# 2.3 将原始数据转化为培养单位形式，用于单位名识别、归一、定位、替换，与to_data_total互逆
to_data_unit <- function(data_total){
  mcol <- c("C_unit", "F_unit", "S_unit")
  data_unit <- melt(data_total, 
                    id.vars = "T_id", 
                    measure.vars = mcol, 
                    variable.name = 'Sig', 
                    value.name = 'Unit') %>% 
    .[,Sig:=as.character(Sig)]
  setkey(data_unit, "T_id")
  return(data_unit)
}

# 2.4 将培养单位形式的数据转化为原始形式，用于单位归一化后数据形式的重置，与to_data_unit互逆
to_data_total <- function(data_unit, data_total){
  # 注意data_unit和data_total都以T_id为key，因此可以直接连接
  temp_names <- names(data_total) %>% setdiff(c("C_unit", "F_unit", "S_unit"))
  temp_data_total <- data_total[, ..temp_names]
  new_data_total <- dcast(data_unit, T_id~Sig, value.var = "Unit") %>% setkey("T_id") %>% .[temp_data_total,]
  return(new_data_total)
}

# 2.5 获奖数据中的培养单位名称标准化，逻辑是，将原数据转化为data_unit形式，单位标准化，再转为data_total形式
unit_standardization <- function(data_total){
  unit_name_convert <- my_read_csv('0-unit_name_convert.csv')   #读取培养单位名称标准化替换数据
  nrow_uncon <- nrow(unit_name_convert) 
  data_unit_cond <- to_data_unit(data_total)
  # 循环将data_unit_cond中的original转换为standard
  for(i in 1:nrow_uncon){
    o_name <- unit_name_convert[i, original]
    s_name <- unit_name_convert[i, standard]
    temp_sig <- data_unit_cond[Unit==o_name,]         #有替换时，才进行替换，让message更准确
    if(nrow(temp_sig)>0){
      data_unit_cond[Unit==o_name, Unit:=s_name]
      message(sprintf('%d/%d：已将【%s】替换为【%s】', i, nrow_uncon, o_name, s_name))    
    }
  }
  # 循环结束，将data_unit_cond恢复为data_total
  data_total_cond <- to_data_total(data_unit_cond, data_total)
  message("-----已完成替换-----")
  return(data_total_cond)
}

# 2.6 爬虫获取培养单位信息（位置精确到省份，爬虫为一次性使用，现版本已经用完全不到了）
unit_info_crawler <- function(){
  res <- data.table()
  n    <- 1
  ourl <- 'https://yz.chsi.com.cn'
  page <- paste0(ourl, "/sch/search.do?start=0")
  message(">>>正在进行网络爬虫...")
  while(page!=paste0(ourl,'#')){
    url <- page
    headers <- c("Referer" = url,
                 "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36")
    debugInfo <- debugGatherer()
    handle    <- getCurlHandle(debugfunction = debugInfo$update, followlocation = TRUE, cookiefile = "", verbose = TRUE)
    content   <- getURL(url, .opts = list(httpheader = headers), .encoding = "utf-8", curl = handle)
    temp_content <- read_html(content) 
    info_table   <- html_table(temp_content) %>% '[['(1) %>% .[,1:2]
    res <- rbind(res, info_table)
    next_page <-  html_nodes(temp_content, "div[class='pager-box clearfix'] li a") %>% 
      html_attr('href') %>% 
      tail(1) %>% 
      paste0(ourl,.)
    message(sprintf('第【%d】页收集完毕',n))
    page <- next_page
    n <- n+1
    Sys.sleep(runif(1, 1, 3))
  }
  # 由于改用REmap，绘图不在需要省份全名，改用短名，因此下面这部分注释掉
  # names(res) <- c('Unit', 'Location')
  # unit_location_convert <- my_read_csv('0-unit_location_convert.csv')
  # unit_info_internet <- res[unit_location_convert, on='Location', nomatch=0] %>% .[,-2]                  #将培养单位位置转换为培养单位信息       
  names(res) <- c('Unit', 'Province')
  unit_info_internet <- res %>% .[, Country:="中国"]
  message(sprintf('>>>爬虫结束，共收集到【%d】条数据',nrow(res)))
  return(unit_info_internet)
}

# 2.7 对比数据中单位与单位库，用于查看数据中单位是否有新加入的（不再考虑爬虫得到的数据，相关函数见上一个版本）
unit_info_genrating <- function(data_total){
  unit_info <- my_read_csv('3-unit_info_dataset.csv') %>% .[!duplicated(Unit), ] %>% setkey("Unit")
  unit_data <- to_data_unit(data_total) %>% .[, "Unit"] %>% unique() %>% setkey("Unit")
  unit_data_info <- unit_info[unit_data,]     #连接单位信息与数据中的单位名
  unit_data_info_na <- unit_data_info[is.na(City), ] %>% setorder(Unit)     
  unit_data_info_re <- unit_data_info[!is.na(City), ] %>% setorder(Unit)       #无缺失值部分按名称排序
  unit_info_dataset <- rbind(unit_data_info_na, unit_data_info_re)
  return(unit_info_dataset)
}

# 2.8 区分个体并计算成员连续获奖次数，v7.0修改后，此函数只能用于识别Series>1的情形
series_finding <- function(year, group){
  dy <- diff(year)
  if(any(dy<0)) stop('没排序啊!')
  ldy <- length(dy)
  s <- integer(ldy+1)                          #统计重复次数
  g <- character(ldy+1)                        #统计分组信息
  sig <- c(0, dy, 0)
  sig[sig>1] <- 0
  w <- which(sig==0)                           #这里的w长度至少大于2
  n <- 1
  for(i in 2:length(w)){
    lo <- w[i-1]:(w[i]-1)
    le <- seq_along(lo)
    s[lo] <- le
    g[lo] <- paste(group[1], n, sep = '-')    #将所有断开的连续视为不同个体，更符合大多数情况，无奈之举
    n <- n+1
  }
  return(list(sig = s, gro = g))
}

# 2.9 生成成员获奖数据
member_data_generating <- function(data_total){
  unit_info_dataset <- my_read_csv('3-unit_info_dataset.csv') %>% setkey("Unit")
  col_id <- c("T_id", "T_numb", "A_type", "Year")
  col_name <- c("C_name", "F_name", "S_name")
  col_unit <- c("C_unit", "F_unit", "S_unit")
  col_sele <- c("T_id", "M_id", "M_unique", "Series", "Year", "A_type", "Name", "Unit")
  
  # 初步划分，识别unit+name相同的group，赋值组内长度（time）和队员唯一id（M_unique）
  member_temp <- data_total %>% 
    .[A_type %in% c('一等奖','二等奖','三等奖'),] %>%    #获奖不包括"成功参与奖"，若包含将极其耗时
    melt(id = col_id, measure.vars = list(Name=col_name, Unit=col_unit)) %>%
    .[Name!='',] %>% 
    .[, c("T_id", "A_type", "Year", "Name", "Unit")]%>% 
    .[, M_id:=seq_along(T_id)] %>% 
    .[, Year:=as.integer(Year)] %>% 
    setorder(Year) %>% 
    .[, c("time","M_unique"):=list(.N, .GRP), by=c("Name", "Unit")]
  
  # 提取time==1的行，它们的series必然是1
  member_data_1 <- copy(member_temp[time==1,]) %>% 
    .[,Series:=1] %>% 
    .[, ..col_sele] %>% 
    setkey("Unit")
  
  # 提取time>1的行，计算求series
  member_data_2 <- copy(member_temp[time>1,]) %>% 
    .[, c('Series', 'Group'):=list(series_finding(Year, .GRP)$sig, 
                                   series_finding(Year, .GRP)$gro), by=c("M_unique")] %>% 
    .[, ..col_sele] %>%
    setkey("Unit")
    
  # 合并两组数据，并连接培养单位信息
  member_data <- rbind(member_data_1, member_data_2) %>% 
    setkey("Unit") %>% 
    unit_info_dataset[.,] %>% 
    .[, c(col_sele, "Country", "Province", "City"), with=F]
  
  # 返回结果
  return(member_data)
}


##### 3. 数据处理-生成总数据、数据单位名矫正、生成成员数据 #####
# 3.1 将年份获奖数据整合，不考虑国际赛道
data_total <- data_collection(international=F)                                     

# 3.2 培养单位名称归一化
data_total <- unit_standardization(data_total)               

# 3.3 生成培养单位信息（初步，后续需人工查验补齐）
# 网络爬虫生成的本地数据库不再更新，此v7.0后这部分内容删掉，要想查看见上一个版本
# 手动查漏补缺步骤：
# 此函数，若输出结果中，存在NA值，则有以下两种情况：
#    1.新增培养单位
#    2.旧培养单位新的不规范名字
# 对于1，需要修改3-unit_info_dataset.csv文件，添加新培养单位的信息（此文件中培养单位都是标准名称）
# 对于2，需要修改0-unit_name_convert.csv文件，添加新的培养单位标准化规则
# 至于是1还是2，将unit_info_datase  t保存为999-temp-unit_info_dataset.csv挨个查看
# 如果某个country和province为NA的unit，向下找，找不到同单位的，说明是1，找到说明是2
# 注意，在3-unit_info_dataset.csv中，不要添加重复名字
# 结束这两步后，重新运行4.1和4.2，直至此函数运行结果中无NA值为止，crawler=F
unit_info_dataset <- unit_info_genrating(data_total);unit_info_dataset
my_write_csv(unit_info_dataset, "999-temp-unit_info_dataset.csv")   #临时储存以便查看结果

# 3.4 生成获奖名单（处理后的）中的单位名称集（唯一化）（用于培养单位名称标准化手动的查漏补缺）
# T_id==7247有个空值，没有问题，该队只有两个队友
# total_unit_unique <- to_data_unit(data_total) %>% .[!duplicated(Unit)] %>% .[order(Unit)]  #比下面代码更麻烦
total_unit_unique <- to_data_unit(data_total) %>% unique(by="Unit") %>% setorder(Unit)

# 3.5 生成成员获奖数据
member_data <- member_data_generating(data_total)


##### 4. 保存数据-保存合并的获奖名单和培养单位信息数据库 #####
# 4.1 保存合并获奖名单数据
my_write_csv(data_total, '2-data_total.csv')

# 4.2 保存培养单位信息数据库，谨慎运行，会将csv3覆盖为，仅包含data_total中unit的信息，额外的会全部被删掉
# my_write_csv(unit_info_dataset, '3-unit_info_dataset.csv')

# 4.3 保存单位名称集
my_write_csv(total_unit_unique, '4-unit_data_unique.csv')

# 4.4 保存成员获奖数据
my_write_csv(member_data, '5-data_member.csv')

  