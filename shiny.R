library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
library(tidyverse)
library(ggrepel)
source("./vectorizing_strings.R")

Jobs_list <- list.files(path = "./csv files", pattern = "*.csv", full.names = TRUE)
Jobs <- lapply(Jobs_list, read.delim, header = TRUE, sep = ',')

Job_info1 <- read_csv("csv files/HR.csv")
Job_info2 <- read_csv("csv files/文字_傳媒工作類.csv")
Job_info3 <- read_csv("csv files/生產製造_品管_環衛類.csv")
Job_info4 <- read_csv("csv files/行政_總務_法務類.csv")
Job_info5 <- read_csv("csv files/行銷_企劃_專案管理類.csv")
Job_info6 <- read_csv("csv files/客服_門市_業務_貿易類.csv")
Job_info7 <- read_csv("csv files/研發相關類.csv")
Job_info8 <- read_csv("csv files/軍警消_保全類.csv")
Job_info9 <- read_csv("csv files/財會_金融專業類.csv")
Job_info10 <- read_csv("csv files/傳播藝術_設計類.csv")
Job_info11 <- read_csv("csv files/資材_物流_運輸類.csv")
Job_info12 <- read_csv("csv files/資訊軟體系統類.csv")
Job_info13 <- read_csv("csv files/學術_教育_輔導類.csv")
Job_info14 <- read_csv("csv files/操作_技術_維修類.csv")
Job_info15 <- read_csv("csv files/餐飲_旅遊_美容美髮類.csv")
Job_info16 <- read_csv("csv files/營建_製圖類.csv")
Job_info17 <- read_csv("csv files/醫療_保健服務類.csv")
Job <- list(Job_info1, Job_info2, Job_info3, Job_info4, Job_info5, Job_info6, Job_info7, Job_info8, 
            Job_info9, Job_info10, Job_info11, Job_info12, Job_info13, Job_info14, Job_info15, Job_info16, Job_info17)
Job_content1 <- read_csv("工作內容csv/經營_人資類content.csv")
Job_content2 <- read_csv("工作內容csv/文字_傳媒工作類content.csv")
Job_content3 <- read_csv("工作內容csv/生產製造_品管_環衛類content.csv")
Job_content4 <- read_csv("工作內容csv/行政_總務_法務類content.csv")
Job_content5 <- read_csv("工作內容csv/行銷_企劃_專案管理類content.csv")
Job_content6 <- read_csv("工作內容csv/客服_門市_業務_貿易類content.csv")
Job_content7 <- read_csv("工作內容csv/研發相關類content.csv")
Job_content8 <- read_csv("工作內容csv/軍警消_保全類content.csv")
Job_content9 <- read_csv("工作內容csv/財會_金融專業類content.csv")
Job_content10 <- read_csv("工作內容csv/傳播藝術_設計類content.csv")
Job_content11 <- read_csv("工作內容csv/資材_物流_運輸類content.csv")
Job_content12 <- read_csv("工作內容csv/資訊軟體系統類content.csv")
Job_content13 <- read_csv("工作內容csv/學術_教育_輔導類content.csv")
Job_content14 <- read_csv("工作內容csv/操作_技術_維修類content.csv")
Job_content15 <- read_csv("工作內容csv/餐飲_旅遊_美容美髮類content.csv")
Job_content16 <- read_csv("工作內容csv/營建_製圖類content.csv")
Job_content17 <- read_csv("工作內容csv/醫療_保健服務類content.csv")
Job_content_list <- list(Job_content1, Job_content2, Job_content3, Job_content4, Job_content5, Job_content6, Job_content7, Job_content8, 
                         Job_content9, Job_content10, Job_content11, Job_content12, Job_content13, Job_content14, Job_content15, Job_content16, Job_content17)
titles <- c("經營_人資類", "文字_傳媒工作類", "生產製造_品管_環衛類", "行政_總務_法務類", "行銷_企劃_專案管理類", 
            "客服_門市_業務_貿易類", "研發相關類", "軍警消_保全類", "財會_金融專業類", "傳播藝術_設計類", 
            "資材_物流_運輸類", "資訊軟體系統類", "學術_教育_輔導類", "操作_技術_維修類", "餐飲_旅遊_美容美髮類", 
            "營建_製圖類", "醫療_保健服務類")

#define UI
ui = fluidPage(
  #titlePanel("Application Name"),
  
  navlistPanel(
    tabPanel("文字雲",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "area",
                             label = "Choose area: ",
                             choices = c("經營_人資類" = 1, "文字_傳媒工作類" = 2, "生產製造_品管_環衛類" = 3, "行政_總務_法務類" = 4, "行銷_企劃_專案管理類" = 5, 
                                         "客服_門市_業務_貿易類" = 6, "研發相關類" = 7, "軍警消_保全類" = 8, "財會_金融專業類" = 9, "傳播藝術_設計類" = 10, 
                                         "資材_物流_運輸類" = 11, "資訊軟體系統類" = 12, "學術_教育_輔導類" = 13, "操作_技術_維修類" = 14, "餐飲_旅遊_美容美髮類" = 15, 
                                         "營建_製圖類" = 16, "醫療_保健服務類" = 17),
                             multiple = FALSE),
                 selectInput(inputId = "cloud",
                             label = "Compare: ",
                             choices = c("Skills" = 1, "Languages" = 2, "Majors" = 3, "Tools" = 4),
                             multiple = FALSE),
               ),
               mainPanel(
                 plotOutput(outputId = "wordcloud")
               )
               
             )
             ),
    tabPanel("軟實力詞彙統計",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "area2",
                             label = "Choose area: ",
                             choices = c("經營_人資類" = 1, "文字_傳媒工作類" = 2, "生產製造_品管_環衛類" = 3, "行政_總務_法務類" = 4, "行銷_企劃_專案管理類" = 5, 
                                         "客服_門市_業務_貿易類" = 6, "研發相關類" = 7, "軍警消_保全類" = 8, "財會_金融專業類" = 9, "傳播藝術_設計類" = 10, 
                                         "資材_物流_運輸類" = 11, "資訊軟體系統類" = 12, "學術_教育_輔導類" = 13, "操作_技術_維修類" = 14, "餐飲_旅遊_美容美髮類" = 15, 
                                         "營建_製圖類" = 16, "醫療_保健服務類" = 17),
                             multiple = FALSE),
               ),
               mainPanel(
                 plotOutput(outputId = "graph")
               )
               
             )
             ),
    tabPanel("長條圖",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "area3",
                             label = "Choose area: ",
                             choices = c("所有領域"),
                             multiple = FALSE),
                 selectInput(inputId = "cloud3",
                             label = "Compare: ",
                             choices = c("英文能力" = 1, "學歷背景" = 2, "科系背景" = 3),
                             multiple = FALSE),
               ),
             mainPanel(
               plotOutput(outputId = "final")
             )
             )
  )
))


#define server logic
server = function(input, output){
  output$wordcloud <- renderPlot({
    file = Job[[as.integer(input$area)]]
    cloud = input$cloud
    
    #English
    get_required_languages <- function(Job_language, no_specify = TRUE) #split all the languages
    { Job_language <- Job_language %>% select(語文條件)
    if (no_specify == FALSE) Job_language <- Job_language %>% filter(語文條件 != "不拘")
    required_language <- vector("character", length = length(Job_language))
    for (i in 1:length(Job_language[[1]]))
    {
      required_language <- c(required_language, sapply(strsplit(Job_language[[1]][i], split = "  ")[[1]], function(x) return(strsplit(x, split = " -- ")[[1]][1])))
    }
    required_language <- required_language %>% unlist() %>% unname()
    return(required_language)
    }
    total_language_vec <- get_required_languages(file)
    language_tb <- tibble(語言 = total_language_vec) #build a tibble
    English_rate <- language_tb %>%
      filter(語言 %in% c("不拘", "英文")) %>%
      count(語言) %>%
      mutate(total = sum(n)) %>%
      mutate(rate = n / total * 100) %>%
      filter(語言 == "英文")
    English_rate
    
    #Major
    get_required_majors <- function(Job_major, no_specify = FALSE)
    {
      Job_major <- Job_major %>% select(科系要求)
      if (no_specify == FALSE) Job_major <- Job_major %>% filter(科系要求 != "不拘")
      required_major <- vector("character", length = length(Job_major))
      for (i in 1:length(Job_major[[1]])) {
        required_major <- c(required_major, strsplit(Job_major[[1]][i], split = "、"))
      }
      required_major <- required_major %>% unlist() %>% unname()
      return(required_major)
    }
    total_major_vec <- get_required_majors(file)
    
    #Tool
    get_required_tools <- function(Job_tool, no_specify = FALSE)
    {
      Job_tool <- Job_tool %>% select(擅長工具)
      if (no_specify == FALSE) Job_tool <- Job_tool %>% filter(擅長工具 != "不拘")
      required_tool <- vector("character", length = length(Job_tool))
      for (i in 1:length(Job_tool[[1]]))
      {
        required_tool <- c(required_tool, strsplit(Job_tool[[1]][i], split = "、"))
      }
      required_tool <- required_tool %>% unlist() %>% unname()
      return (required_tool)
    }
    total_tool_vec <- get_required_tools(file)
    
    #skill
    get_required_skills <- function(Job_skill, no_specify = FALSE)
    {
      Job_skill <- Job_skill %>% select(工作技能) 
      if (no_specify == FALSE) Job_skill <- Job_skill %>% filter(工作技能 != "不拘")
      required_skill <- vector("character", length = length(Job_skill))
      for (i in 1:length(Job_skill[[1]]))
      {
        required_skill <- c(required_skill, strsplit(Job_skill[[1]][i], split = "、"))
      }
      required_skill <- required_skill %>% unlist() %>% unname()
      return(required_skill)
    }
    total_skill_vec <- get_required_skills(file)
    
    #make a word cloud
    if (cloud == 1){
      vec <- total_skill_vec
    } else if (cloud == 2){
      vec <- total_language_vec
    }else if (cloud == 3){
      vec <- total_major_vec
    }else if (cloud == 4){
      vec <- total_tool_vec
    }
    
    #vec <- cloud#put a vec here
    docs <- Corpus(VectorSource(vec))
    inspect(docs)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=10, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"), scale = c(6, 3))
    
  }, width = 600, height = 800)
  
  #################################
  
  
  output$graph <- renderPlot({
    file = Job_content_list[[as.integer(input$area2)]]
    #cloud = input$cloud
    
    job_content <- function(file) {
      #file <- read_csv(File_path)
      keyword <- c("分析", "批判性思考", "邏輯", "積極學習", "自我學習", "問題解決", "自我管理", "創意", "創造力", "創新", "領導力", "社群影響力", "適應力", "抗壓性高", "理解能力", "溝通能力", "說服", "協商", "團隊合作")
      frequent <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      for (r in 1:length(file[[1]])) {
        for (w in 1:length(keyword)) {
          if (str_detect(file[[2]][r], keyword[w])) {
            frequent[w] <- frequent[w] + 1
          }
        }
      }
      result <- data.frame(
        關鍵字 = keyword,
        出現次數 = frequent
      )
      ggplot(data = result) +
        geom_bar(mapping = aes(reorder(x = 關鍵字, -出現次數), y = 出現次數), stat = "identity") +
        labs(title = titles[[as.integer(input$area2)]],
             x = "關鍵字") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    job_content(file)
    
    
  })
  
  ###############################
  output$final <- renderPlot({
    #file = Job_content_list[[as.integer(input$area2)]]
    #English
    cloud3 = input$cloud3
    if (cloud3 == 1){
      get_required_languages <- function(Job_language, no_specify = TRUE) #split all the languages
      { Job_language <- Job_language %>% select(語文條件)
      if (no_specify == FALSE) Job_language <- Job_language %>% filter(語文條件 != "不拘")
      required_language <- vector("character", length = length(Job_language))
      for (i in 1:length(Job_language[[1]]))
      {
        required_language <- c(required_language, sapply(strsplit(Job_language[[1]][i], split = "  ")[[1]], function(x) return(strsplit(x, split = " -- ")[[1]][1])))
      }
      required_language <- required_language %>% unlist() %>% unname()
      return(required_language)
      }
      languages_vecs <- sapply(Job, get_required_languages)
      
      English_rates <- vector(mode = "list", length = 17)
      for(i in 1:length(languages_vecs)){
        total_language_vec <- languages_vecs[[i]] #a vec of all the languages and 不拘
        language_tb <- tibble(語言 = total_language_vec) #build a tibble
        English_rate <- language_tb %>%
          filter(語言 %in% c("不拘", "英文")) %>%
          count(語言) %>%
          mutate(total = sum(n)) %>%
          mutate(rate = n / total * 100) %>%
          filter(語言 == "英文") #rate of jobs that required English
        English_rates[[i]] <- English_rate
      }
      English_rates <- bind_rows(English_rates)
      English_rates <- English_rates %>%
        mutate(領域 = titles) %>%
        arrange(desc(rate))
      
      ggplot(English_rates, aes(reorder(x = 領域, -rate), y = rate)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "各領域需要英文能力的職缺比例",
             x = "領域",
             y = "比例")
    } else if(cloud3 == 2){
      
      
      degree_level <- c("不拘", "高中以下", "高中", "高中以上", "專科", "專科以上", "大學", "大學以上", "碩士", "碩士以上", "博士")
      above_uni <- c("大學", "大學以上", "碩士", "碩士以上", "博士")
      regex <- ".*/(.*)\\.csv"
      J <- Jobs %>% 
        lapply(get_least_degree) %>% 
        lapply(function(x){
          return(data.frame(degree = x) %>% 
                   count(degree) %>%
                   mutate(total = sum(n)) %>%
                   mutate(rate = n / total * 100) %>%
                   arrange(factor(degree, levels = degree_level))
          )}) %>%
        lapply(function(x){
          x$degree <- x$degree %>% sapply(function(x){
            return(if (x %in% above_uni) "大學以上" else "大學以下")
          }) %>%
            unname()
          return(x)
        }) %>% # if need all degrees, comment the lapply below(care for the parentheses)
        lapply(function(x){
          return(x %>%
                   group_by(degree) %>%
                   summarise(n = sum(n), rate = n / total * 100) %>%
                   ungroup() %>%
                   unique()
          )})
      for (i in 1:length(J))
      {
        J[[i]] <- J[[i]] %>% mutate(catagory = gsub(regex, '\\1', Jobs_list[i]))
      }
      J
      
      ## All catagories in a plot
      
      df <- data.frame()
      for (i in 1:length(J))
      {
        df <- rbind(df, J[[i]] %>% filter(degree == "大學以上"))
      }
      df <- df %>% arrange(desc(rate))
      df
      
      
      ## Plotting all catagories
      
      all_plt <- df %>% ggplot() +
        geom_bar(mapping = aes(reorder(x = catagory, -rate), y = rate), stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("職業類別") +
        ylab("需要大學以上學歷比例")
      all_plt
      
    }else if(cloud3 == 3){
      Major_required <- function(Job_info) {
        Major_requirement <- Job_info %>%
          select(科系要求)
        return(mean(Major_requirement[[1]] != "不拘"))
      }
      Major_required_rate <- sapply(Job, Major_required) #rate of jobs that required certain majors
      Major_rates <- tibble(rate = Major_required_rate * 100, 領域 = titles)
      ggplot(Major_rates, aes(reorder(x = 領域, -rate), y = rate)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "各領域需要特定科系的職缺比例",
             x = "領域", 
             y = "比例")
    }
    
  })
}


#run the application
shinyApp(ui = ui, server = server)