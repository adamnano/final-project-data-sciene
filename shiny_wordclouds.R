library(shiny)
library(readr)
library(dplyr)
library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")

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

#define UI
ui = fluidPage(
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
  
  # selectInput(inputId = "area",
  #             label = "Choose area: ",
  #             choices = c("經營_人資類" = 1, "文字_傳媒工作類" = 2, "生產製造_品管_環衛類" = 3, "行政_總務_法務類" = 4, "行銷_企劃_專案管理類" = 5, 
  #                         "客服_門市_業務_貿易類" = 6, "研發相關類" = 7, "軍警消_保全類" = 8, "財會_金融專業類" = 9, "傳播藝術_設計類" = 10, 
  #                         "資材_物流_運輸類" = 11, "資訊軟體系統類" = 12, "學術_教育_輔導類" = 13, "操作_技術_維修類" = 14, "餐飲_旅遊_美容美髮類" = 15, 
  #                         "營建_製圖類" = 16, "醫療_保健服務類" = 17),
  #             multiple = FALSE),
  # selectInput(inputId = "cloud",
  #             label = "Compare: ",
  #             choices = c("Skills" = 1, "Languages" = 2, "Majors" = 3, "Tools" = 4),
  #             multiple = FALSE),
  # plotOutput(outputId = "wordcloud")
)


#define server logic
server = function(input, output){
  output$wordcloud <- renderPlot({
    file = Job[[as.integer(input$area)]]
    cloud = input$cloud
    
    
    # #major#2
    # get_required_majors <- function(Job_major, no_specify = FALSE)
    # {
    #   Job_major <- Job_major %>% select(科系要求)
    #   if (no_specify == FALSE) Job_major <- Job_major %>% filter(科系要求 != "不拘")
    #   required_major <- vector("character", length = length(Job_major))
    #   for (i in 1:length(Job_major[[1]])) {
    #     required_major <- c(required_major, strsplit(Job_major[[1]][i], split = "、"))
    #   }
    #   required_major <- required_major %>% unlist() %>% unname()
    #   return(required_major)
    # }
    # total_major_list <- sapply(Job, get_required_majors) #list of all the majors
    # 
    # library("RColorBrewer") #wordcloud
    # library("tm")
    # library("wordcloud")
    # #17 word clouds of majors from 17 industries respectively
    # docs <- Corpus(VectorSource(total_major_list[[as.integer(input$num)]]))
    # inspect(docs)
    # dtm <- TermDocumentMatrix(docs)
    # m <- as.matrix(dtm)
    # v <- sort(rowSums(m),decreasing=TRUE)
    # d <- data.frame(word = names(v),freq=v)
    # head(d, 10)
    # set.seed(1234)
    # wordcloud(words = d$word, freq = d$freq, min.freq = 1,
    #           max.words=200, random.order=FALSE, rot.per=0.35, 
    #           colors=brewer.pal(8, "Dark2"))
    # 
    
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
    
  }, width = 700, height = 700)
}


#run the application
shinyApp(ui = ui, server = server)