library(shiny)
library(readr)
library(dplyr)
library("RColorBrewer") #wordcloud
library("tm")
library("wordcloud")
library(stringr)
library(ggplot2)

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
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "area2",
                  label = "Choose area: ",
                  choices = c("經營_人資類" = 1, "文字_傳媒工作類" = 2, "生產製造_品管_環衛類" = 3, "行政_總務_法務類" = 4, "行銷_企劃_專案管理類" = 5, 
                              "客服_門市_業務_貿易類" = 6, "研發相關類" = 7, "軍警消_保全類" = 8, "財會_金融專業類" = 9, "傳播藝術_設計類" = 10, 
                              "資材_物流_運輸類" = 11, "資訊軟體系統類" = 12, "學術_教育_輔導類" = 13, "操作_技術_維修類" = 14, "餐飲_旅遊_美容美髮類" = 15, 
                              "營建_製圖類" = 16, "醫療_保健服務類" = 17),
                  multiple = FALSE),
      # selectInput(inputId = "cloud",
      #             label = "Compare: ",
      #             choices = c("Skills" = 1, "Languages" = 2, "Majors" = 3, "Tools" = 4),
      #             multiple = FALSE),
    ),
    mainPanel(
      plotOutput(outputId = "graph")
    )
    
  )
  
)


#define server logic
server = function(input, output){
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
}


#run the application
shinyApp(ui = ui, server = server)