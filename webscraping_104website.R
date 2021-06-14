# libs for webscraping
library(rvest) # extracting data from website
library(xml2) # reading website DOM/HTML/XML
library(RSelenium) # headless browsing
library(stringr) # data cleaning
library(httr)

rD <- rsDriver(verbose = FALSE, port = 8887L, browser = "firefox")
remDr <- rD$client
remDr$open()

numPages <- 15

source_links <- vector("character", length = numPages)
for (i in 1:numPages){
  source_links[i] <- paste0("https://www.104.com.tw/jobs/search/?ro=2&jobcat=2002000000&expansionType=area,spec,com,job,wf,wktm&order=11&asc=0&rostatus=1024&page=",i,"&mode=s&jobsource=student2020")
}
source_links
 
link_list <- c()

job_function <- function(search_page_link) {
  for (i in 1:length(search_page_link)){
    resp <- GET(search_page_link[i])
    html <- content(resp)
    link <- html %>% html_nodes("#js-job-content div.b-block__left > h2 > a") %>% html_attr("href")
    for (x in 1:length(link)) {
      link[x] <- paste0("https:", link[x])
    }
    link_list <- c(link_list, link)
  }
  
  Job_attr1 <- vector("character", length = length(link_list))
  Job_attr2 <- vector("character", length = length(link_list))
  Job_attr3 <- vector("character", length = length(link_list))
  Job_attr4 <- vector("character", length = length(link_list))
  Job_attr5 <- vector("character", length = length(link_list))
  Job_attr6 <- vector("character", length = length(link_list))
  Job_attr7 <- vector("character", length = length(link_list))
  Job_attr8 <- vector("character", length = length(link_list))
  
  for (i in 1:length(link_list)) {
    remDr$navigate(link_list[i])
    pageHTML <- read_html(remDr$getPageSource()[[1]])
    Job_attr1[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(1) > div.col.p-0.job-requirement-table__data") %>% html_text()
    Job_attr2[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(2) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr3[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(3) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr4[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(4) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr5[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(5) > div.col.p-0.job-requirement-table__data") %>% html_text()
    Job_attr6[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(6) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr7[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(7) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    #Job_attr8[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(8) > div.col.p-0.job-requirement-table__data") %>% html_text()
    print(i)
  }
  Job_info <- data.frame(
    index = 1:length(link_list),
    接受身份 = Job_attr1,
    工作經歷 = Job_attr2,
    學歷要求 = Job_attr3,
    科系要求 = Job_attr4,
    語文條件 = Job_attr5,
    擅長工具 = Job_attr6,
    工作技能 = Job_attr7
    #其他條件 = Job_attr8
  )
  View(Job_info)
  write.csv(Job_info,"webpage21.csv", row.names = FALSE)
}
job_function(source_links)

remDr$close()
rD$server$stop()