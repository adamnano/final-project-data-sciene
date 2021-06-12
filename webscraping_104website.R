# libs for webscraping
library(rvest) # extracting data from website
library(xml2) # reading website DOM/HTML/XML
library(RSelenium) # headless browsing
library(stringr) # data cleaning
library(httr)

##### TLDR; GUIDE #### 
# OFFICIAL DOCS: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
# navigating: remDr$navigate("http://www...")
# finding DOM element: remDr$findElement(using = "class", "big-title"), or (using = "xpath", "//h2[@id=abc]), or (using = "css", "input#email-input")
# get DOM element attribute value: element$getElementAttribute("class") .. or "value", "name", "id",.. etc.
# get DOM element text value: element$getElementText()
# click DOM element: buttonElement$clickElement()
# get current URL: remDr$getCurrentUrl()
# get current Title: remDr$getTitle()
# 
# Example, scraping links from one page and extracting URLs into a list.. 
#   multipleLinks <- remDr$findElements(using = "css", ".main-content > a.title-link[href]")
#   urls <- unlist(sapply(multipleLinks, function(x){ x$getElementAttribute("href") }))  <---- this will extract urls, from a list of <a> elements on a webpage..
#   for each url in urls... navigate.. scrape.. store in a list.. export later or whatever
##################################

# IMPORTANT INFO
# The process is composed of two parts a server (the Selenium Server) and a client (the browser you initiate). 
# The close method of the remoteDriver class closes the client (the browser). 
# The server also needs to be stopped when you are finished, by calling rD$server$stop()

## SETUP
# create and launch selenium webdriver server and webdriver client (browser) Chrome
#binman::rm_platform("phantomjs")
#wdman::selenium(retcommand = TRUE)

rD <- rsDriver(verbose = FALSE, port = 8887L, browser = "firefox")
remDr <- rD$client
remDr$open()


## MAIN PROCEDURE

# navigate to URL
#remDr$navigate("http://www.google.com")

job_function <- function(search_page_link, attribute_selector) {
  resp <- GET(search_page_link)
  html <- content(resp)
  link <- html %>% html_nodes("#js-job-content div.b-block__left > h2 > a") %>% html_attr("href")
  for (x in 1:length(link)) {
    link[x] <- paste0("https:", link[x])
  }
  Job_attr1 <- vector("character", length = length(link))
  Job_attr2 <- vector("character", length = length(link))
  Job_attr3 <- vector("character", length = length(link))
  Job_attr4 <- vector("character", length = length(link))
  Job_attr5 <- vector("character", length = length(link))
  Job_attr6 <- vector("character", length = length(link))
  Job_attr7 <- vector("character", length = length(link))
  Job_attr8 <- vector("character", length = length(link))
  
  for (i in 1:length(link)) {
    remDr$navigate(link[i])
    pageHTML <- read_html(remDr$getPageSource()[[1]])
    Job_attr1[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(1) > div.col.p-0.job-requirement-table__data") %>% html_text()
    Job_attr2[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(2) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr3[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(3) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr4[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(4) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr5[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(5) > div.col.p-0.job-requirement-table__data") %>% html_text()
    Job_attr6[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(6) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    Job_attr7[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(7) > div.col.p-0.job-requirement-table__data > p") %>% html_text()
    #Job_attr8[i] <- pageHTML %>% html_nodes("div.job-requirement-table.row > div:nth-child(8) > div.col.p-0.job-requirement-table__data") %>% html_text()
    print(Job_attr1[i])
  }
  Job_info <- data.frame(
    index = 1:length(link),
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
  write.csv(Job_info,"Job_info2.csv", row.names = FALSE)
}
#job_function("https://www.104.com.tw/jobs/search/?ro=2&jobcat=2004000000&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&order=15&asc=0&rostatus=1024&page=7&mode=s&jobsource=student2020", "#job-detail-info > div:nth-child(4) > div.job-detail-panel-content > dl > dd:nth-child(8) > span")
job_function("https://www.104.com.tw/jobs/search/?ro=2&jobcat=2001000000&expansionType=area%2Cspec%2Ccom%2Cjob%2Cwf%2Cwktm&order=11&asc=0&rostatus=1024&page=1&mode=s&jobsource=student2020", "#job-detail-info > div:nth-child(4) > div.job-detail-panel-content > dl > dd:nth-child(8) > span")

## Scraping data examples

#Option 1: extract HTML source, parse data with Rvest
# pageHTML <- read_html(remDr$getPageSource()[[1]])
# someText <- pageHTML %>% html_nodes(".gb_Fd") %>% html_text()
# str(someText)

# Option2: 
# someElement <- remDr$findElement(using = "xpath", "/html/body/div[1]/div[3]/form/div[1]/div[1]/div[3]/center/input[1]")
# someElementText <- someElement$getElementAttribute("value")
# str(someElementText)


## END OF MAIN PROCEDURE

# At the end: close the driver+server
remDr$close()
rD$server$stop()