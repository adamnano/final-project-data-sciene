library(rvest)
library(httr)
resp <- GET("https://www.104.com.tw/jobs/search/?ro=0&isnew=14&kwop=7&keyword=%E5%AF%A6%E7%BF%92%20intern&order=7&asc=0&sr=99&rostatus=1024&page=2&mode=s&jobsource=intern_hot")
html <- content(resp)
link <- html %>% html_nodes("#js-job-content div.b-block__left > h2 > a") %>% html_attr("href")

for (x in 1:length(link))
{
  link[x] <- paste0("https:", link[x])
}

Job_attr <- vector("character", length = length(link))

#for (i in 1:length(link))
#{
#  resp_b <- GET(link[i])
#  html_b <- content(resp_b)
  # html_nodes("#app > div.container.jb-container.pt-4.position-relative > div > div.col.main > div.dialog.container-fluid.bg-white.rounded.job-description.mb-4.pt-6.pb-6 > div.job-description-table.row > div:nth-child(9) > div.col.p-0.job-description-table__data > p") %>%
  #Job_attr[i] <- html_b #%>% 
#  target <- html_nodes(html_b, xpath = "/html/body/div[2]/div[2]/div/div[1]/div[2]/div[2]/div[2]/div[2]/p")
#  text_output <- html_text(target)
#  print(text_output)
#}

#Job_attr


#resp_b <- GET("https://www.104.com.tw/job/73qr9?jobsource=intern_hot")
#html_b <- content(resp_b)
#vysledok <- html_b %>% 
#  html_nodes(xpath = "/html/body/div[2]/div[6]/div/div[2]") %>%
#  html_text()
#vysledok

#stringr::str_detect("<p data-v-865e422e=\"\" data-v-a1bba18e=\"\" class=\"t3 mb-0\">.*</p>") %>% 

library(magrittr)
vysledok <- readLines(url("https://www.1111.com.tw/job/92112965/")) %>% 
  stringr::str_detect("日班") %>% 
  any()
vysledok



library(RSelenium)
#specify the url
url <- 'https://www.104.com.tw/job/73qr9?jobsource=intern_hot'

#Create the remote driver / navigator
rsd <- rsDriver(browser = "firefox")
remDr <- rsd$client

#Go to your url
remDr$navigate(url)
page <- read_html(remDr$getPageSource()[[1]])

#get your horses data by parsing Selenium page with Rvest as you know to do
page %>% html_nodes(xpath = "/html/body/div[2]/div[2]/div/div[1]/div[2]/div[2]/div[3]/div[2]/p") %>% html_text()

