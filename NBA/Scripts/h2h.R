# clear console and workspace
cat("\014")
rm(list = ls())

# create remote driver for dailyfantasynerd website
startServer()
remDr <- remoteDriver(browserName = "chrome")
remDr$open()

remDr$navigate("https://chrome.google.com/webstore/detail/rotogrinders-fanduel-tool/felhhccenjfgepphdanniaeclbjhklca?hl=en")
# Sys.sleep(5)
addToChrome <- remDr$findElement("xpath", '/html/body/div[5]/div[1]/div/div/div[2]/div[3]/div/div[1]/div/div[2]')
addToChrome$clickElement()
# remDr$acceptAlert()

# navigate to login page
remDr$navigate("https://www.fanduel.com/p/login")
# type username and password and submit form
username <- remDr$findElement("id", "email")
username$sendKeysToElement(list("aitken711@gmail.com"))
password <- remDr$findElement("id", "password")
password$sendKeysToElement(list("scroll$7"))
submit <- remDr$findElement("name", "login")
submit$clickElement()

# navigate to fanduel
# remDr$navigate("https://www.fanduel.com")
NBA <- remDr$findElement("xpath", '//*[@id="ui-skeleton"]/div/div/section/section[1]/div[2]/ul/li[2]/a/i')
remDr$mouseMoveToLocation(webElement = NBA)
remDr$click()
H2H <- remDr$findElement("xpath", '//*[@id="ui-skeleton"]/div/div/section/section[1]/div[3]/ul/li[4]/a/span')
remDr$mouseMoveToLocation(webElement = H2H)
remDr$click()

a <- remDr$findElement("class name", "sharkdetails")
remDr$mouseMoveToLocation(webElement = a)
remDr$click()

# download fanduel table
pool <- readHTMLTable(remDr$getPageSource()[[1]], stringsAsFactors = F)[[1]]