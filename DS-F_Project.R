install.packages("rvest")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("mosaicData")

library(rvest)
library(dplyr)
library(tidyverse)

course_details = data.frame();

for (x in 21:50) { # LOOP FOR 21 TO 50 PAGES, 16 ENTRIES EACH PAGE
  
  link = paste0("https://www.tutorialspoint.com/latest/courses?page=",x)
  page = read_html(link)
  
  course_name = page %>% html_nodes(".pb-5 a") %>% html_text()
  
  publisher =  page %>% html_nodes(".h24 a") %>% html_text()
  
  lectures =  page %>% html_nodes(".mb-0:nth-child(1)") %>% html_text()
  
  duration =  page %>% html_nodes(".mb-0+ .mb-0") %>% html_text()
  
  price = page %>% html_nodes(".price, .price-free") %>% html_attr("data-usd")
  
  #BIND NEW DATAFRAME WITH PREVIOUS
  course_details = rbind(course_details, data.frame(
    course_name=course_name, 
    publisher = publisher, 
    lectures = lectures, 
    duration = duration, 
    price = price, 
    stringsAsFactors = FALSE))
}

# REMOVE " LECTURES" FROM LECTURES COLUMN
course_details$lectures = gsub("([0-9]+) Lectures", "\\1", course_details$lectures)

# CONVERT LECTURES COLUMN INTO INTEGER
course_details$lectures = as.integer(as.character(course_details$lectures))

# ALL FREE COURSES(NA) CONVERTED TO 0
course_details$price[is.na(course_details$price)] <- 0
# CONVERT PRICE COLUMN INTO INTEGER
course_details$price = as.numeric(as.character(course_details$price))

# CONVERT LECTURES COLUMN INTO INTEGER 
course_details$lectures = as.integer(as.character(course_details$lectures))

course_details$price_type[which(course_details$price <= 15)] <- "affordable"
course_details$price_type[which(course_details$price <= 08)] <- "cheap"
course_details$price_type[which(course_details$price == 00)] <- "free"
course_details$price_type[which(course_details$price  > 15)] <- "expensive"

# DIGITS REMOVED FROM DURATION, ONLY UNITS ARE TAKEN
course_details$time_unit = gsub("[ 0-9]*.[0-9]* ( *)", "\\1", course_details$duration)


# UNIT REMOVED FROM DURATION, ONLY TIME TAKEN
course_details$time = gsub("(0-9.)* [a-z, A-Z]*", "\\1", course_details$duration)

# CONVERT TIME COLUMN INTO INTEGER
course_details$time = as.numeric(as.character(course_details$time))

# IF TIME UNIT IS HOUR, MULTIPLY WITH 60
course_details$time_in_min <- ifelse(course_details$time_unit=="hour", course_details$time * 60, course_details$time)

#REMOVE ALL UNNECESSARY COLUMNS
course_details <- course_details[,!names(course_details) %in% c("duration", "time_unit", "time")]

course_details

############# MEAN MEDIAN MODE ##############

#MEAN OF COURSE FEE
mean_price = mean(course_details$price)

if (mean_price > 15) {
  print(paste0("Mean Price of Tutorial Point's Course is: USD ", round(mean_price,2), ". Most of the courses are expensive!"))
} else if  (mean_price > 8) {
  print(paste0("Mean Price of Tutorial Point's Course is: USD ", round(mean_price,2), ". Most of the courses are affordable!"))
} else if  (mean_price > 0) {
  print(paste0("Mean Price of Tutorial Point's Course is: USD ", round(mean_price,2), ". Most of the courses are cheap!"))
} else {
  print(paste0("Mean Price of Tutorial Point's Course is: USD ", round(mean_price,2), ". Every Courses are free!"))
}

#MEDIAN OF DURATION
median_time = median(course_details$time_in_min)
print(paste0("Median of Duration of the courses is ", median_time, " minutes."))

#MODE OF COURSE FEES
unique_price <- unique(course_details$price)
print(paste0("Mode Fees of COurses: USD ", unique_price[which.max(tabulate(match(course_details$price, unique_price)))]))

library(ggplot2)
library(mosaicData)

ggplot(data = course_details, mapping = aes(y = time_in_min, x = price)) + geom_point()

ggplot(course_details, aes(price_type)) + geom_bar()

ggplot(course_details, aes(publisher)) + geom_bar()

hist(course_details$time_in_min[!course_details$time_in_min >1700], breaks=1000)





