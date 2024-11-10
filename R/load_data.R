

load_data <- function(filename){
  
  path <- "E:/Portfolio/R/Projects"
  #filename <- "career_path_data_en.csv"
  
  # -- build url
  url <- file.path(path, filename)
  
  # -- colClasses
  colClasses <- c("category" = "character",
                  "section" = "character",
                  "level" = "numeric",
                  "start.date" = "character",
                  "end.date" = "character",
                  "title" = "character",
                  "sub.title.1" = "character",
                  "sub.title.2" = "character",
                  "company" = "character")
  
  # -- read data
  data <- read.csv(file = url,
                   header = TRUE,
                   colClasses = colClasses)
  
  # -- date
  data$start.date <- as.Date(data$start.date)
  data$end.date <- as.Date(data$end.date)
  
  # -- return
  data
  
}
