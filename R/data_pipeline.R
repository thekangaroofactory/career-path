

library(dplyr)



data_pipeline <- function(data){
  
  # -- replace date NAs by current date
  data[is.na(data$end.date), ]$end.date <- Sys.Date()
 
  # -- turn category & section into factors
  #data$category <- factor(data$category, levels = c("Expériences", "Compétences"))
  #data$section <- factor(data$section, levels = c("Rôles", "Projets", "Data", "Fonctionelles"))
  data$category <- factor(data$category)
  data$section <- factor(data$section)
  
  
  # -- insert levels
  # 1 level for everyone (make extra room for the timeline)
  # 2 levels between Projet/Roles & Data/Generales
  data$level <- data$level + 1
  #data[data$section %in% c("Data", "Fonctionelles"), ]$level <- data[data$section %in% c("Data", "Fonctionelles"), ]$level + 2
  data[data$section %in% levels(data_2$section)[1:2], ]$level <- data[data$section %in% levels(data_2$section)[1:2], ]$level + 2
  
  # -- order & return
  data %>%
    arrange(category, section)
  
}



company_pipeline <- function(data){
  
  data %>%
    filter(company != "") %>%
    select(start.date, end.date, company) %>%
    group_by(company) %>%
    summarise(start.date = min(start.date),
              end.date = max(end.date))
  
}


category_pipeline <- function(data){
  
  data %>%
    select(category, level, start.date) %>%
    group_by(category) %>%
    summarise(max = max(level),
              mean = mean(level),
              x = min(start.date)) %>%
    mutate(x = min(x))
    
}


section_pipeline <- function(data){
  
  data %>%
    select(section, level, start.date) %>%
    group_by(section) %>%
    summarise(max = max(level),
              mean = mean(level),
              min =  min(level),
              x = min(start.date)) %>%
    mutate(x = min(x))
  
}


timeline_pipeline <- function(data){
  
  data %>%
    reframe(label = unique(format(start.date, "%Y")),
              start.date = as.Date(paste0(label, "-01-01")))
  
}
