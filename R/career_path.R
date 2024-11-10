

library(ggplot2)
library(ggtext)
library(ragg)
library(RColorBrewer)

career_path <- function(data, profile_title, profile_description){

  # -- colors
  background_color_fill <- "grey95"
  background_color <- "grey85"
  text_color <- "grey30"
  
  # -- segments
  bar_height <- 0.5

  rect_alpha <- 0.7
  

  text_size <- 3
  text_nudge_y <- -0.4
  
  seg_contract <- 75
  
  RANGE_SIZE <- 4
  
  # -- timeline
  timeline_text_y <- -0.5
  
  # -- category & section
  category_nudge_x <- 2000
  section_nudge_x <- 1300
  
  # -- company
  y_company <- -1.5
  
  
    
  # -- init
  data %>% 
    ggplot() +
    
    # -- experiences & compétences ---------------------------------------------
    # -- segments
    geom_segment(
      aes(x = start.date + seg_contract,
          xend = end.date - seg_contract,
          y = level,
          yend = level,
          colour = as.numeric(start.date)),
      linewidth = RANGE_SIZE,
      lineend = 'round', 
      alpha = rect_alpha) +
    
    # scale_colour_manual(values = segment_fill) +
    scale_color_distiller(palette = "OrRd", trans = "reverse") +
    
    # -- labels: title
    geom_text(
      aes(
        x = start.date + seg_contract,
        y = level,
        label = title,
        hjust = 0),
      nudge_y = text_nudge_y,
      size = text_size,
      colour = text_color) +
    
    # -- labels: sub-title-1
    geom_text(
      aes(
        x = start.date + seg_contract,
        y = level,
        label = sub.title.1,
        hjust = 0),
      nudge_y = text_nudge_y * 2,
      size = text_size,
      colour = text_color) +
    
    # -- labels: sub-title-2
    geom_text(
      aes(
        x = start.date + seg_contract,
        y = level,
        label = sub.title.2,
        hjust = 0),
      nudge_y = text_nudge_y * 3,
      size = text_size,
      colour = text_color) +
  
    # -- axis (timeline) -------------------------------------------------------
  
    geom_hline(
      aes(yintercept = 0),
      colour = background_color) +
      
    geom_point(
      data = timeline,
      aes(
        x = start.date, 
        y = 0), 
      shape = "triangle down filled",
      fill = background_color,
      size = 2) +
        
    geom_text(
      data = timeline,
      aes(
        x = start.date, 
        y = timeline_text_y, 
        label = label), 
      size = 3,
      fontface = "bold",
      colour = text_color,
      hjust = 0.5) +
    
    # -- category / section ----------------------------------------------------
    # -- separators
    geom_hline(
      data = category,
      aes(yintercept = max + 1),
      linetype = "dotted",
      linewidth = 0.5,
      colour = background_color) +
    
    # -- category
    geom_text(
      data = category,
      aes(x = x - category_nudge_x,
          y = mean,
          label = category),
      angle = 90,
      hjust = 0.5,
      family = "Grandview",
      fontface = "bold",
      colour = background_color,
      alpha = 50,
      size = 8) +
    
    # -- section
    geom_text(
      data = section,
      aes(x = x - section_nudge_x,
          y = min,
          label = section),
      hjust = 0,
      family = "Grandview",
      fontface = "bold",
      colour = text_color,
      size = 5) +
    
    
    # -- company ---------------------------------------------------------------
    # -- segments
    geom_segment(
      data = company,
      aes(x = start.date + seg_contract,
          xend = end.date - seg_contract,
          y = y_company,
          yend = y_company,
          colour = as.numeric(start.date)),
      linewidth = RANGE_SIZE, 
      lineend = 'round', 
      alpha = rect_alpha) +
    
    # -- labels
    geom_text(
      data = company,
      aes(
        x = start.date + seg_contract,
        y = y_company,
        label = company,
        hjust = 0),
      nudge_y = text_nudge_y,
      size = text_size,
      colour = text_color) +
    
    # -- description layer -----------------------------------------------------
    
    ggtext::geom_textbox(
      data = tibble(
        x = min(section$x),
        y = max(data_2$level) + 5,
        label = profile_title),
      aes(
        x = x - section_nudge_x,
        y = y,
        label = label),
      hjust = 0,
      vjust = 1,
      fill = NA,
      box.colour = NA,
      box.padding = unit(c(0, 0, 0, 0), "pt"),
      family = "Grandview",
      fontface = "bold",
      colour = text_color,
      size = 8) +
    
    ggtext::geom_textbox(
      data = tibble(
        x = max(data_2$end.date),
        y = max(data_2$level) + 5,
        label = profile_description),
      aes(
        x = x,
        y = y,
        label = label),
      hjust = 1,
      vjust = 1,
      minwidth = unit(0.5, "npc"),
      size = 3.5,
      colour = text_color,
      fill = background_color_fill,
      box.colour = NA) +
  
    
    # -- axis ------------------------------------------------------------------
    expand_limits(x = as.Date("2026-12-31")) +
    
    # -- theme -----------------------------------------------------------------
    # ggtitle("Career Path") +
    
    #theme_minimal() +
  
    theme(
      
      # -- background
      panel.background = element_blank(),
      panel.border = element_blank(),
      
      plot.background = element_rect(fill = "grey98"),
      panel.grid = element_blank(),
      
      # -- axis
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      
      # -- legend
      legend.position = "none")
  
}
