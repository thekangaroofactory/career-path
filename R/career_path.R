

library(ggplot2)
library(ggtext)
library(ragg)
library(RColorBrewer)

career_path <- function(data, profile_title, profile_description){

  # -- colors
  plot_background_fill <- "grey98"
  background_color_fill <- "grey95"
  background_color <- "grey85"
  text_color <- "grey30"
  
  # -- segments
  segment_contract_x <- 75
  segment_size_y <- 4
  segment_alpha <- 0.7
  segment_text_size <- 3
  segment_text_nudge_y <- -0.4
  
  # -- category & section
  category_nudge_x <- 2000
  section_nudge_x <- 1300
  
  # -- company
  company_y <- -1.5
  
  # -- timeline
  timeline_text_y <- -0.5
  
  # -- axis
  expand_limits_x <- 365
  
    
  # -- init
  data %>% 
    ggplot() +
    
    # -- experiences & skills --------------------------------------------------
    # -- segments
    geom_segment(
      aes(x = start.date + segment_contract_x,
          xend = end.date - segment_contract_x,
          y = level,
          yend = level,
          colour = as.numeric(start.date)),
      linewidth = segment_size_y,
      lineend = 'round', 
      alpha = segment_alpha) +
    
    # -- apply color palette (on start.date)
    scale_color_distiller(palette = "OrRd", trans = "reverse") +
    
    # -- labels: title
    geom_text(
      aes(
        x = start.date + segment_contract_x,
        y = level,
        label = title,
        hjust = 0),
      nudge_y = segment_text_nudge_y,
      size = segment_text_size,
      colour = text_color) +
    
    # -- labels: sub-title-1
    geom_text(
      aes(
        x = start.date + segment_contract_x,
        y = level,
        label = sub.title.1,
        hjust = 0),
      nudge_y = segment_text_nudge_y * 2,
      size = segment_text_size,
      colour = text_color) +
    
    # -- labels: sub-title-2
    geom_text(
      aes(
        x = start.date + segment_contract_x,
        y = level,
        label = sub.title.2,
        hjust = 0),
      nudge_y = segment_text_nudge_y * 3,
      size = segment_text_size,
      colour = text_color) +
  
    # -- axis (timeline) -------------------------------------------------------
    # -- line
    geom_hline(
      aes(yintercept = 0),
      colour = background_color) +
      
    # -- ticks
    geom_point(
      data = timeline,
      aes(
        x = start.date, 
        y = 0), 
      shape = "triangle down filled",
      fill = background_color,
      size = 2) +
        
    # -- legends
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
    
    # -- category titles
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
    
    # -- section titles
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
      aes(x = start.date + segment_contract_x,
          xend = end.date - segment_contract_x,
          y = company_y,
          yend = company_y,
          colour = as.numeric(start.date)),
      linewidth = segment_size_y, 
      lineend = 'round', 
      alpha = segment_alpha) +
    
    # -- labels
    geom_text(
      data = company,
      aes(
        x = start.date + segment_contract_x,
        y = company_y,
        label = company,
        hjust = 0),
      nudge_y = segment_text_nudge_y,
      size = segment_text_size,
      colour = text_color) +
    
    # -- description layer -----------------------------------------------------
    # -- profile title
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
    
    # -- profile description
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
    # expand behond end.date to allow more rooms for labels
    expand_limits(x = max(data$end.date) + expand_limits_x) +
    
    # -- theme -----------------------------------------------------------------
    theme(
      
      # -- background & grid
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = plot_background_fill),
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
