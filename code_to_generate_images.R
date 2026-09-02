library(tidyverse)
library(ggimage)
library(png)
library(grid)
library(patchwork)
library(ggthemes)



icon_size_all <- 0.085
icon_size_sub <- 0.095
label_size_all <- 2.5
label_size_sub <- 3.0

make_person_plot <- function(df, icon_size=0.085, label_size=2.5) {
  
  ##############################################
  ## Set up the grid of social media icons
  ##    in groups of 5 for easy counting
  #x_values <- c(2.00, 1.9, 1.8)
  #y_values <- c(2.9, 2.8, 2.7, 2.6, 2.5)
  x_values <- c(1.95, 2.10, 2.25, 2.40, 2.55)
  y_values <- c(2.6, 2.75, 2.9)
  
  
  # if(df$Apps > 10) {
  #   xs <- c(rep(x_values[1], 5), rep(x_values[2], 5), rep(x_values[3], df$Apps-10) )
  #   ys <- c(y_values, y_values, y_values[1:(df$Apps-10)])
  # } else if(df$Apps > 5){
  #   ys <- c(y_values, y_values[1:(df$Apps-5)])
  #   xs <- c(rep(x_values[1], 5), rep(x_values[2], df$Apps-5) )
  # } else {
  #   ys <- y_values[1:df$Apps]
  #   xs <- rep(x_values[1], df$Apps)
  # }
  
  if(df$Apps > 10) {
    ys <- c(rep(y_values[1], 5), rep(y_values[2], 5), rep(y_values[3], df$Apps-10) )
    xs <- c(x_values, x_values, x_values[1:(df$Apps-10)])
  } else if(df$Apps > 5){
    xs <- c(x_values, x_values[1:(df$Apps-5)])
    ys <- c(rep(y_values[1], 5), rep(y_values[2], df$Apps-5) )
  } else {
    xs <- x_values[1:df$Apps]
    ys <- rep(y_values[1], df$Apps)
  }
  
  df_apps <- data.frame(image=rep("gemini_images/Gemini_Generated_Image_yer454yer454yer4.png", df$Apps),
                        x = xs,
                        y = ys)
  
  ###############################################
  ## Set up the label for each person, "Person ##"
  df_label <- data.frame(x=2.25, y=1.6, label=paste("Person", df$ID) )
  
  ################################################
  ## Set up the human figure for each person
  if(df$Picture == 1) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_t345jkt345jkt345.png")
  } else if(df$Picture == 2) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_ew98qkew98qkew98.png")
  } else if(df$Picture == 3) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_yfkelhyfkelhyfke.png")
  } else if(df$Picture == 4) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_svh5msvh5msvh5ms.png")
  } else if(df$Picture == 5) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_oaqw62oaqw62oaqw.png")
  } else if(df$Picture == 6) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_jg5pkujg5pkujg5p.png")
  } else if(df$Picture == 7) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_fn3wi8fn3wi8fn3w.png")
  } else if(df$Picture == 8) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_7ru49q7ru49q7ru4.png")
  } else if(df$Picture == 9) {
    img <- readPNG("gemini_images/Gemini_Generated_Image_q6alt4q6alt4q6al.png")
  } else {
    img <- readPNG("gemini_images/Gemini_Generated_Image_ekhqavekhqavekhq.png")
  }
  
  img_grob <- rasterGrob(img, interpolate = TRUE)
  
  ##############################################################
  ## Make the plot
  ggplot() +
    geom_blank() + 
    #annotation_raster(img, xmin = 1.75, xmax = 2.75, ymin = 1.5, ymax = 2.5) +
    annotation_custom(img_grob, xmin = 1.5, xmax = 3.0, ymin = 1.5, ymax = 2.5) +
    geom_image(data=df_apps, aes(x=x, y=y, image=image), 
               size=icon_size, inherit.aes = FALSE) +
    geom_label(data=df_label, aes(x=x, y=y, label=label), 
               size=label_size, fill="white" ) +
    coord_cartesian(xlim=c(1.75, 2.75), ylim=c(1.5, 3), clip="on") +
    theme_map() #+ 
    #theme(plot.background = element_rect(colour = "gray20", fill = NA, linewidth = 0.25) )
  
}


set.seed(1)
## Set up strata and Poisson Rates
pop_data <- data.frame(
  Strata = rep(c("Teens", "Twenty", "Thirty", "Fourty", "Fifty"), each=20),
  Lambda = rep(c(5, 9, 8, 5, 3), each=20)
) |>     ## Generate App counts and assign pictures
  mutate(Apps = rpois(n=100, lambda=Lambda),
         Picture = c(rep(1:10, 10) ) ) |>
  slice_sample(n=100) |>   ## Shuffle everyone before assigning a neighborhood
  mutate(Neighborhood = rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                              "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"), each=5) ) |>
  slice_sample(n=100) |>   ## Shuffle again, then assign IDs
  mutate(ID = row_number() )

pop_data |>
  group_by(Strata) |>
  summarize(N=n(), Min = min(Apps), Mean = mean(Apps), Max = max(Apps))

pop_data |>
  group_by(Neighborhood) |>
  summarize(N=n(), Min = min(Apps), Mean = mean(Apps), Max = max(Apps))

all_plots <- lapply(split(pop_data, seq_len(nrow(pop_data))), make_person_plot,
                    icon_size=icon_size_all, label_size=label_size_all)

############################################
## Main Plot -- all people

plot_grid <- wrap_plots(all_plots, ncol=10) +
  plot_annotation(title = "Residents of Statsville") &
  theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.35) )
  

ggsave("statsville_images/people_all.png", plot=plot_grid, 
       width=7.5, height=9.8, units="in",
       dpi=600)

#############################################
#############################################
##
## By strata -- age groups
##
#############################################
#############################################
all_plots <- lapply(split(pop_data, seq_len(nrow(pop_data))), make_person_plot,
                    icon_size=icon_size_sub, label_size=label_size_sub)
                    
teens <- pop_data |>
  filter(Strata=="Teens") |>
  pull(ID)

plot_teens <- wrap_plots(all_plots[teens], ncol=10) &
  plot_annotation(subtitle = "Teenagers",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 1.0),
                                plot.margin = margin(2, 2, 2, 2, unit = "pt")
                  ) )

twenty <- pop_data |>
  filter(Strata=="Twenty") |>
  pull(ID)

plot_twenty <- wrap_plots(all_plots[twenty], ncol=10) &
  plot_annotation(subtitle = "Ages 20-29",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 1.0),
                                plot.margin = margin(2, 2, 2, 2, unit = "pt")
                  ) )

thirty <- pop_data |>
  filter(Strata=="Thirty") |>
  pull(ID)

plot_thirty <- wrap_plots(all_plots[thirty], ncol=10) &
  plot_annotation(subtitle = "Ages 30-39",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 1.0),
                                plot.margin = margin(2, 2, 2, 2, unit = "pt")
                  ) )

forty <- pop_data |>
  filter(Strata=="Fourty") |>
  pull(ID)

plot_forty <- wrap_plots(all_plots[forty], ncol=10) &
  plot_annotation(subtitle = "Ages 40-49",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 1.0),
                                plot.margin = margin(2, 2, 2, 2, unit = "pt")
                  ) )

fifty <- pop_data |>
  filter(Strata=="Fifty") |>
  pull(ID)

plot_fifty <- wrap_plots(all_plots[fifty], ncol=10) &
  plot_annotation(subtitle = "Ages 50-59",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 1.0),
                                plot.margin = margin(2, 2, 2, 2, unit = "pt")
                  ) )

plot_ages <- wrap_elements(plot_teens) /
  wrap_elements(plot_twenty) /
  wrap_elements(plot_thirty) /
  wrap_elements(plot_forty) /
  wrap_elements(plot_fifty)

ggsave("statsville_images/people_ages.png", plot=plot_ages, scale=1.25,
       width=7.5, height=9.8, units="in", 
       dpi=600)

#############################################
#############################################
##
## By clusters -- street names
##
#############################################
#############################################

##  Apple & Beech
street_a <- pop_data |>
  filter(Neighborhood=="A") |>
  pull(ID)

plot_a <- wrap_plots(all_plots[street_a], ncol=5) &
  plot_annotation(subtitle = "Apple Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_b <- pop_data |>
  filter(Neighborhood=="B") |>
  pull(ID)

plot_b <- wrap_plots(all_plots[street_b], ncol=5) &
  plot_annotation(subtitle = "Beech Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

###########################
## Chestnut and Dogwood

street_c <- pop_data |>
  filter(Neighborhood=="C") |>
  pull(ID)

plot_c <- wrap_plots(all_plots[street_c], ncol=5) &
  plot_annotation(subtitle = "Chestnut Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_d <- pop_data |>
  filter(Neighborhood=="D") |>
  pull(ID)

plot_d <- wrap_plots(all_plots[street_d], ncol=5) &
  plot_annotation(subtitle = "Dogwood Drive",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

####################################
## Elm and Falcon

street_e <- pop_data |>
  filter(Neighborhood=="E") |>
  pull(ID)

plot_e <- wrap_plots(all_plots[street_e], ncol=5) &
  plot_annotation(subtitle = "Elm Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )


street_f <- pop_data |>
  filter(Neighborhood=="F") |>
  pull(ID)

plot_f <- wrap_plots(all_plots[street_f], ncol=5) &
  plot_annotation(subtitle = "Falcon Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

########################################
## Goetta and High

street_g <- pop_data |>
  filter(Neighborhood=="G") |>
  pull(ID)

plot_g <- wrap_plots(all_plots[street_g], ncol=5) &
  plot_annotation(subtitle = "Goetta Way",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_h <- pop_data |>
  filter(Neighborhood=="H") |>
  pull(ID)

plot_h <- wrap_plots(all_plots[street_h], ncol=5) &
  plot_annotation(subtitle = "High Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

##########################
## Indigo and Jambalaya

street_i <- pop_data |>
  filter(Neighborhood=="I") |>
  pull(ID)

plot_i <- wrap_plots(all_plots[street_i], ncol=5) &
  plot_annotation(subtitle = "Indigo Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_j <- pop_data |>
  filter(Neighborhood=="J") |>
  pull(ID)

plot_j <- wrap_plots(all_plots[street_j], ncol=5) &
  plot_annotation(subtitle = "Jambalaya Drive",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#######################################
## Kookaburra & Lemon

street_k <- pop_data |>
  filter(Neighborhood=="K") |>
  pull(ID)

plot_k <- wrap_plots(all_plots[street_k], ncol=5) &
  plot_annotation(subtitle = "Kookaburra Court",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_l <- pop_data |>
  filter(Neighborhood=="L") |>
  pull(ID)

plot_l <- wrap_plots(all_plots[street_l], ncol=5) &
  plot_annotation(subtitle = "Lemon Lane",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#########################################
## Maple & Narwhal

street_m <- pop_data |>
  filter(Neighborhood=="M") |>
  pull(ID)

plot_m <- wrap_plots(all_plots[street_m], ncol=5) &
  plot_annotation(subtitle = "Maple Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )


street_n <- pop_data |>
  filter(Neighborhood=="N") |>
  pull(ID)

plot_n <- wrap_plots(all_plots[street_n], ncol=5) &
  plot_annotation(subtitle = "Narwhal Alley",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

########################################
## Oak and Poplar

street_o <- pop_data |>
  filter(Neighborhood=="O") |>
  pull(ID)

plot_o <- wrap_plots(all_plots[street_o], ncol=5) &
  plot_annotation(subtitle = "Oak Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_p <- pop_data |>
  filter(Neighborhood=="P") |>
  pull(ID)

plot_p <- wrap_plots(all_plots[street_p], ncol=5) &
  plot_annotation(subtitle = "Poplar Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#############################################
## Qinoa & Ramblin

street_q <- pop_data |>
  filter(Neighborhood=="Q") |>
  pull(ID)

plot_q <- wrap_plots(all_plots[street_q], ncol=5) &
  plot_annotation(subtitle = "Quinoa Way",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_r <- pop_data |>
  filter(Neighborhood=="R") |>
  pull(ID)

plot_r <- wrap_plots(all_plots[street_r], ncol=5) &
  plot_annotation(subtitle = "Ramblin Road",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#########################################
## Sycamore & Truffula

street_s <- pop_data |>
  filter(Neighborhood=="S") |>
  pull(ID)

plot_s <- wrap_plots(all_plots[street_s], ncol=5) &
  plot_annotation(subtitle = "Sycamore Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_t <- pop_data |>
  filter(Neighborhood=="T") |>
  pull(ID)

plot_t <- wrap_plots(all_plots[street_t], ncol=5) &
  plot_annotation(subtitle = "Truffula Road",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )


plot_streets <- (wrap_elements(plot_a) | wrap_elements(plot_b) ) /
  (wrap_elements(plot_c) | wrap_elements(plot_d) ) /
  (wrap_elements(plot_e) | wrap_elements(plot_f) ) /
  (wrap_elements(plot_g) | wrap_elements(plot_h) ) /
  (wrap_elements(plot_i) | wrap_elements(plot_j) ) /
  (wrap_elements(plot_k) | wrap_elements(plot_l) ) /
  (wrap_elements(plot_m) | wrap_elements(plot_n) ) /
  (wrap_elements(plot_o) | wrap_elements(plot_p) ) /
  (wrap_elements(plot_q) | wrap_elements(plot_r) ) /
  (wrap_elements(plot_s) | wrap_elements(plot_t) )

ggsave("statsville_images/people_streets.png", plot=plot_streets, scale=1.5,
       width=7.5, height=9.8, units="in", dpi=600)
