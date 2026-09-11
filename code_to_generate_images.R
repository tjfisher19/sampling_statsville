library(tidyverse)
library(cowplot)
library(ggimage)
library(png)
library(grid)
library(patchwork)
library(ggthemes)



icon_size_all <- 0.095
icon_size_sub <- 0.095
label_size_all <- 2.7
label_size_sub <- 3.0



##############################################################
##############################################################
##
##  Generate Population
##
##############################################################
##############################################################

set.seed(1)
## Set up strata and Poisson Rates
pop_data <- data.frame(
  Strata = rep(c("Teens", "Twenty", "Thirty", "Fourty", "Fifty"), each=20),
  Lambda = rep(c(5, 9, 8, 5, 3), each=20)
) |>     ## Generate App counts and assign pictures
  mutate(Apps = rpois(n=100, lambda=Lambda),
         Picture = c(rep(1:10, 10) ),
         Sex = c(rep(rep(c("Male", "Female"), each=5), 10) ) ) |>
  slice_sample(n=100) |>   ## Shuffle everyone before assigning a neighborhood
  mutate(Neighborhood = rep(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                              "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"), each=5) ) |>
  slice_sample(n=100) |>   ## Shuffle again, then assign IDs
  mutate(ID = row_number() ) |>
  ## Adding more random variables here, but I do not want to change the ordering..
  ## Men in their teens will be a little shorter with more variability (especially those 13, 14, 15...)
  ## Men in their upper 50s may have started to see very minor shrinking
  ## Women in their upper 50s also may see shrinking
  mutate(Height = case_when(Sex=="Male" & Strata=="Teens" ~ round(rnorm(n(), mean=67.0, sd=3.3), 1),
                            Sex=="Male" & Strata=="Fifty" ~ round(rnorm(n(), mean=68.5, sd=2.9), 1),
                            Sex=="Male"  ~ round(rnorm(n(), mean=69.1, sd=2.9), 1),
                            Sex=="Female" & Strata=="Fifty" ~ round(rnorm(n(), mean=62.7, sd=2.7), 1),
                            Sex=="Female" ~ round(rnorm(n(), mean=63.7, sd=2.7), 1) ),
         Eye = sample(c("Brown", "Blue", "Hazel"), size=100, replace=TRUE, prob=c(70,20,10) ) )

pop_data |>
  group_by(Strata) |>
  summarize(N=n(), Min = min(Apps), Mean = mean(Apps), Max = max(Apps))

pop_data |>
  group_by(Neighborhood) |>
  summarize(N=n(), Min = min(Apps), Mean = mean(Apps), Max = max(Apps))



make_person_plot <- function(ind, icon_size=0.085, label_size=2.5) {
  
  df <- pop_data |>
    filter(row_number() == ind)
  
  ##############################################
  ## Set up the grid of social media icons
  ##    in groups of 5 for easy counting
  #x_values <- c(2.00, 1.9, 1.8)
  #y_values <- c(2.9, 2.8, 2.7, 2.6, 2.5)
  x_values <- c(1.85, 2.05, 2.25, 2.45, 2.65)
  y_values <- c(2.5, 2.7, 2.9)
  
  
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
  
  
  ################################################
  ## Set up the social media icon image
  
  df_apps <- data.frame(image=rep("gemini_images/Gemini_Generated_Image_yer454yer454yer4.png", df$Apps),
                        x = xs,
                        y = ys)
  
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
  
  ###############################################
  ## Set up the label for each person, "Person ##"
  df_label <- data.frame(x=2.25, y=1.6, label=paste("Person", df$ID) )
  df_sex <- data.frame(x=2.25, y=1.76, label=df$Sex)
  df_eye <- data.frame(x=1.82, y=1.9, label=paste(df$Eye, "\nEyes") )
  df_height <- data.frame(x=2.70, y=1.9, label=paste(sprintf("%.1f", df$Height), "\ninches\ntall") )
  
  ##############################################################
  ## Make the plot
  p <- ggplot() +
    geom_blank() + 
    #annotation_raster(img, xmin = 1.75, xmax = 2.75, ymin = 1.5, ymax = 2.5) +
    annotation_custom(img_grob, xmin = 1.5, xmax = 3.0, ymin = 1.5, ymax = 2.5) +
    geom_image(data=df_apps, aes(x=x, y=y, image=image), 
               size=icon_size, inherit.aes = FALSE) +
    geom_label(data=df_label, aes(x=x, y=y, label=label), 
               size=label_size, fill="white" ) +
    geom_label(data=df_sex, aes(x=x, y=y, label=label),
              size=(label_size-0.8), hjust=0.5 ) +
    geom_text(data=df_eye, aes(x=x, y=y, label=label),
              size=(label_size-1.1), hjust=0 ) +
    geom_text(data=df_height, aes(x=x, y=y, label=label),
              size=(label_size-1.1), hjust=1 ) +
    coord_cartesian(xlim=c(1.825, 2.675), ylim=c(1.575, 2.925), clip="on") +
    theme_map() #+ 
    #theme(plot.background = element_rect(colour = "gray20", fill = NA, linewidth = 0.25) )
  ggsave(paste0("person_cards/person_card_", sprintf("%03d", ind), ".png"),
         plot=p, bg="white",
         width=1, height=1.5, device="png",
         dpi=450)
}


all_plots <- lapply(seq_len(nrow(pop_data)), make_person_plot,
                    icon_size=icon_size_all, label_size=label_size_all)
## all_plots contains the file names

############################################
## Main Plot -- all people

# Batch convert all file paths into a list of drawable objects
plot_list <- lapply(all_plots, function(file) {
  ggdraw() + draw_image(file, scale=0.98)
})

all_people_grid <- wrap_plots(plot_list, ncol=10) +
  plot_annotation(title = "Residents of Statsville") &
  theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.75) )


ggsave("statsville_images/people_all.png", plot=all_people_grid, 
       width=7.5, height=10, units="in",
       dpi=600)

#############################################
#############################################
##
## By strata -- age groups
##
#############################################
#############################################

plot_list <- lapply(all_plots, function(file) {
  ggdraw() + draw_image(file, scale=0.97)
})

teens <- pop_data |>
  filter(Strata=="Teens") |>
  pull(ID)

plot_teens <- wrap_plots(plot_list[teens], ncol=10) &
  theme(plot.margin = margin(t = 2, r = 0.25, b = 0.1, l = 0.25, unit = "pt")) &
  plot_annotation(subtitle = "Teenagers",
                  theme = theme(plot.background = element_rect(colour = "gray20", 
                                                               fill = "white", linewidth = 0.7)
                  ) )

twenty <- pop_data |>
  filter(Strata=="Twenty") |>
  pull(ID)

plot_twenty <- wrap_plots(plot_list[twenty], ncol=10) &
  theme(plot.margin = margin(t = 2, r = 0.25, b = 0.1, l = 0.25, unit = "pt")) &
  plot_annotation(subtitle = "Ages 20-29",
                  theme = theme(plot.background = element_rect(colour = "gray20", 
                                                               fill = "white", linewidth = 0.7)
                  ) )


thirty <- pop_data |>
  filter(Strata=="Thirty") |>
  pull(ID)

plot_thirty <- wrap_plots(plot_list[thirty], ncol=10) &
  theme(plot.margin = margin(t = 2, r = 0.25, b = 0.1, l = 0.25, unit = "pt")) &
  plot_annotation(subtitle = "Ages 30-39",
                  theme = theme(plot.background = element_rect(colour = "gray20", 
                                                               fill = "white", linewidth = 0.7)
                  ) )


forty <- pop_data |>
  filter(Strata=="Fourty") |>
  pull(ID)

plot_forty <- wrap_plots(plot_list[forty], ncol=10) &
  theme(plot.margin = margin(t = 2, r = 0.25, b = 0.1, l = 0.25, unit = "pt")) &
  plot_annotation(subtitle = "Ages 40-49",
                  theme = theme(plot.background = element_rect(colour = "gray20", 
                                                               fill = "white", linewidth = 0.7)
                  ) )


fifty <- pop_data |>
  filter(Strata=="Fifty") |>
  pull(ID)

plot_fifty <- wrap_plots(plot_list[fifty], ncol=10) &
  theme(plot.margin = margin(t = 2, r = 0.25, b = 0.1, l = 0.25, unit = "pt")) &
  plot_annotation(subtitle = "Ages 50-59",
                  theme = theme(plot.background = element_rect(colour = "gray20", 
                                                               fill = "white", linewidth = 0.7)
                  ) )


plot_ages <- wrap_elements(plot_teens) /
  wrap_elements(plot_twenty) /
  wrap_elements(plot_thirty) /
  wrap_elements(plot_forty) /
  wrap_elements(plot_fifty)

ggsave("statsville_images/people_ages.png", plot=plot_ages, scale=1.25,
       width=7.5, height=10, units="in", 
       dpi=600)

#############################################
#############################################
##
## By clusters -- street names
##
#############################################
#############################################

plot_list <- lapply(all_plots, function(file) {
  ggdraw() + draw_image(file, scale=0.98)
})

##  Apple & Beech
street_a <- pop_data |>
  filter(Neighborhood=="A") |>
  pull(ID)

plot_a <- wrap_plots(plot_list[street_a], ncol=5) &
  plot_annotation(subtitle = "Apple Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_b <- pop_data |>
  filter(Neighborhood=="B") |>
  pull(ID)

plot_b <- wrap_plots(plot_list[street_b], ncol=5) &
  plot_annotation(subtitle = "Beech Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

###########################
## Chestnut and Dogwood

street_c <- pop_data |>
  filter(Neighborhood=="C") |>
  pull(ID)

plot_c <- wrap_plots(plot_list[street_c], ncol=5) &
  plot_annotation(subtitle = "Chestnut Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_d <- pop_data |>
  filter(Neighborhood=="D") |>
  pull(ID)

plot_d <- wrap_plots(plot_list[street_d], ncol=5) &
  plot_annotation(subtitle = "Dogwood Drive",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

####################################
## Elm and Falcon

street_e <- pop_data |>
  filter(Neighborhood=="E") |>
  pull(ID)

plot_e <- wrap_plots(plot_list[street_e], ncol=5) &
  plot_annotation(subtitle = "Elm Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )


street_f <- pop_data |>
  filter(Neighborhood=="F") |>
  pull(ID)

plot_f <- wrap_plots(plot_list[street_f], ncol=5) &
  plot_annotation(subtitle = "Falcon Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

########################################
## Goetta and High

street_g <- pop_data |>
  filter(Neighborhood=="G") |>
  pull(ID)

plot_g <- wrap_plots(plot_list[street_g], ncol=5) &
  plot_annotation(subtitle = "Goetta Way",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_h <- pop_data |>
  filter(Neighborhood=="H") |>
  pull(ID)

plot_h <- wrap_plots(plot_list[street_h], ncol=5) &
  plot_annotation(subtitle = "High Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

##########################
## Indigo and Jambalaya

street_i <- pop_data |>
  filter(Neighborhood=="I") |>
  pull(ID)

plot_i <- wrap_plots(plot_list[street_i], ncol=5) &
  plot_annotation(subtitle = "Indigo Avenue",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_j <- pop_data |>
  filter(Neighborhood=="J") |>
  pull(ID)

plot_j <- wrap_plots(plot_list[street_j], ncol=5) &
  plot_annotation(subtitle = "Jambalaya Drive",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#######################################
## Kookaburra & Lemon

street_k <- pop_data |>
  filter(Neighborhood=="K") |>
  pull(ID)

plot_k <- wrap_plots(plot_list[street_k], ncol=5) &
  plot_annotation(subtitle = "Kookaburra Court",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_l <- pop_data |>
  filter(Neighborhood=="L") |>
  pull(ID)

plot_l <- wrap_plots(plot_list[street_l], ncol=5) &
  plot_annotation(subtitle = "Lemon Lane",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#########################################
## Maple & Narwhal

street_m <- pop_data |>
  filter(Neighborhood=="M") |>
  pull(ID)

plot_m <- wrap_plots(plot_list[street_m], ncol=5) &
  plot_annotation(subtitle = "Maple Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )


street_n <- pop_data |>
  filter(Neighborhood=="N") |>
  pull(ID)

plot_n <- wrap_plots(plot_list[street_n], ncol=5) &
  plot_annotation(subtitle = "Narwhal Alley",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

########################################
## Oak and Poplar

street_o <- pop_data |>
  filter(Neighborhood=="O") |>
  pull(ID)

plot_o <- wrap_plots(plot_list[street_o], ncol=5) &
  plot_annotation(subtitle = "Oak Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_p <- pop_data |>
  filter(Neighborhood=="P") |>
  pull(ID)

plot_p <- wrap_plots(plot_list[street_p], ncol=5) &
  plot_annotation(subtitle = "Poplar Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#############################################
## Qinoa & Ramblin

street_q <- pop_data |>
  filter(Neighborhood=="Q") |>
  pull(ID)

plot_q <- wrap_plots(plot_list[street_q], ncol=5) &
  plot_annotation(subtitle = "Quinoa Way",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_r <- pop_data |>
  filter(Neighborhood=="R") |>
  pull(ID)

plot_r <- wrap_plots(plot_list[street_r], ncol=5) &
  plot_annotation(subtitle = "Ramblin Road",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

#########################################
## Sycamore & Truffula

street_s <- pop_data |>
  filter(Neighborhood=="S") |>
  pull(ID)

plot_s <- wrap_plots(plot_list[street_s], ncol=5) &
  plot_annotation(subtitle = "Sycamore Street",
                  theme = theme(plot.background = element_rect(colour = "gray20", fill = "white", linewidth = 0.5),
                                plot.margin = margin(1, 1, 1, 1, unit = "pt")
                  ) )

street_t <- pop_data |>
  filter(Neighborhood=="T") |>
  pull(ID)

plot_t <- wrap_plots(plot_list[street_t], ncol=5) &
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
       width=7.5, height=10, units="in", dpi=600)


##################################
## PDF versions

ggsave("statsville_images/statsville_people_all.pdf", plot=all_people_grid,
       width=7.5, height=10, units="in", dpi=600)

ggsave("statsville_images/statsville_people_ages.pdf", plot=plot_ages,
       width=7.5, height=10, units="in", dpi=600)

ggsave("statsville_images/statsville_people_streets.pdf", plot=plot_streets,
       width=7.5, height=10, units="in", dpi=600)
