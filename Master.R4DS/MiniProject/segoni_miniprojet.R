### Mini-projet : animation du mot "MAP5" en sinusoïde
### Lorenzo Segoni — M1 Ingénierie Mathématique et Biostatistique

#install.packages("tidyverse")
#install.packages("gganimate")
#install.packages("av")
library(av)
library(tidyverse)
library(gganimate)

### Nombre d’images
n_frames <- 250

### Fonction d'onde
y_wave <- function(n, amplitude = 0.4, freq = 2, del) {
  amplitude * sin(seq(0, 2 * pi * freq + del, length.out = n))
}

### Données pour chaque lettre
df_M <- tibble(frame = 1:n_frames, x = seq(-17, 11, length.out = n_frames),
               y = y_wave(n_frames, 0.4, 2, 0), letter = "M")
df_A <- tibble(frame = 1:n_frames, x = seq(-15, 13, length.out = n_frames),
               y = y_wave(n_frames, 0.4, 2, 0.8), letter = "a")
df_P <- tibble(frame = 1:n_frames, x = seq(-13, 15, length.out = n_frames),
               y = y_wave(n_frames, 0.4, 2, 1.6), letter = "p")
df_5 <- tibble(frame = 1:n_frames, x = seq(-11, 17, length.out = n_frames),
               y = y_wave(n_frames, 0.4, 2, 2.4), letter = "5")

### Fusion de toutes les lettres
df_all <- bind_rows(df_M, df_A, df_P, df_5) %>%
  mutate(angle_x_val = (x + 10) * 10)

### Graphique non animé
p <- ggplot(df_all, aes(x = x, y = y, group = letter)) +
  geom_text(
    aes(label = letter, angle = angle_x_val),
    color = "navy",
    size = 25,
    fontface = "italic",
    family = "serif",
    vjust = -0.3
  ) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill = gray(0.9), color = NA)) +
  transition_manual(frame)

### Animation et sauvegarde
#mp4
animate(p, fps = 30, duration = 8, width = 600, height = 300, renderer = av_renderer("MAP5_defile.mp4"))
#Autre methode
#anim <- animate( p, fps = 30, duration = 8, width = 600, height = 300, renderer = av_renderer() )
#anim_save("MAP5_defile.mp4", animation = anim)

#gif
animate( p, fps = 30, duration = 8, width = 600, height = 300, renderer = gifski_renderer("MAP5_defile.gif") )
#Autre methode
#anim <- animate( p, fps = 30, duration = 8, width = 600, height = 300, renderer = gifski_renderer() )
#anim_save("MAP5_defile.gif", animation = anim)