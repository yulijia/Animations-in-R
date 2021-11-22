library(ggplot2)
library(ggpacman)
library(ggforce)
base_layer <- ggplot() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black"),
  ) +
  coord_fixed(xlim = c(0, 20), ylim = c(0, 20))

blinky_ghost <- tibble(x = c(0, 1, 1, 0, 0), y = c(0, 0, 1, 1, 0), colour = "Blinky")
blinky_moves <- ggpacman::compute_ghost_coord(blinky_ghost)
map_eyes <- c("eye" = "white", "iris" = "black")


blinky_plot <- base_layer +
  coord_fixed(xlim = c(-1, 2), ylim = c(-1, 2)) +
  geom_polygon(
    data = unnest(blinky_moves, "body"),
    mapping = aes(x = x, y = y, fill = colour, colour = colour, group = step),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(breaks = names(map_eyes), values = map_eyes) +
  scale_colour_manual(breaks = names(map_eyes), values = map_eyes) +
  geom_circle(
    data = unnest(blinky_moves, "eyes"),
    mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part, group = step),
    inherit.aes = FALSE
  )

animated_blinky <- blinky_plot + transition_manual(step)


animate(animated_blinky,height = 300, width = 300)
anim_save("../figure/ggpacman.gif")
