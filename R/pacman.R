library(ggplot2)
library(ggforce)
library(gganimate)
library(ggtext)
library(dplyr)
library(tidyr)
library(purrr)
library(mctemplates)

left_vertical_segments <- tribble(
  ~x, ~y, ~xend, ~yend,
  0, 0, 0, 9,
  0, 17, 0, 26,
  2, 4, 2, 5,
  2, 19, 2, 20,
  2, 22, 2, 24,
  4, 4, 4, 7,
  4, 9, 4, 12,
  4, 14, 4, 17,
  4, 19, 4, 20,
  4, 22, 4, 24,
  6, 2, 6, 5,
  6, 9, 6, 12,
  6, 14, 6, 20,
  6, 22, 6, 24,
  8, 4, 8, 5,
  8, 9, 8, 10,
  8, 12, 8, 15,
  8, 19, 8, 20,
  8, 22, 8, 24
)

centre_vertical_segments <- tribble(
  ~x, ~y, ~xend, ~yend,
  10, 2, 10, 4,
  10, 7, 10, 9,
  10, 17, 10, 19,
  10, 22, 10, 26
)

left_horizontal_segments <- tribble(
  ~x, ~y, ~xend, ~yend,
  0, 0, 10, 0,
  2, 2, 8, 2,
  0, 4, 2, 4,
  8, 4, 10, 4,
  0, 5, 2, 5,
  8, 5, 10, 5,
  2, 7, 4, 7,
  6, 7, 8, 7,
  0, 9, 4, 9,
  8, 9, 10, 9,
  8, 10, 10, 10,
  0, 12, 4, 12,
  8, 12, 10, 12,
  0, 14, 4, 14,
  8, 15, 9, 15,
  0, 17, 4, 17,
  6, 17, 8, 17,
  2, 19, 4, 19,
  8, 19, 10, 19,
  2, 20, 4, 20,
  8, 20, 10, 20,
  2, 22, 4, 22,
  6, 22, 8, 22,
  2, 24, 4, 24,
  6, 24, 8, 24,
  0, 26, 10, 26
)

left_segments <- bind_rows(
  left_vertical_segments,
  left_horizontal_segments
)

right_segments <- left_segments %>%
  mutate(
    x = abs(x - 20),
    xend = abs(xend - 20)
  )

segments <- bind_rows(
  left_segments,
  centre_vertical_segments,
  right_segments
) %>%
  mutate(type = "wall") %>%
  bind_rows(
    tibble(x = 9, y = 15, xend = 11, yend = 15, type = "door")
  )

left_bonus_points <- tribble(
  ~x, ~y, ~type,
  1, c(1:3, 7:8, 18:22, 24:25), "normal",
  1, c(6, 23), "big",
  2, c(1, 3, 6, 8, 18, 21, 25), "normal",
  3, c(1, 3:6, 8, 18, 21, 25), "normal",
  4, c(1, 3, 8, 18, 21, 25), "normal",
  5, c(1, 3:25), "normal",
  6, c(1, 6, 8, 21, 25), "normal",
  7, c(1, 3:6, 8, 18:21, 25), "normal",
  8, c(1, 3, 6, 8, 18, 21, 25), "normal",
  9, c(1:3, 6:8, 18, 21:25), "normal"
)

bonus_points <- bind_rows(
  left_bonus_points,
  tribble(
    ~x, ~y, ~type,
    10, c(1, 21), "normal"
  ),
  mutate(left_bonus_points, x = abs(x - 20))
) %>%
  unnest("y")

state_pacman <-tribble(
  ~state, ~start, ~end,
  "open", 14 / 6 * pi, 4 / 6 * pi,
  "close", 15 / 3 * pi, 3 / 6 * pi
)
pacman <- tribble(
  ~x, ~y,
  10, 6,
  10, 6,
  c(10:12), 6,
  13, c(6:3),
  12, 3,
  11, c(3:1),
  10, 1,
  c(9:2), 1,
  1, c(1:3),
  2, 3,
  3, c(3:5),
  c(3:1), 6,
  1, 7,
  c(1:4), 8,
  5, c(8:25),
  c(4:1), 25,
  1, c(25:18),
  c(1:4, 3:1), 18,
  1, c(18:21),
  c(1:19), 21,
  19, c(22:25),
  c(18:15), 25,
  15, c(25:13),
  c(15:20, 0:7), 13,
  7, c(14:16),
  c(8:10), 16,
  10, 16,
  10, 16,
  10, 16
) %>%
  unnest(c("x", "y")) %>%
  mutate(state = list(c("open", "close"))) %>%
  unnest("state") %>%
  mutate(step = 1:n()) %>%
  left_join(y = state_pacman, by = "state")

bonus_points_eaten <- right_join(bonus_points, pacman, by = c("x", "y")) %>%
  distinct(step, x, y, type) %>%
  mutate(step = map2(step, max(step), ~ seq(.x, .y, 1))) %>%
  unnest("step")

p <- ggplot() +
  theme_void(base_family = "xkcd") +
  # theme_light() +
  theme(
    plot.caption = element_textbox_simple(halign = 1, colour = "white"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black")
  )  +
  coord_fixed(xlim = c(0, 20), ylim = c(0, 26)) +
  scale_x_continuous(breaks = 0:21) +
  scale_y_continuous(breaks = 0:26) +
  scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5)) +
  geom_segment(
    data = segments,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, size = type),
    colour = "dodgerblue3",
    lineend = "round",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_point(
    data = bonus_points,
    mapping = aes(x = x, y = y, size = type),
    colour = "goldenrod2",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_point(
    data = bonus_points_eaten,
    mapping = aes(x = x, y = y, group = step),
    colour = "black",
    size = 3,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_arc_bar(
    data = pacman,
    mapping = aes(x0 = x, y0 = y, r0 = 0, r = 0.5, start = start, end = end, group = step),
    fill = "goldenrod",
    colour = "goldenrod",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  labs(caption = "© Mickaël '<i style='color:#21908CFF;'>Coeos</i>' Canouil") +
  transition_manual(step)
  # transition_time(step)

animate(
  plot = p,
  width = 3.7 * 2.54,
  height = 4.7 * 2.54,
  units = "cm",
  res = 120,
  bg = "black",
  renderer = gifski_renderer(file = "man/figures/pacman.gif")
)

