library(here)
library(ggplot2)
library(ggforce)
library(gganimate)
library(ggtext)
library(dplyr)
library(tidyr)
library(purrr)
library(mctemplates)

### Data time ======================================================================================
pacman_grid_coord <- function() {
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
    # mutate(type = list(c("wall", "wall_black"))) %>%
    # unnest("type") %>%
    bind_rows(tibble(x = 9, y = 15, xend = 11, yend = 15, type = "door"))
}

bonus_points_coord <- function() {
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

  bind_rows(
    left_bonus_points,
    tribble(
      ~x, ~y, ~type,
      10, c(1, 21), "normal"
    ),
    mutate(left_bonus_points, x = abs(x - 20))
  ) %>%
    unnest("y")
}

pacman_state <- function() {
  tribble(
    ~state, ~start, ~end,
    "open_right", 14 / 6 * pi, 4 / 6 * pi,
    "close_right", 15 / 3 * pi, 3 / 6 * pi,
    "open_up", 11 / 6 * pi, 1 / 6 * pi,
    "close_up", 12 / 3 * pi, 0 / 6 * pi,
    "open_left", 8 / 6 * pi, - 2 / 6 * pi,
    "close_left", 9 / 3 * pi, - 3 / 6 * pi,
    "open_down", 5 / 6 * pi, - 5 / 6 * pi,
    "close_down", 6 / 3 * pi, - 6 / 6 * pi
  )
}

make_pacman_coord <- function(data) {
  data %>%
    unnest(c("x", "y")) %>%
    mutate(
      state_x = sign(x - lag(x)),
      state_y = sign(y - lag(y)),
      state = case_when(
        (is.na(state_x) | state_x %in% 0) & (is.na(state_y) | state_y %in% 0) ~ list(c("open_right", "close_right")),
        state_x == 1 & state_y == 0 ~ list(c("open_right", "open_right")),
        state_x == -1 & state_y == 0 ~ list(c("open_left", "close_left")),
        state_x == 0 & state_y == -1 ~ list(c("open_down", "close_down")),
        state_x == 0 & state_y == 1 ~ list(c("open_up", "close_up"))
      )
    )  %>%
    unnest("state") %>%
    mutate(step = 1:n(), colour = "pacman") %>%
    left_join(y = pacman_state(), by = "state")
}

make_ghost_coord <- function(data) {
  base_ghost <- function() {
    ghost_arc <- unnest(tribble(
      ~x0, ~y0, ~r, ~start, ~end, ~part,
      0, 0, 0.5, - 1 * pi / 2, 1 * pi / 2, "top",
      c(-1 / 6, 1 / 6), -0.5 + 1/8, 0.125, pi / 2, 3 * pi / 2, "bottom",
      -0.5, -0.5 + 1/8, 0.125, 2 * pi / 4, 4 * pi / 4, "bottom",
      0.5, -0.5 + 1/8, 0.125, - 2 * pi / 4, - 4 * pi / 4, "bottom",
      c(-1 / 3, 0, 1 / 3), -0.5 + 1/8, 1 / 24, - 1 * pi / 2, 1 * pi / 2, "bottom",
    ), "x0")

    top <- ggplot() +
      geom_arc_bar(
        data = ghost_arc[1, ],
        mapping = aes(x0 = x0, y0 = y0, r0 = 0, r = r, start = start, end = end)
      ) +
      coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))

    top_polygon <- ggplot_build(top)$data[[1]][, c("x", "y")]

    bottom <- ggplot() +
      geom_arc_bar(
        data = ghost_arc[c(5, 3, 2, 4), ],
        mapping = aes(x0 = x0, y0 = y0, r0 = 0, r = 0.17, start = start, end = end)
      ) +
      coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))

    bottom_polygon <- ggplot_build(bottom)$data[[1]][, c("x", "y")]

    bind_rows(
      top_polygon[-nrow(top_polygon), ],
      tibble(x = 0.5, y = -0.5 + 1/8),
      bottom_polygon
    )
  }

  eyes_circle <- tribble(
    ~x0, ~y0, ~r, ~part,
    1/5, 1/8, 1/8, "eye",
    -1/5, 1/8, 1/8, "eye",
    5/20, 1/8, 1/20, "iris",
    -3/20, 1/8, 1/20, "iris"
  )

  bg <- base_ghost()

  ghost_out <- data %>%
    unnest(c("x", "y")) %>%
    mutate(state = list(1:4)) %>%
    unnest("state") %>%
    group_by(colour) %>%
    mutate(
      step = 1:n(),
      noise_x = rnorm(n(), mean = 0, sd = 0.05),
      noise_y = rnorm(n(), mean = 0, sd = 0.05),
      body = pmap(
        .l = list(x, y, noise_x, noise_y),
        .f = function(.x, .y, .noise_x, .noise_y) {
          mutate(
            .data = bg,
            x = x + .x + .noise_x,
            y = y + .y + .noise_y
          )
        }
      ),
      eyes = pmap(
        .l = list(x, y, noise_x, noise_y),
        .f = function(.x, .y, .noise_x, .noise_y) {
          mutate(
            .data = eyes_circle,
            x0 = x0 + .x + .noise_x,
            y0 = y0 + .y + .noise_y
          )
        }
      ),
      x = NULL,
      y = NULL
    ) %>%
    ungroup()

  ghost_out
}

pacman_moves <- tribble(
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
  c(1:4, 4:1), 18,
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
  make_pacman_coord()

segments <- pacman_grid_coord()
bonus_points <- bonus_points_coord()

bonus_points_eaten <- right_join(bonus_points, pacman_moves, by = c("x", "y")) %>%
  distinct(step, x, y, type) %>%
  mutate(
    step = map2(step, max(step), ~ seq(.x, .y, 1)),
    colour = "eaten"
  ) %>%
  unnest("step")

ghosts_vulnerability <- bonus_points_eaten %>%
  filter(type == "big") %>%
  group_by(x, y) %>%
  summarise(step = min(step)) %>%
  ungroup() %>%
  (function(data) unlist(map(data[["step"]], ~seq(.x, .x + 30, 1))))()

blinky <- tribble(
  ~colour, ~x, ~y,
  "Blinky", c(9, 9), c(13, 13),
  "Blinky", 9, 13:14,
  "Blinky", 10, 14:16,
  "Blinky", 11:13, 16,
  "Blinky", 13, 15:11,
  "Blinky", 12:7, 11,
  "Blinky", 7, 10:8,
  "Blinky", 8:9, 8,
  "Blinky", 9, 7:6,
  "Blinky", 8:5, 6,
  "Blinky", 5, 5:3,
  "Blinky", 4:3, 3,
  "Blinky", 3, 4:6,
  "Blinky", 2:1, 6,
  "Blinky", 1, 7:8,
  "Blinky", 2:5, 8,
  "Blinky", 5, 9:18,
  "Blinky", 4:1, 18,
  "Blinky", 1, 18:25
)

pinky <- tribble(
  ~colour, ~x, ~y,
  "Pinky", rep(11, 70), rep(13, 70)
)

inky <- tribble(
  ~colour, ~x, ~y,
  "Inky", rep(9, 70), rep(14, 70)
)

clyde <- tribble(
  ~colour, ~x, ~y,
  "Clyde", rep(11, 70), rep(14, 70)
)


### Plot time ======================================================================================
map_colours <- c(
  "wall" = "dodgerblue3", "door" = "dodgerblue3",
  "normal" = "goldenrod1", "big" = "goldenrod1", "eaten" = "black",
  "pacman" = "yellow",
  "eye" = "white", "iris" = "black",
  "Blinky" = "red", "Pinky" = "pink", "Inky" = "cyan", "Clyde" = "orange",
  "Blinky_weak" = "blue", "Pinky_weak" = "blue", "Inky_weak" = "blue", "Clyde_weak" = "blue"
)

base_grid <- ggplot() +
  theme_void(base_family = "xkcd") +
  theme(legend.position = "none") +
  labs(caption = "© Mickaël '<i style='color:#21908CFF;'>Coeos</i>' Canouil") +
  # theme_light() + theme(panel.grid.minor = element_blank()) +
  # scale_x_continuous(breaks = 0:21, sec.axis = dup_axis()) +
  # scale_y_continuous(breaks = 0:26, sec.axis = dup_axis()) +
  scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5, "eaten" = 3)) +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) +
  theme(
    plot.caption = element_textbox_simple(halign = 1, colour = "white"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black")
  )  +
  coord_fixed(xlim = c(0, 20), ylim = c(0, 26)) +
  geom_segment(
    data = segments,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, size = type, colour = type),
    lineend = "round",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = bonus_points,
    mapping = aes(x = x, y = y, size = type, colour = type),
    inherit.aes = FALSE
  )

p_pacman <- base_grid +
  geom_point(
    data = bonus_points_eaten,
    mapping = aes(x = x, y = y, colour = colour, size = colour, group = step),
    inherit.aes = FALSE
  ) +
  geom_arc_bar(
    data = pacman_moves,
    mapping = aes(x0 = x, y0 = y, r0 = 0, r = 0.5, start = start, end = end, colour = colour, fill = colour, group = step),
    inherit.aes = FALSE
  )

p_ghosts <- p_pacman +
  map(list(blinky, pinky, inky, clyde), .f = function(data) {
    ghost_moves <- make_ghost_coord(data) %>%
      mutate(colour = ifelse(step %in% ghosts_vulnerability, paste0(colour, "_weak"), colour))
    list(
      geom_polygon(
        data = unnest(ghost_moves, "body"),
        mapping = aes(x = x, y = y, fill = colour, colour = colour, group = step),
        inherit.aes = FALSE
      ),
      geom_circle(
        data = unnest(ghost_moves, "eyes"),
        mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part, group = step),
        inherit.aes = FALSE
      )
    )
  })

animate(
  plot = p_ghosts + transition_manual(step),
  width = 3.7 * 2.54,
  height = 4.7 * 2.54,
  units = "cm",
  res = 120,
  bg = "black",
  renderer = gifski_renderer(file = here("figures", "pacman.gif"))
)
