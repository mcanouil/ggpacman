library(here)
library(ggplot2)
library(ggforce)
library(gganimate)
library(dplyr)
library(tidyr)
library(purrr)
library(ggtext)


### Helper functions ===============================================================================
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

compute_pacman_coord <- function(data) {
  pacman_state <- tribble(
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

  data %>%
    unnest(c("x", "y")) %>%
    mutate(
      state_x = sign(x - lag(x)),
      state_y = sign(y - lag(y)),
      state = case_when(
        (is.na(state_x) | state_x %in% 0) & (is.na(state_y) | state_y %in% 0) ~ list(c("open_right", "close_right")),
        state_x == 1 & state_y == 0 ~ list(c("open_right", "close_right")),
        state_x == -1 & state_y == 0 ~ list(c("open_left", "close_left")),
        state_x == 0 & state_y == -1 ~ list(c("open_down", "close_down")),
        state_x == 0 & state_y == 1 ~ list(c("open_up", "close_up"))
      )
    )  %>%
    unnest("state") %>%
    mutate(step = 1:n()) %>%
    left_join(y = pacman_state, by = "state")
}

compute_points_eaten <- function(bonus_points, pacman_moves) {
  right_join(bonus_points, pacman_moves, by = c("x", "y")) %>%
    distinct(step, x, y, type) %>%
    mutate(
      step = map2(step, max(step), ~ seq(.x, .y, 1)),
      colour = "eaten"
    ) %>%
    unnest("step")
}

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

make_ghost_coord <- function(data) {
  eyes_circle <- tribble(
    ~x0, ~y0, ~r, ~part, ~direction,
    1/5, 1/8, 1/8, "eye", c("up", "down", "right", "left", "middle"),
    -1/5, 1/8, 1/8, "eye", c("up", "down", "right", "left", "middle"),
    5/20, 1/8, 1/20, "iris", "right",
    -3/20, 1/8, 1/20, "iris", "right",
    1/5, 1/16, 1/20, "iris", "down",
    -1/5, 1/16, 1/20, "iris", "down",
    3/20, 1/8, 1/20, "iris", "left",
    -5/20, 1/8, 1/20, "iris", "left",
    1/5, 3/16, 1/20, "iris", "up",
    -1/5, 3/16, 1/20, "iris", "up",
    1/5, 1/8, 1/20, "iris", "middle",
    -1/5, 1/8, 1/20, "iris", "middle"
  ) %>%
    unnest("direction")

  bg <- base_ghost()

  ghost_out <- data %>%
    unnest(c("x", "y")) %>%
    mutate(
      X0 = x,
      Y0 = y,
      state_x = sign(round(x) - lag(round(x))),
      state_y = sign(round(y) - lag(round(y))),
      direction = case_when(
        (is.na(state_x) | state_x %in% 0) & (is.na(state_y) | state_y %in% 0) ~ "middle",
        state_x == 1 & state_y == 0 ~ "right",
        state_x == -1 & state_y == 0 ~ "left",
        state_x == 0 & state_y == -1 ~ "down",
        state_x == 0 & state_y == 1 ~ "up"
      )
    ) %>%
    unnest("direction") %>%
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
        .l = list(x, y, noise_x, noise_y, direction),
        .f = function(.x, .y, .noise_x, .noise_y, .direction) {
          mutate(
            .data = filter(eyes_circle, direction == .direction),
            x0 = x0 + .x + .noise_x,
            y0 = y0 + .y + .noise_y,
            direction = NULL
          )
        }
      ),
      x = NULL,
      y = NULL
    ) %>%
    ungroup()

  ghost_out
}

compute_ghost_status <- function(ghost, pacman_moves, bonus_points_eaten) {
  ghosts_vulnerability <- bonus_points_eaten %>%
    filter(type == "big") %>%
    group_by(x, y) %>%
    summarise(step_init = min(step)) %>%
    ungroup() %>%
    mutate(
      step = map(step_init, ~seq(.x, .x + 30, 1)),
      vulnerability = TRUE,
      x = NULL, y = NULL
    ) %>%
    unnest("step")

  ghost_out <- left_join(
    x = make_ghost_coord(ghost),
    y = pacman_moves %>%
      mutate(ghost_eaten = TRUE) %>%
      select(X0 = x, Y0 = y, step, ghost_eaten),
    by = c("X0", "Y0", "step")
  ) %>%
    left_join(y = ghosts_vulnerability, by = "step") %>%
    mutate(
      vulnerability = replace_na(vulnerability, FALSE),
      ghost_name = colour,
      ghost_eaten = ghost_eaten & vulnerability,
      colour = ifelse(vulnerability, paste0(ghost_name, "_weak"), colour)
    )

  pos_eaten_start <- which(ghost_out[["ghost_eaten"]])
  ghosts_home <- which(ghost_out[["X0"]] == 10 & ghost_out[["Y0"]] == 14)
  for (ipos in pos_eaten_start) {
    # if (any(ghosts_home>=ipos)) {
      pos_eaten_end <- min(ghosts_home[ghosts_home>=ipos])
      ghost_out[["colour"]][ipos:pos_eaten_end] <- paste0(unique(ghost_out[["ghost_name"]]), "_eaten")
    # }
  }

  left_join(
    x = ghost_out,
    y = ghost_out %>%
      filter(step == step_init & grepl("eaten", colour)) %>%
      mutate(already_eaten = TRUE) %>%
      select(step_init, already_eaten),
      by = "step_init"
  ) %>%
    mutate(
      colour = case_when(
        already_eaten & X0 == 10 & Y0 == 14 ~ paste0(ghost_name, "_eaten"),
        grepl("weak", colour) & already_eaten ~ ghost_name,
        TRUE ~ colour
      )
    )
}


### Set moves ======================================================================================
pacman_offset <- 11
ghosts_offset <- 9

pacman <- tribble(
  ~x, ~y,
  rep(10, pacman_offset), rep(6, pacman_offset),
  c(11:12), 6,
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
  c(8:9), 16,
  rep(10, 4), rep(16, 4)
) %>%
  mutate(colour = "Pac-Man")
max(compute_pacman_coord(pacman)$step)

blinky <- tribble(
  ~x, ~y,
  rep(10, ghosts_offset), rep(16, ghosts_offset),
  9:7, 16,
  7, 15:13,
  6:5, 13,
  5, 12:8,
  4, 8,
  rep(5, 3), rep(8, 3), # dead
  6, 8,
  6, 9,
  7, 9,
  7, 10,
  8, 10,
  8, 11,
  9, 11,
  9, 12,
  10, 12,
  10, 13,
  rep(10, 4), rep(14, 4),
  10, 15, # alive
  10:13, 16,
  13, 15:8,
  12:11, 8,
  11, 7:6,
  12:15, 6,
  15, 6:3,
  16:17, 3,
  17, 4:6,
  17:19, 6,
  19, 7:8
) %>%
  mutate(colour = "Blinky")
max(make_ghost_coord(blinky)$step)

pinky <- tribble(
  ~x, ~y,
  rep(9, ghosts_offset * 2), rep(13, ghosts_offset * 2),
  9, 14,
  10, 14:16,
  11:13, 16,
  13, 15:11,
  12:7, 11,
  7, 10:8,
  8:9, 8,
  9, 7:6,
  8:5, 6,
  5, 5:3,
  4:3, 3,
  3, 4:6,
  2:1, 6,
  1, 7:8,
  2:5, 8,
  5, 9:18,
  4:3, 18
) %>%
  mutate(colour = "Pinky")
max(make_ghost_coord(pinky)$step)

inky <- tribble(
  ~x, ~y,
  rep(11, ghosts_offset * 3), rep(13, ghosts_offset * 3),
  11, 14,
  10, 14:16,
  9:7, 16,
  7, 15:8,
  8:9, 8,
  9, 7:6,
  10:15, 6,
  15, 6:3,
  16:19, 3,
  19, 2:1,
  18:6, 1
) %>%
  mutate(colour = "Inky")
max(make_ghost_coord(inky)$step)

clyde <- tribble(
  ~x, ~y,
  rep(10, ghosts_offset * 4), rep(13, ghosts_offset * 4),
  10, 14:16,
  11, 16,
  11, 17:18,
  12:13, 18,
  13, 19:21,
  14:15, 21,
  15, 22:25,
  16:17, 25, # dead
  16, 25,
  15, 25:24,
  14, 23:22,
  13, 21:20,
  12, 19:18,
  11, 17:16,
  10, 15,
  rep(10, 4), rep(14, 4),
  10, 15, # alive
  10, 16,
  10, 16,
  10, 16
) %>%
  mutate(colour = "Clyde")
max(make_ghost_coord(clyde)$step)


### Setup data and variables =======================================================================
segments <- pacman_grid_coord()
bonus_points <- bonus_points_coord()
pacman_moves <- compute_pacman_coord(pacman)
bonus_points_eaten <- compute_points_eaten(bonus_points, pacman_moves)
map_colours <- c(
  "READY!" = "goldenrod1",
  "wall" = "dodgerblue3", "door" = "dodgerblue3",
  "normal" = "goldenrod1", "big" = "goldenrod1", "eaten" = "black",
  "Pac-Man" = "yellow",
  "eye" = "white", "iris" = "black",
  "Blinky" = "red", "Blinky_weak" = "blue", "Blinky_eaten" = "transparent",
  "Pinky" = "pink", "Pinky_weak" = "blue", "Pinky_eaten" = "transparent",
  "Inky" = "cyan", "Inky_weak" = "blue", "Inky_eaten" = "transparent",
  "Clyde" = "orange", "Clyde_weak" = "blue", "Clyde_eaten" = "transparent"
)


### Plot time ======================================================================================
base_grid <- ggplot() +
  theme_void(base_family = "xkcd") +
  theme(legend.position = "none") +
  labs(caption = "© Mickaël '<i style='color:#21908CFF;'>Coeos</i>' Canouil") +
  scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5, "eaten" = 3)) +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) +
  theme(
    plot.caption = element_textbox_simple(halign = 0.5, colour = "white"),
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
  ) +
  geom_text(
    data = tibble(x = 10, y = 11, label = "READY!", step = 1:20),
    mapping = aes(x = x, y = y, label = label, colour = label, group = step),
    size = 6
  )

p_points <- list(
  geom_point(
    data = bonus_points_eaten,
    mapping = aes(x = x, y = y, colour = colour, size = colour, group = step),
    inherit.aes = FALSE
  )
)

p_pacman <- list(
  geom_arc_bar(
    data = pacman_moves,
    mapping = aes(x0 = x, y0 = y, r0 = 0, r = 0.5, start = start, end = end, colour = colour, fill = colour, group = step),
    inherit.aes = FALSE
  )
)

p_ghosts <- map(.x = list(blinky, pinky, inky, clyde), .f = function(data) {
  ghost_moves <- compute_ghost_status(
    ghost = data,
    pacman_moves = pacman_moves,
    bonus_points_eaten = bonus_points_eaten
  )
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

# base_grid + p_pacman +
#   theme_light() + theme(panel.grid.minor = element_blank(), legend.position = "none") +
#   scale_x_continuous(breaks = 0:21, sec.axis = dup_axis()) +
#   scale_y_continuous(breaks = 0:26, sec.axis = dup_axis())

# base_grid + p_points + p_pacman + p_ghosts + facet_wrap(vars(step==230))


### Animate ========================================================================================
animate(
  plot = base_grid  + p_points + p_pacman+ p_ghosts + transition_manual(step),
  width = 3.7 * 2.54,
  height = 4.7 * 2.54,
  units = "cm",
  res = 120,
  bg = "black",
  duration = 10,
  renderer = gifski_renderer(file = here("figures", "pacman.gif"))
)
