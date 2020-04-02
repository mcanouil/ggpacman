library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)
library(purrr)
library(gganimate)

ghost_iris <- tibble(
  angle = seq(0, 360, by = 5) * pi / 180,
  r = 1/20,
  part = "iris",
  step_seq = 1:length(angle),
  max_seq = max(step_seq)
) %>%
  mutate(side = list(c("left", "right"))) %>%
  unnest("side") %>%
  left_join(
    y = tibble(side = c("left", "right"), x_offset = c(-1/5, 1/5)),
    by = "side"
  ) %>%
  mutate(
    x_normal = r * cos(angle) + x_offset,
    y_normal = r * sin(angle) + 1/8,
    x_crazy = ifelse(
      side == "left",
      x_normal,
      r * cos(angle + 45 * pi / 180) + x_offset
    ),
    y_crazy = ifelse(
      side == "left",
      y_normal,
      r * sin(angle + 45 * pi / 180) + 1/8
    ),
    x_dead = ifelse(
      side == "left",
      x_normal,
      r * cos(angle + 90 * pi / 180) + x_offset
    ),
    y_dead = ifelse(
      side == "left",
      y_normal,
      r * sin(angle + 90 * pi / 180) + 1/8
    )
  ) %>%
  pivot_longer(
    cols = c(x_normal, y_normal, x_crazy, y_crazy, x_dead, y_dead),
    names_to = c(".value", "state"),
    names_sep = "_"
  ) %>%
  mutate(
    state = factor(state, levels = c("normal", "crazy", "dead")),
    step = step_seq + (as.numeric(state) - 1) * max_seq
  ) %>%
  mutate(step = as.numeric(step))

ghost_body <- get(data("ghost_body", package = "ggpacman")) %>%
  mutate(
    polygon_order = 1:n(),
    max_seq = max(ghost_iris[["max_seq"]]),
    step_seq = list(c(1:unique(max_seq))),
    state = list(c("normal", "crazy", "dead"))
  ) %>%
  unnest("step_seq") %>%
  unnest("state") %>%
  mutate(
    state = factor(state, levels = c("normal", "crazy", "dead")),
    step = step_seq + (as.numeric(state) - 1) * max_seq
  ) %>%
  mutate(step = as.numeric(step))

ghost <- full_join(
  x = ghost_iris %>%
    select(step, state, x, y, r) %>%
    group_by(step, state) %>%
    nest() %>%
    ungroup(),
  y = ghost_body %>%
    select(step, state, x, y) %>%
    group_by(step, state) %>%
    nest() %>%
    ungroup(),
  by = c("state", "step"),
  suffix = c(".iris", ".body")
) %>%
  arrange(step) %>%
  mutate(
    x_noise = map2(step, state!="crazy", ~rnorm(5 * (.y + 1), 0, 0.01)),
    y_noise = map2(step, state!="crazy", ~rnorm(5 * (.y + 1), 0, 0.01))
  ) %>%
  unnest(c("x_noise", "y_noise")) %>%
  mutate(
    data.iris = pmap(
      .l = list(data.iris, x_noise, y_noise),
      .f = ~mutate(..1, x = x + ..2, y = y + ..3)
    ),
    data.body = pmap(
      .l = list(data.body, x_noise, y_noise),
      .f = ~mutate(..1, x = x + ..2, y = y + ..3)
    ),
    data.eyes = pmap(
      .l = list(x_noise, y_noise),
      .f = function(xn, yn) {
        mutate(
          .data = tribble(
            ~x0, ~y0, ~r, ~part,
            1/5, 1/8, 1/8, "eye",
            -1/5, 1/8, 1/8, "eye"
          ),
          x0 = x0 + xn,
          y0 = y0 + yn
        )
      }
    ),
    step = 1:n(),
    x_noise = NULL,
    y_noise = NULL
  )

map_colours <- c("normal" = "red", "crazy" = "blue", "dead" = "transparent")
R = 1
p <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    panel.background = element_rect(fill = "transparent", colour = "transparent"),
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(breaks = names(map_colours), values = map_colours) +
  scale_colour_manual(breaks = names(map_colours), values = map_colours) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed() +
  geom_regon(
    mapping = aes(x0 = 0, y0 = -1/5, sides = 6, angle = 90 * pi / 180, r = R),
    colour = "dodgerblue3", fill = "dodgerblue3"
  ) +
  geom_regon(
    mapping = aes(x0 = 0, y0 = -1/5, sides = 6, angle = 90 * pi / 180, r = R * 0.92),
    colour = "black", fill = "black"
  ) +
  geom_polygon(
    data = unnest(ghost, "data.body"),
    mapping = aes(x = x, y = y, fill = state, colour = state, group = step),
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = unnest(ghost, "data.eyes"),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    colour = "white",
    fill = "white",
    inherit.aes = FALSE
  ) +
  geom_circle(
    data = unnest(ghost, "data.iris"),
    mapping = aes(x0 = x, y0 = y, r = r, group = step),
    colour = "black",
    fill = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    mapping = aes(x = 0, y = -6/10, label = "ggpacman"),
    size = 6, family = "xkcd", colour = "goldenrod1"
  )

animate(
  plot = p + transition_manual(step),
  width = 4.39,
  height = 5.08,
  units = "cm",
  res = 200,
  bg = "transparent",
  duration = 5,
  renderer = gifski_renderer(file = here::here("man", "figures", "ggpacman.gif"))
)
