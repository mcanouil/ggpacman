# library(ggplot2)
# library(ggtext)
# library(ggforce)
# library(tidyr)
# library(dplyr)
# library(purrr)
# library(gganimate)

make_ghost <- function(data) {
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

red_ghost <- tribble(
  ~colour, ~x, ~y,
  "ghost1", 0:10, 0,
  "ghost2", c(0, 0, 0:10), 1,
  "ghost3", 3:10, 0.5,
  "ghost4", c(10, 10:5), 2
) %>%
  make_ghost()

p <- ggplot() +
  theme_void(base_family = "xkcd") +
  # theme_light() +
  theme(
    plot.caption = element_textbox_simple(halign = 1, colour = "white"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black")
  ) +
  coord_fixed(xlim = c(-1, 11), ylim = c(-1, 3)) +
  geom_polygon(
    data = unnest(red_ghost, "body"),
    mapping = aes(x = x, y = y, fill = colour, colour = colour),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_circle(
    data = filter(unnest(red_ghost, "eyes"), part == "eye"),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    colour = "white",
    fill = "white",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_circle(
    data = filter(unnest(red_ghost, "eyes"), part == "iris"),
    mapping = aes(x0 = x0, y0 = y0, r = r),
    colour = "black",
    fill = "black",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("orange", "red", "blue", "pink")) +
  scale_colour_manual(values = c("orange", "red", "blue", "pink"))

animate(
  plot = p + transition_manual(step),
  width = 3.7 * 2.54,
  height = 1 * 2.54,
  units = "cm",
  res = 120,
  bg = "black",
  renderer = gifski_renderer(file = "figures/ghosts.gif")
)
