draw_ghost <- function(x, y, colour) {
  ghost_arc <- tribble(
    ~x0, ~y0, ~r, ~start, ~end, ~part,
    0, 0, 0.5, - 1 * pi / 2, 1 * pi / 2, "top",
    c(-1 / 6, 1 / 6), -0.5 + 1/8, 0.125, pi / 2, 3 * pi / 2, "bottom",
    -0.5, -0.5 + 1/8, 0.125, 2 * pi / 4, 4 * pi / 4, "bottom",
    0.5, -0.5 + 1/8, 0.125, - 2 * pi / 4, - 4 * pi / 4, "bottom",
    c(-1 / 3, 0, 1 / 3), -0.5 + 1/8, 1 / 24, - 1 * pi / 2, 1 * pi / 2, "bottom",
  ) %>%
    unnest("x0")

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

  eyes_circle <- tribble(
    ~x0, ~y0, ~r, ~part,
    1/5, 1/8, 1/8, "eye",
    -1/5, 1/8, 1/8, "eye",
    5/20, 1/8, 1/20, "iris",
    -3/20, 1/8, 1/20, "iris"
  )

  ghost <- bind_rows(
    top_polygon[-nrow(top_polygon), ],
    tibble(x = 0.5, y = -0.5 + 1/8),
    bottom_polygon
  )

  # ggplot() +
  # theme_void() +
  list(
    geom_polygon(
      data = ghost,
      mapping = aes(x = x + {{ x }}, y = y + {{ y }}),
      fill = colour,
      colour = colour
    ),
    geom_circle(
      data = filter(eyes_circle, part == "eye"),
      mapping = aes(x0 = x0 + {{ x }}, y0 = y0+ {{ y }}, r = r),
      colour = "white",
      fill = "white",
      inherit.aes = FALSE
    ),
    geom_circle(
      data = filter(eyes_circle, part == "iris"),
      mapping = aes(x0 = x0 + {{ x }}, y0 = y0+ {{ y }}, r = r),
      colour = "black",
      fill = "black",
      inherit.aes = FALSE
    )
  )
}

p <- ggplot() +
  theme_void(base_family = "xkcd") +
  # theme_light() +
  theme(
    plot.caption = element_textbox_simple(halign = 1, colour = "white"),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.background = element_rect(fill = "black", colour = "black")
  )  +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
  draw_ghost(-0.6, -0.6, "red") +
  draw_ghost(0.6, -0.6, "pink") +
  draw_ghost(-0.6, 0.6, "orange") +
  draw_ghost(0.6, 0.6, "blue")

ggsave(filename = "man/figures/ghosts.png", plot = p, width = 6, height = 6, units = "cm")
