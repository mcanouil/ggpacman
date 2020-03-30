#' Ghost Body
ghost_arc <- dplyr::tribble(
  ~x0, ~y0, ~r, ~start, ~end, ~part,
  0, 0, 0.5, - 1 * pi / 2, 1 * pi / 2, "top",
  -0.5, -0.5 + 1/6, 1 / 6,  pi / 2, 2 * pi / 2, "bottom",
  -1/6, -0.5 + 1/6, 1 / 6,  pi / 2, 3 * pi / 2, "bottom",
  1/6, -0.5 + 1/6, 1 / 6,  pi / 2, 3 * pi / 2, "bottom",
  0.5, -0.5 + 1/6, 1 / 6,  3 * pi / 2,  2 * pi / 2, "bottom"
)

top <- ggplot2::ggplot() +
  ggforce::geom_arc_bar(
    data = ghost_arc[1, ],
    mapping = ggplot2::aes(
      x0 = .data[["x0"]], y0 = .data[["y0"]],
      r0 = 0, r = .data[["r"]],
      start = .data[["start"]], end = .data[["end"]]
    )
  ) +
  ggplot2::coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))

top_polygon <- ggplot2::ggplot_build(top)$data[[1]][, c("x", "y")]

bottom <- ggplot2::ggplot() +
  ggforce::geom_arc_bar(
    data = ghost_arc[c(5, 3, 2, 4), ],
    mapping = ggplot2::aes(
      x0 = .data[["x0"]], y0 = .data[["y0"]],
      r0 = 0, r = 0.17,
      start = .data[["start"]], end = .data[["end"]]
    )
  ) +
  ggplot2::coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1))

bottom_polygon <- ggplot2::ggplot_build(bottom)$data[[1]][, c("x", "y")]

ghost_body <- dplyr::bind_rows(
  top_polygon,
  dplyr::tribble(
    ~x, ~y,
    0.5, 0,
    0.5, -0.5 + 1/6
  ),
  bottom_polygon,
  dplyr::tribble(
    ~x, ~y,
    -0.5, -0.5 + 1/6,
    -0.5, 0
  )
)

#' Ghost Eyes
ghost_eyes <- dplyr::tribble(
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
  tidyr::unnest("direction")

#' use_data()
usethis::use_data(ghost_body, ghost_eyes, overwrite = TRUE)


# ggplot() +
#   coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
#   geom_polygon(
#     data = ghost_body,
#     mapping = aes(x = x, y = y),
#     inherit.aes = FALSE
#   ) +
#   geom_circle(
#     data = ghost_eyes,
#     mapping = aes(x0 = x0, y0 = y0, r = r, colour = part, fill = part),
#     inherit.aes = FALSE,
#     show.legend = FALSE
#   ) +
#   facet_wrap(vars(direction), ncol = 3)
