#' Ghost Body
ghost_arc <- tidyr::unnest(dplyr::tribble(
  ~x0, ~y0, ~r, ~start, ~end, ~part,
  0, 0, 0.5, - 1 * pi / 2, 1 * pi / 2, "top",
  c(-1 / 6, 1 / 6), -0.5 + 1/8, 0.125, pi / 2, 3 * pi / 2, "bottom",
  -0.5, -0.5 + 1/8, 0.125, 2 * pi / 4, 4 * pi / 4, "bottom",
  0.5, -0.5 + 1/8, 0.125, - 2 * pi / 4, - 4 * pi / 4, "bottom",
  c(-1 / 3, 0, 1 / 3), -0.5 + 1/8, 1 / 24, - 1 * pi / 2, 1 * pi / 2, "bottom",
), "x0")

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
  top_polygon[-nrow(top_polygon), ],
  dplyr::tibble(x = 0.5, y = -0.5 + 1/8),
  bottom_polygon
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
