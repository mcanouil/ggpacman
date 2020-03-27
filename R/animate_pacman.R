#' animate_pacman
#'
#' @param pacman something
#' @param ghosts something
#' @param file something
#' @param caption something
#' @param font_family something
#'
#' @return something
#' @export
animate_pacman <- function(
  pacman,
  ghosts = list(blinky, pinky, inky, clyde),
  file = NULL,
  caption = "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil",
  font_family = ""
) {
  ## Setup data and variables ----------------------------------------------------------------------
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

  ## Plot time -------------------------------------------------------------------------------------
  base_grid <- ggplot2::ggplot() +
    ggplot2::theme_void(base_family = font_family) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(caption = caption) +
    ggplot2::scale_size_manual(values = c("wall" = 2.5, "door" = 1, "big" = 2.5, "normal" = 0.5, "eaten" = 3)) +
    ggplot2::scale_fill_manual(breaks = names(map_colours), values = map_colours) +
    ggplot2::scale_colour_manual(breaks = names(map_colours), values = map_colours) +
    ggplot2::theme(
      plot.caption = ggtext::element_textbox_simple(halign = 0.5, colour = "white"),
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(fill = "black", colour = "black"),
      panel.background = ggplot2::element_rect(fill = "black", colour = "black")
    )  +
    ggplot2::coord_fixed(xlim = c(0, 20), ylim = c(0, 26)) +
    ggplot2::geom_segment(
      data = segments,
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        xend = .data[["xend"]], yend = .data[["yend"]],
        size = .data[["type"]],
        colour = .data[["type"]]
      ),
      lineend = "round",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = bonus_points,
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        size = .data[["type"]],
        colour = .data[["type"]]
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = dplyr::tibble(x = 10, y = 11, label = "READY!", step = 1:20),
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        label = .data[["label"]],
        colour = .data[["label"]],
        group = .data[["step"]]
      ),
      size = 6
    )

  p_points <- list(
    ggplot2::geom_point(
      data = bonus_points_eaten,
      mapping = ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        colour = .data[["colour"]], size = .data[["colour"]],
        group = .data[["step"]]
      ),
      inherit.aes = FALSE
    )
  )

  p_pacman <- list(
    ggforce::geom_arc_bar(
      data = pacman_moves,
      mapping = ggplot2::aes(
        x0 = .data[["x"]], y0 = .data[["y"]],
        r0 = 0, r = 0.5,
        start = .data[["start"]], end = .data[["end"]],
        colour = .data[["colour"]], fill = .data[["colour"]],
        group = .data[["step"]]
      ),
      inherit.aes = FALSE
    )
  )

  p_ghosts <- purrr::map(.x = ghosts, .f = function(data) {
    ghost_moves <- compute_ghost_status(
      ghost = data,
      pacman_moves = pacman_moves,
      bonus_points_eaten = bonus_points_eaten
    )
    list(
      ggplot2::geom_polygon(
        data = tidyr::unnest(ghost_moves, "body"),
        mapping = ggplot2::aes(
          x = .data[["x"]], y = .data[["y"]],
          fill = .data[["colour"]], colour = .data[["colour"]],
          group = .data[["step"]]
        ),
        inherit.aes = FALSE
      ),
      ggforce::geom_circle(
        data = tidyr::unnest(ghost_moves, "eyes"),
        mapping = ggplot2::aes(
          x0 = .data[["x0"]], y0 = .data[["y0"]],
          r = .data[["r"]],
          colour = .data[["part"]], fill = .data[["part"]],
          group = .data[["step"]]
        ),
        inherit.aes = FALSE
      )
    )
  })

  ## Animate ---------------------------------------------------------------------------------------
  gganimate::animate(
    plot = base_grid + p_points + p_pacman+ p_ghosts + gganimate::transition_manual(step),
    width = 3.7 * 2.54,
    height = 4.7 * 2.54,
    units = "cm",
    res = 120,
    bg = "black",
    duration = 10,
    renderer = gganimate::gifski_renderer(file = file)
  )
}