#' Compute Point Eaten by Pac-Man
#'
#' @param bonus_points Bonus points coordinates.
#' @param pacman_moves Pac-Man computed moves.
#'
#' @keywords internal
compute_points_eaten <- function(bonus_points, pacman_moves) {
  dplyr::right_join(bonus_points, pacman_moves, by = c("x", "y")) %>%
    dplyr::distinct(.data[["step"]], .data[["x"]], .data[["y"]], .data[["type"]]) %>%
    dplyr::mutate(
      step = purrr::map2(.data[["step"]], max(.data[["step"]]), ~ seq(.x, .y, 1)),
      colour = "eaten"
    ) %>%
    tidyr::unnest("step")
}