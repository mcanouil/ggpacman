#' Compute Pac-Man Coordinates
#'
#' @param data A `data.frame` with the coordinates of Pac-Man moves.
#'
#' @return Returns a `tibble` of cartesian coordinates and Pac-Man direction.
#'
#' @export
compute_pacman_coord <- function(data) {
  pacman_state <- dplyr::tribble(
    ~state, ~start, ~end,
    "open_right", 14 / 6 * pi, 4 / 6 * pi,
    "close_right", 15 / 6 * pi, 3 / 6 * pi,
    "open_up", 11 / 6 * pi, 1 / 6 * pi,
    "close_up", 12 / 3 * pi, 0 / 6 * pi,
    "open_left", 8 / 6 * pi, - 2 / 6 * pi,
    "close_left", 9 / 6 * pi, - 3 / 6 * pi,
    "open_down", 5 / 6 * pi, - 5 / 6 * pi,
    "close_down", pi, - pi
  )

  data %>%
    tidyr::unnest(c("x", "y")) %>%
    dplyr::mutate(
      state_x = sign(.data[["x"]] - dplyr::lag(.data[["x"]])),
      state_y = sign(.data[["y"]] - dplyr::lag(.data[["y"]])),
      state = dplyr::case_when(
        (is.na(.data[["state_x"]]) | .data[["state_x"]] %in% 0) &
          (is.na(.data[["state_y"]]) | .data[["state_y"]] %in% 0) ~ list(c("open_right", "close_right")),
        .data[["state_x"]] == 1 & .data[["state_y"]] == 0 ~ list(c("open_right", "close_right")),
        .data[["state_x"]] == -1 & .data[["state_y"]] == 0 ~ list(c("open_left", "close_left")),
        .data[["state_x"]] == 0 & .data[["state_y"]] == -1 ~ list(c("open_down", "close_down")),
        .data[["state_x"]] == 0 & .data[["state_y"]] == 1 ~ list(c("open_up", "close_up"))
      )
    )  %>%
    tidyr::unnest("state") %>%
    dplyr::mutate(step = 1:dplyr::n()) %>%
    dplyr::left_join(y = pacman_state, by = "state")
}