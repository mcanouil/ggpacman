#' Title
#'
#' @param offset something
#'
#' @return something
#' @export
pacman <- function(offset = 11) {
  dplyr::tribble(
    ~x, ~y,
    rep(10, offset), rep(6, offset),
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
    dplyr::mutate(colour = "Pac-Man")
}