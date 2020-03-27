#' Inky
inky_moves <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(11, offset * 3), rep(13, offset * 3),
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
    dplyr::mutate(colour = "Inky")
}

#' Clyde
clyde_moves <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(10, offset * 4), rep(13, offset * 4),
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
    dplyr::mutate(colour = "Clyde")
}

#' Pinky
pinky_moves <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(9, offset * 2), rep(13, offset * 2),
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
    dplyr::mutate(colour = "Pinky")
}

#' Blinky
blinky_moves <- function(offset = 9) {
  dplyr::tribble(
    ~x, ~y,
    rep(10, offset), rep(16, offset),
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
    dplyr::mutate(colour = "Blinky")
}

#' Pac-Man
pacman_moves <- function(offset = 11) {
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

#' use_data()
inky <- inky_moves()
pinky <- pinky_moves()
blinky <- blinky_moves()
clyde <- clyde_moves()
pacman <- pacman_moves()
usethis::use_data(inky, pinky, blinky, clyde, pacman, overwrite = TRUE)
