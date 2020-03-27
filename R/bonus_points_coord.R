bonus_points_coord <- function() {
  left_bonus_points <- dplyr::tribble(
    ~x, ~y, ~type,
    1, c(1:3, 7:8, 18:22, 24:25), "normal",
    1, c(6, 23), "big",
    2, c(1, 3, 6, 8, 18, 21, 25), "normal",
    3, c(1, 3:6, 8, 18, 21, 25), "normal",
    4, c(1, 3, 8, 18, 21, 25), "normal",
    5, c(1, 3:25), "normal",
    6, c(1, 6, 8, 21, 25), "normal",
    7, c(1, 3:6, 8, 18:21, 25), "normal",
    8, c(1, 3, 6, 8, 18, 21, 25), "normal",
    9, c(1:3, 6:8, 18, 21:25), "normal"
  )

  dplyr::bind_rows(
    left_bonus_points,
    dplyr::tribble(
      ~x, ~y, ~type,
      10, c(1, 21), "normal"
    ),
    dplyr::mutate(left_bonus_points, x = abs(.data[["x"]] - 20))
  ) %>%
    tidyr::unnest("y")
}