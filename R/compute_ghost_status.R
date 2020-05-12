#' Compute Ghost Status
#'
#' Compute a ghost status, *i.e.*, normal, weak or eaten.
#'
#' @param ghost A `data.frame` with the coordinates of Ghost moves.
#' @param pacman_moves Pac-Man computed moves.
#' @param bonus_points_eaten Computed datga for points eaten by Pac-Man.
#'
#' @return Returns a `tibble` with ghost status as a column along side coordinates.
#'
#' @export
compute_ghost_status <- function(ghost, pacman_moves, bonus_points_eaten) {
  ghosts_vulnerability <- bonus_points_eaten %>%
    dplyr::filter(.data[["type"]] == "big") %>%
    dplyr::group_by(.data[["x"]], .data[["y"]]) %>%
    dplyr::summarise(step_init = min(.data[["step"]])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      step = purrr::map(.data[["step_init"]], ~ seq(.x, .x + 30, 1)),
      vulnerability = TRUE,
      x = NULL,
      y = NULL
    ) %>%
    tidyr::unnest("step")

  ghost_out <- dplyr::left_join(
    x = compute_ghost_coord(ghost),
    y = pacman_moves %>%
      dplyr::mutate(ghost_eaten = TRUE) %>%
      dplyr::select(c("X0" = "x", "Y0" = "y", "step", "ghost_eaten")),
    by = c("X0", "Y0", "step")
  ) %>%
    dplyr::left_join(y = ghosts_vulnerability, by = "step") %>%
    dplyr::mutate(
      vulnerability = tidyr::replace_na(.data[["vulnerability"]], FALSE),
      ghost_name = .data[["colour"]],
      ghost_eaten = .data[["ghost_eaten"]] & .data[["vulnerability"]],
      colour = ifelse(.data[["vulnerability"]], paste0(.data[["ghost_name"]], "_weak"), .data[["colour"]])
    )

  pos_eaten_start <- which(ghost_out[["ghost_eaten"]])
  ghosts_home <- which(ghost_out[["X0"]] == 10 & ghost_out[["Y0"]] == 14)
  for (ipos in pos_eaten_start) {
    pos_eaten_end <- min(ghosts_home[ghosts_home>=ipos])
    ghost_out[["colour"]][ipos:pos_eaten_end] <- paste0(unique(ghost_out[["ghost_name"]]), "_eaten")
  }

  dplyr::left_join(
    x = ghost_out,
    y = ghost_out %>%
      dplyr::filter(.data[["step"]] == .data[["step_init"]] & grepl("eaten", .data[["colour"]])) %>%
      dplyr::mutate(already_eaten = TRUE) %>%
      dplyr::select(c("step_init", "already_eaten")),
      by = "step_init"
  ) %>%
    dplyr::mutate(
      colour = dplyr::case_when(
        .data[["already_eaten"]] & .data[["X0"]] == 10 & .data[["Y0"]] == 14 ~ paste0(.data[["ghost_name"]], "_eaten"),
        grepl("weak", .data[["colour"]]) & .data[["already_eaten"]] ~ .data[["ghost_name"]],
        TRUE ~ .data[["colour"]]
      )
    )
}