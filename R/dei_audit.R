suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

dei_audit <- function(df, group_cols = c("gender", "age_band", "department")) {
  out <- list()
  for (g in group_cols[group_cols %in% names(df)]) {
    tab <- df |>
      group_by(.data[[g]]) |>
      summarise(
        n = dplyr::n(),
        dz = (mean(post_pss - pre_pss, na.rm = TRUE)) / sd(post_pss - pre_pss, na.rm = TRUE),
        .groups = "drop"
      )
    tab$grouping <- g
    out[[g]] <- tab
  }
  if (length(out)) {
    dplyr::bind_rows(out)
  } else {
    tibble::tibble()
  }
}

flag_disparities <- function(dei_tbl, threshold = 0.20) {
  if (!nrow(dei_tbl)) {
    return(tibble::tibble())
  }
  dei_tbl |>
    group_by(grouping) |>
    mutate(ref = max(dz, na.rm = TRUE), gap = ref - dz, flag = gap > threshold) |>
    ungroup()
}
