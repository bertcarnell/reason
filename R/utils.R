# Copyright 2022 Robert Carnell

# Build the reason-code data.frame from a contributions matrix.
# Returns a data.frame with 2*n_reasons columns interleaved as
# reason_1, reason_1_contribution, reason_2, reason_2_contribution, ...
.build_reason_df <- function(contributions_df, n_reasons) {
  n_obs       <- nrow(contributions_df)
  pred_names  <- colnames(contributions_df)
  contrib_mat <- as.matrix(contributions_df)

  n_col_pairs <- 2L * n_reasons
  result      <- data.frame(matrix(NA_character_, nrow = n_obs,
                                   ncol = n_col_pairs),
                             stringsAsFactors = FALSE)

  col_names <- character(n_col_pairs)
  for (k in seq_len(n_reasons)) {
    col_names[2L * k - 1L] <- paste0("reason_", k)
    col_names[2L * k]      <- paste0("reason_", k, "_contribution")
  }
  colnames(result) <- col_names

  for (i in seq_len(n_obs)) {
    ranked <- order(abs(contrib_mat[i, ]), decreasing = TRUE)
    for (k in seq_len(n_reasons)) {
      result[i, 2L * k - 1L] <- pred_names[ranked[k]]
      result[i, 2L * k]      <- contrib_mat[i, ranked[k]]
    }
  }

  # Coerce contribution columns to numeric
  contrib_cols <- seq(2L, n_col_pairs, by = 2L)
  result[contrib_cols] <- lapply(result[contrib_cols], as.numeric)

  result
}
