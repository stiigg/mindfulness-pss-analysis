logf <- function(out, file, x, append = TRUE) {
  dir.create(out, FALSE, TRUE)
  p <- file.path(out, file)
  con <- file(p, if (append) "a" else "w")
  on.exit(close(con), add = TRUE)
  if (is.character(x)) {
    writeLines(x, con)
  } else {
    capture.output(x, file = con, append = TRUE)
  }
}

manifest_start <- function(out_dir, cfg) {
  seed <- if (!is.null(cfg$runtime$seed)) cfg$runtime$seed else 20251104
  set.seed(seed)
  t0 <- Sys.time()
  attr(t0, "t0") <- TRUE
  yaml::write_yaml(cfg, file.path(out_dir, "config_snapshot.yml"))
  logf(out_dir, "MANIFEST.txt", sprintf("Start: %s\nHost: %s\nR: %s",
    t0, Sys.info()[["nodename"]], R.version.string), append = FALSE)
  pkgs <- installed.packages()
  pkgs_df <- data.frame(
    package = rownames(pkgs),
    version = as.character(utils::packageVersion(rownames(pkgs))),
    row.names = NULL
  )
  logf(out_dir, "SESSION.txt", paste(capture.output(sessionInfo()), collapse = "\n"), append = FALSE)
  readr::write_csv(pkgs_df, file.path(out_dir, "packages.csv"))
  invisible(t0)
}

manifest_end <- function(out_dir, t0, warnings_vec = NULL) {
  t1 <- Sys.time()
  dur <- as.numeric(difftime(t1, t0, units = "secs"))
  if (length(warnings_vec)) {
    readr::write_lines(warnings_vec, file.path(out_dir, "WARNINGS.txt"))
  }
  logf(out_dir, "MANIFEST.txt", sprintf("End: %s\nDuration_s: %.1f", t1, dur))
}
