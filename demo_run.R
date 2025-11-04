if (!dir.exists("data")) dir.create("data")
source("R/simulate_data.R")
d <- simulate_pss()
writexl::write_xlsx(list(pre = d$pre, post = d$post, dose = d$dose), "data/example_mindfulness.xlsx")
source("R/analysis.R")
latest <- tail(list.dirs("outputs", recursive = FALSE, full.names = TRUE), 1)
if (length(latest) && dir.exists(latest)) {
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render("reports/executive_summary.Rmd", params = list(run_dir = latest))
  }
}
