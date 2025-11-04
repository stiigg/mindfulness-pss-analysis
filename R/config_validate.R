config_diff <- function(cfg_path, snapshot_path) {
  if (!file.exists(snapshot_path)) {
    return(tibble::tibble(key = character(), old = character(), new = character()))
  }
  old <- yaml::read_yaml(snapshot_path)
  new <- yaml::read_yaml(cfg_path)
  flatten <- function(x, pref = "") {
    out <- lapply(names(x), function(k) {
      v <- x[[k]]
      key <- if (pref == "") k else paste0(pref, ".", k)
      if (is.list(v)) {
        flatten(v, key)
      } else {
        setNames(as.character(v), key)
      }
    })
    unlist(out)
  }
  o <- flatten(old)
  n <- flatten(new)
  keys <- union(names(o), names(n))
  tibble::tibble(key = keys, old = o[keys], new = n[keys]) |>
    dplyr::filter(is.na(old) | is.na(new) | old != new)
}
