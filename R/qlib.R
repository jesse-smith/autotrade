#' Load Packages Quickly and Quietly
#'
#' @param ... `[chr | sym]` One or more packages to attach; can be character
#'   vectors or symbols
#' @param .warn_conflicts `[lgl]` Should warnings be printed regarding conflicts
#'   when attaching new packages?
#' @param .quietly `[lgl]` Should all messages be suppressed?
#' @return A `character` vector of attached packages (invisibly)
#'
#' @export
qlib <- function(..., .warn_conflicts = FALSE, .quietly = TRUE) {
  pkgs <- unique(unlist(lapply(rlang::enexprs(...), function(x) {
    if (rlang::is_symbol(x)) {
      rlang::as_string(x)
    } else {
      rlang::as_character(eval(x))
    }
  })))
  attached <- unique(unlist(lapply(pkgs, function(pkg) {
    if (.quietly) {
      suppressMessages(library(
        pkg, character.only = TRUE, warn.conflicts = .warn_conflicts
      ))
    } else {
      library(pkg, character.only = TRUE, warn.conflicts = .warn_conflicts)
    }
  })))
  invisible(attached)
}
