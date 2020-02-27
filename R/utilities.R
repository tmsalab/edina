
## Model selection ----

#' Extract the Best Model
#'
#' Extracts the best model from the `auto_*()` search procedure.
#'
#' @param x   An `auto_edina` object
#' @param ic  Selection criteria
#' @param ... Not used.
#'
#' @return
#' An `edina` object
#'
#' @export
best_model = function(x, ...) {
    UseMethod("best_model")
}

best_model.auto_edina = function(x, ic = c("ppp", "bic", "dic"), ...) {
    ic = tolower(ic)
    ic = match.arg(ic)

    x$edina_models[[which.min(x$criterion[,ic])]]
}

best_model.default = function(x, ...) {
    stop_bad_class(x, "auto_edina")
}
