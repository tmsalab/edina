create_q_matrix = function(x) {

    j = nrow(x)
    k = ncol(x)

    colnames(x) = sprintf(paste0("Trait","%0",
                                 nchar(k), "d"), seq_len(k))

    rownames(x) = sprintf(paste0("Items","%0",
                                 nchar(j), "d"), seq_len(j))

    identified_q = check_identifiability(x)

    class(x) = c('q_matrix', class(x))
    attr(x, 'identifiable') = identified_q

    x
}

#' Create a Q Matrix Object
#'
#' Provides a way to cast an object as a `"q_matrix"`.
#'
#' @param x        Either a `data.frame` or `matrix`.
#' @export
q_matrix = function(x) {
    as.q_matrix(x)
}

#' Coerce `data.frame` and `matrix` classes to Q Matrix.
#'
#' `as.q_matrix` acts as an aliases.
#'
#' @param x        Either a `data.frame` or `matrix`.
#' @param ...      Not used
#'
#' @rdname as_q_matrix
#' @export
as.q_matrix = function(x, ...) {
    UseMethod("as.q_matrix")
}

#' @export
#' @rdname as_q_matrix
as.q_matrix.data.frame = function(x, ...) {
    x = as.matrix(x)
    stopifnot(typeof(x) != "character")
    create_q_matrix(x)
}

#' @export
#' @rdname as_q_matrix
as.q_matrix.matrix = function(x, ...) {
    stopifnot(typeof(x) != "character")
    create_q_matrix(x)
}

#' @export
#' @rdname as_q_matrix
as.q_matrix.default = function(x, ...) {
    stop("Type not yet supported for conversion to `q_matrix`.")
}

#' Printing out a Q Matrix Object
#'
#' Custom print method for the Q Matrix Object.
#'
#' @param x        An `q_matrix` object
#' @param ...      Additional methods passed onto the `print.matrix` method.
#'
#' @export
print.q_matrix = function(x, ... ) {

    cat("Q Matrix properties\n")
    cat("   Items: ", nrow(x), "\n")
    cat("   Traits: ", ncol(x), "\n\n")

    if(attr(x, "identifiable")) {
        cat("The Q Matrix supplied is not identifiable. \n")
    } else {
        message("The Q Matrix supplied is not identifiable.")
    }
    class(x) = "matrix"
    attributes(x)["identifiable"] = NULL
    print(x, ...)
    invisible(x)
}

#' Extract Q Matrix
#'
#' Given a modeling object, extract the Q Matrix
#'
#' @param x    An `edina`, `dina`, `errum`, or `rrum` object
#' @param ...  Additional parameters
#'
#' @return A `matrix` that is either dichotomous or estimated.
#'
#' @rdname extract_q
#' @export
extract_q_matrix = function(x, ...) {
    UseMethod("extract_q_matrix")
}

#' @param binary_q   Classified Q matrix or a rounded Q matrix.
#' @rdname extract_q
#' @export
extract_q_matrix.edina = function(x, binary_q = FALSE, ...) {
    stopifnot(inherits(x, "edina"))
    pull_est_q_matrix(x, binary_q)
}

#' @inheritParams best_model
#' @rdname extract_q
#' @export
extract_q_matrix.auto_edina = function(x, binary_q = FALSE,
                                       ic = c("heuristic", "bic", "dic"),
                                       ...) {

    stopifnot(inherits(x, "auto_edina"))
    pull_est_q_matrix(best_model(x, ic = ic), binary_q)
}

#' @rdname extract_q
#' @export
extract_q_matrix.errum = function(x, binary_q = FALSE, ...) {
    stopifnot(inherits(x, "errum"))

    pull_est_q_matrix(x, binary_q)
}

#' @rdname extract_q
#' @export
extract_q_matrix.q_matrix = function(x, ...) {
    x
}

#' @rdname extract_q
#' @export
extract_q_matrix.default = function(x, ...) {
    stop("`x` must be a supported type.")
}


pull_est_q_matrix = function(x, binary_q = FALSE) {
    if(binary_q) {
        x$est_q
    } else {
        x$avg_q
    }
}

format_q_matrix = function(x) {
    colnames(x) = paste0("Trait", seq_len(ncol(x)))
    rownames(x) = paste0("Item", seq_len(nrow(x)) )
    x
}
    stop("'", class(x)[1], "' is not yet supported for extracting a `q_matrix`.")
}
