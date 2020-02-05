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

#' Graph Q Matrix
#'
#' Provides a heatmap approach to showing the estimated binary or averaged
#' values of the Q Matrix.
#'
#' @param x Either an `edina`, `errum`, or `q_matrix` object.
#' @param ... Additional paramters not used
#' @export
#' @rdname q_graph
q_graph = function(x, ...) {
    UseMethod("q_graph")
}

#' @inheritParams best_model
#' @export
#' @rdname q_graph
q_graph.auto_edina = function(x, binary_q = TRUE,
                              ic = c("heuristic", "bic", "dic"), ... ) {
    q_graph(best_model(x, ic), binary_q)
}

#' @param binary_q   Classified Q matrix or a rounded Q matrix.
#' @export
#' @rdname q_graph
q_graph.edina = function(x, binary_q = TRUE, ... ){

    if(binary_q == TRUE) {
        p = x$est_q
    } else {
        p = x$avg_q
    }

    q_type = if (binary_q == TRUE) {
        "Binary"
    } else {
        "Average"
    }

    q_title = paste("Estimated ", q_type, "Q Matrix")

    q_heatmap(p, q_title)
}

#' @export
#' @rdname q_graph
q_graph.matrix = function(x, ... ){
    q_heatmap(x, q_title = "Q Matrix")
}

#' @export
#' @rdname q_graph
q_graph.q_matrix = function(x, ... ){
    q_heatmap(x, q_title = "Q Matrix")
}


#' @importFrom reshape2 melt
#' @importFrom ggplot2 theme_minimal scale_fill_gradient geom_tile
q_heatmap = function(x, q_title = "Estimated Q Matrix") {

    Trait = Item = Value = NULL

    dgrid = reshape2::melt(x)
    colnames(dgrid) = c("Item", "Trait", "Value")

    ggplot(dgrid, aes(Trait, Item)) +
        geom_tile(aes(fill = Value), color = "white") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(title = q_title,
             subtitle = paste0("J = ", nrow(x), ", K = ", ncol(x)),
             x = "Items",
             y = "Traits",
             fill = "Estimated Value") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
}
