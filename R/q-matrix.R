## Formatting Tools for Q Matrices ----

#' Format Q Matrix
#'
#' Applies a common naming scheme to the Q Matrix.
#'
#' @param x A [`base::matrix()`] with dimensions \eqn{J \times K}{J x K}.
#'
#' @noRd
format_q_matrix = function(x) {

    # Extract dimensions
    j = nrow(x)
    k = ncol(x)

    # Pad naming with 0's
    colnames(x) = sprintf(paste0("Trait%0", nchar(k), "d"), seq_len(k))
    rownames(x) = sprintf(paste0("Item%0", nchar(j), "d"), seq_len(j))

    # Release
    x
}

## Convert Data to Q Matrix Object ----

#' Constructor for Q Matrix
#'
#' Standardizes the initialization for a Q Matrix in _R_.
#'
#' @param x A [`base::matrix()`] with dimensions \eqn{J x K}.
#'
#' @return
#' A `q_matrix` object with a fallback to `matrix`.
#'
#' @noRd
create_q_matrix = function(x) {

    # Verify Q matrix is identified
    identified_q = all(x %in% c(0, 1)) && check_identifiability(x)

    # Structure Q matrix
    x = format_q_matrix(x)

    # Apply classes
    class(x) = c('q_matrix', class(x))

    # Embed information
    attr(x, 'identifiable') = identified_q

    # Release result
    x
}

#' Create a Q Matrix Object
#'
#' Provides a way to create an object as a `"q_matrix"`.
#'
#' @param x        Either a `data.frame` or `matrix`.
#'
#' @return
#' A `q_matrix` object.
#'
#' @export
#'
#' @examples
#' # Q matrix values
#' x = matrix(c(1, 0, 0, 1), nrow = 2)
#'
#' # Construct class
#' q_mat = q_matrix(x)
q_matrix = function(x) {
    as_q_matrix(x)
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
as_q_matrix = function(x, ...) {
    UseMethod("as_q_matrix")
}

#' @export
#' @rdname as_q_matrix
as_q_matrix.data.frame = function(x, ...) {
    x = as.matrix(x)
    create_q_matrix(x)
}

#' @export
#' @rdname as_q_matrix
as_q_matrix.matrix = function(x, ...) {
    create_q_matrix(x)
}

#' @export
#' @rdname as_q_matrix
as_q_matrix.default = function(x, ...) {
    stop(class(x)[1], " is not yet supported for conversion to `q_matrix`.")
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
    cat("   Traits: ", ncol(x), "\n")
    cat("   Identifiable: ")

    # Creative use of STDERROR vs. STDOUT. Might back fire.
    if(attr(x, "identifiable")) {
        cat("Yes. \n\n")
    } else {
        message(" No.\n")
    }

    class(x) = "matrix"
    attributes(x)["identifiable"] = NULL
    print(x, ...)
    invisible(x)
}

## Extract Q Matrices from Data Objects ----

#' Extract Q Matrix
#'
#' Given a modeling object, extract the Q Matrix
#'
#' @param x      An `edina` or `q_matrix` object
#' @param binary Boolean to indicate whether the _Q_ matrix is shown in
#'               dichotomous form or in an estimated form.
#' @param ...    Additional parameters
#'
#' @return A `matrix` that is either dichotomous or estimated.
#'
#' @rdname extract_q
#' @export
extract_q_matrix = function(x, ...) {
    UseMethod("extract_q_matrix")
}

#' @rdname extract_q
#' @export
extract_q_matrix.q_matrix = function(x, ...) {
    x
}


#' @rdname extract_q
#' @export
extract_q_matrix.edina = function(x, binary = TRUE, ...) {
    if(isTRUE(binary)) {
        x$est_q
    } else {
        x$avg_q
    }
}

#' @rdname extract_q
#' @export
extract_q_matrix.default = function(x, ...) {
    stop("'", class(x)[1], "' is not yet supported for extracting a `q_matrix`.")
}
