#' Auto EDINA model selection routine
#'
#' Automatically select an appropriate $K$ dimension for a $Q$ matrix
#' under the Exploratory Determinatistic Input, Noise And gate (EDINA) Model.
#'
#' @param data          Binary responses to assessements in `matrix`
#'                      form with dimensions \eqn{N \times J}{N x J}.
#' @param k             Number of Attribute Levels as a positive `integer`.
#' @param burnin        Number of Observations to discard on the chain.
#' @param chain_length  Length of the MCMC chain
#' @param save_results  Output intermediary estimations to R binary.
#'                      Default: `FALSE`
#' @param save_filename Name stem to save each `k` iteration to. Relevant if
#'                      `save_results` is enabled.
#' @return An `auto_edina` object
#' @export
#' @examples
#' \dontrun{
#' library("tmsadata")
#'
#' # Load data
#' data("trial_matrix", package="tmsadata")
#'
#' # Coerce to matrix
#' trial_matrix = as.matrix(trial_matrix)
#'
#' edina_models = auto_edina(trial_matrix, k = 1:2)
#' }
auto_edina = function(data, k = 2:4,
                      burnin = 10000, chain_length = 20000,
                      save_results = FALSE, save_filename = "edina_model_data") {

    stopifnot(is.logical(save_results))

    ## Note:
    # Chain length is adjusted in edina

    # Compute the number of _K_ to estimate
    num_k = length(k)
    num_j = ncol(data)

    message("Starting the estimation procedure ... ")

    if(save_results) {
        # Get the length of the string e.g. 200 => 3
        nlen = max(nchar(k))

        # Format string
        save_file_ids = sprintf(paste0("%0",nlen,"d"), k)
    }

    # Setup storage for EDINA Object
    outobj = vector('list', num_k)

    criterions = matrix(NA, nrow = num_k, ncol = 4)
    colnames(criterions) = c("k", "bic", "dic", "heuristic")

    for(i in seq_along(k)) {
        k_idx = k[i]
        message("Working on k = ", k_idx, " ... ")

        modeled_value = edina(data,
                              k = k_idx,
                              burnin = burnin,
                              chain_length = chain_length)

        modeled_value_summary = summary(modeled_value)

        outobj[[i]] = modeled_value_summary

        criterions[i,] = outobj[[i]][["model_fit"]]

        message("Time Elapsed: ",  outobj[[i]][["timing"]][3])

        if(save_results) {
            # Create a single edina object
            ## consider doing an `assign()` to match the filename
            edina_obj = outobj[[i]]
            save(edina_obj, file = paste0(save_filename, "_", save_file_ids[i],".rda"))
        }
    }

    # Output all EDINA objects
    structure(list("edina_models" = outobj,
                   "criterions" = criterions,
                   "k_checked" = k,
                   "j" = num_j
    )
    , class = "auto_edina" )
}


#' Print method for `auto_dina`
#'
#' Custom print method for displaying the results of the Auto EDINA method.
#'
#' @param x   An `auto_edina` object
#' @param ... Additional methods passed onto the `print.data.frame` method.
#' @export
print.auto_edina = function(x, ...) {
    cat("The results of searching Q-matrices between", min(x$k_checked),
        "and", max(x$k_checked), "...\n")
    print(as.data.frame(x$criterions), digits = 4, row.names = FALSE, ...)
}

#'
#' Displays information about the value of each model information criterion
#' for a given model across the dimensions the Q matrix is estimated.
#' Custom summarization method for displaying the results of the `auto_edina`.
#'
#'
#'
#' @export
}

