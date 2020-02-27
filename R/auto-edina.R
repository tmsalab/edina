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
#'
#' @return
#' An `auto_edina` object.
#'
#' @export
#' @examples
#' if(requireNamespace("simcdm", quietly = TRUE)) {
#'
#' # Set a seed for reproducibility
#' set.seed(1512)
#'
#' # Setup data simulation parameters
#' N = 15   # Number of Examinees / Subjects
#' J = 10   # Number of Items
#' K = 2    # Number of Skills / Attributes
#'
#' # Note:
#' # Sample size and attributes have been reduced to create a minimally
#' # viable example that can be run during CRAN's automatic check.
#' # Please make sure to have a larger sample size...
#'
#' # Assign slipping and guessing values for each item
#' ss = gs = rep(.2, J)
#'
#' # Simulate an identifiable Q matrix
#' Q = simcdm::sim_q_matrix(J, K)
#'
#' # Simulate subject attributes
#' subject_alphas = simcdm::sim_subject_attributes(N, K)
#'
#' # Simulate items under the DINA model
#' items_dina = simcdm::sim_dina_items(subject_alphas, Q, ss, gs)
#'
#' \dontrun{
#' # Requires at least 15 seconds of execution time.
#' # Three EDINA models will be fit with increasing number of attributes.
#' model_set_edina = auto_edina(items_dina, k = 2:4)
#'
#' # Display results
#' edina_model
#'
#' table = summary(edina_model)
#' }
#' }
#'
auto_edina = function(data, k = 2:4,
                      burnin = 10000, chain_length = 20000) {

    ## Note:
    # Chain length is adjusted in edina

    # Compute the number of _K_ to estimate
    num_k = length(k)
    num_j = ncol(data)

    message("Starting the estimation procedure ... ")

    # Setup storage for EDINA Object
    outobj = vector('list', num_k)

    criterions = matrix(NA, nrow = num_k, ncol = 4)
    colnames(criterions) = c("k", "bic", "dic", "ppp")

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

    }

    # Output all EDINA objects
    structure(list("edina_models" = outobj,
                   "criterions" = criterions,
                   "k_checked" = k,
                   "j" = num_j
    )
    , class = "auto_edina" )
}


#' Print method for `auto_edina`
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

#' Summarize `auto_edina` model data
#'
#' Custom summarization method for displaying the results of the `auto_edina`.
#'
#' @param object An `auto_edina` object
#' @param ...    Additional methods passed onto the `print.data.frame` method.
#'
#' @return
#' An invisible `data.frame` containing criterion information.
#'
#' @export
summary.auto_edina = function(object, ...) {
    print(object, ...)
    invisible(as.data.frame(object$criterions))
}

