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
        message("Working on k = ", k_idx)
        message("Estimated runtime is: ", format(convert_seconds_to_time(2^(k_idx+4))))

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

#' Extract the Best Model
#'
#' Extracts the best model from the `auto_*` search procedure.
#'
#' @param x  An `auto_edina` object
#' @param ic Selection criteria
#' @return An `edina` object
#' @export
best_model = function(x, ic = c("heuristic", "bic", "dic")) {
    ic = tolower(ic)
    ic = match.arg(ic)

    x$edina_models[[which.min(x$criterion[,ic])]]
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

#' Graph the Auto EDINA Object
#'
#' Presents either the fitting of model heuristics or the evolution of parameters
#' on a graph
#'
#' @param object An `auto_edina` object.
#' @param type   Kind of graph to display. Valid types: `"selection"` or `"evolution"`.
#' @param ... Not used.
#'
#' @return A `ggplot2` object.
#'
#' @export
#' @importFrom ggplot2 autoplot ggplot geom_line geom_point geom_vline facet_wrap labs aes theme_bw theme element_text
autoplot.auto_edina = function(object,
                               type = c("selection", "guessing", "slipping", "evolution"),
                               ...) {

    type = tolower(type)
    type = match.arg(type)

    switch(type,
           "selection"  = model_selection_graph(object, ...),
           "guessing"   = parameter_evolution_graph(object, type = type, ...),
           "slipping"   =  parameter_evolution_graph(object, type = type, ...),
           "evolution"  =  parameter_evolution_graph(object, type = type, ...),
           stop('Only the following types are valid: `"selection"`, `"guessing"`, or `"slipping"`')
    )

}

#' View Model Selection Statistics Across Models
#'
#' Displays information about the value of each model information criterion
#' for a given model across the dimensions the Q matrix is estimated.
#'
#' @param x   An `auto_edina` or `auto_errum` object.
#' @param ... Not used
#' @export
#'
#' @importFrom stats reshape
model_selection_graph = function(x, ...){
    UseMethod("model_selection_graph", x)
}

#' @export
model_selection_graph.auto_edina = function(x, ...) {

    K = ic_value = NULL

    colnames(x$criterions) = toupper(colnames(x$criterions))
    ic_type_names = colnames(x$criterions)[-1]

    df = reshape(as.data.frame(x$criterions),
                 varying   = list(ic_type_names),
                 direction = "long",
                 idvar     = "k",
                 v.names   = "ic_value",
                 timevar   = "ic_type",
                 times     = ic_type_names)

    subset_df = do.call(rbind, by(df, df$ic_type, function(x) x[which.min(x$ic_value), ] ))

    ggplot(df, aes(x = as.factor(K), y = ic_value)) +
        facet_wrap(~ic_type, scales = "free_y") +
        geom_line(aes(group = 1)) +
        geom_point() +
        geom_point(data = subset_df,
                   colour="red", size = 3) +
        labs(title = "Auto EDINA Model Selection",
             y     = "Information Criterion Score",
             x     = "K Dimension of Q Matrix") +
        theme_bw()
}

#' @export
model_selection_graph.default = function(x, ...){
    stop("Please supply an `auto_edina` object.")
}

#' View Slipping and Guessing Parameter Changes Across Models
#'
#' Displays the slipping and guessing parameter changes for each model across
#' the dimensions the Q matrix is estimated.
#'
#' @param x   An `auto_edina` or `auto_errum` object.
#' @param ... Not used
#' @export
parameter_evolution_graph = function(x, ...) {
    UseMethod("parameter_evolution_graph", x)
}

#' @export
parameter_evolution_graph.auto_edina = function(x,
                                                type = c("evolution", "guessing", "slipping"), ...) {

    type = match.arg(type)

    # Globals to quiet CRAN check
    K = ic_value = estimate = param_type = NULL

    J = x$j
    nmodels = length(x$edina_models)

    # Get the length of the string e.g. 200 => 3
    nlen = nchar(J)

    # Potentially add pis class? unlist(m_pi))

    extract_estimates = do.call(rbind, lapply(x$edina_models, `[[`, 1))

    o = data.frame(K          = rep(rep(x$k_checked, each = J), 2),
                   param_name = c(rep(
                       rep(sprintf(paste0("Item %0", nlen, "d"), seq_len(J)),
                           nmodels), 2)
                   ),
                   param_type = c(rep("Guessing", J*nmodels),
                                  rep("Slipping", J*nmodels)
                   ),
                   estimate   = c(extract_estimates[,"Guessing"], extract_estimates[,"Slipping"])

    )

    if(type %in% c("guessing", "slipping")) {
        o = o[grepl(type, o$param_type, ignore.case = TRUE),]
    }

    ggplot(o, aes(x = K, y = estimate, color = param_type)) +
        geom_point() + geom_line() +
        facet_wrap(~param_name) +
        labs(
            title = paste0("Evolution of the DINA Parameters"),
            subtitle = "Over Changes in Q Matrix's K Dimension",
            y = "Parameter Estimate of the DINA Parameters",
            x = "Q Matrix of a given K Dimension",
            color = "Parameter Type"
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = 1, angle = 75))
}

#' @export
parameter_evolution_graph.default = function(x, ...){
    stop("Please supply an `auto_edina` object.")
}
