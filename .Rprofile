source("renv/activate.R")

.req <- function(.pkgs) {
    .pkgs |>
    sapply(require, character.only = TRUE, quietly = TRUE) |>
    all() |> stopifnot()
}

fromArgs <- function(
    defaults,
    useDefaults = interactive(),
    trailingOnly = TRUE
) {
    return(
        if (useDefaults) {
            defaults
        } else {
            base::commandArgs(trailingOnly = trailingOnly)
        }
    )
}