##' @title Extract imported functions and packages
##'
##' @description Extract imported functions and packages from a
##' package's R files that are documented with roxygen2-style comments.
##'
##' @details This function reads the R files in the R/ directory of a
##' package and extracts the imported functions and packages that are
##' documented with roxygen2-style comments. The function returns a list
##' of data frames, one for each R file, with the imported functions and
##' packages. The packages are extracted from lines that start with the
##' text "@import" and the functions are extracted from lines that start
##' with the text "@importFrom".
##'
##' @param pkg_path The path to the package directory. Default is the
##' current working directory.
##'
##' @return A list of data frames, one for each R file, with the
##' imported functions and packages.
##' @author Fernando Mayer
##'
##' @import dplyr
##' @importFrom tibble tibble
extract_imports <- function(pkg_path = ".") {
    ## Get a list of R files in the R/ directory
    r_files <- list.files(file.path(pkg_path, "R"),
        pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
    ## Initialize an empty list to store results
    import_list <- vector("list", length(r_files))
    ## Loop through each R file
    i <- 0
    for (file in r_files) {
        ## Read the file contents
        file_lines <- readLines(file)
        ## Extract lines with @import and @importFrom
        import_lines <- grep("^##' @import\\s+", file_lines, value = TRUE)
        import_from_lines <- grep("^##' @importFrom", file_lines, value = TRUE)
        ## Parse @import lines
        import_data <-
            if (length(import_lines) > 0) {
                il_pkgs <- strsplit(
                    sub("##' @import\\s+", "", import_lines), "\\s+")[[1]]
                tibble(
                    Type = "@import",
                    Package = il_pkgs,
                    Function = NA)
            } else {
                tibble(Type = "@import", Package = NA, Function = NA)
            }
        ## Parse @importFrom lines
        import_from_data <-
            if (length(import_from_lines) > 0) {
                do.call(rbind, lapply(import_from_lines, function(line) {
                    parts <- strsplit(sub("##' @importFrom\\s+", "", line), "\\s+")[[1]]
                    tibble(
                        Type = "@importFrom",
                        Package = parts[1],
                        Function = parts[2])
                }))
            } else {
                tibble(Type = "@importFrom",
                    Package = NA, Function = NA)
            }
        ## Combine the data
        combined_data <- bind_rows(import_data, import_from_data)
        ## Add the data to the list with the file name as the component name
        i <- i + 1
        import_list[[i]] <- combined_data
    }
    names(import_list) <- basename(r_files)
    ## class(import_list) <- c("ext_imp", class(import_list))
    return(import_list)
}


unique_imports <- function(x) {
    stopifnot(inherits(x, "list"))
    bind_rows(x) |>
        distinct() |>
        filter(!is.na(Package)) |>
        arrange(Type, Package, Function) |>
        as.data.frame()
}
