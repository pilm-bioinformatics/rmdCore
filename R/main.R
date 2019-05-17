# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @import git2r
download_repo <- function(repo, local = ".", url = "https://github.com/"){
    if (dir.exists(repo)){
        message("Detected local repo")
        if (is.null(local))
            return(normalizePath(repo))
        message("Detected local repo, copy to final folder: ", local)
        if (!dir.exists(file.path(local, basename(repo))))
            file.copy(repo, local, recursive = TRUE)
        return(file.path(local, basename(repo)))
    }
    parse_repo <- unlist(strsplit(repo, "/"))
    if (length(parse_repo) != 2){
        stop("repo is not well formatted, expecting something like user/repository")
    }
    if (is.null(local)){
        stop("to download the template, local needs to be specified.")
    }
    folder <- unlist(strsplit(repo, "/"))[2]
    local <- file.path(normalizePath(local), folder)
    if (!dir.exists(local)){
        clone(paste0(url, repo), local)
        message("clonning ", url, "/",repo, " into ", local)
    }
    return(local)
}

#' Main function to render templates from github or local folders
#' @param repo character with the owner and repo names like: foo/bar
#' @param local character with the folder where you want to clone the repo
#' @param output_file character with the final HTML
#' @param options list with options matching the params in the main.R file from the repository
#' @param params_file optional YAML file with the options matching the params in the main.R file from the repository
#' @param ... any option for [rmarkdown::render()]
#' @import yaml
#' @import rmarkdown
#' @import tools
#' @export
run_template <- function(repo, local = ".", output_file = NULL,
                         options=list(), params_file=NULL,
                         ...){
    local <- download_repo(repo, local)
    # TODO check repo
    check_template(local)
    # TODO check output_file to be html or pdf
    if (!is.null(params_file)){
        options <- yaml::read_yaml(params_file)[["params"]]
    }

    if (is.null(output_file)){
        # only download if output_file not given
        message("The template has been downloaded on ", local )
        return(0)
    }
    stopifnot(tools::file_ext(output_file) %in% c("html", "pdf"))

    output_file <- file.path(normalizePath(dirname(output_file)),
                             basename(output_file))
    # render function with params
    if (!is.null(options[["output_dir"]])){
        stopifnot(dir.exists(options[["output_dir"]]))
        options[["output_dir"]] <- normalizePath(options[["output_dir"]])
    }else{
        options[["output_dir"]] <- file.path(dirname(output_file),
                                             tools::file_path_sans_ext(
                                                 basename(output_file)))
    }
    opts_normalized <- lapply(names(options), function(p){
        if (is.character(options[[p]])){
            if (file.exists(options[[p]]))
                return(normalizePath(options[[p]]))
        }else if (grepl("file", p)){
            return(normalizePath(options[[p]]))
        }
        message(p, "->", options[[p]])
        options[[p]]
    })
    names(opts_normalized) <- names(options)

    main = file.path(local, "main.Rmd")
    rmarkdown::render(main,
                      params = options,
                      output_file = output_file,
                      ...)

}

#' Check template directory to make sure is valid
#' @param repo character with the owner and repo names like: foo/bar
#' @param local character with the folder where you want to clone the repo
#' @param working folder where the test will be performed. By default one level up
#' @param clean boolean whether to remove or not the working directory
#' @param deps boolean whether to install the deps of the template
#' @param test boolean whether to run the test example
#' @export
check_template <- function(repo, local = NULL, working = NULL,
                           clean = TRUE,
                           deps = FALSE, test = FALSE){

    local <- download_repo(repo, local)

    if (!dir.exists(local))
        stop("Template folder doesn't exists ", local)
    if (!file.exists(file.path(local, "main.Rmd")))
        stop("Template folder doesn't contain a main.Rmd file")
    if (!dir.exists(file.path(local, "data")))
        stop("Template folder doesn't contain a data folder")
    if (length(list.files(file.path(local, "data"))) == 0)
        stop("Data folder is empty")
    if (!file.exists(file.path(local, "README.md")))
        stop("Template folder doesn't contain a README.md file")
    if (!file.exists(file.path(local, "config", "install.R")))
        stop("Template folder doesn't contain a config/install.R file")

    local <- normalizePath(local)
    header <- yaml_front_matter(file.path(local, "main.Rmd"),
                                encoding = getOption("encoding"))

    if (!("output_dir" %in% names(header[["params"]])))
        stop("output_dir is a mandatory param to be defined in the main.Rmd file")

    if (deps){
        # install deps
        source(file.path(local, "config", "install.R"))
    }
    if (test){
        # run testing
        origin <- getwd()
        if (is.null(working)){
            setwd(file.path(local, ".."))
            tmp <- "checking"
        } else {
            tmp <- normalizePath(working, mustWork = TRUE)
        }

        logs <- tryCatch({
            dir.create(file.path(tmp, "run", "example"), recursive = TRUE)
            tmp <- normalizePath(tmp)
            run_template(local, tmp,
                         output_file = "test.html",
                         options = list(output_dir = file.path(tmp, "run", "example")),
                         output_dir = file.path(tmp, "run", "example"))

            if (clean)
                unlink(tmp, recursive = TRUE)

            }, error = function(e) {
                print(e)
                warning("Working directory is at:", tmp)
                setwd(origin)
            } , finally = {
                setwd(origin)
            }
        )
        print(logs)
    }
    message("All is good.")
}
