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
    parse_repo <- unlist(strsplit("l/p", "/"))
    if (length(parse_repo) != 2){
        stop("repo is not well formatted, expecting something like user/repository")
    }
    folder <- unlist(strsplit(repo, "/"))[2]
    local <- file.path(normalizePath(local), folder)
    if (!dir.exists(local)){
        clone(paste0(url, repo), local)
        message("clonning ", url, "/",repo, " into ", local)
    }
    return(local)
}


#' @import yaml
#' @export
run_template <- function(repo, local = ".", output_file = NULL,
                         options=list(), params_file=NULL,
                         ...){
    local <- download_repo(repo, local)

    if (!is.null(params_file)){
        options <- yaml::read_yaml(params_file)[["params"]]
    }

    if (is.null(output_file)){
        # only download if output_file not given
        message("The template has been downloaded on ", local )
        return(0)
    }

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
        message(p)
        if (is.character(options[[p]])){
            if (file.exists(options[[p]]))
                return(normalizePath(options[[p]]))
        }else if (grepl("file", p)){
            return(normalizePath(options[[p]]))
        }
        options[[p]]
    })
    names(opts_normalized) <- names(options)

    main = file.path(local, "main.Rmd")
    rmarkdown::render(main,
                      params = options,
                      output_file = output_file,
 #                     envir = baseenv(),
                      ...)

}
