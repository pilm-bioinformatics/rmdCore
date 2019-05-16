test_that("clone-works", {
    expect_error(download_repo("l/p/r/p"))
    expect_message(download_repo(system.file(package = "rmdCore", "test-template")),
                   "Detected local repo")
    # unlink("templates-rmd-de", recursive = TRUE)
    # run_template("pilm-bioinformatics/templates-rmd-de", ".")
    # unlink("templates-rmd-de", recursive = TRUE)
})


test_that("render-works", {
    parent <- system.file(package = "rmdCore", "test-template")
    run_template(parent, ".", quiet = TRUE,
                 output_file = "cars.html",
                 options = list(in_file = file.path(parent, "data", "in.rds")))
    expect_true(file.exists("cars.html"))
    unlink("test-template", recursive = TRUE)
    unlink("cars", recursive = TRUE)
    unlink("cars.html", recursive = TRUE)
    unlink("cars_files", recursive = TRUE)
})
