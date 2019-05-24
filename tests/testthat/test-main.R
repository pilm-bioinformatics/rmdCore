test_that("clone-wrong-repo", {
    expect_error(download_repo("l/p/r/p"))
})

test_that("clone-null-local", {
    expect_error(download_repo("pilm-bioinformatics/templates-rmd-de", NULL))
})

test_that("clone-local-repo", {
        expect_message(download_repo(system.file(package = "rmdCore", "test-template")),
                   "Detected local repo")
    unlink("test-template", recursive = TRUE)
})

test_that("clone-github", {
    run_template("pilm-bioinformatics/templates-rmd-de", ".")
    expect_true(file.exists("templates-rmd-de"))
    unlink("templates-rmd-de", recursive = TRUE)
})

parent <- system.file(package = "rmdCore", "test-template")

test_that("render-error-output_dir", {
    expect_error(run_template(parent, output_file = "test.html", output_dir = "test"))
    unlink("test-template", recursive = TRUE)
    unlink("tests", recursive = TRUE)
})

test_that("render-output_file", {
    run_template(parent, local = NULL, output_file = "working/test.html")
    expect_true(file.exists(file.path("working",
                                      "test.html")))
    unlink("working", recursive = TRUE)

})


test_that("render-fast-test", {
    # fast check
    expect_message(check_template(parent), "All is good.")
})

test_that("render-test", {
    # run test in ../checking
    check_template(parent, test = TRUE, clean = FALSE)
    working <- file.path(parent, "..")
    expect_true(file.exists(file.path(working,
                                      "checking",
                                      "run",
                                      "example",
                                      "test.html")))
    unlink(file.path(working, "checking"), recursive = TRUE)
})

test_that("render-test-working", {
    # run test in working
    dir.create("working")
    check_template(parent, working = "working", test = TRUE, clean = FALSE)
    expect_true(file.exists(file.path(
                                      "working",
                                      "run",
                                      "example",
                                      "test.html")))
    unlink("working", recursive = TRUE)

})
