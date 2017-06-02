context("mendeley load and save secrets")

skip_if_no_credentials <- function() {
  mendeley_conf <- get_conf_environment()
  if (is.null(mendeley_conf) && !file.exists(".mendeley_conf.json")) {
    skip("App credentials not available")
  }
}

skip_if_no_token <- function() {
  skip_if_no_credentials()
  options("httr_oauth_cache" = TRUE)
  token <- tryCatch({
    mdl_token()
  }, error = function(e) NULL)
  if (is.null(token)) {
    skip("User credentials (token) not available")
  }
  return(token)
}


test_that("mdl_conf_load and mdl_conf_save work", {
  client_id <- "bar"
  client_secret <- "foo"
  filename <- tempfile()
  on.exit(unlink(filename))
  mdl_conf_save(client_id = client_id, client_secret = client_secret,
                where = filename)
  expect_identical(mdl_conf_load(where = filename, try_env = FALSE),
                   list(client_id = client_id, client_secret = client_secret))

})

test_that("Folders are retreived", {
  token <- skip_if_no_token()
  folders <- mdl_folders(token)
  expect_equal(nrow(folders), 1)
  expect_equal(folders$name, "Test1")
})

test_that("Documents are retreived", {
  token <- skip_if_no_token()
  docs_in_test1 <- mdl_documents(token, folder_name = "Test1")
  expect_equal(nrow(docs_in_test1), 1)
  expect_equal(docs_in_test1$authors, "John Doe")
})

test_that("Files are retreived", {
  token <- skip_if_no_token()
  files <- mdl_files(token, document_id = "fc609003-8cd6-34ce-a7a5-3747737464cd")
  fn <- tempfile()
  on.exit({unlink(fn)})
  mdl_download_file(token, files[1,], destfile = fn)
  expect_equal(readLines(fn), "My test 1")
 })
