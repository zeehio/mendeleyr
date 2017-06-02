context("mendeley load and save secrets")

skip_if_no_credentials <- function() {
  mendeley_conf <- get_conf_environment()
  if (!is.null(mendeley_conf)) {
    return(mendeley_conf)
  }
  if (file.exists(".mendeley_conf.json")) {
    return(mdl_conf_load(where = ".mendeley_conf.json"))
  }
  if (file.exists("../../.mendeley_conf.json")) {
    return(mdl_conf_load(where = "../../.mendeley_conf.json"))
  }
  skip("App credentials not available")
}

skip_if_no_token <- function() {
  mendeley_conf <- skip_if_no_credentials()
  token_cache <- NA
  # in Travis we load the test oauth from the just unencrypted file,
  # out of travis we follow the default
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    token_cache <- file.path(Sys.getenv("TRAVIS_BUILD_DIR"), ".httr-oauth")
  } else {
    possible_files <- c(".httr-oauth", "../.httr-oauth", "../../.httr-oauth", "../../../.httr-oauth")
    for (pos in possible_files) {
      if (file.exists(pos)) {
        token_cache <- pos
        break
      }
    }
  }
  if (!file.exists(token_cache)) {
    skip("User credentials (token) not available")
  }
  token <- mdl_token(mendeley_conf = mendeley_conf, cache = token_cache)
  return(token)
}

test_that("mdl_conf_new gives error", {
  expect_error(mdl_conf_new())
})

test_that("get_conf_environment() returns null if environment not set", {
  client_id <- Sys.getenv("MENDELEY_CLIENT_ID", unset = NA)
  Sys.unsetenv("MENDELEY_CLIENT_ID")
  expect_null(get_conf_environment())
  if (!is.na(client_id)) {
    Sys.setenv("MENDELEY_CLIENT_ID" = client_id)
  }
})


test_that("mdl_conf_load without environment and with wrong file fails", {
  expect_error(mdl_conf_load(where = tempfile(), try_env = FALSE))
})

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
  expect_equal(nrow(folders), 2)
  expect_equal(sort(folders$name), sort(c("Test1", "Test2")))
})

test_that("Documents are retreived", {
  token <- skip_if_no_token()
  docs_in_test1 <- mdl_documents(token, folder_name = "Test1")
  expect_equal(nrow(docs_in_test1), 1)
  expect_equal(docs_in_test1$id, "fc609003-8cd6-34ce-a7a5-3747737464cd")
})

test_that("Files are retreived", {
  token <- skip_if_no_token()
  files <- mdl_files(token, document_id = "fc609003-8cd6-34ce-a7a5-3747737464cd")
  fn <- tempfile()
  on.exit({unlink(fn)})
  mdl_download_file(token, files[1,], destfile = fn)
  expect_equal(readLines(fn), "My test 1")
  # File with automatic file name:
  dest_file <- mdl_download_file(token, files[1,])
  on.exit({unlink(dest_file)})
  expect_equal(readLines(dest_file), "My test 1")
})

test_that("Can get documents even if reply is paginated", {
  token <- skip_if_no_token()
  docs <- mdl_documents(token, folder_name = "Test2")
  expect_equal(nrow(docs), 30)
})

test_that("Create document works and delete works as well", {
  token <- skip_if_no_token()
  doc <- mdl_document_new(token, "Test New Document", "journal", hidden = TRUE)
  mdl_document_delete(token, doc$id)
  expect_equal(doc$last_modified, doc$created)
})
