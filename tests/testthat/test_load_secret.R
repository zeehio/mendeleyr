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
  expect_true(all(c("Test1", "Test2", "Test3") %in% folders$name))
})

test_that("mdl_folders filters by folder name", {
  token <- skip_if_no_token()
  folder <- mendeleyr::mdl_folders(token, condition = function(obj) {obj$name == "Test1"})
  expect_equal(nrow(folder), 1)
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


test_that("We have a group", {
  token <- skip_if_no_token()
  groups <- mdl_groups(token)
  expect_equal(nrow(groups), 1)
  expect_equal(groups$name, "TestGroup")
})

test_that("Get document as bibtex", {
  token <- skip_if_no_token()
  expect_equal(
    mdl_docid_as_bibtex(token, document_id = "bae8c2a5-6744-3947-87ed-9e84c1e31f02"),
    paste0(c("@article{Paskin1999TowardIdentifiers,\n", "    title = {{Toward unique identifiers}},\n",
             "    year = {1999},\n", "    journal = {Proceedings of the IEEE},\n",
             "    author = {Paskin, Norman},\n", "    isbn = {0262193736},\n",
             "    doi = {10.1109/5.771073},\n", "    issn = {00189219}\n", "}"),
           collapse = ""))
})


test_that("Whole folder to bibtex", {
  token <- skip_if_no_token()
  bibfile <- mdl_to_bibtex(token, folder_id = "01db097b-0d38-420d-929c-5ce2f95e22ac")
  on.exit({unlink(bibfile)})
  expect_equal(bibfile, "Test3.bib")
  a <- readLines(bibfile)
  expect_equal(a,
               c("@article{Paskin1999TowardIdentifiers,", "    title = {{Toward unique identifiers}},",
                 "    year = {1999},", "    journal = {Proceedings of the IEEE},",
                 "    author = {Paskin, Norman},", "    isbn = {0262193736},",
                 "    doi = {10.1109/5.771073},", "    issn = {00189219}", "}"
               ))
})


test_that("There are two documents with the same attached file", {
  token <- skip_if_no_token()
  result <- mdl_docs_with_common_files(token)
  expect_equal(nrow(result$remove), 1)
  expect_equal(result$remove$id, "62bbab36-f0d2-1c9f-3481-5b02f65b9287")
})


test_that("Retreive only two most recent documents", {
  token <- skip_if_no_token()
  result1 <- mdl_documents(token)
  recent_date <- sort(result1$last_modified)[nrow(result1) - 1]
  result2 <- mdl_documents(token, modified_since = recent_date)
  expect_equal(nrow(result2), 2)
})

test_that("mdl_documents with a folder works", {
  token <- skip_if_no_token()
  result1 <- mdl_documents(token, folder_name = "Test6")
  expect_equal(nrow(result1), 2)
})
