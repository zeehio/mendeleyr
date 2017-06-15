#' Create, Load and Save client_id and client_secret
#' @param client_id The client id given by Mendeley when you create an app at http://dev.mendeley.com/myapps.html
#' @param client_secret The client secret given by Mendeley when you create an app
#' @param where File name where mendeleyr will store the client_id and client_secret
#' @param try_env Try loading parameters from OS environment variables `MENDELEY_CLIENT_ID` and `MENDELEY_CLIENT_SECRET`
#' @name mendeley_conf
NULL


#' @rdname mendeley_conf
#' @export
mdl_conf_new <- function() {
  stop("mdl_conf_save requires the client_id and client_secret.\n",
       "You can obtain these registering an app at Mendeley at http://dev.mendeley.com/myapps.html.\n",
       "Please use http://localhost:1410/ as 'Redirect URL'\n",
       "Then you can use:\n",
       'mdl_conf_save(client_id = "given-by-mendeley", client_secret = "given-by-mendeley")\n')
}

#' @rdname mendeley_conf
#' @export
mdl_conf_save <- function(client_id, client_secret, where = ".mendeley_conf.json") {
  if (missing(client_id) || missing(client_secret)) {
  }
  cfg <- jsonlite::toJSON(list(client_id = client_id,
                               client_secret = client_secret))
  write(cfg, file = where)
  message("Your client_id and client_secret have been stored at '",
          file.path(where), "'.\n", "Do not publish your mendeley secrets.")
}

get_conf_environment <- function() {
  client_id <- Sys.getenv("MENDELEY_CLIENT_ID", unset = NA)
  client_secret <- Sys.getenv("MENDELEY_CLIENT_SECRET", unset = NA)
  if (is.na(client_id) || is.na(client_secret)) {
    return(NULL)
  } else {
    return(list(client_id = client_id, client_secret = client_secret))
  }
}


#' @rdname mendeley_conf
#' @export
mdl_conf_load <- function(where = ".mendeley_conf.json", try_env = TRUE) {
  if (isTRUE(try_env)) {
    mendeley_conf <- get_conf_environment()
    if (!is.null(mendeley_conf)) {
      return(mendeley_conf)
    }
  }
  if (!file.exists(where)) {
    stop("Mendeley client_id and client_secret not found in '", where, "'.\n",
         "Please obtain them with mdl_conf_new() and save them with mdl_conf_save()")
  }
  jsonlite::fromJSON(where)
}

#' Obtains a Mendeley Token valid for the current session.
#'
#' The first time you get the token you will need to authorise your application
#' (mendeleyr) access to your account.
#' @param mendeley_conf a list with two items: `client_id` and `client_secret`.
#'  If it is a file path to a json file, it will be loaded with [mdl_conf_load()]
#' @inheritParams httr::oauth2.0_token
#' @examples
#' \dontrun{
#'  # loads secret from ".mendeley_conf.json" in current directory:
#' token <- mdl_token()
#' # loads secret from "secret.json" in current directory
#' token <- mdl_token("secret.json")
#' # Loads secret from code (not recommended, as it may be accidentally redistributed)
#' token <- mdl_token(list(client_id = "given-by-mendeley", client_secret = "given-by-mendeley"))
#'
#' }
#' @export
mdl_token <- function(mendeley_conf, cache = NA) {
  if (missing(mendeley_conf)) {
    mendeley_conf <- mdl_conf_load()
  } else if (is.character(mendeley_conf) &&
             length(mendeley_conf) == 1 &&
             file.exists(mendeley_conf)) {
    mendeley_conf <- mdl_conf_load(mendeley_conf)
  } else if (!is.list(mendeley_conf)) {
    stop("mdl_token has an invalid mendeley_conf")
  }

  # 1. OAuth settings for mendeley:
  mendeley_oauth <- httr::oauth_endpoint(
    authorize = "https://api.mendeley.com/oauth/authorize",
    access =    "https://api.mendeley.com/oauth/token"
  )

  app <- httr::oauth_app("my_script", mendeley_conf$client_id,
                         mendeley_conf$client_secret)

  # 3. Get OAuth credentials
  token <- httr::oauth2.0_token(mendeley_oauth, app,
                                scope = "all",
                                use_oob = FALSE,
                                use_basic_auth = TRUE,
                                cache = cache)
  token
}

mdl_next_page <- function(response_headers) {
  if (!"link" %in% names(response_headers)) {
    return(NULL)
  }
  links <- which(names(response_headers) == "link")
  all_next <- grep(pattern = 'rel="next"$', x = response_headers[links])
  if (length(all_next) == 0) {
    return(NULL)
  }
  if (length(all_next) > 1) {
    stop("Many next links")
  }
  next_link <- response_headers[[links[all_next[1]]]]
  next_url <- gsub(pattern = "^<(.*)>$", replacement = "\\1",
                   x = strsplit(next_link, ";", fixed = TRUE)[[1]][1])
  return(next_url)
}

httr_to_json_to_r <- function(rsp) {
  jsonlite::fromJSON(rawToChar(httr::content(rsp)), simplifyVector = FALSE)
}

mdl_num_objects <- function(url, ..., max_objects = Inf) {
  doc_rsp <- httr::GET(url, ...)
  return(min(max_objects, as.numeric(doc_rsp[["headers"]][["mendeley-count"]])))
}

mdl_get_objects <- function(url, ..., max_objects = Inf, condition = NULL) {
  if (!is.null(condition) && !is.function(condition)) {
    stop("condition should be a function taking an object and returning a logical, or NULL")
  }
  obj_found <- 0
  all_objects <- list()
  while (!is.null(url) && length(all_objects) < max_objects) {
    doc_rsp <- httr::GET(url, ...)
    stopifnot(doc_rsp$status_code == 200)
    list_of_objs <- httr_to_json_to_r(doc_rsp)
    if (is.null(condition)) {
      objects_matching <- seq_along(list_of_objs)
    } else {
      objects_matching <- which(vapply(list_of_objs, condition, FUN.VALUE = logical(1)))
    }
    objects_found_in_page <- length(objects_matching)
    list_of_objs <- list_of_objs[objects_matching]
    if (obj_found + objects_found_in_page > max_objects) {
      # Truncate result
      all_objects <- c(all_objects, list_of_objs[seq_len(max_objects - obj_found)])
      #obj_found <- max_objects
      return(all_objects)
    } else {
      obj_found <- obj_found + objects_found_in_page
      all_objects <- c(all_objects, list_of_objs)
    }
    url <- mdl_next_page(httr::headers(doc_rsp))
  }
  return(all_objects)
}

my_bind_rows <- function(x) {
  # We need to specify list columns...
  y <- lapply(
    x,
    function(row) {
      for (i in seq_len(length(row))) {
        if (class(row[[i]]) == "list") {
          row[[i]] <- list(row[[i]])
        }
      }
      return(row)
    })

  dplyr::bind_rows(y)
}

#' Gets all the group information
#' @inheritParams mdl_common_params
#' @export
mdl_groups <- function(token, max_objects = Inf, condition = NULL) {
  grps <- mdl_get_objects("https://api.mendeley.com/groups", token,
                          max_objects = max_objects, condition = condition)
  my_bind_rows(grps)
}

get_folder_id <- function(token, folder_name, folder_id, group_id = NULL) {
  if (!is.null(folder_id)) {
    if (!is.null(folder_name)) {
      stop("folder_name and folder_id can't be not null at the same time")
    } else {
      return(folder_id)
    }
  }
  # Group ID is NULL
  if (!is.null(folder_name)) {
    info <- mdl_folder_info(token = token, group_id = group_id, folder_name = folder_name)
    if (nrow(info) == 0) {
      stop("No folder found with name '", folder_name, "'")
    }
    if (nrow(info) > 1) {
      # Is this even possible?
      stop("More than one folder found with name '", folder_name, "'")
    }
    return(info$id[1])
  }
  return(NULL)
}

get_group_id <- function(token, group_name, group_id) {
  if (!is.null(group_id)) {
    if (!is.null(group_name)) {
      stop("group_name and group_id can't be not null at the same time")
    } else {
      return(group_id)
    }
  }
  # Group ID is NULL
  if (!is.null(group_name)) {
    matched_groups <- mdl_groups(token,
                                 condition = function(obj) {"name" %in% names(obj) && obj[["name"]] == group_name})

    if (nrow(matched_groups) == 0) {
      stop("No group found with name '", group_name, "'")
    }
    if (nrow(matched_groups) > 1) {
      # Is this even possible?
      stop("More than one group found with name '", group_name, "'")
    }
    return(matched_groups$id[1])
  }
  return(NULL)
}

#' mdl Common Parameters
#'
#' These are parameters common to several mendeleyr functions
#' @name mdl_common_params
#' @param token A Token given by [mdl_token()] that provides access to your account.
#' @param document_id One or more document IDs. Get the document IDs with [mdl_documents()]
#' @param group_name A group name. Read group IDs through [mdl_groups()]
#' @param group_id A group ID. Read group IDs through [mdl_groups()]
#' @param folder_name A folder name. Get folder names with [mdl_folders()].
#' @param folder_id A folder ID. Get folder names with [mdl_folders()].
#' @param max_objects The maximum number of objects (documents, folders, files...) to retreive
#' @param condition Either `NULL` (default) or a function taking an object (document,
#'        folder, file, ...) represented as a list and returning a logical value.
#'        The function will return the objects that have returned `TRUE`.
NULL

#' Retrieve all the Mendeley folders
#' @inheritParams mdl_common_params
#' @export
#' @examples
#' \dontrun{
#' token <- mendeleyr::mdl_token()
#' mdl_folders(token, condition = function(obj) {obj$name == "YourFolderName"})
#' mdl_folders(token, condition = function(obj) {
#'    mdl_to_POSIXct(obj[["modified"]]) > mdl_to_POSIXct("2017-06-14T00:00:00Z")
#' })
#' }
mdl_folders <- function(token, group_name = NULL, group_id = NULL, max_objects = Inf, condition = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  url <- form_url("https://api.mendeley.com/folders/", list(group_id = group_id, limit = 200))
  my_bind_rows(
    mdl_get_objects(url,
                    token,
                    httr::accept('application/vnd.mendeley-folder.1+json'),
                    max_objects = max_objects,
                    condition = condition))
}

#' Gets information from a Mendeley Folder
#' @inheritParams mdl_common_params
#' @export
mdl_folder_info <- function(token, folder_id = NULL, folder_name = NULL, group_id = NULL, group_name = NULL) {
  if (is.null(folder_id) && is.null(folder_name)) {
    stop("Missing both folder_id and folder_name")
  }
  if (!is.null(folder_id) && !is.null(folder_name)) {
    stop("Both folder_id and folder_name are given")
  }
  if (!is.null(folder_id)) {
    condition <- function(obj) {"id" %in% names(obj) && obj[["id"]] == folder_id }
  } else if (!is.null(folder_name)) {
    condition <- function(obj) {"name" %in% names(obj) && obj[["name"]] == folder_name }
  } else {
    condition <- NULL # impossible case
  }
  return(mdl_folders(token, group_name = group_name, group_id = group_id, max_objects = 1, condition = condition))
}

#' Convert a date time text given by Mendeley into a POSIXct time
#' @param date_text a character vector with the date times as given by mendeley
#' @return A POSIXct object with the date times converted
#' @export
mdl_to_POSIXct <- function(date_text) {
  if (is.null(date_text)) {
    return(NA)
  }
  ifelse(date_text == "NULL",
         NA,
         as.POSIXct(date_text, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}


get_doc_ids_from_folder <- function(token, folder_id, group_id) {
  if (is.null(folder_id)) {
    stop("No given folder id")
  }
  url <- paste0("https://api.mendeley.com/folders/", folder_id, "/documents")
  url <- form_url(url, list(group_id = group_id, limit = 200))
  df <- my_bind_rows(
    mdl_get_objects(url,
                    token,
                    httr::accept("application/vnd.mendeley-document.1+json")))
  return(df)
}

#' Get the Document IDs from a given folder_id
#' @inheritParams mdl_common_params
#' @param modified_since Only retrieve documents modified since that date.
#' @export
mdl_documents <- function(token, folder_name = NULL, folder_id = NULL,
                          group_name = NULL, group_id = NULL,
                          modified_since = NULL,
                          max_objects = Inf, condition = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  folder_id <- get_folder_id(token, folder_name, folder_id, group_id = group_id)

  if (is.null(folder_id)) {
    document_id <- NULL
  } else {
    document_id <- get_doc_ids_from_folder(token, folder_id, group_id)$id
  }

  if (!is.null(modified_since)) {
    if (inherits(modified_since, "POSIXt")) {
      modified_since <- format(modified_since, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    }
  }

  url <- form_url("https://api.mendeley.com/documents",
                  list(group_id = group_id,
                       modified_since = modified_since,
                       limit = 200,
                       view = "bib"))

  if (is.null(document_id)) {
    df_list <- mdl_get_objects(url,
                               token,
                               httr::accept("application/vnd.mendeley-document.1+json"),
                               max_objects = max_objects,
                               condition = condition)
  } else {
    num_max_req_docs <- min(length(document_id), max_objects)
    num_all_docs <- mdl_num_objects(url, token, httr::accept("application/vnd.mendeley-document.1+json"))
    if (num_max_req_docs < ceiling(num_all_docs/100)) {
      df_list <- list()
      for (doc_id in document_id) {
        if (length(df_list) >= num_max_req_docs) {
          break
        }
        url <- form_url(paste0("https://api.mendeley.com/documents/", doc_id),
                        list(view = "bib"))
        doc_rsp <- httr::GET(url, token, httr::accept("application/vnd.mendeley-document.1+json"))
        stopifnot(doc_rsp$status_code == 200)
        obj <- httr_to_json_to_r(doc_rsp)
        if (is.null(condition)) {
          df_list <- c(df_list, list(obj))
        } else {
          if (isTRUE(condition(obj))) {
            df_list <- c(df_list, list(obj))
          }
        }
      }
    } else {
      if (is.null(condition)) {
        condition2 <- function(obj) obj$id %in% document_id
      } else {
        condition2 <- function(obj) condition(obj) && obj$id %in% document_id
      }
      df_list <- mdl_get_objects(url,
                                 token,
                                 httr::accept("application/vnd.mendeley-document.1+json"),
                                 max_objects = max_objects,
                                 condition = condition2)
    }
  }
  df <- my_bind_rows(df_list)
  for (col in c("created", "last_modified")) {
    if (col %in% colnames(df)) {
      df[[col]] <- as.POSIXct(df[[col]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    }
  }
  df
}

#' Download a file
#' @inheritParams mdl_common_params
#' @param file_row A row from [mdl_files()] with the file we want to download
#' @param destfile The path where we want to save destfile, by default keeps
#'                 the original file name in the current working directory
#' @export
mdl_download_file <- function(token, file_row, destfile = NULL) {
  if (is.null(destfile)) {
    outdir <- getwd()
    file_name <- tools::file_path_sans_ext(file_row$file_name)
    file_ext <- tools::file_ext(file_row$file_name)
    destfile <- file.path(outdir, paste0(file_name, ".", file_ext))
    i <- 0
    while (file.exists(destfile)) {
      i <- i + 1
      destfile <- file.path(outdir, paste0(file_name, "-" , i,  ".", file_ext))
    }
  }
  url <- paste0("https://api.mendeley.com/files/", file_row$id)
  result_1 <- httr::GET(url, token)
  curl::curl_download(result_1$url, destfile)
  return(destfile)
}

mdl_docid_as_bibtex_one <- function(token, document_id) {
  url <- form_url(paste0("https://api.mendeley.com/documents/",
                         curl::curl_escape(document_id)),
                  list(view = "bib"))
  doc_rsp <- httr::GET(url,
                       token,
                       httr::accept('application/x-bibtex'))
  stopifnot(doc_rsp$status_code == 200)
  rawToChar(httr::content(doc_rsp))
}

#' Get a document ID as bibtex
#' @inheritParams mdl_common_params
#' @export
mdl_docid_as_bibtex <- function(token, document_id) {
  bibtex <- lapply(document_id,
                   function(document) {
                     mdl_docid_as_bibtex_one(token, document)
                   })
  do.call(paste, c(bibtex, list(sep = "\n\n")))
}

#' Creates a bib file from a Mendeley folder or group
#' @inheritParams mdl_common_params
#' @param bibfile "*.bib" file to write the documents from folder_name
#' @param overwrite logical value to overwrite the bibfile. If `NULL` (default) we
#'        overwrite only when the bibfile is older than the folder
#'        last modification
#' @export
mdl_to_bibtex <- function(token, folder_name = NULL, folder_id = NULL,
                          group_name = NULL, group_id = NULL,
                          bibfile = NULL, overwrite = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  folder_info <- mdl_folder_info(token, folder_id = folder_id,
                                 folder_name = folder_name, group_id = group_id)
  folder_id <- folder_info$id[1]
  folder_name <- folder_info$name[1]
  if ("modified" %in% colnames(folder_info)) {
    folder_modified <- folder_info$modified[1]
  } else {
    folder_modified <- NA
  }

  if (is.null(bibfile)) {
    bibfile <- paste0(folder_name, ".bib")
  }

  bibfile_exists <- file.exists(bibfile)
  if (is.null(overwrite) && bibfile_exists) {
    bibfile_mtime <- file.mtime(bibfile)
    overwrite <- isTRUE(folder_modified > bibfile_mtime)
  } else {
    overwrite <- TRUE
  }

  if (bibfile_exists && !isTRUE(overwrite)) {
    return(bibfile)
  }

  all_documents <- mdl_documents(token, folder_id = folder_id, group_id = group_id)
  bibtex <- mdl_docid_as_bibtex(token, all_documents$id)

  dir.create(dirname(bibfile), recursive = TRUE)
  write(bibtex, file = bibfile)
  return(bibfile)
}

form_url <- function(url, params) {
  include_params <- !vapply(params, is.null, logical(1))
  param_url <- paste(curl::curl_escape(names(params[include_params])),
                     curl::curl_escape(params[include_params]),
                     collapse = "&", sep = "=")
  if (nchar(param_url) == 0) {
    url
  } else {
    paste0(url, "?", param_url)
  }
}

#' Retrieve the list of files attached to documents or groups
#' @inheritParams mdl_common_params
#' @export
mdl_files <- function(token, document_id = NULL, group_name = NULL, group_id = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  url <- form_url("https://api.mendeley.com/files",
                  list(document_id = document_id,
                       limit = 200,
                       group_id = group_id, view = "client"))
  all_entries_list <- mdl_get_objects(
    url,
    token,
    httr::accept('application/vnd.mendeley-file.1+json'),
    httr::content_type('application/vnd.mendeley-file.1+json')
  )
  all_entries <- my_bind_rows(lapply(all_entries_list, my_bind_rows))
  return(all_entries)
}

#' Find documents with duplicated files
#'
#' If a two documents have the same file, we consider that one of them is
#' duplicated. It's better to review manually the entries.
#' @inheritParams mdl_common_params
#' @export
#' @examples
#' \dontrun{
#' token <- mdl_token()
#' docs <- mdl_docs_with_common_files(token, group_name = "mygroup")
#' View(docs$remove)
#' mdl_delete_document(token, docs$remove$document_id)
#' }
mdl_docs_with_common_files <- function(token, group_name = NULL, group_id = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  all_entries <- mdl_files(token, group_id = group_id)
  dupl_rows <- which(duplicated(all_entries$filehash))
  # If a document has two times the same file (that would be inconsistent,
  # but not the purpose of this tool) the document is not duplicated
  false_positive <- which(duplicated(paste(all_entries$document_id,
                                           all_entries$filehash, sep = "_")))
  dupl_rows <- setdiff(dupl_rows, false_positive)

  keep_rows <- setdiff(seq_len(nrow(all_entries)), dupl_rows)
  all_entries_remove <- all_entries[dupl_rows, , drop = FALSE]
  all_entries_keep <- all_entries[keep_rows, , drop = FALSE]
  list(all = all_entries, keep = all_entries_keep, remove = all_entries_remove)
}

# not ready... many more attributes available
mdl_document_new <- function(token, title, type, group_id = NULL, hidden = TRUE) {
  doc_params <- list(title = title, type = type, hidden = hidden)
  if (!is.null(group_id)) {
    doc_params$group_id <- group_id
  }
  resp <- httr::POST("https://api.mendeley.com/documents",
                     token,
                     httr::content_type('application/vnd.mendeley-document.1+json'),
                     httr::accept('application/vnd.mendeley-document.1+json'),
                     body = doc_params,
                     encode = "json")
  stopifnot(resp$status_code == 201)
  document <- httr_to_json_to_r(resp)
  return(document)
}


#' Add document to folder
#' @inheritParams mdl_common_params
#' @export
mdl_document_to_folder <- function(token, document_id, folder_name = NULL,
                                   folder_id = NULL, group_name = NULL, group_id = NULL) {
  group_id <- get_group_id(token, group_name, group_id)
  folder_id <- get_folder_id(token, folder_name, folder_id, group_id)
  for (doc_id in document_id) {
    result_1 <- httr::POST(paste0("https://api.mendeley.com/folders/",
                                  curl::curl_escape(folder_id), "/documents"),
                           token,
                           httr::content_type('application/vnd.mendeley-document.1+json'),
                           body = list("id" = curl::curl_escape(doc_id)),
                           encode = "json")
    stopifnot(result_1$status_code == 201)
  }
  return()
}


mdl_document_delete_one <- function(token, document_id) {
  result_1 <- httr::DELETE(paste0("https://api.mendeley.com/documents/",
                                  curl::curl_escape(document_id)), token)
  stopifnot(result_1$status_code == 204)
}

#' Delete one or more Mendeley documents
#' @inheritParams mdl_common_params
#' @param show_progress logical value to show a progress bar
#' @export
mdl_document_delete <- function(token, document_id, show_progress = interactive()) {
  if (show_progress && length(document_id) > 5) {
    pb <- utils::txtProgressBar(min = 0, max = length(document_id), style = 3)
  } else {
    pb <- NULL
  }
  for (i in seq_len(length(document_id))) {
    mdl_document_delete_one(token, document_id[i])
    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (!is.null(pb)) {
    close(pb)
  }
}

