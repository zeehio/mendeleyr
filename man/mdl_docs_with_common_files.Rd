% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mendeleyr.R
\name{mdl_docs_with_common_files}
\alias{mdl_docs_with_common_files}
\title{Find documents with duplicated files}
\usage{
mdl_docs_with_common_files(token, group_name = NULL, group_id = NULL)
}
\arguments{
\item{token}{A Token given by \code{\link[=mdl_token]{mdl_token()}} that provides access to your account.}

\item{group_name}{A group name. Read group IDs through \code{\link[=mdl_groups]{mdl_groups()}}}

\item{group_id}{A group ID. Read group IDs through \code{\link[=mdl_groups]{mdl_groups()}}}
}
\description{
If a two documents have the same file, we consider that one of them is
duplicated. It's better to review manually the entries.
}
\examples{
\dontrun{
token <- mdl_token()
docs <- mdl_docs_with_common_files(token, group_name = "mygroup")
View(docs$remove)
mdl_delete_document(token, docs$remove$document_id)
}
}
