mendeleyr
================
Sergio Oller
2017-06-01

mendeleyr provides a subset of the Mendeley API through R.

Browse source code
------------------

Checkout the code and browse it at <http://github.com/zeehio/mendeleyr>.

How to install mendeleyr:
-------------------------

### Package installation

-   To install the latest development version:

        devtools::install_github("zeehio/mendeleyr")

Quick start:
------------

### Setup:

Mendeley's API is based on oAuth2. This means that mendeleyr should be registered as an application in \[<http://dev.mendeley.com/myapps.html>\] and receive a `client_id` and a `client_secret`. As `mendeleyr` is open source, the package can't provide a secret so it is required that the `mendeleyr` user registers its own app.

Fortunately it's an easy step, and `mdl_conf_new()` provides detailed instructions on how to do that:

``` r
library(mendeleyr)
mdl_conf_new()
```

    ## Error in mdl_conf_new(): mdl_conf_save requires the client_id and client_secret.
    ## You can obtain these registering an app at Mendeley at http://dev.mendeley.com/myapps.html.
    ## Please use http://localhost:1410/ as 'Redirect URL'
    ## Then you can use:
    ## mdl_conf_save(client_id = "given-by-mendeley", client_secret = "given-by-mendeley")

Once you have registered you app, you will be able to save the secret to a file.

``` r
mdl_conf_save(client_id = "given-by-mendeley", client_secret = "given-by-mendeley",
              where = "/choose/a/path/mendeley_conf.json")
```

The final step is to grant `mendeleyr` access to your account. Your web browser will open and ask you login and authorize. With this `mendeleyr` never sees your username and password :-)

``` r
token <- mdl_token("/choose/a/path/mendeley_conf.json")
```

### Using mendeleyr

#### Export mendeley folders to a bibtex file:

``` r
all_my_folders <- mdl_folders(token)
mdl_to_bibtex(token, folder_name = "my_folder", bibfile = "my_folder.bib")
```

But we can work with groups as well:

``` r
all_folders_from_a_group <- mdl_folders(token, group_name = "my_group")

mdl_to_bibtex(token, folder_name = "shared_folder", group_name = "my_group",
              bibfile = "shared_folder.bib")
```

#### Download a file:

``` r
my_docs <- mdl_documents(token, folder_name = "my_folder")
all_files <- mdl_files(token, document_id = my_docs$id[1])
mdl_download_file(token, all_files[1,])
```