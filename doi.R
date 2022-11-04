.libs <- c("rvest", "jsonlite", "purrr", "magrittr")

for (i in seq(.libs)) {
  if (!any(installed.packages()[, 1] == .libs[i])) {
    install.packages(.libs[i], dependencies = TRUE)
  }
  library(.libs[i], character.only = TRUE, quietly = TRUE)
}

doi <- function(.doi = NULL, .apa = TRUE, .cite = TRUE) {
  if (is.null(.doi)) {
    stop()
  }
  clean_doi <- function(.doi) {
    gsub("^.*doi.org\\/", "", .doi)
  }

  doi_to_apa <- function(.doi, .lang = "en-GB") {
    paste0(
      "https://citation.crosscite.org/format?doi=",
      clean_doi(.doi),
      paste0("&style=apa&lang=", .lang),
      collapse = ""
    ) %>%
      rvest::read_html(.) %>%
      rvest::html_text2(.)
  }

  doi_details <- function(.doi, .author = TRUE, .year = FALSE, .ref_count = FALSE) {
    paste0("https://api.crossref.org/works/",
      clean_doi(.doi),
      collapse = ""
    ) %>%
      rvest::read_html(.) %>%
      rvest::html_text(.) %>%
      jsonlite::parse_json(.) %$%
      .[["message"]] %$% list(
        purrr::when(., .author ~ purrr::map_chr(.[["author"]], ~ .[["family"]]), ~NULL),
        purrr::when(., .year ~ unlist(.[["issued"]], use.names = FALSE) %>%
          .[`==`(nchar(.), 4)], ~NULL),
        purrr::when(., .ref_count ~ .[["references-count"]], ~NULL)
      ) %>%
      purrr::discard(~ is.null(.))
  }

  doi_to_citation <- function(.doi) {
    x <- doi_details(
      clean_doi(.doi),
      .author = TRUE,
      .year = TRUE
    )

    x[[1]] %>%
      purrr::when(
        length(.) == 1 ~ .[1],
        length(.) == 2 ~ paste0(.[1], " & ", .[2]),
        length(.) == 3 ~ paste0(.[1], ", ", .[2], " & ", .[3]),
        length(.) >= 4 ~ paste0(.[1], " et al."),
        ~.
      ) %>%
      c("(", ., ", ", x[[2]], ")") %>%
      paste0(collapse = "")
  }
  
  if (.apa & !.cite) {
    return(doi_to_apa(.doi))
  } else if (!.apa & .cite) {
    return(doi_to_citation(.doi))
  } else if (.apa & .cite) {
    return(list(
      doi_to_apa(.doi),
      doi_to_citation(.doi)
    ))
  }
}
