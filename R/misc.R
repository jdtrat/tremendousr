
trem_url <- function(sandbox) {
  if (sandbox) {
    "https://testflight.tremendous.com"
  } else if (!sandbox) {
    "https://www.tremendous.com"
  }
}




#' Get useragent info for tremendousr API Package
#'
#' Adapted from [chimpr](https://github.com/jdtrat/chimpr/).
#'
#' @return useragent info for tremendousr API Package
#' @export
#'
#'
trem_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("tremendousr/%s", utils::packageVersion("tremendousr"))
  )
  paste0(versions, collapse = " ")
}

# Copied from chimpr package:
# https://github.com/jdtrat/chimpr/
err_catcher <- function(x) {
  if (x$status_code > 201) {
    if (grepl("json", x$response_headers$`content-type`)) {

      xx <- jsonlite::fromJSON(x$parse("UTF-8"))
      xx <- paste0("\n  ", paste(names(xx), unname(xx), sep = ": ",
                                 collapse = "\n  "))
      stop(xx, call. = FALSE)
    } else {
      x$raise_for_status()
    }
  }
}

check_client <- function(.client) {
  if (!inherits(.client, "tremClient")) {
    cli::cli_abort("Tremendous API Client required.
                    Please create one with {.fn trem_client_new}.")
  }
}
