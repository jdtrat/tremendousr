
trem_url <- function(sandbox) {
  if (sandbox) {
    "https://testflight.tremendous.com"
  } else if (!sandbox) {
    "https://www.tremendous.com"
  }
}

# Adapted from chimpr package:
# https://github.com/sckott/chimpr/
trem_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("tremendousr/%s", utils::packageVersion("tremendousr"))
  )
  paste0(versions, collapse = " ")
}

# Copied from chimpr package:
# https://github.com/sckott/chimpr/
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
    cli::cli_abort("Invalid Tremendous API Client supplied.
                    Please recreate one with {.fn trem_client_new} or provide {.arg api_key} and {.arg sandbox} directly.")
  }
}
