#' Perform a GET request to Tremendous API
#'
#' @description This function provides lower-level access to perform GET
#'   requests via Tremendous API. Available endpoints can be found on the
#'   official [Tremendous API
#'   documentation](https://developers.tremendous.com/).
#'
#' @inheritParams send_payment
#' @inheritParams trem_post
#'
#' @param ... Curl options passed to [crul::verb-GET]
#'
#' @return If `parse = TRUE` (default), a list containing the
#'   response from the API request. Otherwise, the R6 HttpResponse object
#'   containing API request data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # The recommended method is to create a new Tremendous API Client, # which
#' provides an object to store the API key and environment. # You could also
#' manually pass in api key and sandbox to `trem_post()`. test_client <-
#' trem_client_new(api_key = "TEST_YOUR-API-KEY-HERE", sandbox = TRUE) # Sandbox
#' environment so no actual money is sent
#'
#' # Use a GET request to list funding sources available in your Tremendous
#' Account. # Documentation:
#' https://developers.tremendous.com/reference/core-funding-source-index
#' trem_get(trem_client, "funding_sources")
#'
#' # Use a GET request to list all invoices on your Tremendous Account. #
#' Documentation:
#' https://developers.tremendous.com/reference/core-invoices-index
#' trem_get(trem_client, "invoices")
#'
#' # Use a GET request to list all orders (payment history) on your Tremendous
#' Account. # Documentation:
#' https://developers.tremendous.com/reference/core-orders-index
#' trem_get(trem_client, "orders")
#'
#' # Use a GET request to list a specific order's information (payment history)
#' from your Tremendous Account. # Documentation:
#' https://developers.tremendous.com/reference/core-orders-show
#' trem_get(trem_client, "orders/YOUR-ORDER-ID")
#'
#'   }
#'

trem_get <- function(client, path,
                     query = list(), disk = NULL, stream = NULL,
                     api_key, sandbox, parse = TRUE, ...) {


  if (missing(client)) {
    if (missing(sandbox) | missing(api_key)) {
      cli::cli_abort("Tremendous API Client not supplied.
                     Please create one with {.fn trem_client_new} or provide {.arg api_key} and {.arg sandbox} directly.")
    }
    .key <- api_key
    .sandbox <- sandbox
  } else if (!missing(client)) {
    check_client(client)
    .key <- client$key
    .sandbox = client$sandbox
  }

  tr <- crul::HttpClient$new(
    url = trem_url(.sandbox),
    opts = c(list(useragent = trem_ua(), ...)),
    headers = list(
      Accept = "application/json",
      Authorization = paste0("Bearer ", check_api_key(.key,
                                                      sandbox = .sandbox))
    )
  )

  res <- tr$get(path = file.path("api/v2/", path),
                query = query,
                disk = disk,
                stream = stream)

  err_catcher(res)

  if (!parse) {
    return(res)
  } else if (parse) {
    jsonlite::fromJSON(res$parse("UTF-8"))
  }
}



