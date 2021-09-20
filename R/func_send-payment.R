#' Send a Reward via Tremendous API
#'
#' The most likely reason to use the tremendousr package is to send rewards
#' This function, `trem_send_reward()`, provides an easy interface to do so. See the
#' examples for more details.
#'
#' @param client A Tremendous API Client object, created with
#'   \code{\link{trem_client_new}}.
#' @param name Name of the recipient.
#' @param email Email address of the recipient.
#' @param phone Phone number of the recipient (US phone numbers only).
#' @param reward_amount Amount of the reward (numeric).
#' @param currency_code Currency of the reward (default to "USD").
#' @param delivery_method Default to "EMAIL", for sending the reward to the
#'   recipient via email. Alternatively, reward can be delivered via a link
#'   ("LINK") or text message ("PHONE").
#' @param payment_description_id Unique ID for specific order. This will appear
#'   as `external_id` on Tremendous Dashboard.
#' @param funding_source_id ID of the funding source linked to your account, to
#'   draw funds from for this order. One of the IDs from
#'   `trem_get("funding_sources")`.
#' @param reward_types A character vector of product ids -- reward options --
#'   for the recipient to choose from. Available options can be found
#'   [here](https://www.tremendous.com/catalog).
#' @param api_key API key from
#'   [tremendous.com](https://developers.tremendous.com/). Can either pass in
#'   here as a character string or set for repeated use with
#'   \code{\link{trem_set_api_key}}.
#' @param sandbox Logical: `TRUE` (default) and any API requests are performed
#'   within the Tremendous sandbox environment, a free and fully-featured
#'   environment for application developing and testing. `FALSE` and the API
#'   requests are performed within the Tremendous production environment. **This
#'   will involve sending actual money, so be certain you wish to do this!**
#' @param parse Logical: Should the API Response results be parsed into a data
#'   frame?
#' @param ... Curl options passed to [crul::verb-POST]
#'
#' @return If `parse = TRUE` (default), a list containing the response
#'   from payment API request. Otherwise, the R6 HttpResponse object containing
#'   API request data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'   # The recommended method is to create a new Tremendous API Client,
#'   # which provides an object to store the API key and environment.
#'
#'   test_client <- trem_client_new(api_key = "TEST_YOUR-KEY-HERE",
#'                                  sandbox = TRUE) # Sandbox environment so no actual money is sent
#'
#'   # To send a payment, you can simply pass in the client
#'   # and specify the necessary fields.
#'   payment1 <- trem_send_reward(client = test_client,
#'                            name = "first last",
#'                            email = "email@website.com",
#'                            reward_amount = 10,
#'                            currency_code = "USD",
#'                            delivery_method = "EMAIL",
#'                            payment_description_id = "payment-from-tremendousr-examples",
#'                            funding_source_id = "your-funding-id-from-tremendous",
#'                            reward_types = "Q24BD9EZ332JT", # ID for virtual visa gift card
#'                            parse = TRUE # Return a parsed API response
#'   )
#'
#'   # If you don't wish to use a Tremendous API Client, you can also pass in
#'   # the `api_key` and `sandbox` arguments manually:
#'   payment2 <- trem_send_reward(name = "first last",
#'                            email = "email@website.com",
#'                            reward_amount = 10,
#'                            currency_code = "USD",
#'                            delivery_method = "EMAIL",
#'                            payment_description_id = "payment-from-tremendousr-examples",
#'                            funding_source_id = "your-funding-id-from-tremendous",
#'                            reward_types = "2JFKPXBWDC1K", # ID for Applebee's Gift Card
#'                            parse = TRUE, # Return a parsed API response
#'                            api_key =  "TEST_YOUR-KEY-HERE",
#'                            sandbox = TRUE)
#' }
#'
#'
trem_send_reward <- function(client,
                         name, email = NULL, phone = NULL,
                         reward_amount, currency_code = "USD", delivery_method = "EMAIL",
                         payment_description_id, funding_source_id, reward_types,
                         api_key, sandbox, parse = TRUE, ...) {

  payment_body <- create_order_body(recipient_name = name, recipient_email = email, recipient_phone = phone,
                                    reward_amount = reward_amount, currency_code = currency_code,
                                    delivery_method = delivery_method, payment_description_id = payment_description_id,
                                    funding_source_id = funding_source_id, reward_types = reward_types)

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
    .sandbox <- client$sandbox
  }

  tr <- crul::HttpClient$new(
    url = trem_url(sandbox = .sandbox),
    opts = c(list(useragent = trem_ua(), ...)),
    headers = list(
      Accept = "application/json",
      Authorization = paste0("Bearer ", check_api_key(.key,
                                                      sandbox = .sandbox))
    )
  )
  res <- tr$post(path = "api/v2/orders",
                 body = payment_body,
                 encode = "json"
                 )
  err_catcher(res)
  if (!parse) {
    return(res)
  } else if (parse) {
    jsonlite::fromJSON(res$parse("UTF-8"))
  }

}


