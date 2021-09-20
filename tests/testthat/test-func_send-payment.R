virtual_visa_id <- "Q24BD9EZ332JT"
amazon_id <- "VW9JLMPRL9N7"

test_that("trem_send_reward throws errors - no client or API Key", {

  expect_error(
    trem_send_reward(name = "jdt",
                 email = "jdt@jdtrat.com",
                 reward_amount = 36,
                 currency_code = "USD",
                 delivery_method = "EMAIL",
                 payment_description_id = "this won't send",
                 funding_source_id = Sys.getenv("TREM_FUND_ID"),
                 reward_types = c(virtual_visa_id, amazon_id),
                 sandbox = TRUE
                 ), regexp = "Tremendous API Client not supplied.")

})

test_that("trem_send_reward throws errors - no client or sandbox", {

  expect_error(
    trem_send_reward(name = "jdt",
                 email = "jdt@jdtrat.com",
                 reward_amount = 36,
                 currency_code = "USD",
                 delivery_method = "EMAIL",
                 payment_description_id = "this won't send",
                 funding_source_id = Sys.getenv("TREM_FUND_ID"),
                 reward_types = c(virtual_visa_id, amazon_id),
                 api_key = NULL # Uses system API key
    ), regexp = "Tremendous API Client not supplied.")

})

test_that("trem_send_reward works - no client", {

  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("trem-send-payment_no-client", {
    testPayment <- trem_send_reward(name = "jdt",
                                email = "jdt@jdtrat.com",
                                reward_amount = 0.36,
                                currency_code = "USD",
                                delivery_method = "EMAIL",
                                payment_description_id = paste0("sent-from-test_no-client", format(Sys.time(), "%H:%M_%m-%d-%y")),
                                funding_source_id = Sys.getenv("TREM_FUND_ID"),
                                reward_types = c(virtual_visa_id, amazon_id),
                                api_key = NULL, # Uses system API key
                                sandbox = TRUE,
                                parse = TRUE
    )
  })

  expect_type(testPayment, "list")
  expect_named(testPayment, "order")

  expect_type(testPayment$order, "list")
  expect_named(testPayment$order, c("id", "external_id", "created_at",
                                    "status", "payment", "rewards"))

  expect_type(testPayment$order$id, "character")
  expect_type(testPayment$order$external_id, "character")
  expect_type(testPayment$order$created_at, "character")
  expect_type(testPayment$order$status, "character")
  expect_type(testPayment$order$payment, "list")
  expect_s3_class(testPayment$order$rewards, "data.frame")

})

test_that("trem_send_reward works - with client", {

  skip_on_cran()
  skip_on_travis()

  test_client <- trem_client_new(api_key = NULL, # Uses system env API key
                                 sandbox = TRUE)

  vcr::use_cassette("trem-send-payment_client", {
    testPaymentClient <- trem_send_reward(client = test_client,
                                      name = "jdt",
                                      email = "jdt@jdtrat.com",
                                      reward_amount = 0.36,
                                      currency_code = "USD",
                                      delivery_method = "EMAIL",
                                      payment_description_id = paste0("sent-from-test_client", format(Sys.time(), "%H:%M_%m-%d-%y")),
                                      funding_source_id = Sys.getenv("TREM_FUND_ID"),
                                      reward_types = c(virtual_visa_id, amazon_id),
                                      parse = TRUE
                                      )
  })

  expect_type(testPaymentClient, "list")
  expect_named(testPaymentClient, "order")

  expect_type(testPaymentClient$order, "list")
  expect_named(testPaymentClient$order, c("id", "external_id", "created_at",
                                    "status", "payment", "rewards"))

  expect_type(testPaymentClient$order$id, "character")
  expect_type(testPaymentClient$order$external_id, "character")
  expect_type(testPaymentClient$order$created_at, "character")
  expect_type(testPaymentClient$order$status, "character")
  expect_type(testPaymentClient$order$payment, "list")
  expect_s3_class(testPaymentClient$order$rewards, "data.frame")

})




