test_that("trem_get throws errors - no client or API Key", {

  expect_error(
    trem_get(path = "funding_sources",
             sandbox = TRUE
             ),
    regexp = "Tremendous API Client not supplied.")

})

test_that("trem_get throws errors - no client or sandbox", {

  expect_error(
    trem_get(path = "funding_sources",
             api_key = NULL # Uses system API key
             ),
    regexp = "Tremendous API Client not supplied.")

})

test_that("trem_get works (funding sources) - no client", {

  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("trem-get_funding-sources_no-client", {
    funding_sources <- trem_get(path = "funding_sources",
                                api_key = NULL, # Uses system API key
                                sandbox = TRUE,
                                parse = TRUE)
  })

  expect_type(funding_sources, "list")
  expect_named(funding_sources, "funding_sources")

  expect_s3_class(funding_sources$funding_sources, "data.frame")
  expect_named(funding_sources$funding_sources, c("method", "id", "meta"))

  expect_type(funding_sources$funding_sources$method, "character")
  expect_type(funding_sources$funding_sources$id, "character")
  expect_s3_class(funding_sources$funding_sources$meta, "data.frame")

})

test_that("trem_get works (funding sources) - with client", {

  skip_on_cran()
  skip_on_travis()

  test_client <- trem_client_new(api_key = NULL, # Uses system env API key
                                 sandbox = TRUE)

  vcr::use_cassette("trem-get_funding-sources_client", {
    funding_sourcesClient <- trem_get(test_client,
                                      path = "funding_sources",
                                      parse = TRUE)
  })

  expect_type(funding_sourcesClient, "list")
  expect_named(funding_sourcesClient, "funding_sources")

  expect_s3_class(funding_sourcesClient$funding_sources, "data.frame")
  expect_named(funding_sourcesClient$funding_sources, c("method", "id", "meta"))

  expect_type(funding_sourcesClient$funding_sources$method, "character")
  expect_type(funding_sourcesClient$funding_sources$id, "character")
  expect_s3_class(funding_sourcesClient$funding_sources$meta, "data.frame")


})


test_that("trem_get works (orders) - no client", {

  skip_on_cran()
  skip_on_travis()

  vcr::use_cassette("trem-get_orders_no-client", {
    orders <- trem_get(path = "orders",
                       api_key = NULL, # Uses system API key
                       sandbox = TRUE,
                       parse = TRUE)
  })

  expect_type(orders, "list")
  expect_named(orders, "orders")

  expect_s3_class(orders$orders, "data.frame")
  expect_named(orders$orders, c("id", "external_id", "created_at",
                                "status", "payment", "rewards"))

  expect_type(orders$order$id, "character")
  expect_type(orders$orders$external_id, "character")
  expect_type(orders$orders$created_at, "character")
  expect_type(orders$orders$status, "character")
  expect_s3_class(orders$orders$payment, "data.frame")
  expect_type(orders$orders$rewards, "list")

})

test_that("trem_get works (orders) - with client", {

  skip_on_cran()
  skip_on_travis()

  test_client <- trem_client_new(api_key = NULL, # Uses system env API key
                                 sandbox = TRUE)

  vcr::use_cassette("trem-get_orders_client", {
    ordersClient <- trem_get(test_client,
                             path = "orders",
                             parse = TRUE)
  })

  expect_type(ordersClient, "list")
  expect_named(ordersClient, "orders")

  expect_s3_class(ordersClient$orders, "data.frame")
  expect_named(ordersClient$orders, c("id", "external_id", "created_at",
                                "status", "payment", "rewards"))

  expect_type(ordersClient$order$id, "character")
  expect_type(ordersClient$orders$external_id, "character")
  expect_type(ordersClient$orders$created_at, "character")
  expect_type(ordersClient$orders$status, "character")
  expect_s3_class(ordersClient$orders$payment, "data.frame")
  expect_type(ordersClient$orders$rewards, "list")

})



