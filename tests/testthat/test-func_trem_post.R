test_that("trem_post throws errors - no client or API Key", {

  expect_error(
    trem_post(path = "members",
              body = list(email = "example@website.com",
                          name = "Example Person",
                          role = "MEMBER"),
              sandbox = TRUE
    ),
    regexp = "Tremendous API Client not supplied.")

})

test_that("trem_post throws errors - no client or sandbox", {

  expect_error(
    trem_post(path = "members",
             body = list(email = "example@website.com",
                         name = "Example Person",
                         role = "MEMBER"),
             api_key = NULL # Uses system API key
    ),
    regexp = "Tremendous API Client not supplied.")

})

