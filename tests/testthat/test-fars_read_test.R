test_that("fars_read is returning a correct df", {
  expect_equal(fars_read("t.csv") %>% dplyr::slice(1) %>% dplyr::pull(1), "a")
})
