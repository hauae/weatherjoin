test_that("join_weather is available", {
  expect_true(exists("join_weather"))
  expect_true(is.function(join_weather))
})

test_that("cache dir helper returns a path", {
  p <- .cache_dir(cache_scope = "project", pkg = "weatherjoin")
  expect_true(is.character(p))
  expect_true(nchar(p) > 0)
})
