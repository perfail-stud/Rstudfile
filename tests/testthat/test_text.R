library(testthat)


context('N-gramm extruction')


test_that('Simple test', {
  expect_equal(nGramm(c(1, 2, 3, 4, 5), 3), list(c(1, 2, 3), c(2, 3, 4), c(3, 4, 5)))
  expect_equal(nGramm(c('a', 'b', 'c', 'd', 'e'), 3), list(c('a', 'b', 'c'), c('b', 'c', 'd'), c('c', 'd', 'e')))

  expect_equal(nGramm(c(1, 2, 3, 4, 5), 5), list(c(1, 2, 3, 4, 5)))
  expect_equal(nGramm(c(1, 2, 3, 4), 3), list(c(1, 2, 3), c(2, 3, 4)))
  expect_equal(nGramm(c(1, 2, 3, 4, 5), 2), list(c(1, 2), c(2, 3), c(3, 4), c(4, 5)))
  expect_equal(nGramm(c(1, 2, 3, 4, 5), 1), list(c(1), c(2), c(3), c(4), c(5)))
})


test_that('Errors check', {
  expect_error(nGramm(c(1, 2, 3), 4))

  expect_error(nGramm('hello', 2))

  expect_error(nGramm(c(1, 2, 3, 4), 'a'))
})
