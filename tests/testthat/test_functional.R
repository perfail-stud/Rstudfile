library(testthat)


context('Convertion lambda expressions to functions')


test_that('Correctly converts and works', {
  expect_equal(lambda_to_func(~ 42)(), 42)
  expect_equal(lambda_to_func(x ~ x + 1)(10), 11)
  expect_equal(lambda_to_func(x ~ paste(x, 'world'))('hello'), 'hello world')
  expect_equal(lambda_to_func(x ~ y ~ x + y)(10, 20), 30)
  expect_equal(lambda_to_func(x ~ y ~ z ~ x + y + z)(10, 20, 100), 130)

  expect_equal(lambda_to_func(x ~ {
    a <- x * 2
    return (3*x - a)
  })(73), 73)
})


test_that('Syntax variation', {
  expect_equal(lambda_to_func(x ~ x * 2)     (10), 20)
  expect_equal(lambda_to_func( ~ x * 2)      (10), 20)
  expect_equal(lambda_to_func(~ some_arg * 2)(10), 20)
  expect_equal(lambda_to_func(~ . * 2)       (10), 20)

  expect_equal(lambda_to_func(x ~ y ~ z ~ x*y + z)(1, 2, 3), 5)
  expect_equal(lambda_to_func(~ x*y + z)(1, 2, 3), lambda_to_func(x ~ y ~ z ~ x*y + z)(1, 2, 3))

  expect_equal(lambda_to_func(~ paste(x, y))        ('hello', 'world'), lambda_to_func(x ~ y ~ paste(x, y))        ('hello', 'world'))
  expect_equal(lambda_to_func(~ paste(x, y, sep=''))('hello', 'world'), lambda_to_func(x ~ y ~ paste(x, y, sep=''))('hello', 'world'))
  expect_equal(lambda_to_func(~ {
    dbl <- x * 2
    return (dbl + y)
  })(5, 3), 13)
  expect_equal(lambda_to_func(~ {
    dbl <- .x * 2
    trpl <- .y * 3
    return (dbl + trpl)
  })(5, 10), 40)
})


test_that('Exceptions', {
  expect_error(lambda_to_func(function(x) x))
  expect_error(lambda_to_func(x + 1))
  expect_error(lambda_to_func('Последний фонарик устал'))
})
