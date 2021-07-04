library(testthat)


context('Porter stemmer for russian words')


test_that('Simple test', {
  expect_equal(porterStem.ru('академия'), 'академ')
  expect_equal(porterStem.ru('русский'), 'русск')
  expect_equal(porterStem.ru('область'), 'област')
  expect_equal(porterStem.ru('человеческий'), 'человеческ')
  expect_equal(porterStem.ru('гибридной'), 'гибридн')
  expect_equal(porterStem.ru('благополучие'), 'благополуч')
})

test_that('Errors check', {
  expect_equal(porterStem.ru(''), '')
  expect_equal(porterStem.ru('Somebody'), '')

  expect_error(porterStem.ru(1))
})


context('Porter stemmer for english words')


test_that('Simple test', {
  expect_equal(porterStem.en('development'), 'develop')
  expect_equal(porterStem.en('question'), 'question')
  expect_equal(porterStem.en('combinations'), 'combin')
  expect_equal(porterStem.en('based'), 'base')
  expect_equal(porterStem.en('needs'), 'need')
  expect_equal(porterStem.en('robotalization'), 'robot')
})


test_that('Errors check', {
  expect_equal(porterStem.en(''), '')
  expect_equal(porterStem.en('шалом'), 'шалом')
  expect_equal(porterStem.en('TREES'), 'TREES')

  expect_error(porterStem.en(1))
})
