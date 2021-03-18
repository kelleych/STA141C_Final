fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

test_that("sigma.blblm parallelization returns same value(s)", {
  expect_equal(sigma.blblm(fit), sigma.blblm(fit, T))
})

test_that("coef.blblm parallelization returns same type",{
  expect_equal(typeof(coef.blblm(fit)), typeof(coef.blblm(fit,T)))
})

test_that("coef.blblm parallelization returns same value(s)", {
  expect_equal(coef.blblm(fit), coef.blblm(fit, T))
})

test_that("confint.blblm parallelization returns same type",{
  expect_equal(typeof(confint.blblm(fit)), typeof(confint.blblm(fit,T)))
})

test_that("confint.blblm parallelization returns same value(s)", {
  expect_equal(confint.blblm(fit), confint.blblm(fit, T))
})

test_that("predict.blblm parallelization returns same type", {
  expect_equal(typeof(predict.blblm(fit, new_data = data.frame(wt = c(2.5, 3), hp = c(150, 170)),confidence = TRUE)), typeof(predict.blblm(fit, T, new_data = data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence=TRUE)))
})

test_that("predict.blblm parallelization returns same value(s)", {
  expect_equal(predict.blblm(fit, new_data = data.frame(wt = c(2.5, 3), hp = c(150, 170)),confidence = TRUE), predict.blblm(fit, T, new_data = data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence=TRUE))
})
