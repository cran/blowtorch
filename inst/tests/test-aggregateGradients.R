context("aggregateGradients")

DGp <- aggregateGradients(Dg1, Dg2)

test_that("gradients get aggregated to a matrix equal to DG", {
  expect_that(dim(DGp(1:3)), equals(dim(DG(1:3))))
  expect_that(DGp(1:3), is_equivalent_to(DG(1:3)))
})