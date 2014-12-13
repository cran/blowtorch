context("aggregateHessians")

HGp <- aggregateHessians(Hg1, Hg2)

test_that("gradients get aggregated to an array equal to HG", {
  expect_that(dim(HGp(1:3)), equals(dim(HG(1:3))))
  expect_that(HGp(1:3), is_equivalent_to(HG(1:3)))
})