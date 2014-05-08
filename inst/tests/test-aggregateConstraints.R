context("aggregateConstraints")

Gp <- aggregateConstraints(g1, g2)

test_that("constraints get aggregated to a vector equal to G", {
  expect_that(length(Gp(1:3)), equals(2))
  expect_that(Gp(1:3), equals(G(1:3)))
})