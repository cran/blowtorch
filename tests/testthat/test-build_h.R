context("build_h")

hp <- build_h(G, Df, DG)

test_that("build_h yields a function with proper output", {
  expect_that(hp, is_a("function"))
  expect_that(length(hp(1:3, 4:5)), equals(1))
})

test_that("build_h output matches h in elaborate example", {
  foreach(testPt = iter(testPts, by="row")) %do% {
    X <- testPt[1:3]; lambda <- testPt[4:5]
    expect_that(hp(X, lambda), equals(h(X, lambda)))
  }
})

test_that("build_h works with aggregate* suite", {
  hp2 <- build_h(G=aggregateConstraints(g1, g2),
                 Df,
                 DG=aggregateGradients(Dg1, Dg2))
  
  foreach(testPt = iter(testPts, by="row")) %do% {
    X <- testPt[1:3]; lambda <- testPt[4:5]
    expect_that(hp2(X, lambda), equals(h(X, lambda)))
  }
})

