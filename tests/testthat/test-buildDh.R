context("buildDh")

Dhp <- buildDh(G, Df, DG, Hf, HG)

test_that("buildDh creates a function whose output is the right dimensions", {
  expect_that(Dhp, is_a("function"))
  expect_that(length(Dhp(1:3, 4:5)), 
              equals(5))
})

test_that("buildDh's output matches Dh from helper example", {
  foreach(testPt = iter(testPts, by="row")) %do% {
    X <- testPt[1:3]; lambda <- testPt[4:5]
    expect_that(Dhp(X, lambda), equals(Dh(X, lambda)))
  }
})

test_that("buildDh and aggregate* fxns work together", {
  Dhp2 <- buildDh(G=aggregateConstraints(g1, g2),
                  Df, DG=aggregateGradients(Dg1, Dg2),
                  Hf, HG=aggregateHessians(Hg1, Hg2))
  foreach(testPt = iter(testPts, by="row")) %do% {
    X <- testPt[1:3]; lambda <- testPt[4:5]
    expect_that(Dhp(X, lambda), equals(Dhp2(X, lambda)))
  }
})

# print(Df(X) - DG(X) %*% lambda)
# print(collapseHessian(HG))