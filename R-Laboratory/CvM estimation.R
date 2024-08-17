## (empirical) Data
x <- rgamma(50, scale = 0.5, shape = 3)

## parametric family of probability measures
B <- BetaFamily(shape1 = 0.5, shape2 = 2)

## Kolmogorov(-Smirnov) minimum distance estimator
MDEstimator(x = x, ParamFamily = G, distance = KolmogorovDist)

## von Mises minimum distance estimator with default mu
MDEstimator(wyk2$similarity, BetaFamily(shape1 = 0.5, shape2 = 2), distance = CvMDist)
MDEstimator(Shapes$similarity, BetaFamily(shape1 = 8, shape2 = 9), distance = CvMDist)
