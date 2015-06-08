devtools::load_all("..")

# generate normal data (2 members per ensemble)
set.seed(987)
ens <- cbind(rnorm(10), rnorm(10))
ens.ref <- cbind(rnorm(10), rnorm(10))
obs <- cbind(rnorm(10))
