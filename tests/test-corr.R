devtools::load_all("..")

# generate normal data (2 members per ensemble)
set.seed(987)
ens <- cbind(rnorm(10), rnorm(10))
ens.ref <- cbind(rnorm(10), rnorm(10))
obs <- cbind(rnorm(10))

# run correlation functions
Corr(ens, obs)
CorrDiff(ens, ens.ref, obs)

# set one row and one column to NA's
ens[1,] <- NA
ens.ref[,1] <- NA
Corr(ens, obs)
CorrDiff(ens, ens.ref, obs)

# N=1
ens <- 1
ens.ref <- 2
obs <- 3
Corr(ens, obs)
CorrDiff(ens, ens.ref, obs)


