//
// this is the C implementation of the CRPS for 
// Gaussian ensemble dressing
//
// requires the gnu scientific library (GSL)
// 
// compile with:
//   PKG_LIBS="-lgsl -lm -L/usr/lib" R CMD SHLIB dresscrps.c
//
// the -L/usr/lib indicates where the gsl files live, 
// might be different for different users
//
// to call the DLL from R do:
//   dyn.load("dresscrps.so")
//   ens <- c(1,2,3)
//   ker.wd <- c(1,1,1)
//   obs <- 2.5
//   .C("dresscrps", as.double(ens), 
//      as.integer(length(ens)), 
//      as.double(ker.wd), as.double(obs), 
//      crps=double(1)
//     )$crps
//
// for 15 member ensembles, the C function is 50 times faster compared
// to the native R implementation
//

#include <math.h>                // for sqrt()
#include <gsl/gsl_randist.h>     // for gsl_ran_ugaussian_pdf() = dnorm()
#include <gsl/gsl_cdf.h>         // for gsl_cdf_ugaussian_P()   = pnorm()

void dresscrps(double *x, int *K, double *s, double *y,  double *crps) {

  int KK = *K;
  double yy = *y;

  double sum1 = 0.0;
  for (int i = 0; i < KK; ++i) {
    double zi = (x[i] - yy) / s[i];
    sum1 += (x[i] - yy) * (2.0 * gsl_cdf_ugaussian_P(zi) - 1) + 2.0 * s[i] * gsl_ran_ugaussian_pdf(zi) - s[i] / KK * 0.5 * M_2_SQRTPI;
  }
  sum1 /= KK;

  double sum2 = 0.0;
  for (int i = 1; i < KK; ++i) {
    for (int j = 0; j < i; ++j) {
      double eiej = x[j] - x[i];
      double sisj = sqrt(s[i]*s[i] + s[j]*s[j]);
      double argg = eiej / sisj;
      sum2 += eiej * (2.0 * gsl_cdf_ugaussian_P(argg) - 1) + 2.0 * sisj * gsl_ran_ugaussian_pdf(argg); 
    }
  }
  sum2 = sum2 / KK / KK;

  *crps = sum1 - sum2; 
}

