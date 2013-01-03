/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2012   The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *
 * Application Routines, typically implemented in  ../appl/
 * ----------------------------------------------  ========
 */

/* This header file contains routines which are in the R API and ones which
   are not.

   Those which are not can be used only at the user's risk and may change
   or disappear in a future release of R.
*/


#ifndef R_APPLIC_H_
#define R_APPLIC_H_

#include <R_ext/Boolean.h>
#include <R_ext/RS.h>		/* F77_... */
#include <R_ext/BLAS.h>

#ifdef  __cplusplus
extern "C" {
#endif

/* Entry points in the R API */

/* appl/integrate.c */
typedef void integr_fn(double *x, int n, void *ex);
/* vectorizing function   f(x[1:n], ...) -> x[]  {overwriting x[]}. */

void Rdqags(integr_fn f, void *ex, double *a, double *b,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last, int *iwork, double *work);

void Rdqagi(integr_fn f, void *ex, double *bound, int *inf,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last,
	    int *iwork, double *work);

/* main/optim.c */
typedef double optimfn(int, double *, void *);
typedef void optimgr(int, double *, double *, void *);

void vmmin(int n, double *b, double *Fmin,
	   optimfn fn, optimgr gr, int maxit, int trace,
	   int *mask, double abstol, double reltol, int nREPORT,
	   void *ex, int *fncount, int *grcount, int *fail);
void nmmin(int n, double *Bvec, double *X, double *Fmin, optimfn fn,
	   int *fail, double abstol, double intol, void *ex,
	   double alpha, double bet, double gamm, int trace,
	   int *fncount, int maxit);
void cgmin(int n, double *Bvec, double *X, double *Fmin,
	   optimfn fn, optimgr gr,
	   int *fail, double abstol, double intol, void *ex,
	   int type, int trace, int *fncount, int *grcount, int maxit);
void lbfgsb(int n, int m, double *x, double *l, double *u, int *nbd,
	    double *Fmin, optimfn fn, optimgr gr, int *fail, void *ex,
	    double factr, double pgtol, int *fncount, int *grcount,
	    int maxit, char *msg, int trace, int nREPORT);
void samin(int n, double *pb, double *yb, optimfn fn, int maxit,
	   int tmax, double ti, int trace, void *ex);

/* appl/interv.c: Also in Utils.h, used in package eco */
int findInterval(double *xt, int n, double x,
		 Rboolean rightmost_closed,  Rboolean all_inside, int ilo,
		 int *mflag);


/* ------------------ Entry points NOT in the R API --------------- */

/* The following are registered for use in .C/.Fortran */

/* appl/pretty.c: hidden  */
void R_pretty(double *lo, double *up, int *ndiv, int *min_n,
	      double *shrink_sml, double *high_u_fact,
	      int *eps_correction);

/* appl/strsignif.c: hidden */
void str_signif(void *x, int *n, const char **type, int *width, int *digits,
		const char **format, const char **flag, char **result);

/* appl/ch2inv.f */
void F77_NAME(ch2inv)(double *x, int *ldx, int *n, double *v, int *info);

/* appl/chol.f: used in package nlme */
void F77_NAME(chol)(double *a, int *lda, int *n, double *v, int *info);

/* appl/eigen.f */
int F77_NAME(cg)(int *nm, int *n, double *ar, double *ai,
		 double *wr, double *wi, int *matz, double *zr, double *zi,
		 double *fv1, double *fv2, double *fv3, int *ierr);
int F77_NAME(ch)(int *nm, int *n, double *ar, double *ai,
		 double *w, int *matz, double *zr, double *zi,
		 double *fv1, double *fv2, double *fm1, int *ierr);
int F77_NAME(rg)(int *nm, int *n, double *a, double *wr, double *wi,
		 int *matz, double *z, int *iv1, double *fv1, int *ierr);
/* used in package nlme */
int F77_NAME(rs)(int *nm, int *n, double *a, double *w,
		 int *matz, double *z, double *fv1, double *fv2, int *ierr);

/* find qr decomposition, dqrdc2() is basis of R's qr(), also used by nlme */
void F77_NAME(dqrdc2)(double *x, int *ldx, int *n, int *p,
		      double *tol, int *rank,
		      double *qraux, int *pivot, double *work);
void F77_NAME(dqrls)(double *x, int *n, int *p, double *y, int *ny,
		     double *tol, double *b, double *rsd,
		     double *qty, int *k,
		     int *jpvt, double *qraux, double *work);

/* appl/dqrutl.f: interfaces to dqrsl */
void F77_NAME(dqrqty)(double *x, int *n, int *k, double *qraux,
		      double *y, int *ny, double *qty);
void F77_NAME(dqrqy)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *qy);
void F77_NAME(dqrcf)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *b, int *info);
void F77_NAME(dqrrsd)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *rsd);
void F77_NAME(dqrxb)(double *x, int *n, int *k, double *qraux,
		     double *y, int *ny, double *xb);

/* end of registered */

/* hidden, for use in R.bin/R.dll/libR.so */

/* appl/cpoly.c : for use in complex.c */
void R_cpolyroot(double *opr, double *opi, int *degree,
		 double *zeror, double *zeroi, Rboolean *fail);

/* appl/machar.c: for use in platform.c */
void machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep,
	    int *negep, int *iexp, int *minexp, int *maxexp,
	    double *eps, double *epsneg, double *xmin, double *xmax);



/* For use in package stats */

/* appl/fft.c */
/* NOTE:  The following functions use GLOBAL (static) variables !!
 * ----   some of R-core think that this should be changed,
 *        which will INEVITABLY extend the argument lists ...!
 */
/* non-API, but used by package RandomFields */
void fft_factor(int n, int *pmaxf, int *pmaxp);
Rboolean fft_work(double *a, double *b, int nseg, int n, int nspn,
/* TRUE: success */ int isn, double *work, int *iwork);

/* appl/fmin.c: used by package pcaPA */
double Brent_fmin(double ax, double bx, double (*f)(double, void *),
		  void *info, double tol);

/* appl/uncmin.c : */

/* type of pointer to the target and gradient functions */
typedef void (*fcn_p)(int, double *, double *, void *);

/* type of pointer to the hessian functions */
typedef void (*d2fcn_p)(int, int, double *, double *, void *);

void fdhess(int n, double *x, double fval, fcn_p fun, void *state,
	    double *h, int nfd, double *step, double *f, int ndigit,
	    double *typx);

/* used in packages nlme, pcaPP */
void optif9(int nr, int n, double *x,
	    fcn_p fcn, fcn_p d1fcn, d2fcn_p d2fcn,
	    void *state, double *typsiz, double fscale, int method,
	    int iexp, int *msg, int ndigit, int itnlim, int iagflg,
	    int iahflg, double dlt, double gradtl, double stepmx,
	    double steptl, double *xpls, double *fpls, double *gpls,
	    int *itrmcd, double *a, double *wrk, int *itncnt);


/* Others */

/* appl/pretty.c non-API but used by engine.c, rgl and formerly GWAtoolbox */
double R_pretty0(double *lo, double *up, int *ndiv, int min_n,
		 double shrink_sml, double high_u_fact[],
		 int eps_correction, int return_bounds);

/* appl/uncmin.c : used in packages nlme, pcaPP */
void optif0(int nr, int n, double *x, fcn_p fcn, void *state,
	    double *xpls, double *fpls, double *gpls, int *itrmcd,
	    double *a, double *wrk);

#ifdef  __cplusplus
}
#endif

#endif /* R_APPLIC_H_ */