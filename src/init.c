#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _optimalclpm_minv(SEXP);
extern SEXP _optimalclpm_mm(SEXP, SEXP);
extern SEXP _optimalclpm_mmm(SEXP, SEXP, SEXP);
extern SEXP _optimalclpm_mmmm(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_optimalclpm_minv", (DL_FUNC) &_optimalclpm_minv, 1},
    {"_optimalclpm_mm",   (DL_FUNC) &_optimalclpm_mm,   2},
    {"_optimalclpm_mmm",  (DL_FUNC) &_optimalclpm_mmm,  3},
    {"_optimalclpm_mmmm", (DL_FUNC) &_optimalclpm_mmmm, 4},
    {NULL, NULL, 0}
};

void R_init_optimalclpm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
