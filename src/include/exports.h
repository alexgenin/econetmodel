
// Note: passing 2D arrays to functions in C
// http://www.geeksforgeeks.org/pass-2d-array-parameter-c/

#ifndef NETMODR_EXPORTS
#define NETMODR_EXPORTS

// Functional response(s)
void frhill(int nsp,
            int extant_species[nsp],
            int extant_n,
            double F[nsp][nsp],
            double y[nsp],
            double w[nsp][nsp],
            double atk[nsp][nsp],
            double h[nsp],
            double q);

// Safe pow() for negative numbers (see .c file)
double pow2(double x, 
            double y);

// Non-trophic responses
void nti_ratfun_vec(int nsp, 
                    int extant_species[nsp],
                    int extant_n,
                    double newp[nsp],
                    double ab[nsp],  // abundance of nt interactor
                    double ab0[nsp], // typical abundance of nt interactor
                    double p[nsp],  // parameter, trophic     
                    double dp[nsp][nsp]);
void nti_ratfun_mat(int nsp, 
                    int extant_species[nsp],
                    int extant_n,
                    double newp[nsp][nsp],
                    double ab[nsp],        // abundance of nt interactor
                    double ab0[nsp],       // typical abundance of nt interactor
                    double p[nsp][nsp],    // parameter, trophic only
                    double dp[nsp][nsp][nsp]);

// Control functions
int eq_reached(int *neq, 
               double *ydot, 
               double *zerotol);

#endif
