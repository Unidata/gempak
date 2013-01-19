        FUNCTION PR_PR6X ( p01, p02, p03, p04 )
C************************************************************************
C* PR_PR6X                                                              *
C*                                                                      *
C* This function computes PR6X, the maximum precipitation amount for up *
C* to 4 preciptiation values in inches.					* 
C*                                                                      *
C* REAL PR_PR6X  ( P01, P02, P03, P04 )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      P01		REAL	First precipitation amount		*    
C*	P02 		REAL    Second precipitation amount		* 
C*	P03		REAL    Third precipitation amount		* 
C*	P04		REAL    Fourth precipitation amount		* 
C*                                                                      *
C* Output parameters:                                                   *
C*      PR_PR6X         REAL    Max precip. amount among the 4 values	* 
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC		4/99                                            *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
C------------------------------------------------------------------------
        PR_PR6X = RMISSD
C
        PR_PR6X =  AMAX1 ( p01, p02, p03, p04 ) 
C*
        RETURN
        END

