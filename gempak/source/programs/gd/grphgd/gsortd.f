	SUBROUTINE GSORTD ( d, k, l, kk, iret )
C************************************************************************
C* GSORTD                                                               *
C*                                                                      *
C* This subroutine sorts a two-dimensional array.			*
C* direction.                                                           *
C*                                                                      *
C* GSORTD ( D, K, L, KK, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*      K		INTEGER		First dimension of D		*
C*      L		INTEGER		Second dimension of D		*
C*      KK		INTEGER		Actual number of values to sort	*
C*                                                                      *
C* Input and Output parameters:                                         *
C*      D (K,L)		REAL		Array to be sorted		*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
C
	REAL 	d(k,l)
C*
	iret = 0
C
	DO  i = 1, kk-1
C
	    DO  j = 1, kk-i
C
		IF ( ( d(j,1) .gt. d(j+1,1) .or. 
     +		       d(j,1) .eq. RMISSD ) .and.
     +		       d(j+1,1) .ne. RMISSD )  THEN
C
		    DO  n = 1, l
			x = d(j,n)
			d(j,n) = d(j+1,n)
			d(j+1,n) = x
		    END DO
C
		END IF
C
	    END DO
C
	END DO
C
	RETURN
	END
