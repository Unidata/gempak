	SUBROUTINE GR_STAT  ( z, kx, ky, imin, jmin, imax, jmax, 
     +			      rmin, rmax, ravg, rdev, iret )
C************************************************************************
C* GR_STAT								*
C*									*
C* This subroutine computes grid statistics.				*
C* 									*
C* GR_STAT  ( Z, KX, KY, IMIN, JMIN, IMAX, JMAX, RMIN, RMAX, 		*
C*            RAVG, RDEV, IRET )					*
C*									*
C* Input parameters:							*
C*	Z  (KX,KY)	REAL		Data array			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IMIN		INTEGER		Lower left corner of subgrid	*
C*	JMIN		INTEGER		Lower left corner of subgrid	*
C*	IMAX		INTEGER		Upper right corner of subgrid	*
C*	JMAX		INTEGER		Upper right corner of subgrid	*
C*									*
C* Output parameters:							*
C*	RMIN		REAL		Minimum data value		*
C*	RMAX		REAL		Maximum data value		*
C*	RAVG		REAL		Average data value		*
C*	RDEV		REAL		Standard deviation		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = no data in range		*
C*					 -9 = invalid subset area	*
C**									*
C* Log:									*
C* G. Chatters/RDS	 6/82						*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	12/84	Converted from GEMPLT for GEMPAK	*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/USRA	 7/89	Use REAL*8 to avoid precision errors	*
C* K. Brill              5/90   Return distinct RMIN, RMAX for 0 < n < 4*
C* M. desJardins/GSFC	10/90	Return missing values for no data	*
C* S. Chiswell/Unidata	11/06	use double precision gpt		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		z ( kx, ky )
C*
	DOUBLE PRECISION	sum, sumsq, var, gpt
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check values given for imin, imax, jmin, jmax.
C
	IF  ( ( imin .lt. 1 ) .or. ( imax .gt. kx ) .or. 
     +	      ( imin .gt. imax ) .or. ( jmin .lt. 1 ) .or.
     +	      ( jmax .gt. ky ) .or. ( jmin .gt. jmax ) )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Initialize variables for computing average and standard 
C*	deviation.
C
	sum    = 0.0
	sumsq  = 0.0
	nsum   = 0
	rmax   = RMISSD
	rmin   = RMISSD
C
C*	Read through all the data in the subgrid.
C
	DO  i = imin, imax
	    DO  j = jmin, jmax
		IF  ( .not. ERMISS ( z (i,j) ) )  THEN
		    gpt = z (i,j)
		    IF  ( nsum .eq. 0 )  THEN
			sum   = gpt
			sumsq = gpt * gpt
			nsum  = 1
			rmax  = z (i,j)
			rmin  = z (i,j)
		      ELSE
			sum   = sum + gpt
			sumsq = sumsq + gpt * gpt
			nsum  = nsum + 1
			IF  ( z (i,j) .gt. rmax )  rmax = z (i,j)
			IF  ( z (i,j) .lt. rmin )  rmin = z (i,j)
		    END IF
		END IF
	    END DO
	END DO
C        
C*	Check for at least four points and that data has some range
C*      before computing standard deviation.
C
	IF  ( ( nsum .ge. 4 ) .and. ( rmin .lt. rmax ) )  THEN
	    ravg  = sum / nsum
	    var   = sumsq / nsum - ( sum / nsum ) ** 2
	    IF  ( var .lt. 0. )  THEN
		rdev = 0.0
		iret = -8
	      ELSE
		rdev  = SQRT ( var )
	    END IF
	  ELSE IF ( nsum .gt. 0 ) THEN
	    ravg  = 0.0
	    rdev  = 0.0
	  ELSE
	    iret = -8
	END IF
C*
	RETURN
	END
