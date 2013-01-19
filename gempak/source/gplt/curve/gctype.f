	SUBROUTINE GCTYPE  ( itype, jtype, jend, iret )
C************************************************************************
C* GCTYPE								*
C*									*
C* This subroutine checks the type of curve to be fit.  If the curve is	*
C* to be a cubic spline, the end conditions are also checked.  The	*
C* following types are available:					*
C*		1  -  piecewise linear					*
C*		2  -  cubic spline - jend = 1				*
C*	       21  -  cubic spline - jend = 1				*
C*	       22  -  cubic spline - jend = 2				*
C*	       23  -  cubic spline - jend = 3				*
C* The end conditions for the cubic spline are:				*
C*		1  -  linear, i.e. S"(1) = S"(np) = 0			*
C*		2  -  parabolic,i.e. S"(1)=S"(2) and S"(np)=S"(np-1) 	*
C*		3  -  cubic, i.e. S"(1) and S"(np) are extrapolated	*
C* If an invalid type is entered, a cubic spline with linear ends will	*
C* be used.								*
C*									*
C* GCTYPE ( ITYPE, JTYPE, JEND, IRET )					*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Input type			*
C*									*
C* Output parameters:							*
C*	JTYPE		INTEGER		Output type			*
C*					 1 = linear  2 = cubic		*
C*	JEND		INTEGER		End conditions for spline	*
C*					 1=linear 2=parabolic 3=cubic	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C************************************************************************
C------------------------------------------------------------------------
	iret = 0
	IF  ( itype .eq. 1 ) THEN
	    jtype = 1
	    jend  = 0
	  ELSE IF ( ( itype .eq. 2 ) .or. ( itype .eq. 21 ) )THEN
	    jtype = 2
	    jend  = 1
	  ELSE IF  ( itype .eq. 22 ) THEN
	    jtype = 2
	    jend  = 2
	  ELSE IF  ( itype .eq. 23 ) THEN
	    jtype = 2
	    jend  = 3
	  ELSE
	    jtype = 2
	    jend  = 1
	END IF
C*
	RETURN
	END
