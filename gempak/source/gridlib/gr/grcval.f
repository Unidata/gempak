	SUBROUTINE GR_CVAL  ( rmin, rmax, rint, iret )
C************************************************************************
C* GR_CVAL								*
C*									*
C* This subroutine selects a contour interval, given minimum and 	*
C* maximum data values.  The selected interval will generate		*
C* five to ten contour levels.						*
C*									*
C* GR_CVAL  ( RMIN, RMAX, RINT, IRET )					*
C*									*
C* Input parameters:							*
C*	RMIN		REAL		Minimum data value		*
C*	RMAX		REAL		Maximum data value		*
C*									*
C* Output parameters:							*
C*	RINT		REAL		Contour interval		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -8 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for errors in input.
C
	IF  ( ( ERMISS ( rmin ) ) .or. ( ERMISS ( rmax ) ) .or.
     +	      ( rmin .gt. rmax ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	RANGE is the range of the data.
C*	CSCAL will scale range between 1. and 10.
C*	CNRNG is the normalized (scaled) range.
C
	range = rmax - rmin
	IF  ( range .eq. 0. )  THEN
	    rint = 0.
	    RETURN
	END IF
C*
	iscal = INT ( ALOG10 (range) )
	IF  ( range .lt. 1. )  iscal = iscal - 1
	cscal = 10. ** iscal
	cnrng = range / cscal
C
C*	Compute CINT as follows:
C*	    If normalized range between 1. and 2., normalized CINT = .2
C*	    "       "     "       "     2. and 3.,  "          "   " .3
C*		etc.
C
	nrng  = INT ( cnrng )
	rint  = ( nrng+1 ) * .1 * cscal
C*
	RETURN
	END
