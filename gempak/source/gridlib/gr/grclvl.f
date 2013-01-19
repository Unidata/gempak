	SUBROUTINE GR_CLVL  ( maxlvl, cmin, cmax, cint, dmin, dmax,
     +			      nlvl,   clvl, rint, iret )
C************************************************************************
C* GR_CLVL								*
C*									*
C* This subroutine selects contour levels given the range of data	*
C* values in the grid subset area input for the contour interval	*
C* and the minimum and maximum grid values.  If the minimum or maximum	*
C* input value is missing, the data value will be used.  If the contour	*
C* interval is non-positive, a contour interval producing five to ten 	*
C* contours will be selected.						*
C*									*
C* GR_CLVL  ( MAXLVL, CMIN, CMAX, CINT, DMIN, DMAX, NLVL, CLVL, 	*
C*		RINT, IRET )						*
C*									*
C* Input parameters:							*
C*	MAXLVL		INTEGER		Max number of contour levels	*
C*	CMIN		REAL		Minimum contour value		*
C*	CMAX		REAL		Maximum contour value		*
C*	CINT		REAL		Contour interval		*
C*	DMIN		REAL		Minimum data value		*
C*	DMAX		REAL		Maximum data value		*
C*									*
C* Output parameters:							*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL (NLVL)	REAL		Contour levels			*
C*	RINT		REAL		Computed contour interval	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  1 = no valid contour level	*
C*					 -8 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	Rewrote for GEMPAK4			*
C* G. Krueger/EAI	 8/93	Modified to return computed interval	*
C* T. Lee/GSC		 6/99	Returned when no valid contour level	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		clvl (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	nlvl = 0
C
C*	Check that data range is valid.
C
	IF  ( ( ERMISS ( dmin ) ) .or. ( ERMISS ( dmax ) ) .or.
     +	      ( dmin .gt. dmax ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Set rmin and rmax to the minimum and maximum values to use.
C
	IF  ( ERMISS ( cmin ) .or. ( cmin .lt. dmin ) )  THEN
	    rmin = dmin
	  ELSE
	    rmin = cmin
	END IF
	IF  ( ERMISS ( cmax ) .or. ( cmax .gt. dmax ) )  THEN
	    rmax = dmax
	  ELSE
	    rmax = cmax
	END IF
	IF  ( rmax .lt. rmin )  THEN
	    IRET = 1
	    RETURN
	END IF
C
C*	Get contour interval if none was specified.
C
	IF  ( ( cint .eq. 0. ) .or. ( ERMISS ( cint ) ) )  THEN
	    CALL GR_CVAL  ( rmin, rmax, rint, iret )
	    IF  ( iret .ne. 0 )  RETURN
	  ELSE
	    rint = cint
	END IF
C
C*	If identical minimum and maximum values input, use that as
C*	contour level.
C
	IF  ( rmin .eq. rmax )  THEN
	    nlvl = 1
	    clvl ( nlvl ) = rmin
C
C*	    Otherwise compute levels as even multiples of the contour
C*	    interval.  If no levels are generated, make the minimum 
C*	    and maximum values the only levels.
C
	  ELSE
	    CALL GR_CMPV  ( rmin, rmax, rint, maxlvl, nlvl, clvl, iret )
	    IF  ( nlvl .eq. 0 )  THEN
		nlvl = 2
		clvl ( 1 ) = rmin
		clvl ( 2 ) = rmax
	    END IF
	END IF
C*
	RETURN
	END
