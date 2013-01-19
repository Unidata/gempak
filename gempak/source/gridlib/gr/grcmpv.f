	SUBROUTINE GR_CMPV  ( rmin, rmax, rint, maxlvl, nlvl, clvl,
     +			      iret )
C************************************************************************
C* GR_CMPV								*
C*									*
C* This subroutine defines contour levels, given the data range and	*
C* the contour interval.						*
C*									*
C* GR_CMPV  ( RMIN, RMAX, RINT, MAXLVL, NLVL, CLVL, IRET )		*
C*									*
C* Input parameters:							*
C*	RMIN		REAL		Minimum value			*
C*	RMAX		REAL		Maximum value			*
C*	RINT		REAL		Contour interval		*
C*	MAXLVL		INTEGER		Max number of contour levels	*
C*									*
C* Output parameters:							*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL (NLVL)	REAL		Contour levels			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C*					 -8 = invalid range		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	Cleaned up for GEMPAK4			*
C* K. Brill/HPC		 8/02	Stop DO WHILE using KTMX		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		clvl (*)
	LOGICAL		done
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	nlvl = 0
C
C*	Check for errors in input.
C
	IF  ( ( rint .le. 0. ) .or. ( rmin .gt. rmax ) .or.
     +	      ( ERMISS ( rmin ) ) .or. ( ERMISS ( rmax ) ) .or.
     +	      ( ERMISS ( rint ) ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Find multiplier to use for rint to compute minimum value.
C
	r   = ( rmin / rint )
	knt = INT ( r )
	IF  ( FLOAT (knt) .eq. knt )  knt = knt - 1
	IF  ( rmin .ge. 0. )  knt = knt + 1
	ktmx = INT ( rmax / rint ) + 1
C
C*	Add contour values until rmax is reached or until MAXLVL levels
C*	are generated.
C
	done = .false.
	DO WHILE  ( .not. done )
	    tval = knt * rint
	    IF  ( tval .ge. rmin )  THEN
		IF  ( tval .gt. rmax )  THEN
		    done = .true.
		  ELSE IF  ( nlvl .ge. maxlvl )  THEN
		    done = .true.
		  ELSE
		    nlvl = nlvl + 1
		    clvl ( nlvl ) = tval
		END IF
	    END IF
	    knt = knt + 1
	    IF ( knt .gt. ktmx ) done = .true.
	END DO
C*
	RETURN
	END
