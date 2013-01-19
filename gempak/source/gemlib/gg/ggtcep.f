	SUBROUTINE GG_TCEP ( xlat, xlon, npts, inout, ipt, nseg, iret )
C************************************************************************
C* GG_TCEP								*
C*									*
C* This subroutine gets the endpoints for the bounding segments of the  *
C* tropical cyclone danger area.                                        *
C*									*
C* GG_TCEP ( XLAT, XLON, NPTS, INOUT, IPT, NSEG, IRET )                 *
C*									*
C* Input parameters:							*
C*	XLAT (*)    	REAL		Danger area latitudes for a pt. *
C*	XLON (*)   	REAL		Danger area longitudes for a pt.*
C*	NPTS 		INTEGER		Number of lat/long pairs for pt.*
C*	INOUT (*)	INTEGER		Inside/outside polygon flags    *
C*									*
C* Output parameters:							*
C*      IPT (2,*)	INTEGER		Indices for bounding seg endpts *
C*	NSEG		INTEGER		Number of bounding segments     *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		inout (*), ipt (2,*)
	REAL		xlat (*), xlon (*)
C*
	LOGICAL		bnd
C-----------------------------------------------------------------------
	iret = 0
C
	IF ( inout ( 1 ) .eq. 0 ) THEN
C
C*	    The first point is on a bounding segment.
C
	    bnd  = .true.
	    nseg = 1
	    ipt ( 1, nseg ) = 1
	  ELSE
	    bnd  = .false.
	    nseg = 0
	END IF
C
	DO ii = 2, npts
	    IF ( inout ( ii ) .ne. inout ( ii - 1 ) ) THEN
		IF ( bnd ) THEN
C
C*		    Last point of a segment was found.
C
		    ipt ( 2, nseg ) = ii - 1
		    bnd = .false.
		  ELSE
C
C*		    First point of a segment was found.
C
		    nseg = nseg + 1
		    ipt ( 1, nseg ) = ii
		    bnd = .true.
		END IF
	    END IF
	END DO
C
	IF ( bnd ) THEN
	    IF ( nseg .eq. 1 ) THEN
		ipt ( 2, nseg ) = npts
	      ELSE
		ipt ( 1, 1 ) = ipt ( 1, nseg )
		nseg = nseg - 1
	    END IF
	END IF
C*
	RETURN
	END
