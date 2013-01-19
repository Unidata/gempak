	SUBROUTINE GH_BKPO ( ibkpt, iarea, iret )
C************************************************************************
C* GH_BKPO								*
C*									*
C* This subroutine draws island segments - in ordered pairs of 		*
C* breakpoints, currently for Cuba, Hispaniola and Puerto Rico.		*
C*									*
C* GH_BKPO ( IBKPT, IAREA, IRET )					*
C*									*
C* Input parameters:							*
C*	IBKPT (2)	INTEGER		Array of breakpoint numbers	*
C*	IAREA		INTEGER		Geographic area designator      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/01   					*
C* A. Hardy/GSC		 6/01	Added color tag;GH_SAVE;GH_REST         *
C* A. Hardy/SAIC	 8/01	Increased ilwidp line widths	        *
C* D. Kidwell/NCEP	 4/03	Changed to use table values in ghcmn    *
C* D. Kidwell/NCEP	 2/04	Corrected prolog only, CCW -> CW        *
C* m.gamazaychikov/SAIC	04/04	From GH_BKHI with modifications		*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		ibkpt (*)
C------------------------------------------------------------------------
	iret = 0
C
	ibeg = indxbk ( iarea )
	iend = indxbk ( iarea + 1 ) - 1
	last = nbkpts ( iarea ) + 1
C
C*	Get the breakpoint pairs for the current type.
C
	ilo  = ibkpt ( 1 )
	ihi  = ibkpt ( 2 )
	IF ( ilo .eq. ihi ) THEN
C
C*          This is the convention used to define the entire island
C
            ilo = 1
            ihi = last
        END IF
	ipass = 1
	IF ( ilo .gt. ihi ) THEN
	    isav = ihi
	    ihi  = last
	    ipass = 2
	END IF
	DO ii = 1, ipass
C
C*	    Find the beginning and ending indices in the
C*	    breakpoint sequence array.
C
	    indxb = 0
	    indxe = 0
	    DO kk = ibeg, iend
		IF ( ibkseq ( kk ) .eq. ilo ) indxb = kk
		IF ( ibkseq ( kk ) .eq. ihi ) indxe = kk
	    END DO
	    IF ( ihi .eq. last ) indxe = iend
	    IF ( ( indxb * indxe ) .gt. 0 ) THEN
		np = indxe - indxb + 1
C
C*		Draw the coastline connecting the points in the 
C*		pair.
C
		CALL GLINE ( 'M', np, bklat ( indxb ),
     +			     bklon ( indxb ), ier )
	    END IF
C
	    IF ( ipass .eq. 2 ) THEN
		ilo = 1
		ihi = isav
	    END IF
	END DO			
C*
	RETURN
	END
