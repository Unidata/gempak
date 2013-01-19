	SUBROUTINE RA_P24I ( ipxaft, asoflg, p24i, iret )
C************************************************************************
C* RA_P24I								*
C*									*
C* This subroutine decodes the 24-hr precipitation			*
C* in an airways report.						*
C*									*
C* RA_P24I  ( IPXAFT, ASOFLG, P24I, IRET )						*
C*									*
C* Input parameters:							*
C*	IPXAFT		INTEGER		First field after max/min	*
C*					temperature			*
C*	ASOFLG		LOGICAL		ASOS station flag		*
C*									*
C* Output parameters:							*
C*	P24I		REAL		24-hr precipitation in inches	*
C*	IRET		INTEGER		Return code			*
C*					   0  = normal return		*
C*					  -1  = p24i not found		*
C**									*
C* Log:									*
C* K. MILES/FSU	 6/90							*
C* J. Walker/FSU	 1/91		Rewrote				*
C* P. Bruehl/Unidata	 2/94		Changed search slightly		*
C* P. Bruehl/Unidata	 3/94		Added ASOS precip		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C
	LOGICAL		done, asoflg
	REAL		p24i
C-----------------------------------------------------------------------
	iret   = 0
	p24i   = RMISSD
	inc    = ipxaft
	done   = .false.
C
C*	Look for a 5 digit number that starts with a 2.
C
	DO WHILE  ( .not. done )
	    IF  ( inc .gt. nfield )  THEN
		done = .true.
		inc = ipxaft
		iret = -1
	    ELSE IF  ( ( asoflg) .and.
     +	               ( ifsize (inc) .eq. 5 ) .and.
     +		       ( iftype (inc) .eq. 2 ) .and.
     +		       ( (ifintg (inc) / 10000 ) .eq. 7 ) )  THEN
C
C*		ASOS station - 7RRRR group
C
		done = .true.
		p24i = MOD ( ifintg (inc), 70000) / 100.
	    ELSE IF  ( ( .not. asoflg ) .and.
     +		       ( ifsize (inc) .eq. 5 ) .and.
     +		       ( iftype (inc) .eq. 2 ) .and.
     +		       ( (ifintg (inc) / 10000 ) .eq. 2 ) )  THEN
		done = .true.
		p24i = MOD ( ifintg (inc), 20000) / 100.
		IF  ( p24i .gt. 99.99 )  p24i = RMISSD
	    ELSE
		inc = inc + 1
	    END IF
	END DO 
C*
	RETURN
	END
