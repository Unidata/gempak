	FUNCTION PR_CTCC  ( chc1, chc2, chc3 )
C************************************************************************
C* PR_CTCC								*
C*									*
C* This function computes CLCT from CHC1, CHC2, and CHC3.  CLCT is the	*
C* maximum cloud coverage in CHC1, CHC2, and CHC3, which are the 	*
C* combined cloud height and coverage parameters, with coverage values 	*
C* ordered as follows:							*
C* 									*
C*                0      no clouds					*
C*                1      clear						*
C*                6      thin scattered					*
C*                2      scattered					*
C*                7      thin broken					*
C*                3      broken						*
C*                8      thin overcast					*
C*                4      overcast					*
C*                9      partially obscured				*
C*                5      obscured					*
C*									*
C* REAL PR_CTCC  ( CHC1, CHC2, CHC3 )					*
C*									*
C* Input parameters:							*
C*	CHC1		REAL		Low cloud combined hght/cvr	*
C*	CHC2		REAL		Medium cloud combined hght/cvr	*
C*	CHC3		REAL		High cloud combined hght/cvr	*
C*									*
C* Output parameters:							*
C*	PR_CTCC		REAL		Maximum cloud cover		*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91	Created from PR_CLCT			*
C************************************************************************
	INTEGER		list ( 0: 9 ), invert ( 10 )
C*
	DATA		list   / 1, 2, 4, 6, 8, 10, 3, 5, 7, 9 /
	DATA		invert / 0, 1, 6, 2, 7,  3, 8, 4, 9, 5 /
C------------------------------------------------------------------------
C*	Compute ordered values for each cloud cover, with checks for
C*      out-of-bounds values.
C
	ilow = 1
	imed = 1
	ihi  = 1
	iclcl = MOD ( NINT ( chc1 ), 10 )
	IF  ((iclcl .ge. 0) .and. (iclcl .lt. 10)) ilow = list ( iclcl )
	iclcm = MOD ( NINT ( chc2 ), 10 )
	IF  ((iclcm .ge. 0) .and. (iclcm .lt. 10)) imed = list ( iclcm )
	iclch = MOD ( NINT ( chc3 ), 10 )
	IF  ((iclch .ge. 0) .and. (iclch .lt. 10)) ihi  = list ( iclch )
C
C*	Compute maximum value.
C
	maxval = MAX ( ilow, imed, ihi )
C*
	IF ( maxval .eq. 9 ) THEN
	    maxval = MAX ( imed, ihi )
	    IF ( maxval .lt. 3 ) maxval = 9
	END IF
C
C*	Invert maximum value to get numeric cloud coverage.
C
	PR_CTCC = invert ( maxval )
C
	RETURN
	END
