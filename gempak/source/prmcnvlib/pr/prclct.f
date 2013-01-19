	FUNCTION PR_CLCT  ( clcl, clcm, clch )
C************************************************************************
C* PR_CLCT								*
C*									*
C* This function computes CLCT from CLCL, CLCM, and CLCH.  CLCT is the	*
C* maximum cloud coverage of CLCL, CLCM, and CLCH, with coverage 	*
C* values ordered as follows:						*
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
C* REAL PR_CLCT  ( CLCL, CLCM, CLCH )					*
C*									*
C* Input parameters:							*
C*	CLCL		REAL		Low cloud cover			*
C*	CLCM		REAL		Medium cloud cover		*
C*	CLCH		REAL		High cloud cover		*
C*									*
C* Output parameters:							*
C*	PR_CLCT		REAL		Maximum cloud cover		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* G. Huffman/GSC        7/88	Documentation				*
C* K. Brill/NMC		11/91	Check for ILOW = 9 when IMED or IHI 	*
C*				not zero.				*
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
	iclcl = clcl
	IF  ((iclcl .ge. 0) .and. (iclcl .lt. 10)) ilow = list ( iclcl )
	iclcm = clcm
	IF  ((iclcm .ge. 0) .and. (iclcm .lt. 10)) imed = list ( iclcm )
	iclch = clch
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
	PR_CLCT = invert ( maxval )
C
	RETURN
	END
