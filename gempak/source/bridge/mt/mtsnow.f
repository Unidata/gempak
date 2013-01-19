	SUBROUTINE MT_SNOW ( snincr, lenrmk, islsh, iret )
C************************************************************************
C* MT_SNOW						                *
C*								        *
C* This subroutine will decode depth of fresh snow and total snow 	*
C* amount from the field following key word SNINCR.			*
C* The values are stored in common.					*
C*								        *
C* MT_SNOW ( SNINCR, LENRMK, ISLSH, IRET )                              *
C*								        *
C* Input parameters:						        *
C*	SNINCR		CHAR*		Snow amount string              *
C*      LENRMK		INTEGER		Length of string                *
C*      ISLSH		INTEGER		Position of '/' in string       *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRSNOW)	REAL		Total snow amount in inches     *
C*	RIVALS(IRSNEW)	REAL		Depth of fresh snow in inches   *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  29 = miscoded field 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	11/96	Original author		                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP       6/97   Replaced ST_CRNM with ST_INTG           *
C* D. Kidwell/NCEP       4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	snincr
C------------------------------------------------------------------------
	iret = 0
C
	IF ( ( islsh .gt. 1 ) .and. ( islsh .lt. lenrmk ) ) THEN
	    CALL ST_INTG ( snincr ( 1:islsh - 1 ), isnown, jret )
	    CALL ST_INTG ( snincr ( islsh + 1:lenrmk ), isnowd, kret )
	    rivals ( irsnew ) = FLOAT ( isnown )
	    rivals ( irsnow ) = FLOAT ( isnowd )
	    iret = min ( jret, kret )
	  ELSE
	    iret = -1
	END IF
	IF ( iret .ne. 0 ) iret = 29
C*
	RETURN
	END
