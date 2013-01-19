	SUBROUTINE IN_PWND  ( winsym, wintyp, skyflg, condtn, iret )
C************************************************************************
C* IN_PWND								*
C*									*
C* This subroutine decodes the input for a wind barb, a wind arrow, or  *
C* a directional arrow.  The input must be in the form                  *
C*                      : size : width : type : headsiz                 *
C* The arrow/barb size is a multiple of the base size.  Type 1 plots a  *
C* circle or an arrowhead for calm winds.  Type 2 does not plot         *
C* anything for calm winds.  The arrowhead size is a multiple of the    *
C* base arrowhead size.						        *
C* If the user has entered a condition, it must precede the first :     *
C*									*
C* IN_PWND  ( WINSYM, WINTYP, SKYFLG, CONDTN, IRET )			*
C*									*
C* Input parameters:							*
C*	WINSYM		CHAR*		Wind symbol input		*
C*	WINTYP		CHAR*1		Wind type			*
C*					  B = wind barb 		*
C*					  A = wind arrow		*
C*					  D = directional arrow		*
C*	SKYFLG		LOGICAL		Flag set for sky cover plot	*
C*									*
C* Output parameters:							*
C*	CONDTN		CHAR*		Condition			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	From IN_WIND				*
C* M. desJardins/NMC	 3/92	Set head size to arrow size if not	*
C*				explicitly set				*
C* D. Kidwell/NCEP	 5/98   Added GSDARR, cleaned up prologue       *
C************************************************************************
	CHARACTER*(*)	winsym, wintyp, condtn
	LOGICAL		skyflg
C*
	REAL		rwind (4)
	CHARACTER	symbol*24
C------------------------------------------------------------------------
	iret  = 0
C
C*	Check for colon.
C
	ipos = INDEX ( winsym, ':' )
	IF  ( ipos .eq. 0 )  THEN
	    condtn = winsym
	    symbol = ' '
	  ELSE
	    condtn = winsym ( 1:ipos-1 )
	    symbol = winsym ( ipos+1:  )
	END IF
C
C*	Decode wind attributes.
C
	CALL ST_RLST  ( symbol, ':', 0., 4, rwind, n, ier )
	size   = rwind (1)
	iwidth = NINT ( rwind (2) )
	itype  = NINT ( rwind (3) )
        sizehd = rwind (4)
	IF  ( sizehd .eq. 0. )  sizehd = size
C
C*	Set calm circle size if required.
C
	ipart = itype / 10
	ipart = MOD ( ipart, 10 )
	IF  ( ipart .eq. 0 )  THEN
	    IF  ( skyflg )  THEN
		itype = itype + 30
	      ELSE
		itype = itype + 10
	    END IF
	END IF
C
C*	Set size, width, type, and/or arrow head size.
C
	IF  ( wintyp .eq. 'B' )  THEN
	    CALL GSBARB  ( size, iwidth, itype, ier )
	  ELSE IF ( wintyp .eq. 'A' )  THEN
	    CALL GSARRW  ( size, sizehd, iwidth, itype, ier )
	  ELSE IF ( wintyp .eq. 'D' )  THEN
	    CALL GSDARR  ( size, sizehd, iwidth, itype, ier )
	END IF
C*
	RETURN
	END
