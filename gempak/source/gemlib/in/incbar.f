	SUBROUTINE IN_CBAR  ( clrbar, icbar, size, ilblfq, orient,
     +			      cbrflg, ixjust, iyjust, pos, iret )
C************************************************************************
C* IN_CBAR								*
C*									*
C* This subroutine converts the user input for the CBAR variable	*
C* (character string) into the border color, size, orientation, label	*
C* frequency, and color bar line variables.				*
C*									*
C* The CBAR input must be of the form:					*
C*									*
C*	color/orient/anchor/x;y/len;wid/freq				*
C*									*
C* If the line type is set to a single negative number, negative	*
C* values will have the line type specified and positive values		*
C* will be solid (line type = 1).  If the label is set to a single	*
C* value n, then every nth value will be labeled.			*
C*									*
C* IN_CBAR ( CLRBAR, ICBAR, SIZE, ILBLFQ, ORIENT, CBRFLG, IXJUST,	*
C*	     IYJUST, POS, IRET )					*
C*									*
C* Input parameters:							*
C*	CLRBAR		CHAR*		CLRBAR input			*
C*									*
C* Output parameters:							*
C*	ICBAR		INTEGER		Border color			*
C*	SIZE (2)	REAL		Size of color bar in view coord	*
C*	ILBLFQ		INTEGER		Color bar label frequency	*
C*	ORIENT		CHAR*		Color bar orientation H/V	*
C*	CBRFLG		LOGICAL		Color bar flag			*
C*	IXJUST		INTEGER		Horizontal justification	*
C*					  1 = Left			*
C*					  2 = Center			*
C*					  3 = Right			*
C*	IYJUST		INTEGER		Vertical justification		*
C*					  1 = Bottom			*
C*					  2 = Center			*
C*					  3 = Top			*
C*	POS (2)		REAL		Pos of color bar in view coord	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 5/92						*
C* P. Bruehl/Unidata	 4/93	Added LOGICAL declaration of cbrflg	*
C* G. Krueger/EAI	 8/93	Modified parameter list for view coord	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	clrbar, orient
	REAL		size (*), pos (*)
	LOGICAL		cbrflg
C*
	CHARACTER	group (7)*20, just*2
C------------------------------------------------------------------------
	iret = 0
C
C*	Check to see whether a color bar should be drawn.
C
	IF ( clrbar .eq. ' ' .or. clrbar .eq. '0' ) THEN
	    cbrflg = .false.
	    RETURN
	ELSE
	    cbrflg = .true.
	END IF
C
C*	Break list into groups.
C
	CALL ST_CLST  ( clrbar, '/', ' ', 7, group, ng, ier )
C
C*      Decode the color.
C
	IF  ( group (1) .eq. ' ' )  THEN
	    icbar = 1
	ELSE
	    CALL IN_COLR ( group (1), 1, icbar, ier )
	END IF
C
C*	Decode the orientation.
C
	CALL ST_LCUC ( group (2)(1:1), orient, ier )
	IF  ( orient .ne. 'H' )  orient = 'V'
C
C*	Decode the justification.
C
	CALL ST_LCUC ( group (3)(1:2), just, ier )
C
C*	Set the X justification.
C
	IF  ( just (2:2) .eq. 'R' ) THEN
	    ixjust = 3
	ELSE IF ( just (2:2) .eq. 'C' ) THEN
	    ixjust = 2
	ELSE
	    ixjust = 1
	ENDIF
C
C*	Set the Y justification.
C
	IF  ( just (1:1) .eq. 'U' ) THEN
	    iyjust = 3
	ELSE IF ( just (1:1) .eq. 'C' ) THEN
	    iyjust = 2
	ELSE
	    iyjust = 1
	ENDIF
C
C*	Decode the position.
C
	CALL ST_RLST ( group (4), ';', RMISSD, 2, pos, numpos, ier )
	IF  ( pos (1) .lt. 0. )  pos (1) = .005
	IF  ( pos (2) .lt. 0. )  pos (2) = .05
C
C*	Decode the size.
C
	CALL ST_RLST ( group (5), ';', RMISSD, 2, size, num, ier )
	IF  ( size (1) .lt. 0. )  size (1) = .5
	IF  ( size (2) .lt. 0. )  size (2) = .01
C
C*	Decode the label frequency.
C
	IF  ( group (6) .eq. ' ' )  THEN
	    ilblfq = 1
	ELSE
	    CALL ST_LSTR ( group (6), ilen1, ier )
	    CALL ST_INTG ( group (6)(:ilen1), ilblfq ,ier)
	END IF
C*
	RETURN
	END
