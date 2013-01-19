	SUBROUTINE GG_BOX  ( region, icolor, ilntyp, ilnwid, iret )
C************************************************************************
C* GG_BOX								*
C*									*
C* This subroutine draws a box around the specified area.  If the	*
C* color is zero, no box is drawn.  If the line type is zero, the	*
C* default line type is used.  If the width is 0, a width of 1 is	*
C* set.									*
C*									*
C* GG_BOX  ( REGION, ICOLOR, ILNTYP, ILNWID, IRET )			*
C*									*
C* Input parameters:							*
C*	REGION		CHAR*		Coordinate region		*
C*					  'D' = device			*
C*					  'N' = normalized		*
C*					  'V' = view			*
C*					  'P' = plot			*
C*	ICOLOR		INTEGER		Color number			*
C*	ILNTYP		INTEGER		Line type			*
C*	ILNWID		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = invalid region		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 1/85						*
C* M. desJardins/GSFC	 6/88	Adapted from GPBOX			*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	CHARACTER	region*(LLMXLN)
C*
	CHARACTER	reg*1
	REAL		x (5), y (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert first character to upper case and check that it is
C*	valid.
C
	CALL ST_LCUC  ( region(1:1), reg, ier )
	IF  ( ( reg .ne. 'D' ) .and. ( reg .ne. 'N' ) .and.
     +	      ( reg .ne. 'V' ) .and. ( reg .ne. 'P' ) )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	If color number is zero, do not draw box.
C
	IF  ( icolor .eq. 0 )  THEN
	    RETURN
	END IF
C
C*	Get region boundaries.
C
	CALL GQBND  ( reg, eft, bot, right, top, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GEMPLT', ier, ' ', iret )
	    iret = -9
	    RETURN
C
C*	    Fill an array with the box coordinates.
C
	  ELSE
	    x (1) = eft
	    y (1) = bot
	    x (2) = right
            y (2) = bot
            x (3) = right
            y (3) = top
            x (4) = eft
            y (4) = top
            x (5) = eft
            y (5) = bot
	END IF
C
C*	Set color and line type numbers.
C
	IF  ( ilnwid .eq. 0 )  THEN
	    jlnwid = 1
	  ELSE
	    jlnwid = ilnwid
	END IF
	CALL GQLINE  ( iltypo, ilthwo, iwido, iwhwo, iret )
	CALL GSCOLR  ( icolor, ier )
	CALL GSLINE  ( ilntyp, 0, jlnwid, 0, iret )
C
C*	Draw box around region.
C
	CALL GLINE  ( reg, 5, x, y, ier )
C
C*	Reset line attributes.
C
	CALL GSLINE  ( iltypo, 0, iwido, 0, ier )
C*
	RETURN
	END
