	SUBROUTINE IN_MARK  ( marker, mkcolr, iret )
C************************************************************************
C* IN_MARK								*
C*									*
C* This subroutine decodes the marker string which is in the form:	*
C*									*
C*          color # / marker # / size / width / hw, sw flag		*
C*									*
C* Note that the hw, sw flag can appear anywhere in the string.		*
C*									*
C* The marker size is a real number which is a multiplier for the	*
C* base marker size.  If the size is 0.0, the current size will be 	*
C* used.  If the marker color is 0, no marker will be drawn.  If	*
C* the marker color is blank, color number 1 will be used.  If 		*
C* the marker number is missing or 0, the current marker number		*
C* will be used.  The marker type, size and width are set in this	*
C* subroutine, while the color is returned so that the program may	*
C* set it when actually plotting markers.  The GEMPLT package must	*
C* be initialized before this subroutine is called.			*
C*									*
C* IN_MARK  ( MARKER, MKCOLR, IRET )					*
C*									*
C* Input parameters:							*
C*	MARKER		CHAR*		Marker input			*
C*									*
C* Output parameters:							*
C*	MKCOLR		INTEGER		Marker color			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* G. Huffman/GSC	 1/89	Color .lt. 0 reset to 0			*
C* S. Schotz/GSC	 1/90	Added marker width			*
C* M. desJardins/GSFC	 9/90	Fix default color; call IN_COLR		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	marker
C*
	REAL		rmark (4)
	CHARACTER	ctemp*48, color*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for hw/sw flag in string.
C
	CALL ST_LCUC  ( marker, ctemp, ier )
	CALL ST_RMST  ( ctemp, '/HW', ihwpos, ctemp, ier )
        CALL ST_RMST  ( ctemp, '/SW', iswpos, ctemp, ier )
	ipos = ihwpos + iswpos
	IF  ( ipos .eq. 0 )  THEN
C
C*          Check for flag at beginning of string
C
   	    CALL ST_RMST  ( ctemp, 'HW/', ihwpos, ctemp, ier )
            CALL ST_RMST  ( ctemp, 'SW/', iswpos, ctemp, ier )
	    ipos = ihwpos + iswpos
        END IF
C*
	IF  ( ihwpos .ne. 0 )  THEN
	    ihwsw = 2
	  ELSE IF  ( iswpos .ne. 0 )  THEN
	    ihwsw = 1
	  ELSE
	    ihwsw = 0
	END IF
C
C*	Extract color from string.
C
	ipos =  INDEX ( ctemp, '/' )
	IF  ( ipos .eq. 0 )  THEN
	    color = ctemp
	  ELSE IF  ( ipos .eq. 1 )  THEN
	    color = ' '
	  ELSE
	    color = ctemp ( : ipos-1 )
	END IF
	CALL IN_COLR  ( color, 1, mkcolr, ier )
C
C*	Get four reals from the string and ignore color.
C
	CALL ST_RLST  ( ctemp, '/', 0., 4, rmark, n, ier )
C
C*	Check on marker size.
C
	sizmrk = rmark (3)
C
C*      Get marker width
C
	imkwid = NINT ( rmark (4) )
C
C*	Get marker type.
C
	imtype = NINT ( rmark (2) )
C
C*	Set marker values.
C
	CALL GSMRKR  ( imtype, ihwsw, sizmrk, imkwid, ier )
C*
	RETURN
	END
