	SUBROUTINE IN_TXTN  ( text, ifont, ihwsw, siztxt, itxwid,
     +  		      ibrdr, irrotn, ijust, iret )
C************************************************************************
C* IN_TXTN								*
C*									*
C* This subroutine decodes the text string which is in the form:	*
C*									*
C*	    text size / font / width / border / rel rotn /		*
C*	    just / hw, sw flag						*
C*									*
C* IN_TXTN ( TEXT, IFONT, IHWSW, SIZTXT, ITXWID, IBRDR, IRROTN, IJUST,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	TEXT		CHAR*		Text input			*
C*									*
C* Output parameters:							*
C*	IFONT		INTEGER		Text font number		*
C*					  0 = no change			*
C*	IHWSW		INTEGER		Text software/hardware flag	*
C*					  1 = software			*
C*					  2 = hardware			*
C*	SIZTXT		REAL		Text size multiplier		*
C*					 <=0 = no change		*
C*	ITXWID		INTEGER		Text width multiplier		*
C*					 <=0 = no change		*
C*	IBRDR           INTEGER         Text border/blank fill flag     *
C*                                        <=0 = no change               *
C*      IRROTN          INTEGER         Text north-relative rot flag    *
C*                                        1 = screen relative           *
C*                                        2 = north relative            *
C*                                        otherwise = no change         *
C*      IJUST           INTEGER         Text justification              *
C*                                         1 = left                     *
C*                                         2 = center                   *
C*                                         3 = right                    *
C*                                        otherwise = no change         *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C* S. Schotz/GSC	 1/90	Added text width			*
C* S. Maxwell/GSC	 1/97	Removed call to GSTEXT			*
C* S. Maxwell/GSC	 1/97	Modified documentation			*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C* S. Jacobs/NCEP	10/97	Use a temp var so TEXT does not change	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* I. Durham/GSC	12/97   Added variables ibrdr, irrotn, and ijust*
C* I. Durham/GSC	12/97   Allowed for character inputs		*
C* I. Durham/GSC	12/97   Cleaned up comment lines		*
C* S. Jacobs/NCEP	 7/98	Added call to TB_FONT for size names	*
C************************************************************************
	CHARACTER*(*)	text
C*
	CHARACTER	chtext (6) *72
	CHARACTER	sss*160, ctext*160
C------------------------------------------------------------------------
	iret = 0
	sss = text
C
C*	Check for hw/sw flag in string.
C
	ihwsw = 0
	CALL ST_LCUC ( sss, ctext, ier )
	CALL ST_RMST ( ctext, '/HW', ihwpos, ctext, ier )
	CALL ST_RMST ( ctext, '/SW', iswpos, ctext, ier )
	ipos = ihwpos + iswpos
	IF ( ipos .eq. 0 ) THEN
C
C*          Check for flag at beginning of string
C
      	    CALL ST_RMST ( ctext, 'HW/', ihwpos, ctext, ier )
	    CALL ST_RMST ( ctext, 'SW/', iswpos, ctext, ier )
            ipos = ihwpos + iswpos
        END IF
C
        IF  ( ipos .ne. 0 )  THEN
            IF ( ihwpos .ne. 0 ) THEN
		ihwsw = 2
	    ELSE 
		ihwsw = 1
	    END IF
	    CALL ST_LSTR ( ctext, lenstr, ier )
	    sss = ctext ( : lenstr )
	END IF
C
C*	Get six characters from the string.
C
	CALL ST_CLST  ( sss, '/', ' ', 6, chtext, n, ier )
C
C*	Check on text size.
C
	CALL ST_CRNM ( chtext (1), siztxt, ier )
	IF  ( siztxt .le. 0. )  THEN
	    IF  ( ier .eq. 0 )  THEN
		siztxt = -1.
	      ELSE
		CALL TB_FONT  ( chtext(1), rsize, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    siztxt = -1.
		  ELSE
		    siztxt = rsize
		END IF
	    END IF
	  ELSE 
	    siztxt = siztxt
	END IF
C
C*	Get text font and type.
C
	CALL ST_NUMB ( chtext (2), ifont, ier )
C
C*      Get text width.
C
	CALL ST_NUMB ( chtext (3), itxwid, ier )
	IF ( itxwid .lt. 0 ) itxwid = 0
C
C*	Get text border.
C
	CALL ST_NUMB ( chtext (4), ibrdr, ier )
	IF ( ibrdr .lt. 0 ) ibrdr = 0
C
C*	Get text rotation.
C
	CALL ST_LCUC ( chtext (5), chtext (5), ier )
	IF ( chtext (5) .eq. 'N' ) THEN
	    irrotn = 2
	ELSE IF ( chtext (5) .eq. 'S' ) THEN
	    irrotn = 1
	ELSE
	    irrotn = 0
	END IF
C
C*	Get text justification.
C
	CALL ST_LCUC ( chtext (6), chtext (6), ier )
	IF ( chtext (6) .eq. 'R' ) THEN
	    ijust = 3
	ELSE IF ( chtext (6) .eq. 'C' ) THEN
	    ijust = 2
	ELSE IF ( chtext (6) .eq. 'L' ) THEN
	    ijust = 1
	ELSE
	    ijust = 0
	END IF
C*
	RETURN
	END
