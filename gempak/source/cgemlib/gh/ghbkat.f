	SUBROUTINE GH_BKAT ( iseq, number, dev, jpri, iret )
C************************************************************************
C* GH_BKAT								*
C*									*
C* This subroutine sets the color and line width attributes for a       *
C* breakpoint line, and sets the order for plotting the watch/warning   *
C* lines.                                                               *
C*									*
C* GH_BKAT ( ISEQ, NUMBER, DEV, JPRI, IRET )                            *
C*									*
C* Input parameters:							*
C*      ISEQ		INTEGER         Sequence index value            *
C*	NUMBER (*)	INTEGER		Counts of breakpoints or pairs  *
C*	DEV		CHAR*		Device                          *
C*									*
C* Output parameters:							*
C*	JPRI		INTEGER		Watch/warn type flag            *
C*					  1 = hurricane warning         *
C*					  2 = hurricane watch           *
C*					  3 = tropical storm warning    *
C*					  4 = tropical storm watch      *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/03	Common code from GH_BKPR,GH_BKLI,GH_BKHI*
C************************************************************************
	CHARACTER*(*)	dev
	INTEGER		number (*)
C*
	INTEGER		iorder (4), ilwid (4), icolr (4), ilwidp (4)
	CHARACTER	hscolr (4)*12, coltag*33
C*
        DATA            iorder /  4,  2,  3,  1 /
	DATA		ilwid  /  7, 13,  7, 13 / 
	DATA		ilwidp / 28, 52, 28, 52 /
	DATA		icolr  / 20, 11,  4,  2 /
	DATA		hscolr  / 'w_ts_watch', 'w_hur_watch',
     +                            'w_ts_warn', 'w_hur_warn' /
C------------------------------------------------------------------------
	iret = 0
C
	jpri   = iorder ( iseq )
	IF ( number ( jpri ) .gt. 0 ) THEN
            coltag = hscolr ( iseq )
            CALL ST_LSTR ( coltag, lens, ier )
            CALL GH_COLR ( coltag ( :lens ), icolr ( iseq ), ier)
	    IF ( dev ( :2 ) .ne. 'PS' ) THEN
	        llwid = ilwid ( iseq )
	      ELSE
	        llwid = ilwidp ( iseq )
	    END IF
            CALL GSLINE ( 0, 0, llwid, 0, ier )
	END IF
C*
	RETURN
	END
