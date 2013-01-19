	SUBROUTINE TG_VTOF  ( intvdt, intfdt, iret )
C************************************************************************
C* TG_VTOF								*
C*									*
C* This subroutine converts an integer grid time in V syntax to		*
C* an integer time in F syntax.						*
C*									*
C* TG_VTOF  ( INTVDT, INTFDT, IRET )					*
C*									*
C* Input parameters:							*
C*	INTVDT (3)	INTEGER		Date, time, fcast as V		*
C*									*
C* Output parameters:							*
C*	INTFDT (3)	INTEGER		Date, time, fcast as F		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89						*
C* M. desJardins/GSFC	 4/89	Changed to use integer values		*
C************************************************************************
	INTEGER		intvdt (*), intfdt (*)
C*
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the forecast hours and minutes.
C
	iftype = intvdt (3) / 100000
	iftime = intvdt (3) - iftype * 100000
	ifhour = iftime / 100
	ifmin  = iftime - ifhour * 100
C
C*	Break the time into a five integer array.
C
	idtarr (1) = intvdt (1) / 10000
	iiiiii     = intvdt (1) - idtarr (1) * 10000
	idtarr (2) = iiiiii / 100
	idtarr (3) = iiiiii - idtarr (2) * 100
	idtarr (4) = intvdt (2) / 100
	idtarr (5) = intvdt (2) - idtarr (4) * 100
C
C*	Subtract minutes.
C
	idtarr (5) = idtarr (5) - ifmin
	IF  ( idtarr (5) .lt. 0 )  THEN
	    idtarr (5) = idtarr (5) + 60
	    idtarr (4) = idtarr (4) - 1
	END IF
C
C*	Subtract hours.
C
	idtarr (4) = idtarr (4) - ifhour
	DO WHILE  ( idtarr (4) .lt. 0 )
	    idtarr (4) = idtarr (4) + 24
	    CALL TI_SUBD ( idtarr, idtarr, ier )
	END DO
C
C*	Convert back from five integers to three.
C
	intfdt (1) = idtarr (1) * 10000 + idtarr (2) * 100 + idtarr (3)
	intfdt (2) = idtarr (4) * 100 + idtarr (5)
	intfdt (3) = 100000 + iftime
C*
	RETURN
	END
