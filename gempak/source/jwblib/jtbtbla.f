	SUBROUTINE JTB_TBLA ( np, parm, iret )
C************************************************************************
C* JTB_TBLA								*
C*									*
C* This subroutine scans information dumped by SUBROUTINE GETABDB to	*
C* get a list of the first NP Table A entries found.			*
C*									*
C* If NP = 0, no Table A entries were found.				*
C*									*
C* JTB_TBLA ( NP, PARM, IRET )						*
C*									*
C* Input and output parameters						*
C*	NP		INTEGER		Max number of parm names	*
C*					OUTPUT:  actual # of parm names	*
C*									*
C* Output parameters:							*
C*	PARM   (NP)	CHAR*		Array of entry parm names	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	parm (*)
C*
	CHARACTER*1	ctype
	CHARACTER*8	substr (64)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan for A parameters.  They are only referenced once in the
C*	table, as a D parameter.
C
	ip = 0
	ie = 0
	ctype = 'D'
	DO WHILE ( ie .lt. nntrys .and. ip .lt. np .and.
     +		   ctype .eq. 'D' )
	    ie = ie + 1
	    ctype = entrys (ie) (1:1)
	    IF ( ctype .eq. 'D' ) THEN
		CALL ST_CLST ( entrys (ie), ' ', ' ', 64, substr,
     +			       num, ier )
		imp = 0
		iq = 0
		DO WHILE ( imp .lt. nntrys .and. ctype .eq. 'D' .and.
     +			   iq .eq. 0 .and. ier .ge. 0 )
		    imp = imp + 1
		    ctype = entrys (imp) (1:1)
		    IF ( ctype .eq. 'D' ) THEN
			iq = INDEX ( entrys (imp), substr (2) )
			IF ( iq .eq. 3 ) iq = 0
		    END IF
		END DO
		IF ( iq .eq. 0 ) THEN
		    ip = ip + 1
		    parm (ip) = substr (2)
		END IF
		ctype = 'D'
	    END IF
	END DO
	np = ip
C*
	RETURN
	END
