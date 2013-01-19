	SUBROUTINE JTB_PDMP ( lunout, seqnm, parms, np, iret )
C************************************************************************
C* JTB_PDMP								*
C*									*
C* This subroutine writes out information about each parameter in	*
C* parms.								*
C*									*
C* JTB_PDMP ( LUNOUT, SEQNM, PARMS, NP, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNOUT		INTEGER		Open output unit number		*
C*	SEQNM		CHAR*		Sequence mnemonic		*
C*	PARMS (NP)	CHAR*8		Parameter names			*
C*	NP		INTEGER		# of parameter names		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = wrtie error		*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C* T. Lee/GSC		12/98	Fixed FORMAT statement, X -> 1X		*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	seqnm, parms (*)
C*
	CHARACTER*40	descr
	CHARACTER*8	prm
	CHARACTER*12	unit
	LOGICAL		fnddsc, fndpck
C-----------------------------------------------------------------------
	iret = 0
	WRITE (lunout, 1001, IOSTAT=ios) seqnm
1001	FORMAT ( /, ' FOR SEQUENCE ', A, ':', / )
	IF ( ios .ne. 0 ) THEN
	    iret = -10
	    RETURN
	END IF
	IF ( np .eq. 0 ) RETURN
	WRITE (lunout, 1002)
1002	FORMAT ( 3X, 'NAME', 12X, 'PARAMETER DESCRIPTION', 11X, 'SCL',
     +          4X, 'REF', 3X, 'BIT', 4X, 'UNITS' )
1003	FORMAT ( 1X, A8, 1X, A40, 1X, I3, 1X, I8, 1X, I3, 1X, A12 )
	DO ip = 1, np
	    fnddsc = .false.
	    jdesc = 0
	    CALL ST_LSTR ( parms (ip), l1, ier )
	    DO WHILE ( .not. fnddsc .and. jdesc .lt. nmbprm )
		jdesc = jdesc + 1
		CALL ST_LSTR ( tbbprm (jdesc), l2, ier )
		fnddsc = ( parms (ip) (1:l1) .eq.
     +			   tbbprm (jdesc) (1:l2) )
	    END DO
	    IF ( .not. fnddsc ) THEN
		descr = ' NOT FOUND'
	    ELSE
		descr = ' NO DESCRIPTION AVAILABLE'
	    END IF
	    fndpck = fnddsc
	    jpack = jdesc
	    IF ( .not. fndpck ) THEN
		iscl = 0
		iref = 0
		ibit = 0
		unit = ' '
	    ELSE
		iscl = ipkscl (jpack)
		iref = ipkref (jpack)
		ibit = ipkbit (jpack)
		unit = pkunit (jpack)
	    END IF
	    prm = parms (ip)
	    CALL ST_LSTR ( prm, lng, ier )
	    IF ( lng .lt. 8 ) THEN
		DO iq = lng+1, 8
		    prm (iq:iq) = ' '
		END DO
	    END IF
	    CALL ST_LSTR ( descr, lng, ier )
	    IF ( lng .lt. 40 ) THEN
		DO iq = lng+1, 40
		    descr (iq:iq) = ' '
		END DO
	    END IF
	    CALL ST_LSTR ( unit, lng, ier )
	    IF ( lng .lt. 12 ) THEN
		DO iq = lng+1, 12
		    unit (iq:iq) = ' '
		END DO
	    END IF
	    WRITE (lunout, 1003) prm (1:8), descr (1:40),
     +				 iscl, iref, ibit, unit (1:12)
	END DO
C*
	RETURN
	END
