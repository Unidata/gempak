	SUBROUTINE JTB_QPRM ( parms, np, descr, iscl, iref, ibit, unit,
     +			      iret )
C************************************************************************
C* JTB_QPRM								*
C*									*
C* This subroutine returns information about the parameters listed in	*
C* the input array PARMS.						*
C*									*
C* JTB_QPRM ( PARMS, NP, DESCR, ISCL, IREF, IBIT, UNIT, IRET )		*
C*									*
C* Input parameters:							*
C*	PARMS (NP)	CHAR*8		Parameter names			*
C*	NP		INTEGER		# of parameter names		*
C*									*
C* Output parameters:							*
C*	DESCR (NP)	CHAR*40		Parameter descriptions		*
C*	ISCL  (NP)	INTEGER		Scaling values			*
C*	IREF  (NP)	INTEGER		Reference values		*
C*	IBIT  (NP)	INTEGER		Bit count values		*
C*	UNIT  (NP)	CHAR*12		Units				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	parms (*), descr (*), unit (*)
	INTEGER		iscl (*), iref (*), ibit (*)
C*
	CHARACTER*8	prm
	LOGICAL		fnddsc, fndpck
C-----------------------------------------------------------------------
	iret = 0
	descr (1) = ' '
	iscl (1) = 0
	iref (1) = 0
	ibit (1) = 0
	unit (1) = ' '
	IF ( np .eq. 0 ) RETURN
	DO ip = 1, np
	    descr (ip) = ' '
	    iscl (ip) = 0
	    iref (ip) = 0
	    ibit (ip) = 0
	    unit (ip) = ' '
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
		descr (ip) = ' NOT FOUND'
	    ELSE
		descr (ip) = ' NO DESCRIPTION AVAILABLE.'
	    END IF
	    fndpck = fnddsc
	    jpack = jdesc
	    IF ( .not. fndpck ) THEN
		iscl (ip) = 0
		iref (ip) = 0
		ibit (ip) = 0
		unit (ip) = ' '
	    ELSE
		iscl (ip) = ipkscl (jpack)
		iref (ip) = ipkref (jpack)
		ibit (ip) = ipkbit (jpack)
		unit (ip) = pkunit (jpack)
	    END IF
	    prm = parms (ip)
	    CALL ST_LSTR ( prm, lng, ier )
	    IF ( lng .lt. 8 ) THEN
		DO iq = lng+1, 8
		    prm (iq:iq) = ' '
		END DO
	    END IF
	    CALL ST_LSTR ( descr (ip), lng, ier )
	    IF ( lng .lt. 40 ) THEN
		DO iq = lng+1, 40
		    descr (ip) (iq:iq) = ' '
		END DO
	    END IF
	    CALL ST_LSTR ( unit (ip), lng, ier )
	    IF ( lng .lt. 12 ) THEN
		DO iq = lng+1, 12
		    unit (ip) (iq:iq) = ' '
		END DO
	    END IF
	END DO
C*
	RETURN
	END
