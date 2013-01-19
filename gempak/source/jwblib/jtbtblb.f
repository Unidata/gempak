	SUBROUTINE JTB_TBLB ( np, parm, iscl, iref, ibit, punit, iret )
C************************************************************************
C* JTB_TBLB								*
C*									*
C* This subroutine scans information dumped by SUBROUTINE GETABDB to	*
C* get a list of the Table B entries.					*
C*									*
C* If NP = 0, there are no table B entries.				*
C*									*
C* JTB_TBLB ( NP, PARM, ISCL, IREF, IBIT, PUNIT, IRET )			*
C*									*
C* Input and output parameters						*
C*	NP		INTEGER		Max number of parm names	*
C*					OUTPUT:  actual # of parm names	*
C*									*
C* Output parameters:							*
C*	PARM   (NP)	CHAR*		Array of parameter names	*
C*	ISCL    (NP)	INTEGER		Power of 10 scaling		*
C*	IREF 	(NP)	INTEGER		Scaled reference value		*
C*	IBIT 	(NP)	INTEGER		Number of bits			*
C*	PUNIT	(NP)    CHAR*(*)	Parameter array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = Missing packing info	*
C*					 -2 = Too many Table B entries	*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	parm (*), punit (*)
	INTEGER		iscl (*), iref (*), ibit (*)
C*
	INTEGER		ipkp (3)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Scan for Table B parameters.
C
	ip = 0
	ie = 0
	DO WHILE ( ie .lt. nntrys )
	    ie = ie + 1
	    IF ( entrys (ie) (1:1) .eq. 'B' ) THEN
		ip = ip + 1
		IF ( ip .gt. np ) THEN
		    iret = -2
		    RETURN
		END IF
		parm (ip) = entrys (ie) (3:10)
		punit (ip) = entrys (ie) (12:33)
		CALL ST_ILST ( entrys (ie) (35: ), ' ', -9999, 3,
     +			       ipkp, num, ier )
		IF ( ier .ne. 0 .or. num .ne. 3 ) THEN
		    iret = -1
		    RETURN
		END IF
		iscl (ip) = ipkp (1)
		iref (ip) = ipkp (2)
		ibit (ip) = ipkp (3)
	    END IF
	END DO
	np = ip
C*
	RETURN
	END
