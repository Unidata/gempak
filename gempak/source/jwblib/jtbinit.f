	SUBROUTINE JTB_INIT ( lunbfr, np, tblas, iret )
C************************************************************************
C* JTB_INIT								*
C*									*
C* This subroutine initializes the BUFR table parsing result data	*
C* structures.  The first NP or all TABLE A entries are returned.	*
C*									*
C* JTB_INIT ( LUNBFR, NP, TBLAS, IRET )					*
C*									*
C* Input parameters:							*
C*	LUNBFR		INTEGER		Unit # of BUFR file (connected)	*
C*									*
C* Input and output parameters:						*
C*	NP		INTEGER		Max number of TABLE A names	*
C*					OUTPUT:  actual # of names	*
C* Output parameters:							*
C*	TBLAS  (NP)	CHAR*		Array of entry TABLE A names	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = MXTBLN too small		*
C*					 -5 = No TABLE B entries	*
C*					 -8 = No TABLE D entries	*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	tblas (*)
C*
	LOGICAL		found
C-----------------------------------------------------------------------
	iret = 0
C
C*	Fill the internal TABLE information storage array ENTRYS in
C*	COMMON.
C
	CALL GETABDB ( lunbfr, entrys, MXTBLN, nntrys )
	IF ( nntrys .eq. MXTBLN ) THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Read TABLE A entries for output.
C
	CALL JTB_TBLA ( np, tblas, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Get the TABLE B entry names.
C
	nmbprm = MXPRNM
	CALL JTB_TBLB ( nmbprm, tbbprm, ipkscl, ipkref, ipkbit, pkunit,
     +			iret )
	IF ( iret .ne. 0 ) RETURN
	IF ( nmbprm .eq. 0 ) THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Get the sequence names.
C
	numseq = MXSQNM
	CALL JTB_LDSQ ( MXSQPR, numseq, seqnam, seqprm, nsqprm, iret )
	IF ( iret .ne. 0 ) RETURN
	IF ( numseq .eq. 0 ) THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Flag the actual parameter names in SEQPRM.
C
	DO i = 1, numseq
	    DO j = 1, nsqprm (i)
		found = .false.
		k = 0
		DO WHILE ( .not. found .and. k .lt. nmbprm )
		    k = k + 1
		    CALL ST_LSTR ( seqprm (i,j), l1, ier )
		    CALL ST_LSTR ( tbbprm (k), l2, ier )
		    found = ( seqprm (i,j) (1:l1)
     +			      .eq. tbbprm (k) (1:l2) )
		END DO
		sqpflg (i,j) = found
	    END DO
	END DO
C*
	RETURN
	END
