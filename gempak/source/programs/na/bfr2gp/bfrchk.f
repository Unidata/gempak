	SUBROUTINE BFRCHK ( lunin, catprm, ncats, ncatvl, catvls, msgok,
     +			    lvlflg, iret )
C************************************************************************
C* BFRCHK								*
C*									*
C* This subroutine reads the values of the screening parameters from	*
C* a Woollen BUFR file and sets logical flag for acceptance.		*
C*									*
C*									*
C*									*
C* BFRCHK ( LUNIN, CATPRM, NCATS, NCATVL, CATVLS, MSGOK, LVLFLG, IRET ) *
C*									*
C* Input parameters:							*
C*	LUNIN		INTEGER		BUFR file unit number		*
C*	CATPRM		CHAR*		Blank-separated mnemonic list	*
C*	NCATS		INTEGER		Number in the list		*
C*	NCATVL (NCATS)	INTEGER		# of values for each mnemonic	*
C*	CATVLS (*)	REAL		Values				*
C*									*
C* Output parameters:							*
C*	MSGOK		LOGICAL		Flag to accept message		*
C*	LVLFLG (*)	LOGICAL		Flag to accept levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-38 = not all chk parms found	*
C**									*
C* Log:									*
C* K. Brill/EMC		 7/98						*
C* K. Brill/HPC		 3/01	Return LVLFLG; error -39		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( MXDAT = 2000 )
C*
	CHARACTER*(*)	catprm
	INTEGER		ncatvl (*)
	REAL		catvls (*)
	LOGICAL		lvlflg (*), msgok
C*
	REAL*8		fbufr (MXDAT)
	LOGICAL		ok, found
	INCLUDE		'ERMISS.FNC'
C----------------------------------------------------------------------
	iret = 0
	msgok = .true.
	IF ( ncats .eq. 0 ) RETURN
C*
	CALL ST_LSTR ( catprm, lng, ier )
	CALL JB_READ ( lunin, catprm(1:lng), MXDAT, fbufr, np, nlv,
     +			ier )
	IF ( ier .ne. 0 .or. np .ne. ncats ) THEN
	    iret = -38
	    RETURN
	END IF
	IF ( nlv .gt. LLMXLV ) THEN
	    iret = -39
	    RETURN
	END IF
C*
	msgok = .false.
	indx = 0
	DO k = 1, nlv
	    ok = .true.
	    DO i = 1, np
		indx = indx + 1
C
C*		Isolate CATVLS values to compare.
C
		IF ( i .ne. 1 ) THEN
		    istrt = 0
		    DO jp = 1, i-1
			istrt = istrt + ncatvl (jp)
		    END DO
		    istop = istrt + ncatvl (i)
		    istrt = istrt + 1
		ELSE
		    istrt = 1
		    istop = ncatvl (1)
		END IF
		found = .false.
		ii = istrt
		DO WHILE (  .not. found .and. ii .le. istop )
		    found = ( ABS ( fbufr (indx) - catvls (ii) ) .lt.
     +			     .0005 )
		    ii = ii + 1
		END DO
		ok = ok .and. found
	    END DO
	    lvlflg (k) = ok
	    msgok = ok .or. msgok
	END DO
C*
	RETURN
	END
