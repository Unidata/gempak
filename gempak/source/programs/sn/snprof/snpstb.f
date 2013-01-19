	SUBROUTINE SNPSTB  ( stndex, stncol, stparm, nstprm, istcol,
     +			     iret )
C************************************************************************
C* SNPSTB								*
C*									*
C* This subroutine gets the list of stability indicies to be used	*
C* in SNPROF.								*
C*									*
C* SNPSTB  ( STNDEX, STNCOL, STPARM, NSTPRM, ISTCOL, IRET )		*
C*									*
C* Input parameters:							*
C*	STNDEX		CHAR*		List of stability indicies	*
C*	STNCOL		CHAR*		Color				*
C*									*
C* Output parameters:							*
C*	STPARM (NSTPRM)	CHAR*		Stability indicies		*
C*	NSTPRM		INTEGER		Number of stability indicies	*
C*	ISTCOL		INTEGER		Color of stability indicies	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC          7/90   Fixed missing stability	parm		*
C* J. Whistler/SSAI	11/93	Return correct number of parms		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stndex, stncol, stparm (*)
C*
	CHARACTER	tmcnd (MMPARM)*12
	LOGICAL		cmpflg (MMPARM), chrflg (MMPARM)
C----------------------------------------------------------------------
	iret = 0
C
C*	Get list of parameters from input string.
C
	CALL IN_PRMC  ( MMPARM, stndex, stparm, tmcnd, nstprm, ier )
C
C*	See which parameters can be computed.
C
	IF  ( nstprm .gt. 0 )  THEN
	    CALL PC_DFST  ( nstprm, stparm, chrflg, cmpflg, nc, ier )
C
C*	    Check each parameter.
C
	    np = 0
	    DO  i = 1, nstprm
		IF  ( .not. cmpflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +1, stparm (i), ier )
		  ELSE IF  ( chrflg (i) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +2, stparm (i), ier )
		  ELSE
		    np = np + 1
		    stparm (np) = stparm (i)
		END IF
	    END DO
C
C*	    If there are parameters, reset computation if required.
C
	    IF  ( np .eq. 0 )  THEN
		nstprm = 0
	      ELSE IF  ( np .ne. nstprm )  THEN
		nstprm = np
		CALL PC_DFST  ( nstprm, stparm, chrflg, cmpflg, nc, 
     +				ier )
	    END IF
	END IF
C
C*	Check color for stability indicies.
C
	CALL ST_ILST  ( stncol, '/', 0, 1, istcol, n, ier )
	IF  ( ( nstprm .gt. 0 ) .and. ( istcol .eq. 0 ) )  THEN
	    CALL ER_WMSG  ( 'SNPROF', +5, ' ', ier )
	    nstprm = 0
	END IF
C
C*	Set conditions on station parameters.
C
	IF  ( nstprm .gt. 0 )  CALL PC_SSCD ( nstprm, tmcnd, ier )
C*
	RETURN
	END
