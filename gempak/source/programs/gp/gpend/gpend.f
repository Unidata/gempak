	PROGRAM GPEND
C************************************************************************
C* PROGRAM GPEND							*
C*									*
C* This program terminates GEMPLT.					*
C**									*
C* Log:									*
C* I. Graffman/RDS	 1/85						*
C* M. desJardins/GSFC	 6/88						*
C* K. Brill/NMC		01/92	Replace GERROR with ER_WMSG		*
C* T. Piper/SAIC	03/07	Replaced GG_INIT with CCHECK/CENDMQ	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ADBUFF.CMN'
C
	INTEGER		irecv(2), isend(3)
	LOGICAL		respnd
C-----------------------------------------------------------------------
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPEND', -1, ' ', ier )
	    CALL SS_EXIT
	ENDIF
C
	CALL CCHECK( 0, mbchan, iwact, iret)
	IF ( iret .eq. 0 .and. iwact .eq. 1 )  THEN
C*  GPLT process found; stop it
	    ntypsr = 0
	    irtype = 2
	    iwtype = 1
	    isend(1) = 3
	    isend(2) = FINITP
	    isend(3) = 0
	    CALL GPUT (isend, 3, iret)
	    IF ( iret .eq. NORMAL )  THEN 
		CALL GGET (irecv, 2, ier)
		IF ( ier .eq. NORMAL )  THEN
		    iret = irecv(1)
		ELSE
		    iret = ier
		ENDIF
		IF ( iret .eq. 0 )  THEN
C*  Stop device driver
		    CALL GENDP  ( 1, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG ( 'GEMPLT', iret, ' ', ier )
		    ENDIF
		ELSE
		    CALL ER_WMSG ( 'GEMPLT', iret, ' ', jerr )
		    CALL ER_WMSG ( 'GPEND', -3, ' ', ier )
		ENDIF
	    ENDIF
	ENDIF
	CALL CENDMQ(mbchan, iret)
	CALL IP_EXIT(ier)
	CALL SS_EXIT
	END
