	SUBROUTINE GR_TMFL  ( gdatim, gdfile, cycle, maxt,
     +			      ntime, timfnd, iret )
C************************************************************************
C* GR_TMFL								*
C*									*
C* This subroutine gets a list of times to be input to the grid		*
C* programs.								*
C*									*
C* GR_TMFL  ( GDATIM, GDFILE, CYCLE, MAXT, NTIME, TIMFND, IRET )	*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		Input grid time     		*
C*	GDFILE		CHAR*		Grid file name or alias		*
C*	CYCLE		CHAR*		Reference cycle time		*
C*	MAXT		INTEGER		Maximum number of times		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times			*
C*	TIMFND (NTIME)	CHAR*		Times				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-20 = no times selected		*
C**									*
C* Log:									*
C* P. Bruehl/Unidata	 8/94	Subroutine in GDCNTR			*
C* M. desJardins/NMC	 8/94	Moved to subroutine			*
C* D.W.Plummer/NCEP	11/96	Started with GR_TLST; converted to 	*
C*				process filename, not file number	*
C* D.W.Plummer/NCEP	 7/98	Add cycle; split gdatim before GD_GTMF	*
C* D.W.Plummer/NCEP	 8/98	Rewrote to improve algorithm		*
C* D.W.Plummer/NCEP	12/98	Replaced gdtl array MXLOOP w/ LLMXGT	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gdfile, timfnd (*), cycle
C*
	CHARACTER*20	gdtlst(LLMXGT)
	CHARACTER*20	timout*40, gdat(MXLOOP), gdtl(LLMXGT)
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
C
C*	If there is a grid with two times (i.e. containing a ':'),
C*	assume that the user has input a simple list of times.
C
	itwotm = INDEX ( gdatim, ':')
	IF  ( itwotm .gt. 0 )  THEN
C
C*	    Break input line into list of times.
C
	    CALL ST_CLST  ( gdatim, ';', ' ', maxt, timfnd, ntime,
     +			    iret )
	    RETURN
C
	END IF
C
	    ngdftm = 0
	    CALL ST_CLST ( gdatim, ';', ' ', MXLOOP, gdat, ngdat, iret )
C
	    DO  n = 1, ngdat
C
	        CALL GD_GTMF ( gdfile, gdat(n), cycle, LLMXGT, 
     +			       ngdtl, gdtl, ier )
C
		DO  k = 1, ngdtl
C
		    found = .FALSE.
		    l = 1
		    DO WHILE ( .not. found .and. l .le. ngdftm )
			IF ( gdtl(k) .eq. gdtlst(l) )  found = .TRUE.
			l = l + 1
		    END DO
C
		    IF ( .not. found )  THEN
			ngdftm = ngdftm + 1
			gdtlst(ngdftm) = gdtl(k)
		    END IF
C
		END DO
C
	    END DO
C
            IF  ( ngdftm .eq. 0 )  THEN
                iret = -4
                CALL ER_WMSG ( 'TG', iret, ' ', ier )
                iret = -20
            END IF
C
C*	Call TG_FIND to get list of times specified in gdatim
C
	CALL TG_FIND ( gdatim, ngdftm, gdtlst, timout, 
     +			ntime, timfnd, ier )
C
	IF ( ( ier .ne. 0 ) .or. ( ntime .eq. 0 ) )  THEN
		iret = -20
		CALL ER_WMSG ( 'GR', iret, ' ', ier )
	END IF
C
	IF ( ntime .gt. LLMXGT ) THEN
		CALL ER_WMSG ( 'TG', +3, ' ', ier )
		ntime = LLMXGT
	END IF
C*
	RETURN
	END
