	PROGRAM  GOFTXT
C************************************************************************
C* PROGRAM GOFTXT  							*
C*									*
C* This program creates offshore forecast text based on the gridded	*
C* data (Grid to Text) within specific bound areas.			*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 8/06						*
C* T. Piper/SAIC	01/08	Added GD_INIT; removed from IN_BDTA	*
C* T. Lee/SAIC		01/08	Removed IP_IDNT and blank in GDATTM	*
C* T. Lee/SAIC		01/08	Added table-driven parameters		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ERMISS.FNC'
C*
	CHARACTER	gdfile*(LLMXLN), gdattm*(LLMXLN)
	CHARACTER	glevel*(LLMXLN), gvcord*(LLMXLN), gfunc*(LLMXLN)
	CHARACTER	alias*(LLMXLN*2)
C*
	CHARACTER	pfunc*80, parm*12, parm2*12
	INTEGER		level(2)
	REAL		gridu(LLMXGD), gridv(LLMXGD)
	CHARACTER	time (LLMXGT)*20, trange*36, timfnd*36
	LOGICAL		respnd, proces, gottm, out, contin
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*	Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*	    Initialize grid library common area grdcmn.cmn.
C
		CALL GD_INIT ( ier )
C
C*		Initialize the DG library.
C
		CALL DG_INTL ( iperr )
C
C*		User input.
C
		PRINT*, 'Enter User I/P Parameters'
		READ ( 5, 10 ) gdfile
		READ ( 5, 10 ) gdattm
		READ ( 5, 10 ) alias
10		FORMAT ( A )
		CALL ST_RMBL ( gdattm, gdattm, ng, ier )
C
C*		Set flag to indicate processing will be done.
C
		out = .true.
		proces = .true.
	      ELSE
		proces = .false.
	    END IF
	  ELSE
	    proces = .false.
	END IF
C
	IF ( proces )  THEN
C
C*	    Set device.
C
	    CALL GG_SDEV  ( 'GN', ier )
	    IF  ( ier .ne. NORMAL )  proces = .false.
C
C*	    Initialize CLO variables.
C
	    CALL CLO_INIT ( ier )
            IF ( ier .ne. NORMAL )  proces = .false.
	END IF
C
C*	Open text table and zone files.
C
	IF ( proces )  THEN
	    CALL G2T_OPEN ( lunb, lunp, lunz, iret )
	    IF ( iret .ne. NORMAL ) THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		proces = .false.
	    END IF
	END IF
C
C*	Read all the zones under the alias and save to common.
C
	IF ( proces )  THEN
C
C*	    Get zone information.
C
	    CALL G2T_RDZONE ( alias, lunz, nzone, iret )
	    IF ( iret .ne. NORMAL ) THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Set quadrant names to common.
C
	    CALL G2T_SQUAD ( alias, iret ) 
	    IF  ( iret .ne. NORMAL )  THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Read parameters from the table.
C
	    CALL G2T_RDPARM ( lunp, iret )
	    IF  ( iret .ne. NORMAL )  THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		proces = .false.
	    END IF
	END IF
C
	IF ( proces )  THEN
	    contin = .true.
	    nz = 1
	    DO WHILE ( contin )
C
C*		Process the GDFILE input.
C
  		CALL DG_NFIL ( gdfile, ' ', ierr )
 		IF ( ierr .ne. NORMAL ) THEN
		    CALL ER_WMSG ( 'DG', ierr, ' ', ier )
		    proces = .false.
		    out = .false.
		  ELSE
		    proces = .true.
		END IF
C
C*		Process the GDATTM input.
C
		IF ( proces ) THEN
		    CALL DG_NDTM ( gdattm, ierr )
		    IF ( ierr .ne. NORMAL ) THEN
			CALL ER_WMSG ( 'DG', ierr, ' ', ier )
			proces = .false.
			out = .false.
		      ELSE
			CALL DG_QTMS  ( LLMXGT, .false., time, ntms,
     +					trange, ierr )
		    END IF
		END IF
C
C*		Initialize G2T variables. Open G2T output files.
C
		CALL G2T_OPNF   ( nz, lunt, ier ) 
		CALL G2T_GSZONE ( nz, nsub, ier )
		CALL G2T_INIT ( iret )
C
C*		Get the time to process.
C
		gottm = proces
		nt = 0
		DO WHILE   ( gottm )
		    CALL DG_NTIM ( .true., .true., time, gottm, ierr )
		    proces = ( ierr .eq. 0 .and. gottm )
		    IF ( ierr .ne. NORMAL ) THEN
			ierr = +2
			CALL ER_WMSG ( 'GOFTXT', ierr, time (1), ier )
		    END IF
     		    CALL TG_DUAL ( time, timfnd, ierr )
C
C*		    Compute the new grid.
C
		    IF  ( proces )  THEN
C
C*			Compute the scalar diagnostic grid.
C
			nt = nt + 1
			glevel = '0'
			gvcord = 'none'
			gfunc  = 'htsgw'
			CALL DG_GRID (  timfnd, glevel, gvcord, 
     +					gfunc, pfunc, gridu, 
     +					igx, igy, time, level, 
     +					ivcord, parm, igerr )
C
			IF  ( igerr .ne. NORMAL )  THEN
			    CALL ER_WMSG ( 'DG', igerr, pfunc, ier )
			    out = .false.
			  ELSE
			    DO nb = 1, nsub
				CALL G2T_DRIV ( 1, nz, nb, nt, time, 
     +						gridu, gridv, igx, igy, 
     +						iret )
				IF ( iret .ne. NORMAL )  THEN
				    iret = -7
				    CALL ER_WMSG ('GOFTXT',iret,' ',ier)
				    out = .false.
				END IF
			    END DO
			END IF
C
C*			Compute the vector diagnostic grid.
C
			glevel = '10'
			gvcord = 'hght'
			gfunc  = 'obs'
			CALL DG_VECR  ( timfnd, glevel, gvcord, 
     +					gfunc, pfunc, gridu, gridv,
     +					igx, igy, time, level, 
     +					ivcord, parm, parm2, igerr )
			IF  ( igerr .ne. NORMAL )  THEN
			    CALL ER_WMSG ( 'DG', igerr, pfunc, ier )
			    out = .false.
			  ELSE
			    DO nb = 1, nsub
				CALL G2T_DRIV ( 2, nz, nb, nt, time, 
     +						gridu, gridv, igx, igy, 
     +						iret )
				IF ( iret .ne. NORMAL )  THEN
				    iret = -8
				    CALL ER_WMSG ('GOFTXT',iret,' ',ier)
				    out = .false.
				END IF
			    END DO
			END IF
		    END IF
		END DO
C
C*		Write out the text.
C
		IF  ( out )  THEN
		    CALL G2T_OUTPUT ( lunt, lunb, nz, ier )	
		    IF ( ier .ne. NORMAL )  THEN
	  		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		    END IF
		END IF
		CALL FL_CLOS ( lunt, ier )
		CALL DG_NEND ( ier )
C
		nz = nz + 1
		IF ( nz .gt. nzone ) contin = .false.
	    END DO
	END IF
C
C*	Close output files.
C
	CALL G2T_CLOS ( lunb, lunp, lunz, ier )
C
	IF ( iperr .ne. NORMAL ) 
     +	    CALL ER_WMSG ( 'GOFTXT', iperr, ' ', ier )
	CALL IP_EXIT  ( iperr )
C*
	END
