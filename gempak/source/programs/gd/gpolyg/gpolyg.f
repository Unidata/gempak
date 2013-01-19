	PROGRAM  GPOLYG
C************************************************************************
C* PROGRAM GPOLYG  							*
C*									*
C* This program creates offshore warning polygons from gridded data. To	*
C* use this program properly, a set of map bounds needs to be created 	*
C* in poly_parm.tbl.  These maps are used to clip against the polygons	*
C* of interests.  Two outputs are generated after executing of the	*
C* program: a set of VG files which plot the original polygons (dashed	*
C* lines) and warning polygons (shaded areas) based on warning criteria.*
C* A set of XML files which contain vertices (lat/lon) of the polygons	*
C* in common alerting protocol (CAP) format.				*   
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 2/08						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ERMISS.FNC'
C*
	CHARACTER	gdfile*(LLMXLN), gdattm*(LLMXLN), garea*(LLMXLN)
	CHARACTER	glevel*(LLMXLN), gvcord*(LLMXLN), gfunc*(LLMXLN)
	CHARACTER	gzone*(LLMXLN)
C*
	CHARACTER	pfunc*80, parm*12
	CHARACTER	garout*72, prjout*72
	INTEGER		level(2)
	REAL		grid(LLMXGD)
	CHARACTER	time (LLMXGT)*20, trange*36, timfnd*36
	LOGICAL		respnd, proces, gottm
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF ( iperr .eq. 0 )  THEN
C
C*	    Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*		Initialize the grid library common area grdcmn.cmn.
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
		READ ( 5, 10 ) gzone
		READ ( 5, 10 ) garea
10		FORMAT ( A )
		CALL ST_RMBL ( gdattm, gdattm, ng, ier )
C
C*		Set flag to indicate processing will be done.
C
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
	    CALL GG_SDEV  ( 'GN', ierr )
	    IF  ( ier .ne. NORMAL )  THEN
		CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Initialize CLO variables.
C
	    CALL CLO_INIT ( ierr )
	    IF ( ierr .ne. NORMAL )  THEN
		CALL ER_WMSG ( 'CLO', ierr, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Set variables to common.
C
	    CALL POLY_SZONE ( gzone, gdattm, ierr )
	    IF ( ierr .ne. NORMAL )  THEN
		CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
		proces = .false.
	    END IF
	 END IF
C
	IF ( proces )  THEN
C
C*	    Process the GDFILE input.
C
  	    CALL DG_NFIL ( gdfile, ' ', ierr )
 	    IF ( ierr .ne. NORMAL ) THEN
		CALL ER_WMSG ( 'DG', ierr, ' ', ier )
		proces = .false.
	      ELSE
		proces = .true.
	    END IF
C
C*	    Process the GDATTM input.
C
	    IF ( proces ) THEN
		CALL DG_NDTM ( gdattm, ierr )
		IF ( ierr .ne. NORMAL ) THEN
		    CALL ER_WMSG ( 'DG', ierr, ' ', ier )
		    proces = .false.
		  ELSE
		    CALL DG_QTMS  ( LLMXGT, .false., time, ntms,
     +				    trange, ierr )
		END IF
	    END IF
C
C*          Set the map projection and graphics area.
C
	    IF  ( proces )  THEN
		CALL DG_FIXA ( 'GRID', 'CED', garout, prjout, ier )
		CALL GG_MAPS ( prjout, garout, ' ', idrpfl, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the time to process.
C
	    gottm = proces
	    nt = 0
	    DO WHILE ( gottm )
		CALL DG_NTIM ( .true., .true., time, gottm, ierr )
		proces = ( ierr .eq. 0 .and. gottm )
		IF ( ierr .ne. NORMAL ) THEN
		    ierr = +1
		    CALL ER_WMSG ( 'GPOLYG', ierr, time (1), ier )
		END IF
     		CALL TG_DUAL ( time, timfnd, ierr )
C
C*		Compute the new grid.
C
		IF  ( proces )  THEN
		    nt = nt + 1
C
C*		    Initialize POLY variables.
C
		    CALL POLY_INIT ( iret )
C
		    CALL POLY_RDPARM ( ierr )
		    IF ( ierr .ne. NORMAL )  THEN
			CALL ER_WMSG ( 'GPOLYG', ierr, ' ', ier )
			proces = .false.
		    END IF
C
C*		    Get latitude/longitude of the grid points.
C
		    IF ( nt .eq. 1 .and. proces )  THEN
			CALL DG_GRID  ( timfnd, '0', 'none', 'latr', 
     +					pfunc, grid, igx, igy, time, 
     +					level, ivcord, parm, ierr )
C
			IF  ( ierr .ne. NORMAL )  THEN
			    CALL ER_WMSG ( 'DG', ierr, pfunc, ier )
			    iret = -5
			    CALL ER_WMSG ( 'GPOLYG', iret, ' ', ier )
			  ELSE
			    CALL POLY_LTLN ( 1, grid, igx, igy, ier )
			END IF
C
			CALL DG_GRID  ( timfnd, '0', 'none', 'lonr', 
     +					pfunc, grid, igx, igy, time, 
     +					level, ivcord, parm, ierr )
C
			IF  ( ierr .ne. NORMAL )  THEN
			    CALL ER_WMSG ( 'DG', ierr, pfunc, ier )
			    iret = -5
			    CALL ER_WMSG ( 'GPOLYG', iret, ' ', ier )
			  ELSE
			    CALL POLY_LTLN ( 2, grid, igx, igy, ier )
			END IF
		    END IF
C
C*		    Compute the vector diagnostic grid.
C
		    glevel = '10'
		    gvcord = 'hght'
		    gfunc  = 'maxv'
		    IF ( proces )  THEN
			CALL DG_GRID  ( timfnd, glevel, gvcord, gfunc, 
     +					pfunc, grid, igx, igy, time, 
     +					level, ivcord, parm, ierr )
			IF  ( ierr .ne. NORMAL ) THEN
			    CALL ER_WMSG ( 'DG', ierr, pfunc, ier )
			    iret = -6
			    CALL ER_WMSG ( 'GPOLYG', iret, ' ', ier )
			  ELSE
			    CALL POLY_DRIV ( nt, time, grid, ierr )
			    IF ( ierr .ne. NORMAL )  THEN
				iret = -3
				CALL ER_WMSG ('GPOLYG', ierr, ' ', ier )
			    END IF
			END IF
		    END IF
		END IF
	    END DO
C
	    CALL DG_NEND ( ier )
C
	END IF
C*
	STOP
	END
