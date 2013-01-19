	SUBROUTINE GG_DMAP  ( mapfil, mapcol, maptyp, mapwid, mapflt,
     +			      iret )
C************************************************************************
C* GG_DMAP								*
C*									*
C* This subroutine draws a map on the graphics device.  		*
C*									*
C* GG_DMAP  ( MAPFIL, MAPCOL, MAPTYP, MAPWID, MAPFLT, IRET )		*
C*									*
C* Input parameters:                                                    *
C*      MAPFIL          CHAR*           Map file name                   *
C*      MAPCOL          INTEGER         Map line color                  *
C*      MAPTYP          INTEGER         Map line type                   *
C*      MAPWID          INTEGER         Map line width                  *
C*      MAPFLT          INTEGER         Map point filter		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				 	 -7 = map not drawn		*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 1/97	Taken from GG_MAP			*
C* S. Maxwell/GSC	 1/97	Added check for map alias name.		*
C* S. Jacobs/NCEP	 5/99	Added mapflt to calling sequence	*
C* T. Piper/GSC		 5/01	Added GSSMTH				*
C* T. Piper/SAIC	01/06	Increased mfile to 72 characters	*
C* T. Piper/SAIC	01/06	Set mfile to mapfil when not found	*
C************************************************************************
	CHARACTER*(*)	mapfil
	CHARACTER	malias*12, mname*24, mfile*72, mpfl*72
	LOGICAL		found
C-----------------------------------------------------------------------
	iret = 0
C
C*	Check the table for a map alias name.
C
	CALL ST_UCLC ( mapfil, mpfl, ier )
	CALL FL_TBOP ( 'mapfil.tbl', 'config', lun, ier )
	found = .false.
	iostat = 0
	DO WHILE (( .not. found ) .and. ( iostat .eq. 0 ))
	   READ ( lun, 1000, IOSTAT = iostat ) malias, mname, mfile
1000	   FORMAT ( A, 1X, A, 1X, A )
	   IF ( mpfl .eq. malias ) found = .true.
	END DO
	CALL FL_CLOS ( lun, ier )
C
C*	If mapfil was not found in the table, set mfile to mapfil.
C
	IF ( .not. found ) mfile = mapfil
C
C*	Set the map file name.
C
	IF ( mfile .ne. ' ' )  THEN
	    CALL GSMFIL  ( mfile, ier )
	IF  ( ier .ne. 0 )  CALL ER_WMSG  ( 'GEMPLT', ier,
     +						' ', ierr )
	END IF
C
C*	Check the filter flag and set the line filter.
C
	IF  ( mapflt .eq. 1 )  THEN
	    CALL GSRDUC ( 0.05, ier )
	END IF
C
C*      Check if map is to be drawn, ie. mapcol <> 0.
C
	IF  ( mapcol .gt. 0 )  THEN
C
C*	    Save current line and smooth information.
C
	    CALL GQLINE  ( ilto, ilhwo, iwdo, iwdhwo, ier )
	    CALL GQSMTH  ( ismtyp, dens, ier )
	    CALL GSLINE  ( maptyp, 0, mapwid, 0, ier )
	    CALL GSSMTH  ( 0, 0., ier )
C
C*	    Set map color and draw map.
C
	    CALL GSCOLR  ( mapcol, ier )
	    CALL GDRMAP  ( ier )
	    IF  ( ier .ne. 0 )  THEN	
	        CALL ER_WMSG  ( 'GEMPLT', ier, ' ', iret )
	        iret = -7
	        CALL ER_WMSG  ( 'GG', iret, ' ', ier )
	    END IF
C
C*	    Restore current line and smooth type.
C
	    CALL GSLINE  ( ilto, 0, iwdo, 0, ier )
	    CALL GSSMTH  ( ismtyp, dens, ier )
	END IF
C
C*	Reset the filter factor, if it has been set.
C
	IF  ( mapflt .eq. 1 )  THEN
	    CALL GSRDUC ( 0.0, ier )
	END IF
C*
	RETURN
	END
