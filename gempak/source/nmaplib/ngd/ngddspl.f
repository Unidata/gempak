	SUBROUTINE NGD_DSPL ( panel, dattim, alias, isbcat, cycle,
     +			      rstfil, endtim, mrange, intrvl, match, 
     +			      minute, ititl, iret )
C************************************************************************
C* NGD_DSPL								*
C*									*
C* This routine plots the requested GRID data to the current display	*
C* device.								*
C*									*
C* NGD_DSPL ( PANEL, DATTIM, ALIAS, ISBCAT, CYCLE, RSTFIL, ENDTIM,	*
C*	      MRANGE, INTRVL, MATCH, MINUTE, ITITL, IRET )		*
C*									*
C* Input parameters:							*
C*	PANEL		CHAR*		GEMPAK panel			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	ALIAS		CHAR*		Alias for GRID data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	RSTFIL		CHAR*		Restore file name		*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C*	MATCH		INTEGER		Flag for time match scheme	*
C*	MINUTE		INTEGER		Number of minutes diff for match*
C*	ITITL		INTEGER		Title line			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  +1 = no match for time	*
C*					  -3 = no files found		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C* S. Jacobs/NCEP	10/99	Rearranged parm setting; Close all open	*
C*				files; Clean up				*
C* S. Jacobs/NCEP	 4/00	Use alias as GDFILE instead of full	*
C*				file name; Fixed title line		*
C* R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
C* S. Jacobs/NCEP	 4/01	Check locally for the restore file	*
C* S. Jacobs/NCEP	 7/01	Changed to call NGD_RSFL		*
C* S. Jacobs/NCEP	 2/02	Check return from NGD_RSFL		*
C* T. Lee/SAIC		08/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		01/04	Added reference time flag to NGD_TLST	*
C* T. Lee/SAIC		04/04	Added delta reference time		*
C* R. Tian/SAIC		12/04	Changed for time/file mngmnt		*
C* R. Tian/SAIC		 1/05	Added GD_CLOS				*
C* m.gamazaychikov/SAIC	04/06	Passed tmstrc to TG_MTCH		*
C* S. Chiswell/Unidata	10/06	Changes gdfile from LLMXLN to 256	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MXTARR = 20 * MXNMFL )
C*
	CHARACTER*(*)	panel, dattim, alias, cycle, rstfil, endtim
C*
	CHARACTER	times(MXNMFL)*20, timstr*(MXTARR), dtm*20,
     +			fctm*20, gdfile*256, pname*8, value*256,
     +			gdfval*256, title*256, ttlval*256, ttlln*12,
     +			filnam*256, tmstrc(MXNMFL)*11
	CHARACTER	bangs(20)*256, filnms(MMFILE)*256
	INTEGER		iftm(3)
C------------------------------------------------------------------------
	iret = 0
	dtm  = dattim
	iflag = -1
	idelta = -1
C
C*	Get all times in the range.
C
	CALL NGD_TLST ( alias, cycle, rstfil, isbcat, endtim, mrange, 
     +			intrvl, iflag, idelta, timstr, lenstr, numt, 
     +			iret )
	IF  ( iret .ne. 0 )  RETURN
	CALL ST_CLSL ( timstr, ';', ' ', numt, times, ntime, ier )
C
C*	Make sure the time in GEMPAK format is passed to TG_MTCH
C
        DO ITIME = 1, ntime
           tmstrc ( itime ) = times ( itime ) ( :11 )
        END DO
C
C*	Find the appropriate match for the input time.
C
	CALL TG_MTCH ( match, dtm, tmstrc, ntime, minute, ipos, ierr )
	IF  ( ( ierr .ne. 0 ) .or. ( ipos .eq. 0 ) )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Create a full date/time.
C
	CALL TG_CTOI ( times(ipos), iftm, ier )
	CALL TG_ITOC ( iftm, fctm, ier )
C
C*	Get the actual restore file name.
C
	CALL NGD_RSFL ( rstfil, filnam, lenf, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Read the restore file and save all of the parameters and
C*	values for use in GDPLTB.
C
	CALL FL_SOPN ( filnam, lunf, ier )
	CALL FL_TDAT ( lunf, ier )
C
	iostat = 0
	gdfval = ' '
	DO WHILE  ( iostat .eq. 0 )
	    READ ( lunf, 1000, IOSTAT = iostat ) pname, value
1000	    FORMAT ( A, A )
	    CALL ST_LCUC ( pname, pname, ier )
	    IF  ( iostat .eq. 0 )  THEN
		IF  ( pname(1:1) .ne. '!' ) THEN
		    CALL GDPSTP ( pname, value, ier )
		    IF  ( pname .eq. 'GDFILE' )  THEN
			gdfval = value
		     ELSE IF  ( pname .eq. 'TITLE' )  THEN
			ttlval = value
		    END IF
		END IF
	    END IF
	END DO
C
	CALL FL_CLOS ( lunf, ier )
C
C*	Set values for the control paramters for the GDPLOT2 library.
C
	CALL GDPSTT ( 'PLOT_MAP',    .false., ier )
	CALL GDPSTT ( 'VERBOSE',     .false., ier )
	CALL GDPSTT ( 'SHORT_TITLE', .false., ier )
C
C*	Explicitly set the parameter values that are most important.
C*	(Do not use the file values for the map or latlon lines.)
C*	(Use the input values for gdattim.)
C*	(Use the input value for gdfile or the alias.)
C*	(Replace the title line from the restore file.)
C
	IF  ( gdfval .eq. ' ' )  THEN
	    gdfile = alias // '|' // cycle
	  ELSE
	    gdfile = ' '
	    CALL ST_CLST ( gdfval, '!', ' ', 20, bangs, nbang, ier )
	    DO ibang = 1, nbang
		IF ( ibang .gt. 1 ) THEN
		    CALL ST_LSTR ( gdfile, lens, ier )
		    gdfile = gdfile(:lens) // '!'
		END IF
	        CALL ST_CLST ( bangs(ibang), '+', ' ', MMFILE, 
     +	                       filnms, nfile, ier )
	        DO ifile = 1, nfile
		    IF ( ifile .gt. 1 ) THEN
		        CALL ST_LSTR ( gdfile, lens, ier )
		        gdfile = gdfile(:lens) // '+'
		    END IF
	            IF ( INDEX ( filnms(ifile), '|' ) .gt. 0 ) THEN
		        CALL ST_LSTR ( gdfile, len1, ier )
		        CALL ST_LSTR ( filnms(ifile), len2, ier )
	                gdfile = gdfile(:len1) // 
     +			         filnms(ifile)(:len2)
	              ELSE
		        CALL ST_LSTR ( gdfile, len1, ier )
		        CALL ST_LSTR ( filnms(ifile), len2, ier )
		        CALL ST_LSTR ( cycle, len3, ier )
	                gdfile = gdfile(:len1) // 
     +			         filnms(ifile)(:len2) // '|' // 
     +                           cycle(:len3)
	            END IF
	        END DO
	    END DO
	END IF
	CALL GDPSTP ( 'GDFILE',  gdfile,  ier )
C
	CALL GDPSTP ( 'GDATTIM', fctm,    ier )
	CALL GDPSTP ( 'PANEL',   panel,   ier )
	CALL GDPSTP ( 'MAP',     '0',     ier )
	CALL GDPSTP ( 'LATLON',  '0',     ier )
	CALL GDPSTT ( 'CLEAR',   .false., ier )
C
	CALL ST_INCH ( ititl, ttlln, ier )
	CALL ST_LSTR ( ttlln, lentl, ier )
	IF  ( ttlval .eq. ' ' )  THEN
	    title = '31/' // ttlln
	  ELSE
	    CALL ST_NOCC ( ttlval, '/', 1, ipos1, ier )
	    CALL ST_NOCC ( ttlval, '/', 2, ipos2, ier )
	    title = ttlval(:ipos1) // ttlln(:lentl) // ttlval(ipos2:)
	END IF
	CALL GDPSTP ( 'TITLE',   title,   ier )
C
C*	Plot the data and title using GDPLTB.
C
	CALL GDPLTB ( 2, alias, ier )
	CALL GD_CLOS ( -1, ier )
C*
	RETURN
	END
