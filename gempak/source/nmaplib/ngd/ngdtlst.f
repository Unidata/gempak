	SUBROUTINE NGD_TLST ( alias, cycle, rstfil, isbcat, endtim,
     +			      mrange, intrvl, iflag, idelrt, timstr, 
     +			      lenstr, ntime, iret )
C************************************************************************
C* NGD_TLST								*
C*									*
C* This routine returns a list of times given GRID information. The 	*
C* alias is used to locate the grid data files on disk. The times are	*
C* returned in a single string separated by a semi-colon (;).		*
C*									*
C* NGD_TLST ( ALIAS, CYCLE, RSTFIL, ISBCAT, ENDTIM, MRANGE, INTRVL,	*
C*		IFLAG, IDELRT, TIMSTR, LENSTR, NTIME, IRET )		*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for GRID data		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	RSTFIL		CHAR*		Restore file			*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C*	IFLAG		INTEGER		Reference time index		*
C*									*
C* Input and output parameters:						*
C*	IDELRT		INTEGER		Minutes in delta time reference	*
C*									*
C* Output parameters:							*
C*	TIMSTR		CHAR*		String containing all times	*
C*	LENSTR		INTEGER		Length of string 		*
C*	NTIME		INTEGER		Number of times returned	*
C*	IRET		INTEGER		Return code			*
C*					  -2 = invalid max time		*
C*					  -3 = no files found		*
C*					  -5 = invalid subcategory	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C* S. Jacobs/NCEP	 7/00	Added more checks for SCAT_ANL type	*
C* R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
C* S. Jacobs/NCEP	 7/01	Added rstfil and check for GDATTIM	*
C* S. Jacobs/NCEP	 9/01	Fixed check for number of times found	*
C* S. Jacobs/NCEP	 9/01	Get all times for analysis-type grids	*
C* S. Jacobs/NCEP	 2/02	Check return from NGD_RSFL		*
C* S. Jacobs/NCEP	 1/03	Removed etime, used original endtim	*
C* T. Lee/SAIC		 8/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		 1/04	Added reference time, auto-update flags	*
C* T. Lee/SAIC		 4/04	Added delta reference time		*
C* R. Tian/SAIC		12/04	Changed gdtval 'ALL' to 'FIRST-LAST'	*
C* R. Tian/SAIC		 1/05	Added GD_CLOS				*
C* S. Jacobs/NCEP	 3/05	Removed special analysis grid check	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	alias, cycle, rstfil, endtim, timstr
C*
	CHARACTER	times(MXNMFL)*20, timarr(MXNMFL)*20, 
     +			tms(MXNMFL)*20, basetm*20,
     +			strtim*20, filnam*256,
     +			gdtval*256, pname*8, value*256, gdfile*256
	INTEGER		itarr(5), jtarr(5)
	LOGICAL		done
C------------------------------------------------------------------------
	iret   = 0
	gdtval = ' '
	basetm = ' '
	iauto = 0
C
C*	Check for a valid time range.  If time range is negative for 
C*	forecast grids, all time periods will be returned.
C
	IF  ( ( isbcat .eq. SCAT_ANL ) .and. ( mrange .le. 0 ) )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Compute the start time of the range from the end time
C*	and the number of minutes. MRANGE could be -1 to get
C*	all forecast periods.
C
	IF  ( mrange .gt. 0 )  THEN
	    CALL TI_CTOI ( endtim, itarr, ier )
	    CALL TI_SUBM ( itarr, mrange, jtarr, ier )
	    CALL TI_ITOC ( jtarr, strtim, ier )
	END IF
C
C*	Check for GDATTIM in the restore file. If it exists, then
C*	use the value to get the list of times. Otherwise, get ALL
C*	of the times.
C
	CALL NGD_RSFL ( rstfil, filnam, lenf, ier )
	IF  ( ier .eq. 0 )  THEN
	    CALL FL_SOPN ( filnam, lunf, ier )
	    CALL FL_TDAT ( lunf, ier )
C
	    iostat = 0
	    gdtval = ' '
	    DO WHILE  ( iostat .eq. 0 )
		READ ( lunf, 1000, IOSTAT = iostat ) pname, value
1000		FORMAT ( A, A )
		CALL ST_LCUC ( pname, pname, ier )
		IF  ( iostat .eq. 0 )  THEN
		    IF  ( pname(1:1) .ne. '!' ) THEN
			IF  ( pname .eq. 'GDATTIM' )  THEN
			    gdtval = value
			    iostat = -1
			END IF
		    END IF
		END IF
	    END DO
C
	    CALL FL_CLOS ( lunf, ier )
	END IF
C
	IF  ( gdtval .eq. ' ' )  THEN
	    gdtval = 'FIRST-LAST'
	END IF
C
C*	Get the times for the type of data.
C
	gdfile = alias // '|' // cycle
	CALL GDPTMS ( gdtval, gdfile, ' ', MXNMFL,
     +		      nt, tms, ier )
C
C*	Convert all the times to valid times.
C
	done = .false.
	knt  = 0
	i    = 1
	DO WHILE  ( ( i .le. nt ) .and. ( .not. done ) )
C
C*	    If this is a set of analyses, check the time range given.
C
	    IF  ( isbcat .eq. SCAT_ANL )  THEN
		CALL TG_DIFF ( strtim, tms(i), mdif, ier1 )
		CALL TG_DIFF ( tms(i), endtim, ndif, ier2 )
		IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		    IF  ( ( mdif .le. 0 ) .and. ( ndif .le. 0 ) )  THEN
			knt = knt + 1
			CALL TG_VALD ( tms(i), timarr(knt), ier )
		      ELSE IF  ( ndif .gt. 0 )  THEN
			done = .true.
		    END IF
		END IF
	      ELSE
C
C*		Otherwise, just convert the time.
C
		knt = knt + 1
		CALL TG_VALD ( tms(i), timarr(knt), ier )
	    END IF
	    i = i + 1
	END DO
C
C*	Return grid times based on time range and interval. 
C
	IF  ( isbcat .eq. SCAT_ANL )  THEN
	    idir = 0
	    CALL TI_TMLN  ( timarr, knt, mrange, intrvl, idir, iflag,
     +			    iauto, basetm, endtim, idelrt, times, ntime,
     +			    iret ) 
	  ELSE
C
C*	    Return number of frames based on time range and interval.
C
	    idir = 1
	    CALL TI_TMLN  ( timarr, knt, mrange, intrvl, idir, iflag, 
     +			    iauto, basetm, endtim, idelrt, times, ntime,
     +			    iret ) 
	END IF
C
C*	Construct a single string from the array of times.
C
	IF  ( iret .eq. 0 )  THEN
C
C*	    Check for a valid number of times.
C
	    IF  ( nt .eq. 0 )  THEN
		iret = -3
	      ELSE
	      	IF  ( knt .eq. 0 )  THEN
		    iret = -7
		  ELSE
C
C*		    Sort the times.
C
		    CALL TI_SORT ( ntime, times, times, ier )
C
C*	    	    Create the string of times.
C
		    CALL ST_LSTC ( times, ntime, ';', timstr, ier )
		    CALL ST_LSTR ( timstr, lenstr, ier )
		END IF
	    END IF
	END IF
	CALL GD_CLOS ( -1, ier )
C*
	RETURN
	END
