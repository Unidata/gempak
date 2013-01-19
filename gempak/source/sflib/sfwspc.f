	SUBROUTINE SF_WSPC ( isffln, ihhmm, string, iret )
C************************************************************************
C* SF_WSPC								*
C*									*
C* This subroutine writes a special airways or metar string of data to	*
C* a surface data file. The time and station must be set before this	*
C* subroutine is called. The station time will be stored if the station	*
C* time flag, STMFLG, was set when the file was created.		*
C*									*
C* The special reports will be written to the file in groups of two at	*
C* a time, with each group being a maximum of 80 characters long.  A    *
C* report can fill at most two groups; at most 160 characters are       *
C* written per report.  At most 30 groups will be written (at least 15, *
C* and at most 30, special reports.)                                    *
C*									*
C* SF_WSPC  ( ISFFLN, IHHMM, STRING, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	IHHMM		INTEGER		Time (HHMM)			*
C*	STRING 		CHAR*		String of data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*					  +2 = special already exists	*
C*				   	  -3 = file not open		*
C*					  -7 = location not set		*
C*				  	 -12 = DM error			*
C*				  	 -29 = number of specials > 30  *
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/96	Copied from SF_WSTR			*
C* D. Kidwell/NCEP	10/98	Changed to save up to 160 characters    *
C* D. Kidwell/NCEP	11/05	Changed to save up to 30 groups         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'   
C*
	CHARACTER*(*)	string	
C*
	CHARACTER	tstr*2400
	INTEGER		idthdr (LLSTHL)
	LOGICAL		done
C------------------------------------------------------------------------
C*	Read any existing data.
C
	CALL SF_RSPC ( isffln, tstr, nch, ihm, nrep, iret )
	IF  ( iret .lt. 0 )  RETURN
C
C*	Check for the max number of special reports.
C
	IF  ( nrep .ge. 30 )  THEN
	    iret = -29
	    RETURN
	END IF
C
C*	If there is no data, yet, set the number of reports to zero.
C
	IF  ( iret .eq. 1 )  THEN
	    nrep = 0
	    iret = 0
	END IF
C
C*	Find the length of the input string. Only use a maximum of 
C*	160 characters, broken into groups of 80.
C
	CALL ST_LSTR ( string, lens, ier )
	length = MIN ( lens, 80 )
C
C*	Check for this data string already in the file.  Only check
C*	the first 80 characters for a match.
C
	DO  i = 1, nrep
	    indx = (i-1) * 80 + 1
	    IF  ( tstr ( indx:indx+length-1 ) .eq. string ( :length ) )
     +		  THEN
		iret = 2
		RETURN
	    END IF
	END DO
C
C*	Check for input string length greater than 160.
C
	leng2 = MIN ( lens, 160 )
	istrt = 1
	iend  = length
	done  = .false.
C
	DO WHILE ( .not. done )
C
C*	    Set the number of reports and the time for the header.
C
	    idthdr (1) = nrep + 1
	    idthdr (2) = ihhmm
C
C*	    Set the output string.
C
	    nchar = ( nrep / 2 + 1 ) * ( 80 * 2 )
	    ipos  = nrep * 80 + 1
	    tstr ( ipos:ipos+length-1 ) = string ( istrt:iend )
C
C*	    See if there is more input to process.
C
	    IF ( ( leng2 .le. 80 ) .or. ( istrt .eq. 81 ) .or.
     +		 ( nrep .eq. 29 ) ) THEN
		done = .true.
	      ELSE
		nrep   = nrep + 1
		length = leng2 - 80
		istrt  = 81
		iend   = leng2
	    END IF
	END DO
C
C*	Write the data to the file.
C
	CALL DM_WDTC  ( isffln, krow (isffln), kcol (isffln),
     +			'SFSP', idthdr, tstr, nchar, ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END
