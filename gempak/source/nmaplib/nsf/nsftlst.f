	SUBROUTINE NSF_TLST ( alias, cycle, isbcat, endtim, mrange,
     +			      intrvl, iflag, idelrt, timstr, lenstr, 
     +			      ntime, iret )
C************************************************************************
C* NSF_TLST								*
C*									*
C* This routine returns a list of times given SFC information. The 	*
C* alias is used to locate the image data files on disk. The times are	*
C* returned in a single string separated by a semi-colon (;).		*
C*									*
C* NSF_TLST ( ALIAS, CYCLE, ISBCAT, ENDTIM, MRANGE, INTRVL, IFLAG,	*
C*		IDELRT, TIMSTR,	LENSTR,	NTIME, IRET )			*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for SFC data		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C* 	IFLAG		INTEGER		Reference time index		*
C*									*
C* Input and output parameters:						*
C*	IDELRT		INTEGER		Minutes in delta reference time	*
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
C* M. Li/GSC             5/00   Added MXNMFL and MXFLSZ                 *
C* R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* M. Li/SAIC		03/02	Add 15 minitue to endtime		*
C* R. Tian/SAIC		07/02	Remove the 15 minutes to endtime	*
C* M. Li/SAIC		09/02	Added SFC_MINUTES to endtime		*
C* M. Li/SAIC		09/02	Added check for ival>20 min		*
C* T. Lee/SAIC		08/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		01/04	Added reference time, auto-update flags	*
C* T. Lee/SAIC		04/04	Added delta reference time		*
C* T. Lee/SAIC		09/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* T. Piper/SAIC        1/05    Initialized cval                        *
C* m.gamazaychikov/SAIC 01/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed tmplt string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	alias, cycle, endtim, timstr
C*
	CHARACTER	filnam*(MXFLSZ+81), prmlst(MMPARM)*4, 
     +			path*25, tmplt*(MXTMPL), times(MXNMFL)*20,
     +			sftim(MXNMFL)*20, strtim*20, ttm*20, etime*20,
     +			tms(MXNMFL)*20, basetm*20
	CHARACTER	sfcmin*(20), cval*(20)
	CHARACTER*(MXFLSZ)      files (MXNMFL), fnull
	INTEGER		itarr(5), jtarr(5)
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
        nexp = MXNMFL
	basetm = ' '
C
C*	Check for a valid maximum number of times.
C
	IF  ( ( mrange .le. 0 ) .and. ( isbcat .ne. SCAT_SFF ) )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Compute the start time of the range from the end time
C*	and the number of minutes.
C
	CALL TI_CTOI ( endtim, itarr, ier )
C
C*	Add SFC_MINUTES to endtim
C
	CALL ST_NULL ( 'SFC_MINUTES', sfcmin, lens, ier )
        cval = ' '
	CALL CTB_PFSTR ( sfcmin, cval, ier1 )
	CALL ST_NUMB ( cval, ival, ier2 )
	
	IF ( (ier1 .ne. 0) .or. (ier2 .ne. 0) .or. (ival .lt. 0) ) THEN
	    ival = 0
	END IF
	IF ( ival .gt. 20 ) ival = 20
 
	CALL TI_ADDM ( itarr, ival, itarr, ier )
	CALL TI_ITOC ( itarr, etime, ier )
C
	IF  ( mrange .gt. 0 )  THEN
	    CALL TI_SUBM ( itarr, mrange, jtarr, ier )
	    CALL TI_ITOC ( jtarr, strtim, ier )
	END IF
C
C*	Get the times based on the data subcategory.
C
	iauto  = 0
	IF  ( isbcat .eq. SCAT_SFF )  THEN
C
C*	    Construct a file name given the alias and cycle.
C
	    CALL FL_MFIL ( alias, cycle, filnam, ier )
C
C*	    If no file name is available, return with an error.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*	    Open the SFC data file, get the times and close the file.
C
	    CALL SF_OPNF ( filnam, .false., isffln, iflsrc,
     +			   nparm, prmlst, ier )
	    CALL SF_GTIM ( isffln, LLMXTM, nt, tms, ier )
	    CALL SF_CLOS ( isffln, ier )

C
C*	    Return requested times based on time range and interval.
C
	    idir  = 1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag,
     +			   iauto, basetm, endtim, idelrt, times, 
     +			   ntime, iret )
C
	ELSE IF  ( isbcat .eq. SCAT_SFC )  THEN
C
C*	    Get the directory name and template from the
C*	    data alias table.
C
	    CALL ST_NULL ( alias, fnull, na, ier )
	    path  = ' '
	    tmplt = ' '
	    CALL CTB_DTGET ( fnull, path, tmplt, ic, is, if, ir, ii,
     +			ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
	    CALL ST_RNUL ( path, path, lens, ier )
	    CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
C*	    Scan the directory for matching files.
C
	    iorder = -1
	    CALL FL_SCND ( path, tmplt, iorder, nexp, files, nfile, 
     +                     ier )
C
C*	    If no file name is available, return with an error.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*  Open the files in reverse order and get as many times
C*  as needed from each file.
C
	    done  = .false.
	    nt = 0
	    i = 1
	    DO WHILE  ( ( i .le. nfile ) .and. ( .not. done ) )
		CALL ST_LSTR ( path, lenp, ier )
		filnam = path(1:lenp) // '/' // files(i)
		CALL SF_OPNF ( filnam, .false., isffln, iflsrc,
     +			       nparm, prmlst, ier )
		IF ( ier .eq. 0 ) THEN
		    CALL SF_GTIM ( isffln, LLMXTM, nts, sftim, ier1 )
		    CALL SF_CLOS ( isffln, ier2 )
		    IF ( ier1 .eq. 0 ) THEN
			j = nts
			DO WHILE  ( ( j .gt. 0 ) .and. ( .not. done ) )
			    CALL TI_DIFF ( strtim, sftim(j), mdif, ier1 )
			    CALL TI_DIFF ( sftim(j), etime, ndif, ier2 )
			    IF  ( ( ier1 .eq. 0 ) .and.
     +				  ( ier2 .eq. 0 ) )  THEN
				IF  ( ( mdif .le. 0 ) .and.
     +					( ndif .le. 0 ) )  THEN
				    nt = nt + 1
				    tms ( nt ) = sftim (j)
				ELSE IF  ( mdif .gt. 0 )  THEN
				    done = .true.
				END IF
			    END IF
			    j = j - 1
			END DO
		    END IF
		END IF
		i = i + 1
	    END DO
C
C*  Return requested times based on time range and interval.
C
	    idir   = 0
	    idelrt = -1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag, 
     +			   iauto, basetm, endtim, idelrt, times, 
     +			   ntime, iret )
C
	ELSE IF  ( isbcat .eq. SCAT_SHP )  THEN
C
C*  Get the directory name and template from the
C*  data alias table.
C
	    CALL ST_NULL ( alias, fnull, na, ier )
	    path  = ' '
	    tmplt = ' '
	    CALL CTB_DTGET ( fnull, path, tmplt, ic, is, if, ir, ii,
     +			ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
	    CALL ST_RNUL ( path, path, lens, ier )
	    CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
C*  Scan the directory for matching files.
C
	    iorder = 1
	    CALL FL_SCND ( path, tmplt, iorder, nexp, files, nfile, 
     +                     ier )
C
C*  If no file name is available, return with an error.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*  Convert each file name to a time and check the time
C*  range for whether to include the time or not.
C
	    nt = 0
	    DO  i = 1, nfile
		CALL FL_MDAT ( files(i), tmplt, etime, ttm, ier )
		ttm(10:11) = '00'
		CALL TI_DIFF ( strtim, ttm, mdif, ier1 )
		CALL TI_DIFF ( ttm, etime, ndif, ier2 )
		IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		    IF  ( ( mdif .le. 0 ) .and.
     +			  ( ndif .le. 0 ) )  THEN
			nt = nt + 1
			tms (nt) = ttm
		    END IF
		END IF
	    END DO
C
C*	    Return requested times based on time range and interval.
C
	    idir = 0
	    idelrt = -1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag,
     +			   iauto, basetm, endtim, idelrt, times, 
     +			   ntime, iret )
C
	ELSE IF  ( isbcat .eq. SCAT_FFG )  THEN
C
C*  Get the directory name and template from the
C*  data alias table.
C
	    CALL ST_NULL ( alias, fnull, na, ier )
	    path  = ' '
	    tmplt = ' '
	    CALL CTB_DTGET ( fnull, path, tmplt, ic, is, if, ir, ii,
     +			ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
	    CALL ST_RNUL ( path, path, lens, ier )
	    CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
C*  Scan the directory for matching files.
C
	    iorder = 1
	    CALL FL_SCND ( path, tmplt, iorder, nexp, files, nfile, 
     +                     ier )
C
C*  If no file name is available, return with an error.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*  Convert each file name to a time and check the time
C*  range for whether to include the time or not.
C
	    nt = 0
	    DO  i = 1, nfile
		CALL FL_MDAT ( files(i), tmplt, etime, ttm, ier )
		ttm(8:11) = '1212'
		CALL TI_DIFF ( strtim, ttm, mdif, ier1 )
		CALL TI_DIFF ( ttm, etime, ndif, ier2 )
		IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		    IF  ( ( mdif .le. 0 ) .and.
     +			  ( ndif .le. 0 ) )  THEN
			nt = nt + 1
			tms (nt) = ttm
		    END IF
		END IF
	    END DO
C
C*  Return requested times based on time range and interval.
C
	    idir = 0
	    idelrt = -1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag,
     +			   iauto, basetm, endtim, idelrt, times, 
     +			   ntime, iret )
C
	ELSE
	    iret = -5
	END IF
C
C*  Construct a single string from the array of times.
C
	IF  ( iret .eq. 0 )  THEN
C
C*	    Check for a valid number of times.
C
	    IF  ( ntime .eq. 0 )  THEN
		iret = -3
	      ELSE
C
C*	    	Make sure the times are sorted.
C
		CALL TI_SORT ( ntime, times, times, ier )
C
C*	    	Create the string of times.
C
		CALL ST_LSTC ( times, ntime, ';', timstr, ier )
		CALL ST_LSTR ( timstr, lenstr, ier )
	    END IF
	END IF
C*
	RETURN
	END
