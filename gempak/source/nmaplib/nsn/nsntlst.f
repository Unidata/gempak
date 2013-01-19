	SUBROUTINE NSN_TLST ( alias, cycle, isbcat, endtim, mrange,
     +			      intrvl, iflag, idelrt, timstr, lenstr, 
     +			      ntime, iret )
C************************************************************************
C* NSN_TLST								*
C*									*
C* This routine returns a list of times given SND information. The 	*
C* alias is used to locate the image data files on disk. The times are	*
C* returned in a single string separated by a semi-colon (;).		*
C*									*
C* NSN_TLST ( ALIAS, CYCLE, ISBCAT, ENDTIM, MRANGE, INTRVL, IFLAG	*
C* 		IDELRT, TIMSTR, LENSTR,	NTIME, IRET )			*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for SND data		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C*	IFLAG		INTEGER		Reference time index		*
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
C* M. Li/SAIC		 3/02	Add 30 minutes to endtime		*
C* R. Tian/SAIC		07/02	Remove the 30 minutes to endtime	*
C* T. Lee/SAIC		08/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		01/04	Added reference time, auto-update flags	*
C* T. Lee/SAIC		04/04	Added delta reference time		*
C* T. Lee/SAIC		09/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
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
     +			sntim(MXNMFL)*20, strtim*20, etime*20,
     +			tms(MXNMFL)*20, basetm*20
	CHARACTER*(MXFLSZ)      files (MXNMFL), fnull
	INTEGER		itarr(5), jtarr(5)
	LOGICAL		done, mrgdat
C------------------------------------------------------------------------
	iret = 0
	basetm = ' '
C
C*	Check for a valid maximum number of times.
C
	IF  ( ( mrange .le. 0 ) .and. ( isbcat .ne. SCAT_SNF ) )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Compute the start time of the range from the end time
C*	and the number of minutes.
C
	CALL TI_CTOI ( endtim, itarr, ier )
C
C*	Add 0 minutes to endtim
C
	CALL TI_ADDM (itarr, 0, itarr, ier )
	CALL TI_ITOC ( itarr, etime, ier )
C
	IF ( mrange .gt. 0 )  THEN
	    CALL TI_SUBM ( itarr, mrange, jtarr, ier )
	    CALL TI_ITOC ( jtarr, strtim, ier )
	END IF
C
C*	Get the times based on the data subcategory.
C
	iauto = 0
	IF  ( isbcat .eq. SCAT_SNF )  THEN
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
C*	    Open the SND data file, get the times and close the file.
C
	    CALL SN_OPNF ( filnam, .false., isnfln, iflsrc,
     +			   nparm, prmlst, ivert, mrgdat, ier )
	    CALL SN_GTIM ( isnfln, LLMXTM, nt, tms, ier )
	    CALL SN_CLOS ( isnfln, ier )
C
C*	    Return requested times based on time range and interval.
C
	    idir   = 1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag, iauto,
     +			   basetm, endtim, idelrt, times, ntime, iret )
C
	  ELSE IF  ( isbcat .eq. SCAT_SND )  THEN
C
C*	    Get the directory name and template from the
C*	    data alias table.
C
	    CALL ST_NULL ( alias, fnull, na, ier )
	    path  = ' '
	    tmplt = ' '
	    CALL CTB_DTGET ( fnull, path, tmplt, ic, is, if, ir, ii,
     +          	ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
	    CALL ST_RNUL ( path, path, lens, ier )
	    CALL ST_RNUL ( tmplt, tmplt, len, ier )

C
C*	    Scan the directory for matching files.
C
            nexp   = MXNMFL
	    iorder = -1
	    CALL FL_SCND (path, tmplt, iorder, nexp, files, nfile, ier)
C
C*	    If no file name is available, return with an error.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*	    Open the files in reverse order and get as many times
C*	    as needed from each file.
C
	    done  = .false.
	    nt = 0
	    i = 1
	    DO WHILE  ( ( i .le. nfile ) .and. ( .not. done ) )
		CALL ST_LSTR ( path, lenp, ier )
		filnam = path(1:lenp) // '/' // files(i)
		CALL SN_OPNF ( filnam, .false., isnfln, iflsrc,
     +			       nparm, prmlst, ivert, mrgdat, ier )
		IF ( ier .eq. 0 ) THEN
		    CALL SN_GTIM ( isnfln, LLMXTM, nts, sntim, ier1 )
		    CALL SN_CLOS ( isnfln, ier2 )
		    IF ( ier1 .eq. 0 ) THEN
			j = nts
			DO WHILE  ( ( j .gt. 0 ) .and. ( .not. done ) )
			    CALL TI_DIFF (strtim, sntim(j), mdif, ier1)
			    CALL TI_DIFF (sntim(j), etime, ndif, ier2 )
			    IF  ( ( ier1 .eq. 0 ) .and.
     +				  ( ier2 .eq. 0 ) )  THEN
				IF  ( ( mdif .le. 0 ) .and.
     +					( ndif .le. 0 ) )  THEN
				    nt = nt + 1
				    tms (nt) = sntim (j)
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
C*          Return requested times based on time range and interval.
C
	    idir   = 0
	    idelrt = -1
	    CALL TI_TMLN ( tms, nt, mrange, intrvl, idir, iflag, iauto, 
     +			   basetm, endtim, idelrt, times, ntime, iret )
C
	  ELSE
	    iret = -5
	END IF
C
C*	Construct a single string from the array of times.
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
