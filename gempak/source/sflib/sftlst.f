	SUBROUTINE SF_TLST ( sffile, dattim, sffcur, datcur, newfil,
     +			     timlst, nts, iret )
C************************************************************************
C* SF_TLST								*
C*									*
C* This routine returns a list of times given SFFILE and DATTIM		*
C* information.								* 
C*									*
C* SF_TLST ( SFFILE, DATTIM, SFFCUR, DATCUR, NEWFIL, TIMLST, NTS, IRET )*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Surface file			*
C*	DATTIM		CHAR*		Data time			*
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current surface file		*
C*	DATCUR		CHAR*		Current time			*
C*	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Output parameters:							*
C*	TIMLST(NTS)	CHAR*		Array of times			*
C*	NTS		INTEGER		Number of times returned	*
C*	IRET		INTEGER		Return code			*
C*					  -3 = file is not open		*
C*					 -20 = invalid time		*
C*					 -25 = no files found		*
C**									*
C* Log:									*
C* T. Lee/GSC		 5/01	Initial coding				*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* T. Lee/SAIC		 6/03	Fixed single file plotting		*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Fixed return code			*
C* A. Hardy/NCEP	11/04   Added calls ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, dattim, sffcur, datcur, timlst(*)
C*
	CHARACTER	filnam*80, prmlst(MMPARM)*4, path*80,
     +			lpath*80, rpath*80, tmplt*48,
     +			sftim(MXNMFL)*20, strtim*20, ttm*20,
     +			fname*80, cycle*20, farr(2)*80, endtim*20
	CHARACTER	files (MXNMFL)*(MXFLSZ)
	LOGICAL		tpl, newfil, done
C------------------------------------------------------------------------
	iret = 0
        nexp = MXNMFL
C
C*	Check to see if file requested is current file.
C
	IF  ( ( sffile .eq. sffcur ) .and. ( sffile .ne. ' ' ) .and.
     +	      (	dattim .eq. datcur ) .and. ( dattim .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	  ELSE
	    sffcur = ' '
	    datcur = ' '
	END IF
C
C*	Get time range from DATTIM. 
C
	CALL TI_RANG ( dattim, strtim, endtim, mrange, iret )
C
C*	Parse file name and cycle.
C
	CALL ST_CLST ( sffile, '|', ' ', 2, farr, nfarr, iret )
	fname = farr (1)
	IF  ( nfarr .eq. 2 )  THEN
	    cycle = farr (2)
	  ELSE
	    cycle = ' '
	END IF
C
C*	Get the directory name and template from the data 
C*	alias table.
C
	tpl   = .true.
	rpath = ' '
	tmplt = ' '
	CALL ST_NULL ( fname, fname, nf, ier )
	CALL CTB_DTGET ( fname, rpath, tmplt, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	IF ( ier .ne. 0 )  tpl = .false. 
C
	CALL ST_RNUL ( rpath, rpath, lens, ier )
	CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
	IF  ( .not. tpl )  THEN
	    CALL FL_PATH ( fname, rpath, tmplt, iret )
	    CALL ST_LCUC ( fname, fname, ier )
	    IF ( cycle .ne. ' ' )  THEN
	        is = SCAT_SFF
	      ELSE
		is = SCAT_SFC
	    END IF
	END IF
C
C*	Check if the input file uses template for file name, e.g.,
C*	$OBS/hrly/YYYYMMDD.hrly. If so, set tpl to be .TRUE.
C
	IF  (  mrange .gt. 0 )  THEN
	    CALL FL_MNAM ( strtim, fname, filnam, iret )
	    IF  ( fname .ne. filnam ) tpl = .true.
	END IF  
C
C*	If the file name is not a template or the range equals zero, 
C*	get data time from one single file. 
C
	IF  ( ( .not. tpl ) .or. ( mrange .eq. 0 ) )  THEN
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'FL', iret, filnam, ier )
		iret = -25
		RETURN
	    END IF
C
C*	    Open new file.
C
	    CALL SF_OPNF ( filnam, .false., isffln, isrc, nparm,
     +			   prmlst, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -3
		RETURN
	    END IF
C
C*	    Set current file and data time. Initialize PC package.
C
	    sffcur = sffile
	    datcur = dattim	
	    newfil = .true.
	    CALL PC_INIT ( 0, nparm, prmlst, ier )
C
C*	    Get the times and close the file.
C
	    CALL SF_GTIM ( isffln, LLMXTM, nts, timlst, ier )
	    CALL SF_CLOS ( isffln, ier )
	  ELSE
C
C*	    Get the times based on the data subcategory.
C
	    IF  ( is .eq. SCAT_SFF )  THEN
C
C*		Construct a file name given the alias and cycle.
C
		CALL FL_MFIL ( fname, cycle, filnam, iret )
C
C*		If no file name is available, return with an error.
C
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'FL', iret, filnam, ier )
		    iret = -25
		    RETURN
		END IF
C
C*		Open the SFC data file, get the times and close the file.
C
		CALL SF_OPNF  ( filnam, .false., isffln, iflsrc,
     +				nparm, prmlst, ier )
		CALL SF_GTIM ( isffln, LLMXTM, nts, timlst, ier )
		CALL SF_CLOS ( isffln, ier )
C
	      ELSE IF  ( is .eq. SCAT_SFC )  THEN
C
C*		Scan the directory for matching files. If an alias is
C*		used, start to scan from the local directory and then
C*		remotely.
C
		lpath = '.'
		iorder = -1
		IF  ( tpl )  THEN
		    CALL FL_SCND ( lpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    IF  ( nfile .eq. 0 )  THEN
		        CALL FL_SCND  ( rpath, tmplt, iorder, nexp, 
     +					files, nfile, ier )
			path = rpath
		      ELSE
		        path = lpath
		    END IF
		  ELSE IF  ( rpath .eq. ' ' )  THEN
		    CALL FL_SCND ( lpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    path = lpath
	          ELSE 
		    CALL FL_SCND ( rpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    path = rpath
		END IF
C
C*		If no file name is available, return with an error.
C
		IF  ( ier .ne. 0 )  THEN
		    iret = -25
		    RETURN
		END IF
C
C*		Open the files in reverse order and get as many times
C*		as needed from each file.
C
		done = .false.
		nts  = 0
		i    = 1
		DO WHILE  ( ( i .le. nfile ) .and. ( .not. done ) )
		    CALL ST_LSTR ( path, lenp, ier )
		    filnam = path(1:lenp) // '/' // files(i)
		    CALL SF_OPNF ( filnam, .false., isffln, iflsrc,
     +				   nparm, prmlst, iret )
		    IF  ( iret .ne. 0 )  THEN
			iret = -3
			RETURN
		    END IF
		    CALL SF_GTIM ( isffln, LLMXTM, nt, sftim, ier )
		    CALL SF_CLOS ( isffln, ier )
C
		    j = nt
		    DO WHILE  ( ( j .gt. 0 ) .and. ( .not. done ) )
			CALL TI_DIFF ( strtim, sftim(j), mdif, ier1 )
			CALL TI_DIFF ( sftim(j), endtim, ndif, ier2 )
			IF  ( ( ier1 .eq. 0 ) .and.
     +			      ( ier2 .eq. 0 ) )  THEN
			    IF  ( ( mdif .le. 0 ) .and.
     +				  ( ndif .le. 0 ) )  THEN
				nts = nts + 1
				timlst(nts) = sftim(j)
				IF  ( nts .ge. LLMXTM ) done = .true.
			      ELSE IF  ( mdif .gt. 0 )  THEN
				done = .true.
			    END IF
			END IF
			j = j - 1
		    END DO
		    i = i + 1
		END DO
C
	      ELSE IF  ( is .eq. SCAT_SHP )  THEN
C
C*		Scan the directory for matching files. If an alias is
C*		used, start to scan from the local directory and then 
C*		remotely.
C
		lpath = '.'
		iorder = 1
		IF  ( tpl )  THEN
		    CALL FL_SCND ( lpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    IF  ( nfile .eq. 0 )  THEN
			CALL FL_SCND  ( rpath, tmplt, iorder, nexp, 
     +					files, nfile, ier )
			path = rpath
		      ELSE
			path = lpath
		    END IF
		  ELSE IF  ( rpath .eq. ' ' )  THEN
		    CALL FL_SCND ( lpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    path = lpath
		  ELSE 
		    CALL FL_SCND ( rpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    path = rpath
		END IF
C
C*		If no file name is available, return with an error.
C
		IF  ( ier .ne. 0 )  THEN
		    iret = -25
		    RETURN
		END IF
C
C*		Convert each file name to a time and check the time
C*		range for whether to include the time or not.
C
		done = .false.
		nts  = 0
		i    = 1
		DO WHILE  ( ( i .le. nfile ) .and. ( .not. done ) )
		    CALL FL_MDAT ( files(i), tmplt, endtim, ttm, ier )
		    ttm(10:11) = '00'
		    CALL TI_DIFF ( strtim, ttm, mdif, ier1 )
		    CALL TI_DIFF ( ttm, endtim, ndif, ier2 )
		    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
			IF   (  ( mdif .le. 0 ) .and.
     +				( ndif .le. 0 ) )  THEN
			    nts = nts + 1
			    timlst (nts) = ttm
			    IF  ( nts .ge. LLMXTM )  done = .true. 
			END IF
		    END IF
		    i = i + 1
		END DO
C
	      ELSE IF  ( is .eq. SCAT_FFG )  THEN
C
C*		Scan the directory for matching files.
C
		lpath = '.'
		iorder = 1
		CALL FL_SCND ( lpath, tmplt, iorder, nexp, files, 
     +                         nfile, ier )
		IF  ( nfile .eq. 0 )  THEN
		    CALL FL_SCND ( rpath, tmplt, iorder, nexp, files, 
     +				   nfile, ier )
		    path = rpath
		  ELSE
		    path = lpath
		END IF
C
C*		If no file name is available, return with an error.
C
		IF  ( ier .ne. 0 )  THEN
		    iret = -25
		    RETURN
		END IF
C
C*		Convert each file name to a time and check the time
C*		range for whether to include the time or not.
C
		done = .false.
		nts  = 0
		i    = 1
		DO WHILE  ( ( i .le. nfile ) .and. ( .not. done ) )
		    CALL FL_MDAT ( files(i), tmplt, endtim, ttm, ier )
		    ttm(8:11) = '1212'
		    CALL TI_DIFF ( strtim, ttm, mdif, ier1 )
		    CALL TI_DIFF ( ttm, endtim, ndif, ier2 )
		    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
			IF   (  ( mdif .le. 0 ) .and.
     +				( ndif .le. 0 ) )  THEN
			    nts = nts + 1
			    timlst (nts) = ttm
			    IF  ( nts .ge. LLMXTM )  done = .true.
			END IF
		    END IF
		    i = i + 1
		END DO
C
	      ELSE
		iret = -25
	    END IF
C
	END IF
C
C*	Sort the times.
C
	IF  ( iret .eq. 0 )  THEN
C
C*	    Check for a valid number of times.
C
	    IF  ( nts .eq. 0 )  THEN
		iret = -25
	      ELSE
C
C*	    	Make sure the times are sorted.
C
		CALL TI_SORT ( nts, timlst, timlst, ier )
C
		CALL ST_LSTR ( timlst (1), mstr, ier )
		CALL ST_LSTR ( timlst (nts), nstr, ier )
		sffcur = sffile
		datcur = dattim
		newfil = .true.
	    END IF
	END IF
C*
	RETURN
	END
