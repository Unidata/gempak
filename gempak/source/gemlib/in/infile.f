	SUBROUTINE IN_FILE ( infil, outfil, iret )
C************************************************************************
C* IN_FILE								*
C*									*
C* This routine parses the user input file name string (up to 3 files).	*
C* It determines if the input file name is an alias, a template, 	*
C* or a regular file name. The aliases and templates will be expanded 	*
C* to file names, then all resulting file names will be put back 	*
C* into a single string	for use in the file opening functions.		*
C*									*
C* IN_FILE ( INFIL, OUTFIL, IRET )            				*
C*									*
C* Input parameters:							*
C*	INFIL 		CHAR*		Input file name string		*
C*									*
C* Output parameters:							*
C*      OUTFIL          CHAR*           Output file name string         *
C*	IRET		INTEGER		Return code			*
C*					+3 = Invalid time found		*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		02/02						*
C* S. Jacobs/NCEP	 2/02	Fixed problem with time array		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Fixed return code			*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	infil, outfil
C*
	CHARACTER*(MXFLSZ)  file (3), file2 (2), templt, tempfil(3),
     +			    filnam, filelst(MXNMFL), basnam
	CHARACTER	sysdt*12, path*48, cyctim*20, ttim*20,
     +			dattim(MXNMFL)*20
	LOGICAL		istp	
	INTEGER		itype
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Separate the input file name string
C
	CALL ST_CLST ( infil, '+', ' ', 3, file, numf, iret )
C*
	itype = 1
	CALL CSS_GTIM(itype, sysdt, ier)
	DO ii = 1, numf
	   CALL ST_CLST ( file(ii), '|', ' ', 2, file2, num2, iret )
C
C*	   Check the date/time, if the date/time < 11, set it to blank.
C*	   If the date/time > 11, use the first 11 chars
C
	   CALL ST_LSTR  ( file2(2), len, ier )
C*
	   IF (len .lt. 11) THEN
	      iret = +3 
	      cyctim = ' '
	   ELSE
	      cyctim = file2(2)(1:11)
	   END IF
C
C*	   Check for the template
C
	   istp   = .true.
	   path   = ' '
	   templt = ' '
	   CALL ST_NULL ( file2(1), file2(1), nf, ier )
	   CALL CTB_DTGET ( file2(1), path, templt, ic, is, if, ir, ii,
     +		    ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
	   IF ( ier .ne. 0 )  istp = .false. 
C
	   CALL ST_RNUL ( path, path, lens ier )
	   CALL ST_RNUL ( templt, templt, lens ier )
C
C*	   If not alias found, split the path and file name
C
           IF  ( .not. istp )  THEN
	      CALL FL_PATH ( file2(1), path, basnam, ier )	       
C
C*	      If template use it, otherwise go to next loop
C
	      CALL ST_ISTP( basnam, istp, ier)

	      IF ( istp ) templt = basnam
	   END IF
C*	  

	   IF ( istp ) THEN
C
C*            IF full base time is blank, set the system time
C
	      CALL ST_LSTR  ( cyctim, len, ier )
	      IF ( len .eq. 0 ) THEN
 	         cyctim = sysdt
	      END IF
C
C*	      Use the template to get a list of files
C
	      CALL FL_SCND ( path, templt, 1, MXNMFL, 
     +			     filelst, nfile, ier )
C	
C*	      Convert file names to times 
C
	      DO jj = 1, nfile
	        CALL FL_MDAT ( filelst(jj), templt, sysdt, ttim, ier )
		dattim(jj) = ttim(:11)
	      END DO
C
C*	      Sort the times
C
	      CALL TI_SORT(nfile, dattim, dattim, ier)
C
C*	      Find the closest time to full base time from list
C
	      CALL TI_MTCH (4, cyctim, dattim, nfile, 0, ipos, ier)
C
C*	      Use template and found time to make a file name
C
	      CALL FL_MNAM (dattim(ipos), templt, filnam, ier)
C
C*	      Add the path and file name
C
	      CALL ST_LSTR (path, length, ier)

	      IF (length .gt. 0) THEN
	         tempfil(ii) = path(:length) // '/' // filnam 
	      ELSE
		 tempfil(ii) = filnam
	      END IF
	   ELSE
	      tempfil(ii) = file(ii)
	   END IF
C*
 	END DO 
C
C*	Make the output string
C
	CALL ST_LSTC ( tempfil, numf, '+', outfil, iret)	
C*
	RETURN
	END
