	SUBROUTINE FL_MFIL  ( filtyp, dattim, filnam, iret )
C************************************************************************
C* FL_MFIL								*
C* 									*
C* This subroutine finds the most recent file given a data type and	*
C* a date/time.  The data type could have a base time included after a	*
C* bar (|).  If the base time is included, it is used as a limit for	*
C* searching the filenames, and any files with a date after the base	*
C* time are not included in the search.  The current directory is 	*
C* searched first for any valid files.  Then the path associated with	*
C* the data type is searched if no files are found locally.		*
C* 									*
C* FL_MFIL  ( FILTYP, DATTIM, FILNAM, IRET )				*
C* 									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File name type			*
C*	DATTIM		CHAR*		Full Date/time			*
C*									*
C* Output parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return		*
C*					  -11 = no file for time range	*
C*					  -13 = no file for given type	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/98	Copied from FL_MFIL			                *
C* S. Jacobs/NCEP	 9/98	Increased the num of file names 100->200    *
C* S. Jacobs/NCEP	 9/98	Increased the size of the path variable	    *
C* T. Lee/GSC		 4/99	Renamed from FL_FFIL			            *
C* T. Lee/GSC		 7/99	Changed files to 300; Checked fcst hrs	    *
C* S. Jacobs/NCEP	 8/99	Changed call to FL_SCND			            *
C* S. Jacobs/NCEP	 8/99	Changed call to FL_TMPL			            *
C* S. Jacobs/NCEP	 9/99	Changed call to FL_MDAT			            *
C* S. Jacobs/NCEP	11/99	Removed check for previous day data	        *
C* S. Jacobs/NCEP	12/99	Added check for same forecast base times    *
C* D.W.Plummer/NCEP	12/99	Fix for filenames w/o century (Y2K bug)	    *
C* S. Jacobs/NCEP	 1/00	Set minmin to minimum value		            *
C* M. Li/GSC		 5/00	Added MXFLSZ and MXNMFL			            *
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	        *
C* S. Jacobs/NCEP	 7/01	Added check for minutes in the template	    *
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			        *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		        *
C* T. Lee/SAIC		10/04	Fixed return code			                *
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			            *
C* S. Jacobs/NCEP	12/04	Increased size of fnull to 256		        *
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed tmplt string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS      	*
C* F. J. Yen/NCEP   	 4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* S. Jacobs/NCEP	 6/13	Added check for AWIPSDB aliases		        *
C* S. Gilbert/NCEP   9/15   Added cycle to filename for AWIPSDB aliases *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, filnam, dattim
C*
	CHARACTER       path*25, lpath*25, rpath*25, tmplt*(MXTMPL),
     +			farr(2)*160, ndttm*20, bdttm*20, fdttm*20,
     +			fnull*256
	CHARACTER*(MXFLSZ)	files(MXNMFL)
	LOGICAL		found, ftmplt
	INTEGER		itime
C------------------------------------------------------------------------
	iret = 0
C
C*      Check filename length.
C
	CALL ST_LSTR ( filtyp, lf, ier )
	IF ( lf .eq. 0 )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Parse alias name and cycle time.
C
	CALL ST_CLST  ( filtyp, '|', ' ', 2, farr, nfarr, ier )
C
C*	Get template information.
C
	found = .true.
	CALL ST_NULL ( farr(1), fnull, nf, ier )
	rpath = ' '
	tmplt = ' '
	CALL CTB_DTGET ( fnull, rpath, tmplt, ic, is, if, ir, ii, ion, 
     +			 ihb, mnb, iha, mna, mstrct, idtmch, ier )
	IF ( ier .ne. 0 )  found = .false. 
	CALL ST_RNUL ( rpath, rpath, lenr, ier )
	CALL ST_RNUL ( tmplt, tmplt, lens, ier )
C
	IF  ( .not. found )  THEN
C
C*	    If a match was not found, set input file type to filnam.
C
	    filnam = filtyp
C
	ELSE
C
C*	    If the alias refers to the AWIPS Database, set the file name
C*	    and return.
C
	    IF  ( rpath .eq. 'AWIPSDB' )  THEN
		  filnam = rpath(:lenr) // '/' // tmplt
		  IF ( nfarr == 2 ) THEN
		    filnam = filnam // '/' // farr(2)
          END IF
		  RETURN
	    END IF
C
C*	    If dattim is blank, get the system time.
C
	    IF  ( dattim .eq. ' ' )  THEN
		  itime = 1
		  CALL CSS_GTIM ( itime, ndttm, ier )
	    ELSE
C
C*		Otherwise, set the input dattim to "now".
C
		  CALL ST_LCUC ( dattim, ndttm, ier )
C
	    END IF
C
C*	    If the user added a cycle time to the file alias, create
C*	    a full GEMPAK time from the input and the "now" time.
C
	    IF  ( nfarr .eq. 2 )  THEN
		  CALL TI_STAN ( farr(2), ndttm, bdttm, ier )
		  IF  ( INDEX ( tmplt, 'NN' ) .eq. 0 )  THEN
		    bdttm(10:11) = '00'
	      END IF
	    ELSE
		  bdttm = ndttm
	    END IF
C
C*	    Get all of the file names in the directory.
C*	    Search in the current directory first.  If there are
C*	    no files locally, then search in the remote path.
C
	    lpath = '.'
	    iord  = -1
        nexp  = MXNMFL
	    CALL FL_SCND ( lpath, tmplt, iord, nexp, files, nfile,
     +                     ier )
	    IF  ( nfile .eq. 0 )  THEN
		  CALL FL_SCND ( rpath, tmplt, iord, nexp, files, nfile, 
     +                         ier )
		  path = rpath
	    ELSE
		  path = lpath
	    END IF
	    CALL ST_LSTR ( path, lenp, ier )
C
C*	    Convert each file name to a date/time and compare to the
C*	    the computed base time from above.  If the base time is
C*	    equal to or greater than file name time, then choose
C*	    that file.
C
	    i = 1
	    minmin = IMISSD
	    DO  i = 1, nfile
		CALL FL_MDAT ( files(i), tmplt, bdttm, fdttm, ier )
		ftmplt = INDEX ( tmplt, 'fFF' ) .ne. 0
		IF  ( ftmplt )  THEN
		    IF  ( bdttm(1:11) .eq. fdttm(1:11) )  THEN
			IF  ( INDEX ( bdttm, 'F' ) .ne. 0 )  THEN
			    CALL TG_DIFF ( bdttm, fdttm, nmin, ier )
			ELSE
			    iret = -11
			    RETURN
			END IF
		    ELSE
			nmin = -1
		    END IF
		ELSE
		    CALL TI_DIFF ( bdttm, fdttm, nmin, ier )
		END IF
C
		IF  ( nmin .ge. 0 )  THEN
		    IF ( minmin .eq. IMISSD )  minmin = nmin
		    IF  ( nmin .le. minmin )  THEN
		        filnam = path(:lenp) // '/' // files(i)
			minmin = nmin
		    END IF
		END IF
C
	    END DO
C
C*	    If a file was not found return with an error.
C
	    IF  ( minmin .eq. IMISSD )  THEN
		  filnam = ' '
		  iret   = -13
	    END IF
C
	END IF
C*
	RETURN
	END
