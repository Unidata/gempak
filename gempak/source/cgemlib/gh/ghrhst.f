	SUBROUTINE GH_RHST ( flpart, nstrm, strmid, advno, iret )
C************************************************************************
C* GH_RHST								*
C*									*
C* This subroutine compares the contents of the history file (which     *
C* contains all storm identifiers for the season and the most recently  *
C* processed advisory for each) with the contents of the marine         *
C* advisory file.  If any advisories are found which are not in the     *
C* history file, the storm identifier and advisory number for the new   *
C* storm(s) is returned.                                                *
C*									*
C* GH_RHST ( FLPART, NSTRM, STRMID, ADVNO, IRET )                       *
C*									*
C* Input parameters:							*
C*	FLPART		CHAR*		History file name (w/out year)  *
C*									*
C* Output parameters:							*
C*	NSTRM		INTEGER		Number of new storm advisories  *
C*	STRMID (*)	CHAR*		Storm ids for new advisories    *
C* 	ADVNO (*)	CHAR*		New advisory numbers            *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = error opening history file*
C*                                       -2 = no advisories available   *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/01   					*
C* D. Kidwell/NCEP	10/01	Added input parm flpart                 *
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* D. Kidwell/NCEP	 4/02   Used format for lunhst integer read 	*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* S. Gilbert/NCEP	01/06	Added code to check HCNPUB files as well*
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* S. Gilbert/NCEP	06/06	Account for new name of HCNPUB files    *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strmid (*), advno (*), flpart
C*
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
	CHARACTER	path*25, templ*(MXTMPL)
C*
	CHARACTER	strmhs (500)*13, strmnu*8, strmck (500)*13
	LOGICAL		done
C-----------------------------------------------------------------------
	iret  = 0
	nstrm = 0
C
C*	Scan the HCNADV directory for all of the fcst advisory files.
C
	filnam = 'HCNADV'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
 	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
C
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
        iorder = -1
	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*	Scan the HCNPUB directory for all of the public advisory files.
C
	filnam = 'HCNPUB'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
C
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL - nfile
        iorder = -1
        CALL ST_RPST ( templ, 'X', '*', ipos, templ, ier )
	CALL FL_SCND ( path, templ, iorder, nexp, files(nfile+1), 
     +                 mfile, ier )
C
C*      Rename public advisory files so they are similar to forecast adv files
C
        DO ijk= nfile+1, nfile+mfile
           IF ( files(ijk)(17:17) .EQ. 'a' ) THEN
              files(ijk) = files(ijk)(1:8) // '.pubadv.' // 
     +                     files(ijk)(19:21) // 'a'
           ELSE IF ( files(ijk)(17:17) .EQ. 'b' ) THEN
              files(ijk) = files(ijk)(1:8) // '.pubadv.' // 
     +                     files(ijk)(19:21) // 'b'
           ENDIF
        ENDDO
C
        nfile = nfile + mfile
	IF ( nfile .eq. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*      Sort all HCNADV and HCNPUB file names in reverse order
C
        DO ijk=1, nfile
           files(ijk)(10:12) = 'xxx'
        ENDDO
        CALL ST_SORT ( iorder, nfile, files, nout, files, iret )
C
C*	Open the history file.
C
	CALL ST_LSTR ( flpart, lenf, ier ) 
	filnam = flpart ( :lenf) // '.' // files ( 1 ) ( 5:8 )
	CALL FL_SOPN ( filnam, lunhst, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Read the history file.
C
	READ (lunhst,200,IOSTAT=iostat) numhst
200	FORMAT ( I4 )
	IF ( iostat .ne. 0 ) numhst = 0
	IF ( numhst .gt. 0 ) THEN
	    READ (lunhst,100) ( strmhs (ii), ii = 1, numhst )
100         FORMAT (A)
	  ELSE
C
C*	    There are no entries in the history file.  Process all 
C*	    files.
C
	    nstrm = nfile
	    DO ii = 1, nfile
		strmid ( ii ) = files (ii) ( :8 )
		advno  ( ii ) = files (ii) ( 17:20 )
	    END DO
	END IF
	CALL FL_CLOS ( lunhst, ier )
	IF ( nstrm .gt. 0 ) THEN
	    RETURN
	END IF
C
C*	Find the latest advisory for each storm id in the advisory
C*	directory.  The advisories are sorted latest to earliest for a
C*	given storm id.
C
	strmnu = ' '
	kcheck = 0
	DO ii = 1, nfile
	    IF ( files (ii) ( :8 ) .ne. strmnu ) THEN
		strmnu = files (ii) ( :8)
		kcheck = kcheck + 1
		strmck ( kcheck ) = strmnu // '_' // files (ii) (17:20)
	    END IF
	END DO    

C
C*	Compare the latest advisory for each storm id to the history
C*	file.
C
	DO kk = 1, kcheck
	    done = .false.
	    ii   = 1
	    DO WHILE ( .not. done )
		IF ( strmck ( kk ) .eq. strmhs ( ii ) ) THEN
C
C*		    This advisory for this storm id was found in the
C*		    history file, and does not need to be processed.
C
		    done = .true.
		  ELSE
		    ii = ii + 1
		    IF ( ii .gt. numhst ) THEN
C
C*			This advisory for this storm id was not found in
C*			the history file.  Add it to the list to be 
C*			processed.
C
		        nstrm = nstrm + 1
			strmid ( nstrm ) = strmck ( kk ) ( :8 )
			advno  ( nstrm ) = strmck ( kk ) ( 10:13 )
			done  = .true.
		    END IF
		END IF
	    END DO
	END DO
C*
	RETURN
	END
