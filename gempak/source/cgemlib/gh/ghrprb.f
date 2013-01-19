	SUBROUTINE GH_RPRB ( strmid, advno, imax, jmax, grid, iret )
C************************************************************************
C* GH_RPRB 								*
C*									*
C* This subroutine reads the strike probablilty grid array for the TPC. *
C*									*
C* GH_RPRB ( STRMID, ADVNO, IMAX, JMAX, GRID, IRET )			*
C*									*
C* Input parameters:							*
C*      STRMID		CHAR*		Storm identification		*
C*      ADVNO		CHAR*		Advisory number			*
C*      IMAX		INTEGER		Maximum number of rows		*
C*      JMAX		INTEGER		Maximum number of columns	*
C*									*
C* Output parameters:							*
C*	GRID(IMAX,JMAX) REAL		Grid data array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = cannot get prob. grid     *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/01 						*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strmid, advno
        REAL		grid (imax,jmax)
C*
	CHARACTER*(MXFLSZ)    	filnam
	CHARACTER	path*25, templ*(MXTMPL), tfile*128, 
     +			advnum*3, storm*8
C-----------------------------------------------------------------------
	iret = 0
C
C*      Scan the directory for all of the advisory files.
C
	filnam = 'HCNPRB'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, len, ier )
	CALL ST_RNUL ( templ, templ, len, ier )
        CALL ST_LSTR ( path, lenp, ier )
C
C*	Construct the probability table file name for this storm and 
C*	advisory.
C
	CALL ST_LSTR ( strmid, lens, ier )
	CALL ST_LSTR ( advno, lena, ier )
     	IF ( ( lens .ne. 8 ) .or. 
     +	     ( lena .lt. 1 ) .or. ( lena .gt. 3 ) ) THEN
	    iret = -1
	    RETURN
	END IF
	advnum = advno
	IF ( lena .eq. 1 ) advnum = '00' // advno ( :lena ) 
	IF ( lena .eq. 2 ) advnum =  '0' // advno ( :lena )
     	CALL ST_UCLC ( strmid ( :8 ), storm, ier )
C
     	filnam = storm // '.prblty_tbl.' // advnum
	CALL ST_LSTR ( filnam, lenf, ier )
	tfile = path ( :lenp ) // '/' // filnam
C
C*	Read the strike probability array.
C
	CALL FL_SOPN ( tfile, lun, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Read the data.
C
      	DO j = 1, jmax
	    n = imax
      	    DO k = 1, 6
      	        m = n - 15
      	        READ ( lun, 100, IOSTAT = iostat )
     +		     ( grid ( i, j ), i = n, m, -1 )
 100  	        FORMAT ( 16F5.1 )
		IF ( iostat .ne. 0 ) THEN
		    iret = -1
		    CALL FL_CLOS ( lun, ier )
		    RETURN
		END IF
	        n = n - 16
      	    END DO
        END DO
C
C*	Grid is now oriented such that (1,1) is at 1N, 110W
C*	and (96,60) is at 60N, 15W. 
C
	CALL FL_CLOS ( lun, ier )
C*
	RETURN
	END
