	SUBROUTINE GG_NATC ( dattim, icolor, mrktyp, ssize, iwidth,
     +			     lwidth, iflags, value, ifcsth, strnam, iret )
C************************************************************************
C* GG_NATC								*
C*									*
C* This subroutine determines the files to be checked to plot the ATCF  *
C* or ensemble track(s).						*
C*									*
C* GG_NATC ( DATTIM, ICOLOR, MRKTYP, SSIZE, IWIDTH, LWIDTH, IFLAGS,     *
C*	     VALUE, IFCSTH, STRNAM, IRET )                              *
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Initial time for ATCF track     *
C*	ICOLOR(NA)	INTEGER		Line and marker colors		*
C*	MRKTYP(NA)	INTEGER		Marker symbol numbers           *
C*	SSIZE (NA)	REAL		Marker sizes			*
C*	IWIDTH(NA)	INTEGER		Marker line widths		*
C*	LWIDTH(NA)	INTEGER		Line widths			*
C*	IFLAGS (4)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*	VALUE (3)	REAL		Wind levels to color code tracks*
C*	IFCSTH		INTEGER		Single fcst hour for ENS_CYC	*
C*	STRNAM		CHAR*		Tropical storm name or ENS_CYC	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/00	                                        *
C* J. Wu/GSC             7/00   Added checks for TI_STAN return status	*
C*				ier and removed the test for LAST	*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* F. J. Yen/NCEP	 4/01	Updated for new format and renamed	*
C*				temporarily from GG_ACTF to GG_NATC.	*
C* A. Hardy/SAIC         2/02   Changed call FL_SCND			*
C* m.gamazaychikov/SAIC	01/04	Changed call to GG_ALIS			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* T. Lee/SAIC		10/04	Increased FILNAM size to append NULL	*
C* A. Hardy/NCEP	11/04	Added calls ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* m.gamazaychikov/SAIC 04/05   Added code to handle ens cyclone data	*
C* D. Kidwell/NCEP	 5/05	Fixed for time matching for ENCY        *
C* S. Jacobs/NCEP	 5/05	Add call to ST_RNUL for filnam		*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 02/06   Add value to CS, changed CS of GG_ENCY	*
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* m.gamazaychikov/SAIC 07/06   Increased NA to 25			*
C* H. Zeng/SAIC		06/07	Added IFCSTH				*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER 	( NA = 25 )
C*
	CHARACTER*(*)	dattim, strnam
	INTEGER		icolor(*), mrktyp(*), iwidth(*), lwidth(*),
     +		        iflags(*)
	REAL		ssize(*), value(*)
C*
	CHARACTER*(MXFLSZ)	files (MXNMFL), filnam
	CHARACTER	templ*(MXTMPL), tfile*128, models(NA)*20,
     +           	cdttm*20, dattm2*20, yy*2, yyyy*4, trkfil*13,
     +			buffer*80, carr (2)*8, stnm2*2,
     +			fdt(MXNMFL)*20, path*25
	INTEGER		idum (NA), ldum (NA), itype, ifcsth
	LOGICAL		done, match
C-----------------------------------------------------------------------
	iret = 0
C 
C*	Save the current attributes.
C
	CALL GQLINE ( jtyp, jlhw, jwid, jwhw, ier )
	CALL GQCOLR ( jcolr, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, jjust,
     +		      ier )
C
C*	Set attributes.
C
	DO ii = 1, NA
	    IF ( ( mrktyp ( ii ) .le.  0 ) .or.
     +		 ( mrktyp ( ii ) .gt. 22 ) ) mrktyp ( ii ) = 1
	    IF ( ( ssize ( ii ) .le.  0.0 ) .or.
     +		 ( ssize ( ii ) .gt. 10.0 ) ) ssize ( ii ) = 1.0
	    IF ( ( iwidth ( ii ) .le.  0 ) .or.
     +		 ( iwidth ( ii ) .gt. 10 ) ) iwidth ( ii ) = 1
	    IF ( ( lwidth ( ii ) .le.  0 ) .or.
     +		 ( lwidth ( ii ) .gt. 10 ) ) lwidth ( ii ) = 3
	    models ( ii ) = ' '
	END DO
C
C*	Get the valid model names.
C
	CALL FL_TBOP ( 'miscset.tbl', 'config', lun, ier )
	filnam = 'ATCF'  
	IF ( strnam .eq. 'ENS_CYC') filnam = 'ENS_CYC'  
2	FORMAT ( A )
	IF ( ier .eq. 0 ) THEN
	    iostat = 0
	    done   = .false.
	    DO WHILE ( ( iostat .eq. 0 ) .and. ( .not. done ) ) 
		READ ( lun, 2, IOSTAT = iostat ) buffer
		IF ( ( iostat .eq. 0 ) .and. 
     +		     ( buffer ( :5 ) .eq. 'ALIAS' ) ) THEN
		    CALL ST_CLST ( buffer, ' ', ' ', 2, carr, num, ier)
		    IF ( carr ( 2 ) .eq. filnam ) THEN
			CALL GG_ALIS ( lun, NA, idum, ldum, models,ier)
			done = .true.
		    END IF
		END IF
	    END DO
	    CALL FL_CLOS ( lun, ier )
	  ELSE
	    CALL ER_WMSG ( 'FL', ier, 'miscset.tbl', ierr )
	END IF
C
C* 	Get the time for which tracks will be plotted.
C
        CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	CALL CSS_GTIM ( itype, cdttm, ier )
	CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    iret = ier
	    return
	ENDIF
C
C*	For ATCF, get all of the filenames for the requested year.
C*	For ENS_CYC, get the filename for the requested initial time.
C
	path  = ' '
	templ = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
        CALL ST_RNUL ( path, path, lens, ier )
        CALL ST_RNUL ( templ, templ, lens, ier )
        CALL ST_RNUL ( filnam, filnam, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
	CALL FL_MNAM ( dattm2, templ, tfile, ier )
	yy     = dattm2 ( :2 )
	IF ( dattim .eq. 'LAST' ) dattm2 = dattim
C
C*      Process ATCF file(s)
C 
        IF ( filnam .eq. 'ATCF' ) THEN
C
C*	   Convert a two digit year to a four digit year
C
	   CALL ST_INTG ( yy, iyy, ier )
	   CALL TI_YY24 ( iyy, iyyyy, ier )
	   CALL ST_INCH ( iyyyy, yyyy, ier )	
           nexp   = MXNMFL
	   iorder = 1
	   CALL FL_SCND ( path, tfile, iorder, nexp, files, nfile, ier)
	   CALL ST_ALNM ( strnam ( 3:3 ), ityp, ier )
	   IF ( ityp .eq. 1 ) THEN
C
C*	      A storm number (id) of form (al|ep)nn is assumed.  
C*	      Construct the requested file name from the storm id 
C*            and the date.
C
	      CALL ST_UCLC ( strnam ( :2 ), stnm2, iret )
	      trkfil = 'a' // stnm2 // strnam ( 3:4 ) // yyyy // '.dat'
	    ELSE
C
C*	      An alphabetic storm name is requested, or no storm name
C*	      is specified.
C
	      trkfil = ' '
	   END IF
C
C*	   Loop on all ATCF files.
C
	   ifl  = 1
	   done = .false.
	   DO WHILE ( ( ifl .le. nfile ) .and. ( .not. done ) )
	      IF ( ( trkfil .eq. ' ' ) .or. 
     +		   ( trkfil .eq. files ( ifl ) ) ) THEN
	          tfile = path ( :lenp ) // '/' // files ( ifl )
	          CALL GG_NARP ( tfile, dattm2, icolor, mrktyp, ssize,
     +		   	     iwidth, lwidth, iflags, strnam, models, 
     +			     match, ier )
	          IF ( match ) done = .true.
	      END IF
	      ifl = ifl + 1
	   END DO
C
C*      Process ENS_CYC file(s)
C 
          ELSE
	   nexp   = MXNMFL
	   iorder = 1
	   CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier)
	   DO  kk = 1, nfile
	   	CALL FL_MDAT ( files(kk), templ, 'YYMMDD/HH00',
     +				fdt(kk), ier )
     	   END DO
	   IF ( dattm2 .ne. 'LAST' ) THEN
	       CALL TI_MTCH ( 2, dattm2, fdt, nfile, 0, ipos, ier )
	       dattm2 = fdt ( ipos )
	     ELSE
	       ipos = nfile
	   END IF
	   tfile = path ( :lenp ) // '/' // files ( ipos )
	   CALL GG_ENCY ( tfile, dattm2, icolor, mrktyp,
     +			  ssize, iwidth, lwidth, iflags, models, 
     +			  value, ifcsth, ier)
        END IF
C
C*	Reset the attributes.
C
	CALL GSLINE ( jtyp, jlhw, jwid, jwhw, ier )
	CALL GSCOLR ( jcolr, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, jjust,
     +		      ier )
C*
	RETURN
	END
