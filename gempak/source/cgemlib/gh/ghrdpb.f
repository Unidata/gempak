	SUBROUTINE GH_RDPB ( strnam, advno, wtype, timstr, 
     +                       wname, wadnm, wwnd, gust, wdir, wsped, 
     +                       wpres, rlat, rlon, iret )
C************************************************************************
C* GH_RDPB								*
C*									*
C* This subroutine reads the public advisory text bulletin for          *
C* tropical storms.							*
C*									*
C* GH_RDPB ( STRNAM, ADVNO, WTYPE, TIMSTR, WNAME, WADNM, 		*
C*           WWND, WDIR, WSPED, WPRES, RLAT, RLON,  IRET )              *
C*									*
C* Input parameters:							*
C*	STRNAM		CHAR*		Tropical storm name		*
C*	ADVNO		CHAR*           Advisory number			*
C*									*
C* Output parameters:							*
C* 	WTYPE 		CHAR*		Tropical storm type		*
C* 	TIMSTR 		CHAR*		Date/time time string 		*
C*	WNAME 		CHAR*		Tropical storm name		*
C*	WADNM 		CHAR*		Advisory number			*
C*	WWND 		CHAR*		Max sustained winds		*
C*	GUST 		CHAR*		Wind gust			*
C*	WDIR 		CHAR*		Movement direction		*
C*	WSPED 		CHAR*		Movement speed (kts)		*
C*	WPRES 		CHAR*		Central minimum pressure (mb)	*
C*	RLAT 		REAL		Current/forecasted Latitudes	*
C*	RLON 		REAL		Current/forecasted Longitudes	*
C*	IRET		INTEGER		Return code			*
C*                                        0 = normal return             *
C*                                       -1 = could not open public     *
C*                                            advisory                  *
C*                                       -2 = could not read public     *
C*                                            advisory header           *
C*                                       -3 = could not find public     *
C*                                            advisory                  *
C*									*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	 1/06	Modified from GH_RDAD                   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* S. Gilbert/NCEP	 6/06	Added call to FL_SCND to get file list  *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* C. Lauer/NHC          3/09   Added call to GH_PSUM for new pub format*
C* X. Guo/CWS		02/10   Changed GH_PSUM to support the new	*
C*                              public format and called GH_PSUM09 to   *
C*                              read the old format                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strnam, advno, wtype, wname, wpres,
     +                  wadnm,  wwnd, wdir, wsped, timstr, gust
        REAL            rlat, rlon
C*
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
	CHARACTER	path*25, templ*25, tfile*128,
     +                  adnum*4, record*10000, rec*128
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*      Get path to public advisory files
C
        filnam = 'HCNPUB'
        path  = ' '
        templ = ' '
        CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
        CALL ST_RNUL ( path, path, lenp, ier )
        CALL ST_RNUL ( templ, templ, lent, ier )
C
C*      Get list of public advisory files
C
        CALL ST_NULL ( advno, advno, lena, ier )
        CALL GH_ADVN( advno, iadnm, iflag, ier )
        WRITE ( adnum, fmt='(I3.3)' ) iadnm
        IF ( iflag .eq. 1 ) THEN
            CALL ST_RPST ( templ, 'X', 'a', ipos, templ, ier )
        ELSE IF ( iflag .eq. 2 ) THEN
            CALL ST_RPST ( templ, 'X', 'b', ipos, templ, ier )
        END IF
        nexp   = MXNMFL
        iorder = 1
        CALL FL_SCND ( path, templ, iorder, nexp, files, nfiles, ier )
        tfile = ' '
        DO ifl = 1, nfiles
           IF ( files(ifl)(1:8) .eq. strnam .AND. 
     +          files(ifl)(19:21) .eq. adnum ) THEN
              tfile = path(:lenp) // '/' // files(ifl)
           ENDIF
        ENDDO
        IF ( tfile .eq. ' ' ) THEN
            iret = -3
            RETURN
        ENDIF
C
C*	Open/read public advisory
C
        CALL FL_SOPN ( tfile, lunf, ier )
        IF ( ier .ne. 0 ) THEN
            iret = -1
            RETURN
        ENDIF
C
        lrec = 0
        iostat = 0
        DO WHILE  ( iostat .eq. 0 )
            READ  ( lunf, 1000, IOSTAT = iostat ) rec
 1000       FORMAT (A)
            CALL ST_LSTR( rec, lent, ier )
            IF ( lrec .eq. 0 ) THEN
                lrec = lent
                record = rec (:lent)
            ELSE
                CALL ST_LSTR( record, lrec, ier )
                record = record(:lrec) // ' ' // rec (:lent)
                CALL ST_LSTR( record, lrec, ier )
            END IF     
        END DO
        CALL FL_CLOS ( lunf, ier )
C
	CALL ST_LSTR ( record, lrec1, ier )
	CALL ST_UNPR ( record, lrec1, record, lrec, ier )
	CALL ST_LCUC ( record, record, ier )
C
C*      Decode the advisory header lines.
C
        CALL GH_PUHD ( record, wtype, wname, wadnm, timstr, ier )
        IF ( ier .ne. 0 ) THEN
            iret = -2
        END IF
C
C*      Find and decode the Repeat Section. (GH_PRPT is old format)
C
 	CALL GH_PSUM ( record, lrec, rlat, rlon, wdir, wsped, 
     +                 wpres, wwnd, gust, ier )
        IF ( ier .ne. 0 ) THEN
            CALL GH_PSUM09 ( record, lrec, rlat, rlon, wdir, wsped, 
     +                 wpres, wwnd, gust, ier )
	    IF ( ier .ne. 0 ) THEN
	    	iret = -3
	    END IF
	    
        END IF
C
	RETURN
	END
