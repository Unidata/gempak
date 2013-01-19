	SUBROUTINE GDXDSP  ( gdfile, gfunc, endpts, npts, iscale,
     +	                     gdtime, gvcord, nlvl, clvl, dmin, dmax,
     +			     icolor, iline, ilwid, labflg, nflvl, flvl,
     +			     ifcolr, iflabl, ifltyp, device, panel, 
     +			     gvect, points, wind, contin, iret )
C************************************************************************
C* GDXDSP								*
C*									*
C* This subroutine allows the user to exit from GDCROSS.		*
C*									*
C* GDXDSP  ( GDFILE, GFUNC, ENDPTS, NPTS, ISCALE, GDTIME, GVCORD,	*
C*	     NLVL, CLVL, DMIN, DMAX, ICOLOR, ILINE, ILWID, LABFLG, 	*
C*	     NFLVL, FLVL, IFCOLR, IFLABL, IFLTYP, DEVICE, PANEL,	*
C*	     GVECT, POINTS, WIND, CONTIN, IRET )			*
C*									*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	GFUNC		CHAR*		Grid function			*
C*	ENDPTS          CHAR*           Cross section end points	*
C*	NPTS		INTEGER		Number of points		*
C*	ISCALE		INTEGER		Scaling factor			*
C*	GDTIME		CHAR*		Grid dat/time			*
C*	GVCORD		CHAR*		Grid vertical coordinate	*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL  (NLVL)	REAL		Contour levels			*
C*	DMIN		REAL 		Minimum data value		*
C*	DMAX		REAL 		Maximum data value		*
C*	ICOLOR(NLVL)	INTEGER		Contour colors			*
C*	ILINE (NLVL)	INTEGER		Contour line type		*
C* 	ILWID (NLVL)	INTEGER		Contour line width		*
C*	LABFLG(NLVL)	INTEGER		Label flag			*
C*	NFLVL  		INTEGER		Number of fill levels		*
C*	FLVL   (NFLVL+1)REAL		Fill levels			*
C*	IFCOLR (NFLVL+1)INTEGER		Fill colors			*
C*	IFLABL (NFLVL+1)INTEGER		Fill label values		*
C*	IFLTYP (NFLVL+1)INTEGER		Fill types			*
C*	DEVICE		CHAR*		Graphics device			*
C*	PANEL		CHAR*		Graphics panel location		*
C*	GVECT		CHAR*		Vector diagnostic function	*
C*	POINTS		CHAR*		Vector plot points		*
C*	WIND		CHAR*		Vector plot type		*
C*	CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. F. Brill/GSC	 6/89   Created from GDPDSP AND GDNDSP		*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* K. Brill/NMC		01/92	Added the contour fill output		*
C* T. Lee/SAIC		11/01	Displayed contour fill types		*
C************************************************************************
	CHARACTER*(*)	gdfile, gfunc, endpts, gdtime, gvcord,
     +			device, panel, gvect, points, wind
	INTEGER		icolor (*), iline (*), ilwid (*), labflg (*),
     +			ifcolr (*), iflabl (*), ifltyp (*)
	REAL		clvl (*), flvl (*)
	LOGICAL		respnd, contin
C------------------------------------------------------------------------
	iret = 0
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' GDCROSS PARAMETERS' / )
	CALL ST_LSTR  ( gdfile, lenf, ier )
	CALL ST_LSTR  ( gdtime, lent, ier )
	CALL ST_LSTR  ( gvcord, lenv, ier )
	CALL ST_LSTR  ( gfunc,  leng, ier )
	CALL ST_LSTR  ( endpts, lene, ier )
	CALL ST_LSTR  ( device, lend, ier )
	CALL ST_LSTR  ( panel, lenp, ier )
	CALL ST_LSTR  ( gvect, lenu, ier )
	CALL ST_LSTR  ( points, leno, ier )
	CALL ST_LSTR  ( wind, lenw, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat )  gdfile ( : lenf ), 
     +					     gdtime ( : lent ),
     +					     gvcord ( : lenv ),
     +					     gfunc ( : leng ),
     +					     gvect ( : lenu ),
     +					     endpts ( : lene ), npts,
     +					     iscale, panel
1005	FORMAT ( ' Grid file:                 ', A /
     +           ' Date/time:                 ', A /
     +           ' Vertical coordinate:       ', A /
     +           ' Grid function:             ', A /
     +           ' Vector function:           ', A /
     +           ' Endpoints:                 ', A ,10x,
     +           ' Number of points:  ',        I4 /
     +           ' Scaling factor:        10**', I2 ,5x,
     +           ' Panel:  ',                    A / )
C
C*	Write out levels in groups of 7.
C
	IF ( nlvl .gt. 0 ) THEN
	    nblk = ( nlvl - 1 ) / 7 + 1
	    i1 = 1
	    i2 = 7
	    DO i = 1, nblk
	    	IF  ( i2 .gt. nlvl )  i2 = nlvl
	    	WRITE ( 6, 1010 ) ( clvl   (j), j = i1, i2 )
	    	WRITE ( 6, 1011 ) ( icolor (j), j = i1, i2 )
	    	WRITE ( 6, 1012 ) ( iline  (j), j = i1, i2 )
	    	WRITE ( 6, 1013 ) ( ilwid  (j), j = i1, i2 )
	    	WRITE ( 6, 1014 ) ( labflg (j), j = i1, i2 )
1010	    	FORMAT ( /' LEVELS:    ',7F9.2 )
1011	    	FORMAT (  ' COLORS:    ',7I9 )
1012	    	FORMAT (  ' LINTYP:    ',7I9 )
1013	    	FORMAT (  ' LINWID:    ',7I9 )
1014	    	FORMAT (  ' LABEL:     ',7I9 )
	    	i1 = i1 + 7
	    	i2 = i2 + 7
	    END DO
        END IF
C
C*      Write out contour fill information.
C
        IF  ( nflvl .gt. 0 )  THEN
          WRITE ( 6, 2015 ) 
2015      FORMAT ( /, ' FILLED CONTOURS: ' )
          nblk = ( nflvl - 1 ) / 7 + 1
          i1 = 1
          i2 = 7
          DO  i = 1, nblk
            i3 = i2
            IF  ( i2 .ge. nflvl )  THEN
                i2 = nflvl
                i3 = nflvl + 1
            END IF
            WRITE (6, 1010 ) ( flvl  (j), j = i1, i2 )
            WRITE (6, 2011 ) ( ifcolr (j), j = i1, i3 )
2011        FORMAT ( ' COLORS:', 8 ( I7, 2X ) )
            WRITE (6, 2012 ) ( ifltyp (j), j = i1, i3 )
2012        FORMAT ( '  TYPES:', 8 ( I7, 2X ) )
	    i1 = i1 + 7
	    i2 = i2 + 7
          END DO
        END IF
C
C*	If respond is set, wait for user to accept parameters.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
	        CALL TM_ACCP  ( ier )
	        IF  ( ier .eq. 2 )  iret = 3
	    END IF
	END IF
C*
	RETURN
	END
