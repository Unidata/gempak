	SUBROUTINE GDTSSP ( gdfile, gfunc, npts, iscale, gdtime,
     +			    gvcord, nlvl, clvl, dmin, dmax, icolor,
     +			    iline, ilwid, labflg, device, panel, iret ) 
C************************************************************************
C* GDTSSP								*
C*									*
C* This subroutine allows the user to exit from GDTSPDF.		*
C*									*
C* GDTSSP  ( GDFILE, GFUNC, NPTS, ISCALE, GDTIME, GVCORD, NLVL,		*
C*	     CLVL, DMIN, DMAX, ICOLOR, ILINE, ILWID, LABFLG, DEVICE,	*
C*	     PANEL, IRET )			*
C*									*   
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	GFUNC		CHAR*		Grid function			*
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
C*	DEVICE		CHAR*		Graphics device			*
C*	PANEL		CHAR*		Graphics panel location		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. Li/SAIC		10/07						*
C************************************************************************
	CHARACTER*(*)	gdfile, gfunc, gdtime, gvcord,
     +			device, panel
	INTEGER		 icolor (*), iline (*), ilwid (*), labflg (*)
	REAL		clvl (*)
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' GDTSPDF PARAMETERS' / )
	CALL ST_LSTR  ( gdfile, lenf, ier )
	CALL ST_LSTR  ( gdtime, lent, ier )
	CALL ST_LSTR  ( gvcord, lenv, ier )
	CALL ST_LSTR  ( gfunc,  leng, ier )
	CALL ST_LSTR  ( device, lend, ier )
	CALL ST_LSTR  ( panel, lenp, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat ) gdfile ( : lenf ), 
     +					    gdtime ( : lent ),
     +					    gvcord ( : lenv ),
     +					    gfunc ( : leng ),
     +					    iscale, panel, dmax, dmin
1005	FORMAT ( ' Grid file:                 ', A /
     +           ' Date/time:                 ', A /
     +           ' Vertical coordinate:       ', A /
     +           ' Grid function:             ', A /
     +           ' Scaling factor:        10**', I2 ,5x,
     +           ' Panel:  ',                    A / 
     +           ' Max: ',F10.4,' -- Min: ',F10.4)
C
C*	Write out levels in groups of 7.
C
	nblk = ( nlvl - 1 ) / 7 + 1
	i1 = 1
	i2 = 7
	DO i = 1, nblk
	    IF ( i2 .gt. nlvl ) i2 = nlvl
	    WRITE ( 6, 1010 ) ( clvl   (j), j = i1, i2 )
	    WRITE ( 6, 1011 ) ( icolor (j), j = i1, i2 )
	    WRITE ( 6, 1012 ) ( iline  (j), j = i1, i2 )
	    WRITE ( 6, 1013 ) ( ilwid  (j), j = i1, i2 )
	    WRITE ( 6, 1014 ) ( labflg (j), j = i1, i2 )
1010	    FORMAT ( /' LEVELS:    ',7F9.2 )
1011	    FORMAT (  ' COLORS:    ',7I9 )
1012	    FORMAT (  ' LINTYP:    ',7I9 )
1013	    FORMAT (  ' LINWID:    ',7I9 )
1014	    FORMAT (  ' LABEL:     ',7I9 )
	    i1 = i1 + 7
	    i2 = i2 + 7
	END DO
C
C*	If respond is set, wait for user to accept parameters.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1 
	END IF
C*
	RETURN
	END
