	SUBROUTINE GDTDSP  ( gdfile, gfunc,  gpoint, rgx,    rgy,  rlat,
     +			     rlon,   datmin, datmax, iscale, npts, 
     +			     trange, panel,  iret )
C************************************************************************
C* GDTDSP								*
C*									*
C* This subroutine allows the user to exit from GDTSER.			*
C*									*
C* GDTDSP  ( GDFILE, GFUNC,  GPOINT, RGX,  RGY,    RLAT,  RLON,		*
C*           DATMIN, DATMAX, ISCALE, NPTS, TRANGE, PANEL, IRET )	*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	GFUNC		CHAR*		Grid function			*
C*	GPOINT		CHAR*		Grid location			*
C*	RGX		REAL		X grid point			*
C*	RGY		REAL		Y grid point			*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	DATMIN		REAL		Minimum data value		*
C*	DATMAX		REAL		Maximum data value		*
C*	ISCALE		INTEGER		Data scaling factor		*
C*	NPTS		INTEGER		Number of points		*
C*	TRANGE		CHAR*		Time range of grids		*
C*	PANEL		CHAR*		User input for panel location	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Adapted from GDPDSP			*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* J. Whistler/SSAI	 5/91	Added commas around "/" in format 	*
C*				statements				*
C* R. Tian/SAIC         10/02   Added call to DG_IGRG                   *
C************************************************************************
	CHARACTER*(*)	gdfile, gfunc, gpoint, trange, panel
	LOGICAL		respnd
C*
C------------------------------------------------------------------------
	iret = 0
C
C*      Display grid as reference grid
C
	CALL DG_IGRG ( 1, rgx, rgy, rgx, rgy, ier )
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' GDTSER PARAMETERS', /  )
	CALL ST_LSTR  ( gdfile, lenf, ier )
	CALL ST_LSTR  ( gfunc,  leng, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat )  gdfile ( : lenf ), 
     +					     gfunc ( : leng )
1005	FORMAT ( ' Grid file:                 ', A, /,
     +           ' Grid function:             ', A )
C
	isep = INDEX ( gpoint, ';' ) + INDEX ( gpoint, '/' )
	IF  ( isep .eq. 0 )  WRITE  ( 6, 1007 )  gpoint
1007	FORMAT ( ' Station:                   ', A )
C
	WRITE  ( 6, 1009 )  rgx,    rgy,    rlat,   rlon, npts, trange, 
     +			    datmin, datmax, iscale, panel
1009	FORMAT ( ' X,Y grid point:          ',    0P2F8.2    , /,
     +           ' Lat/lon:                 ',    2F8.2      , /,
     +           ' Number of times:         ',    I4         , /,
     +           ' Date range:              ',    A          , /,
     +           ' Data range:              ',    1P2G12.4   , /,
     +           ' Data scale factor:       ',    '10 **', I3, /,
     +           ' Panel location:          ',    A           )
C
C*	Allow user to exit if respond is set.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = 1
	END IF
C*
	RETURN
	END
