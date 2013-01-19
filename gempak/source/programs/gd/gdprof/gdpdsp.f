	SUBROUTINE GDPDSP  ( gdfile, gdatim, gfunc, rgx, rgy, rlat, 
     +			     rlon, npts, rmindt, rmaxdt, iscale, 
     +			     gvect, wind, contin, iret )
C************************************************************************
C* GDPDSP								*
C*									*
C* This subroutine allows the user to exit from GDPROF.			*
C*									*
C* GDPDSP  ( GDFILE, GDATIM, GFUNC, RGX, RGY, RLAT, RLON, NPTS, RMINDT,	*
C*           RMAXDT, ISCALE, GVECT, WIND, CONTIN, IRET )		*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	GDATIM		CHAR*		Grid time			* 
C*	GFUNC		CHAR*		Grid function			*
C*	RGX		REAL		X grid point			*
C*	RGY		REAL		Y grid point			*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	NPTS		INTEGER		Number of points		*
C*	RMINDT		REAL		Minimum data value		*
C*	RMAXDT		REAL		Maximum data value		*
C*	ISCALE		INTEGER		Scaling factor			*
C*      GVECT           CHAR*           Grid vector function		*
C*      WIND            CHAR*           Wind symbol and size		*
C*      CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 4/89	Added min, max				*
C* K. Brill/GSC         12/89   Added gvect and wind			*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* T. Lee/GSC		 7/01	Added contin				*
C* R. Tian/SAIC		10/02	Added call to DG_IGRG			*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gfunc, gvect, wind
	LOGICAL		respnd, contin
C------------------------------------------------------------------------
	iret = 0
C
C*	Display grid as reference grid
C
	CALL DG_IGRG ( 1, rgx, rgy, rgx, rgy, ier )
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' GDPROF PARAMETERS' / )
	CALL ST_LSTR  ( gdfile, lenf, ier )
	CALL ST_LSTR  ( gdatim, lent, ier )
	CALL ST_LSTR  ( gfunc,  leng, ier )
	CALL ST_LSTR  ( gvect,  lenv, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat )  gdfile ( : lenf ), 
     +					     gdatim ( : lent ),
     +					     gfunc ( : leng ),
     +                                       gvect ( : lenv ),
     +					     rgx, rgy, rlat, rlon, npts,
     +					     rmindt, rmaxdt, iscale,
     +                                       wind
1005	FORMAT ( ' Grid file:                 ', A /
     +           ' Grid time:                 ', A /
     +           ' Grid function:             ', A /
     +           ' Grid vector function:      ', A /
     +           ' X,Y grid point:          ',    2F8.2 /
     +           ' Lat/lon:                 ',    2F8.2 /
     +           ' Number of levels:         ',     I4 /
     +           ' Data range:              ',     2F8.2 /
     +           ' Scaling factor:             10**', I2 /
     +           ' Wind display:            ',   A )
C
C*	Allow user to exit if respond is set.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
	        CALL TM_ACCP  ( ier )
	        IF  ( ier .eq. 2 )  iret = 1
	    END IF
	END IF
C*
	RETURN
	END
