	SUBROUTINE SNTDSP  ( snfile, parm, stnprm, area,  datmin, 
     +			     datmax, npts, trange, panel, respnd, iret )
C************************************************************************
C* SNTDSP								*
C*									*
C* This subroutine allows the user to exit from SNTSER.			*
C*									*
C* SNTDSP  ( SNFILE, PARM,  STNPRM, AREA, DATMIN, DATMAX, NPTS,		*
C*           TRANGE, PANEL, RESPND, IRET )							*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*	PARM		CHAR*		Level parameter			*
C*	STNPRM		CHAR*		Station parameter		*
C*	AREA		CHAR*		Station				*
C*	DATMIN		REAL		Minimum value in data		*
C*	DATMAX		REAL		Maximum value in data		*
C*	NPTS		INTEGER		Number of points		*
C*	TRANGE		CHAR*		Time range			*
C*	PANEL		CHAR*		User input for panel location	*
C*	RESPND		LOGICAL		Interactive response flag	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  1 = user entered EXIT		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from GDTDSP			*
C* J. Whistler/SSAI	 5/91	Added commas around "/" in format	*
C*				statements				*
C************************************************************************
	CHARACTER*(*)	snfile, parm, stnprm, area, trange, panel
	LOGICAL		respnd
C
	CHARACTER*4	outprm
C*
C------------------------------------------------------------------------
	iret = 0
	IF  ( parm .ne. ' ' )  THEN
	    outprm = parm
	  ELSE
	    outprm = stnprm
	END IF
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' SNTSER PARAMETERS', /  )
	CALL ST_LSTR  ( snfile, lenf, ier )
	CALL ST_LSTR  ( outprm, leng, ier )
	CALL ST_LSTR  ( area,   lena, ier )
	WRITE  ( 6, 1005, IOSTAT = iostat )  snfile ( :lenf ), 
     +					     outprm ( :leng ),
     +					     area   ( :lena ), npts,
     +					     trange, datmin, datmax,
     +					     panel
1005	FORMAT ( ' Sounding file:      ', A       , /,
     +           ' Parameter:          ', A       , /,
     +           ' Area:               ', A       , /,
     +           ' Number of times:    ', I4      , /,
     +           ' Date range:         ', A       , /,
     +           ' Data range:         ', 1P2G12.4, /,
     +           ' Panel location:     ', A           )
C
C*	Allow user to exit if respond is set.
C
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = 1
	END IF
C*
	RETURN
	END
