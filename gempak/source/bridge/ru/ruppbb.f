	SUBROUTINE RU_PPBB ( report, lenr, wnknot, ipoint, zdata, nzwnd,
     +			     pdata,  npwnd, iret )
C************************************************************************
C* RU_PPBB								*
C*									*
C* This subroutine decodes PPBB reports.  These reports contain		*
C* significant wind data below 100 mb.  These winds may be on		*
C* height or pressure surfaces.  Height data is returned as  		*
C* HGHT DRCT SPED .  Pressure data is returned as  PRES DRCT SPED .	*
C*									*
C* RU_PPBB  ( REPORT, LENR, WNKNOT, IPOINT, ZDATA, NZWND, PDATA,	*
C*            NPWND,  IRET )						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PPBB report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	ZDATA (3,NZWND)	REAL		Sig wind data on height		*
C*	NZWND		INTEGER		Number of height levels		*
C*	PDATA (3,NPWND)	REAL		Sig wind data on pressure	*
C*	NPWND		INTEGER		Number of pressure levels	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/87	Written for GEMPAK4			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C
	REAL		zdata ( 3, * ), pdata ( 3, * )
	CHARACTER*(*)	report
	LOGICAL		wnknot
C*
	LOGICAL		above, pres
C------------------------------------------------------------------------
	iret  = 0
	above = .false.
	nzwnd = 0
	npwnd = 0
C	
C*	Check for pressure data which is preceded by "21212".
C
	ipres =  INDEX ( report ( ipoint: lenr ), '21212' )
	IF  ( ipres .eq. 0 )  THEN
	    pres  = .false.
	    lens  = lenr
	  ELSE
	    ipres = ipoint + ipres - 1
	    pres  = .true.
	    lens  = ipres - 1
	    ipres = ipres + 5
	END IF
C
C*	Process the height data.
C
	CALL RU_SIGW  ( report, lens, wnknot, above, ipoint, zdata, 
     +			nzwnd,  ier )
C
C*	Process the pressure data.
C
	IF  ( pres )  THEN
	    ipoint = ipres 
	    CALL RU_PWND ( report, lenr, wnknot, above, ipoint, pdata, 
     +			   npwnd,  ier )
	END IF
C*
	RETURN
	END
