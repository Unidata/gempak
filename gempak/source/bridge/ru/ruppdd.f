	SUBROUTINE RU_PPDD ( report, lenr, wnknot, ipoint, zdata, nzwnd,
     +			     pdata,  npwnd, iret )
C************************************************************************
C* RU_PPDD								*
C*									*
C* This subroutine decodes PPDD reports.  These reports contain		*
C* significant wind data from above 100 mb on height or pressure	*
C* surfaces.  The output height data are ordered  HGHT DRCT SPED .	*
C* The output pressure data are ordered  PRES DRCT SPED .		*
C*									*
C* RU_PPDD  ( REPORT, LENR, WNKNOT, IPOINT, ZDATA, NZWND, PDATA, 	*
C*           NPWND,  IRET )						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PPDD report			*
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
C* B. Doty/RDS		 9/87		Mod of RU_PPBB			*
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
	above = .true.
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
