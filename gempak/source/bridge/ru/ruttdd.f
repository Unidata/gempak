	SUBROUTINE RU_TTDD ( report, lenr, wnknot, ipoint, tdata, ntemp,
     +			     wdata,  nwind, iret )
C************************************************************************
C* RU_TTDD								*
C*									*
C* This subroutine decodes TTDD reports.  These reports contain		*
C* significant temperature data above 100 mb and may also contain	*
C* significant wind data on pressure surfaces.  The output temperature	*
C* data are ordered PRES TEMP DWPT.  The output wind data are ordered	*
C* PRES DRCT SPED .							*
C*									*
C* RU_TTDD  ( REPORT, LENR, WNKNOT, IPOINT, TDATA, NTEMP, WDATA,	*
C*            NWIND, IRET )						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		TTDD report			*
C*	LENR		INTEGER		Length of report		*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	TDATA (3,NTEMP)	REAL		Significant temperature data	*
C*	NTEMP		INTEGER		Number of temperature levels	*
C*	WDATA (3,NWIND)	REAL		Significant wind data		*
C*	NWIND		INTEGER		Number of wind levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* B. Doty	 9/87		Copy of RU_TTBB				*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C
	REAL		tdata ( 3, * ), wdata ( 3, * )
	CHARACTER*(*)	report
	LOGICAL		wnknot
C*
	LOGICAL		above, wind
C------------------------------------------------------------------------
	iret  = 0
	above = .true.
	level = -1
	nwind = 0
	ntemp = 0
C	
C*	Check for significant wind data which is preceded by "21212".
C
	iwind  =  INDEX ( report ( ipoint: lenr ), '21212' )
	IF  ( iwind .eq. 0 )  THEN
	    wind  = .false.
	    lens  = lenr
	  ELSE
	    iwind = ipoint + iwind - 1
	    wind  = .true.
	    lens  = iwind - 1
	    iwind = iwind + 5
	END IF
C
C*	Process the temperature data.
C
	CALL RU_STMP ( report, lens, above, ipoint, tdata, ntemp, ier )
C
C*	Process the wind data.
C
	IF  ( wind )  THEN
	    ipoint = iwind
	    CALL RU_PWND ( report, lenr, wnknot, above, ipoint, wdata, 
     +			   nwind,  ier )
	END IF
C*
	RETURN
	END
