	SUBROUTINE RU_TTCC  ( report, lenr, itopwn, wnknot, ipoint, 
     +			      data, nlev, tdata, nlevt, wxdata, nlevw,
     +			      iret )
C************************************************************************
C* RU_TTCC								*
C*									*
C* This subroutine decodes a TTCC report.  These reports contain	*
C* mandatory, tropopause and max wind data above 100 mb.  The mandatory *
C* data are ordered PRES TEMP DWPT DRCT SPED HGHT.  Tropopause data are *
C* ordered PRES TEMP DWPT DRCT SPED.  Max wind data are ordered         *
C* PRES DRCT SPED.                      	          		*
C*									*
C* RU_TTCC  ( REPORT, LENR, ITOPWN, WNKNOT, IPOINT, DATA, NLEV, TDATA,  *
C* 	      NLEVT, WXDATA, NLEVW, IRET )  	                        *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		TTCC report			*
C*	LENR		INTEGER		Length of station report	*
C*	ITOPWN		INTEGER		Pressure for last wind report	*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*									*
C* Input and output parameters:						*
C*	IPOINT		INTEGER		Pointer within report		*
C*									*
C* Output parameters:							*
C*	DATA (6,NLEV)	REAL		Mandatory level data		*
C*	NLEV		INTEGER		Number of mandatory levels	*
C*	TDATA (5,NLEVT)	REAL		Tropopause level data		*
C*	NLEVT		INTEGER		Number of tropopauses           *
C*	WXDATA(3,NLEVW)	REAL		Maximum wind level data		*
C*	NLEVW		INTEGER		Number of maximum wind levels	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* D. Kidwell/NCEP	 2/01	Added tropopause and max wind data      *
C* D. Kidwell/NCEP	 2/05	Added drop (925mb) flag to RU_MLVL call *
C************************************************************************
	CHARACTER*(*)	report
	REAL		data (*), tdata (*), wxdata (*)
	LOGICAL		wnknot
C*
	LOGICAL		above
C------------------------------------------------------------------------
	iret  = 0
	above = .true.
C
C*	Read the mandatory level data.
C
	CALL RU_MLVL ( report, lenr, wnknot, itopwn, above, .false., 
     + 		       ipoint, data, nlev, iret )
C
C*	Read the tropopause data.
C
	CALL RU_MTRP ( report, lenr, wnknot, above, ipoint, tdata,
     +		       nlevt, ier )
C
C*	Read the maximum wind data.
C
	CALL RU_MMXW ( report, lenr, wnknot, above, ipoint, wxdata,
     +		       nlevw, ier )
C*
	RETURN
	END
