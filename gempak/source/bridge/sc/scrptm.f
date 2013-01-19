	SUBROUTINE SC_RPTM  ( report, irpnt, iday, ihour, iminit, iret )
C************************************************************************
C* SC_RPTM							        *
C*								        *
C* This subroutine gets the date/time of an individual SCD report.      *
C*								        *
C* SC_RPTM  ( REPORT, IRPNT, IDAY, IHOUR, IMINIT, IRET)                 *
C*								        *
C* Input parameters:						        *
C*	REPORT		CHAR*		Report    		        *
C*								        *
C* Output parameters:						        *
C*	IRPNT		INTEGER		Pointer in report	        *
C*	IDAY		INTEGER		Report day		        *
C*	IHOUR		INTEGER		Report hour		        *
C*	IMINIT		INTEGER 	Report minute		        *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return	        *
C*                                       17 = time decode problem       *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP     	 3/97	Based on MT_DATE                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C* D. Kidwell/NCEP     	 9/02	Replaced SC_DTTM with BR_DTTM           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*	
	CHARACTER*(*)	report
C*
	CHARACTER 	strdat*10
C*
C------------------------------------------------------------------------
        iret = 0 
C
C*	Skip over station identifier and characters 'SCD'.
C
	irpnt = INDEX ( report, ' SCD ' ) + 5
C
C*	Skip over characters 'COR' or 'RTD' if present.
C
	IF ( ( report ( irpnt:irpnt+2 ) .eq. 'COR' ) .or.
     +       ( report ( irpnt:irpnt+2 ) .eq. 'RTD' ) ) irpnt = irpnt + 4
C
C*	Get date/time from report.
C
	iend = INDEX ( report ( irpnt: ), ' ' )
	IF ( iend .gt. 1 ) THEN
	    strdat = report ( irpnt:irpnt + iend - 2 )
	    CALL ST_LSTR ( strdat, lendat, iret ) 
	    irpnt = irpnt + lendat
	    IF ( lendat .eq. 4 .or. lendat .eq. 6 ) lendat = lendat + 1
	    CALL BR_DTTM ( strdat, lendat, iday, ihour, iminit, iret )
	  ELSE
            iday   = IMISSD
            ihour  = IMISSD
            iminit = IMISSD
            iret = 17
	END IF 
C*
	RETURN
	END
