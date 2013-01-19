	SUBROUTINE MT_SHDR  ( report, irpnt, iday, ihour, iminit, iret )
C************************************************************************
C* MT_SHDR							        *
C*								        *
C* This subroutine gets the date/time of an individual METAR report.    *
C* The station id is stored in common.					*
C*								        *
C* MT_SHDR  ( REPORT, IRPNT, IDAY, IHOUR, IMINIT, IRET )  	        *
C*								        *
C* Input parameters:						        *
C*	REPORT		CHAR*		Report    		        *
C*								        *
C* Input and output parameters:					        *
C*	IRPNT		INTEGER		Pointer in report	        *
C*								        *
C* Output parameters:						        *
C*      CIVALS(ICSTID)  CHAR*           Report ID                       *
C*	IDAY		INTEGER		Report day		        *
C*	IHOUR		INTEGER		Report hour		        *
C*	IMINIT		INTEGER 	Report minute		        *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return	        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP     	10/95	 				        *
C* D. Kidwell/NCEP     	 3/96	Modified for NCO processing of stn tbl  *
C* D. Kidwell/NCEP     	 9/96	Cleaned up, added country check for id  *
C* D. Kidwell/NCEP     	11/96	Modified to use binary search routine   *
C* K. Tyle/GSC		 1/97	Check for "METAR" in report text; 	*
C*				eliminate BUFR conditionally compiled	*
C*				code; reorganize header and comments	*
C* K. Tyle/GSC		 4/97	Allow for 8-char station id's		*
C* D. Kidwell/NCEP     	 6/97	ST_LSTR -> INDEX; use explicit char len *
C* D. Kidwell/NCEP       4/98   New interface; changed calling sequence;*
C*				removed country determination           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'mtcmn.cmn'
C*	
	CHARACTER*(*)	report
C*	
	CHARACTER       id*8
C------------------------------------------------------------------------
        iret = 0 
C
C*	Get station id:  first string in report.
C
	iblk = INDEX ( report ( irpnt:irpnt+8 ), ' ' )
	IF ( iblk .lt. 2 ) THEN
	    id = ' '
	  ELSE
	    id = report ( irpnt:irpnt + iblk - 2 )
	    irpnt = irpnt + iblk 
	END IF
	civals ( icstid ) = id
C
C*	Search for "METAR" in field after station ID.
C*	If it exists, increment pointer past it.
C
	imtr = INDEX ( report ( irpnt:irpnt+6 ), 'METAR' )
	IF ( imtr .gt. 0 ) irpnt = irpnt + imtr + 5 
	
C
C*	Get date/time from report.
C
	CALL MT_DATE ( report ( :irpnt+6 ), irpnt, iday, ihour, iminit,
     +		       idecd, kret )
C*
	RETURN
	END
