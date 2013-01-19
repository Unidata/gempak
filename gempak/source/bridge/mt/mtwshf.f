	SUBROUTINE MT_WSHF ( stwshf, lenwsh, iret )
C************************************************************************
C* MT_WSHF							        *
C*								        *
C* This subroutine will decode the time of a wind shift.  The time	*
C* may be in minutes only, or in hours and minutes.  The values are	*
C* stored in common.							*
C*								        *
C* MT_WSHF ( STWSHF, LENWSH, IRET )			   	        *
C*								        *
C* Input parameters:						        *
C*	STWSHF		CHAR*		Wind shift string		*
C*	LENWSH		INTEGER		Length of string	        *
C*	IRHOUR		INTEGER 	Hour of report                  *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRWSHH)	REAL		Hour of wind shift              *
C*	RIVALS(IRWSHM)	REAL		Minute of wind shift            *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 -2 = decode error 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	12/95	Original author		                *
C* K. Tyle/GSC		 1/97	Reorganize header and comments		*
C* D. Kidwell/NCEP       6/97   Replaced ST_CRNM with ST_INTG           *
C* D. Kidwell/NCEP       4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stwshf
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	IF ( ERMISS ( rivals ( irwshh ) ) ) THEN
	    IF ( lenwsh .eq. 2 ) THEN
	        CALL ST_INTG ( stwshf ( 1:2 ), iwshfm, iret )
		rivals ( irwshm ) = FLOAT ( iwshfm )
	        rivals ( irwshh ) = FLOAT ( irhour )
	      ELSE IF ( lenwsh .eq. 4 ) THEN
	        CALL ST_INTG ( stwshf ( 1:4 ), iwshft, iret )
	        IF ( iret .eq. 0 ) THEN
		    rivals ( irwshh ) = FLOAT ( iwshft / 100 )
		    rivals ( irwshm ) = FLOAT ( MOD ( iwshft, 100 ) ) 
	        END IF
	    END IF
	END IF
C*
	RETURN
	END
