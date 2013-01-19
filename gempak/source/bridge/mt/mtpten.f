	SUBROUTINE MT_PTEN ( stpten, iret )
C************************************************************************
C* MT_PTEN							        *
C*								        *
C* This subroutine will decode the characteristic and amount of the	*
C* 3-hourly pressure tendency.  The result is saved in common.		*
C*								        *
C* MT_PTEN ( STPTEN, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STPTEN		CHAR*		Pressure tendency string	*
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRP03D)  REAL            3 hour pressure tendency (hPa)  *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  22 = decode error 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	11/95	Original author		                *
C* K. Tyle/GSC	 	 1/97	Use PR_P03D; reorganize hdr & comments  *
C* K. Tyle/GSC		 2/97	Change error processing			*
C* D. Kidwell/NCEP       4/98   New interface                           *
C* D. Kidwell/NCEP       3/00   Ensure that string is all numeric       *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*)	stpten
C*	
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_INTG ( stpten ( 2:5 ), intg, ier )
	IF ( ier .eq. 0 ) THEN
C
            pten = PR_P03D ( stpten ( 2:5 ) )
            IF ( ERMISS ( pten ) ) THEN
	        iret = 22
              ELSE
	        rivals ( irp03d ) = pten
	    END IF
	  ELSE
	    iret = 22
	END IF
C*
	RETURN
	END
