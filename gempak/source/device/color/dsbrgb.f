	SUBROUTINE DSBRGB  ( icbank, ncolr, icolrs, 
     + 					ireds, igrns, iblus, iret )
C************************************************************************
C* DSBRGB								*
C*									*
C* This subroutine defines the color components of a set of colors 	*
C* by specifying the values of red, green, and blue for each color.	*
C* The color components must be in the range 0 - 255.  The set of	*
C* colors are identified by their color bank ID and the color indices	*
C* in that bank. 							*
C*									*
C* DSBRGB  ( ICBANK, NCOLR, ICOLRS, IREDS, IGRNS, IBLUS, IRET )		*
C*									*
C* Input parameters:							*
C* 	ICBANK		INTEGER		Color bank ID			*
C* 	NCOLR		INTEGER		Number of colors 		*
C* 	ICOLRS (NCOLR)	INTEGER		Array of color indices 		*
C*	IREDS  (NCOLR)	INTEGER		Array of red color components	*
C*	IGRNS  (NCOLR)	INTEGER		Array of green color components	*
C*	IBLUS  (NCOLR)	INTEGER		Array of blue color components	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		 Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		06/95   from DSCRGB()				*
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME;Mod. RGB range	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'COLTBL.CMN'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
        INTEGER         icolrs(*), ireds(*), igrns(*), iblus(*)
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*      Exit, if this is not a color device.
C
	IF  ( .not. colcmp )  RETURN

	DO i = 1, ncolr
C
C*	    Check that this is valid color number on this device.
C
C
C*	    Check that colors are in correct range.
C
	    IF  ( ireds (i) .lt. 0 )  THEN
	        irrr = 0
	      ELSE IF  ( ireds (i) .gt. 255 )  THEN
	        irrr = 255
	      ELSE
	        irrr = ireds(i)
	    END IF
	    IF  ( igrns (i) .lt. 0 )  THEN
	        iggg = 0
	      ELSE IF  ( igrns (i) .gt. 255 )  THEN
	        iggg = 255
	      ELSE
	        iggg = igrns (i)
	    END IF
	    IF  ( iblus (i) .lt. 0 )  THEN
	        ibbb = 0
	      ELSE IF  ( iblus (i) .gt. 255 )  THEN
	        ibbb = 255
	      ELSE
	        ibbb = iblus (i)
	    END IF
C
C*	Check whether color should be added to tables.
C
c	IF  ( icolr .le. MXCLNM )  THEN
C
C*	    Save color information.
C
c	    cname   (icolr) = ' '
c	    icred   (icolr) = irrr
c	    icgrn   (icolr) = iggg
c	    icblue  (icolr) = ibbb
c	    cxname  (icolr) = ' '
c	END IF
C
C*	Set color components on the device.
C
	    CALL HSBRGB  ( icbank, icolrs(i), irrr, iggg, ibbb, iret )
	END DO
C*
	RETURN
	END
