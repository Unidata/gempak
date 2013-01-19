	SUBROUTINE GSBRGB ( icbank, ncolr, icolrs, 
     +				ireds, igrns, iblus, iret )
C************************************************************************
C* GSBRGB								*
C*									*
C* This subroutine defines the color components of a set of colors 	*
C* by specifying the values of red, green, and blue for each color.	*
C* The color components must be in the range 0 - 255.  The set of	*
C* colors are identified by their color bank ID and the color indices	*
C* in that bank. 							*
C*									*
C* GSBRGB  ( ICBANK, NCOLR, ICOLRS, IREDS, IGRNS, IBLUS, IRET )		*
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
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 6/95	from GSCRGB()				*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER		icolrs(*), ireds(*), igrns(*), iblus(*)
C-----------------------------------------------------------------------
	CALL DSBRGB  ( icbank, ncolr, icolrs, 
     +   			ireds, igrns, iblus, iret )
C*
	RETURN
	END
