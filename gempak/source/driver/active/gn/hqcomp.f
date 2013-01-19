	SUBROUTINE HQCOMP ( icolr, color, ired, igreen, iblue, xname,
     +			    iret )
C************************************************************************
C* HQCOMP - GN								*
C*									*
C* This subroutine returns the red, green, and blue components of a	*
C* color.  The color components are defined in the range 0 - 255.  If	*
C* the color was defined by name, the GEMPAK color name is also		*
C* returned.  The X Window System color name is returned if it is	*
C* available.								*
C*									*
C* HQCOMP  ( ICOLR, COLOR, IRED, IGREEN, IBLUE, XNAME, IRET )		*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	COLOR		CHAR*		GEMPAK color name		*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*	XNAME		CHAR*		X Window System color name	*
C*	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* L. Williams/EAI	 3/96						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C* T. PIper/SAIC	1/02	Set all output values			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	color, xname
C-----------------------------------------------------------------------
	color = ' '
	ired = 0
	igreen = 0
	iblue = 0
	xname = ' ' 
C*
	iret = NORMAL
C*
	RETURN
	END
