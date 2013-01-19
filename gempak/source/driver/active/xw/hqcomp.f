	SUBROUTINE HQCOMP ( icolr, color, ired, igreen, iblue, xname,
     +			    iret )
C************************************************************************
C* HQCOMP - XW								*
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
C* S. Maxwell/GSC	 6/97	Documentation changes			*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	color, xname
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Retrieve the values from structure.
C
	color(1:1) = ' '
	xname(1:1) = ' '
C
 	CALL XQCOMP( icolr, color, ired, igreen, iblue, xname, ilen,
     +		     ilxn, iret )

C
C*	Clear the unused space of Color name and X color name
C
 	color(ilen+1: ) = ' '
 	xname (ilxn+1: ) = ' '

C*
	RETURN
	END
