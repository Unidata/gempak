	SUBROUTINE DSCNAM  ( icolr, color, iret )
C************************************************************************
C* DSCNAM								*
C*									*
C* This subroutine assigns a GEMPAK color name to a color number.	*
C* The color name must be one defined in the color table COLTBL.TBL .	*
C*									*
C* DSCNAM  ( ICOLR, COLOR, IRET )					*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	COLOR		CHAR*		GEMPAK Color name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Fixes to save in common			*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* L. Williaims/EAI	 3/96	Added call to HSCNAM; Removed reference *
C*				to common block.			*
C************************************************************************
C*
	CHARACTER*(*)	color
C-----------------------------------------------------------------------
C
C*	Find the requested color in the color lookup table.
C
	CALL HSCNAM ( icolr, color, iret )

	RETURN
	END
