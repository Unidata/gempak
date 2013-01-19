	SUBROUTINE GSCNAM  ( icolr, color, iret )
C************************************************************************
C* GSCNAM								*
C*									*
C* This subroutine defines the color corresponding to a color 		*
C* number by specifying a color name.  Maximum characters of		*
C* the color name = 40.  The colors are stored in coltbl.tbl.		*
C*									*
C* GSCNAM  ( ICOLR, COLOR, IRET )					*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	COLOR		CHAR*		Color name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* D. Keiser/GSC	12/95	Removed reference to GEMTBL in comments	*
C* L. Williams/EAI	 3/96   Removed kolor and Call to ST_LCUC	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C
	CHARACTER*(*) 	color
C-----------------------------------------------------------------------
C*	Set the color by name.
C
	CALL DSCNAM  ( icolr, color, iret )
C*
	RETURN
	END
