	SUBROUTINE GDESCL  ( rec, iscale, iret )
C************************************************************************
C* GDESCL								*
C*									*
C* This subroutine extracts the scale factor from the grid edit file.	*
C*									*
C* GDESCL  ( REC, ISCALE, IRET )					*
C*									*
C* Input parameters:							*
C*	REC		CHAR*		Data record			*
C*									*
C* Output parameters:							*
C*	ISCALE		INTEGER		Scale factor			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	CHARACTER*(*)	rec
C------------------------------------------------------------------------
	iret   = 0
	iscale = 0
C
C*	Check for the '**' characters.
C
	iexp = INDEX  ( rec, '**' )
	IF  ( iexp .eq. 0 )  RETURN
C
C*	Check for the scale factor.
C
	CALL ST_NUMB  ( rec ( iexp+2 : ), iscale, ier )
	IF  ( ier .ne. 0 )  iscale = 0
C*
	RETURN
	END
