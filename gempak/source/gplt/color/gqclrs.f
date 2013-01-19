	SUBROUTINE GQCLRS  ( ibank, ncolr, iret )
C************************************************************************
C* GQCLRS								*
C* 									*
C* This subroutine returns the colors in a color bank.			*
C* 									*
C* GQCLRS  ( IBANK, NCOLR, IRET )					*
C*									*
C* Input parameters:							*
C* 	IBANK		INTEGER		Color bank number		*
C*					0 = graphics			*
C*					1 = satellite			*
C*					2 = radar			*
C* Output parameters:							*
C* 	NCOLR		INTEGER		Number of colors		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	11/95						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL DQCLRS ( ibank, ncolr, iret )
C*
	RETURN
	END
