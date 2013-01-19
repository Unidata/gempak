	SUBROUTINE IM_QSIZ  ( ncol, nrow, iret )
C************************************************************************
C* IM_QSIZ								*
C*									*
C* This subroutine returns the original image size: number of columns   *
C* (the number of pixels or X dimension), and the number of rows (the   *
C* number of lines or Y dimension).  					*
C*									*
C* IM_QSIZ  ( NCOL, NROW, IRET )					*
C*									*
C* Output parameters:							*
C*	NCOL		INTEGER		Number of columns (pixels) 	*
C*	NROW		INTEGER		Number of rows (lines) 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = Invalid image navigation  *
C**									*
C* Log:									*
C* C. Lin/EAI	 8/97							*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C--------------------------------------------------------------------
	iret = 0
C
C*	Check if a file had been read.
C
        IF ( imftyp .eq. IFINVD ) THEN
	    iret = -5
	    RETURN
	ENDIF
C
	ncol = imnpix
	nrow = imnlin
C*
	RETURN
	END
