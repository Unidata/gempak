	SUBROUTINE IM_QLUT  ( lutfil, iret )
C************************************************************************
C* IM_QLUT								*
C*									*
C* This subroutine returns the default lookup table file name.		*
C*									*
C* IM_QLUT  ( LUTFIL, IRET )						*
C*									*
C* Output parameters:							*
C*	LUTFIL		CHAR*		Lookup table name 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = file hasn't been read	*
C**									*
C* Log:									*
C* C. Lin/EAI		12/97						*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/SAIC	07/06	Removed call to ST_NULL!  Adding a null *
C*				is responsibility of the caller from 'C'*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
        CHARACTER*(*)   lutfil
C*
C--------------------------------------------------------------------
C
C*	Check if a file was read.
C
        IF ( imftyp .eq. IFINVD ) THEN
	    iret = -5
	    lutfil = ' '
	ELSE
	    iret = 0
	    lutfil = cmlutf
	ENDIF
C
	RETURN
	END
