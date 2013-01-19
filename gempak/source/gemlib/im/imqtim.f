	SUBROUTINE IM_QTIM  ( idate, itime, iret )
C************************************************************************
C* IM_QTIM								*
C*									*
C* This subroutine returns the original image date in (yyyymmdd) and    *
C* image time in (hhmmss).						*
C*									*
C* IM_QTIM  ( IDATE, ITIME, IRET )					*
C*									*
C* Output parameters:							*
C*	IDATE		INTEGER		Image date (yyyymmdd) 		*
C*	ITIME		INTEGER		Image time (hhmmss) 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = Invalid image navigation  *
C**									*
C* Log:									*
C* C. Lin/EAI		 6/98						*
C* T. Piper/GSC		11/98	Updated prolog				*
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
	idate = imdate
	itime = imtime
C*
	RETURN
	END
