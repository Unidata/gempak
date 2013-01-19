	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - RBK								*
C*									*
C* This subroutine draws lines on a graphics device.			*
C*									*
C* HLINE  ( NP, IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC          9/98   Modified from utf's HLINE; Added do loop*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		ix (*), iy (*) 
        INTEGER         nbop
C------------------------------------------------------------------------
        nbop = ( np / 4090 ) + 1
C       
        DO i = 1, nbop
	    CALL ALINE ( np, ix, iy, iret )
        END DO
C*
	RETURN
	END
