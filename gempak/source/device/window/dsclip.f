	SUBROUTINE DSCLIP  ( iwndw, iret )
C************************************************************************
C* DSCLIP								*
C*									*
C* This subroutine saves the clipping bounds to be used in /DVWNDW/.	*
C*									*
C* DSCLIP  ( IWNDW, IRET )						*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Coordinate system number	*
C*					  1 = D/N			*
C*					  2 = V				*
C*					  3 = P 			*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER	Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	5/85	GEMPLT Version 3.1			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set clipping bounds.
C
	IF  ( ( iwndw .ge. 1 ) .and. ( iwndw .le. 3 ) ) THEN
	    icleft = iwleft (iwndw)
	    icbot  = iwbot  (iwndw)
	    icrght = iwrght (iwndw)
	    ictop  = iwtop  (iwndw)
	END IF
C*
	RETURN
	END
