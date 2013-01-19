	SUBROUTINE DSDARR ( szdarw, szdarh, idarwd, idartp, size, 
     +                      sizehd, jdarwd, jdartp, iret )
C************************************************************************
C* DSDARR								*
C* 									*
C* This subroutine sets the directional arrow size/width multipliers and*
C* the arrow type.  If these parameters are not positive, they are not	*
C* changed.								*
C* 									*
C* DSDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, SIZE, SIZEHD, JDARWD, 	*
C*           JDARTP, IRET )						*
C*                                                                    	*
C* Input parameters:							*
C* 	SZDARW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*	SZDARH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*	IDARWD		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C*	IDARTP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C*	SIZEHD		REAL		Arrow head size			*
C* 	JDARWD		INTEGER		Arrow width			*
C*	JDARTP		INTEGER		Arrow type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set arrow size, head size, width, and type in active common 
C*      area.
C
	IF ( szdarw .gt. 0. ) twdasz = szdarw
        IF ( szdarh .gt. 0. ) tdahsz = szdarh
	IF ( idarwd .gt. 0  ) mdarwd = idarwd
	IF ( idartp .gt. 0  ) mdartp = idartp
	size = twdasz
	sizehd = tdahsz
	jdarwd = mdarwd
	jdartp = mdartp
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSWIND ( 3, size, jdarwd, jdartp, sizehd, iret )
	END IF
C*
	RETURN
	END
