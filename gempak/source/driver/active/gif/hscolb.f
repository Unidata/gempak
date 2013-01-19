	SUBROUTINE HSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* HSCOLB - GIF 							*
C* 									*
C* This subroutine sets the color in a color bank.			*
C* 									*
C* HSCOLB  ( ICBANK, ICOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank number		*
C*	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Nielsen-G/TAMU	12/96						*
C* T. Lee/GSC		 7/00	Renamed gdr_applyLUT to wscolb		*
C* R. Tian/SAIC		05/02	Added fax image type(3)			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'COLTBL.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'driver.cmn'
C*
	INTEGER		zero, ltz, index
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( ( icbank .lt. 0 ) .OR. ( icbank .gt. 3 ) )  RETURN
C
C*	For graphics color.
C
	IF  ( icbank .eq. 0 )  THEN
	    CALL HSCOLR ( icolr, iret )
	    RETURN
	END IF
C
C*	Make sure plot file is open.
C
	zero = 0
	ltz  = -1
	IF  ( .not. opnfil )  THEN
	    CALL HOPEN  ( iret )
	    IF ( iret .ne. NORMAL )  RETURN
	END IF
C
C*	Determine color index
C
	IF ( icbank .eq. 1 )  THEN
	    index = ibgsat + icolr - 1
	ELSE IF ( icbank .eq. 2 )  THEN
	    index = ibgrad + icolr - 1
	ELSE IF ( icbank .eq. 3 )  THEN
	    index = ibgfax + icolr - 1
	END IF
C
C*	Check bounds
C
	IF ( index .ge. MXCOL ) RETURN
C
C*	Activate color
C
	CALL WSCOLB  ( index, iret )
C
C*	Now that color has been set, reset flag.
C
	resetc = .FALSE.
C*
	RETURN
	END
