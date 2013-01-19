	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - GIF 								*
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
C* M. desJardins/GSFC	12/90						*
C* M. desJardins/GSFC	 2/91	Draw dots using HDOTS			*
C* M. desJardins/NMC	 4/91	Draw dots when all points are same	*
C* J. Whistler/SSAI	 6/91	Fixed bad placement of psplot		*
C* T. Lee/GSC		 7/00	Renamed gdr_line to wline		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'driver.cmn'
C*
	INTEGER		ix (*), iy (*)
C*
C------------------------------------------------------------------------
C
C*	Make sure that plot file is open.  Put terminal in vector mode.
C
	IF  ( .not. opnfil )  THEN
	    CALL HOPEN  ( iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	END IF

C*	Set color components if they have changes.
C
	IF  ( resetc )  THEN
	    CALL HSCOLR  ( mcolr, ier )
	END IF
C
C*	Draw line segments.
C
	CALL WLINE ( np, ix, iy, iret )

	gfplot = .true.
C*
	RETURN
	END
