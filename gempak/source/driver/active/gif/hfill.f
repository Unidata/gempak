	SUBROUTINE HFILL  ( np, ix, iy, iret )
C************************************************************************
C* HFILL - GIF 								*
C*									*
C* This subroutine fills a polygon on a graphics device.		*
C*									*
C* HFILL  ( NP, IX, IY, IRET )						*
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
C* J. Whistler/SSAI	10/91						*
C* M. desJardins/NMC	12/91	Buffer output; use symbolic commands	*
C* T. Lee/GSC		 7/00	Renamed gdr_fill to wfill & added iret	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'driver.cmn'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Make sure that plot file is open.  Put terminal in vector mode.
C
	IF  ( .not. opnfil )  THEN
	    CALL HOPEN  ( iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	END IF
	gfplot = .true.
C
C*      Set color components if they have changes.
C
        IF  ( resetc )  THEN
            CALL HSCOLR  ( mcolr, ier )
        END IF
C
C*	Draw the filled polygon.
C
	CALL WFILL ( np, ix, iy, iret )
C*
	RETURN
	END
