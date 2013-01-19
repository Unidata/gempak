	SUBROUTINE UPDVXY
C************************************************************************
C* UPDVXY								*
C* 									*
C* This subroutine updates the coordinate systems for the V level.	*
C* All higher levels are also updated.					*
C* 									*
C* UPDVXY								*
C**									*
C* Log:									*
C* G.Chatters/RDS	 7/84						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C-------------------------------------------------------------------------
C	Check to see if a device has been set before doing computations.
C
	IF ( ddev .ne. ' ' ) THEN
	   xbndlv = xbndlf * ( xbndrn - xbndln ) + xbndln
	   ybndbv = ybndbf * ( ybndtn - ybndbn ) + ybndbn
	   xbndrv = xbndrf * ( xbndrn - xbndln ) + xbndln
	   ybndtv = ybndtf * ( ybndtn - ybndbn ) + ybndbn
C
C*	Compute and set clipping window positions
C*	Note: INTDLM ( x, ix1, ix2 ) rounds x towards ix1, limit at ix1, ix2
C
	   ixwlv = INTDLM ( andx1 * xbndlv + andx0, ixbndl, ixbndr )
	   iywbv = INTDLM ( andy1 * ybndbv + andy0, iybndb, iybndt )
	   ixwrv = INTDLM ( andx1 * xbndrv + andx0, ixbndr, ixbndl )
	   iywtv = INTDLM ( andy1 * ybndtv + andy0, iybndt, iybndb )
	END IF
C
C*	Update higher coordinate systems.
C
	CALL UPDPXY
C*
	RETURN
	END
