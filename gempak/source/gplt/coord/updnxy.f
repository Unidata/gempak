	SUBROUTINE UPDNXY
C************************************************************************
C* UPDNXY								*
C* 									*
C* This subroutine updates the coordinate systems for the N level.	*
C* All higher levels are also updated.					*
C* 									*
C* UPDNXY								*
C**									*
C* Log:									*
C* G.Chatters/RDS	 7/84						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C-------------------------------------------------------------------------
C*	Check to see if a device is set before doing computations.
C
	IF ( ddev .ne. ' ' ) THEN
	   xbndln = 0.0
	   ybndbn = 0.0
	   zeta = aspect * ABS ( ( ybndtd - ybndbd ) /
     +                                 ( xbndrd - xbndld ) )
	   IF ( zeta .EQ. 1.0 ) THEN
	      xbndrn = 1.0
	      ybndtn = 1.0
	   ELSE IF ( zeta .LT. 1.0 ) THEN
	      xbndrn = 1.0
	      ybndtn = zeta
	   ELSE IF ( zeta .GT. 1.0 ) THEN
	      xbndrn = ( 1.0 / zeta )
	      ybndtn = 1.0
	   END IF
C
C*	   Compute N to D coordinate transform coefficients.
C
	   andx1 = ( xbndrd - xbndld ) / ( xbndrn - xbndln )
	   andx0 = xbndld - xbndln * andx1
	   andy1 = ( ybndtd - ybndbd ) / ( ybndtn - ybndbn )
	   andy0 = ybndbd - ybndbn * andy1
C
C*	   Compute and set clipping window boundaries.
C
	   ixwln = ixbndl
	   iywbn = iybndb
	   ixwrn = ixbndr
	   iywtn = iybndt
	END IF
C
C*	Update higher coordinate systems.
C
	CALL UPDVXY
C*
	RETURN
	END
