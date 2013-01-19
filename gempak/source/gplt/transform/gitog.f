	SUBROUTINE GITOG  ( np, xm, ym, xg, yg, iret )	
C************************************************************************
C* GITOG								*
C* 									*
C* This subroutine converts points in the grid linear intermediate	*
C* coordinate system into grid coordinates.				*
C* 									*
C* GITOG  ( NP, XM, YM, XG, YG, IRET )					*
C* 									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C* 	XM (NP)		REAL		X input coordinates		*
C*	YM (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XG (NP)		REAL		X output coordinates		*
C* 	YG (NP)		REAL		Y output coordinates		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	6/86	Corrected names of scaling terms for	*
C*				graph transformations			*
C* M. desJardins/GSFC	10/86	Added polar coordinate system		*
C* M. desJardins/GSFC	 2/87	Corrected polar coord when graph not set*
C* M. desJardins/GSFC	 1/88	Missing data fix			*
C* M. desJardins/GSFC	 6/88	Adapted from GMTOG			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xm (*), ym (*), xg (*), yg (*)
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
C*	Convert linear intermediate to grid coordinates.
C
	IF  ( ( igmode .eq. 1 ) .and. ( mgset ) )  THEN
	    DO  i = 1, np
		IF  ( ( .not. ERMISS ( xm (i) ) ) .and.
     +		      ( .not. ERMISS ( ym (i) ) ) )  THEN
		    xg (i) = ( xm (i) - aglx0 ) / aglx1
		    yg (i) = ( ym (i) - agly0 ) / agly1
		END IF
	    END DO
C*
	  ELSE IF  ( ( igmode .eq. 2 ) .and. ( ggset ) )  THEN
	    DO  i = 1, np
		IF  ( ( .not. ERMISS ( xm (i) ) ) .and.
     +		      ( .not. ERMISS ( ym (i) ) ) )  THEN
		    xg (i) = ( xm (i) - gxx0 ) / gxx1
		    yg (i) = ( ym (i) - gyy0 ) / gyy1
		END IF
	    END DO
C*
	  ELSE
	    iret = NIPROJ
	END IF
C*
	RETURN
	END
