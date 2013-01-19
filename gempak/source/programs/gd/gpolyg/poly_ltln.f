	SUBROUTINE POLY_LTLN ( jtype, grid, igx, igy, iret )
C************************************************************************
C* POLYT_LTLN  								*
C*									*
C* This subroutine saves lat/lon information to common.			*
C*									*
C* POLY_LTLN ( JTYPE, GRID, IGX, IGY, IRET )				*
C*									*
C* Input parameters:							*
C*	JTYPE		INTEGER		Data type			*
C*					 1 = latitude			*
C*					 2 = longitude			*
C*	GRID		REAL		Lat/Lon values			*
C*	IGX		INTEGER		Grid size in x-direction	*
C*	IGY		INTEGER		Grid size in y-direction	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		02/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	REAL		grid (igx*igy)
C-----------------------------------------------------------------------
	iret = 0
C
	igxd = igx
	igyd = igy
	igxyd = igx * igy
	IF  ( jtype .eq. 1 )  THEN
	    DO ii = 1, igxyd
		ggrlat ( ii ) = grid ( ii ) * RTD
	    END DO
	  ELSE
	    DO jj = 1, igxyd
		ggrlon ( jj ) = grid ( jj ) * RTD
	    END DO
	END IF
C*
	RETURN
	END
