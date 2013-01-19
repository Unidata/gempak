	SUBROUTINE DG_NRDT  ( ifpn, time, level,ivcord, parm,
     +			      grid, igx, igy, ighd, iret )
C************************************************************************
C* DG_NRDT								*
C*									*
C* This subroutine reads the requested grid from a grid file by calling *
C* GD_RDAT. It also applies any specific functions to the grid, for	*
C* example, adding a column of data or subsetting.			*
C*									*
C* DG_NRDT  ( IFPN, TIME, LEVEL, IVCORD, PARM, GRID, IGX, IGY,		*
C*            IGHD,  IRET )						*
C*									*
C* Input parameters:							*
C*	IFPN  		INTEGER		GDFILE entry position number	*
C*	TIME (2)	CHAR*   	GEMPAK grid date-time		*
C*	LEVEL (2)	INTEGER		GEMPAK grid levle		*
C*	IVCORD		INTEGER		GEMPAK vertical coordinate	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	PARM		CHAR*   	GEMPAK parameter name		*
C*									*
C* Output parameters:							*
C*	GRID (*)       	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points	*
C*	IGHD           	INTEGER		Grid header			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = grid not found		*
C*					-30 = open grid failed		*
C*					-31 = navigation not same	*
C**									*
C* Log:									*
C* R. Tian/SAIC          5/06   Fortran wrapper of DGC_NRDT             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid (*)
	CHARACTER*(*)	time (2), parm
	INTEGER		level (2), ighd (*)
C*
	CHARACTER	time1*(LLMXLN), time2*(LLMXLN), tmpprm*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( time (1), time1, nt, ier )
	CALL ST_NULL ( time (2), time2, nt, ier )
	CALL ST_NULL ( parm, tmpprm, nt, ier )
	CALL DGC_NRDT ( ifpn, time1, time2, level (1), level (2), ivcord,
     +                  tmpprm, grid, igx, igy, ighd, iret )
C*
	RETURN
	END
