	SUBROUTINE GD_GNUMW  ( iacss, gdattm1, gdattm2, level1, level2,
     +	                       ivcord, parm, ignum, iret )
C************************************************************************
C* GD_GNUMW								*
C*									*
C* This subroutine gets the grid number for the requested grid.		*
C*									*
C* GD_GNUMW  ( IACSS, GDATTM1, GDATTM2, LEVEL1, LEVEL2, IVCORD, PARM,	*
C*             IGNUM, IRET )						*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		File acess number		*
C*	GDATTM1		CHAR*20		GEMPAK times			*
C*	GDATTM2		CHAR*20		GEMPAK times			*
C*	LEVEL1		INTEGER		Vertical levels			*
C*	LEVEL2		INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	IGNUM		INTEGER		Grid number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C*					-12 = grid does not exist	*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdattm1, gdattm2, parm
C*
	CHARACTER	gdattm (2)*20
	INTEGER		level (2)
C------------------------------------------------------------------------
	gdattm (1) = gdattm1
	gdattm (2) = gdattm2
	level (1) = level1
	level (2) = level2
	CALL GD_GNUM ( iacss, gdattm, level, ivcord, parm, ignum, iret )
C*
	RETURN
	END
