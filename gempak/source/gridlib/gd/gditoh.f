	SUBROUTINE GD_ITOH ( gdattm, level, ivcord, parm, ihdarr, iret )
C************************************************************************
C* GD_ITOH								*
C*									*
C* This subroutine creates a GEMPAK 4 grid header array from a grid	*
C* identifier.								*
C*									*
C* GD_ITOH  ( GDATTM, LEVEL, IVCORD, PARM, IHDARR, IRET )		*
C*									*
C* Input parameters:							*
C*	GDATTM (2)	CHAR*20		GEMPAK date/times		*
C*	LEVEL  (2)	INTEGER		Levels				*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	IHDARR (10)	INTEGER		Column header			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 1/88	Added grid forecast time		*
C* M. desJardins/GSFC	 4/89	Rewrote grid time routines		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (*), parm
	INTEGER		level (*), ihdarr (*)
C*
	CHARACTER	parm12*12
	INTEGER		intdtf (3)
C------------------------------------------------------------------------
	iret   = 0
	parm12 = parm
C
C*	Change the first GEMPAK time into three integers and then
C*	convert to two integers.
C
	CALL TG_CTOI  ( gdattm (1), intdtf, ier )
	CALL TG_ITOF  ( intdtf, ihdarr, ier )
C
C*	Change the second GEMPAK time if it is not blank.
C
	CALL TG_CTOI  ( gdattm (2), intdtf, ier )
	CALL TG_ITOF  ( intdtf, ihdarr (3), ier )
C
C*	Move the levels and the vertical coordinate.
C
	ihdarr (5) = level (1)
	ihdarr (6) = level (2)
	ihdarr (7) = ivcord
C
C*	Break the parameter name into three integers.
C
	CALL ST_STOI  ( parm12, 12, nval, ihdarr (8), ier )
C*
	RETURN
	END
