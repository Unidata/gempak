	SUBROUTINE GD_HTOI ( ihdarr, gdattm, level, ivcord, parm, iret )
C************************************************************************
C* GD_HTOI								*
C*									*
C* This subroutine creates a grid identifier from a GEMPAK 4 grid 	*
C* header array.							*
C*									*
C* GD_HTOI  ( IHDARR, GDATTM, LEVEL, IVCORD, PARM, IRET )		*
C*									*
C* Input parameters:							*
C*	IHDARR (10)	INTEGER		Column header			*
C*									*
C* Output parameters:							*
C*	GDATTM (2)	CHAR*20		GEMPAK date/times		*
C*	LEVEL  (2)	INTEGER		Levels				*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 1/88	Added code for forecast times		*
C* M. desJardins/GSFC	 4/89	Rewrote grid time modules		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (*), parm
	INTEGER		level (*), ihdarr (*)
C*
	INTEGER		intdtf (3)
C------------------------------------------------------------------------
	iret = 0
C
C*	Change the two integer time into a three integer time and then
C*	into a character GEMPAK grid time.
C
	CALL TG_FTOI  ( ihdarr, intdtf, ier )
	CALL TG_ITOC  ( intdtf, gdattm (1), ier )
C
C*	Change the second GEMPAK time.
C
	CALL TG_FTOI  ( ihdarr (3), intdtf, ier )
	CALL TG_ITOC  ( intdtf, gdattm (2), ier )
C
C*	Move the levels and the vertical coordinate.
C
	level (1) = ihdarr (5) 
	level (2) = ihdarr (6) 
	ivcord    = ihdarr (7) 
C
C*	Change the last three integers into the parameter name.
C
	CALL ST_ITOS  ( ihdarr (8), 3, nchar, parm, ier )
C*
	RETURN
	END
