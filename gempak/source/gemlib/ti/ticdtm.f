	SUBROUTINE TI_CDTM  ( idate, itime, dattim, iret )
C************************************************************************
C* TI_CDTM								*
C*									*
C* This subroutine converts an integer date (YYMMDD) and time (HHMM) 	*
C* into a standard GEMPAK time.						*
C*									*
C* TI_CDTM  ( IDATE, ITIME, DATTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	IDATE		INTEGER	 	Date (YYMMDD)			*
C*	ITIME		INTEGER	 	Time (HHMM)			*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*	 	GEMPAK time			*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* I. Graffman/RDS	6/86						*
C* M. desJardins/GSFC	6/86	Added error check			*
C************************************************************************
	CHARACTER*(*)	dattim
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	idtarr (1) = idate / 10000
	idtarr (2) = ( idate - idtarr (1) * 10000 ) / 100
	idtarr (3) = MOD  ( idate, 100 )
	idtarr (4) = itime / 100
	idtarr (5) = MOD  ( itime, 100 )
	CALL TI_ITOC  ( idtarr, dattim, iret )
C*
	RETURN
	END
