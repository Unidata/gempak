	SUBROUTINE TI_IDTM  ( dattim, idate, itime, iret )
C************************************************************************
C* TI_IDTM								*
C*									*
C* This subroutine converts a standard GEMPAK time into an integer	*
C* date (YYMMDD) and time (HHMM).					*
C*									*
C* TI_IDTM  ( DATTIM, IDATE, ITIME, IRET )				*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		 GEMPAK time			*
C*									*
C* Output parameters:							*
C*	IDATE		INTEGER		Date (YYMMDD)			*
C*	ITIME		INTEGER		Time (HHMM)			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = invalid time		*
C*					  -7 = invalid year		*
C*					  -8 = invalid month		*
C*					  -9 = invalid day		*
C*					 -10 = invalid hour		*
C*					 -11 = invalid minute		*
C**									*
C* Log:									*
C* I. Graffman/RDS	6/86						*
C* M. desJardins/GSFC	6/86	Check for errors			*
C************************************************************************
	CHARACTER* (*)	dattim
C*
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
C*	Separate into components.
C
	CALL TI_CTOI  ( dattim, idtarr, iret )
C
C*	Construct the date and time.
C
	IF  ( iret .eq. 0 )  THEN
	    iyear = MOD (idtarr (1), 100)
	    idate = iyear * 10000 + idtarr (2) * 100 + idtarr (3)
	    itime = idtarr (4) * 100 + idtarr (5)
	  ELSE
	    idate = 0
	    itime = 0
	END IF
C*
	RETURN
	END
