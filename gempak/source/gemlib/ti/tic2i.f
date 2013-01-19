	SUBROUTINE TI_C2I  ( dattim, iyyyy, immdd, ihhmm, iret )
C************************************************************************
C* TI_C2I								*
C*									*
C* This subroutine converts a standard GEMPAK time into three		*
C* integers representing the year, month and day, and hour and		*
C* minute.								*
C*									*
C* Example:  DATTIM = 840317/1200 					*
C*           IYYYY = 1984  IMMDD = 317  IHMM = 1200			*
C*									*
C* TI_C2I  ( DATTIM, IYYYY, IMMDD, IHHMM, IRET )			*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		GEMPAK time 			*
C*									*
C* Output parameters:							*
C*	IYYYY		INTEGER 	Year  (YYYY)			*
C*	IMMDD		INTEGER 	Month and day  (MMDD)		*
C*	IHHMM		INTEGER 	Hour and minute  (HHMM)		*
C*	IRET		INTEGER 	Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* M. Goodman/RDS	 4/84	Original source code			* 
C* M. desJardins/GSFC	 6/84						*
C************************************************************************
	CHARACTER*(*) 	dattim
	INTEGER		idtarr (5)
C-----------------------------------------------------------------------
	iret= 0
C
C*	Break into 5 integers.
C
	CALL TI_CTOI  ( dattim, idtarr, iret )
	IF  ( iret .ne. 0 )  THEN
	    iyyyy = 0
	    immdd = 0
	    ihhmm = 0
	    RETURN
	END IF
C
C*	Form output variables
C
	iyyyy = idtarr (1)
	immdd = idtarr (2) * 100 + idtarr (3)
	ihhmm = idtarr (4) * 100 + idtarr (5)
C*
	RETURN
	END	
