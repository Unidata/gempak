	SUBROUTINE TI_I2C ( iyyyy, immdd, ihhmm, dattim, iret )
C************************************************************************
C* TI_I2C								*
C*									*
C* This subroutine converts three integer time variables containing	*
C* the year, month and day, and hour an minute to a GEMPAK standard	*
C* time.								*
C*									*
C* Example:   IYYYY = 1984   IMMDD = 317   IHHMM = 1200			*
C*           DATTIM = '840317/1200'					*
C*									*
C* TI_I2C  ( IYYYY, IMMDD, IHHMM, DATTIM, IRET )			*
C*									*
C* Input parameters:							*
C*	IYYYY		INTEGER		Year				*
C*	IMMDD		INTEGER 	Month and day as MMDD		*
C*	IHHMM		INTEGER 	Hour and minute as HHMM		*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK time			*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* M. Goodman/RDS	3/84	Original source code			*
C************************************************************************
	CHARACTER*(*)  	dattim
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	idtarr (1) = iyyyy
	idtarr (2) = immdd / 100
	idtarr (3) = MOD (immdd, 100)
	idtarr (4) = ihhmm / 100
	idtarr (5) = MOD (ihhmm, 100)
	CALL TI_ITOC  ( idtarr, dattim, iret )
C*
	RETURN
	END
