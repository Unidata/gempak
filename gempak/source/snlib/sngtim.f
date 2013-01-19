	SUBROUTINE SN_GTIM  ( isnfln, maxtim, ntime, timlst, iret )
C************************************************************************
C* SN_GTIM 								*
C*									*
C* This subroutine returns a list of times available in a sounding 	*
C* data file.  The times are ordered from the earliest to the latest.	*
C*									*
C* SN_GTIM  ( ISNFLN, MAXTIM, NTIME, TIMLST, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*	MAXTIM		INTEGER	 	Maximum number of times		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER	 	Number of times returned	*
C*	TIMLST (NTIME)	CHAR*	 	GEMPAK times			*
C*	IRET		INTEGER	 	Return code			*
C*					   0 = normal return		*
C*					  -4 = file not open		*
C*					 -14 = too many times in file	*
C**									*
C* Log:									*
C* I.Graffman/RDS 	5/87						*
C************************************************************************
	CHARACTER* (*) timlst (*)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get the list of times from the file.
C
	CALL DM_GTIM ( isnfln, maxtim, ntime, timlst, iret )
C
C*	Check for too many times in file.
C
	IF  ( iret .ne. 0 )  iret = -14
C*
	RETURN
	END
