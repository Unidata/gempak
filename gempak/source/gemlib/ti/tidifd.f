	SUBROUTINE TI_DIFD  ( dattm1, dattm2, days, iret )
C************************************************************************
C* TI_DIFD								*
C*									*
C* This subroutine computes the time difference in days between 	*
C* two standard GEMPAK times.  The time difference is time1 - time2.	*
C*									*
C* TI_DIFD  ( DATTM1, DATTM2, DAYS, IRET )				*
C*									*
C* Input parameters:							*
C*	DATTM1		CHAR*		First GEMPAK time		*
C*	DATTM2		CHAR*		Second GEMPAK time		*
C*									*
C* Output parameters:							*
C*	DAYS		REAL  		Difference in days		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				  	-12 = invalid time range	*
C**									*
C* Log:									*
C* M. Goodman/RDS	12/85						*
C* M. desJardins/GSFC	11/87	GEMPAK 4				*
C* M. desJardins/GSFC	 1/89	From TI_DIFF and TI_MDIF		*
C* J. Nielsen/SUNYA      2/91   Changed jday > 59 to imm > 2		*
C* I. Durham/GSC	 9/98   Cleaned up				*
C* I. Durham/GSC	10/98   Made ndays to days and made 1440 a real	*
C************************************************************************
	CHARACTER*(*)	dattm1, dattm2
C------------------------------------------------------------------------
	CALL TI_DIFF ( dattm1, dattm2, nmin, iret )
C
	days = nmin / 1440.
C
	RETURN
	END
