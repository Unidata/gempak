	SUBROUTINE SF_RSPC ( isffln, string, nchar, ihhmm, nrep, iret )
C************************************************************************
C* SF_RSPC								*
C*									*
C* This subroutine reads a set of special reports from a surface	*
C* airways or metar data file.						*
C* The time and	station must be set before calling this subroutine.	*
C*									*
C* SF_RSPC ( ISFFLN, STRING, NCHAR, IHHMM, NREP, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String of data			*
C*	NCHAR		INTEGER		Length of string		*
C*	IHHMM		INTEGER		Hour and minute			*
C*	NREP		INTEGER		Number of special reports	*
C*	IRET		INTEGER		Return code			*
C*					  1 = no data at station	*
C*					  0 = normal return		*
C*					 -3 = file not open		*
C*					 -7 = location not set		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/96	Copied from SF_RSTR			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	string
C*
	INTEGER		idthdr (LLSTHL)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	IF ( (krow (isffln) .le. 0) .or. (kcol (isffln) .le. 0) ) THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Set the number of reports and the time to missing.
C
	idthdr (1) = IMISSD
	idthdr (2) = IMISSD
C
C*	Get the data.
C
	CALL DM_RDTC  ( isffln, krow (isffln), kcol (isffln),
     +			'SFSP', idthdr, string, nchar, ier)
	IF  ( ier .ne. 0 )  THEN
	    iret = 1
C
C*	    Clear the data buffer.
C
	    string = ' '
	END IF
C
C*	Get the number of reports and the time.
C
	nrep  = idthdr (1)
	ihhmm = idthdr (2)
C*
	RETURN
	END
