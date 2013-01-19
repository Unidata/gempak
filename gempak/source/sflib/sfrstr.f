	SUBROUTINE SF_RSTR ( isffln, string, ihhmm, nchar, iret )
C************************************************************************
C* SF_RSTR								*
C*									*
C* This subroutine reads a string of data from a surface data file.	*
C* The time and	station must be set before calling this subroutine.	*
C*									*
C* SF_RSTR ( ISFFLN, STRING, IHHMM, NCHAR, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String of data			*
C*	IHHMM		INTEGER		Hour and minute			*
C*	NCHAR		INTEGER		Length of string		*
C*	IRET		INTEGER		Return code			*
C*					  1 = no data at station	*
C*					  0 = normal return		*
C*					 -3 = file not open		*
C*					 -7 = location not set		*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/96	Copied from SF_RDAT			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* m.gamazaychikov/SAIC 10/02	Replaced 'CHCR' with 'CHLF' for Linux   *
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
C*	Set time to missing.
C
	idthdr (1) = IMISSD
C
C*	Get the data.
C
	CALL DM_RDTC  ( isffln, krow (isffln), kcol (isffln),
     +			'SFTX', idthdr, string, nchar, ier)
c
        DO i = 1, nchar
           IF (string(i:i).eq.CHCR)  string(i:i)=CHLF
        END DO
c
	IF  ( ier .ne. 0 )  THEN
	    iret = 1
C
C*	    Clear the data buffer.
C
	    string = ' '
	END IF
C
C*	Compute time.
C
	ihhmm = idthdr (1)
C*
	RETURN
	END
