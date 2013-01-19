	SUBROUTINE SN_RSTR ( isnfln, part, string, ihhmm, nchar, iret )
C************************************************************************
C* SN_RSTR								*
C*									*
C* This subroutine reads a string of data from an unmerged sounding     *
C* data file.  The time and station must be set before calling this     *
C* subroutine.	                                                        *
C*									*
C* SN_RSTR ( ISNFLN, PART, STRING, IHHMM, NCHAR, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	PART		CHAR*4		Part name                       *
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String of data			*
C*	IHHMM		INTEGER		Hour and minute			*
C*	NCHAR		INTEGER		Length of string		*
C*	IRET		INTEGER		Return code			*
C*					  1 = no data at station	*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -8 = station not set		*
C*					-13 = DM error                  *
C*					-17 = invalid merge type        *
C*					-22 = invalid part name         *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01	                       			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER*(*)	part, string
C*
	INTEGER		idthdr (LLSTHL)
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station has been set.
C
	irow = krow ( isnfln )
	icol = kcol ( isnfln )
	IF  ( ( irow .le. 0 ) .or. ( icol .le. 0 ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Check that this is an unmerged data set.
C
	IF  ( mrgtyp ( isnfln ) )  THEN
	    iret = -17
	    RETURN
	END IF
C
C*	Set time to missing.
C
	idthdr (1) = IMISSD
C
C*	Get the data.
C
	CALL DM_RDTC  ( isnfln, irow, icol, part, idthdr, string,
     +			nchar, ier )
	IF  ( ier .ne. 0 )  THEN
C
C*	    Clear the data buffer.
C
	    string = ' '
	    IF ( ier .eq. -15 ) THEN
	        iret = 1
	      ELSE IF ( ier .eq. -10 ) THEN
		iret = -22
	      ELSE
		iret = -13
	    END IF
	END IF
C
C*	Compute time.
C
	ihhmm = idthdr (1)
C*
	RETURN
	END
