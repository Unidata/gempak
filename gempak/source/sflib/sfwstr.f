	SUBROUTINE SF_WSTR ( isffln, ihhmm, string, iret )
C************************************************************************
C* SF_WSTR								*
C*									*
C* This subroutine writes a string of data to a surface data file. The 	*
C* time and station must be set before this subroutine is called. The	*
C* station time will be stored if the station time flag, STMFLG,	*
C* was set when the file was created.					*
C*									*
C* SF_WSTR  ( ISFFLN, IHHMM, STRING, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	IHHMM		INTEGER		Time (HHMM)			*
C*	STRING 		CHAR*		String of data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*					  -7 = location not set		*
C*				  	 -12 = DM error			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/96	Copied from SF_WDAT			*
C* S. Jacobs/NCEP	 6/96	Use idthdr in call to DM_WDTC		*
C* m.gamazaychikov/SAIC 10/02	Replaced 'CHLF' with 'CHCR' for Linux   * 
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
C*	Check that station has been set.
C
	irow = krow (isffln)
	icol = kcol (isffln)
	IF  ( (irow .le. 0) .or. (icol .le. 0) )  THEN
	    iret = -7
	    RETURN
	END IF
C	   
C*	Write out the data.
C
	idthdr (1) = ihhmm
	CALL ST_LSTR  ( string, nchar, ierr )
c
        DO i=1, nchar
           IF (string(i:i).eq.CHLF) string(i:i)=CHCR
        END DO
c
	CALL DM_WDTC  ( isffln, irow, icol, 'SFTX', idthdr, string, 
     +			nchar, ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END
