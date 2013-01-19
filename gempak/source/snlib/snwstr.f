	SUBROUTINE SN_WSTR ( isnfln, part, ihhmm, string, iret )
C************************************************************************
C* SN_WSTR								*
C*									*
C* This subroutine writes a string of data to an unmerged sounding data *
C* file. The time and station must both be set before this subroutine   *
C* is called. The station time will be stored if the station time flag, *
C* STMFLG, was set when the file was created.  The valid part names are *
C* TXTA, TXTB, TXTC and TXPB.                                           *
C*									*
C* SN_WSTR  ( ISNFLN, PART, IHHMM, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	PART		CHAR*4		Part name                       *
C*	IHHMM		INTEGER		Time (HHMM)			*
C*	STRING 		CHAR*		String of data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				          -4 = file not open		*
C*				          -8 = station not set		*
C*				  	 -13 = DM error			*
C*				  	 -17 = invalid merge type       *
C*				  	 -22 = invalid part name        *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01	                                        *
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
C*      Check that this is an unmerged data set.
C
        IF  ( mrgtyp ( isnfln ) )  THEN
            iret = -17
            RETURN
        END IF
C	   
C*	Write out the data.
C
	idthdr (1) = ihhmm
	CALL ST_LSTR  ( string, nchar, ierr )
	CALL DM_WDTC  ( isnfln, irow, icol, part, idthdr, string, 
     +			nchar, ier )
C
C*      Check for errors.
C
        IF  ( ier .eq. -10 )  THEN
            iret = -22
          ELSE IF  ( ier .ne. 0 )  THEN
            iret = -13
        END IF
C*
	RETURN
	END
