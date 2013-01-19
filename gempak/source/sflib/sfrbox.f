	SUBROUTINE SF_RBOX  ( isffln, data, ihhmm, iret )
C************************************************************************
C* SF_RBOX								*
C*									*
C* This subroutine reads data from a surface watch-by-county file.  The	*
C* time and station must be set before calling this subroutine.		*
C*									*
C* SF_RBOX  ( ISFFLN, DATA, IHHMM, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	DATA (NPARM)	REAL		Station data			*
C*	IHHMM		INTEGER		Station hour and minute		*
C*	IRET		INTEGER	 	Return code			*
C*				    	   1 = no data at station	*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				   	  -7 = location not set		*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97	From SF_RDAT				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	REAL		data (*)
C*
	INTEGER		idthdr (LLSTHL)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	IF ( (krow (isffln) .le. 0) ) THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Set observation time to missing.
C
	idthdr (1) = IMISSD
C
C*	Get the data.
C
	CALL DM_RDTR  ( isffln, krow (isffln), 1, 'CNBX', idthdr, 
     +			data, nw, ier)
	IF  ( ier .ne. 0 )  THEN
	    iret = 1
C
C*	    Clear the data buffer.
C
	    DO  i = 1, kparm (isffln)
	        data (i) = RMISSD
	    END DO
	END IF
C
C*	Compute station time.
C
	ihhmm = idthdr (1)
C*
	RETURN
	END
