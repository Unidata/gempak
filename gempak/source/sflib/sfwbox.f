	SUBROUTINE SF_WBOX (isffln, ihhmm, data, iret)
C************************************************************************
C* SF_WBOX								*
C*									*
C* This subroutine writes data to a surface watch-by-county file.  The	*
C* time and station must both be set before this subroutine is called.	*
C* The station time will be stored if the station time flag, STMFLG,	*
C* was set when the file was created.					*
C*									*
C* SF_WBOX  ( ISFFLN, IHHMM, DATA, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	IHHMM		INTEGER		Station time (HHMM)		*
C*	DATA (NPARM)	REAL		Surface data array		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*					  -7 = location not set		*
C*				  	 -12 = DM error			*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97	From SF_WDAT				*
C* T. Piper/GSC		11/98	Fixed typo in prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'   
C*
	REAL 		data (*)
C*
	INTEGER		idthdr (LLSTHL)
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station has been set.
C
	irow = krow (isffln)
	icol = 1
	IF  ( (irow .le. 0) )  THEN
	    iret = -7
	    RETURN
	END IF
C	   
C*	Write out the data.
C
	idthdr (1) = ihhmm
	CALL DM_WDTR  ( isffln, irow, icol, 'CNBX', ihhmm, data, 
     +			kparm (isffln), ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END
