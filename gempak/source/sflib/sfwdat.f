	SUBROUTINE SF_WDAT (isffln, ihhmm, data, iret)
C************************************************************************
C* SF_WDAT								*
C*									*
C* This subroutine writes data to a surface data file.  The time and	*
C* station must both be set before this subroutine is called.  The	*
C* station time will be stored if the station time flag, STMFLG,	*
C* was set when the file was created.					*
C*									*
C* SF_WDAT  ( ISFFLN, IHHMM, DATA, IRET )				*
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
C* I. Graffman/RDS	 5/84						*
C* M. desJardins/GSFC	 2/88	Cleaned up				*
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
	icol = kcol (isffln)
	IF  ( (irow .le. 0) .or. (icol .le. 0) )  THEN
	    iret = -7
	    RETURN
	END IF
C	   
C*	Write out the data.
C
	idthdr (1) = ihhmm
	CALL DM_WDTR  ( isffln, irow, icol, 'SFDT', ihhmm, data, 
     +			kparm (isffln), ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END
