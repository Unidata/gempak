	SUBROUTINE SN_WDAT  ( isnfln, ihhmm, nlev, data, iret )
C************************************************************************
C* SN_WDAT								*
C*									*
C* This subroutine writes data to a sounding data file.  The time	*
C* and station must both be set before this subroutine is called.	*
C* The station time will be stored if the station time flag, STMFLG,	*
C* was set when the file was created.  This subroutine will only	*
C* write data to a merged data file.  The subroutine SN_WPRT must be	*
C* used to write data to an unmerged file.				*
C*									*
C* SN_WDAT  ( ISNFLN, IHHMM, NLEV, DATA, IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	IHHMM		INTEGER		Station time (HHMM)		*
C*	NLEV		INTEGER		Number of levels		*
C*	DATA (*)	REAL		Sounding data array		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*					  -8 = station not set		*
C*				  	 -13 = DM error			*
C*					 -17 = invalid merge type	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'   
C*
	REAL 		data (*)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 )  RETURN
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
C*	Check that this is merged data.
C
	IF  ( .not. mrgtyp ( isnfln ) )  THEN
	    iret = -17
	    RETURN
	END IF
C	   
C*	Write out the data.
C
	ndata = nlev * kparm ( isnfln )
	CALL DM_WDTR ( isnfln, irow, icol, 'SNDT', ihhmm, data, ndata,
     +		       ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -13
	END IF
C*
	RETURN
	END
