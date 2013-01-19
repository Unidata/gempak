	SUBROUTINE SN_WPRT  ( isnfln, part, ihhmm, nlev, data, zwind, 
     +			      iret )
C************************************************************************
C* SN_WPRT								*
C*									*
C* This subroutine writes data to an unmerged sounding data file.  The 	*
C* time and station must both be set before this subroutine is called.	*
C* The station time will be stored if the station time flag, STMFLG,	*
C* was set when the file was created.  This subroutine will only	*
C* write data to an unmerged data file.  The subroutine SN_WDAT must	*
C* be used to write data to a merged file.  The valid part names	*
C* are: TTAA, PPAA, TTBB, PPBB, TTCC, PPCC, TTDD, PPDD, TRPA, TRPC,     *
C* MXWA and MXWC.  The flag, ZWIND, is used only for significant wind   *
C* data (PPBB or PPDD).  If set, the winds are reported on height       *
C* surfaces; otherwise, the report is on pressure surfaces.	        *
C*									*
C* SN_WPRT  ( ISNFLN, PART, IHHMM, NLEV, DATA, ZWIND, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	PART		CHAR*4		Part name			*
C*	IHHMM		INTEGER		Station time (HHMM)		*
C*	NLEV		INTEGER		Number of levels		*
C*	DATA (*)	REAL		Sounding data array		*
C*	ZWIND		LOGICAL		Flag for sig wind in z coord	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*					  -8 = station not set		*
C*				  	 -13 = DM error			*
C*					 -17 = invalid merge type	*
C*					 -22 = invalid part name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 9/87	Added significant winds on pres sfc.	*
C* M. desJardins/GSFC	10/87	Error in combining TTAA & PPAA data	*
C* M. desJardins/GSFC	 4/89	Added ABOVE in call to SN_CMAN		*
C* D. Kidwell/NCEP	 2/01	Removed mand. combine and merge, added  *
C*				check for tropopause part               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'   
C*
	CHARACTER*(*)	part
	REAL 		data (*)
	LOGICAL		zwind
C*
	CHARACTER	pprt*4
	LOGICAL		fix
	REAL		data2 ( 3, LLMXLV )
C*
	INCLUDE		'ERMISS.FNC'
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
C*	Check that this is an unmerged data set.
C
	IF  ( mrgtyp ( isnfln ) )  THEN
	    iret = -17
	    RETURN
	END IF
C
C*	Compute number of data values to write.
C
	CALL ST_LCUC ( part, pprt, ier )
	IF  ( ( pprt .eq. 'TTAA' ) .or. ( pprt .eq. 'TTCC' ) )  THEN
	    ndata = nlev * 6
	  ELSE IF ( ( pprt .eq. 'TRPA' ) .or. ( pprt .eq. 'TRPC' )) THEN
	    ndata = nlev * 5
	  ELSE
	    ndata = nlev * 3
	END IF
C
C*	If this is significant wind data on a pressure surface, set the
C*	first valid pressure to be negative.
C
	fix = .false.
	IF  ( ( ( pprt .eq. 'PPBB' ) .or. ( pprt .eq. 'PPDD' ) ) .and.
     +	      ( .not. zwind ) )  THEN
	    fix = .true.
C
C*	    Move data to second array.
C
	    ipt = 1
	    DO  i = 1, nlev
		DO  j = 1, 3
		    data2 ( j, i ) = data ( ipt )
		    ipt = ipt + 1
		END DO
	    END DO
C
C*	    Make first valid level negative.
C
	    ilev = 1
	    DO WHILE  ( ilev .le. nlev )
		IF  ( .not. ERMISS ( data2 ( 1, ilev ) ) )  THEN
		    data2 ( 1, ilev ) = - data2 ( 1, ilev )
		    ilev = nlev + 1
		  ELSE
		    ilev = ilev + 1
		END IF
	    END DO
	END IF
C	   
C*	Write out the data.
C
	IF  ( fix )  THEN
	    CALL DM_WDTR  ( isnfln, irow, icol, pprt, ihhmm, data2, 
     +			    ndata,  ier )
	  ELSE
	    CALL DM_WDTR  ( isnfln, irow, icol, pprt, ihhmm, data, 
     +			    ndata,  ier )
	END IF
C
C*	Check for errors.
C
	IF  ( ier .eq. -10 )  THEN
	    iret = -22
	  ELSE IF  ( ier .ne. 0 )  THEN
	    iret = -13
	END IF
C*
	RETURN
	END
