	SUBROUTINE SN_RPRT  ( isnfln, part, ihhmm, nlev, data, zwind, 
     +			      iret )
C************************************************************************
C* SN_RPRT								*
C*									*
C* This subroutine reads data from a sounding data file.  This		*
C* subroutine will only read data from an unmerged data file.		*
C* The valid part names are:  TTAA, PPAA, TTBB, PPBB, TTCC, PPCC, TTDD, *
C* PPDD, TRPA, TRPC, MXWA and MXWC.  The flag, ZWIND, is used only for  *
C* significant wind data (PPBB or PPDD).  If set, the winds are reported*
C* on height surfaces; otherwise, the report is on pressure surfaces.	*
C*									*
C* SN_RPRT  ( ISNFLN, PART, IHHMM, NLEV, DATA, ZWIND, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	IHHMM		INTEGER		Station time (HHMM)		*
C*	NLEV		INTEGER		Number of levels		*
C*	DATA (*)	REAL		Sounding data array		*
C*	ZWIND		LOGICAL		Flag for sig wind in z coord	*
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
C* T. Piper/GSC		 3/99	Corrected typo in prolog		*
C* D. Kidwell/NCEP	 2/01	Added check for trop part, init nlev    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'   
C*
	CHARACTER*(*)	part
	REAL 		data (*)
	LOGICAL		zwind
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	nlev = 0
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
C*	Read in the data.
C
	CALL DM_RDTR  ( isnfln, irow, icol, part, ihhmm, data,
     +			ndata,  ier )
C
C*	Check for errors.
C
	IF  ( ier .eq. -10 )  THEN
	    iret = -22
	  ELSE IF  ( ier .ne. 0 )  THEN
	    iret  = +1
	    ndata = 0
	END IF
C
C*	Compute number of levels.
C
	IF  ( ( part .eq. 'TTAA' ) .or. ( part .eq. 'TTCC' ) )  THEN
	    nlev = ndata / 6
	  ELSE IF ( ( part .eq. 'TRPA' ) .or. ( part .eq. 'TRPC' )) THEN
	    nlev = ndata / 5
	  ELSE
	    nlev = ndata / 3
	END IF
C
C*	If this is significant wind data on a pressure surface, the
C*	first valid pressure is negative.
C
	IF  ( ( part .eq. 'PPBB' ) .or. ( part .eq. 'PPDD' ) )  THEN
C
C*	    If first valid level is negative, set ZWIND flag.
C
	    zwind = .true.
	    ilev  = 1
	    knt   = 1
	    DO WHILE  ( ilev .le. nlev )
		IF  ( .not. ERMISS ( data ( knt ) ) )  THEN
		    IF  ( data ( knt ) .lt. 0. ) THEN
			zwind = .false.
			data ( knt ) = - data ( knt )
		    END IF
		    ilev = nlev + 1
		  ELSE
		    ilev = ilev + 1
		    knt  = knt  + 3
		END IF
	    END DO
	END IF
C*
	RETURN
	END
