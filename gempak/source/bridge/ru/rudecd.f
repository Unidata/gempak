	SUBROUTINE RU_DECD  ( isnfln, dattim, istnm, stid, part, itopwn,
     +			      wnknot, ihhmm,  report, lenr, txtflg, 
     +			      xlat, xlon, iwndht, irpnt, iret )
C************************************************************************
C* RU_DECD								*
C*									*
C* This subroutine decodes a single upper air report and writes the	*
C* data to a file.  The header information and DATTIM must be found	*
C* before this subroutine is called.  IRPNT must point to the first	*
C* field after the header.  This subroutine may be used for either	*
C* real-time or archived data.  Note that this subroutine		* 
C* has no ADDSTN option.  Any reporting station which is not		*
C* already in the sounding file will be added to it if there is room.	*
C*									*
C* RU_DECD  ( ISNFLN, DATTIM, ISTNM, STID, PART, ITOPWN, WNKNOT, IHHMM,	*
C*            REPORT, LENR, TXTFLG, XLAT, XLON, IWNDHT, IRPNT, IRET )	*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		Observation time		*
C*	ISTNM		INTEGER		Station number			*
C*	STID		CHAR*		Station identifier		*
C*	PART		CHAR*4		Part name			*
C*	ITOPWN		INTEGER		Top level reporting winds	*
C*	WNKNOT		LOGICAL		Flag for wind in knots		*
C*	IHHMM		INTEGER		Actual hour/minute of report	*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*	TXTFLG		LOGICAL		Flag to save undecoded text     *
C*	XLAT		REAL		Dropsonde latitude              *
C*	XLON		REAL		Dropsonde longitude             *
C*      IWNDHT          INTEGER         Dropsonde wind cutoff value     *
C*									*
C* Input and output parameters:						*
C*	IRPNT		INTEGER		Pointer to next field		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no data found		*
C*					 -4 = error in setting station	*
C*					 -5 = error in setting time	*
C*					xx - 900 = error xx in SN_WPRT	*
C*					xx - 800 see RU_TMST            *
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87						*
C* M. desJardins/GSFC	 4/89	Correction to call to SN_WPRT		*
C* D. Kidwell/NCEP	10/98	Corrected prologue; added -5 return code*
C* D. Kidwell/NCEP	 2/01	Added text output option, trop & max wnd*
C* D. Kidwell/NCEP	 3/01	Added xlat, xlon to call sequences      *
C* S. Chiswell/Unidata	 5/01	Added stid, changed RU_TMST call seq.	*
C* m.gamazaychikov/SAIC 07/05   Added iwndht to CS, code for sfc wind	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, part, report, stid
	LOGICAL		wnknot, txtflg
C*
	CHARACTER	tpart*4, dpart*4
	LOGICAL		zwind, dzwind, drpwnd
	REAL		data  ( 3 * LLMXLV ), wdata ( 3 * LLMXLV ),
     +			tdata ( 3 * LLMXLV ), wxdata ( 3 * LLMXLV ),
     +			ddata ( 3 * LLMXLV )
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	nlev1 = 0
	nlev2 = 0
	nlevt = 0
	nlevw = 0
	nlevd = 0
	drpwnd = .false.
C
C*	Decode data for various parts.
C
	IF  ( part .eq. 'TTAA' )  THEN
	    CALL RU_TTAA  ( report, lenr,  itopwn, wnknot, iwndht, 
     +			    irpnt, istnm, data, nlev1, tdata, nlevt, 
     +			    wxdata, nlevw, drpwnd, iret)
C 
C*          Compare the existing PPBB data to the current TTAA data
C*          to find the non-missing wind with the highest pressure
C 
            IF ( drpwnd ) THEN
	       dpart = 'PPBB'
	       CALL RU_DUPE ( isnfln, dattim, istnm, stid, dpart,
     +		              ihhmm, nlevd, ddata, dzwind, ier )
	       IF ( ier .eq. 1 ) THEN
	          IF ( ( .not. dzwind ) .and. 
     +                 ( ERMISS ( data ( 4 ) ) ) ) THEN
	             CALL RU_DRP2 ( nlev1, data, nlevd, ddata, sfcdir, 
     +                              sfcspd, ier )
	             data ( 4 ) = sfcdir
	             data ( 5 ) = sfcspd
                  END IF
	       END IF
	       drpwnd = .false.
	    END IF
C 
	  ELSE IF  ( part .eq. 'TTBB' )  THEN
	    CALL RU_TTBB  ( report, lenr,  wnknot, iwndht, irpnt, 
     +                      istnm, data, nlev1, wdata,  nlev2, drpwnd,
     +			    iret )
C 
C*          Compare the existing TTAA data to the current TTBB data
C*          to find the non-missing wind with the highest pressure
C 
            IF ( drpwnd ) THEN
	       dpart = 'TTAA'
	       CALL RU_DUPE ( isnfln, dattim, istnm, stid, dpart,
     +		              ihhmm, nlevd, ddata, dzwind, ier )
	       drpwnd = .false.
	       IF ( ier .eq. 1 ) THEN
	          IF ( ERMISS ( ddata ( 4 ) ) )  THEN
	             CALL RU_DRP2 ( nlevd, ddata, nlev2, wdata, 
     +                              sfcdir, sfcspd, ier )
	             ddata ( 4 ) = sfcdir
	             ddata ( 5 ) = sfcspd
	             drpwnd = .true.
	          END IF
	       END IF
	    END IF
C 
	  ELSE IF  ( part .eq. 'TTCC' )  THEN
	    CALL RU_TTCC  ( report, lenr,  itopwn, wnknot, irpnt,  
     +			    data,   nlev1, tdata, nlevt, wxdata, nlevw,
     +			    iret )
C 
	  ELSE IF  ( part .eq. 'TTDD' )  THEN
	    CALL RU_TTDD  ( report, lenr,  wnknot, irpnt, data,  nlev1,
     +			    wdata,  nlev2, iret )
C 
	  ELSE IF  ( part .eq. 'PPAA' )  THEN
	    CALL RU_PPAA  ( report, lenr, wnknot, irpnt, data,  nlev1, 
     +			    iret )
C 
	  ELSE IF  ( part .eq. 'PPBB' )  THEN
	    CALL RU_PPBB  ( report, lenr,  wnknot, irpnt, data, nlev1,
     +			    wdata,  nlev2, iret )
C 
	  ELSE IF  ( part .eq. 'PPCC' )  THEN
	    CALL RU_PPCC  ( report, lenr, wnknot, irpnt, data, nlev1, 
     +			    iret )
C 
	  ELSE IF  ( part .eq. 'PPDD' )  THEN
	    CALL RU_PPDD  ( report, lenr,  wnknot, irpnt, data, nlev1,
     +			    wdata,  nlev2, iret )
	END IF
C
C*	Write the data to the file.
C
	IF  ( ( iret .eq. 0 ) .and. 
     +	      ( ( nlev1 .gt. 0 ) .or. ( nlev2 .gt. 0 )  .or.
     +	      ( ( nlevd .gt. 0 ) .and.  drpwnd ) ) ) THEN
C
C*	    Set correct time and station in sounding file.
C                                          
	    CALL RU_TMST  ( isnfln, dattim, istnm, stid, xlat, xlon, 
     +			    iret )
	    IF  ( iret .ne. 0 )  THEN
		RETURN
	    END IF
C
C*	    Rewrite part of TTAA with updated surface wind information.
C
            IF  ( ( nlevd .gt. 0 ) .and.  drpwnd )  THEN
		zwind = .true.
		CALL SN_WPRT  ( isnfln, dpart, ihhmm, nlevd, ddata,
     +				zwind,  ier )
	    END IF
C
C*	    Write the first report to the file.
C
	    IF  ( nlev1 .gt. 0 )  THEN
		zwind = .true.
		CALL SN_WPRT  ( isnfln, part, ihhmm, nlev1, data, 
     +				zwind,  ier )
C
C*	        Write the tropopause data.
C
		IF ( nlevt .gt. 0 ) THEN
		    IF ( part .eq. 'TTAA' ) THEN
			tpart = 'TRPA'
		      ELSE 
			tpart = 'TRPC'
		    END IF
      		    CALL SN_WPRT ( isnfln, tpart, ihhmm, nlevt, tdata,
     +				   zwind, ier )
		END IF
C
C*	        Write the max wind data.
C
		IF ( nlevw .gt. 0 )  THEN
		    IF ( part .eq. 'TTAA' ) THEN
			tpart = 'MXWA'
		      ELSE
			tpart = 'MXWC'
		    END IF
      		    CALL SN_WPRT ( isnfln, tpart, ihhmm, nlevw, wxdata,
     +				   zwind, ier )
		END IF
C
C*	        Write the text.
C
		IF ( txtflg ) THEN
		    IF ( ( part .eq. 'TTAA' ) .or.
     +			 ( part .eq. 'TTBB' ) .or.
     +		         ( part .eq. 'TTCC' ) .or.
     +			 ( part .eq. 'PPBB' ) ) THEN
			tpart = 'TX' // part ( 2:3)
 			CALL SN_WSTR ( isnfln, tpart, ihhmm, 
     +				       report ( :lenr ), ier )
		    END IF
		END IF
	    END IF
C
C*	    Write the second report to the file.
C
	    IF  ( nlev2 .gt. 0 )  THEN
		zwind = .false.
		IF  ( part .eq. 'TTBB' )  THEN
		    tpart = 'PPBB'
		  ELSE IF  ( part .eq. 'TTDD' )  THEN
		    tpart = 'PPDD'
		  ELSE
		    tpart = part
		END IF
		CALL SN_WPRT  ( isnfln, tpart, ihhmm, nlev2, wdata, 
     +				zwind,  ier )
C
C*	        Write the text.
C
		IF ( txtflg ) THEN
		    IF ( ( part .eq. 'PPBB' ) .or.
     +		         ( part .eq. 'TTBB' .and. nlev1 .le. 0 ) ) THEN
			tpart = 'TXPB' 
 			CALL SN_WSTR ( isnfln, tpart, ihhmm, 
     +				       report ( :lenr ), ier )
		    END IF
		END IF
	    END IF
C
C*	    Set return code if there was a write error.
C
	    IF  ( ier .ne. 0 )  iret = ier - 900
C 
	END IF
C*
	RETURN
	END		
