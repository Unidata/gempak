	SUBROUTINE SFXDTA  ( sffile, times, ntime, stnold, stns,
     +			     clear, trange, itrace, mtrace, trace,
     +			     trcur, exit, parms, ntparm, icolor,
     +			     iptprm, prmtyp, range, witnes, nparms,
     +			     data, cdata, iret ) 
C************************************************************************
C* SFXDTA								*
C*									*
C* This subroutine reads the data for a single station from the		*
C* surface file.							*
C*									*
C* SFXDTA  ( SFFILE, TIMES, NTIME, STNOLD, STNS, CLEAR, TRANGE,		*
C*	     ITRACE, MTRACE, TRACE, TRCUR, EXIT, PARMS, NTPARM,		*
C*	     ICOLOR, IPTPRM, PRMTYP, RANGE, WITNES, NPARMS, DATA,	*
C*	     CDATA, IRET )						*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Surface file			*
C*	TIMES (NTIME)	CHAR*		Array of times			*
C*	NTIME		INTEGER		Number of times			*
C*	STNOLD		CHAR*		Old station			*
C*	STNS(*)		CHAR*		Stations			*
C*	CLEAR		LOGICAL		Screen clear flag		*
C*	TRANGE		CHAR*		Time range			*
C*	ITRACE		INTEGER		ith trace to plot		*
C*	MTRACE		INTEGER		Number of traces		*
C*	TRACE (5)	CHAR*		Input for traces		*
C*									*
C* Input and output parameters:						*
C*	TRCUR (5)	CHAR*		Current trace values		*
C*	EXIT		LOGICAL		Exit flag			*
C*									*
C* Output parameters:							*
C*	PARMS (4,2,5)	CHAR*		Parameters			*
C*	NTPARM (2,5)	INTEGER		Number of parameters		*
C*	ICOLOR (4,2,5)	INTEGER		Colors				*
C*	IPTPRM (4,2,5)	INTEGER		Pointers to parameters		*
C*	PRMTYP (4,2,5)	CHAR*		Parameter type			*
C*	RANGE  (2,5)	CHAR*		Input for range			*
C*	WITNES (2,5)	CHAR*		Input for witness lines		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	DATA		REAL		Real data values		*
C*	 (NTIME,NPARMS)							*
C*	CDATA		CHAR*		Character data values		*
C*	 (NTIME,NPARMS)							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no data			*
C*					-10 = invalid station		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 4/90	Rewrote					*
C* K. Brill/EMC		 8/96	area*8 -> area*12 (for 8-char stn IDs)	*
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99	Removed ispri = 0 			*
C* T. Lee/GSC		 5/01	Retrieved data from multiple files	*
C* T. Lee/SAIC		 2/02	Set sffcur = ' '			*
C* T. Piper/SAIC	 4/02	Fixed UMR; Initialized isffln to 0	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, stnold, stns (*), trace (*), trcur (*),
     +			parms (4,2,*), prmtyp (4,2,*), range (2,*),
     +			witnes (2,*), times (*), cdata (NTIME,*),
     +			trange
	INTEGER		icolor (4,2,*), ntparm (2,*), iptprm (4,2,*)
	REAL		data (NTIME, *)
C*
	CHARACTER	arecur*24, area*12, stnout*8, stid*8
	REAL		datain (MMPARM), pcdata (MMPARM)
	CHARACTER	pcchar (MMPARM)*8, filnam*80, sffcur*80,
     +			fname*80
			
	LOGICAL		found, newfil, proces, exit, clear
C------------------------------------------------------------------------
	iret = 0
	isffln = 0
	proces = .true.
	found  = .false.
C
C*	Loop through times finding data.
C
	arecur = ' '
	area   = '@' // stnold 
	sffcur = ' '
	DO  i = 1, ntime
C
C*	    Construct a file name if the template is used in SFFILE.
C
	    CALL FL_MNAM ( times (i), sffile, fname, iret )
C
C*	    Open the surface file.
C
	    CALL FL_MFIL ( fname, times (i), filnam, iret )
	    IF  ( iret .ne. 0 )  CALL ER_WMSG ( 'FL', iret, ' ', ier )
	    CALL SFXFIL ( filnam, sffcur, isffln, newfil, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set the area using the faster subroutine SF_UARE.
C
	    IF  ( proces )  THEN
	        CALL SF_UARE  ( isffln, area, newfil, arecur, stnout, 
     +				ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SF', ier, stnold, ier2 )
		    proces = .false.
		END IF
	    END IF
C
C*	    Get parameter information, then halt processing if there
C*	    is no valid parameter.
C
	    IF ( proces )  THEN
		CALL SFXPRM  (  newfil, mtrace, trace, trcur, parms,
     +				ntparm, icolor, iptprm, prmtyp, range,
     +				witnes, nparms, iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    CALL ER_WMSG ( 'SFGRAM', iret, ' ', ier )
		  ELSE IF ( ( ntparm (1,itrace) + ntparm (2,itrace) )
     +			.eq. 0 )  THEN
		    proces = .false.
		END IF
	    END IF
C
C*	    Give user a chance to exit.
C
	    IF  ( proces .and. (i .eq. 1) .and. (itrace .eq. 1) )  THEN
		CALL SFXDSP   ( filnam, trace, mtrace, stns, ntime,
     +				trange, iret )
		IF  ( iret .ne. 0 )  THEN
		    exit = .true.
		    CALL SF_CLOS ( isffln, ier )
		    RETURN
		END IF
C
C*		Clear the screen if requested.
C
		IF  ( clear )  CALL GCLEAR ( ier )
C
	    END IF
C
C*	    Set the next time.
C
	    IF  ( proces )  THEN 
		CALL SF_STIM  ( isffln, times (i), ier )
		IF  ( ier .ne. 0 ) proces =  .false.
	    END IF
C
	    IF  ( proces )  THEN
		CALL SF_SNXT  ( isffln, stid, istnm, slat, slon, selv, 
     +				ispri, ier )
		IF  ( ier .ne. 0 ) proces =  .false.
	    END IF
C
C*	    Read the data.
C
	    IF  ( proces )  THEN
		CALL PC_SSTN  ( stid, istnm, slat, slon, selv,
     +				ispri, 0, 1, ier )
C
C*		Read the data and get values from PC package.
C
		CALL SF_RDAT  ( isffln, datain, ihhmm, ier )
		IF  ( ier .eq. 0 )  THEN
		    CALL PC_CMVS ( 0., 0, datain, pcdata, pcchar, ier )
		    found = .true.
		    DO  ii = 1, nparms
			data  ( i, ii ) = pcdata (ii)
			cdata ( i, ii ) = pcchar (ii)
		    END DO
		END IF
	    END IF
C
C*	    Set data values to missing.
C
	    IF  ( ier .ne. 0 )  THEN
		DO  ii = 1, nparms
		    data  ( i, ii ) = RMISSD
		    cdata ( i, ii ) = ' '
		END DO
	    END IF
	END DO
C
C*	Check for some data reported.
C
	IF  ( .not. found )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'SFGRAM', iret, stnold, ier )
	END IF
C
C*	Close the file.
C
	CALL SF_CLOS ( isffln, ier )
C*
	RETURN
	END
