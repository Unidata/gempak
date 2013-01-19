	SUBROUTINE SNLPDT  ( isnfln, times, ntime, ipttyp, luns, nlun,
     +			     iret )
C************************************************************************
C* SNLPDT								*
C*									*
C* This subroutine gets data by part from the sounding file.		*
C* The proper subroutines are called to list the data.			*
C*									*
C* SNLPDT  ( ISNFLN, TIMES, NTIME, IPTTYP, LUNS, NLUN, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	TIMES (NTIME)	CHAR*		Times				*
C*	NTIME		INTEGER		Number of times			*
C*	IPTTYP		INTEGER		Unmerged type			*
C*					  1 = man below 100 mb		*
C*					  2 = man & sig below 100 mb	*
C*					  3 = man & sig below & above	*
C*	LUNS   (NLUN)	INTEGER		Output device LUNs		*
C*	NLUN		INTEGER		Number of output devices	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no data listed		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89	Modify to list unmerged data		*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* D. Kidwell/NCEP	 2/01	Added PPAA and PPCC                     *
C* D. Kidwell/NCEP	 3/01	Added tropopause and maximum wind       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	times (*)
	INTEGER		luns  (*)
C*
	REAL		ttaadt (6,LLMXLV), ttbbdt (3,LLMXLV),
     +			ppbbdt (3,LLMXLV), ttccdt (6,LLMXLV),
     +			ttdddt (3,LLMXLV), ppdddt (3,LLMXLV),
     +			ppaadt (3,LLMXLV), ppccdt (3,LLMXLV),
     +			trpadt (5,LLMXLV), trpcdt (5,LLMXLV),
     +			mxwadt (3,LLMXLV), mxwcdt (3,LLMXLV)
	LOGICAL		z, zpb, zpd, prmwrt
	CHARACTER	stid*8
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times and stations listing data.
C
	prmwrt = .false.
	DO  i = 1, ntime
C
C*	    Set the time.
C
	    CALL SN_STIM  ( isnfln, times (i), ier )
C
C*	    Loop through stations.
C
	    iout = 0
	    DO WHILE  ( iout .eq. 0 )
		CALL SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv, 
     +				iout )
C
C*		Get the data.
C
		IF  ( iout .eq. 0 )  THEN
C
C*		    Always read the mandatory data.
C
		    CALL SN_RPRT  ( isnfln, 'TTAA', ihmta, nlevta,
     +				    ttaadt, z, ier )
		    CALL SN_RPRT  ( isnfln, 'PPAA', ihmpa, nlevpa,
     +				    ppaadt, z, ier )
C
C*		    Always read the tropopause data.
C
		    CALL SN_RPRT  ( isnfln, 'TRPA', ihmra, nlevra,
     +				    trpadt, z, ier )
C
C*		    Always read the max wind data.
C
		    CALL SN_RPRT  ( isnfln, 'MXWA', ihmma, nlevma,
     +				    mxwadt, z, ier )
C
C*		    Read data below 100 mb.
C
		    IF  ( ipttyp .ne. 1 )  THEN
			CALL SN_RPRT  ( isnfln, 'TTBB', ihmtb, nlevtb,
     +					ttbbdt, z, ier )
			CALL SN_RPRT  ( isnfln, 'PPBB', ihmpb, nlevpb,
     +					ppbbdt, zpb, ier )
		      ELSE
			nlevtb = 0
			nlevpb = 0
		    END IF
C
C*		    Get data above 100 mb.
C
		    IF  ( ipttyp .eq. 3 )  THEN
			CALL SN_RPRT  ( isnfln, 'TTCC', ihmtc, nlevtc,
     +					ttccdt, z, ier )
			CALL SN_RPRT  ( isnfln, 'PPCC', ihmpc, nlevpc,
     +					ppccdt, z, ier )
			CALL SN_RPRT  ( isnfln, 'TTDD', ihmtd, nlevtd,
     +					ttdddt, z, ier )
			CALL SN_RPRT  ( isnfln, 'PPDD', ihmpd, nlevpd,
     +					ppdddt, zpd, ier )
		        CALL SN_RPRT  ( isnfln, 'TRPC', ihmrc, nlevrc,
     +				        trpcdt, z, ier )
		        CALL SN_RPRT  ( isnfln, 'MXWC', ihmmc, nlevmc,
     +				        mxwcdt, z, ier )
		      ELSE
			nlevtc = 0
			nlevpc = 0
			nlevtd = 0
			nlevpd = 0
			nlevrc = 0
			nlevmc = 0
		    END IF
C
C*		    Check to see if there is any data.
C
		    n = nlevta + nlevtb + nlevpb + nlevtc + nlevtd +
     +			nlevpd + nlevpa + nlevpc + nlevra + nlevma +
     +			nlevrc + nlevmc
		    IF  ( n .gt. 0 )  THEN
			prmwrt = .true.
			CALL SNLWST  ( times (i), stid, istnm, slat, 
     +				       slon, selv, IMISSD, luns, 
     +				       nlun, ier )
			IF  ( ( nlevta .gt. 0 ) .or. ( nlevpa .gt. 0 ) )
     +			        CALL SNLWMN  ( ttaadt, nlevta, ihmta,
     +					       ppaadt, nlevpa, ihmpa, 
     +					       .false., luns, nlun, 
     +					       ier )
			IF  ( ( nlevra .gt. 0 ) .or. ( nlevma .gt. 0 ) )
     +				CALL SNLWTM  ( trpadt, nlevra, ihmra,
     +					       mxwadt, nlevma, ihmma,
     +					       .false., luns, nlun,
     +					       ier )
			IF  ( ( nlevtb .gt. 0 ) .or. ( nlevpb .gt. 0 ) )
     +				CALL SNLWSG  ( ttbbdt, nlevtb, ihmtb,
     +					       ppbbdt, nlevpb, ihmpb,
     +					       zpb, .false., luns, nlun,
     +					       ier )
			IF  ( ( nlevtc .gt. 0 ) .or. ( nlevpc .gt. 0 ) )
     +			        CALL SNLWMN  ( ttccdt, nlevtc, ihmtc, 
     +					       ppccdt, nlevpc, ihmpc,
     +					       .true., luns, nlun, ier )
			IF  ( ( nlevrc .gt. 0 ) .or. ( nlevmc .gt. 0 ) )
     +				CALL SNLWTM  ( trpcdt, nlevrc, ihmrc,
     +					       mxwcdt, nlevmc, ihmmc,
     +					       .true., luns, nlun, ier )
			IF  ( ( nlevtd .gt. 0 ) .or. ( nlevpd .gt. 0 ) )
     +				CALL SNLWSG  ( ttdddt, nlevtd, ihmtd,
     +					       ppdddt, nlevpd, ihmpd,
     +					       zpd, .true., luns, nlun,
     +					       ier )
		    END IF
		END IF
	    END DO
	END DO
C
C*	Check whether any data was listed.
C
	IF  ( .not. prmwrt )  iret = -9
C*
	RETURN
	END
