	SUBROUTINE SNLDTA  ( isnfln, times, ntime, vlevel, nlev, lvert,
     +			     levtyp, prmlst, nparms, stnprm, nstnp, 
     +			     luns, nlun, iret )
C************************************************************************
C* SNLDTA								*
C*									*
C* This subroutine reads and lists merged data from the sounding	*
C* file for SNLIST.							*
C*									*
C* SNLDTA  ( ISNFLN, TIMES, NTIME, VLEVEL, NLEV, LVERT, LEVTYP, 	*
C*           PRMLST, NPARMS, STNPRM, NSTNP, LUNS, NLUN, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	TIMES (NTIME)	CHAR*		Times				*
C*	NTIME		INTEGER		Number of times			*
C*	VLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	LEVTYP		INTEGER		Level type			*
C*					  1 = list			*
C*					  2 = range without increment	*
C*	PRMLST (NPARMS)	CHAR*		Level parameters		*
C*	NPARMS		INTEGER		Number of level parameters	*
C*	STNPRM (NSTNP)	CHAR*		Station parameters		*
C*	NSTNP		INTEGER		Number of station parameters	*
C*	LUNS   (NLUN)	INTEGER		Output device LUNs		*
C*	NLUN		INTEGER		Number of output devices	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no data listed		*
C**									*
C* Log:									*
C* S. Lilly/NCEP	 8/10	Copied from SNLIST			*
C* S. Lilly/NCEP	 8/10	Removed printing the list of parms	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmlst (*), stnprm (*), times (*)
	REAL		vlevel (*)
	INTEGER		luns   (*)
C*
	REAL		data ( LLMXDT ), rdata ( LLMXDT )
	CHARACTER	stid*8, cdata ( MMPARM )*8
	LOGICAL		good, stnflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times and stations listing data.
C
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
		stnflg = .false.
		CALL SN_SNXT  ( isnfln, stid, istnm, slat, slon, selv, 
     +				iout )
C
C*		Get the data.
C
		IF  ( iout .eq. 0 )  THEN
		    CALL SN_RDAT  ( isnfln, ndlev, data, ihhmm, ier )
		    IF  ( ier .eq. 0 )  THEN
			good = .true.
			ispri = 0
			CALL PC_SSTN  ( stid, istnm, slat, slon, selv,
     +					ispri, ihhmm, ndlev, ier )
		      ELSE
			good = .false.
		    END IF
		  ELSE
		    good = .false.
		END IF
C
C*		Extract the station data.
C
		IF  ( good .and. ( nstnp .gt. 0 ) )  THEN
		    CALL PC_CMST  ( data, rdata, cdata, ier )
		    CALL SNLWST  ( times (i), stid, istnm, slat, slon,
     +				   selv, ihhmm, luns, nlun, ier )
		    stnflg = .true.
		    CALL SNLWSP  ( stnprm, nstnp, rdata, luns, nlun, 
     +				   ier )
		END IF
C
C*		Extract the level data.
C
		IF  ( good .and. ( nparms .gt. 0 ) .and. 
     +		    ( nlev .gt. 0 ) )  THEN
		    CALL SNLLVD  ( data, ndlev, nparms, vlevel, nlev,
     +				   lvert, levtyp, rdata, mlev, ier )
		    IF  ( mlev .gt. 0 )  THEN
			IF  ( .not. stnflg )  THEN
			    CALL SNLWST  ( times (i), stid, istnm,
     +					   slat, slon, selv, ihhmm,
     +					   luns, nlun, ier )
			END IF
			CALL SNLWLP  ( prmlst, nparms, rdata, mlev, 
     +				       luns, nlun, ier )
		    END IF
		END IF
	    END DO
	END DO
C*
	RETURN
	END
