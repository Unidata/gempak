	SUBROUTINE MS_CLIM ( stid, climtm, parms, nparm, iflsrc, stntbl,
     +			     iadstn, maxtim, ifcstm, prmscl, nprmcl,
     +			     irhour, rdata, iret ) 
C************************************************************************
C* MS_CLIM								*
C*									*
C* This subroutine reads the climatology data for an GFSX MOS station,  *
C* calculates anomalies, and returns an updated output file including   *
C* both climatology and anomalies to the calling program.               *
C*									*
C* MS_CLIM ( STID, CLIMTM, PARMS, NPARM, IFLSRC, STNTBL, IADSTN,        *
C*	     MAXTIM, IFCSTM, PRMSCL, NPRMCL, IRHOUR, RDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	STID  		CHAR*		Station identifier              *
C*	CLIMTM  	CHAR*		Initial GEMPAK climatology time *
C*	PARMS(*)	CHAR*		GEMPAK parameter list           *
C*	NPARM		INTEGER		Number of parameters            *
C*	IFLSRC		INTEGER		File source			*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Max num of additional stations	*
C*	MAXTIM		INTEGER		Max num of times for land data	*
C*	IFCSTM		INTEGER		Number of forecast times        *
C*	PRMSCL(*)	CHAR*		GEMPAK climate parameter list   *
C*	NPRMCL		INTEGER		Number of climate parameters    *
C*	IRHOUR		INTEGER		Report hour			*
C*									*
C* Input and output parameters:						*
C*	RDATA(IFCSTM,MMPARM)	REAL	On input, forecast data; on     *
C*	  				output, forecast + climo + anom *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                        0 = normal return             *
C*					 -7 = problem getting climo data*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00	From MM_CLIM                            *
C* m.gamazaychikov/SAIC	11/03	Replaced references to AVN/MRF with 	*
C*				references to GFS/GFSX			*
C* m.gamazaychikov/SAIC 07/05	Added irhour to CS, changed defn day00	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, climtm, parms (*), stntbl, prmscl (*)
	REAL		rdata (ifcstm,MMPARM)
C*
	CHARACTER	gemtim*15
	INTEGER		idtarr (5)
	REAL		cldata (MMPARM)
	LOGICAL		good, problm, day00
C*
	PARAMETER	( NUMOUT = 4 )
	PARAMETER	( NUMCLM = 5 )
	CHARACTER	dparms (NUMOUT)*4, cparms (NUMOUT)*4,
     +			aparms (NUMOUT)*4, parmsc (NUMCLM)*4
	INCLUDE		'ERMISS.FNC'
C*
C*	Do not change the order of these data elements.
C*
	DATA		dparms / 'TNTF', 'TDYF', 'PP12', 'PP24'/
	DATA		cparms / 'TNCF', 'TDCF', 'PP1C', 'PP2C'/
	DATA		aparms / 'TNAF', 'TDAF', 'PP1A', 'PP2A'/
	DATA		parmsc / 'TNTF', 'TDYF', 'PPNT', 'PPDY', 'PP24'/
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the climatology file number.
C
	CALL MS_OPNC ( climtm, iflsrc, stntbl, iadstn, maxtim, iflno, 
     +		       iret )
	IF ( iret .ne. 0 ) THEN
	    CALL DC_WLOG ( 2, 'MS', iret, climtm, ier )
	    iret = -7
	    RETURN
	END IF
C
C*	Set the station.
C
	CALL SF_FSTN ( iflno, stid, iret )
	IF ( iret .ne. 0 ) THEN
	    CALL DC_WLOG ( 4, 'SF', iret, stid, ier )
	    iret = -7
	    RETURN
	END IF
C
	gemtim = climtm
	problm = .false.
	IF ( irhour .eq. 0 ) THEN
	   day00  = .false.
	 ELSE IF ( irhour .eq. 12) THEN
	   day00  = .true.
	END IF
C
C*	Loop on the times.
C
	DO i = 1, ifcstm
	    IF ( day00 ) THEN
C
C*	        Increment the day.
C
	        CALL TI_CTOI ( gemtim, idtarr, ier )
	        IF ( ier .eq. 0 ) THEN
		    CALL TI_ADDD ( idtarr, idtarr, ier )
C
C*		    Make sure that the year does not change from 00.
C
		    idtarr (1) = 0
C
		    IF ( ier .eq. 0 ) CALL TI_ITOC ( idtarr, gemtim, 
     +						     ier )
	        END IF
	        IF ( ier .ne. 0 ) THEN
		    CALL DC_WLOG ( 4, 'TI', ier, ' ', ierr )
		    iret = -7
		    RETURN
	        END IF
		day00 = .false.
	      ELSE
		day00 = .true.
	    END IF
C
	    good = .true.
C
C*	    Set the time.
C
	    CALL SF_FTIM ( iflno, gemtim, iret )
	    IF ( iret .eq. -11 ) THEN
C
C*		Get another climatology file.
C
	        CALL MS_OPNC ( gemtim, iflsrc, stntbl, iadstn, maxtim,
     +			       iflno, iret )
		IF ( iret .eq. 0 ) THEN
C
C*		    Set the time and the station for the new file.
C
	            CALL SF_FSTN ( iflno, stid, iret )
	            IF ( iret .ne. 0 ) THEN
	                CALL DC_WLOG ( 4, 'SF', iret, stid, ier )
			good = .false.
	            END IF
	            CALL SF_FTIM ( iflno, gemtim, iret )
	            IF ( iret .ne. 0 ) THEN
	                CALL DC_WLOG ( 4, 'SF', iret, gemtim, ier )
			good = .false.
	            END IF
		  ELSE
	            CALL DC_WLOG ( 2, 'MS', iret, climtm, ier )
	            good = .false.
	        END IF
	      ELSE IF ( iret .ne. 0 ) THEN
	        CALL DC_WLOG ( 4, 'SF', iret, gemtim, ier )
		good = .false.
	    END IF
C
	    IF ( good ) THEN
C
C*	        Read the station data.
C
	        CALL SF_RDAT ( iflno, cldata, ihhmm, iret )
	        IF ( iret .ne. 0 ) THEN
	            CALL DC_WLOG ( 4, 'SF', iret, stid, ier )
	            good = .false.
	        END IF
	    END IF
C
	    IF ( good ) THEN
C
C*	        Find the locations of the data, climatology and
C*		anomaly parameters in the output and climatology arrays.
C
		IF ( day00 ) THEN
		    init = 2
		    incr = 1
		  ELSE
		    init = 1
		    incr = 2
		END IF
		DO j = init, NUMOUT, incr
		    k = j
		    IF ( day00 .and. ( j .gt. init ) ) k = k + 1
		    CALL ST_FIND ( dparms (j), parms,  nparm,  idrdat,
     +				   ier )
		    CALL ST_FIND ( cparms (j), parms,  nparm,  icrdat,
     +				   ier )
		    CALL ST_FIND ( aparms (j), parms,  nparm,  iardat,
     +				   ier )
		    CALL ST_FIND ( parmsc (k), prmscl, nprmcl, icclim,
     +				   ier )
		    IF ( ( idrdat .gt. 0 ) .and. ( icrdat .gt. 0 ) .and.
     +                   ( iardat .gt. 0 ) .and. ( icclim .gt. 0 ) )
     +			 THEN
C 
C*			Store the climatology values.
C
			IF ( .not. ERMISS ( cldata ( icclim ) ) )
     +			     rdata ( i, icrdat ) = cldata ( icclim )
C
C*	                Calculate and store anomalies.
C
		        IF ( .not. ERMISS ( rdata ( i, idrdat ) ) .and.
     +		             .not. ERMISS ( rdata ( i, icrdat ) ) )
     +                       rdata ( i, iardat ) = 
     +			       rdata ( i, idrdat ) - rdata ( i, icrdat )
		    END IF
	        END DO
	      ELSE
		problm = .true.
	    END IF
	END DO
C
	IF ( problm ) iret = -7
C*
	RETURN
	END
