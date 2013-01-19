	SUBROUTINE SNSSTM  ( cxstns, isnfln, nparms, msdsiz, times,
     +			     ntime, taxis, ndtim, ipsdat, nlvls, 
     +			     stndat, idtype, sloc, xmin, xmax, xtlbl,
     +			     ctlbl, nxlbl, ixlbfr, ixglfr, ixtmfr, 
     +			     iret )
C************************************************************************
C* SNSSTM								*
C*									*
C* This subroutine gets the stations to analyze for the cross section	*
C* program.								*
C*									*
C* SNSSTM  ( CXSTNS, ISNFLN, NPARMS, MSDSIZ, TIMES, NTIME, TAXIS, 	*
C*           NDTIM, IPSDAT, NLVLS, STNDAT, IDTYPE, SLOC, XMIN, XMAX,	*
C*           XTLBL, CTLBL, NXLBL, IXLBFR, IXGLFR, IXTMFR, IRET )	*
C*									*
C* Input parameters:							*
C*	CXSTNS		CHAR*		User input station list		*
C*	ISNFLN		INTEGER		File number			*
C*	NPARMS		INTEGER		Number of parameters in file	*
C*	MSDSIZ		INTEGER		Size of station data buffer	*
C*	TIMES (NTIME)	CHAR*		Times				*
C*	NTIME		INTEGER		Number of times			*
C*	TAXIS		CHAR*		Input for time axis		*
C*									*
C* Output parameters:							*
C*	NDTIM		INTEGER		Number of stations		*
C*	IPSDAT (NDTIM)	INTEGER		Pointer to data in STNDAT	*
C*	NLVLS  (NDTIM)	INTEGER		Number of levels at station	*
C*	STNDAT (MSDSIZ)	REAL		Station data buffer		*
C*      IDTYPE (LLMXLV,*) INTEGER	Type of level data		*
C*	SLOC   (NDTIM)	REAL		Station locations on axis	*
C*	XMIN		REAL		Minimum value on x axis		*
C*	XMAX		REAL		Maximum value on x axis		*
C*	XTLBL (NXLBL)	REAL		Location of labels		*
C*	CTLBL (NXLBL)	CHAR*		Labels				*
C*	NXLBL		INTEGER		Number of labels		*
C*	IXLBFR		INTEGER		X label frequency		*
C*	IXGLFR		INTEGER		X grid line frequency		*
C*	IXTMFR		INTEGER		X tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = fewer than four stations	*
C*					 -7 = data buffer too small	*
C*					 -9 = stn ... can't be found	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/90	GEMPAK 5				*
C* M. desJardins/GSFC	 3/91	Modified to use taxis to specify axis	*
C* K. Brill/NMC		01/92	Increment NDTIM before CALL SN_RTYP	*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* S. Jacobs/NCEP	 2/99	Fixed error message for missing times	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	cxstns, times (*), taxis, ctlbl (*)
	REAL		stndat (*), sloc (*), xtlbl (*)
	INTEGER		ipsdat (*), nlvls (*), idtype (LLMXLV, *)
C*
	CHARACTER	stnin*8, cid*8, dtim (LLMXTM)*12
C------------------------------------------------------------------------
C*	Break CXSTNS into station array.
C
	CALL ST_LCUC ( cxstns, cxstns, ier )
	CALL ST_CLST ( cxstns, ';', ' ', 1, stnin, nstin, ier )
C
C*	Return error if there is more than one station.
C
	IF  ( stnin .eq. ' ' )  THEN
	    iret = -6
	  ELSE IF  ( ier .ne. 0 )  THEN
	    iret = -16
	  ELSE
	    iret = 0
	END IF
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SNCROSS', iret, cxstns, ier )
	    RETURN
	END IF
C
C*	Read in the data from the stations.
C
	ndtim  = 0
	iptr   = 1
C
C*	Loop through the times.
C
	DO  itime = 1, ntime
C
C*	    Set the next time and station.
C
	    CALL SN_STIM  ( isnfln, times (itime), ier )
	    CALL SN_SSTN  ( isnfln, stnin, cid, idnum, rlat, rlon, elv,
     +			    ier2 )
	    IF  ( ( ier .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
		ier = -17
		CALL ER_WMSG  ( 'SNCROSS', ier, times (itime), ier2 )
	      ELSE
		CALL SN_RDAT  ( isnfln, numlev, stndat (iptr), ihhmm,
     +				ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCROSS', -17, times (itime), ir2 )
		END IF
	    END IF
C*
	    IF  ( ier .eq. 0 )  THEN
		ndtim = ndtim + 1
	        CALL SN_RTYP  ( isnfln, numlev, idtype (1, ndtim), ier )
		dtim (ndtim)  = times (itime)
		nlvls (ndtim)  = numlev
		ipsdat (ndtim) = iptr
		iptr = iptr + numlev * nparms
		IF  ( iptr .gt. MSDSIZ )  THEN
		    iret = -7
		    CALL ER_WMSG ('SNCROSS', iret, ' ', ier )
		    RETURN
		END IF
	    END IF
	END DO
C
C*	Check that at least four valid times were found.
C
	IF  ( ndtim .lt. 4 )  THEN
	    iret = -6
	    CALL ER_WMSG ( 'SNCROSS', iret , ' ', ier )
	    RETURN
	END IF
C
C*	Set up time axis.
C
	CALL IN_TAXS  ( taxis, LLTMCX, ndtim, dtim, sloc, xmin, 
     +		       xmax, xtlbl, ctlbl, nxlbl, xmndst, ixlbfr,
     +		       ixglfr, ixtmfr, ier )
C*
	RETURN
	END
