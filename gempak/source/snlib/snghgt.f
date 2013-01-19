	SUBROUTINE SN_GHGT ( isnfln, nparms, dattim, rlat, rlon, tmpk, 
     +			     mxpt, maxdst, tblflg, mstflg, clmflg,
     +			     stid, dist, pres, hght, npt, iret )
C************************************************************************
C* SN_GHGT								*
C*									*
C* This subroutine finds the pressure and height of temperature, TMPK,	*
C* from sounding data.  If TBLFLG is .TRUE., a sounding table is used;	*
C* if MSTFLG is .TRUE., a moist-adiabatic cloud height is computed;	*
C* if CLMFLG is .TRUE., a climatology table is used to compute the	*
C* cloud heights.  If cloud heights cannot be found by stardard		*
C* sounding search, the moist-adiabatical method is employed. If no	*
C* cloud height can be found, it will go back 12h of the sounding data	*
C* and find the cloud height.						*
C*									*
C* SN_GHGT  ( ISNFLN, NPARMS, DATTIM, RLAT, RLON, TMPK, MXPT, MAXDST, 	*
C*	      TBLFLG, MSTFLG, CLMFLG, STID, DIST, PRES, HGHT, NPT,	*
C*	      IRET )							*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file no		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	DATTIM		CHAR*		GEMPAK data/time		*
C*	RLAT		REAL		Latitude of the input point	*
C*	RLON		REAL		Longitude of the input point	*
C*	TMPK		REAL		Temperature in K		*
C*	MXPT		INTEGER		Maximum intersection points 	*
C*	MAXDST		INTEGER		Maximum search distance	(meters)*
C*	TBLFLG		LOGICAL		Sounding table flag		*
C*	MSTFLG		LOGICAL		Moist-adiabatic cloud flag	*
C*	CLMFLG		LOGICAL		Climatology table flag		*
C*									*
C* Output parameters:							*
C*	STID		CHAR*		Station closest the the point	*
C*	DIST		REAL		Dist between the pt and the stn	*
C*	PRES (NPT)	REAL		Pressure in mb			*
C*	HGHT (NPT)	REAL		Height in meters		*
C*	NPT		INTEGER		No of intersections		*
C*	IRET		INTEGER		Return code			*
C*					+10 = climatology data is used	*
C*					 +8 = moist-adiabatic height	*
C*					 +3 = too many intersections	*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-30 = cannot compute moist-	*
C*					      adiabatic cloud height	*
C**									*
C* Log:									*
C* T. Lee/GSC		 8/99	Created					*
C* T. Lee/GSC		 9/99	Checked no of intersections		*
C* T. Lee/GSC		11/99	Added max search distance and MAXSTN	*
C* T. Lee/GSC		12/99	Added sounding table flag		*
C* T. Lee/GSC		 1/00	Added moist-adiabatic cloud height	*
C* T. Lee/GSC		 4/00	Modulized moist-adiabatic cloud height	*
C* T. Lee/GSC		 5/00	Added climatology table			*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* T. Lee/GSC		 4/01	Used station number for station w/o ID	*
C* T. Lee/SAIC		 4/02	Fixed uninitialized memory read		*
C* T. Lee/SAIC		 4/03	Used T-12h data instead of climatology	*
C* T. Lee/SAIC		 5/03	Initialized variables			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( JPRE=1, JTMP=2, JDWP=3, JHGT=6 )
	PARAMETER	( MAXSTN=100, NPMS=7, P600=600., P500=500. )
	CHARACTER*(*)	dattim, stid
	REAL		pres (*), hght (*)
C*
	CHARACTER	stn (LLSTFL)*8, stntbl*20, stnam*8, savstn*8
	CHARACTER	tbfil*72, parms(MMPARM)*4, cstn*8
	INTEGER		istn (LLSTFL)
	REAL		dd (LLSTFL), data (LLMXDT), 
     +			datout (LLMXLV, MMPARM)
	REAL		datlev (MMPARM), outdat (MMPARM), 
     +			adata  (MMPARM), bdata  (MMPARM)
	LOGICAL		proces, found, intflg (7), angflg (7)
	LOGICAL		tblflg, mstflg, clmflg, savflg
	INCLUDE		'ERMISS.FNC'
	DATA		intflg / 7*.true. /, angflg / 7*.false./
C*
C------------------------------------------------------------------------
C
C*	Initialize the output.
C
	stid = ' '
	dist = RMISSD
	DO i = 1, mxpt
	    pres ( i ) = RMISSD
	    hght ( i ) = RMISSD
	END DO
C
C*	Cannot compute moist-adiabatic cloud height with table data.
C
	IF  ( ( tblflg .or. clmflg ) .and. mstflg )  THEN
	    iret = -30
	    RETURN
	  ELSE 
	    IF ( tblflg .or. clmflg )  THEN
		nstn = 1
		dist = 0.
		IF  ( tblflg )  THEN
		    stid = 'STANDARD'
		  ELSE
		    stid = 'CLIMATOLOGY'
		END IF
	    END IF
	END IF
C
C*	Check file.
C
	IF  ( tblflg .or. clmflg )  THEN
	    IF  ( isnfln .lt. 1 )  THEN
		iret = -4
		RETURN
	    END IF
	  ELSE
	    CALL SN_CHKF ( isnfln, iret ) 
	    IF  ( iret .ne. 0 )  RETURN
C
C*	    Compute the distance between the input point and the 
C*	    sounding stations.
C
	    stntbl = 'snstns.tbl'
	    CALL LC_DIST  ( stntbl, rlat, rlon, MAXSTN, maxdst, stn,
     +			    istn, dd, nstn, iret )
C
C*	    Set the time.
C
	    CALL SN_STIM  ( isnfln, dattim, iret )
	END IF
C
C*	Convert temperature to Celsius.
C
 	tmpc = PR_TMKC ( tmpk )
C
C*	Initialize buffer for the nearest station.
C
	savstn = ' '
	savflg = .false.
	savdst = RMISSD
	tsave  = RMISSD
C
C*	Read the data.
C
	found  = .false.
	proces = .false.
	k    = 1
	npt  = 0
C
	DO WHILE  ( ( .not. found ) .and. ( k .le. nstn ) )
C
C*	    Read data from standard sounding table.
C
	    IF  ( tblflg )  THEN
		CALL SN_RTBL ( isnfln, nparms, data, nlev, iret )
		IF  ( iret .ne. 0 )  RETURN
		istnm = 999999
		selv  = 0.
		CALL PC_SSTN  ( stid, istnm, rlat, rlon, selv,
     +				ispri, ihhmm, nlev, iret )
C
C*	      Get cloud height from climatology.
C
	      ELSE IF  ( clmflg )  THEN
		CALL SN_GCLM  ( isnfln, nparms, dattim, rlat, tmpk,
     +				data, h, p, iret )
		IF  ( iret .eq. 0 ) THEN
		    npt = 1
		    pres ( npt ) = p
		    hght ( npt ) = h
		END IF
		RETURN
	      ELSE
C
C*	      Read data from a sounding file.
C
		stid   = stn  ( k )
		IF  ( stid .eq. ' ' )  THEN
		    CALL ST_INCH ( istn (k), cstn, ier )
		    stid = cstn
		END IF
		dist   = dd   ( k )
C
C*		Set station.
C
		CALL SN_SSTN  ( isnfln, stid, stnam, istnm, slat, slon, 
     +				selv, iret )
		IF  ( iret .eq. 0 )  THEN
      		    CALL SN_RDAT  ( isnfln, nlev, data, ihhmm, iret ) 
      		    CALL PC_SSTN  ( stid, istnm, slat, slon, selv,
     +				    ispri, ihhmm, nlev, iret )
		END IF
	    END IF
C
C*	    Save the station information.
C
	    IF  ( iret .eq. 0 )  THEN
		proces = .true.
		IF  ( .not. savflg )  THEN
		    CALL PC_FTOP ( data, nparms, levtop, outdat, ier )
		    IF  ( .not. ERMISS ( outdat (JTMP)  ) )  THEN
			tsave  = outdat (JTMP)
			savflg = .true.
			savstn = stid
			savdst = dist
		    END IF
		END IF
	    END IF
C
C*	    Moist-adiabatic cloud height methodology: Find the most
C*	    unstable level below 600 mb. Lift the parcel to 500 mb.
C*	    If the lifted index is less than 0, (air parcel is warmer
C*	    than the environment), continue lifting the parcel 
C*	    moist-adiabatically until the parcel temperature is cooled 
C*	    off to the cloud temperature, TMPK.
C
	    IF  ( proces .and. mstflg )  THEN
		CALL SN_MGHT  ( data, nparms, tmpk, pres, hght, npt,
     +				iret )
		IF  ( iret .ge. 0 )  THEN
		    found = .true.
		    RETURN
		END IF
C
	      ELSE 
C
		IF  ( proces .and. ( nlev .gt. 1 ) )  THEN
C
C*		    Store data below 20 mb only into the buffer.
C
		    DO i = 1, NPMS
			outdat ( i ) = RMISSD
		    END DO
C
		    jtop = 0
C
		    DO  num = 1, nlev
			CALL PC_GLEV  ( num, data, nparms, 
     +					datlev, ier )
			CALL PC_COMP ( 5, datlev, outdat, ier )
			pp = outdat ( JPRE )
			tt = outdat ( JTMP )
			hh = outdat ( JHGT )
			IF  ( ERMISS ( pp ) .or. ERMISS ( tt ) .or. 
     +			      ERMISS ( hh ) .or. 
     +			    ( ( pp .lt. 20.) .and. ( .not. tblflg ) ) )
     +			    THEN
C
			  ELSE
			    jtop = jtop + 1
			    DO np = 1, NPMS
				datout ( jtop, np ) = outdat ( np )
			    END DO
			END IF
		    END DO
C
		    jbot = jtop
C
C*		    Search cloud temperature top down from the sounding
C*		    data or table.
C
		    tmax = RMISSD
		    DO WHILE ( jbot .ge. 2 )
			tmax = AMAX1 ( tmax, datout ( jbot, JTMP ) )
			jbot = jbot - 1
			sign =  ( tmpc - datout ( jtop, JTMP ) ) * 
     +				( tmpc - datout ( jbot, JTMP ) )
C
			IF  ( sign .le. 0. )  THEN
			    found = .true.
C
C*			    Check if SIGN .eq. 0.  Else, interpolation 
C*			    with respect to temperature.
C
			    IF  ( sign .eq. 0. )  THEN
				IF ( tmpc .eq. datout(jtop, JTMP) ) THEN
				    IF  ( npt .ge. mxpt )  THEN
					iret = +3
					RETURN
				      ELSE
					npt = npt + 1
					pres (npt) = datout (jtop, JPRE)
					hght (npt) = datout (jtop, JHGT)
				    END IF
				END IF
C
			      ELSE
				jb = jbot
				jt = jb + 1
				DO i = 1, NPMS
				    adata ( i ) = datout ( jt, i )
				    bdata ( i ) = datout ( jb, i )
				END DO
				CALL PC_INTT  ( tmpc, adata, bdata,
     +						NPMS, intflg, angflg,
     +						JTMP, outdat, iret )
				IF  ( iret .eq. 0 )  THEN 
				    IF  ( npt .ge. mxpt )  THEN
					iret = +3
					RETURN
				      ELSE
					npt = npt + 1
					pres ( npt ) = outdat ( JPRE ) 
					hght ( npt ) = outdat ( JHGT ) 
				    END IF
				END IF
			    END IF
C
C*			    Reassign top level data.
C
			    jtop = jbot
			    DO i = 1, NPMS
		       		datout ( jtop, i ) = datout ( jbot, i ) 
			    END DO
C
			END IF
C
		    END DO
C
C*		    Check the lowest level data.
C
		    tmax = AMAX1 ( tmax, datout ( 1, JTMP ) )
		    IF  ( proces .and. ( tmpc .eq. datout( 1, JTMP ) ) )
     +			  THEN
			found = .true.
			IF  ( npt .ge. mxpt )  THEN
			    iret = +3
			    RETURN
		          ELSE
			    npt   = npt + 1
			    pres ( npt ) = datout ( jbot, JPRE )
			    hght ( npt ) = datout ( jbot, JHGT )
			END IF
		    END IF
C
C*		    Per AWC's request, if cloud height is not found, 
C*		    use moist adiabatic method to find cloud height.
C*		    If found, send a return code for status message.
C
		    IF  ( ( .not. found ) .and. ( .not. tblflg ) )  THEN
			CALL SN_MGHT  ( data, nparms, tmpk, pres, 
     +					hght, npt, iret )
			IF  ( iret .ge. 0 )  THEN
			    found = .true.
			    iret  = +8
			    RETURN
			END IF
		    END IF
C
		END IF
	    END IF
C
	    IF  ( .not. found )  k = k + 1
C
	END DO
C
C*	If no station is found, climatological data table will be used for 
C*	standard atmospheric search.  For nearest sounding search an error 
C*	code will be returned.
C
	IF  ( .not. found )  THEN
	    IF   ( tblflg )  THEN
		tbfil = 'climate.tbl'
		CALL SN_OPNT ( tbfil, lun, parms, nparms, iret )
		IF  ( ( iret .ne. 0 ) .or. ( nparms .lt. 3 ) )  THEN
		    RETURN
		  ELSE
		    CALL SN_GCLM  ( lun, nparms, dattim, rlat, tmpk,
     +				    data, h, p, iret )
		END IF
C
C*		Write status message for climatology.
C
		IF  ( .not. savflg )  THEN
		    stid = 'CLIMATOLOGY'
		    dist = 0.
		  ELSE
		    stid = savstn
		    dist = savdst
		END IF
C
		IF  ( iret .eq. 0 )  THEN
		    npt = 1
		    pres ( npt ) = p
		    hght ( npt ) = h
		    iret = +10
		  ELSE
		    npt = 0
		END IF
		CALL FL_CLOS ( lun, ier )
	      ELSE 
		stid = savstn
		dist = savdst
		npt = 0
C
C*		Check if cloud temperature is warmer than the entire sounding
C*		data.
C
		IF  ( .not. ERMISS (tmax) .and. .not. ERMISS ( dist ) 
     +		    .and.  (tmpc .gt. tmax) .and. ( nstn .gt. 0 ) ) THEN
		    iret = + 6
		  ELSE
		    iret = -33
		END IF
	    END IF 
	END IF
C*
	RETURN
	END
