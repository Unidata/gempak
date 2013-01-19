	SUBROUTINE G2T_DRIV ( ktype, nz, ns, nt, time, gridu, gridv, 
     +			      igx, igy, iret )
C************************************************************************
C* G2T_DRIV	  							*
C*									*
C* This subroutine computes wind/wave extreme values from the raw data.	*
C* Results are stored in the common.					*
C*									*
C* G2T_DRIV ( KTYPE, NZ, NS, NT, TIME, GRIDU, GRIDV, IGX, IGY, IRET )	*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = scalar			*
C*					 2 = vector			*
C*	NZ		INTEGER		Nth zone area			*
C*	NS		INTEGER		Nth subzone			*
C*	NT		INTEGER		Nth grid time			*
C*	TIME		CHAR*		Grid time			*
C*	GRIDU		REAL		Scalar grid or x-direction grid	*
C*	GRIDV		REAL		Y-direction grid		*
C*	IGX		INTEGER		Grid size in x-direction	*
C*	IGY		INTEGER		Grid size in y-direction	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-7 = fail to compute field	*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		11/07	Clockwise wind direction		*
C* T. Lee/SAIC		11/07	MCLOCK for wind direction control	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INCLUDE		'ERROR.PRM'
	PARAMETER	(LLMODE = 8)
	PARAMETER	(NCOMPS = 8)
	PARAMETER	(NCOM_1 = 7)
	CHARACTER*(*)	time(2)
	REAL		gridu (igx,igy), gridv (igx,igy)
	REAL		xlat(LLMXPT), ylon(LLMXPT)
	CHARACTER	cmode(LLMODE)*2
	CHARACTER	sys_G*2, sys_M*2
	INTEGER		inout(LLMXPT), mode(LLMODE), pmode(2)
	INTEGER		iwind(MXSPED), iwave(MXWHGT), imode (LLMODE)
	REAL		fiq(LLMXPT), fjq (LLMXPT)
	LOGICAL		proces
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
	proces = .true.
	sys_G = 'G'
	CALL ST_NULL ( sys_G, sys_G, lens, ier )
	sys_M = 'M'
	CALL ST_NULL ( sys_M, sys_M, lens, ier )
	CALL ST_LSTR ( time ( 1 ), lstr, ier )
	grdtm ( nt ) = time ( 1 ) ( : lstr )
	ngrdtm = nt
C
	IF  ( proces .and. ktype .eq. 1 )  THEN
	    CALL G2T_BND ( nz, ns, npts, clat, clon, xlat, ylon, iret )
	    IF ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		iret = -7
		RETURN
	    END IF
C
	    DO ii = 1, MXWHGT
		iwave ( ii ) = 0
	    END DO 
C
C*	    Loop over entire grid.
C
	    DO ki = 1, igx
		fiq ( ki ) = ki
	    END DO
C
	    isum = 0
	    sum = 0.
	    wmax = RMISSD
	    wmin = - RMISSD
	    DO kj = 1, igy
		DO ki = 1, igx
		    fjq (ki) = kj
		END DO
		CALL CGR_INPOLY ( sys_G, igx, fiq, fjq,
     +				  sys_M, npts, xlat, ylon, 
     +				  inout, ier )
		DO ki = 1, igx
		    IF ( inout ( ki ) .eq. 0 .or.
     +			 ERMISS ( gridu ( ki, kj ) ) )  THEN
		      ELSE
			sum = gridu ( ki, kj ) + sum
			isum = isum + 1
			IF ( gridu ( ki, kj ) .lt. 1. ) THEN
			    ic = 1
			  ELSE
			    ic = NINT ( gridu ( ki, kj ) )
			END IF
C
C*			Wave heights bin indices
C*
C*  			0---1.5---2.5---3.5---4.5---5.5--- (feet)
C*			  1     2     3     4     5        (indices)
C
			iwave ( ic ) = iwave ( ic ) + 1
			wavhst ( ns, nt, ic ) = iwave ( ic )
			wmax = AMAX1 ( wmax, gridu (ki, kj) )
			wmin = AMIN1 ( wmin, gridu (ki, kj) )
		    END IF
		END DO
	    END DO
	    IF ( .not. ERMISS ( wmax ) .and. .not. ERMISS ( -wmin ) )
     +		THEN
		mxval ( ktype, ns, nt ) = NINT ( wmax ) 
		mnval ( ktype, ns, nt ) = NINT ( wmin )
	      ELSE
		mxval ( ktype, ns, nt ) = IMISSD 
		mnval ( ktype, ns, nt ) = IMISSD
	    END IF
C		
	    CALL G2T_SAVW ( ktype, ns, nt, ier ) 
	    DO kk = 1, MXWHGT
		IF  ( .not. ERMISS ( FLOAT ( wavhst(ns,nt,kk) ) ) ) THEN
		    wavpct ( ns, nt, kk ) = ( wavhst ( ns, nt, kk ) / 
     +					      FLOAT ( isum ) ) * 100.
		END IF
	    END DO
C
	  ELSE IF ( proces .and. ktype .eq. 2 )  THEN
C
C*	    Compute the vector diagnostic grid.
C
	    CALL G2T_BND ( nz, ns, npts, clat, clon, xlat, ylon, iret )
	    IF ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		iret = -7
		RETURN
	    END IF
C
C*          Loop over entire grid
C
            DO ki = 1, igx
		fiq ( ki ) = ki
	    END DO
C
C*	    Initialization.
C
	    DO ii = 1, LLMODE
		mode ( ii ) = 0
	    END DO
C
C*	    Initialize counter.
C
	    DO ii = 1, MXSPED
		iwind ( ii ) = 0
	    END DO
C
	    jsum = 0
	    vmax = RMISSD
	    vmin = - RMISSD
	    DO kj = 1, igy
		DO ki = 1, igx
		    fjq (ki) = kj
		END DO
		CALL CGR_INPOLY ( sys_G, igx, fiq, fjq,
     +				  sys_M, npts, xlat, ylon,
     +                            inout, ier )
                DO ki = 1, igx
		    IF ( inout ( ki ) .eq. 0 .or. 
     +			 ERMISS ( gridu (ki,kj) ) .or.
     +			 ERMISS ( gridv (ki,kj) )  ) THEN
		      ELSE
			jsum = jsum + 1
			ucomp = PR_MSKN ( gridu ( ki, kj ) )
			vcomp = PR_MSKN ( gridv ( ki, kj ) )
                        sped = ( ucomp ** 2 + 
     +				 vcomp ** 2 ) ** .5
C
C*			Wind speed bin indices.
C*
C*  			0---2.5---7.5---12.5---17.5---22.5---27.5---
C*			  1     2     3      4      5      6  (indices)
C*			Light   5    10     15     20     25  (speed)
C
                        ic = INT ( sped + 7.5 ) / 5
			iwind ( ic ) = iwind ( ic ) + 1
			winhst ( ns, nt, ic ) = iwind ( ic )
                        vmax  = AMAX1 ( vmax, sped )
                        vmin  = AMIN1 ( vmin, sped )
C
C*			Compute most prevalent wind directions for 
C*			wind speed >= 5.
C
			IF  ( sped .ge. 5. )  THEN
			    drct = PR_DRCT ( ucomp, vcomp )
			    idrct = INT ( ( drct + 22.5) / 45. )
			    IF  ( idrct .gt. 8 .or. idrct .le. 0 )  THEN
				idrct = 8
			    END IF
			    mode ( idrct ) = mode ( idrct ) + 1
			END IF
                    END IF
		END DO
	    END DO
	    IF ( .not. ERMISS (vmax) .and. .not. ERMISS (-vmin) )  THEN
	        mxval ( ktype, ns, nt ) = ( INT ( vmax + 2.5 ) ) / 5 * 5
	        mnval ( ktype, ns, nt ) = ( INT ( vmin + 2.5 ) ) / 5 * 5
		IF ( mxval ( ktype, ns, nt ) .le. LGHTS )  THEN
		     mxval ( ktype , ns, nt )  =  LGHTS
		END IF
	      ELSE
		mxval ( ktype, ns, nt ) = IMISSD
		mnval ( ktype, ns, nt ) = IMISSD
	    END IF
C
	    CALL G2T_SAVW ( ktype, ns, nt, iret ) 
	    DO kk = 1, MXSPED
		IF ( .not. ERMISS ( FLOAT(winhst(ns,nt,kk)) ) ) THEN
		    winpct ( ns, nt, kk ) = ( winhst ( ns, nt, kk ) / 
     +					      FLOAT ( jsum ) ) * 100.
		END IF
	    END DO
	    IF  ( mxval ( ktype, ns, nt ) .ge. 65 )  THEN
		fhurr ( nt ) = .TRUE.
	      ELSE IF ( mxval ( ktype, ns, nt ) .ge. 50 )  THEN
		fstorm ( nt ) = .TRUE.
	      ELSE IF ( mxval ( ktype, ns, nt ) .ge. 35 )  THEN
		fgale ( nt ) = .TRUE.
	    END IF
C
C*	    Compute prevalent wind directions.
C
	    CALL G2T_MODE ( LLMODE, mode, cmode, imode, iret ) 		
C
C*	    Output two most prevalent wind directions in percentage.
C
	    DO ij = 1, 2
		pmode (ij) = NINT ( mode (ij) / float (jsum) *100 )
	    END DO
C
C*	    Store the wind direction in proper order. For large wind
C*	    spread, keep the same order for now.
C
	    wdrmod ( 1, ns, nt )  = cmode ( 1 )
	    kdrmod ( 1, ns, nt )  = imode ( 1 )
	    wdrmod ( 2, ns, nt )  = cmode ( 2 )
	    kdrmod ( 2, ns, nt )  = imode ( 2 )
	    IF ( MCLOCK .eq. 2 )  THEN
		IF  ( cmode (1) .eq. cmode (2) .or. pmode(1) .eq. 100 )
     +		      THEN
		    wdrmod ( 2, ns, nt )  = ' '
		    kdrmod ( 2, ns, nt ) = IMISSD
		END IF
	      ELSE IF ( MCLOCK .eq. 1 )  THEN
		IF  ( cmode (1) .eq. cmode (2) .or. pmode(1) .eq. 100 )
     +			THEN
		    wdrmod ( 2, ns, nt )  = ' '
		    kdrmod ( 2, ns, nt ) = IMISSD
		  ELSE IF ( ABS ( imode ( 1 ) - imode ( 2 ) ) .le. 1.or.
     +			ABS ( imode ( 1 ) - imode ( 2 ) ) .ge. NCOM_1 )
     +			THEN
		    IF ( imode ( 1 ) .eq. NCOMPS .or. 
     +			 imode ( 2 ) .eq. NCOMPS )  THEN
			IF ( imode (1) .eq. NCOMPS .and. 
     +			     imode (2) .eq. NCOM_1 .or.
     +			     imode (1) .eq. 1 .and. 
     +			     imode (2) .eq. NCOMPS ) THEN
			    wdrmod ( 2, ns, nt )  = cmode ( 1 )
			    kdrmod ( 2, ns, nt )  = imode ( 1 )
			    wdrmod ( 1, ns, nt )  = cmode ( 2 )
			    kdrmod ( 1, ns, nt )  = imode ( 2 )
			END IF
		      ELSE IF ( imode ( 1 ) .gt. imode ( 2 ) )  THEN
			wdrmod ( 2, ns, nt )  = cmode ( 1 )
			kdrmod ( 2, ns, nt )  = imode ( 1 )
			wdrmod ( 1, ns, nt )  = cmode ( 2 )
			kdrmod ( 1, ns, nt )  = imode ( 2 )
		    END IF
		END IF
	    END IF
	END IF
C
	RETURN
C*
	END
