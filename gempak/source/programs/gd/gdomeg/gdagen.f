	SUBROUTINE GDAGEN  ( dattim, gpack, iret )
C************************************************************************
C* GDAGEN								*
C*									*
C* This subroutine computes OMEG and stores it in the grid file.	*
C*									*
C* GDAGEN  ( DATTIM, GPACK, IRET )					*
C*									*
C* Input parameters:							*
C*	DATTIM (2)	CHAR*		GEMPAK time to process		*
C*      GPACK		CHAR*		Data packing information	*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*                                        1 = no sfc pressure		*
C*					  0 = normal return		*
C*                                       -4 = time not available	*
C*                                       -5 = no p levels		*
C*                                       -6 = bad packing input		*
C*					 -7 = grid write failure	*
C**									*
C* Log:									*
C* K. Brill/NMC		10/90						*
C* K. Brill/NMC		05/91	Refinements				*
C* K. Brill/NMC		06/91	Added checks for PALT			*
C* M. desJardins/NMC	 1/92	GDOGEN-->GDAGEN				*
C* K. BRILL/NMC		02/92	Use LLNNAV				*
C* J. Wu/GSC            07/00	Moved INCLUDE 'ERMISS.FNC' before DATA	* 
C*                                statement				*
C* R. Tian/SAIC		01/05	Removed iflno, iflout			*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim (2), gpack
C*
	LOGICAL		havsfc, havsp, havsf0, hvpalt
	CHARACTER	gdatim *48, time (2)*20, glevel*20, gfun2*80,
     +                  gfunc*80, pfunc*80, parm*12, gvcord*12
	INTEGER		lev (2)
	REAL		divbot ( LLMXGD ),    omga ( LLMXGD ),
     +                  sfcp   ( LLMXGD ),  divtop ( LLMXGD ), 
     +                  omout  ( LLMXGD ),   omcor ( LLMXGD ),
     +                  rlnp   ( LLMXGD ),   psave ( LLMXGD )
C*
	INTEGER		level ( 2, LLMXLV )
	REAL            rlvl  ( LLMXLV )
C*
	INTEGER         ighdr ( LLGDHD )
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the grid navigation information.
C
	CALL DG_QKXY ( kx, ky, ier )
	npts = kx * ky
C
C*	Interpret the input time.
C
	CALL TG_DUAL ( dattim, gdatim, ier )
C
C*	Check when vertical coordinate is NONE first.
C
	ivcord = 0
	CALL DG_GLEV  ( 1, dattim, ivcord, LLMXLV, level,
     +		      	nlev, ier )
C*
	havsp = .false.
	havsfc = .false.
	havsf0 = .false.
	hvpalt = .false.
C*
	IF ( nlev .gt. 0 ) THEN
	    DO i = 1, nlev
	       IF ( level ( 1, i ) .eq. 0 ) havsf0 = .true.
	    END DO
	END IF
C
C*	Check when vertical coordinate is PRES.
C
        ivcord = 1
	gvcord = 'PRES'
	CALL DG_GLEV  ( 1, dattim, ivcord, LLMXLV, level,
     +			nlev, ier )
	IF  ( nlev .eq. 0 .or. ier .ne. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'GDOMEG', iret, ' ', ier )
	    RETURN
	END IF
C
C*      Load the levels for sorting and look for surface.
C
	DO i = 1, nlev
	   rlvl ( i ) = FLOAT ( level ( 1, i ) )
	   IF ( level ( 1, i ) .eq. 0 ) havsfc = .true.
	END DO
C
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
	IF ( iret .ne. 0 ) THEN
	   CALL ER_WMSG ( 'LV', iret, ' ', ier )
	   RETURN
	END IF
	istrt = 1
	IF ( havsfc ) istrt = 2
	havsfc = havsfc .or. havsf0
C
C*	Get the lower bound pressure.
C
	glevel = '0'
	IF ( havsfc ) THEN
	    havsp = .true.
            gfunc = 'PRES%NONE'
	    CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			    pfunc, sfcp, igx, igy, time, lev, 
     +			    ivc, parm, iret )
	    IF  ( iret .ne. 0 )  THEN
		gfunc = 'PALT%NONE'
	        CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			        pfunc, sfcp, igx, igy, time, lev, 
     +			        ivc, parm, iret )
	  	IF ( iret .eq. 0 ) hvpalt = .true.
	    END IF
	    IF  ( iret .ne. 0 )  THEN
                gfunc = 'PRES'
	        CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			        pfunc, sfcp, igx, igy, time, lev, 
     +			        ivc, parm, iret )
	        IF ( iret .ne. 0 ) THEN
		    gfunc = 'PALT'
	            CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +			            pfunc, sfcp, igx, igy, time, lev, 
     +			            ivc, parm, iret )
		    IF ( iret .eq. 0 ) hvpalt = .true.
		END IF
		IF ( iret .ne. 0 ) THEN
	            havsp = .false.
	            iret = 1
	            CALL ER_WMSG ( 'GDOMEG', iret, ' ', ier )
                    DO ij = 1, npts
	                sfcp (ij) = rlvl (istrt)
	            END DO
	        END IF
	    END IF
	ELSE
	    iret = 1
	    CALL ER_WMSG ( 'GDOMEG', iret, ' ', ier )
            DO ij = 1, npts
	        sfcp (ij) = rlvl (istrt)
	    END DO
	END IF
C
C*	Compute the starting OMEG value.
C
	gfunc =
     +  'MUL(MUL(SUB(0,GRAVTY),DDEN(PRES,TMPC)),DOT(OBS,GRAD(SELV@0)))'
	IF ( hvpalt ) gfunc =
     +  'MUL(MUL(SUB(0,GRAVTY),DDEN(PALT,TMPC)),DOT(OBS,GRAD(SELV@0)))'
	gfun2 = gfunc (1:58) // '%NONE)))'
	nnlv = nlev - ( istrt - 1 )
	CALL GDASFC ( gdatim, gfunc, gfun2, rlvl (istrt), nnlv,
     +		      npts, sfcp, havsp, 0.5, omga, ier )
C
C*	Convert the surface omega estimate from Pa/s to mb/s.
C
	DO ij = 1, npts
	    omga (ij) = omga (ij) / 100.0
	END DO
C
C*	Compute the starting divergence value.  
C
	gfunc = 'DIV(OBS)'
	gfun2 = ' '
	CALL GDASFC ( gdatim, gfunc, gfun2, rlvl (istrt), nnlv,
     +		      npts, sfcp, havsp, 1.0, divbot, ier )
C
C*	Initialize ln (p) and save the surface pressure.
C
	DO ij = 1, npts
	    IF ( .not. ERMISS ( sfcp (ij) ) )
     +			rlnp  (ij) = ALOG ( sfcp (ij) )
	    psave (ij) = sfcp (ij)
	END DO
C
C*	Reset starting level if necessary.
C
	IF ( havsp ) THEN
C
C*	    Do not reset starting level.
C
	ELSE
	    istrt = istrt + 1
	END IF
C
C*	Do the vertical integration for each layer, grid point by
C*      grid point, and save the result in the grid file.
C
	DO  ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
	DO  i = istrt, nlev
C
C*	    Encode level and compute function.
C
	    intlvl = INT ( rlvl ( i ) )
	    CALL ST_INCH  ( intlvl, glevel, ier )
            gfunc = 'DIV(OBS)'
            CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +		            pfunc,  divtop,  igx, igy, time, lev, 
     +			    ivc, parm, ier )
	    IF  ( ier .ne. 0 )  THEN
C*
	        CALL ER_WMSG ( 'DG', ier, ' ', ire )
C*
  	    ELSE
C
C*	        Loop over grid points, computing omega.
C
	        DO ij = 1, npts
	            omout (ij) = RMISSD
	            IF ( psave (ij) .ge. rlvl (i) .and. 
     +                   .not. ERMISS ( sfcp (ij) )  .and.
     +                   .not. ERMISS ( divtop (ij) ) .and.
     +                   .not. ERMISS ( divbot (ij) )  )THEN
C*
	                dp = sfcp (ij) - rlvl (i)
                        alnp = ALOG ( rlvl (i) )
	                IF ( dp .ne. 0. ) THEN
                            pbar = .5 * ( sfcp (ij) + rlvl (i) )
		            weit = ( ALOG ( pbar ) - rlnp (ij) ) /
     +                               ( alnp - rlnp (ij) )
                            diva = divbot (ij) +
     +                              ( divtop (ij) - divbot (ij) ) * weit
                        ELSE
                            diva = 0.
                        END IF
                        omga (ij) =  omga (ij) + diva * dp
	                divbot (ij) = divtop (ij)
	                sfcp (ij)  = rlvl (i)
                        rlnp (ij)  = alnp
	                omout (ij) = omga (ij)
C
C*	                This may be the last level; so, store omega for
C*                      the correction pass.
C
                        omcor (ij) = omga (ij)
C*
	            END IF
	        END DO
	        parm = 'OMEG'
	        CALL DG_NWDT  ( omout, time, lev, ivcord, parm, ighdr,
     +                          gpack, .true., iret  )
	        IF ( iret .ne. 0 ) THEN
	            CALL ER_WMSG ( 'DG', iret, ' ', ier )
	   	    iret = -7
	            CALL ER_WMSG ( 'GDOMEG', iret, ' ', ier )
	            RETURN
	        END IF
	    END IF
	END DO
C
C*	Do correction pass.
C
C*	Compute denominator of the correction factor so that the
C*	vertical motion on the top level is zero.
C
	rkcap  = nlev - istrt + 1
	corden = rkcap * ( rkcap + 1.0 )
C*
	rk = 0
	DO i = istrt, nlev
	    rk = rk + 1.0
	    intlvl = INT ( rlvl ( i ) )
	    CALL ST_INCH  ( intlvl, glevel, ier )
            gfunc = 'OMEG'
            CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +		            pfunc,  omout,  igx, igy, time, lev, 
     +			    ivc, parm, ier )
	    IF ( ier .ne. 0 ) THEN
	       CALL ER_WMSG ( 'DG', ier, ' ', ire )
	    END IF
	    DO ij = 1, npts
	        IF ( .not. ERMISS ( omout (ij) ) ) THEN
		    corfac = ( rk * ( rk + 1.0 ) ) / corden
	            omout (ij) = omout (ij) - omcor (ij) * corfac
		END IF
	    END DO
	    parm = 'OMEG'
	    CALL DG_NWDT  ( omout, time, lev, ivcord, parm, ighdr,
     +                      gpack, .true., iret  )
	    IF ( iret .ne. 0 ) THEN
	        CALL ER_WMSG ( 'DG', iret, ' ', ier )
	        iret = -7
	        CALL ER_WMSG ( 'GDOMEG', iret, ' ', ier )
	        RETURN
	    END IF
	END DO	
	RETURN
	END
