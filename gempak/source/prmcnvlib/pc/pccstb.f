	SUBROUTINE PC_CSTB  ( datain, p, data, iret )
C************************************************************************
C* PC_CSTB								*
C*									*
C* This subroutine computes requested stability index.			*
C*									*
C* PC_CSTB  ( DATAIN, P, DATA, IRET )					*
C*									*
C* Input parameters:							*
C*	DATAIN (*)	REAL		Station data			*
C*	P		CHAR*		Name of stability index		*
C*									*
C* Output parameters:							*
C*	DATA		REAL		Computed stability index	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up; added documentation		*
C* G. Huffman/USRA	 8/89	BRCH, CAPE, CIN, EQLV, LFC		*
C* M. desJardins/GSFC	 7/90	Major reorganization			*
C* J. Whistler/SSAI	 2/91	Added code to include PWAT		*
C* M. desJardins/NMC	10/91	Check return codes from PC_CMDT		*
C* K. Brill/NMC		01/92	Skip undergrd man lvls for PWAT		*
C* M. desJardins/NMC	 9/92	Fixed long line; use idx. variable	*
C* T. Lee/GSC		 8/97	Added BRCV, CAPV, CINV, EQTV, LFCT,	*
C*				LFTV, and TVFLAG			*
C* T. Piper/SAIC	 4/02	Initialized rdata			*
C* T. Lee/SAIC		 6/03	Added HHAN, MHAN, LHAN			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*)
	CHARACTER*(*)	p
	LOGICAL		tvflag
C*
	REAL		rdata (10), datalv (MMPARM), datout (MMPARM)
	PARAMETER	( IDX1=5, IDX2=6, IDX3=7 )
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	DO i = 1, 10
	    rdata(i) = RMISSD
	END DO 
	IF  ( p .eq. 'SHOW' )  THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier1 )
	    t500  = rdata (2)
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier2 )
	    t850  = rdata (2)
	    td850 = rdata (3)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data  = PS_SHOW ( t850, td850, t500 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'LIFT' ) THEN
	    tvflag = .false.
	    data   = PS_LIFT ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'LFTV' ) THEN
	    tvflag = .true.
	    data   = PS_LIFT ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'SWET' ) THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier1 )
	    t850  = rdata (2)
	    td850 = rdata (3)
	    s850  = PR_SPED ( rdata (4), rdata (5) )
	    d850  = PR_DRCT ( rdata (4), rdata (5) )
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier2 )
	    t500  = rdata (2)
	    s500  = PR_SPED ( rdata (4), rdata (5) )
	    d500  = PR_DRCT ( rdata (4), rdata (5) )
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data = PS_SWET ( t850, td850, t500, s850, s500, d850, 
     +				 d500 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'KINX' ) THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier1 )
	    t850  = rdata (2)
	    td850 = rdata (3)
	    CALL PC_CMDT (idx1, idx2, idx3, 700., 1, datain, rdata, 
     +								ier2 )
	    t700  = rdata (2)
	    td700 = rdata (3)
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier3 )
	    t500  = rdata (2)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) .and. 
     +		  ( ier3 .eq. 0 ) )  THEN
		data = PS_KINX ( t850, t700, t500, td850, td700 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'TOTL' ) THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier1 )
	    t850  = rdata (2)
	    td850 = rdata (3)
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier2 )
	    t500  = rdata (2)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data = PS_TOTL ( t850, td850, t500 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'CTOT' ) THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier1 )
	    td850 = rdata (3)
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier2 )
	    t500  = rdata (2)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data = PS_CTOT ( td850, t500 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'VTOT' ) THEN
	    CALL PC_CMDT (idx1, idx2, idx3, 850., 1, datain, rdata, 
     +								ier1 )
	    t850  = rdata (2)
	    CALL PC_CMDT (idx1, idx2, idx3, 500., 1, datain, rdata, 
     +								ier2 )
	    t500  = rdata (2)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data = PS_VTOT ( t850, t500 )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'BRCH' )  THEN
	    tvflag = .false.
	    data   = PS_BRCH ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'BRCV' )  THEN
	    tvflag = .true.
	    data   = PS_BRCH ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'CAPE' )  THEN
	    tvflag = .false.
	    data   = PS_CAPE  ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'CAPV' )  THEN
	    tvflag = .true.
	    data   = PS_CAPE  ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'CINS' )  THEN
	    tvflag = .false.
	    data   = PS_CINS ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'CINV' )  THEN
	    tvflag = .true.
	    data   = PS_CINS ( datain, jdsprm, tvflag )
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'EQLV' .or. p .eq. 'EQTV' )  THEN
	    CALL PS_PRCL  ( datain, jdsprm, 500., 3, 1, depth, idcord,
     +			    pavg, tavg, tdavg, uavg, vavg, zavg, thavg,
     +			    rmxavg, ier )
	    IF  ( ERMISS ( pavg  ) .or. ERMISS ( tavg ) .or.
     +		  ERMISS ( tdavg ) )  THEN
		data = RMISSD
	      ELSE
		tlcl  = PR_TLCL ( tavg, tdavg )
		plcl  = PR_PLCL ( tavg, pavg, tlcl )
		CALL PC_CMDT  ( idx1,  idx2, idx3, plcl, 1, datain, 
     +				rdata, ier )
		tlclc = PR_TMKC ( tlcl )
		thte  = PR_THTE ( plcl, tlclc, tlclc )
		IF  ( ier .eq. 0 )  THEN
		    IF  ( p .eq. 'EQLV' )  THEN
			tvflag = .false.
		      ELSE
			tvflag = .true.
		    END IF
		    data  = PS_EQLV ( jdsprm, datain, thte, tvflag )
		  ELSE
		    data  = RMISSD
		END IF
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'LFCT' .or. p .eq. 'LFCV' )  THEN
	    CALL PS_PRCL  ( datain, jdsprm, 500., 3, 1, depth, idcord,
     +			    pavg, tavg, tdavg, uavg, vavg, zavg, thavg,
     +			    rmxavg, ier )
	    IF  ( ERMISS ( pavg  ) .or. ERMISS ( tavg ) .or.
     +		  ERMISS ( tdavg ) )  THEN
		data = RMISSD
	      ELSE
		tlcl  = PR_TLCL ( tavg, tdavg )
		plcl  = PR_PLCL ( tavg, pavg, tlcl )
		CALL PC_CMDT  ( idx1, idx2, idx3, plcl, 1, datain, 
     +				rdata, ier )
		tlclc = PR_TMKC ( tlcl )
		thte  = PR_THTE ( plcl, tlclc, tlclc )
		IF  ( ier .eq. 0 )  THEN
		    IF  ( p .eq. 'LFCT' )  THEN
			tvflag = .false.
		      ELSE
			tvflag = .true.
		    END IF
		    data  = PS_LFCV  ( datain, jdsprm, thte, tvflag )
		  ELSE
		    data  = RMISSD
		END IF
	    END IF
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'MLTH' ) THEN
	    CALL PS_PRCL  ( datain, jdsprm, 500., 3, 1, depth, idcord,
     +			    pavg, tavg, tdavg, uavg, vavg, zavg, thavg,
     +			    rmxavg, ier )
	    data = thavg
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'MLMR' ) THEN
	    CALL PS_PRCL  ( datain, jdsprm, 500., 3, 1, depth, idcord,
     +			    pavg, tavg, tdavg, uavg, vavg, zavg, thavg,
     +			    rmxavg, ier )
	    data = rmxavg
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'LCLT' ) THEN
	    CALL PS_LCLL  ( datain, tlcl, plcl, ier )
	    data = tlcl
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'LCLP' ) THEN
	    CALL PS_LCLL  ( datain, tlcl, plcl, ier )
	    data = plcl
C------------------------------------------------------------------------
	  ELSE IF ( p .eq. 'PWAT' ) THEN
	    index = 5
	    pwatb = 0.
	    CALL PC_GLEV ( 1, datain, jdsprm, datalv, ier )
	    CALL PC_COMP ( index, datalv, datout, ier )
	    tdb = datout (3)
	    pb = datout (1)
	    psfc = pb
	    DO ilev = 2, jnumlv
		CALL PC_GLEV ( ilev, datain, jdsprm, datalv, ier )
		CALL PC_COMP ( index, datalv, datout, ier )
	   	IF ( datout (1) .lt. psfc ) THEN
		    pt = datout (1)
		    tdt = datout (3)
		    data = PC_PWTR ( tdb, tdt, pb, pt, pwatb )
		    tdb = tdt
		    pb = pt
		    pwatb = data
		END IF
	    END DO
C------------------------------------------------------------------------
	  ELSE IF ( p (2:4) .eq. 'HAN' ) THEN
	    IF  ( p .eq. 'LHAN' )  THEN 
		rlvl1 = 850.
		rlvl2 = 950.
		itype = 1
	      ELSE IF ( p .eq. 'MHAN' )  THEN
		rlvl1 = 850.
		rlvl2 = 700.
		itype = 2
	      ELSE IF ( p .eq. 'HHAN' )  THEN 
		rlvl1 = 700.
		rlvl2 = 500.
		itype = 3
	    END IF
		
	    CALL PC_CMDT ( idx1, idx2, idx3, rlvl1, 1, datain, rdata, 
     +								ier1 )
	    t1  = rdata (2)
	    td  = rdata (3)
	    CALL PC_CMDT ( idx1, idx2, idx3, rlvl2, 1, datain, rdata, 
     +								ier2 )
	    t2  = rdata (2)
	    IF  ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) )  THEN
		data = PS_HANS ( t1, t2, td, itype, ier )
	      ELSE
		data = RMISSD
	    END IF
C------------------------------------------------------------------------
	  ELSE
	    data  = RMISSD
	END IF
C*
	RETURN
	END
