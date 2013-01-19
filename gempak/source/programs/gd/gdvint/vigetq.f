	SUBROUTINE VI_GETQ  ( lev, iqflg, cmn_data, 
     +                        rlnpi, sphbuf, tmpbuf, grid, iret )
C************************************************************************
C* VI_GETQ								*
C*									*
C* This subroutine obtains either specific humidity or its natural log	*
C* on input level LEV.							*
C*									*
C* VI_GETQ ( LEV, IQFLG, CMN_DATA, RLNPI, SPHBUF, TMPBUF, GRID, IRET )	*
C*									*
C* Input parameters:							*
C*	LEV		INTEGER		Level number			*
C*	IQFLG		INTEGER		Flag for ln (q)			*
C*					     0 = q			*
C*					 not 0 = ln (q)			*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*	SPHBUF(kxky)	REAL		previously local array          *
C*	TMPBUF(kxky)	REAL		previously local array          *
C*									*
C* Output parameters:							*
C*	GRID(*)		REAL		Array of values			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* R. Miller/COMET   06/94  Look for SPFH first, then the MSTPRM second.*
C*                          Always read T into TMPBUF			*
C* R. Tian/SAIC	     04/05  Added GD_OPEN to get input file number	*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		cmn_data (*)
	REAL		rlnpi (kxky,nli)
	REAL		sphbuf (kxky), tmpbuf (kxky)
	REAL		grid (*)
C*
	CHARACTER*4	vcnms (3), prnms (7), psave
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
C
C*	Set possible vertical coordinate names.
C
	vcnms (1) = vcordi
	vcnms (2) = NNONE
	vcnms (3) = NESFC
C
C*	Set possible parameter names.
C
	prnms (1) = NSPFH
	prnms (2) = NRELH
	prnms (3) = NDWPT
	prnms (4) = NDWPC
	prnms (5) = NDWPK
	prnms (6) = NRMIX
	prnms (7) = NMIXR
C
C*	Substitute PARMS (MSTPRM) as the second name to try.
C*	Always search for SPFH first (Miller/Brill).
C
	DO ii = 2, 7
	    IF ( prnms (ii) .eq. parms (mstprm) ) THEN
		psave = prnms (2)
		prnms (2) = parms (mstprm)
		prnms (ii) = psave
	    END IF
	END DO	
C*
	npn = 7
	IF ( lev .eq. 1 ) THEN
	    istop = 3
	ELSE
	    istop = 1
	END IF
	i1 = 0
	jret = 5
	DO WHILE ( i1 .lt. istop .and. jret .ne. 0 )
	    i1 = i1 + 1
	    ip = 0
	    DO WHILE ( ip .lt. npn .and. jret .ne. 0 )
		ip = ip + 1
		CALL VG_READ ( igdfli, vcnms (i1), levin (lev), prnms (ip), 
     +                         cmn_data( sfcval_indx ), 
     +		               cmn_data( vgread_buf_indx ), sphbuf, jret )
	    END DO
	END DO
C
C*	If a moisture parameter was read, convert it to specific
C*	humidity if necessary.
C
	IF ( jret .eq. 0 .and. prnms (ip) .eq. NSPFH ) THEN
C
C*	    Do nothing.
C
	ELSE IF ( jret .eq. 0 .and. prnms (ip) .eq. NRELH ) THEN
C
C*	    Convert RH to q.
C
	    IF ( iprcnt .eq. 0 ) THEN
		iprcnt = 1
	        DO ij = 1, kxky
		    IF ( sphbuf (ij) .gt. 1.1 ) iprcnt = 2
	        END DO
	    END IF
	    jret = 1
            CALL VI_RDTK ( lev, cmn_data, cmn_data( rlnpi_indx ),
     +                          cmn_data( aprs_indx ), tmpbuf, jret )
	    CALL VC_VAPR ( tmpbuf, kxky, tmpbuf, jret )
	    IF ( jret .eq. 0 ) THEN
		DO ij = 1, kxky
		    IF ( .not. ERMISS ( sphbuf (ij) ) .and.
     +			 .not. ERMISS ( tmpbuf (ij) ) .and.
     +			 .not. ERMISS ( rlnpi ( ij,lev ) ) ) THEN
			rh = sphbuf (ij)
			IF ( iprcnt .eq. 2 ) rh = rh / 100.
			ev = rh * tmpbuf (ij)
			r = .622 * ev /
     +			      ( EXP ( rlnpi ( ij,lev ) ) - ev )
			sphbuf (ij) = r / ( 1. + r )
		    ELSE
			sphbuf (ij) = RMISSD
		    END IF
		END DO
	    END IF
C*
	ELSE IF ( jret .eq. 0 .and. ( prnms (ip) .eq. NDWPT .or.
     +		  prnms (ip) .eq. NDWPC .or.
     +		  prnms (ip) .eq. NDWPK ) ) THEN
C
C*	    Convert dewpoint T to q.
C
	    CALL VC_VAPR ( sphbuf, kxky, tmpbuf, jret )
	    IF ( jret .eq. 0 ) THEN
		DO ij = 1, kxky
		    IF ( .not. ERMISS ( tmpbuf (ij) ) .and.
     +			 .not. ERMISS ( rlnpi ( ij,lev ) ) ) THEN
			ev = tmpbuf (ij)
			r = .622 * ev /
     +			      ( EXP ( rlnpi ( ij,lev ) ) - ev )
			sphbuf (ij) = r / ( 1. + r )
		    ELSE
			sphbuf (ij) = RMISSD
		    END IF
		END DO
	    END IF
C*
	ELSE IF ( jret .eq. 0 .and. ( prnms (ip) .eq. NRMIX .or.
     +		  prnms (ip) .eq. NMIXR ) ) THEN
C
C*	    Convert mixing ratio to q.
C
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( sphbuf (ij) ) ) THEN
		    r = sphbuf (ij)
		    IF ( prnms (ip) .eq. NMIXR ) r = r / 1000.
		    sphbuf (ij) = r / ( 1. + r )
		END IF
	    END DO
	END IF
C
C*	Compute ln (q) if requested.
C
	IF ( jret .eq. 0 .and. iqflg .ne. 0 ) THEN
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( sphbuf (ij) ) .and.
     +		     sphbuf (ij) .gt. 0.0 ) THEN
		    grid (ij) = ALOG ( sphbuf (ij) )
		ELSE
		    grid (ij) = RMISSD
		END IF
	    END DO
	ELSE IF ( jret .eq. 0 ) THEN
	    DO ij = 1, kxky
		grid (ij) = sphbuf (ij)
	    END DO
	END IF
C*	     
	IF ( jret .ne. 0 .and. iqflg .eq. 0 ) THEN
C
C*	    Just set the specific humidity to zero and return a
C*	    nonzero return code.
C
	    DO ij = 1, kxky
		grid (ij) = 0.0
	    END DO
	    iret = jret
	    RETURN
C*
	ELSE 
	    iret = jret
	    RETURN
	END IF
C*
	RETURN
	END
