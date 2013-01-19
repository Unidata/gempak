	SUBROUTINE VG_WRIT  ( ip, klev, grid,
     +                        ovcsfc, sfcval, rlnpo, iret )
C************************************************************************
C* VG_WRIT								*
C*									*
C* This subroutine determines the packing information for a grid and	*
C* then writes it to the output file.					*
C*									*
C* VG_WRIT ( IP, KLEV, GRID, OVCSFC, SFCVAL, RLNPO, IRET )		*
C*									*
C* Input parameters:							*
C*	IP		INTEGER		Parameter number		*
C*	KLEV		INTEGER		Output level number		*
C*	GRID(*)		REAL		Grid of values			*
C*									*
C* Input/Ouput parameters:                                              *
C*      OVCSFC(kxky)    REAL            output vc value on sfc          *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPO(kxky,nlo) REAL            ln (p) on output lvls           *
C*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		08/92						*
C* K. Brill/NMC		10/92	Handle Lorenz boundary condition	*
C* K. Brill/NMC		11/92	Fix PSYM for Lorenz boundary cond.	*
C* K. Brill/NMC		12/92	Scale Montgomery Strm Fnctn for GEMPAK	*
C* K. Brill/NMC		11/95	Simple build down for HGHT on P		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   * 
C*                              DATA statement                          *
C* R. Tian/SAIC		 4/05	Added GD_OPEN to get output file number	*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
	PARAMETER	( CP = AKAPPA * RDGAS )
C*
	REAL		grid (*)
	REAL            ovcsfc (kxky)
	REAL            sfcval (kxky,np)
	REAL            rlnpo (kxky,nlo)
C*
	INTEGER		ighdr ( LLGDHD ), lev (2)
	INCLUDE		'ERMISS.FNC'
	DATA		covpok / 139.6443 /
C-----------------------------------------------------------------------
	iret = 0

	CALL GD_OPEN ( gdcuro, .true., 0, 0, igdflo,
     +                 adum1, adum2, mxgrd, iret )
C*
	lev (1) = levout ( klev )
	lev (2) = -1
C
C*	Check for Lorenz boundary condition.
C*	Note:  This condition is applied when the pressure on THETA
C*	is missing.
C
	IF ( sfcovc .and. lorenz .and. lpmrds (ip) ) THEN
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( ovcsfc (ij) ) .and.
     +		     FLOAT ( levout (klev) ) .lt. ovcsfc (ij) .and.
     +		     ERMISS ( rlnpo (ij,klev) ) ) 
     +			    grid (ij) = sfcval (ij,ip)
	    END DO
	END IF
	IF ( parms (ip) .eq. NHGHT .and. vcordo .eq. NPRES ) THEN
C
C*	    Build heights down to underground points using surface
C*	    values and standard atmospheric lapse rate.
C
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( sfcval (ij,1) ) .and.
     +		     .not. ERMISS ( sfcval (ij,2) ) .and.
     +		     .not. ERMISS ( sfcval (ij,3) ) ) THEN
		    pug = lev (1)
		    IF ( pug .gt. sfcval (ij,1) ) THEN
			t0 = sfcval (ij,2)
			rlaps = 6.5 / 1000.
			p0 = sfcval (ij,1)
			rxp = 287.04 * rlaps / 9.81
			fac1 = 1. - ( pug / p0 ) ** rxp
			fac2 = t0 / rlaps
			dz = fac1 * fac2
			grid (ij) = sfcval (ij,3) + dz
		    END IF
		END IF
	    END DO
	END IF
C
C*	Initialize ighdr.
C
	DO ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
	IF ( parms (ip) .eq. NHGHT .and. vcordo .eq. NTHTA ) THEN
C
C*	    Compute Montgomery Stream Function.
C
	    rth = FLOAT ( lev (1) ) * covpok
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( rlnpo ( ij,klev) ) .and. 
     +	             .not. ERMISS ( grid (ij) ) ) THEN
		    prs = EXP ( rlnpo (ij,klev) )
		    cpt = rth * prs ** RKAPPA
		    grid (ij) = cpt + GRAVTY * grid (ij)
		ELSE IF ( lorenz .and.
     +			  lpmrds (2) .and. lpmrds (3) ) THEN
		    IF ( .not. ERMISS ( sfcval (ij,2) ) .and.
     +			 .not. ERMISS ( sfcval (ij,1) ) .and.
     +			 .not. ERMISS ( sfcval (ij,3) ) ) THEN
			grid (ij) = CP * sfcval (ij,2) +
     +				    GRAVTY * sfcval (ij,3) -
     +				    ( ovcsfc (ij) -
     +					FLOAT ( levout (klev) ) )
     +				    * CP *
     +				    ( sfcval (ij,1) / 1000. ) **
     +					RKAPPA
		    ELSE
			grid (ij) = RMISSD
	    	    END IF
		ELSE
		    grid (ij) = RMISSD
		END IF
C
C*		Scale the Montgomery Stream Function for GEMPAK.
C
		IF ( .not. ERMISS ( grid (ij) ) )
     +			grid (ij) = grid (ij) / 100.
	    END DO
	    nbits = 16
	    CALL GD_WPGD ( igdflo, grid, kx, ky, ighdr, gdttm, lev,
     +			   igvco, NPSYM, .false., MDGGRB, nbits,
     +			   ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'GD', ier, ' ', irr )
	    END IF
	ELSE IF ( ip .eq. mstprm ) THEN
C
C*	    The moisture is interpolated as a ln (q) vs ln (p).
C*	    Convert ln (q) to SPFH and store.
C
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( grid (ij) ) )
     +			grid (ij) = EXP ( grid (ij) )
	    END DO
	    CALL VC_NBTS ( grid, kxky, 4, nbits, iret )
     	    CALL GD_WPGD ( igdflo, grid, kx, ky, ighdr, gdttm, lev,
     +		           igvco, 'SPFH', .false., MDGGRB, nbits,
     +			   ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'GD', ier, ' ', irr )
	    END IF
	ELSE
	    CALL VC_NBTS ( grid, kxky, 4, nbits, iret )
     	    CALL GD_WPGD ( igdflo, grid, kx, ky, ighdr, gdttm, lev,
     +		           igvco, parms (ip), .false., MDGGRB, nbits,
     +			   ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'GD', ier, ' ', irr )
	    END IF
	END IF

	RETURN
	END
