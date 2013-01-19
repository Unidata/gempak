	SUBROUTINE VI_BDDN  ( ip, sfcval, rlnpi, valu, tlast, iret )
C************************************************************************
C* VI_BDDN								*
C*									*
C* This subroutine builds values down to the surface from 2 levels	*
C* above the surface.							*
C*									*
C* VI_BDDN ( IP, SVCVAL, RLNPI, VALU, TLAST, IRET )			*
C*									*
C* Input parameters:							*
C*	IP		INTEGER		Parameter number		*
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*      TLAST(kxky)     REAL            last virtual temp               *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL            sfcval (kxky,np), rlnpi (kxky,nli)
	REAL            tlast (kxky), valu (kxky,nli)

	REAL		p (2), v (2)
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	At each grid point, search upward and extrapolate to the
C*	surface using first two levels above the surface.
C
	icnt = 0
	DO ij = 1, kxky
	    sfcval (ij,ip) = RMISSD
C*
	    IF ( .not. ERMISS ( rlnpi (ij,1) ) ) THEN
	    	lvls = 0
		nlvl = 1
	    	DO WHILE ( lvls .lt. 2 .and. nlvl .lt. nli )
		    nlvl = nlvl + 1
		    IF ( havlnp (nlvl) .and. lprmrd (nlvl) ) THEN
			IF ( .not. ERMISS ( rlnpi (ij,nlvl) ) .and.
     +			     .not. ERMISS ( valu (ij,nlvl) ) .and. 
     +			     rlnpi (ij,1) .gt. rlnpi (ij,nlvl) ) THEN
			    lvls = lvls + 1
			    p (lvls) = rlnpi (ij,nlvl)
			    v (lvls) = valu (ij,nlvl)
			    IF ( lvls .eq. 2 ) THEN
				vs = ( rlnpi (ij,1) - p (1) ) /
     +				          ( p (1) - p (2) )
				vs = vs * ( v (1) - v (2) )
				sfcval (ij,ip) = vs + v(1)
				icnt = icnt + 1
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO
C
C*	Load temperature.
C
	IF ( ip .eq. 2 .and. icnt .ne. 0 ) THEN
	    DO ij = 1, kxky
		tlast (ij) = sfcval (ij,ip)
	    END DO
	    IF ( lpmrds (3) .and. lpmrds (1) ) buildz = .true.
	ELSE IF ( ip .eq. 2 .and. icnt .eq. 0 ) THEN
	    buildz = .false.
	END IF
C*
	RETURN
	END
