	SUBROUTINE VI_GETP  ( lev, cmn_data,
     +                        sfcval, rlnpi, valu, undgrd, iret )
C************************************************************************
C* VI_GETP								*
C*									*
C* This subroutine obtains the pressure on input level LEV.		*
C*									*
C* VI_GETP ( LEV, CMN_DATA, SFCVAL, RLNPI, VALU, UNDGRD, IRET )		*
C*									*
C* Input parameters:							*
C*	LEV		INTEGER		Level number			*
C*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data  			*
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*									*
C* Output parameters:							*
C*	UNDGRD		LOGICAL		Flag for underground points	*
C*	IRET		INTEGER		Return code			*
C*					 +3 = no pressure		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* R. Miller/COMET   06/94  Changed 100000 to 10000 for eta and sgma	*
C* K. Brill/EMC	     11/95  Pass sfcval(1,3) into VC_PFET		*
C* R. Tian/SAIC	      4/05  Added GD_OPEN to get input file number	*
C* G. Hull/SAIC      03/08  Add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL            cmn_data (*)
	REAL            sfcval (kxky,np)
	REAL            rlnpi (kxky,nli), valu(kxky,nli)
	LOGICAL		undgrd
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	undgrd = .false.
C*
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
	CALL VG_READ ( igdfli, vcordi, levin (lev), NPRES,
     +		       sfcval, cmn_data( vgread_buf_indx ),
     +		       rlnpi (1,lev), iret )
	IF ( iret .ne. 0 ) THEN
C
C*	   Pressure must be computed on this level.
C
	    IF ( vcordi .eq. 'NPRES' ) THEN
		pppp = FLOAT ( levin (lev) )
		DO ij = 1, kxky
		    rlnpi (ij,lev) = pppp
		END DO
		iret = 0
C*
	    ELSE IF ( vcordi .eq. NTHTA ) THEN

		CALL VI_RDTK ( lev, cmn_data, rlnpi,
     +                         cmn_data( aprs_indx ),
     +                         valu (1,lev), iret )

		IF ( iret .eq. 0 ) THEN
		    havtmp (lev) = .true.
		    theta = FLOAT ( levin (lev) )
		    CALL VC_PFTH ( theta, valu (1,lev), kxky,
     +				   rlnpi (1,lev), iret )
		ELSE
		    iret = +3
		END IF
C*
	    ELSE IF ( vcordi .eq. NSGMA ) THEN
		sigv = FLOAT ( levin (lev) ) / 10000.
		CALL VC_PFSG ( sigv, sfcval (1,1), ptopi, kxky,
     +			       rlnpi (1,lev), iret )
C*
	    ELSE IF ( vcordi .eq. NETA ) THEN
		etav = FLOAT ( levin (lev) ) / 10000.
		CALL VC_PFET ( etav, sfcval (1,1), sfcval (1,3),
     +			       ptopi, kxky, rlnpi (1,lev), iret )
C*
	    ELSE
		iret = +3
	    END IF

C*
	END IF
C*
	IF ( iret .eq. 0 ) THEN
	    havlnp (lev) = .true.
C
C*	    Load current log of pressure array.
C
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( rlnpi (ij,lev) ) ) THEN
		    rlnpi (ij,lev) = ALOG ( rlnpi (ij,lev) )
		    IF ( lpmrds (1) .and. 
     +			 .not. ERMISS ( rlnpi (ij,1) ) ) THEN
		    	IF ( rlnpi (ij,lev) .ge. rlnpi (ij,1) ) THEN
C
C*			    All underground points are assigned surface
C*			    values later in VI_LNPO.  Here, if a point
C*			    on an input level is exactly on the surface;
C*			    then, that point is lifted by a tiny amount
C*			    so that it has a pressure slightly less than
C*			    that at the surface.  Also, if a level has
C*			    points underground, it is flagged.
C
			    IF ( rlnpi (ij,lev) .eq. rlnpi (ij,1) ) THEN
				rlnpi (ij,lev) = rlnpi (ij,lev) - .00005
			    ELSE
			    	undgrd = .true.
			    END IF
			END IF
		    END IF
		END IF
	    END DO
	ELSE
	    havlnp (lev) = .false.
	END IF

	RETURN
	END
