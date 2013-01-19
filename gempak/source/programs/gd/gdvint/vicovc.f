	SUBROUTINE VI_COVC  ( ilev, cmn_data, ovcsfc, sfcval, 
     +                        rlnpi, valu, ovcv, iret )
C************************************************************************
C* VI_COVC								*
C*									*
C* This subroutine computes a grid of output coordinate values on an	*
C* input coordinate surface using current grid values of ln (p) and	*
C* temperature in RLNP and VALU.					*
C*									*
C* VI_COVC ( ILEV, CMN_DATA, OVCSFC, SFCVAL, RLNPI, VALU, OVCV, IRET )	*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level index number (1=sfc)	*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      OVCSFC(kxky)    REAL            output vc value on sfc          *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*									*
C* Output parameters:							*
C*	OVCV (*)	REAL		Output vc values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		cmn_data (*)
	REAL		ovcsfc (kxky), sfcval (kxky,np)
	REAL		rlnpi (kxky,nli), valu (kxky,nli)
	REAL		ovcv (*)

	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C*
	IF ( ( vcordo .ne. NHGHT .and. vcordo .ne. NZAGL )
     +		.and. havlnp (ilev ) ) THEN
C
C*	    Convert ln(p) to pressure and store temporarily.
C
	    DO ij = 1, kxky
	    	IF ( .not. ERMISS ( rlnpi (ij,ilev) ) ) THEN
     	  	    ovcv (ij) = EXP ( rlnpi (ij,ilev) )
	    	ELSE
		    ovcv (ij) = RMISSD
	    	END IF
	    END DO 
	ELSE IF ( .not. havlnp (ilev) ) THEN
	    iret = 2
	    RETURN
	END IF
C
C*	Check the output vertical coordinate and compute it.
C
	IF ( vcordo .eq. NPRES ) THEN
C
C*	    Do nothing, pressure is already in OVCV.
C
	ELSE IF ( vcordo .eq. NHGHT  ) THEN
	    IF ( ilev .eq. 1 .and. lpmrds (3) ) THEN
		DO ij = 1, kxky
		    ovcv (ij) = sfcval (ij,3)
		END DO
	    ELSE IF ( ilev .eq. 1 ) THEN
		iret = 2
		RETURN
	    ELSE
		CALL VI_GETZ ( ilev, .false., .true., cmn_data,
     +                         sfcval, rlnpi, valu, 
     +                         cmn_data( plast_indx ), 
     +                         cmn_data( tlast_indx ), 
     +                         cmn_data( zlast_indx ), 
     +                         cmn_data( hgtbuf_indx ), ovcv, iret )
	    END IF
	ELSE IF ( vcordo .eq. NTHTA ) THEN
	    IF ( havtmp (ilev ) ) THEN 
		CALL VC_THTA ( ovcv, valu (1,ilev), kxky, ovcv, iret )
	    ELSE
		iret = 2
	    END IF
	ELSE IF ( vcordo .eq. NSGMA ) THEN
	    IF ( havlnp (1) ) THEN
	        CALL VC_SGMA ( ovcv, sfcval (1,1), ptopo, kxky,
     +		               ovcv, iret )
	    ELSE
		iret = 2
	    END IF
	ELSE IF ( vcordo .eq. NETA ) THEN
	    IF ( havlnp (1) ) THEN
		CALL VC_ETAC ( ovcv, sfcval (1,1), sfcval (1,3),
     +			       ptopo, kxky, ovcv, iret )
	    ELSE
	   	iret = 2
	    END IF
	ELSE IF ( vcordo .eq. NZAGL ) THEN
	    IF ( ilev .eq. 1 ) THEN
		DO ij = 1, kxky
		    ovcv (ij) = 0.0
		END DO
	    ELSE
		CALL VI_GETZ ( ilev, .false., .true., cmn_data,
     +                         sfcval, rlnpi, valu, 
     +                         cmn_data( plast_indx ), 
     +                         cmn_data( tlast_indx ), 
     +                         cmn_data( zlast_indx ), 
     +                         cmn_data( hgtbuf_indx ), ovcv, iret )
		IF ( iret .eq. 0 .and. lpmrds (3) ) THEN
		    DO ij = 1, kxky
		    	IF ( .not. ERMISS ( ovcv (ij) ) .and.
     +		             .not. ERMISS ( sfcval (ij,3) ) ) THEN
			    ovcv (ij) = ovcv (ij) - sfcval (ij,3)
		    	ELSE
			    ovcv (ij) = RMISSD
		    	END IF
		    END DO
		ELSE
		    iret = 2
	    	END IF
	    END IF
	END IF
C*
	IF ( ilev .ne. 1 .and. subsfc (ilev) .and. sfcovc ) THEN
C
C*	    If the input coordinate surface is underground; then,
C*	    set the output vertical coordinate value to that at
C*	    the surface, if it is not missing.
C*
C*	     The ln(p) and temperatures (valu) must also be set
C*	     to surface values for consistency in VI_POOL.  This
C*	     does not adversely affect VI_GETZ because it has
C*	     already stored the last values in the _LAST arrays.
C
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( rlnpi (ij,1) ) .and.
     +		     .not. ERMISS ( rlnpi (ij, ilev) ) .and.
     +		     .not. ERMISS ( ovcsfc (ij) ) .and.
     +		     ( rlnpi (ij,ilev) .gt. rlnpi (ij,1) ) ) THEN
		    ovcv (ij) = ovcsfc (ij)
		    rlnpi (ij,ilev) = rlnpi (ij,1)
		    valu (ij,ilev) = valu (ij,1)
		END IF
	    END DO
	END IF
C*
	RETURN
	END
