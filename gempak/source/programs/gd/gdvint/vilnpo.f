	SUBROUTINE VI_LNPO  ( cmn_data, 
     +                        ovcsfc, ovcbuf1, ovcbuf2, 
     +                        rlnpi, rlnpo, valu, grid, iret )
C************************************************************************
C* VI_LNPO								*
C*									*
C* This subroutine computes log pressure on the output levels.  This	*
C* routine loads the RLNP (*,NLP+k) array.				*
C*									*
C* VI_LNPO ( CMN_DATA, OVCSFC, OVCBUF1, OVCBUF2, RLNPI, RLNPO, VALU,    *
C*           GRID, IRET )						*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      OVCSFC(kxky)    REAL            output vc value on sfc          *
C*      OVCBUF1(kxky)   REAL            buffer 1 for out vc             *
C*      OVCBUF2(kxky)   REAL            buffer 2 for out vc             *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      RLNPO(kxky,nlo) REAL            ln (p) on output lvls           *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*      GRID (kxky)     REAL            previously a local array        *
C*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		08/92						*
C* K. Brill/NMC		09/92	Reset to surface for underground pts	*
C* T. Lee/GSC		 9/97	Set index of LPMRDS to 2		*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		cmn_data (*) 
	REAL		ovcsfc (kxky), ovcbuf1 (kxky), ovcbuf2 (kxky)
	REAL		rlnpi (kxky,nli), rlnpo (kxky,nlo) 
	REAL            valu (kxky,nli)

	REAL		grid (*) 
	REAL            test (MAXLVL)
	LOGICAL		betwen, switch_updn
	INCLUDE		'ERMISS.FNC'
C*
	BETWEN ( rlev, rlo, rhi ) = 
     +     ( ( rlev .ge. rlo .and. rlev .le. rhi )   .or.
     +       ( rlev .le. rlo .and. rlev .ge. rhi ) )
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set all the level check flags to true.
C
	DO k = 1, MAXLVL
	   uselvl (k) = .true.
	   intlvl (k) = .true.
	END DO
C
C*	Compute the output coordinate values on the surface.
C
	CALL VI_COVC ( 1, cmn_data, ovcsfc, 
     +                 cmn_data( sfcval_indx ), 
     +                 rlnpi, valu, ovcbuf1, iret )

	IF ( iret .ne. 0 ) THEN
	    uselvl (1) = .false.
	    sfcovc = .false.
	    switch_updn = .false.
	    m1 = 0
	ELSE
C
C*	    Store this value.
C
	    sfcovc = .true.
	    DO ij = 1, kxky
	        ovcsfc (ij) = ovcbuf1 (ij)
	    END DO
C
C*	    Find the maximum and minium output coordinate values.
C
	    CALL VI_MXMN ( vcordo, ovcbuf1, kxky, rhi, rlo, iret )
	    vclo (1) = rlo
	    vchi (1) = rhi
C
C*	    Set switching indexes.
C
	    switch_updn = .true.
C
C*	    Set pointer to last good level.
C
	    m1 = 1
	END IF
C
C*	Initialize the array TEST which allows a check for
C*	the existence of pressure on the output levels.
C
	DO kou = 1, nlo
	    test (kou) = 1.
	END DO
C
C*	Loop over all the input LAYERS.
C*	    Compute the output coordinate values on the upper surface.
C
	DO kin = 2, nli
	    IF ( switch_updn ) THEN
	        CALL VI_COVC ( kin, cmn_data, ovcsfc, 
     +                         cmn_data( sfcval_indx ),
     +                         rlnpi, valu, ovcbuf2, iret )
	    ELSE
	        CALL VI_COVC ( kin, cmn_data, ovcsfc, 
     +                         cmn_data( sfcval_indx ),
     +                         rlnpi, valu, ovcbuf1, iret )
	    END IF

	    IF ( iret .ne. 0 ) THEN
	 	uselvl (kin) = .false.
	    ELSE 
		IF ( switch_updn ) THEN
	           CALL VI_MXMN ( vcordo, ovcbuf2, kxky, rhi, rlo, ier )
	        ELSE 
		   CALL VI_MXMN ( vcordo, ovcbuf1, kxky, rhi, rlo, ier )
	        END IF

	    	vclo (kin) = rlo
	    	vchi (kin) = rhi
		IF ( m1 .ne. 0 ) THEN
C
C*	    	    Find ln(p) on all output levels in the layer
C*		    between KIN and KIN - 1.
C
	    	    DO kou = 1, nlo
	            	rlvo = levout ( kou )
C
C*		    	Check the output vertical coordinate and compute
C*			ln(p) on the output surface.
C
		    	IF ( BETWEN
     +			  ( rlvo, vclo (m1), vchi (kin) ) ) THEN

		            IF ( switch_updn ) THEN
		                CALL VI_POOL ( kou, kin, m1, 
     +					       ovcbuf2, ovcbuf1, 
     +                                         cmn_data( sfcval_indx ),
     +					       rlnpi, rlnpo, valu, 
     +                                         cmn_data ( pln_indx ),
     +                                         ier )
		            ELSE 
		                CALL VI_POOL ( kou, kin, m1, 
     +					       ovcbuf1, ovcbuf2, 
     +                                         cmn_data( sfcval_indx ),
     +					       rlnpi, rlnpo, valu,
     +                                         cmn_data ( pln_indx ), 
     +                                         ier )
		            END IF

			    test (kou) = test (kou) * ier
			END IF
	            END DO
		END IF
		m1 = kin
	    	switch_updn = .not. switch_updn
	    END IF
	END DO
C
C*	Set INTLVL interpolation flag.
C
	DO kou = 1, nlo
	    IF ( test (kou) .ne. 0 ) THEN
		intlvl (kou) = .false.
	    ELSE
		intlvl (kou) = .true.
	    END IF
	END DO
C
C*	Assign underground points surface values.
C
	DO k = 2, nli
	    IF ( subsfc (k) .and. lpmrds (1) ) THEN
		DO ij = 1, kxky
		    IF ( .not. ERMISS ( rlnpi (ij,1) ) .and.
     +			 .not. ERMISS ( rlnpi (ij,k) ) ) THEN
			IF ( rlnpi (ij,k) .gt. rlnpi (ij,1) ) THEN
			    rlnpi (ij,k) = rlnpi (ij,1)
			    IF ( lpmrds (2) ) THEN
				valu (ij,k) = valu (ij,1)
			    ELSE
				valu (ij,k) = RMISSD
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO
C
C*	Write out pressure if required.
C
	IF ( lprmwr (1) ) THEN
	    DO kou = 1, nlo
		IF ( intlvl (kou) ) THEN
		    DO ij = 1, kxky
			IF ( lorenz .and. sfcovc .and.
     +			     .not. ERMISS ( rlnpi (ij,1) )
     +			     .and. .not. ERMISS ( ovcsfc (ij) ) .and.
     +			     ERMISS ( rlnpo (ij,kou) ) .and.
     +			     ( FLOAT ( levout (kou) ) .lt.
     +			     			ovcsfc (ij) ) ) THEN
C
C*			    Set the Lorenz coundition if needed.
C*			    Note that the pressure must be missing
C*			    for this condition to apply.
C
			    grid (ij) = EXP ( rlnpi (ij,1) )
C*
			ELSE IF
     +			 ( .not. ERMISS ( rlnpo (ij,kou) ) ) THEN
			    grid (ij) = EXP ( rlnpo (ij,kou) )
			ELSE
			    grid (ij) = RMISSD
			END IF
		    END DO
		    CALL VG_WRIT ( 1, kou, grid,
     +                     cmn_data( ovcsfc_indx ), 
     +                     cmn_data( sfcval_indx ),
     +                     cmn_data( rlnpo_indx ), iret )
		END IF
	    END DO
	END IF
C*
	RETURN
	END
