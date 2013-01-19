	SUBROUTINE VI_VINT  ( ip, cmn_data, ovcbuf1, ovcbuf2, 
     +                        rlnpi, rlnpo, valu, iret )

C************************************************************************
C* VI_VINT								*
C*									*
C* This subroutine performs the vertical interpolation.			*
C*									*
C* VI_VINT ( IP, CMN_DATA, OVCBUF1, OVCBUF2, RLNPI, RLNPO, VALU, IRET )	*
C*									*
C* Input parameters:							*
C*	IP		INTEGER		Parameter index number		*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      OVCBUF1(kxky)   REAL            buffer 1 for out vc             *
C*      OVCBUF2(kxky)   REAL            buffer 2 for out vc             *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      RLNPO(kxky,nlo) REAL            ln (p) on output lvls           *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		08/92						*
C* K. Brill/NMC		11/92	Remove found flag			*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL            cmn_data (*)
	REAL            ovcbuf1 (kxky), ovcbuf2 (kxky)
	REAL            rlnpi (kxky,nli), rlnpo (kxky,nlo)
	REAL            valu (kxky,nli)

	LOGICAL		done, betwen
	INTEGER		kpt ( MAXLVL )
C*
	INCLUDE		'ERMISS.FNC'
C*
	BETWEN ( rlev, rlo, rhi ) = 
     +     ( ( rlev .ge. rlo .and. rlev .le. rhi )   .or.
     +       ( rlev .le. rlo .and. rlev .ge. rhi ) )
C-----------------------------------------------------------------------
	iret = 0
C
C*	If IP = 2, compute ln(p) on all OUTPUT levels.
C
	IF ( ip .eq. 2 ) THEN
	    havtmp (1) = lpmrds (2)

	    CALL VI_LNPO ( cmn_data, cmn_data( ovcsfc_indx ),
     +              ovcbuf1, ovcbuf2, rlnpi, rlnpo,
     +              valu, cmn_data( grid_indx ), ier )
	END IF

	IF ( .not. lprmwr (ip) ) RETURN
C
C*	Make the list of pointers to valid input levels.
C
	klv = 0
	DO i = 1, nli
	    IF ( lprmrd (i) .and. uselvl (i) ) THEN
		klv = klv + 1
		kpt ( klv ) = i
	    END IF
	END DO
C
C*	Check to make sure there are enough levels.
C
	IF ( klv .lt. 3 ) THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Loop over all the output levels.
C
	DO klev = 1, nlo
C
C*	  Check interpolation flag for this output level.
C
	  IF ( intlvl (klev) ) THEN
	    olev = FLOAT ( levout (klev) )
C
C*	    Assign missing values to the output grid.  OVCBUF (*,2)
C*	    stores the immediate values and OVCBUF (*,1) holds
C*	    the final result.
C
	    DO ij = 1, kxky
		ovcbuf1 (ij) = RMISSD
	    END DO
C
C*	    Loop over enough input levels to complete the output surface.
C
	    kkk = 1
	    done = .false.
	    DO WHILE ( .not. done )
C*
		kkk = kkk + 1
		kin = kpt ( kkk )
		kinm1 = kpt ( kkk - 1 )
		IF ( kkk .gt. 2 ) kinm2 = kpt ( kkk - 2 )
		IF ( kkk .ne. klv ) kinp1 = kpt ( kkk + 1 )
C*
		IF ( BETWEN ( olev, vclo (kinm1), vchi (kin) ) ) THEN
C
C*		    To save time, determine the number of input levels
C*		    that are really needed on the first call.
C
		    IF ( icalvi .eq. 1 ) THEN
			newnli = kkk + 1
			IF ( newnli .gt. nli ) newnli = nli
		    END IF
C*
		    IF ( kkk .eq. 2 ) THEN
		 	IF ( ip .eq. 3 .and. klv .gt. 2 ) THEN
			   CALL VC_QUAD ( rlnpi (1,kinm1),
     +                                    rlnpi (1,kin),
     +				          rlnpi (1,kinp1),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm1),
     +                                    valu (1,kin),
     +				          valu (1,kinp1),
     +					  kxky, ovcbuf2, ier )
			ELSE
			   CALL VC_LNIN ( rlnpi (1,kinm1),rlnpi (1,kin),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm1),
     +					  valu (1,kin), kxky,
     +					  ovcbuf2, ier )
			END IF
		    ELSE IF ( kkk .eq. klv ) THEN
			IF ( ip .eq. 3 .and. klv .gt. 2 ) THEN
			   CALL VC_QUAD ( rlnpi (1,kinm2),
     +                                    rlnpi (1,kinm1),
     +					  rlnpi (1,kin),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm2), 
     +					  valu (1,kinm1),
     +				          valu (1,kin),
     +					  kxky, ovcbuf2, ier )
			ELSE
			   CALL VC_LNIN ( rlnpi (1,kinm1),
     +                                    rlnpi (1,kin),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm1),
     +					  valu (1,kin), kxky,
     +					  ovcbuf2, ier )
			END IF
		    ELSE 
C
C*			This section can accomodate a 4-level scheme
C*			in the future.
C
			IF ( ip .eq. 3 ) THEN
			   CALL VC_QUAD ( rlnpi (1,kinm1),
     +                                    rlnpi (1,kin),
     +				          rlnpi (1,kinp1),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm1),
     +                                    valu (1,kin),
     +				          valu (1,kinp1),
     +					  kxky, ovcbuf2, ier )
			ELSE 
C
C*			    Just do 2-level linear interpolation now.
C
			   CALL VC_LNIN ( rlnpi (1,kinm1),
     +                                    rlnpi (1,kin),
     +					  rlnpo (1,klev),
     +					  valu (1,kinm1),
     +					  valu (1,kin), kxky,
     +					  ovcbuf2, ier )
     
		    	END IF
		    END IF
C
C*		    Load non-missing values to output array.
C
		    DO ij = 1, kxky
			IF ( .not. ERMISS ( ovcbuf2 (ij) ) )
     +			    ovcbuf1 (ij) = ovcbuf2 (ij)
		    END DO
C*
		END IF
C*
		IF ( kkk .eq. klv ) done = .true.
C*
	    END DO
C
C*	    This grid is done --- write it out.
C
	    CALL VG_WRIT ( ip, klev, ovcbuf1, 
     +                     cmn_data( ovcsfc_indx ),
     +                     cmn_data( sfcval_indx ),
     +                     cmn_data( rlnpo_indx ), ier )
C*
	  ELSE
	    WRITE (6,*) parms (ip), ' not interpolated to level(',klev,
     +                  ') = ', levout (klev)
	  END IF
	END DO

C	IF ( icalvi .eq. 1 ) THEN
C	    nli = newnli
C	END IF
	icalvi = -999
C*
	RETURN
	END
