	SUBROUTINE VI_POOL  ( ilvo, lt, lb, 
     +                        ovctop, ovcbtm, sfcval,
     +                        rlnpi, rlnpo, valu, pln, iret )
C************************************************************************
C* VI_POOL								*
C*									*
C* This subroutine computes ln (p) on an output level and stores it	*
C* in the internal array.						*
C*									*
C* VI_POOL ( ILVO, LT, LB, OVCTOP, OVCBTM, SFCVAL, RLNPI, RLNPO,        * 
C*           VALU, PLN, IRET )                                          *
C*									*
C* Input parameters:							*
C*	ILVO		INTEGER		Output level # 			*
C*	LT		INTEGER		Top level index			*
C*	LB		INTEGER		Bottom level index		*
C*	OVCTOP		REAL(*) 	upper vc values	                *
C*	OVCBTM		REAL(*) 	lower vc values	                *
C*
C* Input/Ouput parameters:                                              *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      RLNPO(kxky,nlo) REAL            ln (p) on output lvls           *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*	PLN(kxky)       REAL            previously local array          *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		07/92	Added p on z above ground		*
C* R. Miller/COMET   	06/94   Changed 100000 to 10000 for eta sgma    *
C* G. Hull/SAIC         03/08   Changed ovcbuf indxs to ovctop, ovcbtm  *
C************************************************************************
	INCLUDE		'vicmn.cmn'

	REAL		ovctop (kxky), ovcbtm (kxky)
	REAL		sfcval(kxky,np)
	REAL            rlnpi (kxky,nli), rlnpo (kxky,nlo)
	REAL		valu (kxky,nli), pln (kxky)
C*
	LOGICAL		exist
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	exist = .false.
C
C*	Load output array with missing values.
C
	DO ij = 1, kxky
	    pln (ij) = RMISSD
	END DO
C*
	rlvo = levout ( ilvo )
C
C*	Check the output vertical coordinate and compute it.
C
	IF ( vcordo .eq. NPRES ) THEN
	    exist = .true.
	    DO ij = 1, kxky
		pln (ij) = rlvo
	    END DO
	ELSE IF ( vcordo .eq. NHGHT ) THEN
	    exist = .true.
	    CALL VC_PONZ ( rlvo, ovctop, ovcbtm,
     +                     valu (1,lt), valu (1,lb),
     +                     rlnpi (1,lt), rlnpi (1,lb), 
     +                     kxky, pln, iret )
C*
	ELSE IF ( vcordo .eq. NTHTA ) THEN
	    exist = .true.
	    CALL VC_PONT ( rlvo, ovctop, ovcbtm,
     +                     valu (1,lt), valu (1,lb),
     +                     rlnpi (1,lt), rlnpi (1,lb), 
     +			   kxky, pln, iret )
C*
	ELSE IF ( vcordo .eq. NSGMA ) THEN
	    IF ( lpmrds (1) ) THEN
	        exist = .true.
		rlevo = rlvo / 10000.
	    	CALL VC_PFSG ( rlevo, sfcval, ptopo, kxky,
     +			       pln, iret )
	    END IF
C*
	ELSE IF ( vcordo .eq. NETA ) THEN
	    IF ( lpmrds (1) .and. lpmrds (3) ) THEN
		exist = .true.
		rlevo = rlvo / 10000.
		CALL VC_PFET ( rlevo, sfcval,
     +                         sfcval( 1,3 ),
     +                         ptopo, kxky, pln, iret )
	    END IF
C*
	ELSE IF ( vcordo .eq. NZAGL .and. lpmrds (3) ) THEN
	    exist = .true.
C
C*	    VC_PONZ can be used here because it computes only dz
C*	    which means that the offset surface value subtracts out.
C
	    CALL VC_PONZ ( rlvo, ovctop, ovcbtm,  
     +                     valu (1,lt), valu (1,lb),
     +                     rlnpi (1,lt), rlnpi (1,lb),
     +                     kxky, pln, iret )
C*
	END IF

C*
	IF ( .not. exist .or. iret .ne. 0 ) THEN
	    iret = +2
	ELSE
C
C*	    Load the ln (p) array.
C
	    DO ij = 1, kxky
	    	IF ( .not. ERMISS ( pln (ij) ) ) THEN
     		    rlnpo (ij,ilvo ) = ALOG ( pln (ij) )
		    IF ( lpmrds (1) .and. rlnpo (ij,ilvo) .ge.
     +			 rlnpi (ij,1) ) THEN
			rlnpo (ij,ilvo) = RMISSD
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
