	SUBROUTINE VG_WSFC  ( cmn_data, sfcval, rdwrbuf, iret )
C************************************************************************
C* VG_WSFC								*
C*									*
C* This subroutine write the surface data to the output file.		*
C*									*
C* VG_WSFC ( CMN_DATA, SFCVAL, IRET )					*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C**									*
C* Log:									*
C* K. Brill/NMC      	06/92						*
C* K. Brill/NMC		08/92	Convert ln (q) to q			*
C* K. Brill/NMC		10/92	Send other surface parms to output file *
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* R. Tian/SAIC		 4/05	Added GD_OPEN to get input/output file #*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'

	REAL            cmn_data (*), sfcval (kxky,np)
C* this is a temporary buffer used to read and then write out grids.
C
	REAL            rdwrbuf(kxky)

	INTEGER		ighdr ( LLGDHD ), lev (2)
	CHARACTER	ppp*12
	CHARACTER*20	time (2)
	LOGICAL		done
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli,
     +                 adum1, adum2, mxgrd, iret )
	CALL GD_OPEN ( gdcuro, .true., 0, 0, igdflo,
     +                 adum1, adum2, mxgrd, iret )
C
C*	Determine packing information, and write the surface grids.
C
        DO ii = 1, LLGDHD
            ighdr ( ii ) = 0
        END DO
C
	lev (1) = 0
	lev (2) = -1
	DO ip = 1, np
	  IF ( lpmrds (ip) ) THEN
	    IF ( ip .eq. mstprm ) THEN
		ppp = 'SPFH'
		DO ij = 1, kxky
		    IF ( .not. ERMISS ( sfcval (ij,ip) ) )
     +			sfcval (ij,ip) = 
     +                                   EXP ( sfcval (ij,ip) )
		END DO
	    ELSE
		ppp = parms (ip)
	    END IF

	    CALL VC_NBTS ( sfcval (1,ip), kxky, 4, nbits, ier )

	    CALL GD_WPGD ( igdflo, sfcval (1,ip),  kx, ky, ighdr,	
     +			   gdttm, lev, igvco, ppp, .false.,
     +			   MDGGRB, nbits, ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'GD', ier, ' ', irr )
	    END IF
	  END IF
	END DO
C
C*	Write the output vertical coordinate values on the surface.
C
	IF ( sfcovc ) THEN
	    CALL VC_NBTS ( cmn_data( ovcsfc_indx ), kxky, 4, nbits, ier )
	    CALL GD_WPGD ( igdflo, cmn_data( ovcsfc_indx ), 
     +                     kx, ky, ighdr, gdttm, lev,
     +		           igvco, vcordo, .false., MDGGRB, nbits, ier )
	END IF
C
C*	Write out any other parameters with vertical coordinates 
C*	listed in SFVCLT.
C*
	IF ( nsfvc .ne. 0 ) THEN
	    done = .false.
	    igrd = 1
	    DO WHILE ( .not. done )
C
C*	    	Get next grid header.
C
	    	CALL GD_GIDN ( igdfli, igrd, time, lev, jvc, ppp, ier )
	    	IF ( ier .eq. 0 .and. time (1) .eq. gdttm (1) ) THEN	
		    DO i = 1, nsfvc
			IF ( isfvc (i) .eq. jvc ) THEN
C
C*			    Write out this data.
C
			    CALL VG_READ ( igdfli, sfvclt (i), lev (1), 
     +					   ppp, sfcval, 
     +					   cmn_data( vgread_buf_indx ),
     +                                     rdwrbuf, ier )
			    IF ( ier .eq. 0 ) THEN
	    			CALL VC_NBTS ( rdwrbuf, kxky,
     +						4, nbits, ier )
	    			CALL GD_WPGD ( igdflo, rdwrbuf,
     +						kx, ky,
     +						ighdr, gdttm, lev,
     +		           			jvc, ppp, .false.,
     +						MDGGRB, nbits, ier )
			    END IF
			END IF
		    END DO
	        ELSE
		    IF ( ier .ne. 0 ) done = .true.
	    	END IF
		igrd = igrd + 1
	    END DO
	END IF
C*
	RETURN
	END
