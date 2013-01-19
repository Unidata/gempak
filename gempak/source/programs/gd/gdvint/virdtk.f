	SUBROUTINE VI_RDTK  ( ilev, cmn_data, rlnpi, aprs, temp, iret )
C************************************************************************
C* VI_RDTK								*
C*									*
C* This subroutine reads in temperature (K) on input level number ILEV. *
C*									*
C* VI_RDTK ( ILEV, CMN_DATA, RLNPI, APRS, TEMP, IRET )			*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level number			*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      APRS (*)        REAL            previously local array          *
C*									*
C* Output parameters:							*
C*	TEMP(*)		REAL		Temperature (K)			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      06/92						*
C* K. Brill/HPC		11/02	Eliminate use of the subset flag	*
C* R. Tian/SAIC		 4/05	Added GD_OPEN to get input file number	*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		cmn_data (*)
	REAL		rlnpi (kxky,nli)
	REAL		aprs (kxky)
	REAL		temp (*)
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
C
C*	Check to see if this level exists.
C
	IF ( ilev .gt. nli ) THEN
	    iret = 999
	    RETURN
	END IF
C
C*	Try Kelvin temperature first.
C
	CALL VG_READ ( igdfli, vcordi, levin (ilev), NTMPK,
     +	       	       cmn_data( sfcval_indx ),
     +	       	       cmn_data( vgread_buf_indx ), temp, iret )
C*
	IF ( iret .ne. 0 ) THEN
C
C*	    Convert other thermodynamic quantity to Kelvin temperature.
C
	   CALL VG_READ ( igdfli, vcordi, levin(ilev), NTMPC,
     +		          cmn_data( sfcval_indx ), 
     +         	          cmn_data( vgread_buf_indx ), temp, iret )
	   IF ( iret .eq. 0 ) CALL PD_TMCK ( temp, kxky,
     +	       	       			     temp, iret )
	   IF ( iret .ne. 0 .and. havlnp (ilev) ) THEN
		CALL VG_READ ( igdfli, vcordi, levin (ilev), NTHTA,
     +			       cmn_data( sfcval_indx ),
     +         	               cmn_data( vgread_buf_indx ), temp, iret )
	   	IF ( iret .eq. 0 ) THEN
		    DO ij = 1, kxky
			IF ( .not. ERMISS ( rlnpi (ij,ilev) ) )
     +			    aprs (ij) = EXP ( rlnpi (ij,ilev) )
		    END DO
		    CALL PD_TMPK ( aprs, temp,
     +		                   kxky, temp, iret )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
