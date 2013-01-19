	SUBROUTINE VI_RDIN  ( ip, cmn_data, sfcval, rlnpi, valu, iret )
C************************************************************************
C* VI_RDIN								*
C*									*
C* This subroutine reads in all input levels of a single parameter.	*
C*									*
C* VI_RDIN ( IP, CMN_DATA, SFCVAL, RLNPI, VALU, IRET )			*
C*									*
C* Input parameters:							*
C*	IP		INTEGER		Parameter number		*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* R. Tian/SAIC	     04/05	Added GD_OPEN to get input file number	*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	INTEGER       ip
	REAL          cmn_data (*), sfcval(kxky,np)
	REAL          rlnpi (kxky,nli), valu (kxky,nli)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Save level 1 for the surface value.
C
	DO ilev = 2, nli
C
C*	    Set the parameter read flag to .false.
C
	    lprmrd (ilev) = .false.
C*
	    IF ( ip .le. 1 ) THEN
C
C*		Do nothing because pressure and temperature are done
C*		at the same time.
C
	        RETURN
C*
	    ELSE IF ( ip .eq. 2 ) THEN
C
C*		Read in pressure and temperature jointly.
C
		havtmp (ilev) = .false.
		CALL VI_GETP ( ilev, cmn_data, sfcval,
     +                         rlnpi, valu, subsfc (ilev), ier )
		
	    	IF ( .not. havtmp (ilev) ) THEN
C
C*	    	    Read in temperature parameter.
C
		    CALL VI_RDTK ( ilev, cmn_data, rlnpi,
     +                             cmn_data( aprs_indx ),
     +                             valu (1,ilev), ier )
C 
		    IF ( ier .eq. 0 ) THEN
		    	havtmp (ilev) = .true.
	    	    	lprmrd (ilev) = .true.
		    END IF
	   	END IF
C*
	    ELSE IF ( ip .eq. 3 ) THEN
C
C*		Get the height, both temperature and pressure
C*		are availble on the input levels and can be used
C*		to integrate height.  Height values will replace
C*		the temperature values.
C
	    	CALL VI_GETZ ( ilev, .true., .true., cmn_data,
     +                         sfcval, rlnpi, valu, 
     +                         cmn_data( plast_indx ),
     +                         cmn_data( tlast_indx ),
     +                         cmn_data( zlast_indx ),
     +                         cmn_data( hgtbuf_indx ),
     +                         cmn_data( ovcbuf1_indx ), ier )

	    	IF ( ier .eq. 0 ) lprmrd (ilev) = .true.
C*
	    ELSE IF ( ip .eq. mstprm ) THEN
C
C*		Get the ln(q) values so that moisture is interpolated
C*		as a power law relation with pressure.
C
		CALL VI_GETQ ( ilev, 1, cmn_data, rlnpi,
     +                         cmn_data( sphbuf_indx ),
     +                         cmn_data( tmpbuf_indx ),
     +                         valu (1,ilev), ier )

		IF ( ier .eq. 0 ) lprmrd (ilev) = .true.

		havtmp (ilev) = .false.
	    ELSE 
C
C*		Read in any other parameter.
C
		havtmp (ilev) = .false.
		CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                         adum2, mxgrd, iret )

	        CALL VG_READ ( igdfli, vcordi, levin (ilev), parms (ip),
     +			       sfcval, cmn_data( vgread_buf_indx ), 
     +                         valu (1,ilev), ier )
	    	IF ( ier .eq. 0 ) lprmrd (ilev) = .true.
C*
	    END IF
	END DO
C
C*	Build down to the ground if necessary.
C
	IF ( .not. lpmrds (ip) .and. ip .ne. 3 ) THEN
	    CALL VI_BDDN ( ip, sfcval, rlnpi, valu,
     +                     cmn_data( tlast_indx ), ier )
	    IF ( ier .eq. 0 ) lpmrds (ip) = .true.
	END IF
C
C*	Place surface values as the first input level.
C
	IF ( lpmrds (ip) ) THEN
	    lprmrd (1) = .true.
	    IF ( ip .eq. 2 ) havtmp (1) = .true.
	    DO ij = 1, kxky
		valu (ij,1) = sfcval (ij,ip)
	    END DO
	ELSE
	    lprmrd (1) = .false.
	    IF ( ip .eq. 2 ) havtmp (1) = .false.
	    DO ij = 1, kxky
		valu (ij,1) = RMISSD
	    END DO
	END IF
C
C*	Set underground points to surface value unless ip = 2.
C
	IF ( ip .gt. 2 ) THEN
	    DO ilev = 2, nli
	    	IF ( subsfc (ilev) ) THEN
		  DO ij = 1, kxky
		    IF ( rlnpi (ij,1) .eq. rlnpi (ij,ilev) )
     +			  valu (ij,ilev) = valu (ij,1)
		  END DO
	    	END IF
	    END DO
	END IF

	RETURN
	END
