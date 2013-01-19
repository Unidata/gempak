	SUBROUTINE VI_DRIVF ( cmn_data, iret )
C************************************************************************
C* VI_DRIV								*
C*									*
C* This subroutine performs the vertical interpolation.			*
C*									*
C* VI_DRIV ( CMN_DATA, IRET )						*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C* 
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* K. Brill/EMC	     12/96	Modify error processing -- warn & go on *
C* G. Hull/SAIC      03/08      Added cmn_data                          *
C************************************************************************
C	IMPLICIT NONE
	INCLUDE		'vicmn.cmn'
C
C*	cmn_data is dynamically allocated by the calling C function.
C       This memory is for all of the 
C
	REAL            cmn_data (*)
	INTEGER         ip
	CHARACTER*40	cwarn
C-----------------------------------------------------------------------
	iret = 0
C 
C*      Initialize the common data arrays.
C*      These are allocated in C but accessed via cmn_data by using the
C*      following array indices. These indexes are computed in C and then
C*      stored in a common block for the FORTRAN.
C*
	CALL VI_GINDX ( ovcsfc_indx, sfcval_indx,
     +                  ovcbuf1_indx, ovcbuf2_indx,
     +                  rlnpi_indx,  rlnpo_indx,  valu_indx,
     +                  plast_indx,  tlast_indx,  zlast_indx,
     +                  sphbuf_indx, tmpbuf_indx, hgtbuf_indx,
     +                  grid_indx, pln_indx, aprs_indx, 
     +                  rdwrbuf_indx, vgread_buf_indx )

C
C*      We need to subtract 1 from the looping indices since
C*      the 'xxx'_indx variables account for the +1
C
	DO ij = 0, kxky-1
	    cmn_data( ovcsfc_indx + ij ) = RMISSD
	    cmn_data( ovcbuf1_indx + ij ) = RMISSD
	    cmn_data( ovcbuf2_indx + ij ) = RMISSD
	    cmn_data( zlast_indx + ij ) = RMISSD
	    cmn_data( plast_indx + ij ) = RMISSD
	    cmn_data( tlast_indx + ij ) = RMISSD
	END DO

	DO ij = 0, (kxky * np) -1
	    cmn_data( sfcval_indx + ij ) = RMISSD
	END DO

	DO il = 0, (kxky * nli) -1
	    cmn_data( rlnpi_indx + il) = RMISSD
	    cmn_data( valu_indx + il)  = RMISSD
	END DO

	DO il = 0, (kxky * nlo) -1
	    cmn_data( rlnpo_indx + il) = RMISSD
	END DO

C
C*	Load surface values of pressure, height and all other parameters
C*	that are available there.
C
	CALL VI_GTSF ( cmn_data (1), 
     +                 cmn_data ( sfcval_indx ), 
     +                 cmn_data ( rlnpi_indx ),
     +                 cmn_data ( valu_indx ),
     +                 cmn_data ( plast_indx ),
     +                 cmn_data ( tlast_indx ),
     +                 cmn_data ( zlast_indx ) , iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Do the vertical interpolation, looping over input parameters.
C
	DO ip = 2, np
C
C*	    Load the parameter values on all input levels.
C
	    IF ( ip .eq. 2 .or. lprmwr (ip) ) THEN
C*
	    	CALL VI_RDIN ( ip, cmn_data (1), 
     +                             cmn_data( sfcval_indx ), 
     +                             cmn_data( rlnpi_indx ), 
     +                             cmn_data( valu_indx ), iret )
C
C*	    	Now interpolate to the output levels.
C
	    	IF ( iret .eq. 0 ) THEN
		    CALL VI_VINT ( ip, cmn_data (1), 
     +                             cmn_data( ovcbuf1_indx ), 
     +                             cmn_data( ovcbuf2_indx ), 
     +                             cmn_data( rlnpi_indx ), 
     +                             cmn_data( rlnpo_indx ), 
     +                             cmn_data( valu_indx ), iret )
		END IF

		IF ( iret .ne. 0 ) THEN
		    CALL ST_LSTR ( parms (ip), lng, ier )
		    cwarn = 'WARNING ' // parms(ip)(1:lng) //
     +			    ' NOT INTERPOLATED'
		    CALL ER_WMSG ( 'VI', iret, cwarn, irr )
		END IF
	    END IF
C*
	END DO
C
C*	Write the surface grids.
C
	CALL VG_WSFC ( cmn_data, cmn_data( sfcval_indx ),
     +                           cmn_data( rdwrbuf_indx ), iret )
C*
	RETURN
	END
