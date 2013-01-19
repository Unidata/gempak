	SUBROUTINE VI_GETZ  ( lev, ovrwrt, update, cmn_data,
     +                        sfcval, rlnpi, valu,
     +                        plast, tlast, zlast,
     +                        hgtbuf, zgrd, iret )
C************************************************************************
C* VI_GETZ								*
C*									*
C* This subroutine obtains the height on input level LEV.		*
C*									*
C* VI_GETZ ( LEV, OVRWRT, UPDATE, CMN_DATA, SFCVAL, RLNPI, VALU,        *
C*           PLAST, TLAST, ZLAST, HGTBUF, ZGRD, IRET )		        *
C*									*
C* Input parameters:							*
C*	LEV		INTEGER		Level number			*
C*	OVRWRT		LOGICAL		Flag to overwrite valu(*,lev)	*
C*	UPDATE		LOGICAL		Flag to update grids		*
C*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*      PLAST (kxky)    REAL            last ln(p)                      *
C*      TLAST (kxky)    REAL            last virtual tmp                *
C*      ZLAST (kxky)    REAL            last computed z                 *
C*	HGTBUF(kxky)    REAL            previously local array   	*
C*									*
C* Output parameters:							*
C*	ZGRD(*)		REAL		Z values			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      08/92						*
C* R. Miller/COMET   06/94  Have LN (SFCVAL(*,mstprm)), so take EXP	*
C* R. Tian/SAIC	     04/05  Added GD_OPEN to get input file number	*
C* G. Hull/SAIC      03/08  add cmn_data to remove Grid size limit      *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		cmn_data (*)
	REAL		sfcval (kxky,np)
	REAL		rlnpi (kxky,nli)
	REAL		valu (kxky,nli)
	REAL		plast (kxky), tlast (kxky), zlast (kxky)
	REAL		hgtbuf (kxky)
	LOGICAL		ovrwrt, update
	REAL		zgrd (*)
C*
	LOGICAL		havqqq
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	havqqq = .false.
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
C*
	CALL VG_READ ( igdfli, vcordi, levin (lev), NHGHT, sfcval,
     +		       cmn_data( vgread_buf_indx ), zgrd, iret )
	IF ( iret .ne. 0 ) THEN
C
C*	    Height must be computed on this level.
C
C*	    Try reading the Montgomery Stream Function; then, compute
C*	    height using the temperature in VALU (1,lev).
C
	    CALL VG_READ ( igdfli, vcordi, levin (lev), NPSYM, sfcval,
     +		           cmn_data( vgread_buf_indx ), zgrd, iret )
	    IF ( iret .eq. 0 .and. havtmp (lev) ) THEN
		CALL VC_MSHT ( zgrd, valu (1,lev), kxky,
     +				zgrd, iret )
	    END IF
	    IF ( iret .ne. 0 .and. buildz .and.
     +		 havtmp (lev) .and. havlnp (lev) ) THEN
C
C*	  	Try to build height.
C
		CALL VI_GETQ ( lev, 0, cmn_data, rlnpi, 
     +                         cmn_data( sphbuf_indx ), 
     +                         cmn_data( tmpbuf_indx ), hgtbuf, iret )
		IF ( iret .eq. 0 ) havqqq = .true.
		IF ( havqqq .and. lev .eq. 2 .and. lpmrds (2)
     +		     .and. .not. lpmrds (mstprm) ) THEN
C
C*		    Compute virtual temperature at the surface
C*		    using the first level q.
C
		    DO ij = 1, kxky
			IF ( .not. ERMISS ( tlast (ij) ) .and.
     +			     .not. ERMISS ( hgtbuf (ij) ) ) THEN
			    tlast (ij) = ( 1. + .608 * hgtbuf (ij) ) *
     +				           tlast (ij)
			END IF
		    END DO
		END IF
C*
		CALL VC_BLDZ ( valu (1,lev), rlnpi (1,lev),
     +			       hgtbuf, kxky, .false.,
     +			       tlast, plast, zlast, zgrd, iret )
	    END IF
	END IF
	IF ( iret .eq. 0 .and. havtmp (lev) .and.
     +	     havlnp (lev) ) THEN
	    buildz = .true.
	ELSE
	    buildz = .false.
	END IF
C
C*	Update the last levels for building height.
C
	IF ( buildz .and. update ) THEN
	    IF ( .not. havqqq ) THEN
                CALL VI_GETQ ( lev, 0, cmn_data, rlnpi, 
     +                         cmn_data( sphbuf_indx ), 
     +                         cmn_data( tmpbuf_indx ), hgtbuf, iret )
	    END IF
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( valu (ij,lev) ) .and.
     +		     .not. ERMISS ( hgtbuf (ij) ) ) THEN
		    tlast (ij) = valu (ij,lev) *
     +				 ( 1. + .608 * hgtbuf (ij) )
		ELSE IF ( .not. ERMISS ( valu (ij,lev) ) ) THEN
		    tlast (ij) = valu (ij,lev)
		ELSE
		    tlast (ij) = RMISSD
		END IF
		plast (ij) = rlnpi (ij,lev)
		zlast (ij) = zgrd (ij)		
C
C*		If a point is underground, set everything to
C*		surface values.
C
		IF ( lpmrds (1)
     +		     .and. rlnpi (ij,lev) .eq. rlnpi (ij,1) ) THEN
		    IF ( .not. ERMISS ( sfcval (ij,2) ) .and.
     +			 .not. ERMISS ( sfcval (ij,mstprm) ) ) THEN
			tlast (ij) = sfcval (ij,2) *
     +			   ( 1. + .608 * EXP (sfcval (ij,mstprm )) )
		    ELSE IF ( .not. ERMISS ( sfcval (ij,2) ) ) THEN
			tlast (ij) = sfcval (ij,2)
		    ELSE
			tlast (ij) = RMISSD
		    END IF
		    plast (ij) = rlnpi (ij,1)
		    zlast (ij) = sfcval (ij,3)
		END IF
C*
	    END DO
	END IF
C
C*	Write result over temperature if requested.
C
	IF ( ovrwrt .and. iret .eq. 0 ) THEN
	    DO ij=1,kxky
	    	valu (ij,lev) = zgrd (ij)
	    END DO
	    havtmp (lev) = .false.
	END IF
C*
	RETURN
	END
