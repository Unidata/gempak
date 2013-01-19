	SUBROUTINE VI_GTSF ( cmn_data, sfcval, rlnpi, valu,
     +                       plast, tlast, zlast, iret )
C************************************************************************
C* VI_GTSF								*
C*									*
C* This subroutine gets the data at the surface for vertical interpo-	*
C* lation.								*
C*									*
C* VI_GTSF ( CMN_DATA, SFCVAL, RLNPI, VALU, PLAST, TLAST, ZLAST, IRET )	*
C*									*
C* Input/Ouput parameters:                                              *
C*      CMN_DATA (*)    REAL            Common Data                     *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      RLNPI(kxky,nli) REAL            Ln (p) on input lvls            *
C*      VALU (kxky,nli) REAL            values on input lvls            *
C*      PLAST (kxky)    REAL            last ln(p)                      *
C*      TLAST (kxky)    REAL            last virtual tmp                *
C*      ZLAST (kxky)    REAL            last computed z                 *
C*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -6 = required sfc p absent	*
C*					 -7 = required sfc z absent	*
C**									*
C* Log:									*
C* K. Brill/NMC		08/92						*
C* K. Brill/NMC		10/92	Check for NZAGL if no terrain		*
C* R. Miller/COMET	06/94   Changed 100000 to 10000 for eta & sgma. *
C*				Also added check for surface values at  *
C*				SGMA level 10000 (1.0).			*
C* K. Brill/EMC		11/95	Generalize search for T to include HGHT *
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* K. Brill/HPC		11/02	Eliminate use of the subset flag	*
C* R. Tian/SAIC		 4/05	Added GD_OPEN to get input file number	*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL       cmn_data (*) 
	REAL       sfcval (kxky,np), rlnpi (kxky,nli), valu (kxky,nli)
	REAL       plast (kxky), tlast (kxky), zlast (kxky)

	CHARACTER*4	vctry (4)
	INTEGER		isz (3)
	INCLUDE		'ERMISS.FNC'
	DATA		isz / 2, 10, 0 /
C-----------------------------------------------------------------------
	iret = 0
	havlnp (1) = .false.
	havtmp (1) = .false.
C*
	vctry (1) = NHGHT
	vctry (2) = NNONE
	vctry (3) = vcordi
	vctry (4) = NESFC
C
C*	Get the surface pressure.
C*      (Note: sfcval is not read by VC_READ when parm is SFCVAL
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
	CALL VG_READ ( igdfli, vcordi, 0, parms (1),
     +		       sfcval, cmn_data( vgread_buf_indx ),
     +                 sfcval (1,1), jret )
	IF ( jret .ne. 0 ) THEN
	    CALL VG_READ ( igdfli, NNONE, 0, parms (1),
     +			   sfcval, cmn_data( vgread_buf_indx ),
     +                     sfcval (1,1), jret )
	    IF ( jret .ne. 0 ) THEN
		CALL VG_READ ( igdfli, NESFC, 0, parms (1),
     +			       sfcval, cmn_data( vgread_buf_indx ),
     +			       sfcval (1,1), jret )
C
C* 		rjm 06/94  check for SGMA level 10000
C*		(sigma=1.0) for surface values.
C
	        IF ( jret .ne. 0 ) THEN
		    CALL VG_READ ( igdfli, NSGMA, 10000, parms (1),
     +			           sfcval, cmn_data( vgread_buf_indx ),
     +			           sfcval (1,1), jret )
		END IF
	    END IF
	END IF
	IF ( jret .ne. 0 ) THEN
	    lpmrds (1) = .false.
	    IF ( vcordi .eq. NSGMA .or. vcordi .eq. NETA .or.
     +		 vcordo .eq. NSGMA .or. vcordo .eq. NETA ) THEN
		iret = -6
		RETURN
	    END IF
	ELSE
	    lpmrds (1) = .true.
	END IF
C
C*	Get the surface elevation.
C*
	CALL VG_READ ( igdfli, vcordi, 0, NHGHT, 
     +	               sfcval, cmn_data( vgread_buf_indx ),
     +                 sfcval (1,3), jret )

	IF ( jret .ne. 0 ) THEN
	    CALL VG_READ ( igdfli, NNONE, 0, NHGHT, 
     +	                   sfcval, cmn_data( vgread_buf_indx ),
     +                     sfcval (1,3), jret )
	    IF ( jret .ne. 0 ) THEN
		CALL VG_READ ( igdfli, NESFC, 0, NHGHT, 
     +	                        sfcval, cmn_data( vgread_buf_indx ),
     +				sfcval (1,3), jret )
C
C* 		rjm 06/94  check for SGMA level 10000 (sigma=1.0)
C*		for surface values.
C
	        IF ( jret .ne. 0 ) THEN
		    CALL VG_READ ( igdfli, NSGMA, 10000, NHGHT, 
     +	                           sfcval, cmn_data( vgread_buf_indx ),
     +			           sfcval (1,3), jret )
	        END IF
	    END IF
	END IF
	IF ( jret .eq. 0 ) THEN
	    buildz = .true.
	    lpmrds (3) = .true.
	ELSE
	    iirr = +4
	    CALL ER_WMSG ( 'GDVINT', iirr, ' ', ier )
	    buildz = .false.
	    lpmrds (3) = .false.
	    DO ij = 1, kxky
		sfcval (ij,3) = RMISSD
	    END DO
	    IF ( vcordi .eq. NETA .or. vcordi .eq. NZAGL .or.
     +		 vcordo .eq. NETA .or. vcordo .eq. NZAGL ) THEN
		iret = -7
		RETURN
	    END IF
	END IF
C
C*	Read in temperature next.
C
	lpmrds (2) = .false.
	il = 0
	DO WHILE ( .not. lpmrds (2) .and. il .lt. 4 )
	  il = il + 1
	  ilv = 0
	  DO WHILE ( .not. lpmrds (2) .and.
     +      ( ( il .eq. 1 .and. ilv .lt. 3 ) .or.
     + 	      ( il .ne. 1 .and. ilv .eq. 0 ) ) )
	    ilv = ilv + 1
	    IF ( il .eq. 1 ) THEN
		lv = isz (ilv)
	    ELSE
		lv = isz (3)
	    END IF
	    CALL VG_READ ( igdfli, vctry (il), lv, NTMPK, 
     +	                   sfcval, cmn_data( vgread_buf_indx ),
     +		       	   sfcval (1,2), jret )
C*
	    IF ( jret .ne. 0 ) THEN
C
C*	    	Convert other thermodynamic quantity to Kelvin temp.
C
	        CALL VG_READ ( igdfli, vctry (il), lv, NTMPC, 
     +	                       sfcval, cmn_data( vgread_buf_indx ),
     +		               sfcval (1,2), jret )
	   	IF ( jret .eq. 0 ) CALL PD_TMCK ( sfcval (1,2),
     +				        kxky, sfcval (1,2), jret )
	   	IF ( jret .ne. 0 .and. lpmrds (1) ) THEN
		    CALL VG_READ ( igdfli, vctry (il), lv, NTHTA, 
     +	                       sfcval, cmn_data( vgread_buf_indx ),
     +			       sfcval (1,2), jret )
	   	    IF ( jret .eq. 0 ) 
     +		    	    CALL PD_TMPK ( sfcval, sfcval (1,2),
     +			               kxky, sfcval (1,2), jret )
	        END IF
	    END IF

	    IF ( jret .eq. 0 ) lpmrds (2) = .true.

	  END DO
	END DO
C
C*	Load the arrays to hold last pressure and temperature.
C
	IF ( lpmrds (1) ) THEN
	    havlnp (1) = .true.
	    DO ij = 1, kxky
	    	IF ( .not. ERMISS ( sfcval (ij,1) ) ) THEN
     	            plast (ij) = ALOG ( sfcval (ij,1) )
		    rlnpi (ij,1) = plast (ij)
	    	ELSE
	            plast (ij) = sfcval (ij,1)
		    rlnpi (ij,1) = sfcval (ij,1)
	    	END IF
	    END DO
	ELSE
	    buildz = .false.
	END IF
C*
	IF ( lpmrds (2) ) THEN
	    havtmp (1) = .true.
	    DO ij = 1, kxky
    	        tlast (ij) = sfcval (ij,2)
		valu (ij,1)  = sfcval (ij,2)
	    END DO
        ELSE
	    buildz = .false.
        END IF
C
C*	Get the surface values of remaining parameters.
C
	DO i = 4, np
	    IF ( i .eq. mstprm ) THEN
		CALL VI_GETQ ( 1, 0, cmn_data, rlnpi,
     +                         cmn_data( sphbuf_indx ),
     +                         cmn_data( tmpbuf_indx ),
     +                         sfcval (1,i), jret )
	    ELSE
	        CALL VG_READ ( igdfli, vcordi, 0, parms (i), 
     +	                       sfcval, cmn_data( vgread_buf_indx ),
     +		 	       sfcval (1,i), jret )
	        IF ( jret .ne. 0 ) THEN
	      	    CALL VG_READ ( igdfli, NNONE, 0, parms (i), 
     +	                           sfcval, cmn_data( vgread_buf_indx ),
     +				   sfcval (1,i), jret )
	      	    IF ( jret .ne. 0 ) THEN
			CALL VG_READ ( igdfli, NESFC, 0, parms (i), 
     +	                               sfcval, cmn_data(vgread_buf_indx),
     +				       sfcval (1,i), jret )
	      	    END IF
	    	END IF
	    END IF
	    IF ( jret .eq. 0 ) THEN
		lpmrds (i) = .true.
	    ELSE
	 	lpmrds (i) = .false.
	    END IF
	END DO
C
C*	Compute virtual temperature and convert surface q to ln(q).
C
	IF ( lpmrds (mstprm) ) THEN
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( sfcval (ij,mstprm) ) ) THEN
		    IF ( .not. ERMISS ( tlast (ij) ) )
     +			tlast (ij) = tlast (ij) * ( 1. + .608 *
     +				     sfcval (ij,mstprm) )
		    IF ( sfcval (ij,mstprm) .gt. 0.0 )
     + 			 sfcval (ij,mstprm) = 
     +                      ALOG ( sfcval (ij,mstprm) )
		END IF
	    END DO
	END IF
C
C*	Store surface height in array used for integrating z.
C
	DO ij = 1, kxky
	    zlast (ij) =  sfcval (ij,3)
	END DO
C*
	RETURN
	END
