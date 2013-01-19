    	SUBROUTINE GDXDVVF ( iflno, gdatim, gvcord, ystrt,
     +                       ystop, gvecx, time, ivcord, 
     +			     rgx, rgy, nhxs, gridu, gridv, rlvl, ugrd, vgrd,
     +			     ponth, nvxs, parmu, parmv, lvert, lscal, iret )
C************************************************************************
C* GDXDVVF								*
C*									*
C* This subroutine gets the wind data to display in a cross section.	*
C*									*
C* GDXDVVF ( IFLNO, GDATIM, GVCORD, YSTRT, YSTOP, GVECX, TIME, IVCORD,	*
C*           RGX, RGY, NHXS, GRIDU, GRIDV, RLVL, UGRD, VGRD, PONTH, 	*
C*	     NVXS, PARMU, PARMV, LVERT, LSCAL, IRET )			*
C*									*
C* Input parameters:							*
C*      IFLNO             INTEGER       Grid file number                *
C*	GDATIM		  CHAR*		User input date/time		*
C*	GVCORD		  CHAR*		User input vert coord		*
C*      YSTRT             REAL          Bottom vert coord value		*
C*      YSTOP             REAL          Top vert coord value		*
C*	GVECX	 	  CHAR*		User input function		*
C*	TIME  (2)	  CHAR*		Time to search for levels	*
C*	IVCORD		  INTEGER	Vertical coordinate for search	*
C*	RGX  (NHXS)	  REAL		X grid coordinates		*
C*	RGY  (NHXS)	  REAL		Y grid coordinates		*
C*	NHXS		  INTEGER	Number of xsect pts in horiz.	*
C*									*
C* Work parameters:							*
C*      GRIDU (*)	  REAL		work array to read in grid	*
C*      GRIDV (*)	  REAL		work array to read in grid	*
C*									*
C* Output parameters:							*
C*      RLVL (NVXS)	  REAL		Vertical levels in grid		*
C*	UGRD (NHXS, NVXS) REAL		Array of xsect u components	*
C*	VGRD (NHXS, NVXS) REAL		Array of xsect v components	*
C*	PONTH(NHXS, NVXS) REAL		Array of p on theta		*
C*      NVXS              INTEGER       Number of xsect pts in vert.	*
C*	PARMU		  CHAR*		Parameter name of u grid	*
C* 	PARMV		  CHAR*		Parameter name of v grid	*
C*	LVERT		  LOGICAL	Flag for vertical circ comp	*
C*	LSCAL		  LOGICAL	Flag for scaled vert circ comp	*
C*	IRET		  INTEGER	Return code			*
C*					  7 = GVECT not specified	*
C*					  0 = normal return		*
C*					 -6 = invalid vertical coord.	*
C*					-12 = no levels found		*
C*					-13 = @level not allowed	*
C*					-14 = %vcord not allowed	*
C*					-18 = GVECX not valid		*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89   Created from GDXDTA			*
C* K. Brill/GSC         11/89   DATTIM(2)=' ' for GD_GLEV		*
C* K. Brill/GSC         12/89   Subset levels to save time		*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C* K. Brill/NMC		08/90   Change for CIRC operator		*
C* K. Brill/NMC   	11/90   Expand number of levels by one		*
C* K. Brill/NMC		 1/91	Clean up				*
C* S. Jacobs/NMC	 3/95	Removed check for last level to plot	*
C*				  (This was used when putting vectors	*
C*				   on a regular grid.)			*
C* T. Lee/SAIC		 3/02	Initialized HAVSFC			*
C* K. Brill/HPC		 5/02	Allow cross section path to cross grid	*
C*				discontinuity.				*
C* R. Tian/SAIC         10/02   Remove COMMON/GDXS, GR_RARG, GSGPRJ     *
C* K. Brill/HPC		 2/04	Changes for new DG TF mngmnt		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, gvecx, time (2), parmu, parmv
        REAL		rgx ( nhxs ), rgy ( nhxs )
	REAL		ugrd (*), vgrd (*), ponth (*)
	LOGICAL		lscal, lvert
	LOGICAL         havsfc, lavflg
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80, gfunc*4
	CHARACTER	gvect*80, cbuf*8
	INTEGER		lev (2)
	REAL		gridu ( * ), gridv ( * )
C*
	INTEGER		level ( 2, LLMXLV )
	REAL            rlvl  ( * )
C------------------------------------------------------------------------
	iret = 0
	lscal = .false.
	lvert = .false.
	havsfc = .false.
	gfunc = 'PRES'
C
C*	Check to see if the function involves a layer average.
C
	CALL GDXCLA ( gvecx, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN
C 
C* 	Check to see if GVECX contains @level or %vcord specifiers.
C
	iat = index ( gvecx, '@' )
	IF ( iat .ne. 0 ) THEN
	  iret = -13
	  CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	  RETURN
	END IF
	iat = index ( gvecx, '%' )
	IF ( iat .ne. 0 ) THEN
	  iret = -14
	  CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	  RETURN
	END IF
C
C*	Look for trailing flags for scaling or cross section vector.
C
	CALL ST_LCUC ( gvecx, gvect, iret )
	islsh = index ( gvect, 'CIRC' )
	IF ( islsh .ne. 0 ) THEN
          lscal = .true.
	  lvert = .true.
	END IF
	islsh = index ( gvect, '/V' )
	IF ( islsh .ne. 0 ) THEN
          lvert = .true.
	  gvect = gvect ( : islsh-1 )
	END IF
C
C*      Set interpolation type.
C
        inttyp = 1
C
C*	Set the grid relative direction of the tangent vector.  (This
C*	is like a conventional wind direction with the y axis pointing
C*	north.)
C
	CALL GDXDXY ( gddx, gddy, iret )
	IF ( iret .ne. 0 ) RETURN
	dvx = ( rgx ( nhxs ) - rgx ( 1 ) ) * gddx
	dvy = ( rgy ( nhxs ) - rgy ( 1 ) ) * gddy
	ornang = ATAN2 ( -dvx, -dvy)
	CALL DG_OANG ( ornang, iret )
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
C
	dattim ( 1 ) = time ( 1 )
	dattim ( 2 ) = ' '
    	CALL DG_GLEV  ( iflno, dattim, ivcord, LLMXLV, level, nlev, ier )
	IF  ( nlev .eq. 0 )  THEN
	    iret = -12
	    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*      Float and sort the levels.
C
	IF  ( ivcord .eq. 0 )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*      Load the levels for sorting.
C
	DO i = 1, nlev
	   IF ( level ( 1, i ) .eq. 0 ) havsfc = .true.
	   rlvl ( i ) = FLOAT ( level ( 1, i ) )
	END DO
C
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
	IF ( iret .ne. 0 ) THEN
	   RETURN
	END IF
C
C*	Select the range of levels included by ystrt -- ystop.
C
	i = 1
	IF ( havsfc ) i = 2
	istrt = 0
	istop = 0
	DO WHILE ( i .lt. nlev .and.
     +           (istrt .eq. 0 .or. istop .eq. 0 ) )
	  i = i + 1
	  IF ( (ystrt .ge. rlvl ( i-1 ) .and. ystrt .lt. rlvl ( i ) )
     +                               .or.
     +         (ystrt .le. rlvl ( i-1 ) .and. ystrt .gt. rlvl ( i ) ) ) 
     +      istrt = i - 1
	  IF ( (ystop .gt. rlvl ( i-1 ) .and. ystop .le. rlvl ( i ) )
     +                               .or.
     +         (ystop .lt. rlvl ( i-1 ) .and. ystop .ge. rlvl ( i ) ) )
     +      istop = i
	END DO
	IF ( istrt .eq. 0 ) istrt = 1
	IF ( istop .eq. 0 ) istop = nlev
	IF ( lavflg ) THEN
	  IF ( istrt .lt. 2 ) istrt = 2
	  istop = istop + 1
	  IF ( istop .gt. nlev ) istop = nlev
	END IF
	IF ( havsfc .and. istrt .lt. 2 ) istrt = 2
	IF ( lavflg .and. havsfc .and. istrt .lt. 3 ) istrt = 3
C
C*	Loop through single levels finding data.  Also, reset rlvl
C*   	values so that rlvl(1) is the first level in the cross section.
C
	ilev = 0
	indx = 1
	DO  i = istrt, istop, 1
C
C*		Encode level and compute function.
C
	 	IF ( lavflg ) THEN
	 	  ilvl1 = int ( rlvl ( i ) )
	  	  ilvl2 = int ( rlvl ( i - 1 ) )
		  CALL ST_INLN ( ilvl1, glevel, lnth, ier )
	          CALL ST_INCH ( ilvl2, cbuf, ier )
	          glevel = glevel(1:lnth) // ':' // cbuf
		ELSE
		  intlvl = int ( rlvl ( i ) )
		  CALL ST_INCH  ( intlvl, glevel, ier )
	        END IF
	        IF ( .not. lvert ) THEN
		  CALL DG_VECTN ( gdatim, glevel, gvcord, gvect, 
     +				pfunc,  gridu, gridv, kx, ky, dattim,
     +			        lev, jvcord, parmu, parmv, ier )
		ELSE
		  CALL DG_VECRN ( gdatim, glevel, gvcord, gvect, 
     +				pfunc,  gridu, gridv, kx, ky, dattim,
     +			        lev, jvcord, parmu, parmv, ier )
	        END IF
		IF  ( ier .eq. 0 )  THEN
C
C*		    Interpolate to cross section points.
C
		    CALL GR_INTP  ( inttyp, 
     2               rgx, rgy, nhxs, kx, ky, gridu, ugrd ( indx ), ier )
		    CALL GR_INTP  ( inttyp, 
     2               rgx, rgy, nhxs, kx, ky, gridv, vgrd ( indx ), ier )
C
C*		    Get p on theta if necessary for scaling omega.
C
	            IF ( lscal .and. ivcord .eq. 2 ) THEN
		      CALL DG_GRIDN ( gdatim, glevel, gvcord, gfunc,
     +				pfunc, gridu, kx, ky, dattim, 
     +				lev, jvcord, parm, ier2 )
	              IF ( ier2 .eq. 0 ) THEN
		        CALL GR_INTP  ( inttyp, rgx, rgy, nhxs,
     2              		    kx, ky, gridu, ponth ( indx ), ier )
                      ELSE
			instp = indx + nhxs - 1
			DO ij = indx, instp
			  ponth ( ij ) = RMISSD
		        END DO
                      END IF
		    END IF
C
C*		    Increment the cross section grid array index.
C
	            indx = indx + nhxs
	            ilev = ilev + 1
		    IF ( lavflg ) THEN
	              IF ( ivcord .eq. 1 ) THEN
			rlvl ( ilev ) = SQRT ( rlvl ( i ) *
     +					          rlvl ( i - 1 ) )
		      ELSE
	                rlvl ( ilev ) = .5 * ( rlvl ( i ) + 
     +					       rlvl ( i - 1 ) )
		      END IF
		    ELSE
		      rlvl ( ilev ) = rlvl ( i )
		    END IF
		END IF
	END DO
C
C*      Set the second output array dimension (nhxs,nvxs).
C
        nvxs = ilev
	IF ( ilev .eq. 0 ) THEN
	  iret = ier
	  CALL ER_WMSG ( 'DG', iret, pfunc, ier )
	  iret = -18
	  CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	END IF
C*
	RETURN
	END
