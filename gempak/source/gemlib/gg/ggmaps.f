	SUBROUTINE GG_MAPS  ( proj, garea, imgfil, idrpfl, iret )
C************************************************************************
C* GG_MAPS								*
C*									*
C* This subroutine defines the map or graph projection and graphics 	*
C* area in GEMPLT.  If a GEMPLT  error is encountered, an error 	*
C* message is written.  If PROJ = DEF, the current map projection 	*
C* will be retained.  No validity check will be made.			*
C*									*
C* The following simple map projections may be specified:		*
C*     MER    Mercator							*
C*     NPS    North Polar Stereographic					*
C*     SPS    South Polar Stereographic					*
C*     LCC    Northern Hemisphere Lambert Conic Conformal		*
C*     SCC    Southern Hemisphere Lambert Conic Conformal		*
C*     CED    Cylindrical Equidistant					*
C*     MCD    Modified Cylindrical Equidistant				*
C*     UTM    Universal Transverse Mercator				*
C*     NOR    North Orthographic 					*
C*     SOR    South Orthographic					*
C*									*
C* The following full map projections may also be specified:		*
C*     MER    Mercator							*
C*     CED    Cylindrical Equidistant					*
C*     MCD    Modified Cylindrical Equidistant				*
C*     STR    Polar Stereographic					*
C*     AED    Azimuthal equidistant					*
C*     ORT    Orthographic						*
C*     LEA    Lambert equal area					*
C*     GNO    Gnomonic							*
C*     LCC    Northern Hemisphere Lambert Conic Conformal		*
C*     SCC    Southern Hemisphere Lambert Conic Conformal		*
C*     UTM    Universal Transverse Mercator				*
C*     TVM    Transverse Mercator					*
C*									*
C* There are three satellite projections available:			*
C*    NPG     Naval Postgraduate School navigation			*
C*    MCI     MCIDAS image navigation					*
C*    SAT     MCIDAS image navigation w/ image				*
C*									*
C* The graph projections are:						*
C*    POL     polar coordinates						*
C*    LIN     linear x and y						*
C*    LOG     linear x, logarithmic y					*
C*    KAP     linear x, y ** KAPPA  ( KAPPA = 2/7 )			*
C*									*
C* GG_MAPS  ( PROJ, GAREA, IMGFIL, IDRPFL, IRET )			*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection			*
C*	GAREA		CHAR*		Graphics area			*
C*	IMGFIL		CHAR*		Image (sat/rad) file name	*
C*									*
C* Output parameters:							*
C*	IDRPFL		INTEGER		Flag to drop the image		*
C*					  0 = No input from user	*
C*					  1 = Drop image		*
C*					  2 = Do not drop image		*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return		*
C*				  	 -2 = invalid graphics area	*
C*				  	 -5 = invalid projection	*
C*				  	-15 = invalid image file	*
C**									*
C* Log:									*
C* M. Goodman/RDS	 7/84	Original source				*
C* M. desJardins/GSFC	 2/85	Modified to check current values.	*
C* M. desJardins/GSFC	 2/86	Changed satellite navigation		*
C* M. desJardins/GSFC	 4/86	Added NPGS satellite navigation		*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* I. Graffman/RDS	11/86	Added graph mode			*
C* M. desJardins/GSFC	 5/88	Eliminated old values of proj, garea	*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* J. Cowie/NPS         10/93   Added check for MCI projection          *
C* G. Krueger/EAI	 2/94	Added SAT projection code		*
C* S. Jacobs/NMC	 2/94	Copied from GG_SMAP			*
C* S. Jacobs/NMC	 7/94	Removed AOI projection code		*
C* M. desJardins/NMC	 8/94	Cleaned up error messages written	*
C* J. Cowie/COMET	 1/95	Generalized SATFIL to IMGFIL to support	*
C*				image subsetting			*
C* J. Cowie/COMET	 5/95	Changed call to GG_SSAT to IM_SIMG	*
C* S. Jacobs/NMC	 8/95	Changed DRPFLG to IDRPFL		*
C* S. Jacobs/NMC	 8/95	Added check for user input for drop flag*
C* S. Jacobs/NMC	 8/95	Added IM_ISUB for subset area		*
C* S. Jacobs/NCEP	10/95	Moved check for drop flag to inside	*
C*				SAT/RAD projection check		*
C* M. Linda/GSC		12/95	Removed commented-out code		*
C* G. Krueger/EAI	 6/96	Check for default proj. using GAREA	*
C* S. Jacobs/NCEP	 1/97	Added IMGDEF and icbank for later check	*
C* S. Jacobs/NCEP	 2/97	Changed icbank to imbank		*
C* S. Jacobs/NCEP	 7/97	Added margin setting after getting proj	*
C*				from the geog table			*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	proj, garea, imgfil
C
	REAL 		gltln (4), angle (3), zmarg (4)
	REAL		centrd (2)
	CHARACTER	qprj*4, cdproj*30, cproj*4, tproj*72
	CHARACTER	carr(2)*80, cdrflg*4
	LOGICAL		angflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the projection and image drop flag first.
C
	idrpfl = 2
	CALL ST_CLST ( proj, '|', ' ', 2, carr, num, ier )
	tproj = carr(1)
C
C*	Parse the projection string for its parts.
C
	CALL GG_PROJ  ( tproj, cproj, angle, zmarg, angflg, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check for graph mode.
C
	IF  ( ( cproj .eq. 'LIN' ) .or. ( cproj .eq. 'POL' ) .or.
     +	      ( cproj .eq. 'LOG' ) .or. ( cproj .eq. 'KAP' ) )  THEN
C
C*	    Set correct mode.  Check margins to see if they have 
C*	    changed.
C
	    CALL GSMODE  ( 2, ier )
	    CALL GQGMGN  ( xllm, yllm, xurm, yurm, ier )
C
C*	    Set new margins.
C
	    IF  ( ( zmarg (1) .ne. xllm ) .or. 
     +		  ( zmarg (2) .ne. yllm ) .or.
     +		  ( zmarg (3) .ne. xurm ) .or. 
     +		  ( zmarg (4) .ne. yurm ) )  THEN
		CALL GSGMGN  ( zmarg (1), zmarg (2), zmarg (3),
     +			       zmarg (4), ier )
	    END IF
C
C*	    Set map projection.
C
	    CALL GG_SGRF  ( cproj, garea, iret )
C
C*	    Otherwise, this is a map projection.
C
	  ELSE
C
C*	    Set the mode.
C
	    CALL GSMODE  ( 1, ier )
C
C*	    Reset the color bank in case the user is NOT dropping an
C*	    image this time.
C
	    imbank = 0
C
C*	    Set required projection.  Check for satellite projection 
C*	    first.
C
	    IF  ( cproj .eq. 'SAT ' .or. cproj .eq. 'RAD ' )  THEN
C
C*	    	Query margins.
C
		CALL GQMMGN  ( xllm, yllm, xurm, yurm, ier )
C
C*	    	Set new margins.
C
		IF  ( ( zmarg (1) .ne. xllm ) .or. 
     +		      ( zmarg (2) .ne. yllm ) .or.
     +		      ( zmarg (3) .ne. xurm ) .or. 
     +		      ( zmarg (4) .ne. yurm ) )  THEN
		    CALL GSMMGN  ( zmarg (1), zmarg (2), zmarg (3),
     +			           zmarg (4), ier )
		END IF
C
C*		Set the drop flag value.
C
		idrpfl = 0
		IF  ( carr(2) .ne. ' ' )  THEN
		    CALL ST_LCUC ( carr(2), cdrflg, ier )
		    IF  ( cdrflg .eq. 'D' )  THEN
			idrpfl = 1
		      ELSE IF  ( cdrflg .eq. 'ND' )  THEN
			idrpfl = 2
		    END IF
		END IF
C
C*		Set the image navigation.
C
		CALL IM_SIMG ( cproj, imgfil, iret )
C
C*		Check for an error from setting the image nav.
C
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'IM', iret, imgfil, ier )
		    iret = -15
		    RETURN
		END IF
C
C*		Set the sub area for the image.
C
		CALL IM_ISUB ( garea, iret )
		IF ( iret .ne. 0 ) THEN
		    CALL ER_WMSG ( 'IM', iret, imgfil, ier )
		    iret = -2
		    RETURN
		END IF
C
	      ELSE 
C
C*		This is a map projection.  
C
		CALL LC_GARE  ( garea, gltln, cdproj, centrd, ier )
		IF  ( ier .ne. 0 )  THEN
		    iret = -2
		    CALL ER_WMSG  ( 'GG', iret, garea, ier )
		    RETURN
		END IF
C
C*		If no projection or DEF specified, use GAREA default
C*		projection.
C
		IF ( cproj .eq. 'DEF' .or. cproj .eq. ' ' ) THEN
		    CALL GG_PROJ  ( cdproj, cproj, angle, zmarg,
     +				    angflg, iret )
		END IF
C
C*		Query margins.
C
		CALL GQMMGN  ( xllm, yllm, xurm, yurm, ier )
C
C*	    	Set new margins.
C
		IF  ( ( zmarg (1) .ne. xllm ) .or. 
     +		      ( zmarg (2) .ne. yllm ) .or.
     +		      ( zmarg (3) .ne. xurm ) .or. 
     +		      ( zmarg (4) .ne. yurm ) )  THEN
		    CALL GSMMGN  ( zmarg (1), zmarg (2), zmarg (3),
     +			           zmarg (4), ier )
		END IF
C
C*		If angles not input, this is a simple map projection.
C
		IF  ( .not. angflg )  THEN
C
C*		    Compare the current projection with GEMPLT.  
C*		    Only call projection setup if something has changed.
C
		    CALL GQMMAP  ( qprj, qlats, qlonw, qlatn, qlone, 
     +				   ioldp )
		    IF ( ( ioldp .ne. 0 ) .or. ( cproj .ne. qprj ) .or. 
     +			 ( qlats .ne. gltln (1) ) .or. 
     +			 ( qlonw .ne. gltln (2) ) .or. 
     +			 ( qlatn .ne. gltln (3) ) .or. 
     +			 ( qlone .ne. gltln (4) ) )  THEN
			CALL GSMMAP  ( cproj, gltln (1), gltln (2), 
     +				       gltln (3), gltln (4), ier )
		    END IF
		  ELSE
C
C*		    Angles specified.
C*		    Compare the current projection with GEMPLT.  Only 
C*		    call projection setup if something has changed.
C
		    CALL GQMPRJ  ( qprj, qang1, qang2, qang3, qlats, 
     +				   qlonw, qlatn, qlone, ioldp )
		    IF ( ( ioldp .ne. 0 ) .or. ( cproj .ne. qprj ) .or. 
     +			 ( qang1 .ne. angle (1) ) .or.
     +			 ( qang2 .ne. angle (2) ) .or.
     +			 ( qang3 .ne. angle (3) ) .or.
     +			 ( qlats .ne. gltln (1) ) .or. 
     +			 ( qlonw .ne. gltln (2) ) .or. 
     +			 ( qlatn .ne. gltln (3) ) .or. 
     +			 ( qlone .ne. gltln (4) ) )  THEN
			CALL GSMPRJ  ( cproj, angle (1), angle (2), 
     +				       angle (3), gltln (1), gltln (2), 
     +				       gltln (3), gltln (4), ier )
		    END IF
		END IF
	    END IF
	    IF  ( ier .ne. 0 )  THEN
	        iret = -5
		CALL ER_WMSG  ( 'GEMPLT', ier, ' ', ier2 )
	    END IF
  	END IF
C*
	RETURN
	END
