	SUBROUTINE GDOADD  ( gdfile, gdoutf, gpack, gdattim, glevel,
     +	                     gvcord, gfunc, ihzrmp, idrct, iret )
C************************************************************************
C* GDOADD								*
C*									*
C* This subroutine adds the requested grids in the input file to the	*
C* output file.								*
C*									*
C* GDOADD  ( GDFILE, GDOUTF, GPACK, GDATTIM, GLEVEL, GVCORD, GFUNC,	*
C*           IHZRMP, IDRCT, IRET )					*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*  		Input grid file			*
C*	GDOUTF		CHAR*  		Output grid file		*
C*	GPACK		CHAR*		Grid packing information	*
C*	GDATTIM		CHAR*           Grid date/time                  *
C*      GLEVEL          CHAR*           Grid levels                     *
C*      GVCORD          CHAR*           Grid vertical coordinate        *
C*      GFUNC           CHAR*           Grid parameter name             *
C* 	IHZRMP		INTEGER		Horizontal remapping		*
C*	IDRCT		INTEGER		Directional flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = error reading input grid  *
C*					 -8 = error writing input grid	*
C*					-12 = cannot parse user input   *
C*					-13 = cannot match user input   *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88	Adapted from GDDNUM			*
C* M. desJardins/GSFC	 3/89	Added grid packing			*
C* M. desJardins/GSFC	 4/89	Eliminated unused array of vert coords	*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* K. Brill/NMC          9/90   Declare IGHDR; check for too many grids *
C* K. Brill/NMC		02/92	Use LLGDHD				*
C* S. Jacobs/NMC	10/94	Changed error number for writing error	*
C* S. Jacobs/NMC	10/94	Changed add loop to count foreward	*
C* J. Cowie/COMET	 3/95	Changed grid reading error number, and	*
C*				don't return IF read error occurs	*
C* F. Horsfall/TPC      06/96   Added users ability to work without     *
C*                              specifying grid number (multiple levels *
C*                              and parameters)                         *
C* S. Maxwell/GSC	 9/96	Modified; added subroutines GDU_MTCH    *
C*				and GDU_GINP; no longer searches by     *
C*				grid number				*
C* S. Jacobs/NCEP	11/96	Added a warning if no grids were found	*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* K. Brill/HPC		11/02	Use LLMXTG for size of GRID array	*
C* M. Li/SAIC		04/04	Added ihzrmp, and idrct			*
C* R. Tian/SAIC		 3/05	Changed calling seq, removed GDU_GINP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, gdoutf, gpack, glevel, gvcord, gfunc,
     +                  gdattim
C*
	CHARACTER	parm*12, gdattm (2)*20, trange*36, vparm*12,
     +			prmlst (MMPARM)*12, timfnd (LLMXGT)*36
	CHARACTER	tmfst*20, tmlst*20
	CHARACTER	infile*(LLMXLN), oufile*(LLMXLN)
	REAL            rnav1 (LLNNAV), rnav2 (LLNNAV)
	REAL		grid (LLMXTG), rlevel (LLMXLV, 2)
	INTEGER		level (2), ighdr (LLGDHD)
        LOGICAL         match, exist1, exist2, same
C------------------------------------------------------------------------
	iret = 0
C
C*      Check input/output grid files.
C
	CALL FL_INQR ( gdfile, exist1, infile, ier )
	CALL FL_INQR ( gdoutf, exist2, oufile, ier )
        IF ( .not. exist1 .or. .not. exist2 ) THEN
	    iret = -9
	    RETURN
        END IF
C
C*      Parse user input for gdattim.
C
	CALL ST_LCUC ( gdattim, gdattim, ier )
	IF ( gdattim .eq. 'ALL' ) THEN
	    ntime = 1
	    timfnd (1) = gdattim
	  ELSE
            CALL DG_NFIL ( gdfile, gdoutf, ier )
            CALL DG_NDTM ( gdattim, iperr )
            CALL DG_QTMS ( LLMXGT, .true., timfnd, ntime, trange, ier )
            IF ( iperr .ne. 0 .or. ntime .lt. 1 ) THEN
	        iret = -12
	        RETURN
            END IF
	END IF
C
C*      Parse user input for levels and vertical coordinate.
C
        CALL GDU_GLEV ( glevel, LLMXLV, gvcord, nlev, rlevel, levtyp,
     +                  vparm, icord, iperr)
	IF ( iperr .ne. 0 ) THEN
	    iret = -12
	    RETURN
	END IF
C
C*      Parse user input for a list of parameters.
C
        CALL ST_LCUC ( gfunc, gfunc, ier )
        CALL ST_CLST ( gfunc, ';', ' ', MMPARM , prmlst, nparm, iperr )
	IF ( iperr .ne. 0 .or. nparm .lt. 1 ) THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Translate packing information.
C
	CALL GR_PACK ( gpack, ipktyp, nbits, ier )
C
C*	Open input/output grid file.
C
	CALL GD_OPEN ( infile, .false., 0, LLNNAV, iflinp, adum1,
     +	               rnav1, maxgrd, iperr1 )
	CALL GD_OPEN ( oufile, .true.,  0, LLNNAV, iflout, adum1,
     +	               rnav2, maxgrd, iperr2 )
	IF ( iperr1 .ne. 0 .or. iperr2 .ne. 0 ) THEN
	    iret = -9
	    RETURN
	END IF
	CALL GR_CNAV ( rnav1, rnav2, LLNNAV, same, ier )
	IF ( .not. same ) THEN
	    iret = -10
	    RETURN
	END IF
	CALL GD_NGRD ( iflinp, ngrid, tmfst, tmlst, ier )

C
C*	Loop through all grids to find matches.
C
	knt = 0
        DO  nbeg = 1, ngrid
	    CALL GD_GIDN  ( iflinp, nbeg, gdattm, level, ivcord,
     +			    parm, ier )
	    CALL GDU_MTCH ( LLMXLV, gdattm, level, ivcord, parm, levtyp, 
     +			    nlev, rlevel, icord, nparm, prmlst, ntime,
     +			    timfnd, match, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG ( 'GDU', iperr, ' ', ier )
		iret = -13
		RETURN
	    END IF 
C
	    IF  ( match )  THEN
		knt = knt + 1
C
C*          	Write information to the screen. 
C
		WRITE ( 6, 1000 )
1000        	FORMAT ( / '      Adding grid: ' )
		CALL GR_WTRM  ( 6, .false., nbeg, gdattm, level,
     +				ivcord, parm, ier )
C
C*	    	Read grid from input file. 
C
		CALL GD_GGRD  ( iflinp, nbeg, gdattm, level, ivcord,
     +				parm, grid, igx, igy, ighdr, iperr )
		IF  ( iperr .ne. 0 )  THEN
		    iret = -7
		    CALL ER_WMSG  ( 'GDMOD', iret, ' ', ier )
		  ELSE
C
C*	    	    Write grid to output file.
C
		    IF ( ihzrmp .ne. IMISSD ) ighdr( 1 ) = ihzrmp
                    IF ( idrct .ne. IMISSD  ) ighdr( 2 ) = idrct
		    CALL GD_WPGD  ( iflout, grid, igx, igy, ighdr,
     +				    gdattm, level, ivcord, parm,
     +				    .true., ipktyp, nbits, iperr )
		    IF  ( iperr .ne. 0 )  THEN
			iret = -8
			CALL ER_WMSG  ( 'GDMOD', iret, ' ', ier )
			RETURN
		    END IF
		END IF
	    END IF
        END DO
C
C*	If no grids were found, write a warning message.
C
	IF  ( knt .eq. 0 )  THEN
	    CALL ER_WMSG ( 'GDU', 2, ' ', ier )
	END IF
C*
	RETURN
	END
