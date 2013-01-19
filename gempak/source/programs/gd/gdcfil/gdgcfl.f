        SUBROUTINE GDGCFL  ( gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +                       anlyss, iret )
C************************************************************************
C* GDGCFL                                                               *
C*                                                                      *
C* This subrutine creates a GEMPAK grid file if the user input for	*
C* 'gdoutf' does not exist.						*
C*                                                                      *
C* GDGCFL  ( GDFILE, PROJ, GDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS, IRET ) *
C*                                                                      *
C* Input parameters:                                                    *
C*      GDFILE		CHAR*		User input for output gird name	*
C*      PROJ		CHAR*		User input for grid projection	*
C*      GDAREA		CHAR*		User input for grid area	*
C*      KXKY		CHAR*		User input for KXKY		*
C*      MAXGRD		CHAR*		User input for Max number grids	*
C*	CPYFIL		CHAR*		User input for copy file	*
C*	ANLYSS		CHAR*		User input for analysis		*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* R. Tian/SAIC		 1/05	Modified from GDCFIL			*
C* R. Tian/SAIC		 3/05	Set IMXGRD				*
C* m.gamazaychikov/SAIC 01/06   Changed templt string length to MXTMPL  *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* S. Gilbert/NCEP	 7/07	Removed LLMXTG limit check		*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +                  anlyss
C*
	CHARACTER	cpath*(LLMXLN), filnam*(LLMXLN)
	CHARACTER	cprj*24, name*4, templt*(MXTMPL)
	CHARACTER*80	cpyf(2), suba
	LOGICAL		havanl, exist
	REAL		rnvblk (LLNNAV), anlblk (LLNANL)
	REAL		rltln  (4)
	INTEGER		kxsub(2), kysub(2)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize analysis block and header length.
C
	DO  i = 1, LLNANL
	    anlblk (i) = 0.
	END DO
	ihd = LLGDHD
C
C*	Split CPYFIL using '|' to get optional subarea.
C
	CALL ST_CLST ( cpyfil, '|', ' ', 2, cpyf, num, ier )
	filnam = cpyf(1)
	suba   = cpyf(2)
C
	IF  ( filnam .eq. ' ' )  THEN
C
C*	    CASE 1: Build new navigation block from user input.
C
	    CALL GDCNAV  ( proj, kxky, gdarea, cprj, kx, ky,
     +	                   rltln, rnvblk, iret )
	    IF  ( iret .eq. 0 )  THEN
	        CALL GDCANL ( anlyss, rnvblk, anlblk, iret )
	    END IF
C
	  ELSE IF  ( filnam ( 1:1 ) .eq. '#' )  THEN
C
C*	    CASE 2: Build new navigation and analysis blocks from grid
C*	            navigation table input.
C
	    CALL GDCTBL  ( filnam, name, cprj, kx, ky, rltln, 
     +		           rnvblk, anlblk, iret )
	    IF  ( iret .eq. 0 )  THEN
	        havanl = .false.
		IF ( anlblk (1) .eq. 1. .or. anlblk (1) .eq. 2. )
     +		    havanl = .true.
                IF ( suba .ne. ' ' )  THEN
                    CALL GR_SUBA( suba, .false., rnvblk, rltln,
     +                            kxsub, kysub, subset, ier )
                    kx = kxsub(2) - kxsub(1) + 1
                    ky = kysub(2) - kysub(1) + 1
                    havanl = .false.
                END IF
		IF ( havanl .and. anlyss .eq. ' ' ) THEN
C
C*		    Do nothing.
C
		  ELSE
		    CALL GDCANL ( anlyss, rnvblk, anlblk, iret )
		END IF
	    END IF 
C
	  ELSE
C
C*	    CASE 3: Get the navigation and analysis blocks from the
C*	    existing file.
C
	    CALL FL_INQR ( cpyf(1), exist, filnam, ier )
	    IF ( .not. exist ) THEN
C
C*	        'cpyfil' is a template
C
	        CALL ST_NULL ( cpyf(1), filnam, lens, ier )
		CALL CTB_DTGET ( filnam, cpath, templt, ic, is, if, ir,
     +		      ii, ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -8
		END IF
		CALL ST_RNUL ( cpath, cpath, lens, ier )
		CALL ST_RNUL ( templt, templt, lens, ier )
		CALL FL_INQR ( cpath, exist, filnam, ier )
		IF ( ier .eq. 0 .and. exist ) THEN
		    cpath = filnam
C
C*		    IF the template contains forecast hour, replace
C*		    with f000.
C
		    CALL ST_RPST ( templt, 'FFFFF', '00000',
     +		                   ipos, templt, ier )
		    CALL ST_RPST ( templt, 'FFF', '000',
     +		                   ipos, templt, ier )
		    CALL ST_RPST ( templt, 'FF', '00',
     +		                   ipos, templt, ier )
		    CALL FL_SCND ( cpath, templt, -1, 1, filnam,
     +		                   nf, ier )
		    CALL ST_LSTR ( cpath, lp, ier )
		    CALL ST_LSTR ( filnam, lf, ier )
		    filnam = cpath(:lp) // '/' // filnam(:lf)
		END IF
	    END IF
	    CALL GD_OPEN ( filnam, .false., LLNANL, LLNNAV, iflno,
     +                     anlblk, rnvblk, mxgd, ier )
	    IF ( ier .eq. 0 ) THEN
	        CALL GR_RNAV  ( rnvblk, cprj, kx, ky, ier )
	        rltln (1) = rnvblk (7)
	        rltln (2) = rnvblk (8)
	        rltln (3) = rnvblk (9)
	        rltln (4) = rnvblk (10)
	        IF ( ier .eq. 0 ) THEN
	            havanl = .false.
		    IF ( anlblk (1) .eq. 1. .or. anlblk (1) .eq. 2. )
     +		        havanl = .true.
                    IF ( suba .ne. ' ' )  THEN
                        CALL GR_SUBA( suba, .false., rnvblk, rltln,
     +                                kxsub, kysub, subset, ier )
                        kx = kxsub(2) - kxsub(1) + 1
                        ky = kysub(2) - kysub(1) + 1
                        havanl = .false.
                    END IF
		    IF ( havanl .and. anlyss .eq. ' ' ) THEN
C
C*		        Do nothing.
C
		      ELSE
		        CALL GDCANL ( anlyss, rnvblk, anlblk, iret )
		    END IF
	        END IF
	    END IF
	END IF
C
C*	Create the grid file.
C
	IF  ( iret .eq. 0 ) THEN
	    CALL ST_NUMB ( maxgrd, imxgrd, ier )
	    CALL GD_CREF  ( gdfile, LLNNAV, rnvblk, LLNANL,
     +		            anlblk, ihd, imxgrd, iflno, ier )
	    IF  ( ier .eq. 0 ) THEN
		CALL GD_CLOS ( iflno, ier )
	    END IF
	END IF
C*
	RETURN
	END
