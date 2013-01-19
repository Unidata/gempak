	PROGRAM NDFDG2
C************************************************************************
C* NDFDG2								*
C*									*
C* This program will convert GRIB2 grid data to GEMPAK grid data.	*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	10/02	Created from NAGRIB			*
C* K. Brill/HPC		11/02	Use LLMXTG to set MXGRIB parameter	*
C* T. Piper/SAIC	02/03	Fixed missing data for entire grid	*
C* T. Piper/SAIC	04/03	Added nd_gds and gskip parameter	*
C* T. Piper/SAIC	04/03	Replaced NAGCOG with ND_GCOG		*
C* T. Piper/SAIC	04/03	Increased MXGRIB for 2.5 km grids	*
C* T. Piper/SAIC	04/03	Added MXGRIB parameter to GB_READ	*
C* T. Piper/SAIC	05/03	Added CTB_G2READ			*
C* T. Piper/SAIC	05/03	Replaced GR_WNMC with GR_WGB2		*
C* T. Piper/SAIC	05/03	Increased NPDS to accommodate Template 9*
C* T. Piper/SAIC	08/03	Added CPC support			*
C* M. Li/SAIC		04/04	Added ihzrmp, and idrct			*
C* C. Bailey/HPC	06/04	Added missing parameter error statement	*
C* m.gamazaychikov/SAIC	09/05	Added overwr to CSs of NDGINP, GD_WPGD	*
C* T. Piper/SAIC	09/06	Added gg_sdev				*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER	gbfile*(LLMXLN), gdoutf*(LLMXLN), maxgrd*(LLMXLN),
     +			garea*(LLMXLN),  gskip*(LLMXLN),  output*(LLMXLN),
     +			indxfl*1, proj*1, grdarea*1, kxky*1, cpyfil*3
C
	CHARACTER	gdotmp*72
C
	INTEGER		lun (MMFILE), ighdr (LLGDHD),
     +			jtime (3), level (2), ksbx (2), ksby (2)
	REAL		rnvblk (LLNNAV), gdsarr (10)
	CHARACTER	devs (MMFILE)*1, parm*12, gdattm (2)*20,
     +			tmpprm*10, vparm*4, cprj*24, errmsg*128
	LOGICAL		respnd, done, proces, cont, lstflg, prttl,
     +			trape0, subset, overwr
C
C*  GRIB2 variables
C
	PARAMETER ( MXGRIB = 3000000 )
	PARAMETER ( ND5   = MXGRIB )
	PARAMETER ( NIDAT = 50000 )
	PARAMETER ( NRDAT = 50000 )
	PARAMETER ( NDJER = 15 )
	PARAMETER ( NS0 = 16, NS1 = 21, NS2 = 10, NS3 = 96 )
	PARAMETER ( NPDS = 72, NS5 = 49, NS6 = 8, NS7 = 10 )

	REAL     grid(MXGRIB), rdat(NRDAT), xmissp, xmisss
	INTEGER igrid(MXGRIB), idat(NIDAT), ib(MXGRIB)
	INTEGER ipack(ND5), jer(NDJER, 2), kjer
	INTEGER is0(NS0), is1(NS1), is2(NS2), gds(NS3),
     +	  pds(NPDS), is5(NS5), is6(NS6), is7(NS7)
	LOGICAL big, unpk_endian
C
	DATA ipack / ND5*0 /
C
C*	Added common to prevent Stack Over Flow	(SOF)
C
	COMMON/NOSOF/igrid, grid, ib
C------------------------------------------------------------------------
	indxfl = ' '
	lenidx = 0
	proj = ' '
	grdarea = ' '
	kxky = ' '
	cpyfil = 'gds'
C
	kfildo = 6
	new = 1
	iclean = 0
	l3264b = 32
	xmissp = RMISSD
C
C*  Initialize TAE
C
	CALL IP_INIT ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT ( 'NDFDG2', ier )
C
C*  Initialize GEMPLT
C
	    CALL GG_INIT ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
		CALL GG_SDEV("GN", ier )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Initialize GRIB2 parameter table
C
	CALL CTB_G2READ (iret)
	IF  ( iret .ne. 0 )  THEN
	    done = .true.
	END IF
C
C*	Process grids until the user is finished.
C
	DO WHILE ( .not. done )
	    DO ii = 1, LLGDHD
                ighdr ( ii ) = 0
            END DO
C
C*	    Get the user input.
C
	    CALL NDGINP ( gbfile, gdoutf, maxgrd, garea, gskip, 
     +					output, overwr, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		done   = .true.
		proces = .false.
	    ELSE
		proces = .true.
	    END IF
C
	    IF  ( proces )  THEN
C
C*		Process the user's input values
C
C*		GBFILE:  Open the GRIB file.
C
		CALL ST_LSTR ( gbfile, lenfil, ier )
		CALL GB_OPEN ( gbfile, lenfil, indxfl, lenidx, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'NDFDG2', iret, ' ', ier )
		    proces = .false.
		END IF
		IF  ( proces )  THEN
		    igdfln = 0
C
C*		GDOUTF:  Check if user only wants a list
C
		    CALL ST_LCUC ( gdoutf, gdotmp, iret )
		    IF  ( gdotmp .eq. 'LIST' )  THEN
			lstflg = .true.
		    ELSE
			lstflg = .false.
		    END IF
C
C*		MAXGRD:  Set the maximum number of grids from the user input.
C
		    CALL ST_NUMB ( maxgrd, maxg, ier )
		    IF ( maxg .gt. MMHDRS-1 )  THEN
			IF ( nlun .gt. 0 )  THEN
			    DO  ii = 1, nlun
				ilun = lun(ii)
				WRITE ( ilun, * )
     +				' WARNING : Resetting MAXGRD value'//
     +				' to maximum limit (', MMHDRS-1, ').'
			    END DO
			END IF
			maxg = MMHDRS-1
		    END IF
C
C*		GSKIP:  Convert to an integer
C
		    CALL ST_NUMB ( gskip, iskip, ier )
		    IF ( iskip .lt. 0 ) iskip = 0
C
C*		OUTPUT:  Set the output devices.
C
		    CALL IN_OUTT ( output, 'NDFDG2', lun, nlun, devs, ierr )
		    IF  ( ierr .ne. 0 )  nlun = 0
C
C*		Read in grids to be processed until the end of file.
C
		    knt = 0
		    kntmsg = 0
		    trape0 = .false.
		END IF
		DO WHILE  ( proces .and. ( knt .lt. maxg ) )
C
		    kntmsg = kntmsg + 1
C
C*		    Set the start byte of the next GRIB message.  If the
C*		    end of the file has been reached stop processing.
C
		    CALL GB_NEXT ( iedtn, iret )
C
C*		    Check the GRIB edition.  Currently, only set the
C*		    return code to +5.  Decide later whether or not to
C*		    decode GRIB 0.
C
		    IF  ( iedtn .eq. 0 )  THEN
			IF  ( .not. trape0 )  THEN
			    CALL ER_WMSG ( 'NDFDG2', iret, ' ', ier )
			    trape0 = .true.
			END IF
		    END IF
C
C*		    Determine if the program should continue or not.
C
		    IF  ( iret .lt. 0 )  THEN
			proces = .false.
		    ELSE IF  ( iret .gt. 0 )  THEN
			proces = .true.
			cont   = .false.
		    ELSE
			proces = .true.
			cont   = .true.
		    END IF
C
C*		    If there is a next grid, continue processing.
C
		    IF  ( proces )  THEN
C
C*			Unpack the GRIB2 file.
C
			IF  ( cont )  THEN
			    CALL GB_READ ( MXGRIB, ipack, iret )
			    IF  ( iret .lt. 0 )  THEN
				CALL ER_WMSG ( 'NDFDG2', iret, ' ', ier )
				proces = .false.
				cont   = .false.
			    ELSE IF  ( iret .gt. 0 )  THEN
				CALL ER_WMSG ( 'NDFDG2', iret, ' ', ier )
				cont   = .false.
			    END IF
			END IF
			IF ( cont )  THEN
			    big = UNPK_ENDIAN()
			    IF ( .not. big ) THEN
				CALL MV_SWP4(ND5, ipack, ipack)
			    END IF
			    CALL UNPK_GRIB2 ( kfildo, grid, igrid,
     +				    MXGRIB, idat, NIDAT, rdat,
     +				    NRDAT, is0, NS0, is1, NS1,
     +				    is2, NS2, gds, NS3, pds, NPDS,
     +				    is5, NS5, is6, NS6, is7, NS7,
     +				    ib, ibitmap, ipack, ND5, xmissp,
     +				    xmisss, new, iclean, l3264b,
     +				    iendpk, jer, NDJER, kjer )
C
			    IF ( jer(kjer,2) .eq. 1 )  THEN
				CALL ST_INCH ( jer(kjer,1), tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', 3, tmpprm, ier )
			    ELSE IF ( jer(kjer,2) .eq. 2 )  THEN
				CALL ST_INCH ( jer(kjer,1), tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', -9, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 0 - INDICATOR SECTION
C
			IF  ( cont )  THEN
			    IF ( is0(7) .ne. 0 ) THEN
				CALL ST_INCH ( is0(7), tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', -13, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 1 - IDENTIFICATION SECTION
C
			IF  ( cont )  THEN
			    IF ( is1(21) .gt. 1 ) THEN
				CALL ST_INCH ( is1(21), tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', -14, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 2 - LOCAL SECTION
C
			IF  ( cont )  THEN
			    IF ( is2(1) .ne. 0 ) THEN
				CALL ER_WMSG ( 'NDFDG2', -22, ' ', ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 3 - GRID DEFINITION SECTION
C
C*	Source of grid definition (see Code Table 3.0 and Note 1)
C
			IF ( cont ) THEN
			    IF ( gds(6) .ne. 0 ) THEN
				CALL ST_INCH ( gds(6), tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', -8, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	Grid Definition Template Number (see Code Table 3.1)
C
			IF ( cont ) THEN
			    CALL ND_GDS ( gds, gdsarr, iret )
			    jgrdnm = gds(13)
			    IF ( iret .ne. 0 )  THEN
				CALL ST_INCH ( jgrdnm, tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', -4, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 4 - PRODUCT DEFINITION SECTION
C
C*	Product Definition Template Number (see Code Table 4.0)
C
			IF ( cont ) THEN
			    CALL ND_PARM( is0(7), pds, parm, 
     +					  ihzrmp, idrct, iret)
     			    ighdr (1) = ihzrmp
			    ighdr (2) = idrct
			    IF ( iret .ne. 0 )  THEN	        
			        IF (iret .eq. 4 ) THEN		        
			            WRITE(errmsg,1020) is0(7),  	
     +                                   pds(10), pds(11), pds(8)
     
 1020				    FORMAT ("DIS# =", i3, 1x, 
     +                                      "CAT# =", i3, 1x, 
     +                                      "ID# =",  i3, 1x, 
     +                                      "PDT# =", i3)
                                ELSE
                                    errmsg = ' '
                                ENDIF
                                                    
				CALL ER_WMSG ( 'NDFDG2', iret, errmsg, 
     + 							ier )
				cont = .false.
			    END IF
			END IF
			IF ( cont ) THEN
			    jparm = pds(11)
			    CALL ND_FTIM ( is1, pds, jtime, jaccm, iret )
			    IF ( iret .ne. 0 ) THEN
				CALL ER_WMSG ( 'NDFDG2', iret, ' ', ier )
				cont = .false.
			    END IF
			END IF
			IF ( cont ) THEN
			    CALL TG_ITOC ( jtime, gdattm(1), ier )
			    gdattm(2) = ' '
C
C*	Type of first fixed surface (see Code Table 4.5)
C
			    jvcord = pds(23)
			    IF ( jvcord .ne. 1 ) THEN
				CALL ST_INCH ( jvcord, tmpprm, ier )
				CALL ER_WMSG ( 'NDFDG2', +2, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
			IF ( cont ) THEN
			    CALL LV_CORD ( 'NONE', vparm, ivcord, ier )
			    level(1) = 0
			    level(2) = -1
C
C*	SECTION 5 - DATA REPRESENTATION SECTION
C
			    IF ( is5(6) .lt. 1 ) THEN
				CALL ER_WMSG ( 'NDFDG2', -23, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*	SECTION 6 - BIT-MAP SECTION
C
			IF ( cont ) THEN
			    IF ( is6(6) .ne. 255 ) THEN
				CALL ER_WMSG ( 'NDFDG2', -18, tmpprm, ier )
				cont = .false.
			    END IF
			END IF
C
C*			List only or write the data to the GEMPAK file.
C
			IF  ( cont )  THEN
C
C*			Determine if the user only wants a LIST of the
C*			grids.
C
			    IF  ( lstflg )  THEN
C
C*			List the header information.
C
				knt = knt + 1
				prttl = .false.
				modknt = MOD(kntmsg,100)
				IF  ( knt .eq. 1 .or.
     +				      modknt .eq. 1 .or.
     +				      modknt .eq. 51 )
     +				        prttl = .true.
				    IF  ( nlun .ne. 0 )
     +					CALL GR_WGB2 ( lun, nlun, prttl,
     +						   kntmsg, gdattm, level,
     +						   ivcord, parm,
     +						   is0(7), pds(10), pds(11),
     +						   pds(8), jvcord,
     +						   jgrdnm, ier )
			    ELSE
				IF  ( igdfln .eq. 0 )  THEN
C
C*			Create/open the GEMPAK grid file, if necessary.
C
				    CALL ND_GCOG ( gdoutf, proj,
     +					  grdarea, kxky, cpyfil,
     +					  .false., garea, gdsarr, iskip,
     +					  maxg, igdfln, rnvblk, jgrdnm,
     +					  cprj, igx, igy, jx, jy,
     +					  ksbx, ksby, subset, iret )
				    IF  ( iret .ne. 0 )  THEN
					CALL ER_WMSG ( 'NDFDG2', iret,
     +							      ' ', ier )
					cont   = .false.
					proces = .false.
				    ELSE
					igxold = int ( rnvblk(5) )
				    END IF
				END IF
C
C*		Subset and skip as necessary, then write to the GEMPAK file.
C
				IF ( cont ) THEN
				    inx = igx
				    iny = igy
				    IF ( iskip .eq. 0 )  THEN
					IF ( .not. subset )  THEN
					    DO igpt = 1, inx*iny
					    IF (grid(igpt) .eq. 9999.0)
     +					  grid(igpt) = RMISSD
					    END DO
					ELSE
					    CALL ND_GSSG ( igxold, ksbx,
     +							 ksby, iskip,
     +							 grid, inx,
     +							 iny, iret )
					END IF
				    ELSE
				        IF ( .not. subset )  THEN
					    ksbx(1) = 1
					    ksbx(2) = inx
					    ksby(1) = 1
					    ksby(2) = iny
					END IF
					 CALL ND_GSSG ( igxold, ksbx, ksby,
     +					      iskip, grid, inx, iny, iret )
				    END IF
				END IF
				IF  ( cont )  THEN
				    nbits = is5(20) + 2
				    CALL GD_WPGD ( igdfln,
     +						grid, inx, iny,
     +						ighdr, gdattm,
     +						level, ivcord,
     +						parm, overwr,
     +						MDGGRB, nbits,
     +						ierr )
				    IF  ( ierr .eq. 0 )  THEN
					knt = knt + 1
					prttl = .false.
					modknt=MOD(kntmsg,100)       
					IF  ( knt .eq. 1 .or.
     +					      modknt .eq. 1.or.
     +					      modknt .eq. 51 )
     +					      prttl = .true.
					CALL GR_WGB2 ( lun,
     +						nlun, prttl,
     +						kntmsg, gdattm, level,
     +						ivcord, parm,
     +						is0(7), pds(10), pds(11), 
     +						pds(8), jvcord,
     +						jgrdnm, ier )
				    ELSE
					CALL ER_WMSG ( 'GD', ierr,
     +							' ', ier )
					IF ( ierr .eq. -11 )
     +					    proces = .false.
				    END IF
				END IF
			    END IF
			END IF
		    END IF
		END DO
C
C*		Write the output information to the luns.
C
		IF ( nlun .gt. 0 )  THEN
C
		    DO  ii = 1, nlun
C
			ilun = lun(ii)
C
			IF ( proces )  THEN
			    WRITE ( ilun, 1000 ) kntmsg
			ELSE
			    WRITE ( ilun, 1000 ) MAX(0,kntmsg-1)
			END IF
			WRITE ( ilun, 1001 ) gbfile
			IF  ( .not. lstflg )  THEN
			    WRITE ( ilun, 1010 ) knt
			    WRITE ( ilun, 1001 ) gdoutf
			END IF
C
		    END DO
C
		END IF
C
1000		FORMAT ( // 5X, I5,
     +          ' GRIB messages were read or scanned from the',
     +          ' GRIB file:' )
1001		FORMAT ( 5X, A72 / )
1010		FORMAT ( 5X, I5,
     +                  ' grids were written to the GEMPAK file:' )
C
C*		Update parameter values, display grid info 
C*		and close the files.
C
		IF  ( igdfln .ne. 0 )  THEN
C
		    CALL GD_CLOS ( igdfln, ier2 )
C
		    IF ( nlun .ne. 0 )  
     +			CALL GD_GENI ( gdoutf, nlun, lun, ier )
C
		END IF
C
		CALL GB_CLOS ( ier2 )
C
	    END IF
C
C*	    Call dynamic tutor.
C
	    CALL IP_DYNM ( done, ier )
C
	END DO
C
C*	Final error messages.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'NDFDG2', iperr, ' ', ier )
	CALL IP_EXIT ( iret )
C
	END
