	PROGRAM  GD2NDFD
C************************************************************************
C* PROGRAM GD2NDFD							*
C*									*
C* This program converts a GEMPAK grid file to an NDFD (grib2) grid 	*
C* file.								*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	2/03	Created from GDGRIB			*
C* T. Piper/SAIC	5/03	Added CTB_G2READ			*
C* C. Bailey/HPC	7/04	Fixed For AWIPS encoding 		*
C* R. Tian/SAIC         3/05    Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( KFILDO = 6 )
C!					Unit number of the output diagnostic file
	PARAMETER	( MXLGDS = 96 )
C!					Maximum GDS length
	PARAMETER	( MXLPDS = 64 )
C!					Maximum PDS length
	PARAMETER	( ND5    = 800000 )
C!					Dimension of the IPACK array
	PARAMETER	( MISSP  = IMISSD )
C!					Integer primary missing value
	PARAMETER	( XMISSP = RMISSD )
C!					Floating point primary missing value
	PARAMETER	( MISSS	 = IMISSD )
C!					Integer secondary mising value
	PARAMETER	( XMISSS = RMISSD )
C!					Floating point secondary mising value
	PARAMETER	( NEW    = 1 )
C!					Packing only one gridded data field per 
C!					GRIB2 message
	PARAMETER	( MINPK  = 14 )
C!					Minimum size of the groups that the complex 
C!					packing method can break the data down into
	PARAMETER	( L3264B = 32 )
C!					Integer word length in bits of the 
C!					machine being used (either 32 or 64)
        PARAMETER ( ND2X3 = 800000 )
        PARAMETER ( NIDAT = 1 )
        PARAMETER ( NRDAT = 1 )
        PARAMETER ( NDJER = 15 )
        PARAMETER ( NS0 = 16, NS1 = 21, NS2 = 10 )
        PARAMETER ( NS5 = 49, NS6 = 8, NS7 = 10 )

        REAL     	rdat
        INTEGER  	idat, ib(ND2X3), intdtf(3), idtarr(5)
        INTEGER		ipack(ND5), jer(NDJER, 2), kjer
	CHARACTER	cpack*(4*ND5)
        INTEGER 	is0(NS0), is1(NS1), is2(NS2), gds(MXLGDS),
     +          	pds(MXLPDS), is5(NS5), is6(NS6), is7(NS7)
        LOGICAL 	big, match, pk_endian
C*
C*
	CHARACTER	gdfile*(LLMXLN), gfunc*(LLMXLN),
     +			gdatim*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), gbfile*(LLMXLN),
     +			center*(LLMXLN), wmohdr*(LLMXLN)
C*
	CHARACTER	filnam*256
	CHARACTER	gbfcur*(LLMXLN)
C*
	CHARACTER*10	tmpprm
	CHARACTER*20	gdattim(2)
	INTEGER		ighdr(LLGDHD), fcstim, level(2)
	REAL		grid(LLMXTG), anlblk(LLNANL), rnvblk(LLNNAV)
	INTEGER		igrid(LLMXTG), ipos(3), gpid
	REAL		rlevel( LLMXLV, 2 )
	CHARACTER	parm*12, chdr*22, cdd*2, chhmm*4,
     +			prmlst(MMPARM)*12, timfnd(LLMXGT)*36,
     +			tmfst*20, tmlst*20, trange*36, vparm*12
	LOGICAL		respnd, done, proces, exist
	EQUIVALENCE	( igrid, grid ), ( cpack, ipack )
C
C*	COMMONs added to prevent STACK OVERFLOW ERROR
C 
	COMMON/IB/ib
	COMMON/IN/igrid
C*
        DATA is0, is5, is7, ipack / NS0*0, NS5*0, NS7*0, ND5*0 /
	DATA gds, pds / MXLGDS*0, MXLPDS*0 /
	DATA idat, rdat / 0, 0 /
        DATA timfnd / LLMXGT*' ' /
C---------------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT( 'GD2NDFD', ier )
C
C*  Initialize GEMPLT.
C
	    mode = 1
	    CALL GG_INIT  ( mode, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
		CALL DG_INTL ( ier )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C 
C*  Initialize GRIB2 parameter table
C 
	CALL CTB_G2READ (iret)
	IF  ( iret .eq. 0 )  THEN
	    ibitmap = 0
	    iclean  = 0
	    gbfcur  = ' '
	    maxlgd  = MXLGDS
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in user parameters and compute diagnostics.
C                
	DO WHILE ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    ignum = 0
	    proces = .true.
C
C*	    Read in the variables from the user interface.
C
	    CALL NDFDINP( gdfile, gfunc, gdatim, glevel, gvcord,
     +			  gbfile, center, wmohdr, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
		done = .true.
	    ELSE
C
C*		Open the grid file.
C
		CALL FL_INQR( gdfile, exist, filnam, iret )
		IF ( .not. exist ) THEN
		    CALL ER_WMSG( 'FL', iret, ' ', ier )
		    proces = .false.
		END IF
		IF ( proces ) THEN
		    CALL GD_OPEN ( gdfile, .false., LLNANL, LLNNAV, igdfln,
     +                             anlblk, rnvblk, naxgrd, iret )
		    CALL GD_NGRD ( igdfln, numgrd, tmfst, tmlst, ier )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG ( 'GD2NDFD', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Open the output GRIB2 file if it is a new file.
C
		IF ( proces ) THEN
		    IF ( gbfile .ne. gbfcur ) THEN
			IF ( gbfcur .ne. ' ' ) CALL GBF_CLOS( ier )
			CALL ST_LSTR ( gbfile, lng, ier )
			gbfile (lng+1:lng+1) = CHAR(0)
			CALL GBF_AOPN( gbfile, ier )
			proces = ( ier .eq. 0 )
			IF ( proces ) gbfcur = gbfile
		    END IF
		END IF
		IF ( proces ) THEN
C
C*      	    Parse user input for gdatim.
C
        	    CALL ST_LCUC ( gdatim, gdatim, ier )
        	    IF ( gdatim .eq. 'ALL' ) THEN
            		ntime = 1
            		timfnd (1) = gdatim
          	      ELSE
            		CALL DG_NFIL ( gdfile, ' ', ier )
            		CALL DG_NDTM ( gdatim, iperr )
            		CALL DG_QTMS ( LLMXGT, .true., timfnd, ntime,
     +                                 trange, ier )
            		IF ( iperr .ne. 0 .or. ntime .lt. 1 ) THEN
			    proces = .false.
            		END IF
        	    END IF
C
C*      	    Parse user input for levels and vertical coordinate.
C
        	    CALL GDU_GLEV ( glevel, LLMXLV, gvcord, nlev, rlevel,
     +                              levtyp, vparm, icord, iperr)
        	    IF ( iperr .ne. 0 ) THEN
			proces = .false.
        	    END IF
C
C*      	    Parse user input for a list of parameters.
C
        	    CALL ST_LCUC ( gfunc, gfunc, ier )
        	    CALL ST_CLST ( gfunc, ';', ' ', MMPARM , prmlst,
     +                             nparm, iperr )
        	    IF ( iperr .ne. 0 .or. nparm .lt. 1 ) THEN
			proces = .false.
        	    END IF
		END IF
C*
		IF  ( proces )  THEN
		   
C*      Loop through all grids to find matches.
C
		knt = 0
10		DO  nbeg = 1, numgrd
C*
	    proces = .true.
            CALL GD_GIDN( igdfln, nbeg, gdattim, level, ivcord,
     +                      parm, ier )
C*
            CALL GDU_MTCH( LLMXLV, gdattim, level, ivcord, parm, levtyp,
     +                     nlev, rlevel, icord, nparm, prmlst, ntime,
     +                     timfnd, match, iret )
C*
            IF  ( iret .ne. 0 )  THEN
                CALL ER_WMSG( 'GDU', iret, ' ', ier )
C                proces = .false. 
                GO TO 10 
            END IF
C
            IF  ( match )  THEN
                knt = knt + 1
C
C*              Read grid from input file.
C
                CALL GD_GGRD( igdfln, nbeg, gdattim, level, ivcord,
     +                        parm, grid, igx, igy, ighdr, ier )
                IF  ( ier .ne. 0 )  THEN
                    iret = -7
                    CALL ER_WMSG( 'GDMOD', iret, ' ', ier )
                  ELSE
C
C
C*********************************************************************************
C*	Generate Section 0 - Indicator Section
C*********************************************************************************
C
		    is0(7) = 0
C!	Discipline - GRIB Master Table Number (see Code Table 0.0)
C!	0 == Meteorological products
C
C*********************************************************************************
C*	Generate Section 1 - Identification Section
C*********************************************************************************
C
		    is1(5) = 1
C!	Number of section
		    CALL ST_ILST( center, '/', 0, 3, ipos, num, ier )
		    IF ( ipos(1) .ne. 0 ) THEN
		        is1(6) = ipos(1)
		    ELSE
		        is1(6) = 7 
C!      ID of originating/generating center
C!	7 = NCEP
		    END IF
		    IF ( ipos(2) .ne. 0 ) THEN
		        is1(8) = ipos(2)
		    ELSE
		        is1(8) = 5
C!	ID of originating/generating sub-center
C!	5 = HPC
		    END IF
		    IF (ipos (3) .ne. 0) THEN	    
		        gpid = ipos(3)
		    ELSE
		        gpid = 0
	            END IF
C!      Generating Process ID	
		    is1(10) = 1
C!	GRIB Master Tables Version Number (see Code Table 1.0)
C!	1 == Initial operational version number
		    is1(11) = 0
C!	GRIB Local Tables Version Number (see Code Table 1.1)
C!	0 == Local tables not used
		    CALL TG_CTOI( gdattim, intdtf, iret )
		    is1(12) = intdtf(3) / 100000
		    fcstim = mod(intdtf(3), 100000) / 100
C!	Significance of Reference Time (see Code Table 1.2)
		    CALL TI_CTOI( gdattim, idtarr, iret )
		    is1(13) = idtarr(1)
		    is1(15) = idtarr(2)
		    is1(16) = idtarr(3)
		    is1(17) = idtarr(4)
		    is1(18) = idtarr(5)
		    is1(19) = 0 
C!	Reference time of data (YYYY,MM,DD,HH,MM,SS)
		    is1(20) = 0
C!	Production status (see Code Table 1.3)
C!	0 == Operational products
		    is1(21) = 1
C!	Type of processed data (see Code Table 1.4)
C!	1 == Forecast products
C
C*********************************************************************************
C*	Generate Section 2 - Local Use Section
C*********************************************************************************
C
		    is2(5) = 2
C!      Number of section
C
C*********************************************************************************
C*	Generate Section 3 - Grid Definition Section
C*********************************************************************************
C
		    IF ( proces ) THEN
			CALL NDFDGDS( .true., rnvblk, 16, maxlgd, gds,
     +			       ier )  	       
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG( 'GD2NDFD', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*********************************************************************************
C*	Generate Section 4 - Product Definition Section
C*********************************************************************************
C
		    IF ( proces ) THEN
		        CALL NDFDPDS( rnvblk, parm, ivcord, level, 
     +				 idtarr, fcstim, gpid, pds, ier )
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG( 'GD2NDFD', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*********************************************************************************
C*	Generate Section 5 - Data Representation Section
C*********************************************************************************
C
		    is5(5) = 5
C!      Number of section
		    is5(10) = 2
C!	2 = Grid point data - complex packing
C!	Data Representation Template Number (see Code Table 5.0)
C!	Data Representation Template (see Template 5.x, where x is the
C!	Data Representation Template Number given in octets 10-11)
		    is5(21) = 0
C!	Type of original field values (see Code Table 5.1)
C!	0 == Floating point
		    is5(22) = 1
C!	Group splitting method used (see Code Table 5.4)
C!	1 == General Group Splitting
		    is5(23) = 1
C!	Missing value management used (see Code Table 5.5)
C!	1 == Primary missing values included within data values
C
C*********************************************************************************
C*	Generate Section 6 - Bit-Map Section
C*********************************************************************************
C
		    is6(5) = 6
C
C*********************************************************************************
C*	Generate Section 7 - Data Section
C*********************************************************************************
C
		    is7(5) = 7
C
C*		    Finally make the WMO header.
C
		    IF ( proces ) THEN
			ncntr = is1(6) 
			cdd = gdattim(1)(5:6)
			chhmm = gdattim(1)(8:11) 
			CALL GDGWMO( wmohdr, ncntr, cdd, chhmm, chdr,
     +				      ier )
		    END IF
		    IF ( proces .and. ier .lt. 0 ) THEN
			CALL ER_WMSG( 'GD2NDFD', ier , ' ', irr )
			proces = .false.
		    ELSE IF ( ier .gt. 0 ) THEN
			CALL ER_WMSG( 'GD2NDFD', ier , ' ', irr )
		    END IF
C
C*		    Write the grid to the file.
C
		    IF  ( proces )  THEN
C
C*			Write out all sections, as needed.
C
			IF ( chdr(1:20) .ne. ' ' ) THEN
			    CALL GBF_WRIT( 21, chdr, ier )
			END IF
			CALL PK_GRIB2( kfildo, grid, igrid, igx, igy,
     +				       idat, NIDAT, rdat, NRDAT, is0,
     +				       ns0, is1, ns1, gds, MXLGDS, pds,
     +				       MXLPDS, is5, ns5, is6, ns6,
     +				       is7, ns7, ib, ibitmap, ipack,
     +				       nd5, MISSP, XMISSP, MISSS,
     +				       XMISSS, NEW, MINPK, iclean, L3264B,
     +				       jer, ndjer, kjer)
			IF ( jer(kjer,2) .eq. 2 )  THEN
			    CALL ST_INCH( jer(kjer,1), tmpprm, ier )
			    CALL ER_WMSG( 'NDFDG2', -9, tmpprm, ier )
			    proces = .false.
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    big = PK_ENDIAN()
			    IF ( .not. big ) THEN
				CALL MV_SWP4( ND5, ipack, ipack )
			    END IF
			    CALL GBF_WRIT( is0(9), cpack, ier)
			END IF
		    END IF
		END IF
	    END IF
	END DO
C
C*      If no grids were found, write a warning message.
C
        IF  ( knt .eq. 0 )  THEN
            CALL ER_WMSG( 'GDU', 2, ' ', ierr )
        END IF
	END IF
C
C*	Prompt for next diagnostic to be done.
C
		CALL IP_DYNM( done, ier )
	    END IF
	END DO
	IF ( gbfcur .ne. ' ' ) CALL GBF_CLOS( ier )
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG( 'GD2NDFD', iperr, ' ', ier )
	CALL IP_EXIT( iret )
C*
	END
