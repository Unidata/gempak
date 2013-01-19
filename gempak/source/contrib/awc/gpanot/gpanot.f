        PROGRAM GPANOT
C************************************************************************
C* GPANOT                                                               *
C*                                                                      *
C* This program will allow the user to place an object at any location  *
C* on the graphics device.                                              *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Jacobs/SSAI       12/91                                           *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER       device*72, gdfile*72, proj*72, garea*72,
     +                  panel*72, shape*72, info*256,
     +                  loci*144, ctype*72, line*72,
     +                  satfil*(LLMXLN), radfil*(LLMXLN),
     +                  imgfil(MXLOOP)*132, ucproj*72
        LOGICAL         clear, respnd, done, lflag, sflag, nflag, bflag
        LOGICAL         fflag
C*
        REAL            rnvblk(256), anlblk(128)
C------------------------------------------------------------------------
C*      Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPANOT', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*      Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
	    CALL GD_INIT  ( iret )
	    done = .false.
	ELSE 
	    CALL ER_WMSG  ( 'GPANOT', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPANOT', ier )
C
        DO  WHILE ( .not. done )
C
C*          Get input parameters.
C
            CALL GPNINP  ( device, gdfile, satfil, radfil, proj, 
     +                     garea, panel, clear, shape, info, loci, 
     +                     ctype, line, iperr )
            IF  ( iperr .eq. 0 )  THEN
C
C*              Set device.
C
                CALL GG_SDEV  ( device, ier )
C*
                IF  ( ier .eq. 0 )  THEN
C
C*                  If projection=SAT or RAD, see if multiple image
C*                  files have been specified.
C
                    imgfil(1) = ' '
                    CALL ST_LCUC ( proj, ucproj, ier )
                    IF  ( ucproj (1:3) .eq. 'SAT' )  THEN
                        CALL ST_FLST ( satfil, ';', ' ', MXLOOP, imgfil,
     +                                 numimg, ier )
                    ELSE IF  ( ucproj (1:3) .eq. 'RAD' )  THEN
                        CALL ST_FLST ( radfil, ';', ' ', MXLOOP, imgfil,
     +                                 numimg, ier )
                    END IF
C
C*                  Go to the first frame.
C
                    CALL GSTANM ( ier )
C
C*                  Display user options, allow program exit.
C
                    CALL GPNOPT ( device, gdfile, satfil, radfil, proj,
     +                            garea, panel, clear, shape, info, 
     +                            loci, line, ier )
                    IF  ( ier .eq. 0 )  THEN
C
C*                      Clear screen, draw panel, get bounds,
C*                      set map, set grid.
C
                        IF ( clear ) CALL GCLEAR ( iret )
                        CALL GG_PANL ( panel, iret )
                        CALL GG_MAPS ( proj, garea, imgfil(1), idrpfl, 
     +                                 ier )
                        CALL GD_OPNF ( gdfile, .false., igdfln, navsz,
     +                                 rnvblk, ianlsz, anlblk, ihdrsz,
     +                                 maxgrd, ier )
                        IF  ( ier .eq. 0 )  THEN
                            CALL GR_RNAV ( rnvblk, proj, kx, ky, ier )
                            CALL GR_SNAV ( navsz, rnvblk, ier )
                            CALL GD_CLOS ( igdfln, ier )
                        END IF
C
C*                      Choose a section based on SHAPE.
C
                        CALL ST_LCUC ( shape, shape, ier )
C
C*                      Set contour type.
C
                        CALL IN_CTYP  ( ctype, nflag, lflag, sflag,
     +                                  bflag,  fflag, iret )
                        IF ( ( shape .eq. 'ARC' ) .or. 
     +                       ( shape .eq. 'SPARC' ) ) THEN
                            ictyp = 3
                            IF ( fflag ) THEN
                                ictyp = 2
                            END IF
                            IF ( bflag ) THEN
                                ictyp = 1
                            END IF
                        ELSE
                          IF ( fflag ) THEN
                            ictyp = 2
                          ELSE
                            ictyp = 1
                          END IF
                        END IF
C(1)*
                        IF  ( shape .eq. 'TEXT' )  THEN
                            CALL GPNTXT ( ictyp, line, loci,
     +                                    info, 1, ier )
C*
                        ELSE IF  ( shape .eq. 'TXSY' )  THEN
                            CALL GPNTXT ( ictyp, line, loci,
     +                                    info, 2, ier )
C*
                        ELSE IF  ( shape .eq. 'POLYGON' )  THEN
                            CALL GPNPLY ( ictyp, line, loci,
     +                                    info, ier )
C*
                        ELSE IF  ( shape .eq. 'REGPOLY' )  THEN
                            CALL GPNREG ( ictyp, line, loci,
     +                                    info, ier )
C*
                        ELSE IF  ( shape .eq. 'ARC' )  THEN
                            CALL GPNARC ( ictyp, line, loci,
     +                                    info, 1, ier )
C*
                        ELSE IF  ( shape .eq. 'SPARC' )  THEN
                            CALL GPNARC ( ictyp, line, loci,
     +                                    info, 2, ier )
C*
                        ELSE IF  ( shape .eq. 'CURVE' )  THEN
                            CALL GPNCRV ( ictyp, line, loci,
     +                                    info, ier )
C*
                        ELSE IF  ( shape .eq. 'SPLN' )  THEN
                            CALL GPNLIN ( ictyp, line, loci,
     +                                    info, 2, ier )
C*
                        ELSE IF  ( shape .eq. 'LINE' )  THEN
                            CALL GPNLIN ( ictyp, line, loci,
     +                                    info, 1, ier )
C*
                        ELSE IF  ( shape .eq. 'ARROW' )  THEN
                            CALL GPNARW ( ictyp, line, loci,
     +                                    info, ier )
C*
                        ELSE IF  ( shape .eq. 'BARB' )  THEN
                            CALL GPNBRB ( ictyp, line, loci,
     +                                    info, ier )
C*
                        ELSE IF  ( shape .eq. 'COLDFRONT' )  THEN
                            CALL GPNFNT ( ictyp, line, loci,
     +                                    info, 1, ier )
C*
                        ELSE IF  ( shape .eq. 'WARMFRONT' )  THEN
                            CALL GPNFNT ( ictyp, line, loci,
     +                                    info, 2, ier )
C*
                        ELSE IF  ( shape .eq. 'OCCLFRONT' )  THEN
                            CALL GPNFNT ( ictyp, line, loci,
     +                                    info, 4, ier )
C*
                        ELSE IF  ( shape .eq. 'STATFRONT' )  THEN
                            CALL GPNFNT ( ictyp, line, loci,
     +                                    info, 3, ier )
C*
                        ELSE IF  ( shape .eq. 'WEATHER' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 1, ier )
C*
                        ELSE IF  ( shape .eq. 'CLOUD' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 2, ier )
C*
                        ELSE IF  ( shape .eq. 'SKY' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 3, ier )
C*
                        ELSE IF  ( shape .eq. 'MARKER' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 4, ier )
C*
                        ELSE IF  ( shape .eq. 'PRESTEND' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 5, ier )
C*
                        ELSE IF  ( shape .eq. 'PASTWTHR' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 6, ier )
C*
                        ELSE IF  ( shape .eq. 'TURB' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 7, ier )
C*
                        ELSE IF  ( shape .eq. 'ICNG' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 8, ier )
C*
                        ELSE IF  ( shape .eq. 'SPCL' )  THEN
                            CALL GPNSYM ( ictyp, line, loci,
     +                                    info, 9, ier )
C*
C                       ELSE IF  ( shape .eq. 'COLORSCALE' )  THEN
C                           CALL GPNCLR ( ictyp, line, loci, 
C     +                                   info, ier )
C*
                        END IF
C*
                        CALL GEPLOT ( iret)
C*
                    END IF
                END IF
            END IF
            CALL IP_DYNM ( done, iret )
        END DO
C
C*      Exit.
C
        IF  ( iperr .ne. 0 )  CALL ER_WMSG ('GPANOT', iperr, ' ', ier)
        CALL GENDP   ( 0, iret )
        CALL IP_EXIT ( iret )
C*
        END
