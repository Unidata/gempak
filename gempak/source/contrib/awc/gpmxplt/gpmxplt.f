        PROGRAM GPMXPLT
C************************************************************************
C* GPMXPLT                                                              *
C* With a given GAREA, DEVICE, and PROJ this program computes a new     *
C* GAREA that will fill out the entire DEVICE                           *
C*                                                                      *
C* Since this is really only useful for setting GAREA and exiting, we   *
C* only set the area for the first frame in an image loop               *
C**                                                                     *
C* Log:                                                                 *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C*
        CHARACTER       map*(LLMXLN), device*(LLMXLN), panel*(LLMXLN),
     +                  garea*(LLMXLN), proj*(LLMXLN), satfil*(LLMXLN), 
     +                  radfil*(LLMXLN), text*(LLMXLN), nproj*(LLMXLN),
     +                  oproj*(LLMXLN)
C*
        INTEGER         iret, iperr, idrpflg
C*      
        CHARACTER       imgfls(MXLOOP)*132, ucproj*(LLMXLN), 
     +                  value*(LLMXLN), tvalx(2)*20, tvaly(2)*20,
     +                  angles(3)*20, margins(4)*20
C*
        LOGICAL         respond, done, process, modproj
C*
        REAL            xlv, ybv, xrv, ytv,
     +                  xcv, ycv, xcntr, ycntr, xdst, ydst,
     +                  lm, bm, rm, tm,
     +                  vlm, vbm, vrm, vtm,
     +                  xlwr, xupr, ylwr, yupr, rwdth, rhght,
     +                  szmkx, szmky, sztxx, sztxy, szwbx, szwby
     +                  angl1,  angl2,  angl3, dlatll, dlonll, dlatur,
     +                  dlonur
C-----------------------------------------------------------------------
C
C*      Initialize user interface and graphics.
C
        iperr = 0
        respond = .TRUE.
        CALL IP_INIT  ( respond, iperr )
        IF  ( iperr .ne. 0 )  THEN
C
C*          IP_INIT Already reports the error, we just need to exit
C
            CALL SS_EXIT
        END IF
C
C*      Read program parameters from pdf file
C
        CALL IP_IDNT ( 'GPMXPLT', iret )
C
C*      Initialize graphics.
C
        CALL GG_INIT  ( 1, iret )
        IF  ( iret .eq. 0 )  THEN
            done = .false.
        ELSE 
            CALL ER_WMSG  ( 'GPMXPLT', -3, ' ', iret )
            done = .true.
        END IF

C
C*      While we have something to do, doit
C
        DO WHILE ( .not. done )
            process = .true.
C
C*          Get input parameters.
C
            CALL GPMXIN ( device, garea, proj, satfil, radfil, 
     +                    panel, text, iret )
            IF  ( iperr .eq. 0 )  THEN
C
C*              Set device and projection.
C
                CALL GG_SDEV  ( device, iret )
                IF  ( iret .ne. 0 )  process = .false.
C
C*              Set text attributes
C
                CALL IN_TEXT  ( text, iret )
                IF  ( iret .ne. 0 )  process = .false.

                IF  ( process )  THEN
C
C*                  If projection=SAT or RAD, see if multiple image
C*                  files have been specified.
C
                    modproj = .true.
                    CALL ST_LCUC ( proj, ucproj, iret )
                    IF  ( ucproj(1:3) .eq. 'SAT' )  THEN
                        CALL ST_FLST ( satfil, ';', ' ', MXLOOP, imgfls,
     +                                  numimg, iret )
                        modproj = .false.
                    ELSE IF  ( ucproj(1:3) .eq. 'RAD' )  THEN
                        CALL ST_FLST ( radfil, ';', ' ', MXLOOP, imgfls,
     +                                  numimg, iret )
                        modproj = .false.
                    END IF
C
C*                  Set map projection, from first image if specified
C                 
                    CALL GG_MAPS ( proj, garea, imgfls (1), 
     +                            idrpflg, iret )
                    IF  ( iret .ne. 0 )  process = .false.
C
C*                  If that is OK, create the new GAREA
C
                    IF  ( process )  THEN
C
C*                      In case the GAREA was not specified by lat/lon, get
C*                      back the projection since if it was DEF, and we change
C*                      the GAREA to a lat/lon, the DEF proj will not work
C*                      anymore
C
                        CALL GQMPRJ  ( nproj,  angl1,  angl2,  angl3,
     +                       dlatll, dlonll, dlatur, dlonur, iret )
C
C*                      Go to the first frame.
C
                        CALL GSTANM ( iret )
C
C*                      Display user options, allow program exit.
C
                        IF  ( respond ) THEN
                            CALL GPMOPT ( device, garea, proj, satfil, 
     +                                    radfil, panel, text, iret )
                        END IF
                        
                        IF  ( iret .ne. 0 )  process = .false.
C
                        IF  ( process )  THEN
C
C*                          Set the panel.
C
                            CALL GG_PANL ( panel, iret )
C
C*                          Get the edges of the view in normalized coords
C
                            CALL GQBND ('V', xlv, ybv, xrv, ytv, iret )
C
C*                          Get the margin sizes
C
                            CALL GQMMGN ( lm, bm, rm, tm, iret)
C
C*                          If the margins are specified as fractional
C*                          of the view width, just compute the size
C*                          based on the view coords. Otherwise compute
C*                          based on the text size.
C
                            IF ( ( 0.0 .le. lm ) .and. 
     +                           ( lm .lt. 1.0 ) .and.
     +                           ( 0.0 .le. bm ) .and. 
     +                           ( bm .lt. 1.0 ) .and. 
     +                           ( 0.0 .le. rm ) .and. 
     +                           ( rm .lt. 1.0 ) .and.
     +                           ( 0.0 .le. tm ) .and. 
     +                           ( tm .lt. 1.0 ) ) THEN
                                vlm = lm * ( xrv - xlv )
                                vbm = bm * ( ytv - ybv )
                                vrm = rm * ( xrv - xlv )
                                vtm = tm * ( ytv - ybv )
                            ELSE
C
C*                              Get the text size info
C
                                CALL GQSYSZ ( szmkx, szmky, sztxx,
     +                                        sztxy, szwbx, szwby, 
     +                                        iret )
                                vlm = lm * sztxx
                                vbm = bm * sztxy
                                vrm = rm * sztxx
                                vtm = tm * sztxy
                            END IF
C
C*                          Now, adjust the view by the margin amount, this
C*                          will be the largest area we can display, and it
C*                          should be in the correct location
C
                            xlv = xlv + vlm
                            xrv = xrv - vrm
                            ybv = ybv + vbm
                            ytv = ytv - vtm
C
C*                          Now get the map coords for the view, and this
C*                          should be the largest garea possible
C
                            CALL GTRANS ( 'V', 'M', 1, xlv, ybv,
     +                                    xlwr, ylwr, ier )
                            CALL GTRANS ( 'V', 'M', 1, xrv, ytv,
     +                                    xupr, yupr, ier )
C
C*                          If any of the resulting values are missing, then
C*                          we have gone off the ranch (like in a satellite 
C*                          image where we don't have an upper right that
C*                          has a lat/lon since it is off the globe).  In that
C*                          case, we can't do anything so leave it alone
C
                            IF ( .not. ERMISS ( xlwr ) .and. 
     +                           .not. ERMISS ( ylwr ) .and.
     +                           .not. ERMISS ( xupr ) .and. 
     +                           .not. ERMISS ( yupr ) ) THEN
C
C*                              Create and set the new garea.
C                             
                                CALL ST_RLCH ( xlwr, 2, tvalx(1), iret )
                                CALL ST_RLCH ( ylwr, 2, tvaly(1), iret )
                                CALL ST_RLCH ( xupr, 2, tvalx(2), iret )
                                CALL ST_RLCH ( yupr, 2, tvaly(2), iret )
                                CALL ST_LSTR ( tvalx(1), lenx1, iret )
                                CALL ST_LSTR ( tvaly(1), leny1, iret )
                                CALL ST_LSTR ( tvalx(2), lenx2, iret )
                                CALL ST_LSTR ( tvaly(2), leny2, iret )
                                value = 'GAREA=' // 
     +                                  tvalx(1)(:lenx1) // ';' // 
     +                                  tvaly(1)(:leny1) // ';' // 
     +                                  tvalx(2)(:lenx2) // ';' // 
     +                                  tvaly(2)(:leny2)
                                CALL IP_SVAR ( value, iret )
C                             
C*                              Create and set the projection, just in case
C*                              BUT! Only for non SAT or RAD projections
C                             
                                IF ( modproj ) THEN
                                    CALL ST_RLCH ( angl1, 2, angles(1),
     +                                            iret )
                                    CALL ST_RLCH ( angl2, 2, angles(2), 
     +                                            iret )
                                    CALL ST_RLCH ( angl3, 2, angles(3), 
     +                                            iret )
                                    CALL ST_LSTR ( angles(1), lena1, 
     +                                            iret )
                                    CALL ST_LSTR ( angles(2), lena2, 
     +                                            iret )
                                    CALL ST_LSTR ( angles(3), lena3, 
     +                                            iret )
                              
                                    CALL ST_RLCH ( lm, 2, margins(1), 
     +                                            iret )
                                    CALL ST_RLCH ( bm, 2, margins(2), 
     +                                            iret )
                                    CALL ST_RLCH ( rm, 2, margins(3), 
     +                                            iret )
                                    CALL ST_RLCH ( tb, 2, margins(4), 
     +                                            iret )
                                    CALL ST_LSTR ( margins(1), lenm1, 
     +                                            iret )
                                    CALL ST_LSTR ( margins(2), lenm2, 
     +                                            iret )
                                    CALL ST_LSTR ( margins(3), lenm3, 
     +                                            iret )
                                    CALL ST_LSTR ( margins(4), lenm4, 
     +                                            iret )
                              
                                    CALL ST_LSTR ( nproj, lenp, iret )
                                    value = 'PROJ=' // 
     +                                      nproj(:lenp) // '/' //
     +                                      angles(1)(:lena1)  // ';' //
     +                                      angles(2)(:lena2)  // ';' //
     +                                      angles(3)(:lena3)  // '/' //
     +                                      margins(1)(:lenm1) // ';' //
     +                                      margins(2)(:lenm2) // ';' //
     +                                      margins(3)(:lenm3) // ';' //
     +                                      margins(4)(:lenm4) 
                                    IF ( idrpflg .gt. 0 ) THEN
                                        CALL ST_LSTR ( value, lenv, 
     +                                                 iret )
                                        IF ( idrpflg .eq. 1 ) THEN
                                            value = value(:lenv)//'|D'
                                        ELSE
                                            value = value(:lenv)//'|ND'
                                        END IF
                                    END IF
                                    CALL IP_SVAR ( value, iret )
                                END IF
                            ELSE
                                iperr = -4
                            END IF
                        END IF
                    END IF
                END IF
            END IF
C
C*          Call the dynamic tutor.
C
            IF (respond) THEN
                CALL IP_DYNM ( done, iret )
            ELSE
                done = .TRUE.
            END IF
        END DO
C*
        IF  ( iperr .ne. 0 )  THEN
            CALL ER_WMSG  ( 'GPMXPLT', iperr, ' ', iret )
        END IF

        CALL GENDP   ( 0, iret )
        CALL IP_EXIT ( iret )
C*
        END
