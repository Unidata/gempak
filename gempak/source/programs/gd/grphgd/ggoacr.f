	SUBROUTINE GGOACR ( filnam, dattim, type, iret ) 
C************************************************************************
C* GGOACR                                                               *
C*                                                                      *
C* This subroutine creates a new surface data file for the vector.	*
C*                                                                      *
C* GGOACR ( FILNAM, IRET )						* 
C*                                                                      *
C* Input parameters:                                                    *
C*	FILNAM		CHARACTER	Surface file name		*
C*   	DATTIM		CHARACTER	GEMPAK date/time		*
C*      TYPE		CHARACTER       WIND TYPE			*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -1 = file not created          *
C*                                                                     	*
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		 4/05						*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C*
        CHARACTER*(*)   filnam, dattim, type
C
        CHARACTER*4     parms (2)
        CHARACTER*5     stn 

        INTEGER         iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
        REAL            wnd(2)
        LOGICAL         pkflg, stmflg
C------------------------------------------------------------------------
        iret     = 0
	iflsrc   = MFUNKN
	nparm    = 2
	parms(1) = "SPED"
	parms(2) = "DRCT"
	maxrpt   = 4000
	pkflg    = .false.
	stmflg   = .false.
	DO ii = 1, MMPARM
            iscale(ii)   = RMISSD
            iofset(ii)   = RMISSD
            ibits(ii)    = RMISSD
        END DO
C
C*      Create the file.
C
        CALL SF_CSDF  ( filnam, iflsrc, nparm, parms,  maxrpt, pkflg, 
     +                  iscale, iofset, ibits, stmflg, isffln, iret )
        
        IF ( iret .ne. 0 ) THEN
            CALL ER_WMSG  ( 'GGOACR', -1, ' ', ier )
            RETURN
        END IF
C*
        CALL ST_NUMB ( dattim(8:11), ihm, ier )
        DO ii = 1, nwnd
C
C*         Write wind speed to file
C
           IF ( (type(1:1) .eq. 'A' .and. wtype(ii) .eq. 9) .or.
     +          (type(1:1) .eq. 'B' .and. wtype(ii) .eq. 8) ) THEN
               wsped(ii) = PR_KNMS ( wsped(ii) )
               wnd(1) = wsped(ii)
               wnd(2) = wdrct(ii)
               CALL ST_INCH (ii, stn, ier)
               CALL SF_WSDD ( isffln, dattim, stn, 0, wlat(ii),
     +                        wlon(ii), 0, '--', '--', ihm, wnd, ier ) 
           END IF
        END DO
                       
C*
        CALL SF_CLOS  ( isffln, ier )
        RETURN
        END

