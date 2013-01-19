	PROGRAM TESTBR
C************************************************************************
C* PROGRAM TESTBR							*
C*									*
C* This programs tests some of the BR library subroutines.		*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* H. Zeng/SAIC		07/05	added an argument for BR_VTEC		*	
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER	cnties*160, zones(LLSTFL)*6, vtec*44
	CHARACTER	vact*3, vorig*4, vtype*2, vstrt*11, vsig*2,
     +                  vend*11, vnum*4, vprdc*5
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C*
	    WRITE (6,1000)
1000	    FORMAT ('  1 = BR_CNTY      2 = BR_VTEC' )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, nsub, n, ier )
	    IF ( ier .eq. 2 ) THEN
	   	iostat = -1
	   	nsub   = -1	
	    END IF
C------------------------------------------------------------------------
	    IF  ( nsub .eq. 1 )  THEN
		WRITE (6,*) 'Counties/Zones string: '
		READ  (5,2)  cnties 
2		FORMAT (A)
		CALL BR_CNTY (cnties, zones, nzone, iret )
		WRITE (6,*) ' iret = ', iret
		IF ( nzone .gt. 0 ) THEN
		    DO ii = 1, nzone
			WRITE (6,*) zones (ii)
		    END DO
		END IF
	    END IF
C------------------------------------------------------------------------
	    IF  ( nsub .eq. 2 )  THEN
		WRITE (6,*) 'Enter VTEC string - example :'
                WRITE(6,*)'(NEW.KALY.TO.A.1004.0210151422Z-0210252000Z)'
		READ  (5,2)  vtec
                CALL BR_VTEC ( vtec, vact, vorig, vtype, vsig, vnum,
     +                       vstrt, vend, vprdc, iret)
		WRITE (6,*) ' iret = ', iret
                IF ( iret .eq. 0 ) THEN
		    WRITE (6,*) 'Action          = ', vact 
		    WRITE (6,*) 'Office ID       = ', vorig
		    WRITE (6,*) 'Phenomena       = ', vtype
		    WRITE (6,*) 'Significance    = ', vsig
		    WRITE (6,*) 'Event track num = ', vnum
		    WRITE (6,*) 'Beginning time  = ', vstrt
		    WRITE (6,*) 'Ending time     = ', vend
		    WRITE (6,*) 'Product Class   = ', vprdc
                END IF
	    END IF
C------------------------------------------------------------------------
	WRITE (6,*) '  '
	END DO
C*
	END
