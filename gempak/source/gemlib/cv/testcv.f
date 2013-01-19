	PROGRAM TESTCV
C************************************************************************
C* TESTCV								*
C*									*
C* This program tests the CURVE library subroutines.			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/98						*
C* S. Jacobs/NCEP	 8/98	Changed call to CV_PRMT			*
C* S. Jacobs/NCEP	 4/99	Added CV_RDUC				*
C* J. Wu/GSC	   	 8/00	Added CV_MDFY				*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	REAL		xpt (LLMXPT), ypt (LLMXPT),
     +			xcl (LLMXPT), ycl (LLMXPT),
     +			xcv (LLMXPT), ycv (LLMXPT)
        CHARACTER*20	filename
C------------------------------------------------------------------------
	CALL IN_BDTA  ( ier )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  ( 6, 20 )
20	    FORMAT (' 1 = CV_PRMT   2 = CV_RDUC   3 = CV_MDFY')
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	    IF  ( ier .eq. 2 )  THEN
		iostat = -1
		numsub = -1
	    END IF
C------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*) ' Enter number of original points: '
		READ  (5,*) np
		WRITE (6,*) ' Enter ', np, ' value pairs: '
		READ  (5,*) ( xpt (i), ypt (i), i = 1, np )
		WRITE (6,*) ' Enter the smoothing density: '
		READ  (5,*) dens
		WRITE (6,*) ' Enter the curve scaling factor: '
		READ  (5,*) crvscl
		WRITE (6,*) ' Enter the start point of the segment: '
		READ  (5,*) istrt
		WRITE (6,*) ' Enter the end point of the segment: '
		READ  (5,*) iend
		CALL CV_PRMT  ( np, xpt, ypt, dens, LLMXPT, crvscl,
     +				istrt, iend, nout, xcv, ycv, iret )
		WRITE (6,*) 'nout, iret = ', nout, iret
		DO  i = 1, nout
		    WRITE (6,*) xcv (i), ycv (i)
		END DO
		CALL ER_WMSG ( 'CV',  iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 2 )  THEN
		WRITE (6,*) ' Enter number of original points: '
		READ  (5,*) np
		WRITE (6,*) ' Enter ', np, ' value pairs: '
		READ  (5,*) ( xpt (i), ypt (i), i = 1, np )
		WRITE (6,*) ' Enter the tolerance: '
		READ  (5,*) eps
		CALL CV_RDUC  ( np, xpt, ypt, eps,
     +				nout, xcv, ycv, iret )
		WRITE (6,*) 'nout, iret = ', nout, iret
		DO  i = 1, nout
		    WRITE (6,*) xcv (i), ycv (i)
		END DO
		CALL ER_WMSG ( 'CV',  iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 3 )  THEN
		WRITE (6,*) ' Enter your data source: '
	        WRITE (6,40)
40	        FORMAT (' 1 = Type in    2 = Read from file')
		READ  (5,*) choice
	        
		IF  ( choice .eq. 2 )  THEN
		  WRITE (6,*) ' Enter file name ( sample.dat? ): '
		  READ  (5,'(A20)') filename		  
		  WRITE (6,*) ' Enter type of line [0-open 1-close]: '
		  READ  (5,*) nline
		  WRITE (6,*) ' Enter smoothing level [0, 1, 2]: '
		  READ  (5,*) nsm_lvl
		  OPEN (20, FILE = filename, STATUS = 'old', IOSTAT = iv)
                  IF ( iv .GT. 0 ) THEN
		    WRITE (6,*) ' Cannot open your file, please type in: '
		    choice = 1
		  ELSE
		    REWIND(20)
		    READ  (20,*) npo, ( xpt( i ), i = 1, npo )
		    READ  (20, *)  ( ypt( i ), i = 1, npo )
		    READ  (20, *) npc, ( xcl( i ), i = 1, npc )
		    READ  (20, *)  ( ycl( i ), i = 1, npc )
		    CLOSE (20)
		  END IF
		END IF
		
		IF  ( choice .eq. 1 )  THEN
		  WRITE (6,*) ' Enter number of original points: '
		  READ  (5,*) npo
		  WRITE (6,*) ' Enter ', npo, ' value pairs: '
		  READ  (5,*) ( xpt( i ), ypt( i ), i = 1, npo )
		  WRITE (6,*) ' Enter number of clicked points: '
		  READ  (5,*) npc
		  WRITE (6,*) ' Enter ', npc, ' value pairs: '
		  READ  (5,*) ( xcl( i ), ycl( i ), i = 1, npc )
		  WRITE (6,*) ' Enter smoothing level [0, 1, 2]: '
		  READ  (5,*) nsm_lvl
		  WRITE (6,*) ' Enter type of line [0-open 1-close]: '
		  READ  (5,*) nline
                ENDIF
		
		IF ( ( choice .eq. 1 ) .or. ( choice .eq. 2 ) ) THEN
		  CALL CV_MDFY  ( npo, xpt, ypt, npc, xcl, ycl, nsm_lvl, nline,
     +				  LLMXPT, nout, xcv, ycv, is, ie, iret )
		  WRITE (6,*) 'nout, is, ie, iret = ', nout, is, ie, iret		
		  DO  i = 1, nout
		    WRITE (6,*) xcv (i), ycv (i)
		  END DO
		ENDIF
		CALL ER_WMSG ( 'CV',  iret, ' ', ierr)
C------------------------------------------------------------------------
	    END IF
	END DO
C*
	END
