	SUBROUTINE SNLWLP  ( prmlst, nparms, rdata, mlev, luns, nlun, 
     +			     iret )
C************************************************************************
C* SNLWLP								*
C*									*
C* This subroutine writes the level data to the output devices.		*
C*									*
C* SNLWLP  ( PRMLST, NPARMS, RDATA, MLEV, LUNS, NLUN, IRET )		*
C*									*
C* Input parameters:							*
C*	PRMLST (NSTNP)	CHAR*		Level parameters		*
C*	NPARMS		INTEGER		Number of level parameters	*
C*	RDATA		REAL		Level data			*
C*	  (NPARMS,MLEV)							*
C*	MLEV		INTEGER		Number of levels		*
C*	LUNS   (NLUN)	INTEGER		Output LUNS			*
C*	NLUN		INTEGER		Number of output units		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* S. Schotz/GSC	 8/90   Eliminate blank lines on output		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmlst (*)
	REAL		rdata  (NPARMS,MLEV)
	INTEGER		luns   (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Write blank line.
C
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1000 )
1000	    FORMAT ( 1X )
	END DO
C
C*	Write out list of parameters.
C
	DO  i = 1, nlun
            IF  ( nparms .lt. 9 ) THEN
	        WRITE  ( luns (i), 1010, IOSTAT = iostat )  
     +		       ( prmlst (j), j = 1, nparms )
            ELSE
	        WRITE  ( luns (i), 1015, IOSTAT = iostat )  
     +		       ( prmlst (j), j = 1, nparms )
            END IF
1010        FORMAT ( 1X, 8 ( 5X, A4 ) )
1015	    FORMAT ( 1X, 8 ( 5X, A4 ), / , ( 10X, 7 ( 5X, A4 ) ) )
	END DO
C
C*	Write out all levels of data.
C
	DO  i = 1, nlun
	    DO  m = 1, mlev
                IF  ( nparms .lt. 9 )  THEN 
		    WRITE  ( luns (i), 1020, IOSTAT = iostat ) 
     +			   ( rdata (j,m), j = 1, nparms )
                ELSE
		    WRITE  ( luns (i), 1025, IOSTAT = iostat ) 
     +			   ( rdata (j,m), j = 1, nparms )
                END IF
1020            FORMAT ( 1X, 8 F9.2 )
1025		FORMAT ( 1X, 8 F9.2, / , ( 10X, 7 F9.2 ) )
	    END DO
	END DO
C*
	RETURN
	END
