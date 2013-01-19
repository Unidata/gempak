	SUBROUTINE GR_WTRM  ( lun, title, ignum, gdattm, level, ivcord,
     +			      parm, iret )
C************************************************************************
C* GR_WTRM								*
C*									*
C* This subroutine writes a grid identifier to the specified logical	*
C* unit using a standard format.  TITLE is set to indicate that the 	*
C* title line:								*
C*									*
C*    NUM    TIME1     TIME2     LEVEL1    LEVEL2   VCORD     PARM	*
C*									*
C* is to be written first.  If IGNUM is not positive, the grid 		*
C* number will not be written and will not be included in the title.	*
C*									*
C* GR_WTRM  ( LUN, TITLE, IGNUM, GDATTM, LEVEL, IVCORD, PARM, IRET )	*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit for write		*
C*	TITLE		LOGICAL		Flag to write title		*
C*	IGNUM		INTEGER		Grid number			*
C*	GDATTM (2)	CHAR*20		GEMPAK time			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/87						*
C* M. desJardins/GSFC	 6/88	GEMPAK4					*
C* M. desJardins/GSFC	 4/89	Added call to LV_CCRD			*
C* K. Brill/NMC		01/92	Add different WRITE format for VAX	*
C* M. desJardins/NMC	 3/92	Make line 79 characters to stop wrap	*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	gdattm (*), parm
	INTEGER		level  (*)
	LOGICAL		title
C*
	CHARACTER	vcord*4, dt1*18, dt2*16, p*12, lev*5
C-----------------------------------------------------------------------
	iret = 0
C
C*	Translate vertical coordinate.
C
	CALL LV_CCRD  ( ivcord, vcord, ier )
C
C*	Move character strings into variables with correct length.
C
	dt1 = gdattm (1)
	dt2 = gdattm (2)
	p      = parm
C
C*	Make level 2 into a string.  Write nothing if level2 = -1.
C
	IF  ( level (2) .eq. -1 )  THEN
	    lev = ' '
	  ELSE
	    CALL ST_INCH ( level (2), lev, ier )
	END IF
C
C*	Write title if requested, checking whether grid number is to
C*	be written.
C
	IF  ( ( title ) .and. ( ignum .gt. 0 ) )  THEN
	  IF  ( MTMACH .eq. MTVAX )  THEN	
	    WRITE  ( lun, 1000 )
1000	    FORMAT ( 1X, '  NUM', 7X, 'TIME1', 14X, 'TIME2', 11X,
     +                   'LEVL1 LEVL2', 2X, 'VCORD', 1X, 'PARM' )
	  ELSE
	    WRITE  ( lun, 1111 )
1111	    FORMAT ( '  NUM', 7X, 'TIME1', 14X, 'TIME2', 11X,
     +                   'LEVL1 LEVL2', 2X, 'VCORD', 1X, 'PARM' )
	  END IF
	ELSE IF  ( title )  THEN
	  IF  ( MTMACH .eq. MTVAX )  THEN
	    WRITE  ( lun, 1001 )
1001	    FORMAT ( 5X, 'TIME1', 13X, 'TIME2', 9X,
     +                   'LEVL1 LEVL2', 3X, 'VCORD', 1X, 'PARM'  )
	  ELSE
	    WRITE  ( lun, 1002 )
1002	    FORMAT ( 4X, 'TIME1', 13X, 'TIME2', 9X,
     +                   'LEVL1 LEVL2', 3X, 'VCORD', 1X, 'PARM'  )
	  END IF
	END IF
C
C*	Write the grid identifier.
C
	IF  ( ignum .gt. 0 )  THEN
	  IF  ( MTMACH .eq. MTVAX )  THEN
	    WRITE  ( lun, 2000 ) ignum, dt1, dt2, level (1), lev,
     +				 vcord, parm
2000	    FORMAT ( 1X, I5, 5X, A, A, 2x, I6, 2X, A, 2X, A, 1X, A )
	   ELSE
	    WRITE  ( lun, 2111 ) ignum, dt1, dt2, level (1), lev, 
     +				 vcord, parm
2111	    FORMAT ( I5, 5X, A, A, 2x, I6, 2X, A, 2X, A, 1X, A )
	  END IF
	 ELSE
	  IF  ( MTMACH .eq. MTVAX )  THEN
	    WRITE  ( lun, 2001 ) dt1, dt2, level (1), lev, vcord, 
     +				 parm
2001	    FORMAT ( 1X, A, 1X, A, I6, 2X, A, 3X, A, 1X, A )
	   ELSE
	    WRITE  ( lun, 2002 ) dt1, dt2, level (1), lev, vcord, 
     +				 parm
2002	    FORMAT ( A, 1X, A, I6, 2X, A, 3X, A, 1X, A )
	  END IF
	END IF
C*
	RETURN
	END
