	SUBROUTINE TBSGRID (lun,cntr,gid,cid,proj,ang,gar,nx,ny,
     +                      scol,srow,iret)

	INTEGER 	lun,cntr,gid,cid,nx,ny,srow,scol
	CHARACTER*(*)	proj
	REAL		ang(3),gar(4)

	CHARACTER       tbline*80, tbpars (15)*8
	INTEGER		ntbp, ier

	iret = 0
C
C*      Read in next record.
C
        READ   ( lun, 10, IOSTAT = iostat )  tbline
10      FORMAT ( A )
        IF  ( iostat .ne. 0 )  THEN
            iret = -1
            RETURN
        END IF

C
C*      Parse the line, then convert each item, if needed.
C
        CALL ST_CLST  ( tbline, ' ', ' ', 15, tbpars, ntbp, ier )
        IF  ( ( ntbp .lt. 15 ) .or. ( ier .ne. 0 ) )  THEN
            iret = -2
            RETURN
        END IF

	CALL ST_NUMB  ( tbpars (1), cntr, ier0 )
	CALL ST_NUMB  ( tbpars (2), gid,  ier1 )
	CALL ST_NUMB  ( tbpars (3), cid,  ier2 )
	proj = tbpars (4)

        CALL ST_CRNM  ( tbpars (5),  ang (1), ier3 )
        CALL ST_CRNM  ( tbpars (6),  ang (2), ier4 )
        CALL ST_CRNM  ( tbpars (7),  ang (3), ier5 )
        CALL ST_CRNM  ( tbpars (8),  gar (1), ier6 )
        CALL ST_CRNM  ( tbpars (9),  gar (2), ier7 )
        CALL ST_CRNM  ( tbpars (10), gar (3), ier8 )
        CALL ST_CRNM  ( tbpars (11), gar (4), ier9 )

	CALL ST_NUMB  ( tbpars (12), nx, ier10 )
	CALL ST_NUMB  ( tbpars (13), ny, ier11 )
	CALL ST_NUMB  ( tbpars (14), scol, ier12 )
	CALL ST_NUMB  ( tbpars (15), srow, ier13 )

        IF  ( ( ier0 + ier1 + ier2 + ier3  + ier4 + ier5 + ier6 +
     +      ier7 + ier8 + ier9 + ier10 + ier11 + ier12 + ier13) .ne. 0 )
     +      iret = -9
C*
        RETURN
        END
