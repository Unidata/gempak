	SUBROUTINE ND_GCOG ( gdoutf, proj, grdarea, kxky, cpyfil,
     +			    fill, garea, gdsarr, iskip, maxg, igdfln, 
     +			    rnvful, igrdnm, cprj, ikx, iky, jx, jy, 
     +			    ksubx, ksuby, subset, iret )
C************************************************************************
C* ND_GCOG								*
C*									*
C* This routine will create a new GEMPAK grid file, or open an 		*
C* existing file.							*
C*									*
C* ND_GCOG ( GDOUTF, PROJ, GRDAREA, KXKY, CPYFIL, FILL, GAREA, GDSARR,	*
C*	    ISKIP, MAXG, IGDFLN, RNVFUL, IGRDNM, CPRJ, IKX, IKY, JX, 	*
C*	    JY,	KSUBX, KSUBY, SUBSET, IRET )				*
C*									*
C* Input parameters:							*
C*	GDOUTF		CHAR*		GEMPAK output grid file		*
C*	PROJ		CHAR*		Projection of grid to create	*
C*	GRDAREA		CHAR*		Grid area of grid to create	*
C*	KXKY		CHAR*		Number of points for grid	*
C*	CPYFIL		CHAR*		File or table number to copy	*
C*					   for navigation info		*
C*	FILL		LOGICAL		Fill flag			*
C*	GAREA		CHAR*		Grid subset area		*
C*	GDSARR (10)	REAL		GDS projection information	*
C*	ISKIP		INTEGER		Number of points to skip	*
C*									*
C* Input/output parameters:						*
C*	MAXG		INTEGER		Max number of grids for file	*
C*									*
C* Output parameters:							*
C*	IGDFLN		INTEGER		Output grid file number		*
C*	RNVFUL (*)	REAL		Navigation block for full grid	*
C*	IGRDNM		INTEGER		Navigation number from table	*
C*	CPRJ		CHAR*		Projection of the grid		*
C*	IKX		INTEGER		Number of points in X-direction	*
C*	IKY		INTEGER		Number of points in Y-direction	*
C*	JX		INTEGER		Number of X pts on filled grid	*
C*	JY		INTEGER		Number of Y pts on filled grid	*
C*	KSUBX (2)	INTEGER		Subset range in X-direction	*
C*	KSUBY (2)	INTEGER		Subset range in Y-direction	*
C*	SUBSET		LOGICAL		Subset flag			*
C*	IRET		INTEGER		Return code			*
C*						-10 GDOUTF = ' '	*
C*						-11 Error opening file	*
C*						-12 Error creating file	*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	04/03	Modified from NAGCOG 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdoutf, proj, grdarea, kxky, cpyfil, cprj,
     +			garea
	REAL		rnvful(*), gdsarr(*)
	INTEGER		ksubx(2), ksuby(2)
	LOGICAL		fill, subset
C*
	CHARACTER	cpytmp*72, name*4, newfil*132
	REAL		anlblk(LLNANL), rnvtst(LLNNAV),
     +			anltst(LLNANL), altln(4), rnvblk(LLNNAV)
	LOGICAL		exist, gsflag
	DATA		anlblk / LLNANL * 0 /
C------------------------------------------------------------------------
	iret = 0
C
C*	If the output file is blank, return.
C
	IF  ( gdoutf .eq. ' ' )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Check for the existence of the output file. If it exists, open
C*	it, if not, create it.
C
	CALL FL_INQR ( gdoutf, exist, newfil, ier )
	IF  ( exist )  THEN
	    WRITE ( 6, * ) 'Opening the GEMPAK grid file'
	    CALL GD_OPNF ( gdoutf, .true., igdfln, inav, rnvblk,
     +			   ianl, anlblk, ihdsz, maxg, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -11
		RETURN
	    END IF
C
C*	    Check the navigation information against the GRIB info.
C
	    CALL ST_LCUC ( cpyfil, cpytmp, ier )
	    IF  ( cpyfil .eq. ' ' )  THEN
C
C*	    	CASE 1: Get navigation block from the user input.
C
		CALL ND_GNAV ( proj, kxky, grdarea, cprj, ikx, iky,
     +			      altln, rnvful, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -11
		    RETURN
		END IF
	    ELSE IF  ( cpyfil(1:1) .eq. '#' )  THEN
C
C*	    	CASE 2: Get navigation block from the grid
C*			navigation table input.
C
		CALL ND_GTBL ( cpyfil, name, cprj, ikx, iky, altln,
     +			      rnvful, anltst, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -11
		    RETURN
		END IF
		CALL ST_NUMB ( cpyfil(2:), igrdnm, ier )
	    ELSE IF  ( cpytmp .eq. 'GDS' )  THEN
C
C*	    	CASE 3: Get navigation block from the GDS information.
C
		IF ( fill .and. gdsarr (2) + gdsarr (3) .eq. 0. ) THEN
		    iret = -21
		    RETURN
		END IF
		CALL ND_GGDS ( gdsarr, cprj, ikx, iky, altln, rnvful,
     +			      ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -11
		    RETURN
		END IF
	    END IF
	    DO ii = 1, LLNNAV
		rnvtst(ii) = rnvful(ii)
	    END DO
	    CALL GR_SUBA ( garea, fill, rnvtst, altln, ksubx, ksuby,
     +			  subset, iret )
	    IF ( iret .ne. 0 ) RETURN
            IF ( iskip .gt. 0 )  THEN 
		IF ( ( rnvtst(5) / iskip ) .le. 4 .OR.
     +			( rnvtst(6) / iskip ) .le. 4 ) THEN
		    iret = -24
		    RETURN
		ELSE
                    rnvtst(5) = rnvtst(5) / (iskip + 1)
                    rnvtst(6) = rnvtst(6) / (iskip + 1)
		END IF
            END IF  
	    CALL GR_CNAV ( rnvblk, rnvtst, LLNNAV, gsflag, ier )
	    IF  ( .not. gsflag )  THEN
		iret = -11
		RETURN
	    END IF
	ELSE
C
C*	    Create a new file by one of the methods below.
C
	    WRITE ( 6, * ) 'Creating the GEMPAK grid file...'
	    ihd = 10
	    CALL ST_LCUC ( cpyfil, cpytmp, ier )
	    IF  ( cpyfil .eq. ' ' )  THEN
C
C*	    	CASE 1: Build new navigation block from the user input.
C
		CALL ND_GNAV ( proj, kxky, grdarea, cprj, ikx, iky,
     +			      altln, rnvful, ierr )
		IF  ( ierr .eq. 0 )
     +		    CALL ND_GANL ( '4/0;0;0;0', rnvful, anlblk, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -12
		    RETURN
		END IF
	    ELSE IF  ( cpyfil(1:1) .eq. '#' )  THEN
C
C*	    	CASE 2: Build new navigation and analysis blocks from
C*			the grid navigation table input.
C
		CALL ND_GTBL ( cpyfil, name, cprj, ikx, iky, altln,
     +			      rnvful, anlblk, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -12
		    RETURN
		END IF
		CALL ST_NUMB ( cpyfil(2:), igrdnm, ier )
	    ELSE IF  ( cpytmp .eq. 'GDS' )  THEN
C
C*	    	CASE 3: Build new navigation block from the GDS
C*			information.
C
		IF ( fill .and. gdsarr (2) + gdsarr (3) .eq. 0. ) THEN
		    iret = -21
		    RETURN
		END IF
		CALL ND_GGDS ( gdsarr, cprj, ikx, iky, altln, rnvful,
     +			      ierr )
		IF  ( ierr .eq. 0 )
     +		    CALL ND_GANL ( '4/0;0;0;0', rnvful, anlblk, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -12
		    RETURN
		END IF
	    ELSE
C
C*	    	CASE 4: Get navigation and analysis blocks from the
C*			old file.
C
		CALL GD_OPNF ( cpyfil, .false., iflno, inav, rnvful,
     +			       ianl, anlblk, ihd, imxgd, ierr )
		IF  ( ierr .ne. 0 )  THEN
		    iret = -12
		    RETURN
		END IF
		CALL GD_CLOS ( iflno, ier )
		CALL GR_RNAV ( rnvful, cprj, ikx, iky, ier )
		altln(1) = rnvful(7)
		altln(2) = rnvful(8)
		altln(3) = rnvful(9)
		altln(4) = rnvful(10)
	    END IF
C
C*	    Check that the output filename is not blank.
C
	    IF  ( gdoutf .eq. ' ' )  THEN
		iret = -7
		RETURN
	    END IF
C
C*	    Create the file.
C
	    DO ii = 1, LLNNAV
		rnvblk (ii) = rnvful (ii)
	    END DO
	    CALL GR_SUBA ( garea, fill, rnvblk, altln, ksubx, ksuby,
     +			  subset, iret )
	    IF ( iret .ne. 0 ) RETURN
	    IF ( iskip .gt. 0 )  THEN
		IF ( ( rnvblk(5) / iskip ) .le. 4 .OR.
     +			( rnvblk(6) / iskip ) .le. 4 ) THEN
		    iret = -24
		    RETURN
		ELSE
		    rnvblk(5) = rnvblk(5) / (iskip + 1)
		    rnvblk(6) = rnvblk(6) / (iskip + 1)
		END IF            
	    END IF
	    igszch = NINT ( rnvblk(5) ) * NINT ( rnvblk(6) )
	    IF ( igszch .gt. LLMXTG ) THEN
		iret = -21
		RETURN
	    END IF
	    CALL GD_CREF ( gdoutf, LLNNAV, rnvblk, LLNANL, anlblk,
     +			   ihd, maxg, igdfln, ierr )
	    IF  ( ierr .ne. 0 )  THEN
		iret = -12
		RETURN
	    END IF
	END IF
C
C*	If necessary, get dimensions of staggered grid.
C
	IF ( fill ) THEN
	    CALL GR_RNAV ( rnvful, name, jx, jy, ier )
	    jx = ( jx + 1 ) / 2
	END IF
	WRITE ( 6, * ) 'GEMPAK grid file is ready...'
C*
	RETURN
	END
