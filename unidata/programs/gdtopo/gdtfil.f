	SUBROUTINE GDTFIL ( gdfile, ikx, iky, gfunc, time1, time2, 
     +			ivcord, nbits, gltln, iret )
C************************************************************************
C* GDTFIL								*
C*									*
C* This subroutine will create a grid file for the topography data.	*
C*									*
C* GDTFIL ( GDFILE, IKX, IKY, GFUNC, TIME1, TIME2, IVCORD, NBITS, GLTLN,*
C*		 IRET )							*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*(*)	Grid file name			*
C*	IKX		INTEGER		Number of X points		*
C*	IKY		INTEGER		Number of Y points		*
C*	GLTLN(4)	REAL		Lat/lon bounds			*
C* Output parameters:							*
C*	IGDFLN		INTEGER		Grid file number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	11/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, gfunc, time1, time2
	INTEGER		ikx, iky, ivcord, nbits, iret
	REAL		gltln(*)
C*
	CHARACTER	proj*4, newfil*(LLMXLN)
	REAL		dbnds(4), rnvblk(LLNNAV), anlblk(LLNANL)
	INTEGER		ebnds(4)
	LOGICAL		angflg, exist
C------------------------------------------------------------------------
C
C*	See if file exists
C
	CALL FL_INQR ( gdfile, exist, newfil, ier )
C
	IF ( .not. exist ) THEN
	    proj   = 'CED'
	    angle1 = 0.
	    angle2 = 0.
	    angle3 = 0.
	    angflg = .false.
	    deltan = 0.0
	    maxgrd = 10
	    navsz  = LLNNAV
	    ianlsz = LLNANL
	    ihdrsz = 2
	    DO  i = 1, 4
	        ebnds(i) = 0.0
	        dbnds(i)  = RMISSD
	    END DO
C
	    CALL GR_MNAV ( proj, ikx, iky, gltln(1), gltln(2), 
     +		       gltln(3), gltln(4), angle1, angle2,
     +		       angle3, angflg, rnvblk, ier1 )
C
	    CALL GR_MBN2 ( deltan, ebnds, dbnds, rnvblk, anlblk, ier2 )
C
C
	    CALL GD_CREF ( gdfile, navsz, rnvblk, ianlsz, anlblk,
     +		       ihdrsz, maxgrd, igdfln, iret )
	ELSE
C
C*	    Open the grid file
C
	    CALL GD_OPEN ( gdfile, .true., LLNANL, LLNNAV, igdfln,
     +			anlblk, rnvblk, maxgrd, iret )
	END IF
C*
	IF ( iret .eq. 0 ) THEN
	    write(*,*) 'write out grid '
	    CALL cgdtwdt ( igdfln, ikx, iky, gfunc, time1, time2, ivcord, 
     +		nbits, rnvblk, ier)
	    CALL GD_CLOS ( igdfln, ier)
	END IF
C
	RETURN
	END
