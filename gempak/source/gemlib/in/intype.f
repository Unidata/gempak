	SUBROUTINE IN_TYPE  ( type, scavld, vctvld, convld, strvld, 
     +			      pntvld, darvld, mrkvld, grdvld, wintyp, 
     +			      winuni, nflag, lflag, sflag, bflag, 
     +			      fflag, iret )
C************************************************************************
C* IN_TYPE								*
C*									*
C* This subroutine decodes the input for TYPE.				*
C*									*
C*   Valid inputs for type are:						*
C*									*
C*      SCALER TYPEs:							*
C*      C       the original GEMPAK contouring algorithm		*
C*      L       GEMPAK contouring algorithm without subboxes		*
C*      I       spline contour algorithm -- not implemented		*
C*      F       contour fill algorithm					*
C*      X       box algorithm -- same as contour fill, but draws lines	*
C*              around the polygons rather than filling in polygons.	*
C*	Z	fill the grid box according to grid value...no contour	*
C*		algorithm. This is intended for grid/image products.	*
C*      P       plot grid point values					*
C*      D       plot directional wind arrows				*
C*									*
C*      VECTOR TYPEs:							*
C*      A       wind arrows						*
C*      B       wind barbs						*
C*      S       streamlines						*
C*									*
C*      OTHER TYPEs:							*
C*      M       plot grid point markers					*
C*      G       plot grid indices (row/column numbers)			*
C*									*
C*									*
C* IN_TYPE  ( TYPE, SCAVLD, VCTVLD, CONVLD, STRVLD, PNTVLD, DARVLD, 	*
C*            MRKVLD, GRDVLD, WINTYP, WINUNI, NFLAG, LFLAG, SFLAG, 	*
C*	      BFLAG, FFLAG, IRET )					*
C*									*
C* Input parameters:							*
C*	TYPE		CHAR*		Contour attribute input		*
C*									*
C* Output parameters:							*
C*	SCAVLD		LOGICAL		Scaler flag			*
C*	VCTVLD		LOGICAL		Vector flag			*
C*	CONVLD		LOGICAL		Contour flag			*
C*	STRVLD		LOGICAL		Streamline flag			*
C*	PNTVLD		LOGICAL		Grid point flag			*
C*	DARVLD		LOGICAL		Directional arrow flag		*
C*	MRKVLD		LOGICAL		Marker flag			*
C*	GRDVLD		LOGICAL		Grid index flag			*
C*	WINTYP		CHAR*		Wind type (A or B)		*
C*	WINUNI		CHAR*		Wind units (M)			* 
C*      NFLAG           LOGICAL		Contour without subboxes flag	*	
C*	LFLAG		LOGICAL		Lagrangian contour flag		*
C*	SFLAG		LOGICAL		Spline contour flag		*
C*	BFLAG		LOGICAL		Box contour flag		*
C*	FFLAG		LOGICAL		Fill contour flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP 	 1/97						*
C* D.W.Plummer/NCEP 	 2/97	Added additional comments		*
C* D.W.Plummer/NCEP 	 5/98	Added directional arrows		*
C* S. Jacobs/NCEP	 9/98	Declared darvld as a LOGICAL		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* M. Li/GSC		 1/00	Added nflag				*
C* T. Piper/GSC		 1/01	Removed N as a valid vector type	*
C* S. Chiswell/Unidata	 2/01	Added Z scalar type			*
C* S. Gilbert/NCEP       4/07   set nflag to always return false. 'C'   *
C*                              'L' now specify same contour algorithm  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	type, wintyp, winuni
	LOGICAL		scavld, vctvld, convld, strvld
	LOGICAL		pntvld, mrkvld, grdvld, darvld
	LOGICAL		lflag, sflag, bflag, fflag, nflag
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_LCUC ( type, type, iret )
C
C*	Set contour type defaults.
C
	nflag = .false.
	lflag = .false.
	sflag = .false.
	bflag = .false.
	fflag = .false.
C
C*	Set wind information.
C
        wintyp = 'B'
        winuni = 'M'
C
C*	Set defaults.
C
        scavld = .false.
        vctvld = .false.
        convld = .false.
        strvld = .false.
        pntvld = .false.
        darvld = .false.
        mrkvld = .false.
        grdvld = .false.
C
C       SCALER TYPE INDICATORS:
C
C       C - contour lines (Lagrangian)
C       L - contour without subboxes 
C       I - contour lines (spline)
C       F - contour fill
C       X - contour fill boxes
C	Z - fill grid boxes
C       P - grid point values
C       D - directional arrows
C
C       VECTOR TYPE INDICATORS:
C
C       A - wind arrows
C       B - wind barbs
C       S - streamlines
C
C       MISCELLANEOUS TYPE INDICATORS:
C
C       M - grid point markers
C       G - grid index plotting
C
        iposc = INDEX ( type, 'C' )
        iposl = INDEX ( type, 'L' )
        iposi = INDEX ( type, 'I' )
        iposf = INDEX ( type, 'F' )
        iposx = INDEX ( type, 'X' )
        iposp = INDEX ( type, 'P' )
        iposd = INDEX ( type, 'D' )
	iposz = INDEX ( type, 'Z' )
	if((iposf .eq. 0).and.(iposz .ne. 0)) iposf = iposz
C
        iposa = INDEX ( type, 'A' )
        iposb = INDEX ( type, 'B' )
        iposs = INDEX ( type, 'S' )
C
        iposm = INDEX ( type, 'M' )
        iposg = INDEX ( type, 'G' )
C
        IF ( iposc .ne. 0 .or. iposl .ne. 0 .or. iposi .ne. 0 .or. 
     +       iposf .ne. 0 .or. iposx .ne. 0 .or. iposp .ne. 0 .or.
     +	     iposd .ne. 0 )  THEN
            scavld = .true.
            convld = .true.
            IF ( iposp .ne. 0 )  THEN
                pntvld = .true.
                convld = .false.
            END IF
            IF ( iposd .ne. 0 )  THEN
                darvld = .true.
                pntvld = .true.
                convld = .false.
		wintyp = 'D'
            END IF
	    IF ( iposc .ne. 0 )  THEN
		lflag = .true. 
	    END IF
            IF ( iposl .ne. 0 )  THEN
                lflag = .true.
            END IF
	    IF ( iposi .ne. 0 )  THEN
		sflag = .true. 
	    END IF
	    IF ( iposf .ne. 0 )  THEN
		fflag = .true. 
	    END IF
	    IF ( iposx .ne. 0 )  THEN
		bflag = .true. 
	    END IF
        ELSE IF ( iposa .ne. 0 .or. iposb .ne. 0 
     +            .or. iposs .ne. 0 )  THEN
            vctvld = .true.
            pntvld = .true.
            IF ( iposa .ne. 0 )  THEN
                wintyp = 'A'
            END IF
            IF ( iposb .ne. 0 )  THEN
                wintyp = 'B'
            END IF
            IF ( iposs .ne. 0 )  THEN
                pntvld = .false.
                strvld = .true.
            END IF
        END IF
C*
        IF ( iposm .ne. 0 )  THEN
            pntvld = .true.
            mrkvld = .true.
        END IF
        IF ( iposg .ne. 0 )  THEN
            pntvld = .true.
            grdvld = .true.
        END IF
C*
	RETURN
	END
