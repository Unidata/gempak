	SUBROUTINE POLY_GROUP ( indx, rlat, rlon, ki, kj, iret )
C************************************************************************
C* POLYT_GROUP  							*
C*									*
C* This subroutine groups wind data based on warning categories and	*
C* DISTNM defined in POLY_PARM.TBL.					*
C*									*
C* POLY_GROUP ( indx, rlat, rlon, ki, kj, iret )			*
C*									*
C* Input parameters:							*
C*	INDX		INTEGER		Data type			*
C*					 1: gale			*
C*					 2: storm			*
C*					 3: hurricane			*
C*	RLAT		REAL		Latitude 			*
C*	RLON		REAL		Longitude 			*
C*	KI		INTEGER		Grid in x-direction		*
C*	KJ		INTEGER		Grid in y-direction		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					+2 = too many points in polygon	*	
C*					+3 = too many groups		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	INTEGER		igroup (MAXGRP), ng (NCATEG)
	REAL		dist
	LOGICAL		done, found, ok
C-----------------------------------------------------------------------
	iret = 0
	found = .false.
C
	ng ( indx ) = 1
	done = .false.
	ifound = 0
	DO WHILE ( .not. done )
C
	    npts = npoint ( ng ( indx ), indx )
C
C*	    Calculate the distance between the point of interests,
C*	    (RLAT, RLON) and the points in the group.  Repeat for 
C*	    all groups.
C
	    np = npts
	    IF ( np .eq. 0 ) np = 1
	    ok = .false.
	    DO WHILE ( .not. ok ) 
		pr = pgrlat ( np, ng ( indx ), indx )
		ps = pgrlon ( np, ng ( indx ), indx )	
		CALL CLO_DIST ( rlat, rlon, 1, pr, ps, dist, ier )
		IF ( dist .le. ( distnm * NM2M ) )  THEN
		    ifound =  ifound + 1
		    igroup ( ifound ) = ng ( indx )
		    ok = .true.
		END IF
		np = np - 1
		IF ( np .le. 0 ) ok = .true.
	    END DO
C
	    ng ( indx ) = ng ( indx )  + 1
	    IF ( ng ( indx ) .gt. ngroup ( indx ) ) THEN
		done = .true.
	    END IF
	END DO
C
C*	If only one point is found within the radius, the point will be 
C*	added to the existing group.  If the point does not belong to 
C*	any group, a new group will be created.  If the point can be 
C*	added to more than one group, these groups are combined and 
C*	group numbers are re-assigned.
C
	IF ( ifound .eq. 0 )  THEN
	    ngroup ( indx ) = ngroup ( indx ) + 1
	    IF ( ngroup ( indx ) .gt. MAXGRP )   THEN
		iret = +3
	      ELSE
		npoint ( ngroup ( indx ), indx ) = 1
		pgrlat ( 1, ngroup ( indx ), indx )  = rlat
		pgrlon ( 1, ngroup ( indx ), indx )  = rlon  
		kgridu ( 1, ngroup ( indx ), indx )  = ki
		kgridv ( 1, ngroup ( indx ), indx )  = kj
	    END IF
	  ELSE IF ( ifound .eq. 1 )  THEN
	    IF ( ngroup ( indx ) .eq. 0 ) ngroup ( indx ) = 1
	    ng ( indx ) = igroup ( 1 )
	    npoint ( ng ( indx ), indx ) = npoint ( ng (indx),indx ) + 1
	    IF ( npoint ( ng ( indx ), indx ) .gt. MAXPTS )  THEN
		iret = +2
		RETURN
	    END IF
	    pgrlat ( npoint ( ng (indx), indx ), ng (indx), indx )= rlat
	    pgrlon ( npoint ( ng (indx), indx ), ng (indx), indx )= rlon
	    kgridu ( npoint ( ng (indx), indx ), ng (indx), indx )= ki
	    kgridv ( npoint ( ng (indx), indx ), ng (indx), indx )= kj
	  ELSE
C
	    isg = igroup ( 1 )
	    ist = npoint ( isg, indx )
	    itotal = 0
	    DO WHILE ( ifound .gt. 1 )
		ngth = igroup ( ifound )
		npts = npoint ( igroup ( ifound ), indx )
		itotal = itotal + npts
		DO ii = 1, npts
		    ist = ist + 1
		    pgrlat ( ist, isg, indx ) =  pgrlat (ii, ngth, indx)
		    pgrlon ( ist, isg, indx ) =  pgrlon (ii, ngth, indx)
		    kgridu ( ist, isg, indx ) =  kgridu (ii, ngth, indx)
		    kgridv ( ist, isg, indx ) =  kgridv (ii, ngth, indx)
		END DO
C
C*		Move the last group to the vacant group.
C
		iend = npoint ( ngroup ( indx ), indx )
		DO jj = 1, iend
		    pgrlat ( jj, ngth, indx ) =  
     +				pgrlat ( jj, ngroup ( indx ) ,indx )
		    pgrlon ( jj, ngth, indx ) =  
     +				pgrlon ( jj, ngroup ( indx ), indx )
		    kgridu ( jj, ngth, indx ) =  
     +				kgridu ( jj, ngroup ( indx ) ,indx )
		    kgridv ( jj, ngth, indx ) =  
     +				kgridv ( jj, ngroup ( indx ), indx )
		END DO
		ngroup ( indx ) = ngroup ( indx ) - 1
		npoint ( ngth, indx ) = iend
		ifound = ifound - 1
	    END DO
C
C*	    Add the point of interests.
C
	    npoint ( isg, indx ) = itotal + npoint ( isg, indx ) + 1
	    IF ( npoint ( isg, indx ) .gt. MAXPTS )  THEN
		iret = +2
		RETURN
	    END IF
C
	    pgrlat ( npoint ( isg, indx ), isg, indx ) =  rlat
	    pgrlon ( npoint ( isg, indx ), isg, indx ) =  rlon
	    kgridu ( npoint ( isg, indx ), isg, indx ) =  ki
	    kgridv ( npoint ( isg, indx ), isg, indx ) =  kj
	END IF
C*
	RETURN
	END
