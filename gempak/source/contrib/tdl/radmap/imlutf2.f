	SUBROUTINE IM_LUTF2  ( REF, iret )
C************************************************************************
C* IM_LUTF2
C* 
C Modified from GEMPAK original for NIDS only optimization. Store       *
C color map hard-coded here                                             *
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	logical REF
C*
	integer red(16,2)/    0,   0,   1,   0,   0,  0 ,   0, 255, 
     +			    231, 255, 255, 214, 192, 255, 153, 255,  !Ref
     +                        0,  50,   0,   0,   0,   0,  90, 180, 
     +			    118, 200, 255, 255, 230, 250, 250,   0/  !Vel
	integer green(16,2)/  0, 236, 160,   0, 255, 200, 144, 255, 
     +		  	    192, 144,   0,   0,   0,   0,  85, 255,  !Ref
     +                        0,   0, 128, 224, 251, 187, 143, 190, 
     +			    118, 200, 255, 200, 100,   0,   0,   0/  !Vel
	integer blue(16,2)/   0, 236, 246, 246,   0,   0,   0,   0,  
     +			      0,   0,   0,   0,   0, 255, 201, 255,  !Ref
     +                        0, 150, 255, 255, 144,   0,  90, 180,  
     +			     50,   0,   0,  50,   0,  50, 200,   0/  !Vel

	PARAMETER	(MAXCOL = 256)
	INTEGER		icolrs (MAXCOL)
	INTEGER		irgun (MAXCOL), iggun (MAXCOL), ibgun (MAXCOL),
     +			ir (MAXCOL), ig (MAXCOL), ib (MAXCOL)
C------------------------------------------------------------------------
	iret = 0
C
C*      Check if IM_SIMG was called for seting up the navigation.
C
	IF  ( ( imbank .lt. 1 ) .or. ( imbank .gt. 2 ) )  RETURN
C
C*      Get the number of colors for this color bank
C
	CALL GQCLRS ( imbank, ncolr, iret )
	IF ( (iret .ne. 0) .or. (ncolr.eq.1) ) return

	j = 1
	if ( .not. REF ) j = 2

	do i=1,16
	   irgun(i) = red(i,j)
	   iggun(i) = green(i,j)
	   ibgun(i) = blue(i,j)
	end do   
	numcol = 16

        ratio = FLOAT (numcol - 1) / (ncolr - 1)
        DO i = 1, ncolr 
	   index = ((i-1) * ratio + .5) + 1
    	   icolrs (i) = i - 1
	   ir (i) = irgun (index)
	   ig (i) = iggun (index)
	   ib (i) = ibgun (index)
        END DO
	CALL GSBRGB ( imbank, ncolr, icolrs, ir, ig, ib, iret )
	RETURN
	END
