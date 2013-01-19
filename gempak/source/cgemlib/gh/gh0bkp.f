	SUBROUTINE GH_0BKP ( stormname, cadvnum, istrmtype, rlat, rlon, 
     &                       ifont, rsize, iwidth, iret )
C************************************************************************
C* GH_0BKP								*
C*									*
C* This subroutine draws a text box containing tropical storm namne,    *
C* type and advisory number.  It is called when there are no            *
C* watch/warning breakpoints.                                           *
C*									*
C* GH_0BKP ( stormname, cadvnum, istrmtype, rlat, rlon, iret )           *
C*									*
C* Input parameters:							*
C*	STORMNAME	CHARACTER	Array of breakpoint numbers     *
C*	CADVNUM		CHARACTER	Storm advisory number           *
C*	ISTRMTYPE	INTEGER		Storm type                      *
C*	RLAT		REAL		Lat to place zero WW text box   *
C*	RLON		REAL		Lon to place zero WW text box   *
C*	IFONT		INTEGER		Text font in zero WW text box   *
C*	RSIZE		REAL		Text size in zero WW text box   *
C*	IWIDTH		INTEGER		Text width in zero WW text box  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	11/05	                                        *
C* S. Gilbert/NCEP	01/06	        Changed advisory num from int   *
C*                                      to char                         *
C************************************************************************
c	INCLUDE		'GEMPRM.PRM'
c	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		istrmtype, lenc
	INTEGER		lennum, lenname
        REAL		rlat, rlon
	character*(*)	stormname, cadvnum
        character*200   ctext
C*
C------------------------------------------------------------------------
	iret = 0
        call gh_save( ier )
C
C  Create text string to display in box
C
        ctext(1:200)=" "
        if ( istrmtype .EQ. 0 )  ctext(1:) = "Hurricane "
        if ( istrmtype .EQ. 1 )  ctext(1:) = "Tropical Storm "
        if ( istrmtype .EQ. 2 )  ctext(1:) = "Tropical Depression "
        if ( istrmtype .EQ. 3 )  ctext(1:) = "Subtropical Storm "
        if ( istrmtype .EQ. 4 )  ctext(1:) = "Subtropical Depression "
        call st_lstr( cadvnum, lennum, ier )
        call st_lstr( stormname, lenname, ier )

        call st_lstr( ctext, lenc, ier )
        ctext(lenc+1:) = " " // stormname // char(13)
        call st_lstr( ctext, lenc, ier )
        ctext(lenc+1:) = "Advisory " // cadvnum(1:lennum) // char(13)
        call st_lstr( ctext, lenc, ier )
        ctext(lenc+1:) = "No current Watches/Warnings"
        call st_lstr( ctext, lenc, ier )
C        
C   Display text box
C        
        call gscolr( 5, ier)
        call gstext( ifont, 2, rsize, iwidth, 211, 1, 2, ier)
        call gtext( 'M', rlat, rlon, ctext, 0.0, 0, 0, ier)

        call gh_rest( ier )
C*
	RETURN
	END
