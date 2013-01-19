	PROGRAM RADMAP_SW
C***************************************************************************************
C Program to plot radar data on a map.  Derived from the GEMPAK program gpmap.  Uses
C numerous specially modified GEMPAK subroutines, modified for speed and efficiency of
C this specific task.  Assumes this will be built/linked directly with the GDP driver
C rather than through the normal GEMPAK-GPLT-DRIVER method.
C
C Usage:
C
C    radmap -(n|r|s|v|t|m|c) nids_file output_gif_file title_string [plot area]
C
C See the README for details
C
C***************************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'RADMAP.CMN'
C
	INTEGER         isize/1/
	CHARACTER*100	args(4), proj, garea
	LOGICAL         national, regional, site_ref, site_vel, 
     +			thumbnail, DoMissing, Clickable, DoColorBar
	REAL		angle1, angle2, angle3, dlatll, dlonll, dlatur, 
     +			dlonur, xsize(3)/620.,61.,640./, 
     +			ysize(3)/620.,46.,380./, tsize/.857/

C Get the first 4 command line args.  The optional 5th may be grabbed later.

	DO i=1,4
	     CALL GETARG (i, args(i))  ! 1=plot_type 2=nids_file, 3=out_file, 4=title, 5=garea
	END DO	

C Set the plot type flags.

	national = args(1)(2:2) .eq. 'n'
	regional = args(1)(2:2) .eq. 'r'
	site_ref = args(1)(2:2) .eq. 's'
	site_vel = args(1)(2:2) .eq. 'v'
	thumbnail = args(1)(2:2) .eq. 't'
        DoMissing = args(1)(2:2) .eq. 'm'
	Clickable = args(1)(2:2) .eq. 'c'
	if ( DoMissing ) site_vel = .true.
	DoColorBar = .not. thumbnail
	if ( thumbnail ) isize = 2
	if ( Clickable ) isize = 3
	REF = .not. site_vel

C If not a single site plot, get the requested plot area. 

	garea = 'DSET'
	IF  ( national .or. regional .or. thumbnail .or. Clickable ) 
     +			CALL GETARG (5, garea)

C Initialize the plotting device/surface.

	CALL GG_INIT ( 1, iret )
	IF  ( iret .ne. 0 )  THEN
	    print *, "RADMAP:  GG_INIT - iret = ", iret
	    call exit(1)
	ENDIF

	IF  ( args(3) .eq. " " )  args(3) = 'RADMAP.gif'
	CALL GSDEVA ('GIF', 1, args(3), 2, 
     +			xsize(isize), ysize(isize), ier )
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  GSDEVA - ier = ", ier
	    call exit(2)
	ENDIF
C
C Set the text size
C
        IF  ( thumbnail ) tsize = .01      ! Set text size for thumbnail to essentially (but not exactly) zero
        IF  ( Clickable ) tsize = .5

	CALL GSTEXT ( 1, 2, tsize, 1, 111, 1, 1, ier )  !font=2,hw=on,size=tsize,width=1
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  GSTEXT(1) - ier = ", ier
	    call exit(2)
	ENDIF
C
C Set the plotting geometry and plot the image
C
	CALL GG_MAPS ( 'RAD//4;2;0;2', garea, args(2), 
     +					    idrpfl, ier )
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  GG_MAPS - ier = ", ier
	    call exit(3)
	ENDIF
	CALL GSTANM ( ier )
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  GSTANM - ier = ", ier
	ENDIF
	CALL IM_LUTF2 ( REF, ier )
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  IM_LUTF2 - ier = ", ier
	    call exit(3)
	ENDIF
	CALL IM_DROP2 ( DoMissing, DoColorBar, ier )
	IF  ( ier .ne. 0 )  THEN
	    print *, "RADMAP:  IM_DROP2 - ier = ", ier
	    call exit(4)
	ENDIF
	IF ( DoColorBar ) THEN
            CALL IM_CBAR ('1/V/LL/0.001;0.04/0.925;0.0125/1', ier)
        END IF
C
C Overlay a (set of) suitable background map(s) and the station names.
C
	IF  ( national ) THEN
   	     CALL GG_MAP2 ('mepowo.gsf',1,1,23,iret )
	ELSE IF  ( Clickable ) THEN 
   	     CALL GG_MAP2 ('mepowo.gsf',1,1,23,iret )
   	     CALL GSTEXT (1,2,.5,1,111,1,1,ier)    !font=2,hw=on,size=.5,width=1
	     CALL GG_SPLT2 (31,0,1,0.,0, 'gempak_nids.tbl',iret)

        ELSE IF  ( thumbnail ) then
	    CALL GQMPRJ(proj,angle1,angle2,angle3,
     +	    			dlatll,dlonll,dlatur,dlonur,iret)
	    IF  ( abs( dlonll - dlonur ) .lt. 50. ) then
		CALL GG_MAP2 ('mepowo.gsf',1,1,1,iret )
            ELSE
		CALL GG_MAP2 ('loconh.gsf',1,1,1,iret )
	    END IF    
	ELSE  
	    CALL GG_MAP2 ('hicnus.nws',1,1,23,iret )
	    CALL GG_MAP2 ('hipona.nws',1,1,31,iret )
	    CALL GG_MAP2 ('loisus.nws',1,1,15,iret )
	    CALL GG_MAP2 ('lakes.ncp',1,1,31,iret )
	    CALL GSTEXT (1,2,0.857,1,111,1,1,ier)  !font=2(+bold),hw=on,size=1,width=1
	    CALL GG_SPLT2 (31,0,0,0.0,0,'cities.rad',iret) !txtcol = 31,mrkr=off,mtyp=0,msiz=0,mwid=0
	    IF  ( iret .ne. 0 )
     +		print *, "RADMAP:  GG_SPLT2 - iret = ", iret
	ENDIF	

C Add the title if needed.
	IF  ( .not. thumbnail ) THEN 
  	    CALL GSTEXT (1,2,0.857,1,111,1,1,ier) !font=2(+bold),hw=on,size=0.875,width=1
	    CALL GSCOLR ( 1, iret )
	    CALL GG_WSTR ( args(4),  1, iret )
	    CALL GG_WSTR ( args(4), -1, iret )
	ENDIF

C Close the plot.
	CALL GENANM ( iret)
	CALL GENDP ( 0, iret )
	END
