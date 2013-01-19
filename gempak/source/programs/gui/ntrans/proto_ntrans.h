/************************************************************************
 * proto_ntrans.h							*
 *									*
 * This file contains header files and global variables for use in the	*
 * NTRANS routines.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   					*
 * T. Piper/SAIC	05/03	Removed NxmGmpk.c functions.		*
 * T. Piper/SAIC	 1/04	Removed NxmCheckEnv			*
 * C. Bailey/HPC	 5/05	Added setValidTime, clearValidTime,	*
 *				createMpFrame and npf functions		*
 ************************************************************************/

#ifndef PROTO_NTRANS
#define	PROTO_NTRANS

/*
 *  NTRANS prototypes
 */

void 	add_framelist ( char	**title );
void 	AddGpList_Callback	( Widget, long, XtPointer );
void 	add_grouplist ( void );
void 	add_IncNo ( void );
void    alloc_framebuffer ( void );
void 	autogroup ( void );
void 	byte_flip ( unsigned short *temp );
void 	Cancel_Select_Callback	( Widget, XtPointer, XtPointer );
void	ClearAreas		( Widget, XtPointer, XtPointer );
void	clearValidTime ( void );
void 	clear_window ( void );
void 	Close_Meta ( void );
void	create_drawingW ( Widget  parent );
void	create_file_selection ( Widget	parent );
Widget 	create_framelist( Widget parent, int selectpolicy ); 
Widget 	create_grouplist ( Widget  parent );
void 	create_legend ( Widget parent );
void	create_local_products ( Widget parent ); 
void	create_main ( Widget parent );
void 	create_mangroup ( Widget parent );
void 	create_model_menu ( Widget parent );
void	create_models (	Widget parent );
void 	create_model_selection ( Widget parent );
void	createMpFrame ( void );
void 	create_multipanel ( Widget parent );
void 	create_selectframe ( Widget parent );
void 	create_selectgroup ( Widget parent );
void 	create_std_buttons ( Widget parent, XtCallbackProc callback );
void 	create_top_menubar ( Widget parent );
void 	do_color_tab ( void );
void 	do_line ( int   len );
void 	do_polyfill ( int   len );
void 	do_lnwdth ( int	  lnwdth );
void 	do_circle ( void );
void 	do_line_color ( int  indx );
void 	do_fill_color ( int  indx );
void 	do_fill_patrn ( int  indx );
void 	do_text ( int   len );
void 	do_text_set ( void );
void 	toggle_menuWindow ( void );
void 	display_pixmap ( void );
void 	displayPixmap ( void );
void 	Select_File_Callback	( Widget, XtPointer, XmFileSelectionBoxCallbackStruct* );
void	load_meta ( void );
void	initialize ( void );
void 	Save_Popup_Callback	( Widget, long, XtPointer );

void	save_group ( void );
char	*strccpy ( char *s, char *t, char c );
void 	load_model ( Widget  widget, 
                     char    filename[] );
void 	load_group ( int  groupno );
void	setView ( int	row, 
		  int   col );
void	defaultView ( void );
void	loadBeginNotify ( void );
void	loadEndNotify ( void );
void 	reload_group ( void );
void	graf_init ( void );
void 	display_legend ( int	type );
void 	display_legend_message ( int    type, 
				 char   *message );
void 	LocalProduct_Callback	( Widget, long, XmFileSelectionBoxCallbackStruct* );
void 	Search_Callback		( Widget, XtPointer, XtPointer );
void 	delete_framelist ( void );
int 	get_text ( Widget widget, char *text );

void    ntrans_print ( void );
int 	nFrame ( void );
int	loadAllPage ( void );
void 	frame_select		( Widget, long, XtPointer );
void 	load_frame ( int  frameno );
void 	ok_select ( Widget  widget );
void 	delete_grouplist ( void );
FILE	*npf_create (	char	*filnam, 
			int	*hlen,
			int	*iret );
void	npf_open ( 	char 	*filnam, 
			Boolean	crt, 
			FILE 	**fptr, 
			long 	*flen, 
			int 	*iret );
void 	npf_load ( 	char 	*filnam, 
			int 	*iret );
void	npfw_create (	Widget	);
void	npfw_popup (	int	func );
int 	ScanFile ( char *meta_file );
void	setMode0 ( void );
void	setMode1 ( void );
void	setPanelSrc ( 	int	which );
void    setValidTime ( void );
void	Trans_Frame (	int frame_no );
void    update   ( void );
void	update_modellist ( void );
void 	view_frame ( void );

#endif	/* PROTO_NTRANS */
