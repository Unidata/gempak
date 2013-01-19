#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

#define RESOURCE_FILE "Nalarm"

/*
 *  Private functions
 */
static Boolean 	notify_me ( Widget w );
Boolean check_sound ( char *dname );
static void check_alarm_product ( Widget w, XtIntervalId *id );
void get_from_child ( Widget w, int *fid, XtInputId *id );
void update_last_check ( void );
void get_busy ( Widget, XtPointer, XtPointer );
void setup_dir ( void );
void init_last_check ( void );
void exitCb ( Widget, XtPointer, XtPointer );
#ifdef Linux
void _quitOK_Cb ( Widget, XtPointer, XtPointer ) __attribute__((noreturn));
#else
void _quitOK_Cb ( Widget, XtPointer, XtPointer );
#endif

/************************************************************************
 * nalarm.c								*
 *									*
 * This program displays a flashing button when a new product has	*
 * arrived in the users alarm directory.				*
 *									*
 * CONTENTS:								*
 *									*
 *   notify_me()							*
 *   check_sound()							*
 *   check_alarm_product()						*
 *   get_from_child()							*
 *   update_last_check()						*
 *   get_busy()								*
 *   setup_dir()							*
 *   init_last_check()							*
 *   exitCb()								*
 *   _quitOK_Cb()							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/96	Copied from AFOS_ALARM			*
 * S. Jacobs/NCEP	12/96	Added Exit button; Cleaned up		*
 * G. Krueger/EAI	 9/97	NxmExitDialog->NxmExit_create		*
 * G. Krueger/EAI	10/97	Add NxmVers_showTitle & NxmRes_check;	*
 *				Improve header				*
 * S. Jacobs/NCEP	11/97	Fixed check for "terminated" return	*
 *				from child process			*
 * S. Jacobs/NCEP	 3/99	Changed get_from_child to void function	*
 * H. Zeng/EAI           8/99   Make "Exit" button insensitive when     *
 *                              "New Product" button is clicked         *
 * E. Safford/GSC	02/01	update param list on NxmExit_create	*
 * S. Jacobs/NCEP	 3/04	Fixed flashing of button; Use cfl_inqr	*
 * 				to find the naltxt executable		*
* T. Piper/SAIC		11/07	Changed GEMEXE to OS_BIN		*
 ***********************************************************************/

#define	 NUM_SOUND	15

static XtIntervalId	interval_id = (XtIntervalId)NULL;
static XtInputId	input_id = (XtInputId)NULL;
unsigned long 		start_time = 0;
unsigned long		interval = 1000;
static Pixel		init_fg, init_bg;
Widget         		button;
Widget         		exit_button;
Widget         		form;
static time_t		last_check;
pid_t			pid;
int			tp_pipe[2]; 
char			*username;
char			alarm_dir[128];
Boolean			need_sound;
char 			*home_dir;
char 			check_file[128];
static unsigned int 	sound_count = NUM_SOUND;

/*=====================================================================*/

static Boolean notify_me ( Widget wid )
/************************************************************************
 * notify_me								*
 ***********************************************************************/
{

  static unsigned long elapsed_time, last_time = 0;
  static Boolean flag = False;
  struct timeval current_tv;

/*---------------------------------------------------------------------*/

    gettimeofday(&current_tv, (struct timezone *)0);

    elapsed_time = current_tv.tv_sec * 1000000 +
		 (unsigned long)current_tv.tv_usec - start_time;

    if ( (elapsed_time - last_time) < (unsigned long)400000 )
 	return(False);

    last_time = elapsed_time;

    if (flag) {
	if ( need_sound ) {
	    if (sound_count) {
  		XBell(XtDisplay(wid), 100);
		--sound_count;
	    }
	    else {
		need_sound = False;
	    }
	}
	XtVaSetValues(wid, XmNforeground, init_fg,
  			XmNbackground, init_bg,
  			NULL); 
    }
    else {
 	XtVaSetValues(wid, XmNforeground, init_bg,
			XmNbackground, init_fg,
			NULL); 
    }

  flag = !flag;

  return(False);

}

/*=====================================================================*/

Boolean check_sound ( char *dname )
/************************************************************************
 * check_sound								*
 ***********************************************************************/
{
	char *ptr, last[10]="\0";

/*----------------------------------------------------------------------*/

	if (strtok(dname,".") == NULL)
		return (False);
	while ((ptr = strtok(NULL, ".")) != NULL ) 
		strcpy(last, ptr);

	if (strcmp(last, "NTF") == 0) 
		return (True);
	else	return (False);

}

/*=====================================================================*/
/* ARGSUSED */
static void check_alarm_product ( Widget w, XtIntervalId *id )
/************************************************************************
 * check_alarm_product							*
 ***********************************************************************/
{

  char  fullpath[100];
  char  newfile[100];
  struct stat statbuf;
  struct dirent *dirp;
  DIR	*dp;
  char *ptr;
  int file_exist;

/*-----------------------------------------------------------------------*/

  if ( (dp = opendir(alarm_dir)) == NULL) {
  	fprintf(stderr, "Can't read directory %s\n", alarm_dir);
  	exit(1);
  	}

  strcpy(fullpath, alarm_dir);
  ptr = fullpath + strlen(fullpath);
  *ptr++ = '/';
  *ptr = 0;

  file_exist = 0;
  while ( (dirp = readdir(dp)) != NULL ) {
	if ((strcmp(dirp->d_name, ".") == 0) ||
	    (strcmp(dirp->d_name, "..") == 0) )
	  continue;

	strcpy(ptr, dirp->d_name);
	if (lstat(fullpath, &statbuf) < 0) {
	  fprintf(stderr, "stat error for %s\n", fullpath);
	  exit(1);
	  }

	if (S_ISREG(statbuf.st_mode) && (statbuf.st_mtime > last_check)){
	  file_exist = 1;
	  if ( (need_sound = check_sound(dirp->d_name) )) { 
		if (sound_count <= 0) {
			strcpy(newfile, fullpath);
  			*(newfile + strlen(newfile) - 4) = '\0';
			rename(fullpath, newfile);
			sound_count = NUM_SOUND;
			}

		break;
	   } /* end of if (need_sound ... */
	} /* end of if (S_ISREG ... */
  } /* end of while */

  closedir(dp);

  if  ( file_exist )  notify_me ( w );

  interval_id = XtAppAddTimeOut(XtWidgetToApplicationContext(w), 
			interval, 
			(XtTimerCallbackProc)check_alarm_product, 
			w); 

}

/*=====================================================================*/
/* ARGSUSED */
void get_from_child ( Widget wid, int *fid, XtInputId *id )
/************************************************************************
 * get_from_child							*
 ***********************************************************************/
{ 

    char buf[11];

/*---------------------------------------------------------------------*/

    read ( *fid, buf, strlen("terminated")+1 );
    if (strncmp("terminated", buf, 10) == 0) {
	close(*fid); 
	XtSetSensitive(wid, True);
        XtSetSensitive(exit_button, True);
  	if (interval_id)  XtRemoveTimeOut(interval_id);
  	interval_id = XtAppAddTimeOut(
		XtWidgetToApplicationContext(wid), 
		interval, 
		(XtTimerCallbackProc)check_alarm_product, wid);
    }

    sound_count = NUM_SOUND;
    XtRemoveInput(input_id);

} 

/*=====================================================================*/

void update_last_check ( void )
/************************************************************************
 * update_last_check							*
 ***********************************************************************/
{ 

  FILE	*fp;
  char junk[] = "";

/*---------------------------------------------------------------------*/

  fp = fopen(check_file, "w");
  fwrite(junk, 1, sizeof(junk), fp);
  fclose(fp);
  last_check = time(NULL);

} 

/*=====================================================================*/
/* ARGSUSED */
void get_busy ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * get_busy								*
 ***********************************************************************/
{
  char        	system_command[512];
  struct stat 	statbuf;
  char  	fullpath[128];
  struct dirent *dirp;
  DIR		*dp;
  char 		*ptr;
  int 		file_exist;
  char  	newfile[100];
  char		alarm_program[256];
  long		flen;
  int		ierr;

/*---------------------------------------------------------------------*/

  if ( (dp = opendir(alarm_dir)) == NULL) {
  	fprintf(stderr, "Can't read directory %s\n", alarm_dir);
  	exit(1);
  	}

  strcpy(fullpath, alarm_dir);
  ptr = fullpath + strlen(fullpath);
  *ptr++ = '/';
  *ptr = 0;

  file_exist = 0;
  while ( (dirp = readdir(dp)) != NULL ) {
	if ((strcmp(dirp->d_name, ".") == 0) ||
	    (strcmp(dirp->d_name, "..") == 0) )
	  continue;

	strcpy(ptr, dirp->d_name);
	if (lstat(fullpath, &statbuf) < 0) {
	  fprintf(stderr, "stat error for %s\n", fullpath);
	  exit(1);
	  }

	if (S_ISREG(statbuf.st_mode)) {
	  file_exist = 1;
	  if ( (need_sound = check_sound(dirp->d_name)) ) {
		strcpy(newfile, fullpath);
		*(newfile + strlen(newfile) - 4) = '\0';
		rename(fullpath, newfile);
		}
	  }
	} /* end of while */

  closedir(dp);

    if (file_exist) {
	pipe(tp_pipe);
	if ( (pid = fork()) == 0) { /* first child */
	    if ( (pid = fork()) > 0 )
		exit(0); /* second parent == first child */

/* entering grand child */

	    close(tp_pipe[0]); /* close read end of child */

	    cfl_inqr ( "naltxt", "$OS_BIN",
		       &flen, alarm_program, &ierr );

	    sprintf(system_command, 
        	"%s %s  >> /dev/null 2>> /dev/null",
		alarm_program, alarm_dir);

	    system(system_command);

	    write(tp_pipe[1], "terminated", strlen("terminated"));
	    close(tp_pipe[1]);  

	    exit(0);
	} /* end of child */

	else { /* parent */

	    waitpid( pid, NULL, 0); /* wait for first child */

	    close(tp_pipe[1]);

  	    if (interval_id) 
		XtRemoveTimeOut(interval_id);

	    update_last_check();

 	    XtVaSetValues(button, XmNforeground, init_fg,
  	   		XmNbackground, init_bg,
  	   		NULL); 
	    XtSetSensitive(w, False);
            XtSetSensitive(exit_button, False);

	    input_id = XtAppAddInput(
			XtWidgetToApplicationContext(button),
			tp_pipe[0], 
			(XtPointer)XtInputReadMask,
			(XtInputCallbackProc)get_from_child, 
			button);

	} /* end of parent */

    } /* end of if (file_exist) ... */

    return;
}

/*=====================================================================*/

void setup_dir ( void )
/************************************************************************
 * setup_dir								*
 ***********************************************************************/
{ 
  struct stat buf;

/*---------------------------------------------------------------------*/

  username = getenv("LOGNAME"); 
  strcpy(alarm_dir, getenv("AFOS_ALARMS"));
  strcat(alarm_dir, "/");
  strcat(alarm_dir, username);
  if (lstat(alarm_dir, &buf) < 0) {
  	strcpy(alarm_dir, getenv("AFOS_ALARMS"));
 	strcat(alarm_dir, "/");
  	strcat(alarm_dir, "nawips");
	}
}

/*=====================================================================*/

void init_last_check ( void )
/************************************************************************
 * init_last_check							*
 ***********************************************************************/
{ 

    FILE	*fp;
    char junk[] = "";
    struct stat	statbuf;

/*--------------------------------------------------------------------*/

    home_dir = getenv("HOME");
    strcpy(check_file, home_dir);
    strcat(check_file, "/.alarm_text_check");

    if( stat(check_file, &statbuf) < 0) {
	fp = fopen(check_file, "w");
	fwrite(junk, 1, sizeof(junk), fp);
	fclose(fp);
    }
    else {
    	last_check = statbuf.st_atime;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void exitCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * exitCb								*
 ***********************************************************************/
{
    NxmExit_create ( wid, "Exit Confirmation",
		 "OK to EXIT from nalarm?",
		 _quitOK_Cb, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void _quitOK_Cb ( Widget wid, XtPointer clnt, XtPointer call )
{
    XtCloseDisplay(XtDisplay(wid));
    exit(0);
}

/*=====================================================================*/

int main ( int argc, char *argv[] )
{
  XtAppContext   app;
  Widget  	 toplevel;

/*---------------------------------------------------------------------*/

  toplevel = XtVaAppInitialize(&app, RESOURCE_FILE,
    		NULL, 0, &argc, argv, NULL, NULL);

/*
 * check resource file
 */
  NxmRes_check(XtDisplay(toplevel), RESOURCE_FILE, NULL);

/*
 * display version in title string
 */
  NxmVers_showTitle(toplevel);

  form = XtCreateManagedWidget("form",
				xmFormWidgetClass, toplevel,
				NULL, 0);

  setup_dir();
  init_last_check();

  button = XtVaCreateManagedWidget("button",
    				xmPushButtonWidgetClass, form,
     				NULL);

  XtVaGetValues(button, XmNforeground, &init_fg,
  		XmNbackground, &init_bg,
  		NULL);
  XtAddCallback( button, XmNactivateCallback,
		(XtCallbackProc)get_busy, NULL);

  exit_button = XtVaCreateManagedWidget("Exit",
    				xmPushButtonWidgetClass, form,
     				NULL);
  XtAddCallback(exit_button, XmNactivateCallback, exitCb, NULL);

  check_alarm_product(button, 0);

  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);

  return(0);
}
