#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

#include "nmapprm.h"
#include "nmap_data.h"

#ifdef UNDERSCORE
#define imrad2	imrad2_
#endif

/*
 * Private Callback functions
 */
void image_props_ctlBtnCb (Widget wid, int which, XtPointer cbs);
void _lev2_set_parms ();

Widget image_props = NULL;
Widget _tiltW = NULL, _scrollW;

#define RAD_TYPES 3
char *radprmlist[] = { "dz", "vr", "sw" };

int NINDEX = 0;


void
image_rad2set ()
{
  int npos, ier;
/* these sizes must be at least size expected in fortran imrad2() */
  char tilt[32], radprm[32];	/* space padded arrays sent to imrad2 */
  char *cstr;
  XmStringTable item_list;

  /* with SINGLE_SELECT, npos should be 1, and 1 only */
  XtVaGetValues (_scrollW, XmNselectedItemCount, &npos,
		 XmNselectedItems, &item_list, NULL);
  if (npos < 1)
    {
      printf ("no items were selected\n");
      return;
    }

  XmStringGetLtoR (item_list[0], XmFONTLIST_DEFAULT_TAG, &cstr);
  cst_padString (cstr, ' ', 1, 32, radprm);
  XtFree (cstr);



  cstr = XmTextGetString (_tiltW);
  if (cstr != NULL)
    {
      cst_padString (cstr, ' ', 1, 32, tilt);
      XtFree (cstr);
    }

  (void) imrad2 (tilt, radprm, &ier, sizeof (tilt), sizeof (radprm));
}



void
image_props_create (Widget parent, int index)
{
  char tmpstr[LLMXLN];
  int toff = 10;

  Widget pane, form_lev2;
  Widget _tiltlabelW, _paramlabelW;
  XmString xmstr;

  char *btnstr[] = { "OK", "Cancel" };

  NINDEX = index;

  if (!image_props)
    {

      image_props =
	XmCreateFormDialog (XtParent (parent), "image_props_popup", NULL, 0);
      XtVaSetValues (image_props, XmNnoResize, True, XmNdefaultPosition,
		     False, NULL);

/*
 *      Set the window title.
 */
      /*sprintf ( tmpstr, "%s Attributes", _alias ); */
      sprintf (tmpstr, "Image Attributes");
      XtVaSetValues (XtParent (image_props), XmNtitle, tmpstr, NULL);

/*
 *      Create a parent window pane.
 */
      pane = XtVaCreateWidget ("image_props_pane",
			       xmPanedWindowWidgetClass, image_props,
			       XmNsashWidth, 1, XmNsashHeight, 1, NULL);

/*
 *	Create a form container for level2 attributes.
 */
      form_lev2 = XtVaCreateWidget ("form_lev2",
				    xmFormWidgetClass, pane,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNtopAttachment, XmATTACH_FORM, NULL);

      xmstr = XmStringCreateLocalized ("LevelII radar tilt angle");
      _tiltlabelW = XtVaCreateManagedWidget ("tilt_label",
					     xmLabelWidgetClass, form_lev2,
					     XmNlabelString, xmstr,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNtopOffset, toff, NULL);
      XmStringFree (xmstr);

      _tiltW = XtVaCreateManagedWidget ("tilt_value",
					xmTextWidgetClass, form_lev2,
					XmNtopAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget, _tiltlabelW,
					XmNrightAttachment, XmATTACH_FORM,
					XmNwidth, 250, XmNvalue, "0.", NULL);

      xmstr = XmStringCreateLocalized ("LevelII parameter");
      _paramlabelW = XtVaCreateManagedWidget ("param_label",
					      xmLabelWidgetClass, form_lev2,
					      XmNlabelString, xmstr,
					      XmNleftAttachment,
					      XmATTACH_FORM, XmNtopAttachment,
					      XmATTACH_WIDGET,
					      XmNtopWidget, _tiltlabelW,
					      XmNtopOffset, toff, NULL);
      XmStringFree (xmstr);

      _scrollW = XmCreateScrolledList (form_lev2, "params", NULL, 0);
      XtVaSetValues (_scrollW, XmNvisibleItemCount, 5,
		     XmNselectionPolicy, XmSINGLE_SELECT, NULL);
      XtVaSetValues (XtParent (_scrollW),
		     XmNtopAttachment, XmATTACH_WIDGET,
		     XmNtopWidget, _tiltW,
		     XmNrightAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
		     XmNleftWidget, _tiltW, NULL);
      {
	XmStringTable str_list;
	int np;
	str_list = (XmStringTable) XtMalloc ((size_t) RAD_TYPES *
					     sizeof (XmString *));
	for (np = 0; np < RAD_TYPES; np++)
	  str_list[np] = XmStringCreateLocalized (radprmlist[np]);
	XtVaSetValues (_scrollW, XmNitemCount, RAD_TYPES, XmNitems, str_list,
		       NULL);

	for (np = 0; np < RAD_TYPES; np++)
	  XmStringFree ((XmString) str_list[np]);
	XtFree ((XtPointer) str_list);
	XmListSelectPos (_scrollW, 1, FALSE);
      }

      XtManageChild (_scrollW);


      XtManageChild (form_lev2);

/*
 *      Create the OK and Cancel control buttons.
 */
      NxmCtlBtn_create (pane, 1, "image_props_ctlBtn", XtNumber (btnstr),
			btnstr, (XtCallbackProc) image_props_ctlBtnCb, NULL);

      XtManageChild (pane);

/*
 * Set initial values for tilt and parameter.
 */
      _lev2_set_parms ();

    }

}

void
image_props_popup (Widget parent, int index)
{
  if (!image_props)
    image_props_create (parent, index);

  if (image_props == NULL)
    return;

  if (!XtIsManaged (image_props))
    XtManageChild (image_props);

}

void
image_props_popdown ()
{

  if (image_props != NULL)
    if (XtIsManaged (image_props))
      {
	XtUnmanageChild (image_props);
      }
}

int
image_set_props (Widget parent, int index)
{
  int loop, ier, bsiz, i, icnt;
  char imlutf[MXFLSZ], imtype[MXFLSZ], iminfo[MXFLSZ], subcat[MXFLSZ];
  char buf[256], *cpos, bsub[256], defparm[256];
  FILE *fp;
  int found = G_FALSE, done;
  static int oldfound = G_FALSE;

  loop = loop_getCurLoop ();
  if (!loop_getDataChngd (loop))
    return (oldfound);


  nim_qatt (index, imtype, iminfo, imlutf, &ier);
  if ((cpos = strrchr (iminfo, '/')) != NULL)
    {
      strncpy (subcat, iminfo, (size_t) (cpos - iminfo));
      subcat[cpos - iminfo] = '\0';
    }
  else
    strcpy (subcat, iminfo);

  fp = cfl_tbop ("imgset.tbl", "config", &ier);
  bsiz = 256;
  defparm[0] = '\0';
  while ((ier == 0) && (!found))
    {
      cfl_trln (fp, bsiz, buf, &ier);
      if (ier == 0)
	{
	  if ((strncmp (buf, "IMTYPE", 6) == 0) &&
	      (strcmp (&buf[7], imtype) == 0))
	    {
	      cfl_trln (fp, bsiz, buf, &ier);
	      if (ier != 0)
		continue;
	      if ((strncmp (buf, "IMINFO", 6) == 0) &&
		  (strcmp (&buf[7], subcat) == 0))
		{
		  found = G_TRUE;
		  if (!image_props)
		    image_props_create (parent, index);

		  done = G_FALSE;
		  while ((!done) && (image_props))
		    {
		      cfl_trln (fp, bsiz, buf, &ier);
		      if (ier == 0)
			{
			  if (strncmp (buf, "default", 7) == 0)
			    {
			      cpos =
				cst_split (&buf[8], ',', sizeof (defparm),
					   defparm, &ier);
			      if (cpos != NULL)
				cpos =
				  cst_split (cpos, ',', sizeof (bsub), bsub,
					     &ier);
			      if ((ier == 0) && (_tiltW == NULL))
				XmTextSetString (_tiltW, bsub);
			      image_rad2set ();
			    }
			  else if (strncmp (buf, "parameter", 9) == 0)
			    {
			      int np, defpos = 0;
			      char *cpos;
			      XmStringTable str_list;
			      /*printf("set parameter %s\n",buf+10); */
			      XmListDeleteAllItems (_scrollW);
			      icnt = 1;
			      for (i = 10; i < strlen (buf); i++)
				if (buf[i] == ',')
				  icnt++;
			      str_list =
				(XmStringTable) XtMalloc ((size_t) icnt *
							  sizeof (XmString
								  *));
			      cpos = buf + 10;
			      for (np = 0; np < icnt; np++)
				{
				  cpos =
				    cst_split (cpos, ',', sizeof (bsub), bsub,
					       &ier);
				  str_list[np] =
				    XmStringCreateLocalized (bsub);
				  if (strcmp (bsub, defparm) == 0)
				    defpos = np;
				}
			      XtVaSetValues (_scrollW, XmNitemCount, icnt,
					     XmNitems, str_list, NULL);

			      for (np = 0; np < icnt; np++)
				XmStringFree ((XmString) str_list[np]);
			      XtFree ((XtPointer) str_list);
			      XmListSelectPos (_scrollW, defpos + 1, FALSE);
			    }
			  else
			    done = G_TRUE;
			}
		      else
			done = G_TRUE;
		    }
		}
	    }
	}
    }

  cfl_clos (fp, &ier);

  oldfound = found;
  return (found);
}

void
_lev2_set_parms ()
{
  int ier, lens, dummyIdx, loop;

  char imlutf[MXFLSZ], imtype[MXFLSZ], iminfo[MXFLSZ];

  image_rad2set ();

  loop = loop_getCurLoop ();

  dataw_setImageNav ();

  nim_qatt (NINDEX, imtype, iminfo, imlutf, &ier);

  im_qlut (imlutf, &ier, sizeof (imlutf));
  if (ier != 0)
    strcpy (imlutf, "DEFAULT");
  else
    st_null ( imlutf, imlutf, &lens, &ier, sizeof(imlutf), sizeof(imlutf));
  

  /* reset the LUTF to the default associated with this product */
  nim_satt (NINDEX, imtype, iminfo, imlutf, &dummyIdx, &ier);

  loop_restoreLut (loop);
  loop_setDataChngd (loop, TRUE);
}

void
image_props_ctlBtnCb (Widget wid, int which, XtPointer cbs)
{

  switch (which)
    {
      /* OK button: set level2 attributes. */
    case 0:
      _lev2_set_parms ();
      break;

      /* Cancel button: Do nothing. */
    case 1:
      break;
    }

  image_props_popdown ();
}
