#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"



Widget NxmMenuPulldownBuild ( Widget parent, WidgetList return_item_w, 
				char *menu_title,
				KeySym menu_mnemonic,
				_NXMmenuItem *items )
/************************************************************************
 * NxmMenuPulldownBuild                                                 *
 *                                                                      *
 * This function recursively builds up a pulldown menu. Each item of    *
 * the menu is defined in the input parameter.                          *
 *                                                                      *
 * Widget NxmMenuPulldownBuild(parent, return_item_w, menu_title,       *
 *      menu_mnemonic, items)                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent         Widget       parent widget ID                        *
 *  menu_title     char*        title name of the pulldown menu         *
 *  menu_mnemonic  KeySym       mnemonic of the menu                    *
 *  *items         _NXMmenuItem definition of each menu item            *
 *                                                                      *
 * Output parameters:                                                   *
 *  return_item_w  WidgetList   widget ID of each menu item             *
 *                                                                      *
 * Return parameters:                                                   *
 *                 The widget ID of the pulldown menu                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * C. Lin/EAI       	06/96   same background color, free XmString    *
 * E. Safford/GSC	02/01	incorporate AWC Keysym changes		*
 * J. Wu/GSC		05/01	add "XmNcascadingCallback"s 		*
 * T. Piper/SAIC	02/02	fixed mnemonic processing & cleaned up	*
 * T. Piper/SAIC	03/06	Changed type for 'mnemonic' to KeySym	*
 ***********************************************************************/
{
Widget   pulldown, cascade, widget;
int      ii;
XmString xmstr;

/*---------------------------------------------------------------------*/
	pulldown = XmCreatePulldownMenu(parent, "NxmPulldown", NULL, 0);
    	xmstr = XmStringCreateLocalized(menu_title);
	cascade = XtVaCreateManagedWidget(menu_title,
    		xmCascadeButtonWidgetClass, parent,
    		XmNsubMenuId,   pulldown,
    		XmNlabelString, xmstr,
    		XmNmnemonic,   menu_mnemonic,
    		NULL);
    	XmStringFree(xmstr);

/* add menu items */

    	if ( return_item_w != NULL ) {

      	    for(ii=0; items[ii].label != NULL; ii++){
	
        	if(items[ii].subitems) 
            	    return_item_w[ii] = NxmMenuPulldownBuild
			(pulldown, items[ii].sub_buttons,
	    	    	items[ii].label, items[ii].mnemonic, items[ii].subitems);
      		else 
        	    return_item_w[ii] = XtVaCreateManagedWidget(items[ii].label,
	  			*items[ii].class, pulldown,
	  			NULL);

		if(items[ii].mnemonic)
            	    XtVaSetValues(return_item_w[ii], 
                        XmNmnemonic, items[ii].mnemonic, 
                        NULL);

      		if(items[ii].accelerator) {
		    xmstr = XmStringCreateLocalized(items[ii].accel_text);
            	    XtVaSetValues(return_item_w[ii],
	  		XmNaccelerator, items[ii].accelerator,
	  		XmNacceleratorText, xmstr, 
	  		NULL);
    		    XmStringFree(xmstr);
		}

      		if(items[ii].callback) {
            	    if ( items[ii].class == &xmCascadeButtonGadgetClass && 
		         items[ii].subitems ) {			 
		        XtAddCallback(return_item_w[ii],	  		    
	   		    XmNcascadingCallback,
	  		    (XtCallbackProc)items[ii].callback, 
	  		    (XtPointer)items[ii].which_widget);			 
		    }
		    else {	 
		        XtAddCallback(return_item_w[ii],
	  		    (items[ii].class == &xmToggleButtonGadgetClass)?
	   		    XmNvalueChangedCallback :
	   		    XmNactivateCallback,
	  		    (XtCallbackProc)items[ii].callback, 
	  		    (XtPointer)items[ii].which_widget);
                    }
		} /* end of adding callbacks */
	    } /* end of for loop */
    	}
    	else {
      	    for(ii=0; items[ii].label != NULL; ii++){
      		if(items[ii].subitems) 
        	    widget = NxmMenuPulldownBuild(pulldown, items[ii].sub_buttons,
          			items[ii].label, items[ii].mnemonic, 
				items[ii].subitems);

		else
        	    widget = XtVaCreateManagedWidget(items[ii].label,
          			*items[ii].class, pulldown,
          			NULL);

		if(items[ii].mnemonic)
        	    XtVaSetValues(widget, 
                        XmNmnemonic, items[ii].mnemonic, 
                        NULL);

      		if(items[ii].accelerator) {
		    xmstr = XmStringCreateLocalized(items[ii].accel_text);
        	    XtVaSetValues(widget,
          			XmNaccelerator, items[ii].accelerator,
          			XmNacceleratorText, xmstr, 
          			NULL);
		    XmStringFree(xmstr);
		}

      		if(items[ii].callback) {
            	    if ( ( items[ii].class == &xmCascadeButtonGadgetClass ) && 
		         items[ii].subitems ) {			 
			XtAddCallback(widget,	  		    
	   		    XmNcascadingCallback,
	  		    (XtCallbackProc)items[ii].callback, 
	  		    (XtPointer)items[ii].which_widget);
		    }
		    else {	 
           	        XtAddCallback(widget,
          			(items[ii].class == &xmToggleButtonGadgetClass)?
           			XmNvalueChangedCallback :
           			XmNactivateCallback,
          			(XtCallbackProc)items[ii].callback, 
	  			(XtPointer)items[ii].which_widget);
                    }
		} /* end of adding callbacks */
    	    } /* end of for loop */
	}
  	return cascade;
}
