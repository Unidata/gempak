#ifndef _NALARM_H_
#define _NALARM_H_

/* string.o */
XmString Str2XmString( char *string );

/* text.o */
Widget  CreateScrolledText( Widget  parent, char name[] );
Boolean FillWidgetWithFile( Widget  widget, char filename[] );

/* list.o */
void AddToList( Widget widget, char string[], int position );

/* viewfile.o */
Widget CreateViewText( Widget parent );
void FillViewText( char filename[] );

/* label.o */
Widget CreateLabelWidget( Widget parent, char name[], char message[], Arg *args, int n );
void SetLabel( Widget widget, char string[] );

#endif /* _NALARM_H_ */
