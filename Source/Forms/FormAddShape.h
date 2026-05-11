//---------------------------------------------------------------------------

#ifndef FormAddShapeH
#define FormAddShapeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Dialogs.hpp>
//---------------------------------------------------------------------------
class TfrmAddShape : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *gbLimiter;
	TLabel *lPixels;
	TLabel *lSizeX;
	TEdit *ePixels;
	TEdit *ePositionX;
	TEdit *ePositionY;
	TEdit *eSizeX;
	TLabel *Label1;
	TLabel *Label2;
	TShape *shapeColour;
	TLabel *lColour;
	TColorDialog *cdAddShape;
	TLabel *lSizeY;
	TEdit *eSizeY;
	TComboBox *cbShape;
	TComboBox *cbInitialDirection;
	void __fastcall cbShapeChange(TObject *Sender);
	void __fastcall bOKClick(TObject *Sender);
	void __fastcall shapeColourMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
private:

	void SetGuiLanguageText();

public:
	__fastcall TfrmAddShape(TComponent* Owner);

	int SelectedShape = 0;
    int SelectedDirection = 0;
	int SelectedSizeX = 0;
	int SelectedSizeY = 0;
	int SelectedPixels = 0;
	int SelectedX = 0;
	int SelectedY = 0;
    int SelectedColour = 0;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAddShape *frmAddShape;
//---------------------------------------------------------------------------
#endif
