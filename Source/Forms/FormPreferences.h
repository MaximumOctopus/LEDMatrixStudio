// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2024
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#ifndef FormPreferencesH
#define FormPreferencesH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
//---------------------------------------------------------------------------
class TfrmPreferences : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TImage *Image1;
	TSpeedButton *bResetToDefaults;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *gbColours;
	TLabel *lShapeOn1;
	TShape *sMono2;
	TLabel *lShapeOff;
	TShape *sMono1;
	TLabel *Label2;
	TLabel *Label1;
	TShape *sBi2;
	TLabel *Label5;
	TShape *sBi1;
	TLabel *Label8;
	TLabel *Label9;
	TShape *sBi4;
	TLabel *Label10;
	TShape *sBi3;
	TLabel *Label11;
	TLabel *Label12;
	TLabel *Label13;
	TLabel *Label14;
	TLabel *lSelector;
	TShape *ShapeSelection;
	TLabel *lLightBox;
	TShape *ShapeLightBox;
	TLabel *Label3;
	TLabel *Label4;
	TGroupBox *gbMisc;
	TSpeedButton *sbClearRecentFileList;
	TLabel *lHexFormat;
	TComboBox *cbHexFormat;
	TGroupBox *gbLimiter;
	TLabel *Label15;
	TLabel *Label17;
	TLabel *Label18;
	TEdit *eMaxPixels;
	TEdit *eExportPreview;
	TColorDialog *colorDialogPrefs;
	TGroupBox *GroupBox1;
	TCheckBox *cbDisableWarnings;
	void __fastcall sMono1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall sbClearRecentFileListClick(TObject *Sender);
	void __fastcall bResetToDefaultsClick(TObject *Sender);
private:

	void SetGUILanguageText();

public:		// User declarations
	__fastcall TfrmPreferences(TComponent* Owner);
};

//---------------------------------------------------------------------------

struct PrefsMatrixColours
{
	int Mono[2] = { 0, 0 };
	int Bi[4] = { 0, 0, 0, 0 };
	int Selection = 0;
	int LightBox = 0;
};

bool OpenPreferences(PrefsMatrixColours &);

//---------------------------------------------------------------------------
extern PACKAGE TfrmPreferences *frmPreferences;
//---------------------------------------------------------------------------
#endif
