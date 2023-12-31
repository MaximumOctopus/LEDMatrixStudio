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

#ifndef FormFontViewerH
#define FormFontViewerH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "TheMatrix.h"

//---------------------------------------------------------------------------
class TfrmFontViewer : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *bSelectFont;
	TComboBox *cbFonts;
	TCheckBox *cbRGBMode;
	TPanel *Panel2;
	TLabel *lCharacterValue;
	TLabel *Label3;
	TLabel *lCharacter;
	TTrackBar *tbFont;
	TPanel *pFont;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall tbFontChange(TObject *Sender);
	void __fastcall cbFontsChange(TObject *Sender);
	void __fastcall cbRGBModeClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
private:

	void BuildFontList();
	void SetGuiLanguageText();
	void SetLabel();

	TheMatrix *FontMatrix;

	int LastFrame = -1;

public:
	__fastcall TfrmFontViewer(TComponent* Owner);
};

void OpenFontViewer();

//---------------------------------------------------------------------------
extern PACKAGE TfrmFontViewer *frmFontViewer;
//---------------------------------------------------------------------------
#endif
