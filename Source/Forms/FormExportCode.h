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

#ifndef FormExportCodeH
#define FormExportCodeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include "MatrixConstants.h"
#include "TheMatrix.h"

//---------------------------------------------------------------------------
class TfrmExportCode : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel2;
	TGroupBox *gbPlatforms;
	TComboBox *cbPlatforms;
	TGroupBox *gbCodeTemplates;
	TLabel *lDescription;
	TComboBox *cbCode;
	TGroupBox *gbSettings;
	TLabel *Label1;
	TLabel *lSource;
	TLabel *Label3;
	TLabel *lLSB;
	TLabel *Label5;
	TLabel *lFormat;
	TLabel *lNumbers;
	TLabel *Label8;
	TLabel *lGrouping;
	TLabel *Label10;
	TLabel *lOutput;
	TLabel *Label12;
	TLabel *lRGB;
	TLabel *Label14;
	TLabel *Label2;
	TLabel *Label4;
	TLabel *lDirection;
	TLabel *lScan;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label9;
	TLabel *Label11;
	TLabel *lMinWidth;
	TLabel *lMaxWidth;
	TLabel *lMinHeight;
	TLabel *lMaxHeight;
	TImage *iMiW;
	TImage *iMaW;
	TImage *iMiH;
	TImage *iMaH;
	TBitBtn *sbSave;
	TBitBtn *sbCopyToClipboard;
	TBitBtn *bClose;
	TMemo *Memo1;
	TSaveDialog *sdExportCode;
	void __fastcall cbPlatformsChange(TObject *Sender);
	void __fastcall cbCodeChange(TObject *Sender);
	void __fastcall sbSaveClick(TObject *Sender);
	void __fastcall sbCopyToClipboardClick(TObject *Sender);
	void __fastcall FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
          int &MaxWidth, int &MaxHeight);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:

	ExportOptions eeo;

    void SetGuiLanguageText();
	void UpdatePlatformList();
	void UpdateCodeList();
	void LoadCode();
    std::wstring GetDimensionConstraint(TLabel*, int, int, int, TImage*);
	void UpdateSettingsDisplay();

public:		// User declarations
	__fastcall TfrmExportCode(TComponent* Owner);

    TheMatrix *matrix = nullptr;
};

//---------------------------------------------------------------------------

int OpenExportCode(TheMatrix*);

//---------------------------------------------------------------------------
extern PACKAGE TfrmExportCode *frmExportCode;
//---------------------------------------------------------------------------
#endif
