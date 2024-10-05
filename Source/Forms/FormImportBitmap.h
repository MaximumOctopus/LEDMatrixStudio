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

#ifndef FormImportBitmapH
#define FormImportBitmapH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ExtDlgs.hpp>
//---------------------------------------------------------------------------

#include "MatrixConstants.h"

class TfrmImportBitmap : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TLabel *lHelpText;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *gbSettings;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *lWidth;
	TLabel *lHeight;
	TLabel *lImageHeight;
	TLabel *lImageWidth;
	TBevel *Bevel2;
	TComboBox *cbWidth;
	TComboBox *cbHeight;
	TEdit *eFrames;
	TBitBtn *bAuto;
	TCheckBox *cbCreateNew;
	TPageControl *pcImportMethod;
	TTabSheet *tsSingleImage;
	TShape *Shape1;
	TImage *iImport;
	TLabel *lFileName;
	TBitBtn *bSelect;
	TTabSheet *tsMultipleImages;
	TImage *iMultipleImages;
	TLabel *Label5;
	TLabel *Label7;
	TSpeedButton *sbMISelectFirstImage;
	TLabel *Label8;
	TLabel *Label9;
	TLabel *Label10;
	TLabel *lImageLengthExample;
	TEdit *eMIFirstImage;
	TEdit *eMIPattern;
	TEdit *eMIFirstFrame;
	TEdit *eMIPadLength;
	TOpenPictureDialog *opdMain;
	TComboBox *cbImportColourMode;
	void __fastcall sbMISelectFirstImageClick(TObject *Sender);
	void __fastcall bOKClick(TObject *Sender);
	void __fastcall bCancelClick(TObject *Sender);
	void __fastcall bSelectClick(TObject *Sender);
	void __fastcall bAutoClick(TObject *Sender);
	void __fastcall cbImportColourModeChange(TObject *Sender);
private:

	void SetGuiLanguageText();
	void SetMultipleImageDetails();

public:		// User declarations
	__fastcall TfrmImportBitmap(TComponent* Owner);

	ImportMode Import = ImportMode::kInvalid;

	ImportColourMode ImportMode = ImportColourMode::kMono;
	bool CreateNew = false;
	std::wstring ImageFilename = L"";
	int FrameCount = -1;
	int FrameWidth = -1;
	int FrameHeight = -1;

	int FirstFrame = -1;
	int PadLength = -1;
	std::wstring Pattern = L"";

};
//---------------------------------------------------------------------------
extern PACKAGE TfrmImportBitmap *frmImportBitmap;
//---------------------------------------------------------------------------
#endif
