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

#ifndef FormNewBrushH
#define FormNewBrushH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "MatrixConstants.h"
#include "RGBPaletteColours.h"
#include "TheMatrix.h"
#include <Vcl.Graphics.hpp>

//---------------------------------------------------------------------------

struct MatrixSettings
{
	int Width = 0;
	int Height = 0;

	MatrixMode Mode = MatrixMode::kNone;
};

struct NewBrush
{
	bool Proceed = false;

	int Width = 0;
	int Height = 0;
};


//---------------------------------------------------------------------------
class TfrmNewBrush : public TForm
{
__published:	// IDE-managed Components
	TPanel *pMain;
	TBitBtn *sbSave;
	TButton *bSaveBrush;
	TButton *bLoadBrush;
	TPanel *pColours;
	TLabel *lColours;
	TColorListBox *clbMain;
	TBitBtn *bAddColour;
	TBitBtn *bDeleteColour;
	TBitBtn *bOpenColours;
	TBitBtn *bSaveColours;
	TBitBtn *bColourUp;
	TBitBtn *bColourDown;
	TPanel *pRGBPalette;
	TShape *sRGBPaletteColour;
	TShape *sRGBP1;
	TShape *sRGBP2;
	TShape *sRGBP3;
	TShape *sRGBP4;
	TShape *sRGBP5;
	TShape *sRGBP6;
	TShape *sRGBP7;
	TShape *sRGBP8;
	TShape *sRGBP9;
	TShape *sRGBP10;
	TShape *sRGBP11;
	TShape *sRGBP12;
	TShape *sRGBP13;
	TShape *sRGBP14;
	TShape *sRGBP15;
	TShape *sRGBP16;
	TShape *sRGBP17;
	TShape *sRGBP18;
	TShape *sRGBP19;
	TShape *sRGBP20;
	TShape *sRGBP21;
	TLabel *lPaletteColourText;
	TLabel *lPixelColour;
	TShape *sSelectionLMB;
	TShape *sSelectionRMB;
	TShape *sSelectionMMB;
	TBevel *Bevel1;
	TTrackBar *tbRed;
	TEdit *eRed;
	TEdit *eGreen;
	TTrackBar *tbGreen;
	TEdit *eBlue;
	TTrackBar *tbBlue;
	TPanel *pCanvas;
	TColorDialog *cdNewBrush;
	TSaveDialog *sdSaveBrush;
	TOpenDialog *odLoadBrush;
	TImage *Image1;
	TImage *iColoursMiddle;
	TImage *iColoursRight;
	TLabel *lBrush;
	TComboBox *cbAvailableTypes;
	TLabel *Label1;
	TButton *bCreate;
	TBitBtn *sbCancel;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
          int &MaxWidth, int &MaxHeight);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall bAddColourClick(TObject *Sender);
	void __fastcall bColourDownClick(TObject *Sender);
	void __fastcall bColourUpClick(TObject *Sender);
	void __fastcall bDeleteColourClick(TObject *Sender);
	void __fastcall bOpenColoursClick(TObject *Sender);
	void __fastcall bSaveColoursClick(TObject *Sender);
	void __fastcall bLoadBrushClick(TObject *Sender);
	void __fastcall bSaveBrushClick(TObject *Sender);
	void __fastcall bCreateClick(TObject *Sender);
	void __fastcall eRedKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall sRGBPaletteColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
	void __fastcall sRGBPaletteColourMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall tbRedChange(TObject *Sender);
	void __fastcall sSelectionLMBMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations

	static const int CTGradientHorizontalUp      = 0;
	static const int CTGradientHorizontalDown    = 1;
	static const int CTGradientVerticalRight     = 2;
	static const int CTGradientVerticalLeft      = 3;
	static const int CTGradientDiagonalUpRight   = 4;
	static const int CTGradientDiagonalDownRight = 5;
	static const int CTChevronUp                 = 6;
	static const int CTChevronDown               = 7;
	static const int CTChevronRight              = 8;
	static const int CTChevronLeft               = 9;
	static const int CTCheckerboard1x1           = 10;
	static const int CTCheckerboard2x2           = 11;
	static const int CTCheckerboard3x3           = 12;
	static const int CTCheckerboard4x4           = 13;

	int RGBPaletteHistoryIndex = 0;

	void SetGuiLanguageText();

	void SetCaption(const std::wstring);

	void GradientHorizontal(int);
	void GradientVertical(int);
	void GradientDiagonal(int);
	void GenerateChevron(int);
	void GenerateCheckerboard(int);

	void SaveColours(const std::wstring);
	void LoadColours(const std::wstring);

	void AddToPaletteHistory(int);

	LoadData LoadDataParameterType(const std::wstring, bool, bool, bool);

	void LoadBrush(const std::wstring);
	void SaveBrush(const std::wstring);

public:		// User declarations
	__fastcall TfrmNewBrush(TComponent* Owner);

	TheMatrix *MatrixAutomate;

	MatrixSettings Settings;
	TShape *_RGBPaletteHistory[21];
};

NewBrush OpenNewBrush(std::vector<std::wstring> &, MatrixSettings, RGBPaletteColours);

//---------------------------------------------------------------------------
extern PACKAGE TfrmNewBrush *frmNewBrush;
//---------------------------------------------------------------------------
#endif
