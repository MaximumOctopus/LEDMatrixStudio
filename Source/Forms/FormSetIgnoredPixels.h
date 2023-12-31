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

#ifndef FormSetIgnoredPixelsH
#define FormSetIgnoredPixelsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include "MatrixConstants.h"

//---------------------------------------------------------------------------
class TfrmSetIgnoredPixels : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TBevel *Bevel1;
	TGroupBox *GroupBox1;
	TLabel *Label11;
	TLabel *Label1;
	TLabel *Label2;
	TComboBox *cbCustomShape;
	TComboBox *cbCustomShapeParam;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	void __fastcall cbCustomShapeChange(TObject *Sender);
private:

	void SetGuiLanguageText();

public:
	__fastcall TfrmSetIgnoredPixels(TComponent* Owner);

	int MatrixWidth = 0;
    int MatrixHeight = 0;
};

struct SetIgnoredPixels
{
	bool Process = false;
	CustomShape Shape = CustomShape::kNone;
	int Parameter = 0;

	void SetShapeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Shape = CustomShape::kNone;
			break;
		case 1:
			Shape = CustomShape::kCircle;
			break;
		case 2:
			Shape = CustomShape::kBorders;
			break;
		case 3:
			Shape = CustomShape::kTriangle;
			break;

		default:
			Shape = CustomShape::kNone;
		}
	}
};

SetIgnoredPixels OpenIgnoredPixels(int, int);

//---------------------------------------------------------------------------
extern PACKAGE TfrmSetIgnoredPixels *frmSetIgnoredPixels;
//---------------------------------------------------------------------------
#endif
