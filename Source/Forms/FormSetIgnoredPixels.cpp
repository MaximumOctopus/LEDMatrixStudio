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

#include <vcl.h>
#pragma hdrstop

#include <algorithm>

#include "FormSetIgnoredPixels.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmSetIgnoredPixels *frmSetIgnoredPixels;

//---------------------------------------------------------------------------

SetIgnoredPixels OpenIgnoredPixels(int matrix_width, int matrix_height)
{
	TfrmSetIgnoredPixels *frmSetIgnoredPixels = new TfrmSetIgnoredPixels(Application);

	SetIgnoredPixels sip;

	frmSetIgnoredPixels->MatrixWidth  = matrix_width;
	frmSetIgnoredPixels->MatrixHeight = matrix_height;

	if (frmSetIgnoredPixels->ShowModal() == mrOk)
	{
		sip.Process   = true;
		sip.SetShapeFromInt(frmSetIgnoredPixels->cbCustomShape->ItemIndex);
		sip.Parameter = frmSetIgnoredPixels->cbCustomShapeParam->ItemIndex;
	}

	delete frmSetIgnoredPixels;

	return sip;
}

//---------------------------------------------------------------------------
__fastcall TfrmSetIgnoredPixels::TfrmSetIgnoredPixels(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmSetIgnoredPixels::cbCustomShapeChange(TObject *Sender)
{
	cbCustomShapeParam->Clear();

	switch (cbCustomShape->ItemIndex)
	{
	case customShapeNone:
	case customShapeCircle:
		cbCustomShapeParam->Items->Add(GLanguageHandler->Text[kNA].c_str());
		break;
	case customShapeJustBorders:
		int c = std::floor(std::min(MatrixWidth, MatrixHeight) / 2);

		for (int t = 1; t <= c; t++)
		{
			cbCustomShapeParam->Items->Add(IntToStr(t));
		}
		break;
	}

	cbCustomShapeParam->ItemIndex = 0;
}


void TfrmSetIgnoredPixels::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kSetIgnoredPixelsFromPattern].c_str();

	Label1->Caption = GLanguageHandler->Text[kUseCustomShape].c_str();
	Label11->Caption = GLanguageHandler->Text[kBorder].c_str();
	Label2->Caption = GLanguageHandler->Text[kPixels].c_str();

	cbCustomShape->Items->Add(GLanguageHandler->Text[kNoCustomShape].c_str());
	cbCustomShape->Items->Add(GLanguageHandler->Text[kCircle].c_str());
	cbCustomShape->Items->Add(GLanguageHandler->Text[kFrameBorderNoCentre].c_str());
	cbCustomShape->ItemIndex = 0;

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
