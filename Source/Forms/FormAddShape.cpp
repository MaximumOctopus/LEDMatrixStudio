// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
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

#include "FormAddShape.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmAddShape *frmAddShape;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
__fastcall TfrmAddShape::TfrmAddShape(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void TfrmAddShape::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kAddShape].c_str();

	cbShape->Items->Add(GLanguageHandler->Text[kShapeCircle].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeLineHorizontal].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeLineVertical].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeSquare].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeSquareFilled].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeRectangle].c_str());
	cbShape->Items->Add(GLanguageHandler->Text[kShapeRectangleFilled].c_str());
	cbShape->ItemIndex = 0;

	lSizeX->Caption = GLanguageHandler->Text[kRadius].c_str();
	lPixels->Caption = GLanguageHandler->Text[kASPixels].c_str();

	cbInitialDirection->Items->Add(GLanguageHandler->Text[kBottomLeft].c_str());
	cbInitialDirection->Items->Add(GLanguageHandler->Text[kBottomRight].c_str());
	cbInitialDirection->ItemIndex = 0;

	lColour->Caption = GLanguageHandler->Text[kColour].c_str();
}


void __fastcall TfrmAddShape::cbShapeChange(TObject *Sender)
{
	switch (cbShape->ItemIndex)
	{
	case 0:
		ePixels->Enabled = true;
		lSizeX->Caption = GLanguageHandler->Text[kRadius].c_str();
		lSizeY->Caption = L"";
		eSizeY->Enabled = false;
		cbInitialDirection->Enabled = false;
		break;
	case 1:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kLength].c_str();;
		lSizeY->Caption = L"";
		eSizeY->Enabled = false;
		cbInitialDirection->Enabled = false;
		break;
	case 2:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kHeight].c_str();
		lSizeY->Caption = L"";
		eSizeY->Enabled = false;
		cbInitialDirection->Enabled = false;
		break;
	case 3:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kWidth].c_str();
		lSizeY->Caption = L"";
		eSizeY->Enabled = false;
		cbInitialDirection->Enabled = false;
		break;
	case 4:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kWidth].c_str();
		lSizeY->Caption = L"";
		eSizeY->Enabled = false;
		cbInitialDirection->Enabled = true;
		break;
	case 5:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kWidth].c_str();
		lSizeY->Caption = GLanguageHandler->Text[kHeight].c_str();
		eSizeY->Enabled = true;
		cbInitialDirection->Enabled = false;
		break;
	case 6:
		ePixels->Enabled = false;
		lSizeX->Caption = GLanguageHandler->Text[kWidth].c_str();
		lSizeY->Caption = GLanguageHandler->Text[kHeight].c_str();
		eSizeY->Enabled = true;
		cbInitialDirection->Enabled = true;
		break;
	}
}


void __fastcall TfrmAddShape::bOKClick(TObject *Sender)
{
	SelectedShape = cbShape->ItemIndex;
	SelectedColour = shapeColour->Brush->Color;
	SelectedDirection = cbInitialDirection->ItemIndex;

	int SizeX = eSizeX->Text.ToIntDef(-1);
	int SizeY = eSizeY->Text.ToIntDef(-1);
	int Pixels = ePixels->Text.ToIntDef(-1);
	int X = ePositionX->Text.ToIntDef(-1);
	int Y = ePositionY->Text.ToIntDef(-1);

	switch (SelectedShape)
	{
	case 0:
		if (SizeX != -1 && Pixels != -1 && X != -1 && Y != -1)
		{
			SelectedSizeX = SizeX;
			SelectedPixels = Pixels;
			SelectedX = X;
			SelectedY = Y;

			ModalResult = mrOk;
		}
		break;
	case 1:
	case 2:
		if (SizeX != -1 && X != -1 && Y != -1)
		{
			SelectedSizeX = SizeX;
			SelectedX = X;
			SelectedY = Y;

			ModalResult = mrOk;
		}
		break;
	case 3:
	case 4:
		if (SizeX != -1 && X != -1 && Y != -1)
		{
			SelectedSizeX = SizeX;
			SelectedX = X;
			SelectedY = Y;

			ModalResult = mrOk;
		}
		break;
	case 5:
	case 6:
		if (SizeX != -1 && SizeY != -1 && X != -1 && Y != -1)
		{
			SelectedSizeX = SizeX;
			SelectedSizeY = SizeY;
			SelectedX = X;
			SelectedY = Y;

			ModalResult = mrOk;
		}
		break;
    }
}


void __fastcall TfrmAddShape::shapeColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	cdAddShape->Color = shape->Brush->Color;

	if (cdAddShape->Execute())
	{
		shapeColour->Brush->Color = cdAddShape->Color;
	}
}
