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

#include "ExportGIFSettings.h"
#include "FormExportGIF.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm10 *Form10;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

ExportGIFSettings OpenExportGIF(ExportGIFSettings oldgif)
{
	TForm10 *Form10 = new TForm10(Application);

	Form10->eFileName->Text  = oldgif.FileName.c_str();
	Form10->ePixelSize->Text = oldgif.PixelSize;

	switch (oldgif.PixelShape)
	{
	case 0:
		Form10->rbSquare->Checked = true;
		break;
	case 1:
		Form10->rbCircle->Checked = true;
		break;
	case 2:
		Form10->rbRoundRect->Checked = true;
		break;
	}

	Form10->ShapeNorfolkDigital->Brush->Color = TColor(oldgif.Background);

	Form10->seAnimationSpeed->Value = oldgif.AnimationSpeed;

	if (Form10->ShowModal() == mrOk)
	{
		oldgif.Process = true;

		oldgif.FileName = Form10->eFileName->Text;
		oldgif.PixelSize = Form10->ePixelSize->Text.ToIntDef(1);

		oldgif.Background     = Form10->ShapeNorfolkDigital->Brush->Color;
		oldgif.AnimationSpeed = Form10->seAnimationSpeed->Value;

		if (Form10->rbSquare->Checked)
		{
		  	oldgif.PixelShape = 0;
		}
		else if (Form10->rbCircle->Checked)
		{
			oldgif.PixelShape = 1;
		}
		else
		{
			oldgif.PixelShape = 2;
		}
	}

	delete Form10;

	return oldgif;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

__fastcall TForm10::TForm10(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TForm10::ShapeNorfolkDigitalMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (cdExportGIF->Execute())
	{
		ShapeNorfolkDigital->Brush->Color = cdExportGIF->Color;
	}
}


void __fastcall TForm10::eFileNameChange(TObject *Sender)
{
	if (eFileName->Text != "")
	{
		bOk->Enabled = true;
	}
}


void __fastcall TForm10::bSaveClick(TObject *Sender)
{
	if (sdExportGIF->Execute())
	{
		eFileName->Text = sdExportGIF->FileName;

		bOk->Enabled = true;
	}
}


void TForm10::SetGuiLanguageText()
{
	Caption                     = GLanguageHandler->Text[kOK].c_str();

	Label1->Caption              = GLanguageHandler->Text[kFileName].c_str();
	bSave->Caption               = GLanguageHandler->Text[kSave].c_str();
	Label2->Caption              = GLanguageHandler->Text[kPixelSize].c_str();
	Label3->Caption              = GLanguageHandler->Text[kSizeCoefficientHelpText].c_str();
	Label5->Caption              = GLanguageHandler->Text[kBackground].c_str();
	Label4->Caption              = GLanguageHandler->Text[kPixelShape].c_str();
	lAnimationSpeed->Caption     = GLanguageHandler->Text[kAnimationSpeed].c_str();
	lAnimationSpeedHelp->Caption = GLanguageHandler->Text[kAnimationSpeedHelp].c_str();
	rbSquare->Caption            = GLanguageHandler->Text[kSquare].c_str();
	rbCircle->Caption            = GLanguageHandler->Text[kCircle].c_str();
	rbRoundRect->Caption         = GLanguageHandler->Text[kSquareR].c_str();

	bOk->Caption                 = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption             = GLanguageHandler->Text[kCancel].c_str();
}
