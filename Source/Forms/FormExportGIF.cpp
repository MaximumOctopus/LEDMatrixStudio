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
TfrmExportGIF *frmExportGIF;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

ExportGIFSettings OpenExportGIF(ExportGIFSettings oldgif)
{
	TfrmExportGIF *frmExportGIF = new TfrmExportGIF(Application);

	frmExportGIF->eFileName->Text  = oldgif.FileName.c_str();
	frmExportGIF->ePixelSize->Text = oldgif.PixelSize;

	switch (oldgif.PixelShape)
	{
	case 0:
		frmExportGIF->rbSquare->Checked = true;
		break;
	case 1:
		frmExportGIF->rbCircle->Checked = true;
		break;
	case 2:
		frmExportGIF->rbRoundRect->Checked = true;
		break;
	}

	frmExportGIF->ShapeNorfolkDigital->Brush->Color = TColor(oldgif.Background);

	frmExportGIF->seAnimationSpeed->Value = oldgif.AnimationSpeed;

	if (frmExportGIF->ShowModal() == mrOk)
	{
		oldgif.Process = true;

		oldgif.FileName = frmExportGIF->eFileName->Text;
		oldgif.PixelSize = frmExportGIF->ePixelSize->Text.ToIntDef(1);

		oldgif.Background     = frmExportGIF->ShapeNorfolkDigital->Brush->Color;
		oldgif.AnimationSpeed = frmExportGIF->seAnimationSpeed->Value;

		if (frmExportGIF->rbSquare->Checked)
		{
		  	oldgif.PixelShape = 0;
		}
		else if (frmExportGIF->rbCircle->Checked)
		{
			oldgif.PixelShape = 1;
		}
		else
		{
			oldgif.PixelShape = 2;
		}
	}

	delete frmExportGIF;

	return oldgif;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

__fastcall TfrmExportGIF::TfrmExportGIF(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmExportGIF::ShapeNorfolkDigitalMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (cdExportGIF->Execute())
	{
		ShapeNorfolkDigital->Brush->Color = cdExportGIF->Color;
	}
}


void __fastcall TfrmExportGIF::eFileNameChange(TObject *Sender)
{
	if (eFileName->Text != "")
	{
		bOk->Enabled = true;
	}
}


void __fastcall TfrmExportGIF::bSaveClick(TObject *Sender)
{
	if (sdExportGIF->Execute())
	{
		eFileName->Text = sdExportGIF->FileName;

		bOk->Enabled = true;
	}
}


void TfrmExportGIF::SetGuiLanguageText()
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
