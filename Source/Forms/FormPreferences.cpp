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

#include "FormPreferences.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemSettings.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmPreferences *frmPreferences;

//---------------------------------------------------------------------------

bool OpenPreferences(PrefsMatrixColours &colours)
{
	bool accept = false;

	TfrmPreferences *frmPreferences = new TfrmPreferences(Application);

	frmPreferences->sMono1->Brush->Color = TColor(colours.Mono[0]);
	frmPreferences->sMono2->Brush->Color = TColor(colours.Mono[1]);

	frmPreferences->sBi1->Brush->Color = TColor(colours.Bi[0]);
	frmPreferences->sBi2->Brush->Color = TColor(colours.Bi[1]);
	frmPreferences->sBi3->Brush->Color = TColor(colours.Bi[2]);
	frmPreferences->sBi4->Brush->Color = TColor(colours.Bi[3]);

	frmPreferences->ShapeSelection->Brush->Color = TColor(colours.Selection);
	frmPreferences->ShapeLightBox->Brush->Color  = TColor(colours.LightBox);

	frmPreferences->eMaxPixels->Text = GSystemSettings->App.ExportUpdateMaxPixels;
	frmPreferences->eExportPreview->Text = GSystemSettings->App.ExportPreviewSize;

	if (GSystemSettings->App.HexPrefix.empty())
	{
		frmPreferences->cbHexFormat->ItemIndex = 0;
	}
	else
	{
		frmPreferences->cbHexFormat->Text = GSystemSettings->App.HexPrefix.c_str();
	}

	frmPreferences->cbDisableWarnings->Checked = GSystemSettings->App.IgnoreWarnings;

	if (frmPreferences->ShowModal() == mrOk)
	{
		colours.Mono[0] = frmPreferences->sMono1->Brush->Color;
		colours.Mono[1] = frmPreferences->sMono2->Brush->Color;

		colours.Bi[0] = frmPreferences->sBi1->Brush->Color;
		colours.Bi[1] = frmPreferences->sBi2->Brush->Color;
		colours.Bi[2] = frmPreferences->sBi3->Brush->Color;
		colours.Bi[3] = frmPreferences->sBi4->Brush->Color;

		colours.Selection = frmPreferences->ShapeSelection->Brush->Color;
		colours.LightBox = frmPreferences->ShapeLightBox->Brush->Color;

		GSystemSettings->App.ExportUpdateMaxPixels = frmPreferences->eMaxPixels->Text.ToIntDef(100000);
		GSystemSettings->App.ExportPreviewSize = frmPreferences->eExportPreview->Text.ToIntDef(512);

	    GSystemSettings->App.IgnoreWarnings = frmPreferences->cbDisableWarnings->Checked;

		if (frmPreferences->cbHexFormat->ItemIndex == 0)
		{
			GSystemSettings->App.HexPrefix = L"";
		}
		else
		{
			GSystemSettings->App.HexPrefix = frmPreferences->cbHexFormat->Text.c_str();
		}

		accept = true;
	}

	delete frmPreferences;

	return accept;
}


__fastcall TfrmPreferences::TfrmPreferences(TComponent* Owner)
	: TForm(Owner)
{
    SetGUILanguageText();
}


void __fastcall TfrmPreferences::sMono1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
	if (colorDialogPrefs->Execute())
	{
		TShape *shape = (TShape*)Sender;

		shape->Brush->Color = colorDialogPrefs->Color;
	}
}


void __fastcall TfrmPreferences::sbClearRecentFileListClick(TObject *Sender)
{
	if (MessageDlg(GLanguageHandler->Text[kAreYouSure].c_str(), mtWarning, mbYesNo, 0) == mrYes)
	{
		GSystemSettings->FileHistory.clear();
	}
}


void __fastcall TfrmPreferences::bResetToDefaultsClick(TObject *Sender)
{
	if (MessageDlg(GLanguageHandler->Text[kAreYouSure].c_str(), mtWarning, mbYesNo, 0) == mrYes)
	{
		sMono1->Brush->Color         = clBlack;
		sMono2->Brush->Color         = clWhite;

		sBi1->Brush->Color           = clBlack;
		sBi2->Brush->Color           = clRed;
		sBi3->Brush->Color           = clGreen;
		sBi4->Brush->Color           = clYellow;

		ShapeSelection->Brush->Color = clBlue;
		ShapeLightBox->Brush->Color  = TColor(0x00AAAAAA);

		eMaxPixels->Text = L"100000";
		eExportPreview->Text = L"256";

        cbDisableWarnings->Checked = false;
	}
}


void TfrmPreferences::SetGUILanguageText()
{
	Caption = GLanguageHandler->Text[kPreferences].c_str();

	gbColours->Caption = GLanguageHandler->Text[kColours].c_str();

	Label2->Caption = GLanguageHandler->Text[kColourRepresenting1].c_str();
	Label8->Caption = GLanguageHandler->Text[kColourRepresenting2].c_str();
	Label3->Caption = GLanguageHandler->Text[kColourRepresenting3].c_str();
	Label4->Caption = GLanguageHandler->Text[kColourRepresentingLightbox].c_str();

	lLightBox->Caption = GLanguageHandler->Text[kLightBox].c_str();
	lSelector->Caption = GLanguageHandler->Text[kSelector].c_str();

	gbLimiter->Caption = GLanguageHandler->Text[kExportAutoGenerateOnStartupLimiter].c_str();
	Label15->Caption = GLanguageHandler->Text[kMaxPixels].c_str();
	Label17->Caption = GLanguageHandler->Text[kPreviewShowFirst].c_str();
	Label18->Caption = GLanguageHandler->Text[kLines].c_str();

	gbMisc->Caption = GLanguageHandler->Text[kMisc].c_str();
	sbClearRecentFileList->Caption = GLanguageHandler->Text[kClearRecentFilesList].c_str();

	lHexFormat->Caption = GLanguageHandler->Text[kHexFormat].c_str();

    cbDisableWarnings->Caption = GLanguageHandler->Text[kDisableWarnings].c_str();

	sbClearRecentFileList->Caption = GLanguageHandler->Text[kClearRecentFilesList].c_str();

	bResetToDefaults->Caption = GLanguageHandler->Text[kResetToDefaults].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
