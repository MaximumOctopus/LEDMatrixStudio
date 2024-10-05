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

#include "FormImportBitmap.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmImportBitmap *frmImportBitmap;
//---------------------------------------------------------------------------

__fastcall TfrmImportBitmap::TfrmImportBitmap(TComponent* Owner)
	: TForm(Owner)
{
	for (int x = 1; x < 65; x++)
	{
		cbWidth->Items->Add(x);
		cbHeight->Items->Add(x);
	}

	cbWidth->ItemIndex  = 15;
	cbHeight->ItemIndex = 15;

	SetGuiLanguageText();
}


void __fastcall TfrmImportBitmap::bOKClick(TObject *Sender)
{
	Import = ImportMode::kInvalid;

	switch (cbImportColourMode->ItemIndex)
	{
		case 0:
			ImportMode = ImportColourMode::kMono;
			break;
		case 1:
			ImportMode = ImportColourMode::kRGB;
			break;
		case 2:
			ImportMode = ImportColourMode::kRGB3bpp;
			break;
	}

	switch (pcImportMethod->ActivePageIndex)
	{
	case 0:
		if (!ImageFilename.empty())
		{
			Import = ImportMode::kSingleImage;

			FrameCount  = eFrames->Text.ToIntDef(-1);
			FrameWidth  = cbWidth->Text.ToIntDef(0);
			FrameHeight = cbHeight->Text.ToIntDef(0);

			CreateNew   = cbCreateNew->Checked;
		}
		break;
	case 1:
		if (eMIFirstImage->Text != L"")
		{
			Import = ImportMode::kMultipleImages;

			FrameCount  = eFrames->Text.ToIntDef(-1);
			FrameWidth  = cbWidth->Text.ToIntDef(0);
			FrameHeight = cbHeight->Text.ToIntDef(0);

			FirstFrame  = eMIFirstFrame->Text.ToIntDef(0);
			PadLength   = eMIPadLength->Text.ToIntDef(0);
			Pattern     = eMIPattern->Text.ToIntDef(0);

			CreateNew   = cbCreateNew->Checked;
		}
		break;
	}
}

void __fastcall TfrmImportBitmap::bCancelClick(TObject *Sender)
{
	FrameCount = -1;
}


void __fastcall TfrmImportBitmap::bSelectClick(TObject *Sender)
{
	if (opdMain->Execute())
	{
		if (FileExists(opdMain->FileName))
		{
			iImport->Picture->LoadFromFile(opdMain->FileName);

			ImageFilename         = opdMain->FileName;

			lFileName->Caption    = opdMain->FileName;

			lImageWidth->Caption  = iImport->Picture->Width;
			lImageHeight->Caption = iImport->Picture->Height;

            bAutoClick(nullptr);

			bOK->Enabled = true;
		}
	}
}


void __fastcall TfrmImportBitmap::bAutoClick(TObject *Sender)
{
	if (pcImportMethod->ActivePageIndex == 0)
	{
		int count = eFrames->Text.ToIntDef(1);

		int width = std::round((double)iImport->Picture->Width / (double)count);

		cbWidth->Text  = width;
		cbHeight->Text = iImport->Picture->Height;
	}
}


void TfrmImportBitmap::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kImportFromBitmap].c_str();

	tsSingleImage->Caption = GLanguageHandler->Text[kSingleImage].c_str();
	bSelect->Caption = GLanguageHandler->Text[kSelect].c_str();
	lFileName->Caption = GLanguageHandler->Text[kNoImagesSelected].c_str();

	tsMultipleImages->Caption = GLanguageHandler->Text[kMultipleImages].c_str();
	Label5->Caption = GLanguageHandler->Text[kFirstImage].c_str();
	Label8->Caption = GLanguageHandler->Text[kFirstFrame].c_str();
	Label10->Caption = GLanguageHandler->Text[kIndexLength].c_str();
	lImageLengthExample->Caption = GLanguageHandler->Text[kIndexLengthExample].c_str();
	Label7->Caption = GLanguageHandler->Text[kPattern].c_str();

	gbSettings->Caption = GLanguageHandler->Text[kImportSettings].c_str();
	cbCreateNew->Caption = GLanguageHandler->Text[kCreateNewMatrixClearsAllData].c_str();
	Label2->Caption = GLanguageHandler->Text[kFramesToImport].c_str();
	Label3->Caption = GLanguageHandler->Text[kFrameWidth].c_str();
	Label4->Caption = GLanguageHandler->Text[kFrameHeight].c_str();
	bAuto->Caption = GLanguageHandler->Text[kAuto].c_str();

	lWidth->Caption = GLanguageHandler->Text[kWidth].c_str();
	lHeight->Caption = GLanguageHandler->Text[kHeight].c_str();
	lImageWidth->Caption = GLanguageHandler->Text[kNA].c_str();
	lImageHeight->Caption = GLanguageHandler->Text[kNA].c_str();

	lHelpText->Caption = GLanguageHandler->Text[kForNonRGBImport].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


void __fastcall TfrmImportBitmap::sbMISelectFirstImageClick(TObject *Sender)
{
	if (opdMain->Execute())
	{
		iMultipleImages->Picture->LoadFromFile(opdMain->FileName);

		cbWidth->Text       = iMultipleImages->Width;
		cbHeight->Text      = iMultipleImages->Height;

		eMIFirstImage->Text = opdMain->FileName;

		SetMultipleImageDetails();

		bOK->Enabled = true;
	}
}


void TfrmImportBitmap::SetMultipleImageDetails()
{
	bool InNumber = false;
	std::wstring FileName = ExtractFileName(eMIFirstImage->Text).c_str();

	int PatternStart = -1;
	int PatternEnd   = -1;

	for (int t = FileName.length() - 1; t >= 0; t--)
	{
		if (isdigit(FileName[t]))
		{
			if (InNumber)
			{
				FirstFrame = FileName[t] + FirstFrame;
			}
			else
			{
				InNumber = true;

				FirstFrame = FileName[t];

				PatternEnd = t;
			}
		}
		else
		{
			if (InNumber)
			{
				PatternStart = t;

				break;
			}
		}
	}

	if (PatternStart != -1  && PatternEnd != -1)
	{
		eMIFirstFrame->Text = FirstFrame;

		eMIPattern->Text    = ExtractFilePath(eMIFirstImage->Text) + Utility::WS2US(FileName.substr(0, PatternStart) + L"$$" + FileName.substr(0, PatternEnd + 1));

		eMIPadLength->Text  =(PatternEnd - PatternStart);
	}
}


void __fastcall TfrmImportBitmap::cbImportColourModeChange(TObject *Sender)
{
	lHelpText->Visible = cbImportColourMode->ItemIndex > 0;
}
