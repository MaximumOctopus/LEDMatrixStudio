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

#include "FormSaveRange.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmSaveRange *frmSaveRange;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

SaveFrameRangeObject OpenFrameRange(int frame_count)
{
	TfrmSaveRange *frmSaveRange = new TfrmSaveRange(Application);

	SaveFrameRangeObject sfto;

	frmSaveRange->MatrixFrameCount = frame_count;

	if (frmSaveRange->ShowModal() == mrOk)
	{
		sfto.Process = true;
		sfto.StartFrame = frmSaveRange->eStartFrame->Text.ToInt() - 1;
		sfto.EndFrame = frmSaveRange->eEndFrame->Text.ToInt() - 1;
	}

	delete frmSaveRange;

	return sfto;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TfrmSaveRange::TfrmSaveRange(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmSaveRange::eStartFrameChange(TObject *Sender)
{
	bOK->Enabled = ValidateInputs();
}


void TfrmSaveRange::SetGuiLanguageText()
{
  Caption = GLanguageHandler->Text[kSaveARangeOfFrames].c_str();

  lStart->Caption = GLanguageHandler->Text[kStart].c_str();
  lEnd->Caption = GLanguageHandler->Text[kEnd].c_str();

  bOK->Caption = GLanguageHandler->Text[kOK].c_str();
  bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


bool TfrmSaveRange::ValidateInputs()
{
	int sf = eStartFrame->Text.ToIntDef(-1);
	int ef = eEndFrame->Text.ToIntDef(-1);

	return (sf >= 1 && ef >= 1 && sf <= ef && sf <= MatrixFrameCount && ef <= MatrixFrameCount);
}
