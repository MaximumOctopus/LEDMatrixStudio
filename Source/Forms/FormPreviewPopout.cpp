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

#include "FormPreviewPopout.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmPreviewPopout *frmPreviewPopout;


__fastcall TfrmPreviewPopout::TfrmPreviewPopout(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void TfrmPreviewPopout::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kPreview].c_str();
}


void __fastcall TfrmPreviewPopout::bPlayAnimationClick(TObject *Sender)
{
	if (OnCommand)
	{
		TBitBtn *bb = (TBitBtn*)Sender;

		OnCommand(bb->Tag);
	}
}


void TfrmPreviewPopout::SetForPlaybackStart()
{
	bPlayAnimation->Enabled = false;
	bStartFrame->Enabled    = false;
	bEndFrame->Enabled      = false;
	bNextFrame->Enabled     = false;
	bPreviousFrame->Enabled = false;
	bStopAnimation->Enabled = true;
}


void TfrmPreviewPopout::SetForPlaybackStop()
{
	bPlayAnimation->Enabled        = true;
	bStartFrame->Enabled           = true;
	bEndFrame->Enabled             = true;
	bNextFrame->Enabled            = true;
	bPreviousFrame->Enabled        = true;
	bStopAnimation->Enabled        = false;
}


void __fastcall TfrmPreviewPopout::tbFramesTracking(TObject *Sender)
{
	if (OnNewFrame)
	{
		OnNewFrame(tbFrames->Position - 1);
	}
}
