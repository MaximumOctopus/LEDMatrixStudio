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

#include "FormCopyMultiple.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmCopyMultiple *frmCopyMultiple;
//---------------------------------------------------------------------------

CopyMultipleObject OpenCopyMultiple(int frame_count, std::vector<std::wstring> &Layers)
{
	TfrmCopyMultiple *frmCopyMultiple = new TfrmCopyMultiple(Application);

	frmCopyMultiple->FrameCount = frame_count;

	CopyMultipleObject cmo;

	for (int layer = 0; layer < Layers.size(); layer++)
	{
		frmCopyMultiple->cbSourceLayer->Items->Add(Layers[layer].c_str());
		frmCopyMultiple->cbDestinationLayer->Items->Add(Layers[layer].c_str());
	}

	frmCopyMultiple->cbSourceLayer->ItemIndex      = 0;
	frmCopyMultiple->cbDestinationLayer->ItemIndex = 0;

	if (frmCopyMultiple->ShowModal() == mrOk)
	{
		cmo.Process     = true;
		cmo.StartFrame  = frmCopyMultiple->eStartFrame->Text.ToInt();
		cmo.EndFrame    = frmCopyMultiple->eEndFrame->Text.ToInt();
		cmo.CopyTo      = frmCopyMultiple->eCopyTo->Text.ToInt();

		cmo.Source      = frmCopyMultiple->cbSourceLayer->ItemIndex;
		cmo.Destination = frmCopyMultiple->cbDestinationLayer->ItemIndex;

		cmo.AllLayers   = frmCopyMultiple->cbAllLayers->Checked;
	}

	delete frmCopyMultiple;

	return cmo;
}

//---------------------------------------------------------------------------

__fastcall TfrmCopyMultiple::TfrmCopyMultiple(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmCopyMultiple::FormShow(TObject *Sender)
{
	eStartFrameChange(nullptr);
}


void __fastcall TfrmCopyMultiple::eStartFrameChange(TObject *Sender)
{
	bOK->Enabled = ValidateInputs();
}


void __fastcall TfrmCopyMultiple::cbAllLayersClick(TObject *Sender)
{
	cbDestinationLayer->Enabled = !cbAllLayers->Checked;

	eStartFrameChange(nullptr);
}


bool TfrmCopyMultiple::ValidateInputs()
{
	bool warning = false;

	int start = eStartFrame->Text.ToIntDef(-1);
	int end   = eEndFrame->Text.ToIntDef(-1);
	int copyto    = eCopyTo->Text.ToIntDef(-1);
	int maxframes = copyto + (end - start);               // max frame to write to

	if (cbAllLayers->Checked)
	{
		if (copyto >= start && copyto <= end)
		{
			warning = true;
		}
	}
	else
	{
		if (copyto >= start && copyto <= end && cbSourceLayer->ItemIndex == cbDestinationLayer->ItemIndex)
		{
			warning = true;
		}
	}

	lWarningMessage->Visible = warning;

	return (start != -1 && end != -1 && copyto != -1 &&
			start <= end && maxframes <= FrameCount && maxframes >= 1 && !warning);
}


void TfrmCopyMultiple::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kCopyMultipleFrames].c_str();

	Label1->Caption = GLanguageHandler->Text[kStart].c_str();
	Label2->Caption = GLanguageHandler->Text[kEnd].c_str();
	Label3->Caption = GLanguageHandler->Text[kCopyTo].c_str();
	Label5->Caption = GLanguageHandler->Text[kDestinationFramesMustExist].c_str();
	lWarningMessage->Caption = GLanguageHandler->Text[kWarningYouWillLoseData].c_str();
	Label7->Caption = GLanguageHandler->Text[kLayers].c_str();
	Label6->Caption = GLanguageHandler->Text[kSource].c_str();
	Label8->Caption = GLanguageHandler->Text[kDestination].c_str();
	cbAllLayers->Caption = GLanguageHandler->Text[kAllLayers].c_str();

	Label4->Caption = GLanguageHandler->Text[kWarningThisActionCannotBeUndone].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
