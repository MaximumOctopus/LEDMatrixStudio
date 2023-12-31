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

#include "FormAddLayer.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TForm3 *Form3;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

AddObject OpenAddLayer(const std::vector<std::wstring> &layers)
{
	TForm3 *Form3 = new TForm3(Application);

	AddObject ao;

	std::wstring s = GLanguageHandler->Text[kLayer] + L" " + std::to_wstring(layers.size());

	Form3->eName->Text = s.c_str();

	for (int t = 0; t < layers.size(); t++)
		Form3->cbSourceLayer->Items->Add(layers[t].c_str());

	Form3->cbSourceLayer->ItemIndex = 0;

	if (Form3->ShowModal() == mrOk)
	{
		ao.Process = true;
		ao.Name = Form3->eName->Text.c_str();
		ao.CopyFrom = Form3->cbCopyFrom->Checked;
		ao.CopyLayer = Form3->cbSourceLayer->ItemIndex;
	}

	delete Form3;

	return ao;
}


__fastcall TForm3::TForm3(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void TForm3::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kAddNewLayer].c_str();
	Label1->Caption = GLanguageHandler->Text[kName].c_str();
	cbCopyFrom->Caption = GLanguageHandler->Text[kCopyFrom].c_str();
	Label6->Caption = GLanguageHandler->Text[kExtraLayersIncreaseApplicationMemoryUsage].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


void __fastcall TForm3::cbCopyFromClick(TObject *Sender)
{
	cbSourceLayer->Enabled = cbCopyFrom->Checked;
}
