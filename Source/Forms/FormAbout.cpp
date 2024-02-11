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

#include "FormAbout.h"
#include "Utility.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmAbout *frmAbout;
//---------------------------------------------------------------------------

__fastcall TfrmAbout::TfrmAbout(TComponent* Owner)
	: TForm(Owner)
{
}


void __fastcall TfrmAbout::lEmailClick(TObject *Sender)
{
	Utility::ExecuteFile(L"mailto:paul@freshney.org");
}


void __fastcall TfrmAbout::lWebsiteClick(TObject *Sender)
{
	Utility::ExecuteFile(L"https://github.com/MaximumOctopus/LEDMatrixStudio");
}
