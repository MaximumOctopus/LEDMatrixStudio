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

#include "FormCheckVersion.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemConstants.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmCheckVersion *frmCheckVersion;

//---------------------------------------------------------------------------

void OpenCheckForNewVersion(std::wstring cv, std::wstring cd, bool autoclosemode)
{
	TfrmCheckVersion *frmCheckVersion = new TfrmCheckVersion(Application);

	frmCheckVersion->AutoClose = autoclosemode;

	frmCheckVersion->lIVDate->Caption = cd.c_str();
	frmCheckVersion->lIVVersion->Caption = cv.c_str();

	frmCheckVersion->ShowModal();

	delete frmCheckVersion;
}

//---------------------------------------------------------------------------


__fastcall TfrmCheckVersion::TfrmCheckVersion(TComponent* Owner)
	: TForm(Owner)
{
    SetGuiLanguageText();
}


// version file exists with a single line in the format
// date;version
void __fastcall TfrmCheckVersion::FormShow(TObject *Sender)
{
	try
	{
		UnicodeString s = httpMain->Get(__ApplicationVersionFileUrl);

		std::wstring cat = s.c_str(); // means we can avoid all of the horrible unicodestring stuff ;)

        auto pos = cat.find(L';');

		if (pos != std::wstring::npos)
		{
			std::wstring date = cat.substr(0, pos);
			std::wstring version = cat.substr(pos + 1);

			lLADate->Caption    = date.c_str();
			lLAVersion->Caption = version.c_str();

			if (lLADate->Caption != lIVDate->Caption || lLAVersion->Caption != lIVVersion->Caption)
			{
				lWhat->Caption = GLanguageHandler->Text[kANewVersionIsAvailable].c_str();

				mHistory->Text = httpMain->Get(__ApplicationHistoryFileUrl);

				bHistory->Enabled = true;
			}
			else
			{
				lWhat->Caption = GLanguageHandler->Text[kNoNewVersionIsAvailable].c_str();
			}
		}
	}
	catch(...)
	{
		lLADate->Caption    = GLanguageHandler->Text[kError].c_str();
		lLAVersion->Caption = GLanguageHandler->Text[kError].c_str();

		lWhat->Caption      = GLanguageHandler->Text[kUnableToConnectTomaximumoctopuscom].c_str();
	}
}


void __fastcall TfrmCheckVersion::bWebsiteClick(TObject *Sender)
{
	Utility::ExecuteFile(L"https://github.com/MaximumOctopus/LEDMatrixStudio");
}


void __fastcall TfrmCheckVersion::bHistoryClick(TObject *Sender)
{
	if (bHistory->Tag == 0)
	{
		std::wstring c = GLanguageHandler->Text[kHistory] + L" <<";

		bHistory->Tag = 1;
		bHistory->Caption = c.c_str();
		ClientHeight = 494;
	}
	else
	{
		std::wstring c = GLanguageHandler->Text[kHistory] + L" >>";

		bHistory->Tag = 0;
		bHistory->Caption = c.c_str();
		ClientHeight = 271;
	}
}


void TfrmCheckVersion::SetGuiLanguageText()
{
	Label5->Caption = GLanguageHandler->Text[kUpdateCheck].c_str();

	gbInstalledVersion->Caption = GLanguageHandler->Text[kInstalledVersion].c_str();
	Label6->Caption = GLanguageHandler->Text[kDate].c_str();
	Label7->Caption = GLanguageHandler->Text[kVersion].c_str();

	gbLatestVersion->Caption = GLanguageHandler->Text[kLatestAvailableOnline].c_str();
	Label10->Caption = GLanguageHandler->Text[kDate].c_str();
	Label11->Caption = GLanguageHandler->Text[kVersion].c_str();

	bWebsite->Caption = GLanguageHandler->Text[kWebsite].c_str();
	std::wstring c = GLanguageHandler->Text[kHistory] + L" >>";
	bHistory->Caption = c.c_str();
	bClose->Caption = GLanguageHandler->Text[kClose].c_str();
}
