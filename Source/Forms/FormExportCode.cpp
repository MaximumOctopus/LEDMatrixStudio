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

#include "ExportMonoBi.h"
#include "ExportRGB.h"
#include "ExportUtility.h"
#include "Formatting.h"
#include "FormExportCode.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "ProfileHandler.h"
#include "SystemSettings.h"
#include "DateUtility.h"

extern LanguageHandler *GLanguageHandler;
extern ProfileHandler *GProfileHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmExportCode *frmExportCode;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

int OpenExportCode(TheMatrix *thematrix)
{
	TfrmExportCode *frmExportCode = new TfrmExportCode(Application);

	frmExportCode->matrix = thematrix;

	if (frmExportCode->ShowModal() == mrOk)
	{

	}

	delete frmExportCode;

	return 0;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

__fastcall TfrmExportCode::TfrmExportCode(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();

	UpdatePlatformList();
}


void __fastcall TfrmExportCode::FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
		  int &MaxWidth, int &MaxHeight)
{
	MinHeight = 490;
	MinWidth  = 490;
}


void __fastcall TfrmExportCode::FormClose(TObject *Sender, TCloseAction &Action)
{
	Action = caFree;
}


void __fastcall TfrmExportCode::cbPlatformsChange(TObject *Sender)
{
	UpdateCodeList();
}


void __fastcall TfrmExportCode::cbCodeChange(TObject *Sender)
{
	LoadCode();
}


void __fastcall TfrmExportCode::sbSaveClick(TObject *Sender)
{
	if (sdExportCode->Execute())
	{
		Memo1->Lines->SaveToFile(sdExportCode->FileName);
	}
}


void __fastcall TfrmExportCode::sbCopyToClipboardClick(TObject *Sender)
{
	Memo1->SelectAll();
	Memo1->CopyToClipboard();
}


void TfrmExportCode::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kGenerateCode].c_str();
	gbPlatforms->Caption = GLanguageHandler->Text[kPlatforms].c_str();
	gbCodeTemplates->Caption = GLanguageHandler->Text[kCodeTemplate].c_str();

	gbSettings->Caption = GLanguageHandler->Text[kSettings].c_str();

	Label1->Caption = GLanguageHandler->Text[kSource].c_str();
	Label2->Caption = GLanguageHandler->Text[kDirection].c_str();
	Label4->Caption = GLanguageHandler->Text[kScan].c_str();
	Label3->Caption = GLanguageHandler->Text[kLSB].c_str();
	Label5->Caption = GLanguageHandler->Text[kFormat].c_str();
	Label8->Caption = GLanguageHandler->Text[kNumbers].c_str();
	Label10->Caption = GLanguageHandler->Text[kGrouping].c_str();
	Label12->Caption = GLanguageHandler->Text[kOutput].c_str();
	Label14->Caption = GLanguageHandler->Text[kRGB].c_str();
	Label6->Caption = GLanguageHandler->Text[kMinWidth].c_str();
	Label7->Caption = GLanguageHandler->Text[kMaxWidth].c_str();
	Label9->Caption = GLanguageHandler->Text[kMinHeight].c_str();
	Label11->Caption = GLanguageHandler->Text[kMaxHeight].c_str();

	sbSave->Caption = GLanguageHandler->Text[kSave].c_str();
	sbCopyToClipboard->Caption = GLanguageHandler->Text[kCopyToClipboard].c_str();
	bClose->Caption = GLanguageHandler->Text[kClose].c_str();
}


void TfrmExportCode::UpdatePlatformList()
{
	cbPlatforms->Clear();

	std::wstring path = GSystemSettings->App.LMSFilePath + L"codetemplates\\*.*";

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(path.c_str(), &file);

	if (search_handle != INVALID_HANDLE_VALUE)
	{
		do
		{
			if (file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			{
				std::wstring s = file.cFileName;

				if (s[0] != L'.' && s[0] != L'_')
				{
					cbPlatforms->Items->Add(s.c_str());
				}
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}

	if (cbPlatforms->Items->Count != 0)
	{
		cbPlatforms->ItemIndex = 0;
		cbPlatforms->Enabled   = true;

		UpdateCodeList();
	}
	else
	{
		cbPlatforms->Enabled = false;
	}
}


void TfrmExportCode::UpdateCodeList()
{
	cbCode->Clear();

	std::wstring path = GSystemSettings->App.LMSFilePath + L"codetemplates\\" + cbPlatforms->Text.c_str() + L"\\*.*";

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(path.c_str(), &file);

	if (search_handle != INVALID_HANDLE_VALUE)
	{
		do
		{
			if (!(file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
			{
				std::wstring s = file.cFileName;

				if (s[0] != L'.')
				{
					if (s.find(L".template") == std::wstring::npos)
					{
						cbCode->Items->Add(s.c_str());
                    }
				}
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}

	if (cbCode->Items->Count != 0)
	{
		cbCode->ItemIndex = 0;
		cbCode->Enabled   = true;

		LoadCode();
	}
	else
	{
		cbCode->Items->Add(GLanguageHandler->Text[kNoCodeTemplatesFound].c_str());

		cbCode->ItemIndex = 0;
		cbCode->Enabled = false;
	}
}


void TfrmExportCode::LoadCode()
{
	if (!cbCode->Enabled || matrix == nullptr) return;

	std::wstring path = GSystemSettings->App.LMSFilePath + L"codetemplates\\" + cbPlatforms->Text.c_str() + L"\\" + cbCode->Text.c_str() + L".template";

	eeo = GProfileHandler->Load(path.c_str());

	eeo.Code.RGBBrightness = 100;

	if (eeo.Valid)
	{
		lDescription->Caption = eeo.Information.c_str();

		int totalentrycount = 0;

		eeo.Code.StartFrame     = 1;
		eeo.Code.EndFrame       = matrix->GetFrameCount();

		if (eeo.Code.Source == ReadSource::kRows)
		{
			eeo.Code.SelectiveStart = 1;
			eeo.Code.SelectiveEnd = matrix->Details.Height - 1;
		}
		else
		{
			eeo.Code.SelectiveStart = 1;
			eeo.Code.SelectiveEnd = matrix->Details.Width - 1;
		}

		UpdateSettingsDisplay();

		std::vector<std::wstring> Output;
		std::vector<std::wstring> Unique;

		if (eeo.Code.RGBEnabled)
		{
			ExportRGB::CreateExportAnimationRGB(matrix, eeo, Output, totalentrycount, Unique);
		}
		else
		{
			ExportMonoBi::CreateExportAnimation(matrix, eeo, Output, totalentrycount, Unique);
		}

		// ===============================================================

		std::wstring filepath = GSystemSettings->App.LMSFilePath + L"codetemplates\\" + cbPlatforms->Text.c_str() + L"\\" + cbCode->Text.c_str();

		Memo1->Lines->LoadFromFile(filepath.c_str());

		if (Output.size() != 0)
		{
			Memo1->Lines->BeginUpdate();

			// == first lets process the data token ======================

			if (Pos(L"{$LMS_MATRIX_DATA$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_MATRIX_DATA$}", Formatting::VectorToString(Output).c_str(), TReplaceFlags() << rfReplaceAll);
			}

			// == Now the rest of the tokens =============================

			if (Pos(L"{$LMS_FRAMES$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_FRAMES$}", IntToStr(matrix->GetFrameCount()), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_FRAMES_MINUS_ONE$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_FRAMES_MINUS_ONE$}", IntToStr(matrix->GetFrameCount() - 1), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_BYTES$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_BYTES$}", IntToStr(totalentrycount), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_COUNT$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_COUNT$}", IntToStr(totalentrycount), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_MATRIX_WIDTH$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_MATRIX_WIDTH$}", IntToStr(matrix->Details.Width), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_MATRIX_HEIGHT$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_MATRIX_HEIGHT$}", IntToStr(matrix->Details.Height), TReplaceFlags() << rfReplaceAll);
			}

			// == Misc tokens ============================================

			if (Pos(L"{$LMS_DATE$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_DATE$}", DateUtility::GetDate().c_str(), TReplaceFlags() << rfReplaceAll);
			}

			if (Pos(L"{$LMS_TIME$}", Memo1->Text) != 0)
			{
				Memo1->Text = StringReplace(Memo1->Text, L"{$LMS_TIME$}", DateUtility::GetTime().c_str(), TReplaceFlags() << rfReplaceAll);
			}

			// ===========================================================

			Memo1->Lines->EndUpdate();
		}
	}
	else
	{
		std::wstring caption = GLanguageHandler->Text[kErrorLoadingTemplate] +
							   L"\n\n\"" +
							   GSystemSettings->App.LMSFilePath + L"codetemplates\\" + cbPlatforms->Text.c_str() + L"\\" + cbCode->Text.c_str() + L".template\"";

		MessageDlg(caption.c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
	}
}


std::wstring TfrmExportCode::GetDimensionConstraint(TLabel *label, int limit, int dim, int mode, TImage *image)
{
	label->Font->Color = clBlack;

	switch (mode)
	{
	case 0:	// minimum
		if (limit == 0)
		{
			return GLanguageHandler->Text[kUnlimited];
		}
		else
		{
			if (dim < limit)
			{
				label->Font->Color = clMaroon;

				image->Visible = true;

				return std::to_wstring(dim) + L" < " + std::to_wstring(limit);
			}
			else
			{
				return std::to_wstring(limit);
			}
		}
		break;
	case 1:	// maximum
		if (limit == 0)
		{
			return GLanguageHandler->Text[kUnlimited];
		}
		else
		{
			if (dim > limit)
			{
				label->Font->Color = clMaroon;

				image->Visible = true;

				return  std::to_wstring(dim) + L" > " + std::to_wstring(limit);
			}
			else
			{
				return std::to_wstring(limit);
			}
		}
		break;
	}

    return L"";
}


void TfrmExportCode::UpdateSettingsDisplay()
{
	if (cbCode->Enabled)
	{
		switch (eeo.Code.Source)
		{
		case ReadSource::kColumns:
			lSource->Caption    = GLanguageHandler->Text[kColumns].c_str();
			lDirection->Caption = ExportUtility::GetOrientation(eeo, false).c_str();
			lScan->Caption      = ExportUtility::GetScanDirection(eeo, false).c_str();
			lOutput->Caption    = ExportUtility::GetLineContent(eeo, false).c_str();
			break;
		case ReadSource::kRows:
			lSource->Caption    = GLanguageHandler->Text[kRows].c_str();
			lDirection->Caption = ExportUtility::GetOrientation(eeo, false).c_str();
			lScan->Caption      = ExportUtility::GetScanDirection(eeo, false).c_str();
			lOutput->Caption    = ExportUtility::GetLineContent(eeo, false).c_str();
			break;
		}

		lLSB->Caption    = ExportUtility::GetLSB(eeo, false).c_str();
		lFormat->Caption = ExportUtility::GetLanguage(eeo.Code.Language, false).c_str();

		switch (eeo.Code.Format)
		{
		case NumberFormat::kDecimal:
			lNumbers->Caption = GLanguageHandler->Text[kDecimal].c_str();
			break;
		case NumberFormat::kBinary:
			lNumbers->Caption = GLanguageHandler->Text[kBinary].c_str();
			break;
		case NumberFormat::kHex:
			lNumbers->Caption = GLanguageHandler->Text[kHex].c_str();
			break;
		}

		lGrouping->Caption = ExportUtility::GetNumberSize(eeo.Code.Language, eeo.Code.Size, false).c_str();

		if (eeo.Code.RGBEnabled)
		{
			lRGB->Caption = ExportUtility::GetRGBMode(eeo, false).c_str();
		}
		else
		{
			lRGB->Caption = GLanguageHandler->Text[kDisabled].c_str();
		}
	}
	else
	{
		lSource->Caption    = GLanguageHandler->Text[kNA].c_str();
		lDirection->Caption = GLanguageHandler->Text[kNA].c_str();
		lScan->Caption      = GLanguageHandler->Text[kNA].c_str();
		lOutput->Caption    = GLanguageHandler->Text[kNA].c_str();
		lLSB->Caption       = GLanguageHandler->Text[kNA].c_str();
		lFormat->Caption    = GLanguageHandler->Text[kNA].c_str();
		lNumbers->Caption   = GLanguageHandler->Text[kNA].c_str();
		lGrouping->Caption  = GLanguageHandler->Text[kNA].c_str();
		lRGB->Caption       = GLanguageHandler->Text[kNA].c_str();
	}

	// =======================================================================

	lMinWidth->Caption  = GetDimensionConstraint(lMinWidth, eeo.MinWidth, matrix->Details.Width, 0, iMiW).c_str();
	lMaxWidth->Caption  = GetDimensionConstraint(lMaxWidth, eeo.MaxWidth, matrix->Details.Width, 1, iMaW).c_str();

	lMinHeight->Caption = GetDimensionConstraint(lMinHeight, eeo.MinHeight, matrix->Details.Height, 0, iMiH).c_str();
	lMaxHeight->Caption = GetDimensionConstraint(lMaxHeight, eeo.MaxHeight, matrix->Details.Height, 1, iMaH).c_str();
}
