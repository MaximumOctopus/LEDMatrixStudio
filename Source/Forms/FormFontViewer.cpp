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

#include "FontHandler.h"
#include "FormFontViewer.h"
#include "MatrixConstants.h"
#include "SystemSettings.h"
#include "Utility.h"

extern FontHandler *GFontHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmFontViewer *frmFontViewer;

//---------------------------------------------------------------------------

void OpenFontViewer()
{
	TfrmFontViewer *frmFontViewer = new TfrmFontViewer(Application);

	frmFontViewer->ShowModal();

	delete frmFontViewer;
}

//---------------------------------------------------------------------------
__fastcall TfrmFontViewer::TfrmFontViewer(TComponent* Owner)
	: TForm(Owner)
{
	DoubleBuffered = true;
	pFont->DoubleBuffered = true;

	FontMatrix = new TheMatrix(this, pFont);

	FontMatrix->NewMatrix(MatrixMode::kMono, 1, 6, 5, 8, 8, 25, PixelShape::kSquare, true, true, true, 0x00000000);

	FontMatrix->LEDColours[0] = 0x00ffffff;
	FontMatrix->LEDColours[1] = 0x00000000;

	FontMatrix->Render.Draw.Colour = 1;
	FontMatrix->RGBBackground = 0x00FFFFFF;

	SetGuiLanguageText();
}


void __fastcall TfrmFontViewer::FormDestroy(TObject *Sender)
{
    delete FontMatrix;
}



void __fastcall TfrmFontViewer::FormShow(TObject *Sender)
{
	BuildFontList();

	cbFontsChange(nullptr);
}


void __fastcall TfrmFontViewer::cbFontsChange(TObject *Sender)
{
	std::wstring name = cbFonts->Text.c_str();

	std::wstring path = GSystemSettings->App.LMSFilePath + L"fonts\\" + name + L".ledsfont";

	if (FileExists(path.c_str()))
	{
		FontMatrix->LoadTextToolFont(path, name);
	}
	else
	{
		MessageDlg(Utility::WS2US(GLanguageHandler->Text[kCannotFindFont] + L"\n\n" + L"\"" + name + L"\""), mtError, TMsgDlgButtons() << mbOK, 0);
	}

	LastFrame = -1;

	tbFontChange(nullptr);
}


void __fastcall TfrmFontViewer::tbFontChange(TObject *Sender)
{
	if (tbFont->Position != LastFrame)
	{
		FontMatrix->ClearCurrentFrame();

		FontMatrix->Render.Draw.Coords[0].X = 0;
		FontMatrix->Render.Draw.Coords[0].Y = 7;

		FontMatrix->DrawFontCharacter(tbFont->Position - 32, 0);

		LastFrame = tbFont->Position;

		SetLabel();
	}
}


void __fastcall TfrmFontViewer::cbRGBModeClick(TObject *Sender)
{
	if (cbRGBMode->Checked)
	{
		FontMatrix->NewMatrix(MatrixMode::kRGB, 1, 6, 5, 8, 8, 25, PixelShape::kSquare, true, true, true, 0x00000000);
	}
	else
	{
		FontMatrix->NewMatrix(MatrixMode::kMono, 1, 6, 5, 8, 8, 25, PixelShape::kSquare, true, true, true, 0x00000000);
	}

	FontMatrix->Render.Draw.Colour = 1;

	cbFontsChange(nullptr);
}


void TfrmFontViewer::BuildFontList()
{
	if (GFontHandler->Fonts.size() != 0)
	{
		for (int t = 0; t < GFontHandler->Fonts.size(); t++)
		{
			cbFonts->Items->Add(GFontHandler->Fonts[t].c_str());
		}

      	cbFonts->ItemIndex = 0;
	}
	else
	{
		cbFonts->Enabled = false;
    }
}


void TfrmFontViewer::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kFontViewer].c_str();

	bSelectFont->Caption = GLanguageHandler->Text[kSelectFont].c_str();
	cbRGBMode->Caption = GLanguageHandler->Text[kViewInRGBMode].c_str();
	lCharacterValue->Caption = GLanguageHandler->Text[kCharacter].c_str();
}


void TfrmFontViewer::SetLabel()
{
	if (tbFont->Position == 32)
	{
		lCharacter->Caption = L"'space'";
	}
	else
	{
		lCharacter->Caption = Char(tbFont->Position);
	}
}
