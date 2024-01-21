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

#include "FramePalettePanel.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Registry.h"
#include "SystemSettings.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TframePalette *framePalette;
//---------------------------------------------------------------------------
__fastcall TframePalette::TframePalette(TComponent* Owner)
	: TFrame(Owner)
{
}


void TframePalette::Init()
{
	bClear->Caption = GLanguageHandler->Text[kClear].c_str();

	RGBPaletteHistory[0] = sRGBP1; RGBPaletteHistory[1] = sRGBP2; RGBPaletteHistory[2] = sRGBP3; RGBPaletteHistory[3] = sRGBP4;
	RGBPaletteHistory[4] = sRGBP5; RGBPaletteHistory[5] = sRGBP6; RGBPaletteHistory[6] = sRGBP7; RGBPaletteHistory[7] = sRGBP8;
	RGBPaletteHistory[8] = sRGBP9; RGBPaletteHistory[9] = sRGBP10; RGBPaletteHistory[10] = sRGBP11; RGBPaletteHistory[11] = sRGBP12;
	RGBPaletteHistory[12] = sRGBP13; RGBPaletteHistory[13] = sRGBP14; RGBPaletteHistory[14] = sRGBP15; RGBPaletteHistory[15] = sRGBP16;
	RGBPaletteHistory[16] = sRGBP17; RGBPaletteHistory[17] = sRGBP18; RGBPaletteHistory[18] = sRGBP19; RGBPaletteHistory[19] = sRGBP20;
	RGBPaletteHistory[20] = sRGBP21; RGBPaletteHistory[21] = sRGBP22; RGBPaletteHistory[22] = sRGBP23; RGBPaletteHistory[23] = sRGBP24;
	RGBPaletteHistory[24] = sRGBP25; RGBPaletteHistory[25] = sRGBP26; RGBPaletteHistory[26] = sRGBP27; RGBPaletteHistory[27] = sRGBP28;
	RGBPaletteHistory[28] = sRGBP29; RGBPaletteHistory[29] = sRGBP30; RGBPaletteHistory[30] = sRGBP31; RGBPaletteHistory[31] = sRGBP32;
	RGBPaletteHistory[32] = sRGBP33; RGBPaletteHistory[33] = sRGBP34; RGBPaletteHistory[34] = sRGBP35;

	RGBPaletteHistoryIndex = 0;

	LoadPaletteHistory();
}


void TframePalette::DeInit()
{
	SavePaletteHistory();
}


void TframePalette::SetUIToColour(int colour)
{
	int r = colour & 0x0000ff;
	int g = (colour & 0x00ff00) >> 8;
	int b = (colour & 0xff0000) >> 16;

	sRGBPaletteColour->Brush->Color = TColor(colour);

	tbRed->Position = r;
	tbGreen->Position = g;
	tbBlue->Position = b;

	eRed->Text = r;
	eGreen->Text = g;
	eBlue->Text = b;

	lPaletteColourHex->Caption = Utility::WS2US(GSystemSettings->App.HexPrefix) +
								 IntToHex(tbRed->Position, 2) +
								 IntToHex(tbGreen->Position, 2) +
								 IntToHex(tbBlue->Position, 2);

	lPaletteColourInteger->Caption = sRGBPaletteColour->Brush->Color;
}


void __fastcall TframePalette::eRedKeyPress(TObject *Sender, System::WideChar &Key)
{
	if (Key == 13)
	{
		TEdit *edit = (TEdit*)Sender;

		int value = edit->Text.ToIntDef(999);

		if (value >= 0 && value <= 255)
		{
			switch (edit->Tag)
			{
			case 0:
				tbRed->Position   = value;
				break;
			case 1:
				tbGreen->Position = value;
				break;
			case 2:
				tbBlue->Position  = value;
				break;
			}

			tbRedChange(nullptr);
		}
	}
}


void __fastcall TframePalette::sRGBP1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	int colour = shape->Brush->Color;
	int mouse = 0;

	if (Shift.Contains(ssLeft))
	{
		mouse = 0;
	}
	else if (Shift.Contains(ssMiddle))
	{
		mouse = 1;
	}
	else if (Shift.Contains(ssRight))
	{
		mouse = 2;
	}

	SetUIToColour(colour);

	if (Sender == sRGBPaletteColour)
	{
		AddToHistory(colour);
    }

	if (OnColourClick) OnColourClick(mouse, colour);
}


void __fastcall TframePalette::sRGBP1MouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
	if (OnColourMove)
	{
		TShape *shape = (TShape*)Sender;

		OnColourMove(shape->Brush->Color);
	}
}


void __fastcall TframePalette::tbRedChange(TObject *Sender)
{
	if (Sender != nullptr)
	{
		TTrackBar *tb = (TTrackBar*)Sender;

		switch (tb->Tag)
		{
		case CRed:
			eRed->Text = tb->Position;
			break;
		case CGreen:
			eGreen->Text = tb->Position;
			break;
		case CBlue:
			eBlue->Text = tb->Position;
			break;
		}
	}

	lPaletteColourHex->Caption = Utility::WS2US(GSystemSettings->App.HexPrefix) +
								 IntToHex(tbRed->Position, 2) +
								 IntToHex(tbGreen->Position, 2) +
								 IntToHex(tbBlue->Position, 2);


	sRGBPaletteColour->Brush->Color = TColor((tbBlue->Position << 16) +
											 (tbGreen->Position << 8) +
											  tbRed->Position);

	lPaletteColourInteger->Caption = sRGBPaletteColour->Brush->Color;
}


void TframePalette::AddToHistory(int colour)
{
	bool canadd = true;

	for (int t = 0; t < kPalletCount; t++)
	{
		if (colour == RGBPaletteHistory[t]->Brush->Color)
		{
			canadd = false;
			break;
		}
	}

	if (canadd)
	{
		RGBPaletteHistory[RGBPaletteHistoryIndex]->Brush->Color = TColor(colour);

		if (RGBPaletteHistoryIndex == kPalletCount - 1)
		{
			RGBPaletteHistoryIndex = 0;
		}
		else
		{
			RGBPaletteHistoryIndex++;
		}
	}
}


bool TframePalette::LoadPaletteHistory()
{
    HKEY hKey;

	LONG dwRet;

	dwRet = RegOpenKeyEx(HKEY_CURRENT_USER,
						 L"SOFTWARE\\freshney.org\\MatrixBuilder",
						 NULL,
						 KEY_READ,
						 &hKey);

	if (dwRet != ERROR_SUCCESS)
	{
		return false;
	}

	// ===========================================================================

	for (int t = 0; t < kPalletCount; t++)
	{
		int x = Registry::ReadInteger(hKey, L"rgbpalettehistory" + std::to_wstring(t), 0);

		RGBPaletteHistory[t]->Brush->Color = TColor(x);
	}

	// ===========================================================================

	RegCloseKey(hKey);

	return true;
}


bool TframePalette::SavePaletteHistory()
{
	// ===========================================================================
	// == Save User Settings =====================================================
	// ===========================================================================

    HKEY hKey;

	LONG dwRet;

	dwRet = RegOpenKeyEx(HKEY_CURRENT_USER,
						 L"SOFTWARE\\freshney.org\\MatrixBuilder",
						 NULL,
						 KEY_WRITE,
						 &hKey);

	if (dwRet != ERROR_SUCCESS)
	{
		return false;
	}

	// ===========================================================================

	for (int t = 0; t < kPalletCount; t++)
	{
		Registry::WriteInteger(hKey, L"rgbpalettehistory" + std::to_wstring(t), RGBPaletteHistory[t]->Brush->Color);
	}

	// ===========================================================================

	RegCloseKey(hKey);

    return true;
}


void __fastcall TframePalette::bClearClick(TObject *Sender)
{
	if (MessageDlg(GLanguageHandler->Text[kAreYouSureYouWantToClearThePalette].c_str(), mtWarning, mbYesNo, 0) == mrYes)
	{
		for (int t = 0; t < kPalletCount; t++)
		{
            RGBPaletteHistory[t]->Brush->Color = clBlack;
		}

        RGBPaletteHistoryIndex = 0;
	}
}
