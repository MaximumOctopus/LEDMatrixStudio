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

#include "FrameFontPanel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TframeFont *frameFont;

__fastcall TframeFont::TframeFont(TComponent* Owner)
	: TFrame(Owner)
{
}


void __fastcall TframeFont::lFontClick(TObject *Sender)
{
	if (fdMain->Execute())
	{
		lFont->Font = fdMain->Font;

		sbChangeFont->Caption = fdMain->Font->Name;
	}
}


void TframeFont::SetFontAscii(int ascii)
{
	FontAscii = ascii;

	lFont->Caption = Char(ascii);
}
