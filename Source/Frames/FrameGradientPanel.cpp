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

#include <fstream>

#include "ColourUtility.h"
#include "Formatting.h"
#include "FrameGradientPanel.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "FileConstants.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TframeGradient *frameGradient;
//---------------------------------------------------------------------------
__fastcall TframeGradient::TframeGradient(TComponent* Owner)
	: TFrame(Owner)
{
}


void TframeGradient::SetGuiLanguageText()
{
	sbOpenGradient->Caption = GLanguageHandler->Text[kOpen].c_str();
	sbSaveGradient->Caption = GLanguageHandler->Text[kSave].c_str();
	sbClearGradient->Caption = GLanguageHandler->Text[kClear].c_str();

	bFromShades->Caption = GLanguageHandler->Text[kFromShades].c_str();
	bFromCustom->Caption = GLanguageHandler->Text[kFromCustom].c_str();
	sbCopyToBrush->Caption = GLanguageHandler->Text[kCopyBrush].c_str();
}


int TframeGradient::GetColour(int index)
{
	int colour = reinterpret_cast<int>(clbGradient->Items->Objects[index]);

    return colour;
}


void TframeGradient::AddColour(int colour)
{
	clbGradient->AddItem(ColourUtility::RGBPlusInteger(colour, 100).c_str(), (TObject*)colour);
}


int TframeGradient::GetColourCount()
{
	return clbGradient->Items->Count;
}


void __fastcall TframeGradient::eRedKeyPress(TObject *Sender, System::WideChar &Key)
{
	if (Key == 13)
	{
		TEdit *edit = (TEdit*)Sender;

		int value = edit->Text.ToIntDef(999);

		if (value >= 0 && value <= 255)
		{
			switch (edit->Tag)
			{
			case CRed:
				tbRed->Position   = value;
				break;
			case CGreen:
				tbGreen->Position = value;
				break;
			case CBlue:
				tbBlue->Position  = value;
				break;
			}

		  	tbRedChange(nullptr);
		}
	}
}


void __fastcall TframeGradient::clbGradientClick(TObject *Sender)
{
	if (clbGradient->ItemIndex != -1)
	{
		int colour = reinterpret_cast<int>(clbGradient->Items->Objects[clbGradient->ItemIndex]);

		sRGBPaletteColour->Brush->Color = TColor(colour);

		SetSlidersFromColour(sRGBPaletteColour->Brush->Color);
	}
}


void __fastcall TframeGradient::sbOpenGradientClick(TObject *Sender)
{
	if (odGradient->Execute())
	{
		LoadGradient(odGradient->FileName.c_str());
	}
}


void __fastcall TframeGradient::sbSaveGradientClick(TObject *Sender)
{
	if (sdGradient->Execute())
	{
		SaveGradient(sdGradient->FileName.c_str());
	}
}


void __fastcall TframeGradient::sbCopyToBrushClick(TObject *Sender)
{
	if (OnCopy)
	{
		OnCopy();
	}
}


void __fastcall TframeGradient::sbAddColourClick(TObject *Sender)
{
	clbGradient->AddItem(ColourUtility::RGBPlusInteger(sRGBPaletteColour->Brush->Color, 100).c_str(), (TObject*)sRGBPaletteColour->Brush->Color);
}


void __fastcall TframeGradient::sbRemoveColourClick(TObject *Sender)
{
	if (clbGradient->ItemIndex != -1)
	{
		clbGradient->Items->Delete(clbGradient->ItemIndex);
	}
}


void __fastcall TframeGradient::sRGBPaletteColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	if (cdGradient->Execute())
	{
		sRGBPaletteColour->Brush->Color = cdGradient->Color;

		SetSlidersFromColour(cdGradient->Color);
	}
}


void __fastcall TframeGradient::tbRedChange(TObject *Sender)
{
	if (Sender != nullptr)
	{
		TTrackBar *tb = (TTrackBar*)Sender;

		switch (tb->Tag)
		{
		case CRed:
			eRed->Text   = tb->Position;
			break;
		case CGreen:
			eGreen->Text = tb->Position;
			break;
		case CBlue:
			eBlue->Text  = tb->Position;
			break;
		}
	}

	lPaletteColourText->Caption = // LMSSettings.App.HexPrefix +
								  IntToHex(tbRed->Position, 2) +
								  IntToHex(tbGreen->Position, 2) +
								  IntToHex(tbBlue->Position, 2);

	sRGBPaletteColour->Brush->Color = TColor((tbBlue->Position << 16) +
											 (tbGreen->Position << 8) +
											  tbRed->Position);
}


bool TframeGradient::LoadGradient(const std::wstring file_name)
{
	auto ParameterType = [](const std::wstring s) -> enum LoadGradient
	{
		if (s[0] == kDataBlockStart)
		{
			return LoadGradient::kLoadBegin;
		}
		else if (s[0] == kDataBlockEnd)
		{
			return LoadGradient::kLoadEnd;
		}
		else if (s[0] == kGradientColour[0])
		{
			return LoadGradient::kLoadData;
		}
		else
		{
			return LoadGradient::kUnknown;
        }
	};

	std::wifstream file(file_name);

	if (file)
	{
		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					std::wstring v = L"";

					if (s.length() >= 3) v = s.substr(2);

					switch (ParameterType(s))
					{
					case LoadGradient::kLoadData:
					{
						std::wstring colour = L"";

						for (int t = 0; t < v.length(); t++)
						{
							if (v[t] == L' ')
							{
								int col = stoi(colour);

								clbGradient->AddItem(ColourUtility::RGBPlusInteger(col, 100).c_str(), (TObject*)col);

								colour.clear();
							}
							else
							{
								colour += v[t];
							}
						}
						break;
					}
					}
				}
			}
		}

		file.close();

        return true;
	}

	return false;

}


bool TframeGradient::SaveGradient(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kGradientFileHeader + L"\n");

		std::wstring g = L"";

		for (int t = 0; t < clbGradient->Items->Count; t++)
		{
			int colour = reinterpret_cast<int>(clbGradient->Items->Objects[t]);

			g += std::to_wstring(colour) + L" ";
		}

		file << Formatting::to_utf8(kGradientColour + L":" + g + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		file.close();

		return true;
	}

	return false;
}


void TframeGradient::SetSlidersFromColour(int colour)
{
	int r = (colour & 0x0000FF);
	int g = (colour & 0x00FF00) >> 8;
	int b = (colour & 0xFF0000) >> 16;

	tbRed->Position   = r;
	tbGreen->Position = g;
	tbBlue->Position  = b;

	eRed->Text   = r;
	eGreen->Text = g;
	eBlue->Text  = b;
}


void __fastcall TframeGradient::bFromShadesClick(TObject *Sender)
{
	if (OnFromShades)
	{
		clbGradient->Clear();

		OnFromShades();
	}
}


void __fastcall TframeGradient::bFromCustomClick(TObject *Sender)
{
	if (OnFromCustom)
	{
		clbGradient->Clear();

		OnFromCustom();
	}
}


void __fastcall TframeGradient::sbClearGradientClick(TObject *Sender)
{
    clbGradient->Clear();
}
