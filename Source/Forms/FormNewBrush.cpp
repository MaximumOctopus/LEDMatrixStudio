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

#include "CalcUtility.h"
#include "ColourUtility.h"
#include "Convert.h"
#include "FileUtility.h"
#include "Formatting.h"
#include "FormNewBrush.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemSettings.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmNewBrush *frmNewBrush;

//---------------------------------------------------------------------------

NewBrush OpenNewBrush(std::vector<std::wstring> &BrushData, MatrixSettings BrushSettings, RGBPaletteColours PaletteColours)
{
	TfrmNewBrush *frmNewBrush = new TfrmNewBrush(Application);

	frmNewBrush->Settings = BrushSettings;

	NewBrush brush;

	brush.Proceed  = false;

	// =======================================================================

	frmNewBrush->sSelectionLMB->Brush->Color = TColor(PaletteColours.Left);
	frmNewBrush->sSelectionMMB->Brush->Color = TColor(PaletteColours.Middle);
	frmNewBrush->sSelectionRMB->Brush->Color = TColor(PaletteColours.Right);

	for (int t = 0; t < 21; t++)
	{
		frmNewBrush->_RGBPaletteHistory[t]->Brush->Color = TColor(PaletteColours.History[t]);
	}

	// =======================================================================

	if (frmNewBrush->Settings.Mode != MatrixMode::kRGB)
	{
		frmNewBrush->pRGBPalette->Visible = false;
	}

	frmNewBrush->MatrixAutomate->NewMatrix(frmNewBrush->Settings.Mode, 1,
										   4, 4,
										   frmNewBrush->Settings.Width, frmNewBrush->Settings.Height,
										   25, PixelShape::kSquare, true, false, true, 0x00000000);

	if (BrushData.size() != 0)
	{
		for (int t = 0; t < BrushData.size(); t++)
		{
			frmNewBrush->MatrixAutomate->StringToRow(false, BrushData[t], 0, t, 0, false);
		}
	}

	frmNewBrush->MatrixAutomate->SetMouseButtonColours(frmNewBrush->sSelectionLMB->Brush->Color,
													   frmNewBrush->sSelectionMMB->Brush->Color,
													   frmNewBrush->sSelectionRMB->Brush->Color);

	// =======================================================================

	frmNewBrush->MatrixAutomate->LEDColours[0]   = 0x00ffffff;
	frmNewBrush->MatrixAutomate->LEDColours[1]   = 0x00000000;

	frmNewBrush->MatrixAutomate->Render.Draw.Colour = 1;
	frmNewBrush->MatrixAutomate->RGBBackground   = 0x00000000;

	// =======================================================================

	if (frmNewBrush->ShowModal() == mrOk)
	{
		brush.Proceed = true;

		brush.Width   = frmNewBrush->MatrixAutomate->RightBounds() + 1;
		brush.Height  = frmNewBrush->MatrixAutomate->BottomBounds() + 1;

		BrushData.clear();

		for (int t = 0; t < frmNewBrush->Settings.Height; t++)
		{
			BrushData.push_back(frmNewBrush->MatrixAutomate->RowToString(0, t));
		}
	}

	delete frmNewBrush;

	return brush;
}

//---------------------------------------------------------------------------
__fastcall TfrmNewBrush::TfrmNewBrush(TComponent* Owner)
	: TForm(Owner)
{
	RGBPaletteHistoryIndex = 0;

	MatrixAutomate = new TheMatrix(this, pCanvas);
	MatrixAutomate->OnChange           = nullptr;
	MatrixAutomate->OnColourChange     = nullptr;
	MatrixAutomate->OnMouseOver        = nullptr;
	MatrixAutomate->OnPreviewMouseDown = nullptr;

	_RGBPaletteHistory[0]  = sRGBP1;  _RGBPaletteHistory[1]  = sRGBP2;  _RGBPaletteHistory[2]  = sRGBP3;  _RGBPaletteHistory[3]  = sRGBP4;
	_RGBPaletteHistory[4]  = sRGBP5;  _RGBPaletteHistory[5]  = sRGBP6;  _RGBPaletteHistory[6]  = sRGBP7;  _RGBPaletteHistory[7]  = sRGBP8;
	_RGBPaletteHistory[8]  = sRGBP9;  _RGBPaletteHistory[9]  = sRGBP10; _RGBPaletteHistory[10] = sRGBP11; _RGBPaletteHistory[11] = sRGBP12;
	_RGBPaletteHistory[12] = sRGBP13; _RGBPaletteHistory[13] = sRGBP14; _RGBPaletteHistory[14] = sRGBP15; _RGBPaletteHistory[15] = sRGBP16;
	_RGBPaletteHistory[16] = sRGBP17; _RGBPaletteHistory[17] = sRGBP18; _RGBPaletteHistory[18] = sRGBP19; _RGBPaletteHistory[19] = sRGBP20;
	_RGBPaletteHistory[20] = sRGBP21;

	SetGuiLanguageText();
}


void __fastcall TfrmNewBrush::FormDestroy(TObject *Sender)
{
	delete MatrixAutomate;
}


void __fastcall TfrmNewBrush::FormShow(TObject *Sender)
{
	if (MatrixAutomate->Details.Mode != MatrixMode::kRGB)
	{
		clbMain->Enabled    = false;
		bAddColour->Enabled = false;
	}
}


void __fastcall TfrmNewBrush::FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
          int &MaxWidth, int &MaxHeight)
{
	MinHeight = 436;
	MinWidth  = 672;
}


void __fastcall TfrmNewBrush::FormResize(TObject *Sender)
{
	int xc = ClientWidth - 70;
	int yc = ClientHeight - pMain->Height - 25;

	if (pRGBPalette->Visible) xc -= pRGBPalette->Width - 10;
	if (pColours->Visible) xc -= pColours->Width - 10;

	int pxc = std::floor(xc / MatrixAutomate->Details.Width);
	int pyc = std::floor(yc / MatrixAutomate->Details.Height);

	int pixelsize = std::min(pxc, pyc);

	MatrixAutomate->ChangePixelSize(pixelsize);
}


void __fastcall TfrmNewBrush::bAddColourClick(TObject *Sender)
{
	if (cdNewBrush->Execute())
	{
		int color = cdNewBrush->Color;

		clbMain->AddItem(GLanguageHandler->Text[kColour].c_str(), (TObject*)color);
	}
}


void __fastcall TfrmNewBrush::bColourDownClick(TObject *Sender)
{
	if (clbMain->ItemIndex != -1)
	{
		if (clbMain->ItemIndex < clbMain->Items->Count - 1)
		{
			int index = clbMain->ItemIndex;

			clbMain->Items->Move(clbMain->ItemIndex, clbMain->ItemIndex + 1);

			clbMain->ItemIndex = index + 1;
		}
	}
}


void __fastcall TfrmNewBrush::bColourUpClick(TObject *Sender)
{
	if (clbMain->ItemIndex != -1)
	{
		if (clbMain->ItemIndex > 0)
		{
			int index = clbMain->ItemIndex;

			clbMain->Items->Move(clbMain->ItemIndex, clbMain->ItemIndex - 1);

			clbMain->ItemIndex = index - 1;
		}
	}
}


void __fastcall TfrmNewBrush::bDeleteColourClick(TObject *Sender)
{
	clbMain->DeleteSelected();
}


void __fastcall TfrmNewBrush::bOpenColoursClick(TObject *Sender)
{
	odLoadBrush->DefaultExt = L"colours";
	odLoadBrush->Filter     = Utility::WS2US(GLanguageHandler->Text[kColourLists] + L" (*.colours)|*.colours");
	odLoadBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"automate\\colours\\");

	if (odLoadBrush->Execute())
	{
		clbMain->Items->Clear();

		LoadColours(odLoadBrush->FileName.c_str());
	}
}


void __fastcall TfrmNewBrush::bSaveColoursClick(TObject *Sender)
{
	sdSaveBrush->DefaultExt = L"colours";
	sdSaveBrush->Filter     = Utility::WS2US(GLanguageHandler->Text[kColourLists] + L" (*.colours)|*.colours");
	sdSaveBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"automate\\colours\\");

	if (sdSaveBrush->Execute())
	{
		SaveColours(sdSaveBrush->FileName.c_str());
	}
}


void __fastcall TfrmNewBrush::bLoadBrushClick(TObject *Sender)
{
	odLoadBrush->DefaultExt = L"leds";
	odLoadBrush->Filter     = Utility::WS2US(GLanguageHandler->Text[kBrushes] + L" (*.leds)|*.leds");
	odLoadBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"brushes\\");

	if (odLoadBrush->Execute())
	{
		LoadBrush(odLoadBrush->FileName.c_str());

		SetCaption(odLoadBrush->FileName.c_str());
	}
}


void __fastcall TfrmNewBrush::bSaveBrushClick(TObject *Sender)
{
	sdSaveBrush->DefaultExt = L"leds";
	sdSaveBrush->Filter     = Utility::WS2US(GLanguageHandler->Text[kBrushes] + L" (*.leds)|*.leds");
	sdSaveBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"brushes\\");

	if (sdSaveBrush->Execute())
	{
		SaveBrush(sdSaveBrush->FileName.c_str());

		SetCaption(sdSaveBrush->FileName.c_str());
	}
}


void __fastcall TfrmNewBrush::bCreateClick(TObject *Sender)
{
	if (clbMain->Count != 0)
	{
		switch (cbAvailableTypes->ItemIndex)
		{
		case CTGradientHorizontalUp:
		case CTGradientHorizontalDown:
			GradientHorizontal(cbAvailableTypes->ItemIndex);
			break;

		case CTGradientVerticalRight:
		case CTGradientVerticalLeft:
			GradientVertical(cbAvailableTypes->ItemIndex);
			break;

		case CTGradientDiagonalUpRight:
		case CTGradientDiagonalDownRight:
			GradientDiagonal(cbAvailableTypes->ItemIndex);
			break;

		case CTChevronUp:
		case CTChevronDown:
		case CTChevronRight:
		case CTChevronLeft:
			GenerateChevron(cbAvailableTypes->ItemIndex);
			break;

		case CTCheckerboard1x1:
		case CTCheckerboard2x2:
		case CTCheckerboard3x3:
		case CTCheckerboard4x4:
			GenerateCheckerboard(cbAvailableTypes->ItemIndex);
			break;
		}
	}
	else
	{
		MessageDlg(GLanguageHandler->Text[kYouMustAdCcoloursBelowBeforeYouCanGenerateAPattern].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
	}
}


void __fastcall TfrmNewBrush::eRedKeyPress(TObject *Sender, System::WideChar &Key)
{
	if (Key == VK_RETURN)
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


void __fastcall TfrmNewBrush::sRGBPaletteColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	if (Shift.Contains(ssLeft))
	{
		sSelectionLMB->Brush->Color        = shape->Brush->Color;

		MatrixAutomate->LEDRGBColours[1] = shape->Brush->Color;
	}
	else if (Shift.Contains(ssMiddle))
	{
		sSelectionMMB->Brush->Color        = shape->Brush->Color;

		MatrixAutomate->LEDRGBColours[2] = shape->Brush->Color;
	}
	else if (Shift.Contains(ssRight))
	{
		sSelectionRMB->Brush->Color        = shape->Brush->Color;

		MatrixAutomate->LEDRGBColours[3] = shape->Brush->Color;
	}

	MatrixAutomate->SetMouseButtonColours(MatrixAutomate->LEDRGBColours[1],
										MatrixAutomate->LEDRGBColours[2],
										MatrixAutomate->LEDRGBColours[3]);

	if (Sender == sRGBPaletteColour)
	{
		AddToPaletteHistory(shape->Brush->Color);
	}
}


void __fastcall TfrmNewBrush::sRGBPaletteColourMouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	lPixelColour->Caption = L"0x" + IntToHex(ColourUtility::RGBConvertTo32(shape->Brush->Color, RGBMode::kRGB, LeastSignificantBit::kBottomRight, 100), 6);
}


void __fastcall TfrmNewBrush::tbRedChange(TObject *Sender)
{
	if (Sender != nullptr)
	{
		TTrackBar *tb = (TTrackBar*)Sender;

		std::wstring value = std::to_wstring(tb->Position);

		switch (tb->Tag)
		{
		case 0:
			eRed->Text   = value.c_str();
			break;
		case 1:
			eGreen->Text = value.c_str();
			break;
		case 2:
			eBlue->Text  = value.c_str();
			break;
		}
	}

	lPaletteColourText->Caption = //LMSSettings.App.HexPrefix +
								IntToHex(tbRed->Position, 2) +
								IntToHex(tbGreen->Position, 2) +
								IntToHex(tbBlue->Position, 2);

	sRGBPaletteColour->Brush->Color = TColor((tbBlue->Position << 16) +
									  (tbGreen->Position << 8) +
									   tbRed->Position);
}


void __fastcall TfrmNewBrush::sSelectionLMBMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	cdNewBrush->Color = shape->Brush->Color;

	if (cdNewBrush->Execute())
	{
		MatrixAutomate->LEDRGBColours[shape->Tag] = cdNewBrush->Color;

		MatrixAutomate->SetMouseButtonColours(MatrixAutomate->LEDRGBColours[1],
											  MatrixAutomate->LEDRGBColours[2],
											  MatrixAutomate->LEDRGBColours[3]);

		shape->Brush->Color = cdNewBrush->Color;
	}
}


void TfrmNewBrush::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kNewBrush].c_str();

	Label1->Caption = GLanguageHandler->Text[kBuiltInTypes].c_str();
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientHorizontalUp].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientHorizontalDown].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientVerticalRight].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientVerticalLeft].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientDiagonalUpRight].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kGradientDiagonalDownRight].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kChevronUp].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kChevronDown].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kChevronRight].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kChevronLeft].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kCheckerboard1x1].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kCheckerboard2x2].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kCheckerboard3x3].c_str());
	cbAvailableTypes->Items->Add(GLanguageHandler->Text[kCheckerboard4x4].c_str());
	cbAvailableTypes->ItemIndex = 0;

	bCreate->Caption = GLanguageHandler->Text[kCreate].c_str();

	lBrush->Caption = GLanguageHandler->Text[kCustomBrush].c_str();
	bLoadBrush->Caption = GLanguageHandler->Text[kOpen].c_str();
	bSaveBrush->Caption = GLanguageHandler->Text[kSave].c_str();

	lColours->Caption = GLanguageHandler->Text[kColours].c_str();
	bOpenColours->Caption = GLanguageHandler->Text[kOpen].c_str();
	bSaveColours->Caption = GLanguageHandler->Text[kSave].c_str();

	sbSave->Caption = GLanguageHandler->Text[kOK].c_str();
	sbCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


void TfrmNewBrush::SetCaption(const std::wstring path)
{
	Caption = GLanguageHandler->Text[kNewBrush].c_str();

	if (!path.empty())
	{
		std::wstring quotedpath = L" \"" + path + L"\"";

		Caption += Caption + quotedpath.c_str();
	}
}


void TfrmNewBrush::GradientHorizontal(int mode)
{
	int ColourIndex = 0;

	for (int row = 0; row < Settings.Height; row++)
	{
		int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

		for (int column = 0; column < Settings.Width; column++)
		{
			if (mode == CTGradientHorizontalDown)
			{
				MatrixAutomate->PlotPixelMatrix(column, row, colour);
			}
			else
			{
				MatrixAutomate->PlotPixelMatrix(column, Settings.Height - 1 - row, colour);
			}
		}


		CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);
	}

	MatrixAutomate->Refresh();
}


void TfrmNewBrush::GradientVertical(int mode)
{
	int ColourIndex = 0;

	for (int column = 0; column < Settings.Width; column++)
	{
		int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

		for (int row = 0; row < Settings.Height; row++)
		{
			if (mode == CTGradientVerticalRight)
			{
				MatrixAutomate->PlotPixelMatrix(column, row, colour);
			}
			else
			{
				MatrixAutomate->PlotPixelMatrix(Settings.Width - 1 - column, row, colour);
			}
		}

		CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);
	}

	MatrixAutomate->Refresh();
}


void TfrmNewBrush::GradientDiagonal(int mode)
{
	int ColourOffset = 0;

	for (int column = 0; column < Settings.Width; column++)
	{
		int ColourIndex = ColourOffset;

		for (int row = Settings.Height - 1; row >= 0; row--)
		{
			int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

			if (mode == CTGradientDiagonalUpRight)
			{
				MatrixAutomate->PlotPixelMatrix(column, row, colour);
			}
			else
			{
				MatrixAutomate->PlotPixelMatrix(Settings.Width - column - 1, row, colour);
			}

			CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);
		}

		CalcUtility::IncWithRollOver(ColourOffset, clbMain->Items->Count - 1);
	}

	MatrixAutomate->Refresh();
}


void TfrmNewBrush::GenerateChevron(int mode)
{
	int ColourOffset = 0;

	if (mode == CTChevronLeft || mode == CTChevronRight)
	{
		int Height = std::round(Settings.Height / 2);

		for (int row = 0; row < Height; row++)
		{
			int ColourIndex = ColourOffset;

			for (int column = 0; column < Settings.Width; column++)
			{
				int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

				if (mode == CTChevronLeft)
				{
					MatrixAutomate->PlotPixelMatrix(column, row, colour);
					MatrixAutomate->PlotPixelMatrix(column, Settings.Height - 1 - row, colour);
				}
				else
				{
					MatrixAutomate->PlotPixelMatrix(Settings.Width - 1 - column, row, colour);
					MatrixAutomate->PlotPixelMatrix(Settings.Width - 1 - column, Settings.Height - 1 - row, colour);
				}


				CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);
			}

			CalcUtility::IncWithRollOver(ColourOffset, clbMain->Items->Count - 1);
		}
	}
	else
	{
		int Width = std::round(Settings.Width / 2);

		for (int column = 0; column < Width; column++)
		{
			int ColourIndex = ColourOffset;

			for (int row = 0; row < Settings.Height; row++)
			{
				int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

				if (mode == CTChevronUp)
				{
					MatrixAutomate->PlotPixelMatrix(column, row, colour);
					MatrixAutomate->PlotPixelMatrix(Settings.Width - 1 - column, row, colour);
				}
				else
				{
					MatrixAutomate->PlotPixelMatrix(column, Settings.Height - 1 - row, colour);
					MatrixAutomate->PlotPixelMatrix(Settings.Width - 1 - column, Settings.Height - 1 - row, colour);
				}

				CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);
			}

			CalcUtility::IncWithRollOver(ColourOffset, clbMain->Items->Count - 1);
		}
	}

	MatrixAutomate->Refresh();
}


void TfrmNewBrush::GenerateCheckerboard(int mode)
{
	int coeff = 0;

	switch (mode)
	{
	case CTCheckerboard1x1:
		coeff = 1;
		break;
	case CTCheckerboard2x2:
		coeff = 2;
		break;
	case CTCheckerboard3x3:
		coeff = 3;
		break;
	case CTCheckerboard4x4:
		coeff = 4;
		break;

	default:
		coeff = 1;
	}

	int SquareX = 0;
	int SquareY = 0;

	int ColourIndex = 0;
	int ColourIndexOld = 0;

	for (int row = 0; row < Settings.Height; row++)
	{
		for (int column = 0; column < Settings.Width; column++)
		{
			int colour = reinterpret_cast<int>(clbMain->Items->Objects[ColourIndex]);

			MatrixAutomate->PlotPixelMatrix(column, row, colour);

			SquareX++;

			if (SquareX == coeff)
			{
				CalcUtility::IncWithRollOver(ColourIndex, clbMain->Items->Count - 1);

				SquareX = 0;
			}
		}

		SquareX = 0;

		SquareY++;

		if (SquareY == coeff)
		{
			CalcUtility::IncWithRollOver(ColourIndexOld, clbMain->Items->Count - 1);

			ColourIndex = ColourIndexOld;

			SquareY = 0;
		}
		else
		{
			ColourIndex = ColourIndexOld;
		}
	}

	MatrixAutomate->Refresh();
}


void TfrmNewBrush::SaveColours(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int i = 0; i < clbMain->Items->Count; i++)
		{
			int colour = reinterpret_cast<int>(clbMain->Items->Objects[i]);

			file << Formatting::to_utf8(kColoursData + L":" + std::to_wstring(colour) + L"\n");
		}

		file.close();
	}
}


void TfrmNewBrush::LoadColours(const std::wstring file_name)
{
	std::wifstream file(file_name);

	ExportOptions eeo;

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
					int i = 0;

					if (s.find(L"$") != std::wstring::npos)
					{
						i = Convert::HexToInt(s.substr(5)); // col:$
					}
					else
					{
						i = stoi(s.substr(4));    // col:
					}

					clbMain->AddItem(GLanguageHandler->Text[kColour].c_str(), (TObject*)i);
				}
			}
		}

		file.close();
	}
}


void TfrmNewBrush::AddToPaletteHistory(int colour)
{
	bool canadd = false;

	// ensures no duplicates adjacent
	if (RGBPaletteHistoryIndex == 0)
	{
		canadd = !(colour == _RGBPaletteHistory[20]->Brush->Color);
	}
	else
	{
		canadd = !(colour == _RGBPaletteHistory[RGBPaletteHistoryIndex - 1]->Brush->Color);
	}

	if (canadd)
	{
		_RGBPaletteHistory[RGBPaletteHistoryIndex]->Brush->Color = TColor(colour);

		if (RGBPaletteHistoryIndex == 20)
		{
			RGBPaletteHistoryIndex = 0;
		}
		else
		{
			RGBPaletteHistoryIndex++;
		}
	}
}


void TfrmNewBrush::LoadBrush(const std::wstring file_name)
{
	std::wifstream file(file_name);

	if (file)
	{
		clbMain->Clear();

		// ===========================================================================
		// ===========================================================================

		int row = 0;
		bool headermode = false;
		bool coloursmode = false;
		bool matrixmode = false;

		MatrixMode ImportMode = MatrixMode::kMono;

		bool ValidMatrix = true;

		int tempMaxWidth = -1;
		int tempMaxHeight = -1;

		// ===========================================================================
		// ===========================================================================

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
					std::wstring v = s.substr(2);

					std::transform(s.begin(), s.end(), s.begin(), ::tolower);

					switch (FileUtility::LoadDataParameterType(s, headermode, matrixmode, false, false, coloursmode))
					{
					case LoadData::kLoadBlockStartHeader:
						headermode = true;
						break;
					case LoadData::kLoadBlockBegin:
						row = 0;

						switch (v[v.length() - 1])
						{
						case L'2':
							ImportMode = MatrixMode::kBiSequential;
							break;
						case L'3':
							ImportMode = MatrixMode::kBiBitplanes;
							break;
						case L'4':
							ImportMode = MatrixMode::kRGB;
							break;
						case L'5':
							ImportMode = MatrixMode::kRGB3BPP;
							break;

						default:
							ImportMode = MatrixMode::kMono;
						}

						if (ImportMode != MatrixAutomate->Details.Mode)
						{
							ValidMatrix = false;
						}

						headermode = false;
						matrixmode = true;
						break;
					case LoadData::kLoadBlockEnd:
						break;

					case LoadData::kLoadBlockStartColours:
						coloursmode = true;
						headermode  = false;
						matrixmode  = false;
						break;

					case LoadData::kLoadColoursCustom:
						if (coloursmode)
						{
							int colour = stoi(v);

							clbMain->AddItem(GLanguageHandler->Text[kColour].c_str(), (TObject*)colour);
						}
						break;

					// ======================================================================

					//30 : MatrixComment           = v;
					//31 : lRGBBackground          = stoi(v);
					case LoadData::kLoadHeaderEnd:
                        break;

					// ======================================================================

					case LoadData::kLoadMatrixWidth:
						tempMaxWidth  = stoi(v);
						break;
					case LoadData::kLoadMatrixHeight:
						tempMaxHeight = stoi(v);
						break;
					case LoadData::kLoadMatrixData:
						if (ValidMatrix)
						{
							MatrixAutomate->StringToRow(false, v, 1, row, 0, false);

							row++;
						}
						break;
					}
				}
			}
		}

		file.close();

		if (!ValidMatrix)
		{
			MessageDlg(GLanguageHandler->Text[kBrushDoesNotMatchCurrentMatrixType].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
		}
	}

	//  Result.MatrixMode = MatrixMode;
	//  Result.NewWidth   = tempMaxWidth;
	//  Result.NewHeight  = tempMaxHeight;
	//  Result.MaxFrames  = Matrix.Count - 1;
	//  Result.FontMode   = fontmode;
}


void TfrmNewBrush::SaveBrush(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderHeader + L"\n");
		//writeln(tf, 'x:' + MatrixComment);
		//writeln(tf, 'z:' + IntToStr(RGBBackground));
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===================================================================

		switch (MatrixAutomate->Details.Mode)
		{
		case MatrixMode::kMono:
			file << Formatting::to_utf8(L"{" + kBrushPrefixMono + L"\n");
			break;
		case MatrixMode::kBiSequential:
			file << Formatting::to_utf8(L"{" + kBrushPrefixBiSequential + L"\n");
			break;
		case MatrixMode::kBiBitplanes:
			file << Formatting::to_utf8(L"{" + kBrushPrefixBiBitPlanes + L"\n");
			break;
		case MatrixMode::kRGB:
			file << Formatting::to_utf8(L"{" + kBrushPrefixRGB + L"\n");
			break;
		case MatrixMode::kRGB3BPP:
			file << Formatting::to_utf8(L"{" + kBrushPrefixRGB3BPP + L"\n");
			break;
		}

		file << Formatting::to_utf8(kAnimWidthF  + std::to_wstring(MatrixAutomate->Details.Width) + L"\n");
		file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(MatrixAutomate->Details.Height) + L"\n");

		for (int y = 0; y < MatrixAutomate->Details.Height; y++)
		{
			file << Formatting::to_utf8(kAnimRowDataF + MatrixAutomate->RowToString(1, y) + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderColours + L"\n");

		for (int i = 0; i < clbMain->Count; i++)
		{
			int colour = reinterpret_cast<int>(clbMain->Items->Objects[i]);

			file << Formatting::to_utf8(kAnimBrushColoursF + std::to_wstring(colour) + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file.close();
	}
}
