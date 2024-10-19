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

#include "Convert.h"
#include "FileUtility.h"
#include "FormExport.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Optimisation.h"
#include "PresetHandler.h"
#include "ProfileHandler.h"
#include "SystemSettings.h"
#include "Utility.h"

#include "ExportMonoBi.h"
#include "ExportOutputBinary.h"
#include "ExportRGB.h"
#include "ExportRGB3pp.h"
#include "ExportUtility.h"

#include "Example_FastLED.h"

extern LanguageHandler *GLanguageHandler;
extern PresetHandler *GPresetHandler;
extern ProfileHandler *GProfileHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmExport *frmExport;

void OpenExportData(TheMatrix *thematrix, ExportOptions &ieo, ExportSource source, MatrixMode mode) // mode = 0 (animation), 1 = (user memories)
{
	TfrmExport *frmExport = new TfrmExport(Application);

	frmExport->matrix = thematrix;

	frmExport->Mode = mode;

	frmExport->InternalEO.ExportMode   = source;

	if (frmExport->InternalEO.ExportMode == ExportSource::kNone)
	{
		frmExport->InternalEO.ExportMode = ExportSource::kAnimation;

		ieo.ExportMode = ExportSource::kAnimation;
	}

	frmExport->InternalEO.Code.IncludePreamble = true;

	frmExport->BuildUI(ieo);

	if (frmExport->ShowModal() == mrOk)
	{
		ieo.Valid = true;

		ieo = frmExport->InternalEO;
	}
	else
	{
		ieo.Valid = false;
	}

	delete frmExport;
}


//---------------------------------------------------------------------------
__fastcall TfrmExport::TfrmExport(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();

	IsBuilding = true;
	IsUpdating = false;
	LastScrollValue = 0;

	if (InternalEO.ExportMode == ExportSource::kUserMemories)
	{
		lFrame->Caption = L"User Memories";
	}

	cbAutoPreview->Checked = false;

//	cbDirectionChange(nullptr);
//	sbBinaryDataRowsClick(nullptr);

	cbLanguageFormat->ItemIndex = 0;

	cbLineCount->Items->Add(L"8");
	cbLineCount->Items->Add(L"10");
	cbLineCount->Items->Add(L"16");
	cbLineCount->Items->Add(L"20");
	cbLineCount->Items->Add(L"32");
	cbLineCount->Items->Add(L"40");
	cbLineCount->Items->Add(L"50");
	cbLineCount->Items->Add(L"64");
	cbLineCount->Items->Add(L"100");
	cbLineCount->Items->Add(L"128");
	cbLineCount->Items->Add(L"256");
	cbLineCount->Items->Add(L"512");

	cbLineCount->ItemIndex = 1;

	cbAutoPreview->Checked = true;
}


void __fastcall TfrmExport::reExportMouseWheelDown(TObject *Sender, TShiftState Shift,
          TPoint &MousePos, bool &Handled)
{
	if (!IsUpdating)
	{
		TRect rect;

		reExport->Perform(EM_GETRECT, 0, (LPARAM)&rect);
		rect.Left = rect.Left + 1;
		rect.Top  = rect.Bottom - 2;

		TPoint topleft = rect.TopLeft();

		int LastLineIndex   = reExport->Perform(EM_CHARFROMPOS, 0, (LPARAM)&topleft);
		int LastVisibleLine = reExport->Perform(EM_EXLINEFROMCHAR, 0, (LPARAM)LastLineIndex);

		if (LastScrollValue != LastVisibleLine && LastVisibleLine >= reExport->Lines->Count)
		{
			AddPreviewSection();
		}

		LastScrollValue = LastVisibleLine;
	}
}


void __fastcall TfrmExport::reExportMouseWheelUp(TObject *Sender, TShiftState Shift,
          TPoint &MousePos, bool &Handled)
{
	LastScrollValue = 0;
}


void TfrmExport::BuildUI(ExportOptions ieo)
{
	switch (Mode)
	{
	case MatrixMode::kNone:
		break;
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		ProfileExtension = L"ledsexport";
		gbNumberGrouping->Top = gbNumberGroupingRGB->Top;
		break;
	case MatrixMode::kRGB:
		gbNumberGrouping->Visible = false;
		gbNumberGroupingBinary->Visible = false;

		gbRGB->Visible = true;
		gbBinaryRGB->Visible = true;

		gbNumberGroupingRGB->Visible = true;
		gbNumberGroupingBinaryRGB->Visible = true;

		gbRGBColourSpace->Visible = true;
		gbRGBColourSpace->Top = 415;

		gbBinaryColourSpaceRGB->Visible = true;
		gbBinaryColourSpaceRGB->Top = 415;

		ProfileExtension = L"ledsexportrgb";
		break;
	case MatrixMode::kRGB3BPP:
		gbNumberGrouping->Visible = false;
		gbNumberGroupingBinary->Visible = false;

		gbRGB->Visible = false;
		gbBinaryRGB->Visible = false;

		gbNumberGroupingRGB->Visible = false;

		ProfileExtension = L"ledsexportrgb3bpp";
		break;
	}

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		SetMaxFrameCount(matrix->GetFrameCount()); 	// anim
	}
	else
	{
		SetMaxFrameCount(10);						// user memories
	}

	eSelectiveStart->Text = L"1";
	eSelectiveEnd->Text = matrix->Details.Height;

	eBinarySelectiveStart->Text = L"1";
	eBinarySelectiveEnd->Text = matrix->Details.Height;

	if (ieo.ExportMode != ExportSource::kNone)
	{
		BuildFromProfile(ieo);
	}
	else
	{
        sbBinaryDataRowsClick(nullptr);
	}

	cbDirectionChange(nullptr);         // these must be executed after the matrix has been assigned (or crash!!)
}


void TfrmExport::BuildFromProfile(ExportOptions eeo)
{
	if (eeo.Code.Source == ReadSource::kRows)
	{
		sbDataRows->Down = true;
	}
	else
	{
		sbDataColumns->Down = true;
	}

	cbDirectionChange(nullptr);

	cbOptimise->Checked = eeo.Optimise;

	// ======================================================================

	if (eeo.Code.LSB == LeastSignificantBit::kTopLeft)
	{
		sbLSBLeft->Down  = true;
	}
	else
	{
		sbLSBRight->Down = true;
	}

	// ===========================================================================

	cbLanguageFormat->ItemIndex = eeo.LanguageToInt();

	cbIncludeExample->Checked   = eeo.Examples;

	// ===========================================================================

	if (gbNumberFormat->Visible)
	{
		switch (eeo.Code.Format)
		{
		case NumberFormat::kDecimal:
			sbNumberDecimal->Down = true;
			break;
		case NumberFormat::kBinary:
			sbNumberBinary->Down = true;
			break;
		case NumberFormat::kHex:
			sbNumberHex->Down = true;
			break;
		}
	}

	// ===========================================================================

	if (gbNumberGrouping->Visible)
	{
		switch (eeo.Code.Size)
		{
		case NumberSize::k8Bit:
			sbNumberSize8bit->Down      = true;
			break;
		case NumberSize::k16bit:
			sbNumberSize16bit->Down     = true;
			break;
		case NumberSize::k32bit:
			sbNumberSize32bit->Down     = true;
			break;
		case NumberSize::k8bitSwap:
			sbNumberSize8bitSwap->Down  = true;
			break;
		case NumberSize::k16bitSwap:
			sbNumberSize16bitSwap->Down = true;
			break;
		}

		sbNumberSize8bitClick(nullptr);
	}
	else
	{
		switch (eeo.Code.Size)
		{
		case NumberSize::kRGB8bit:
			sbNumberSizeRGB8bits->Down = true;
			break;
		case NumberSize::kRGB16bit:
		case NumberSize::kRGB32bit:
			sbNumberSizeRGB32bits->Down = true;
			break;
		}

		sbNumberSize8bitClick(nullptr);
	}

	// ===========================================================================

	switch (eeo.Code.Content)
	{
	case LineContent::kRowCol:
		sbOutputRow->Down = true;
		break;
	case LineContent::kFrame:
		sbOutputFrame->Down = true;
		break;
	case LineContent::kBytes:
		sbOutputBytes->Down = true;
		break;
	}

	cbDirectionChange(nullptr);

	// ===========================================================================

	cbLineCount->Text = std::to_wstring(eeo.Code.LineCount).c_str();

	// ===========================================================================

	if (gbRGB->Visible)
	{
		switch (eeo.Code.RGBFormat)
		{
		case RGBMode::kRGB:
			sbRGB->Down = true;
			break;
		case RGBMode::kBGR:
			sbBGR->Down = true;
			break;
		case RGBMode::kGRB:
			sbGRB->Down = true;
			break;
		case RGBMode::kBRG:
			sbBRG->Down = true;
			break;
		}

		cbChangeBackgroundPixels->Checked  = eeo.Code.RGBChangePixels;
		shapeBackgroundPixels->Brush->Color = TColor(eeo.Code.RGBChangeColour);

		if (eeo.Code.RGBBrightness > 100)
		{
			eeo.Code.RGBBrightness = 100;
		}

		groupBoxRGBBrightness->Text = eeo.Code.RGBBrightness;

		switch (eeo.Code.ColourSpaceRGB)
		{
		case ColourSpace::kRGB32:
			sbCSRGB32->Down = true;
			break;
		case ColourSpace::kRGB565:
			sbCSRGB565->Down = true;
			break;
		}
	}

	// ===========================================================================

	cbDirection->ItemIndex = eeo.OrientationToInt();

	// ===========================================================================

	cbScanDirection->ItemIndex = eeo.ScanDirectionToInt();

	// ===========================================================================
	// binary options
	// ===========================================================================

	if (eeo.Binary.Source == ReadSource::kRows)
	{
		sbBinaryDataRows->Down = true;
	}
	else
	{
		sbBinaryDataColumns->Down = true;
	}

	sbBinaryDataRowsClick(nullptr);

	// ===========================================================================

	cbBinaryDirection->ItemIndex = eeo.BinaryOrientationToInt();

	// ===========================================================================

	cbBinaryScanDirection->ItemIndex = eeo.BinaryScanDirectionToInt();

	// ===========================================================================

	if (eeo.Binary.LSB == LeastSignificantBit::kTopLeft)
	{
		sbBinaryLSBLeft->Down = true;
	}
	else
	{
		sbBinaryLSBRight->Down = true;
	}

	// ===========================================================================

	switch (eeo.Binary.Size)
	{
	case NumberSize::k8Bit:
		sbBinaryNumberSize8bit->Down = true;
		break;
	case NumberSize::k8bitSwap:
		sbBinaryNumberSize8bitSwap->Down = true;
		break;
	case NumberSize::k16bitSwap:
		sbBinaryNumberSize16bitSwap->Down = true;
		break;
	case NumberSize::kRGB8bit:
		break;
	}

	// ===========================================================================

	switch (eeo.Binary.RGBFormat)
	{
	case RGBMode::kRGB:
		sbBinaryRGB->Down = true;
		break;
	case RGBMode::kBGR:
		sbBinaryBGR->Down = true;
		break;
	case RGBMode::kGRB:
		sbBinaryGRB->Down = true;
		break;
	case RGBMode::kBRG:
		sbBinaryBRG->Down = true;
		break;
	}

	cbBinaryChangeBackgroundPixels->Checked  = eeo.Binary.RGBChangePixels;
	shapeBinaryBackgroundPixels->Brush->Color = TColor(eeo.Binary.RGBChangeColour);

	groupBoxBinaryRGBBrightness->Text = std::to_wstring(eeo.Binary.RGBBrightness).c_str();

	if (eeo.Binary.ColourSpaceRGB == ColourSpace::kRGB32)
	{
		sbBCSRGB32->Down = true;
	}
	else
	{
		sbBCSRGB565->Down = true;
	}

	// =======================================================================

	if (eeo.Binary.Content == BinaryFileContents::kEntireAnimation)
	{
		rbSaveAnimation->Checked = true;
	}
	else
	{
        rbSaveFrame->Checked = true;
	}

	// =======================================================================

	if (cbAutoPreview->Checked && !IsBuilding)
	{
		Preview();
	}
}


void TfrmExport::CreateExportOptions()
{
	if (cbOptimise->Checked)
	{
		InternalEO.Code.IncludePreamble = false;
		InternalEO.Code.CleanMode       = true;
	}
	else
	{
		InternalEO.Code.IncludePreamble = true;
		InternalEO.Code.CleanMode       = false;
	}

	// =======================================================================

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		InternalEO.Code.StartFrame = eFrameStart->Text.ToInt() - 1;
		InternalEO.Code.EndFrame   = eFrameEnd->Text.ToInt() - 1;
	}
	else
	{
		InternalEO.Code.StartFrame = eFrameStart->Text.ToInt() - 1;
		InternalEO.Code.EndFrame   = eFrameEnd->Text.ToInt() - 1;
	}

	// =======================================================================

	int se = 0;
	int ss = eSelectiveStart->Text.ToIntDef(1);

	if (sbDataRows->Down)
	{
		se = eSelectiveEnd->Text.ToIntDef(matrix->Details.Height);

		if (se < 1 || se > matrix->Details.Height)
		{
			se = matrix->Details.Height;
		}
	}
	else
	{
		se = eSelectiveEnd->Text.ToIntDef(matrix->Details.Width);

		if (se < 1 || se > matrix->Details.Width)
		{
			se = matrix->Details.Width;
		}
	}

	InternalEO.Code.SelectiveStart = ss;
	InternalEO.Code.SelectiveEnd   = se;

	// =======================================================================

	if (sbDataRows->Down)
	{
		InternalEO.Code.Source = ReadSource::kRows;
	}
	else
	{
		InternalEO.Code.Source = ReadSource::kColumns;
	}

	// =======================================================================

	InternalEO.OrientationFromInt(cbDirection->ItemIndex);

	// =======================================================================

	InternalEO.ScanDirectionFromInt(InternalEO.Code.Source, cbScanDirection->ItemIndex);

	// =======================================================================

	if (sbLSBLeft->Down)
	{
		InternalEO.Code.LSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.Code.LSB = LeastSignificantBit::kBottomRight;
	}

	// =======================================================================

	InternalEO.LanguageFromInt(cbLanguageFormat->ItemIndex);

	InternalEO.Examples = cbIncludeExample->Checked;

	// =======================================================================

	if (gbNumberFormat->Visible)
	{
		if (sbNumberDecimal->Down)
		{
			InternalEO.Code.Format = NumberFormat::kDecimal;
		}
		else if (sbNumberBinary->Down)
		{
			InternalEO.Code.Format = NumberFormat::kBinary;
		}
		else
		{
			InternalEO.Code.Format = NumberFormat::kHex;
		}
	}
	else
	{
		InternalEO.Code.Format = NumberFormat::kHex;
	}

	// =======================================================================

	if (gbNumberGrouping->Visible)
	{
		if (sbNumberSize8bit->Down)
		{
			InternalEO.Code.Size = NumberSize::k8Bit;
		}
		else if (sbNumberSize16bit->Down)
		{
			InternalEO.Code.Size = NumberSize::k16bit;
		}
		else if (sbNumberSize32bit->Down)
		{
			InternalEO.Code.Size = NumberSize::k32bit;
		}
		else if (sbNumberSize8bitSwap->Down)
		{
			InternalEO.Code.Size = NumberSize::k8bitSwap;
		}
		else if (sbNumberSize16bitSwap->Down)
		{
			InternalEO.Code.Size = NumberSize::k16bitSwap;
		}
	}
	else
	{
		if (sbNumberSizeRGB8bits->Down)
		{
			InternalEO.Code.Size = NumberSize::kRGB8bit;
		}
		else if (sbNumberSizeRGB32bits->Down)
		{
			InternalEO.Code.Size = NumberSize::kRGB32bit;
		}
		else
		{
			InternalEO.Code.Size = NumberSize::kRGB32bit;
		}
	}

	// =======================================================================

	if (sbOutputRow->Down)
	{
		InternalEO.Code.Content = LineContent::kRowCol;
	}
	else if (sbOutputFrame->Down)
	{
		InternalEO.Code.Content = LineContent::kFrame;
	}
	else if (sbOutputBytes->Down)
	{
		InternalEO.Code.Content = LineContent::kBytes;
	}

	// =======================================================================

	if (gbRGB->Visible)
	{
		InternalEO.Code.RGBEnabled = true;

		if (sbRGB->Down)
		{
			InternalEO.Code.RGBFormat = RGBMode::kRGB;
		}
		else if (sbBGR->Down)
		{
			InternalEO.Code.RGBFormat = RGBMode::kBGR;
		}
		else if (sbGRB->Down)
		{
			InternalEO.Code.RGBFormat = RGBMode::kGRB;
		}
		else if (sbBRG->Down)
		{
			InternalEO.Code.RGBFormat  = RGBMode::kBRG;
		}

		InternalEO.Code.RGBChangePixels = cbChangeBackgroundPixels->Checked;
		InternalEO.Code.RGBChangeColour = shapeBackgroundPixels->Brush->Color;

		InternalEO.Code.RGBBrightness   = groupBoxRGBBrightness->Text.ToIntDef(100);

		if (sbCSRGB32->Down)
		{
			InternalEO.Code.ColourSpaceRGB = ColourSpace::kRGB32;
			//InternalEO.Code.Size     = NumberSize::kRGB32bit;
		}
		else
		{
			InternalEO.Code.ColourSpaceRGB = ColourSpace::kRGB565;
			InternalEO.Code.Size     = NumberSize::kRGB16bit;
		}
	}
	else
	{
		InternalEO.Code.RGBEnabled = false;
	}

	// =======================================================================

	InternalEO.Code.LineCount = cbLineCount->Text.ToInt();

	// =======================================================================
	//   binary file specific options
	// =======================================================================

	if (sbBinaryDataRows->Down)
	{
		InternalEO.Binary.Source = ReadSource::kRows;
	}
	else
	{
		InternalEO.Binary.Source = ReadSource::kColumns;
	}

	// =======================================================================

	InternalEO.BinaryOrientationFromInt(cbBinaryDirection->ItemIndex);

	// =======================================================================

	InternalEO.BinaryScanDirectionFromInt(InternalEO.Binary.Source, cbBinaryScanDirection->ItemIndex);

	// =======================================================================

	if (sbBinaryLSBLeft->Down)
	{
		InternalEO.Binary.LSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.Binary.LSB = LeastSignificantBit::kBottomRight;
	}

	// =======================================================================

	if (gbBinaryRGB->Visible)
	{
		if (sbBinaryRGB->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kRGB;
		}
		else if (sbBinaryBGR->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kBGR;
		}
		else if (sbBinaryGRB->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kGRB;
		}
		else if (sbBinaryBRG->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kBRG;
		}

		InternalEO.Binary.RGBChangePixels = cbBinaryChangeBackgroundPixels->Checked;
		InternalEO.Binary.RGBChangeColour = shapeBinaryBackgroundPixels->Brush->Color;

		InternalEO.Binary.RGBBrightness   = groupBoxBinaryRGBBrightness->Text.ToIntDef(100);

		if (sbBCSRGB32->Down)
		{
			InternalEO.Binary.ColourSpaceRGB = ColourSpace::kRGB32;
		}
		else
		{
			InternalEO.Binary.ColourSpaceRGB = ColourSpace::kRGB565;
		}
	}
}


void TfrmExport::CreateBinaryExportOptions()
{
	//  eeo.Language     := -1; // none
	InternalEO.Binary.Content = BinaryFileContents::kEntireAnimation;  // process in frames
	InternalEO.Binary.Format = NumberFormat::kHex;   // always in hex format

	// =========================================================================

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		InternalEO.Binary.StartFrame = eBinaryFrameStart->Text.ToInt() - 1;
		InternalEO.Binary.EndFrame   = eBinaryFrameEnd->Text.ToInt() - 1;
	}
	else
	{
		InternalEO.Binary.StartFrame = eBinaryFrameStart->Text.ToInt() - 1;
		InternalEO.Binary.EndFrame   = eBinaryFrameEnd->Text.ToInt() - 1;
	}

	// =========================================================================

	int se = 0;
	int ss = eBinarySelectiveStart->Text.ToIntDef(1);

	if (sbDataRows->Down)
	{
		se = eBinarySelectiveEnd->Text.ToIntDef(matrix->Details.Height);

		if (se < 1 || se > matrix->Details.Height)
		{
			se = matrix->Details.Height;
		}
	}
	else
	{
		se = eBinarySelectiveEnd->Text.ToIntDef(matrix->Details.Width);

		if (se < 1 || se > matrix->Details.Width)
		{
			se = matrix->Details.Width;
		}
	}

	InternalEO.Binary.SelectiveStart = ss;
	InternalEO.Binary.SelectiveEnd   = se;

	// =========================================================================

	if (sbBinaryDataRows->Down)
	{
		InternalEO.Binary.Source = ReadSource::kRows;
	}
	else
	{
		InternalEO.Binary.Source = ReadSource::kColumns;
	}

	// =========================================================================

	InternalEO.BinaryOrientationFromInt(cbBinaryDirection->ItemIndex);

	// =========================================================================

	InternalEO.BinaryScanDirectionFromInt(InternalEO.Binary.Source, cbBinaryScanDirection->ItemIndex);

	// =========================================================================

	if (sbBinaryLSBLeft->Down)
	{
		InternalEO.Binary.LSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.Binary.LSB = LeastSignificantBit::kBottomRight;
	}

	// =========================================================================

	if (gbNumberGroupingBinary->Visible)
	{
		if (sbBinaryNumberSize8bit->Down)
		{
			InternalEO.Binary.Size = NumberSize::k8Bit;
		}
		else if (sbBinaryNumberSize8bitSwap->Down)
		{
			InternalEO.Binary.Size = NumberSize::k8bitSwap;
		}
		else if (sbBinaryNumberSize16bitSwap->Down)
		{
			InternalEO.Binary.Size = NumberSize::k16bitSwap;
		}
		else
		{
			InternalEO.Binary.Size = NumberSize::kRGB8bit;
		}
	}

	if (gbNumberGroupingBinaryRGB->Visible)
	{
        InternalEO.Binary.Size = NumberSize::kRGB8bit;
	}

	// =========================================================================

	if (gbBinaryRGB->Visible)
	{
		InternalEO.Binary.RGBEnabled = true;

		if (sbBinaryRGB->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kRGB;
		}
		else if (sbBinaryBGR->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kBGR;
		}
		else if (sbBinaryGRB->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kGRB;
		}
		else if (sbBinaryBRG->Down)
		{
			InternalEO.Binary.RGBFormat = RGBMode::kBRG;
		}

		InternalEO.Binary.RGBChangePixels = cbBinaryChangeBackgroundPixels->Checked;
		InternalEO.Binary.RGBChangeColour = shapeBinaryBackgroundPixels->Brush->Color;

		InternalEO.Binary.RGBBrightness   = groupBoxBinaryRGBBrightness->Text.ToIntDef(100);

		if (sbBCSRGB32->Down)
		{
			InternalEO.Binary.ColourSpaceRGB = ColourSpace::kRGB32;
		}
		else
		{
			InternalEO.Binary.ColourSpaceRGB = ColourSpace::kRGB565;
		}
	}
	else
	{
		InternalEO.Binary.RGBEnabled = false;
	}

	// =======================================================================

	if (rbSaveAnimation->Checked)
	{
		InternalEO.Binary.Content = BinaryFileContents::kEntireAnimation;
	}
	else
	{
		InternalEO.Binary.Content = BinaryFileContents::kSingleFrame;
	}
}


void TfrmExport::ToggleControlStatus(bool status)
{
	pcExport->Enabled         = status;

	sbOpen->Enabled           = status;
	cbProfileList->Enabled    = status;
	sbSave->Enabled           = status;

	bBuildCode->Enabled       = status;
	cbAutoPreview->Enabled    = status;

	bExport->Enabled          = status;
	bCopyToClipboard->Enabled = status;

	bClose->Enabled           = status;
	bCancel->Enabled          = status;
}


void TfrmExport::SetMaxFrameCount(int count)
{
    MaxFrameCount = count;
}


#pragma region Box_RGB
void __fastcall TfrmExport::sbRGBClick(TObject *Sender)
{
	if (cbAutoPreview->Checked && !IsBuilding)
	{
		Preview();
	}
}


void __fastcall TfrmExport::shapeBackgroundPixelsMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (cdExport->Execute())
	{
		TShape *shape = (TShape*)Sender;

		shape->Brush->Color = cdExport->Color;

		if (cbAutoPreview->Checked && !IsBuilding)
		{
			Preview();
		}
	}
}
#pragma end_region


#pragma region Box_NumberGrouping
void __fastcall TfrmExport::sbNumberSize8bitClick(TObject *Sender)
{
	if (sbNumberSize8bit->Down)
	{
		sbOutputBytes->Caption = L"Bytes";
	}
	else if (sbNumberSize16bit->Down)
	{
		sbOutputBytes->Caption = L"Words";
	}
	else if (sbNumberSize32bit->Down)
	{
		sbOutputBytes->Caption = L"LWords";
	}
	else if (sbNumberSize8bitSwap->Down)
	{
		sbOutputBytes->Caption = L"Bytes";
	}
	else if (sbNumberSize16bitSwap->Down)
	{
		sbOutputBytes->Caption = L"Words";
	}

	if (cbAutoPreview->Checked && !IsBuilding)
	{
		Preview();
	}
}
#pragma end_region


#pragma region Box_Source
void __fastcall TfrmExport::cbDirectionChange(TObject *Sender)
{
	int oldIndexD = cbDirection->ItemIndex;
	int oldIndexS = cbScanDirection->ItemIndex;

	cbDirection->Clear();
	cbScanDirection->Clear();

	if (sbDataRows->Down)
	{
		cbDirection->Items->Add(GLanguageHandler->Text[kTopToBottom].c_str());
		cbDirection->Items->Add(GLanguageHandler->Text[kBottomToTop].c_str());

		cbScanDirection->Items->Add(GLanguageHandler->Text[kLeftToRight].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kRightToLeft].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kAlternateLeftRight].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kAlternateRightLeft].c_str());

		sbLSBLeft->Caption        = GLanguageHandler->Text[kLeft].c_str();
		sbLSBRight->Caption       = GLanguageHandler->Text[kRight].c_str();

		sbOutputRow->Caption      = GLanguageHandler->Text[kRow].c_str();

		lSelectiveOutput->Caption = GLanguageHandler->Text[kRowxs].c_str();

		eSelectiveEnd->Text       = std::to_wstring(matrix->Details.Height).c_str();
	}
	else
	{
		cbDirection->Items->Add(GLanguageHandler->Text[kLeftToRight].c_str());
		cbDirection->Items->Add(GLanguageHandler->Text[kRightToLeft].c_str());
		cbDirection->Items->Add(GLanguageHandler->Text[kSure24x16].c_str());

		cbScanDirection->Items->Add(GLanguageHandler->Text[kTopToBottom].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kBottomToTop].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kAlternateDownUp].c_str());
		cbScanDirection->Items->Add(GLanguageHandler->Text[kAlternateUpDown].c_str());

		sbLSBLeft->Caption        = GLanguageHandler->Text[kTop].c_str();
		sbLSBRight->Caption       = GLanguageHandler->Text[kBottom].c_str();

		sbOutputRow->Caption      = GLanguageHandler->Text[kColumn].c_str();

		lSelectiveOutput->Caption = GLanguageHandler->Text[kColumnxs].c_str();

		eSelectiveEnd->Text       = std::to_wstring(matrix->Details.Width).c_str();
	}

	if (Sender != nullptr)
	{
		TSpeedButton *speed = (TSpeedButton*)Sender;

		if (speed->Tag == 1)
		{
			cbDirection->ItemIndex     = oldIndexD;
			cbScanDirection->ItemIndex = oldIndexS;
		}
		else
		{
			cbDirection->ItemIndex     = 0;
			cbScanDirection->ItemIndex = 0;
		}
	}
	else
	{
		cbDirection->ItemIndex     = 0;
		cbScanDirection->ItemIndex = 0;
	}

	if (cbAutoPreview->Checked && !IsBuilding)
	{
		Preview();
	}
}
#pragma end_region


#pragma region Generic
void __fastcall TfrmExport::cbOptimiseClick(TObject *Sender)
{
	if (cbAutoPreview->Checked && !IsBuilding)
	{
		Preview();
	}
}
#pragma end_region


void TfrmExport::AddPreviewSection()
{
	std::wstring s = Caption.c_str();

	Caption += Caption + L" :: updating preview :: ";
	pPreviewStatus->Caption = L"Updating...";

	reExport->Lines->BeginUpdate();

	int row = LastRow;
	int x = 0;

	while (x < GSystemSettings->App.ExportPreviewSize && row + x < Output.size())
	{
		reExport->Lines->Add(Output[row + x].c_str());

		x++;
	}

	LastRow = row + x;

	std::wstring caption = std::to_wstring(LastRow) + L" of " + std::to_wstring(Output.size());

	pPreviewStatus->Caption = caption.c_str();

	reExport->Lines->EndUpdate();

	Caption = s.c_str();
}


void TfrmExport::PreviewBinary()
{
	if (!ValidateNumberEdit(eBinaryFrameStart) || !ValidateNumberEdit(eBinaryFrameEnd)) return;

	CreateBinaryExportOptions();

	int endframelimit = 0;
    int entrycount = 0;

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		endframelimit = matrix->GetFrameCount();
	}
	else
	{
		endframelimit = 9;
	}

	if (InternalEO.Binary.StartFrame <= InternalEO.Binary.EndFrame &&
		InternalEO.Binary.EndFrame <= endframelimit &&
		InternalEO.Binary.StartFrame >= 0)
	{
		std::vector<std::wstring> Unique;
		std::vector<std::wstring> IOutput;
		int entrycount;

		auto ClearForRetry = [&]()
		{
			IOutput.clear();
			Unique.clear();
		};

		if (matrix->GetSoftwareMode() == SoftwareMode::kAnimation)
		{

			InternalEO.FontMode = false;
		}
		else
		{
			InternalEO.FontMode = true;
		}

		if (gbRGB->Visible)
		{
			if (cbBinaryOptimise->Checked)
			{
				ExportOutputBinary::BinaryCreateExportAnimationRGB(matrix, InternalEO, IOutput, entrycount, Unique);

				if (!Optimiser::OptimiseData(matrix, InternalEO, IOutput))
				{
					ClearForRetry();

					ExportOutputBinary::BinaryCreateExportAnimationRGB(matrix, InternalEO, IOutput, entrycount, Unique);
				}
			}
			else
			{
				ExportOutputBinary::BinaryCreateExportAnimationRGB(matrix, InternalEO, IOutput, entrycount, Unique);
			}
		}
		else
		{
			if (Mode == MatrixMode::kRGB3BPP)
			{
				ExportOutputBinary::BinaryCreateExportAnimationRGB3bpp(matrix, InternalEO, IOutput, entrycount);
			}
			else
			{
				if (cbOptimise->Checked)
				{
					ExportOutputBinary::BinaryCreateExportAnimation(matrix, InternalEO, IOutput, entrycount, Unique);

					if (!Optimiser::OptimiseData(matrix, InternalEO, IOutput))
					{
						ClearForRetry();

						ExportOutputBinary::BinaryCreateExportAnimation(matrix, InternalEO, IOutput, entrycount, Unique);
					}
				}
				else
				{
					ExportOutputBinary::BinaryCreateExportAnimation(matrix, InternalEO, IOutput, entrycount, Unique);
				}
			}
		}

		// ===================================================================

		mBinary->Lines->Clear();

		mBinary->Lines->BeginUpdate();

		for (int t = 0; t < IOutput.size(); t++)
		{
			mBinary->Lines->Add(IOutput[t].c_str());
		}

		mBinary->Lines->EndUpdate();

		// ===================================================================

		std::wstring caption = GLanguageHandler->Text[kBinary] + L" (" + std::to_wstring(entrycount) + L" " + GLanguageHandler->Text[kBytes] + L")";

		tsBinary->Caption = caption.c_str();
	}
	else
	{
		eFrameStart->Color = clFuchsia;
		eFrameEnd->Color   = clFuchsia;
	}
}


void TfrmExport::Preview()
{
	std::wstring caption = GLanguageHandler->Text[kExportMatrixData] + L" :: " + GLanguageHandler->Text[kBuildingDataPleaseWait];

	Caption = caption.c_str();

	ToggleControlStatus(false);

	switch (pcExport->ActivePageIndex)
	{
	case 0:
		PreviewCode();
		break;
	case 1:
		PreviewBinary();
		break;
	}

	ToggleControlStatus(true);

	Caption = GLanguageHandler->Text[kExportMatrixData].c_str();
}


void TfrmExport::PreviewCode()
{
	if (!ValidateNumberEdit(eFrameStart) || !ValidateNumberEdit(eFrameEnd)) return;

	CreateExportOptions();

	int endframelimit = 0;
	int entrycount = 0;

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		endframelimit = matrix->GetFrameCount();
	}
	else
	{
		endframelimit = 9;
	}

	if (InternalEO.Code.StartFrame <= InternalEO.Code.EndFrame &&
		InternalEO.Code.EndFrame <= endframelimit &&
		InternalEO.Code.StartFrame >= 0)
	{
		std::vector<std::wstring> Unique;
		Output.clear();

		auto ClearForRetry = [&]()
		{
			InternalEO.Code.IncludePreamble = true;
			InternalEO.Code.CleanMode       = false;

			Output.clear();
			Unique.clear();
		};

		if (matrix->GetSoftwareMode() == SoftwareMode::kAnimation)
		{
			InternalEO.FontMode = false;
		}
		else
		{
			InternalEO.FontMode = true;
		}

		switch (Mode)
		{
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			if (cbOptimise->Checked)
			{
				ExportMonoBi::CreateExportAnimation(matrix, InternalEO, Output, entrycount, Unique);

				if (!Optimiser::OptimiseData(matrix, InternalEO, Output))
				{
					ClearForRetry();

					ExportMonoBi::CreateExportAnimation(matrix, InternalEO, Output, entrycount, Unique);
				}
			}
			else
			{
				ExportMonoBi::CreateExportAnimation(matrix, InternalEO, Output, entrycount, Unique);
			}
			break;
		case MatrixMode::kRGB:
			if (cbOptimise->Checked)
			{
				ExportRGB::CreateExportAnimationRGB(matrix, InternalEO, Output, entrycount, Unique);

				if (!Optimiser::OptimiseData(matrix, InternalEO, Output))
				{
					ClearForRetry();

					ExportRGB::CreateExportAnimationRGB(matrix, InternalEO, Output, entrycount, Unique);
				}
			}
			else
			{
				ExportRGB::CreateExportAnimationRGB(matrix, InternalEO, Output, entrycount, Unique);
			}
			break;
		case MatrixMode::kRGB3BPP:
			if (cbOptimise->Checked)
			{
				ExportRGB3BPP::CreateExportAnimationRGB3BPP(matrix, InternalEO, Output, entrycount, Unique);

				if (!Optimiser::OptimiseData(matrix, InternalEO, Output))
				{
					ClearForRetry();

					ExportRGB3BPP::CreateExportAnimationRGB3BPP(matrix, InternalEO, Output, entrycount, Unique);
				}
			}
			else
			{
				ExportRGB3BPP::CreateExportAnimationRGB3BPP(matrix, InternalEO, Output, entrycount, Unique);
			}
			break;
		}

		// ===================================================================

		UpdatePreview();

		// ===================================================================
	}
	else
	{
		eFrameStart->Color = clFuchsia;
		eFrameEnd->Color   = clFuchsia;
	}
}


#pragma region Toolbar_Bottom
void __fastcall TfrmExport::sbOpenClick(TObject *Sender)
{
	if (cbProfileList->Enabled)
	{
		LoadProfile(cbProfileList->Text.c_str());

		Preview();
	}
}


void __fastcall TfrmExport::sbDeleteClick(TObject *Sender)
{
	if (cbProfileList->Enabled)
	{
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kReallyDeleteThisProfile] +
					  L"\n\n" +
					  L"\"" + cbProfileList->Text.c_str() + L"\""), mtWarning, mbYesNo, 0) == mrYes)
		{
			std::wstring name = cbProfileList->Text.c_str();

			std::wstring path = GSystemSettings->App.LMSFilePath + L"export\\" + name + L"." + ProfileExtension;

			if (!GProfileHandler->DeleteExportProfile(path))
			{
				MessageDlg(GLanguageHandler->Text[kCouldntDeleteProfile].c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
			}

			PopulateProfileList();
		}
	}
}


void __fastcall TfrmExport::sbSaveClick(TObject *Sender)
{
	std::wstring s = InputBox(GLanguageHandler->Text[kProfileName].c_str(), L"", L"").c_str();

	if (!s.empty())
	{
		CreateExportOptions();

		std::wstring path = L"export\\" + s + L"." + ProfileExtension;

		GProfileHandler->Save(path,
							  gbRGB->Visible,
							  InternalEO);

		PopulateProfileList();
	}
}


void __fastcall TfrmExport::bBuildCodeClick(TObject *Sender)
{
	if (!IsBuilding)
	{
		Preview();
    }
}


void __fastcall TfrmExport::bExportClick(TObject *Sender)
{
	switch (pcExport->ActivePageIndex)
	{
	case 0:
		sdExport->Filter     = L"C/C++ header file (.h)|*.h|Include file (.inc)|*.inc|Python file (.py)|*.py";
		sdExport->DefaultExt = L".h";

		if (sdExport->Execute())
		{
			Preview();

			FileUtility::SaveVector(sdExport->FileName.c_str(), Output);

			//ModalResult = mrOk;
		}
		break;
	case 1:
		sdExport->Filter     = L"Binary file (.bin)|*.bin|Include file (.inc)|*.inc|Data file (.dat)|*.dat";
		sdExport->DefaultExt = L".bin";

		if (sdExport->Execute())
		{
			PreviewBinary();

			if (SaveBinaryData(sdExport->FileName.c_str()))
			{
				//ModalResult = mrOk;
			}
			else
			{
				MessageDlg(L"Error Saving Binary Data", mtError, TMsgDlgButtons() << mbOK, 0);
			}
		}
        break;
	}
}


void __fastcall TfrmExport::bCopyToClipboardClick(TObject *Sender)
{
	switch (pcExport->ActivePageIndex)
	{
	case 0:
		Preview();

		reExport->SelectAll();
		reExport->CopyToClipboard();
		break;
	case 1:
		PreviewBinary();

		mBinary->SelectAll();
		mBinary->CopyToClipboard();
        break;
	}
}
#pragma end_region


void __fastcall TfrmExport::FormConstrainedResize(TObject *Sender, int &MinWidth,
          int &MinHeight, int &MaxWidth, int &MaxHeight)
{
	MinHeight = 710;
	MinWidth  = 958;
}


void __fastcall TfrmExport::FormClose(TObject *Sender, TCloseAction &Action)
{
	Action = caFree;
}


void __fastcall TfrmExport::FormShow(TObject *Sender)
{
	PixelCount      = ((eSelectiveEnd->Text.ToIntDef(1) - eSelectiveStart->Text.ToIntDef(1)) + 1) * matrix->Details.Height * matrix->Details.Width;
	PixelCountFrame = matrix->Details.Height * matrix->Details.Width;

	eFrameEnd->Text = std::to_wstring(MaxFrameCount).c_str();
	eBinaryFrameEnd->Text = std::to_wstring(MaxFrameCount).c_str();

	PopulateProfileList();

	IsBuilding = false;

	reExport->Lines->Strings[10] = Utility::ReplaceString(reExport->Lines->Strings[10].c_str(), L"$X", std::to_wstring(GSystemSettings->App.ExportPreviewSize)).c_str();

	if (PixelCount <= GSystemSettings->App.ExportUpdateMaxPixels)
	{
		Preview();
	}
}


void TfrmExport::UpdatePreview()
{
	IsUpdating = true;

	if (GSystemSettings->App.ExportPreviewSize != 0)
	{
		reExport->Lines->Clear();
		reExport->Lines->BeginUpdate();

		if (GSystemSettings->App.ExportPreviewSize > 0)
		{
			int row = 0;

			while (row < GSystemSettings->App.ExportPreviewSize && row < Output.size())
			{
				reExport->Lines->Add(Output[row].c_str());

				row++;
			}

			LastRow = row;

			std::wstring caption = std::to_wstring(row) + L" of "  + std::to_wstring(Output.size());

			pPreviewStatus->Caption = caption.c_str();
		}
		else
		{
			reExport->Lines->Clear();

			for (int t = 0; t < Output.size(); t++)
			{
				reExport->Lines->Add(Output[t].c_str());
			}

			std::wstring caption = std::to_wstring(Output.size()) + L" of "  + std::to_wstring(Output.size());

			pPreviewStatus->Caption = caption.c_str();
		}

		reExport->Lines->EndUpdate();

		if (InternalEO.Examples)
		{
			AddExampleCode();
		}
	}

	IsUpdating = false;
}


void TfrmExport::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kExportMatrixData].c_str();

	tsCode->Caption = GLanguageHandler->Text[kCode].c_str();

	gbSource->Caption = GLanguageHandler->Text[kSource].c_str();
	sbDataRows->Caption = GLanguageHandler->Text[kRows].c_str();
	sbDataColumns->Caption = GLanguageHandler->Text[kColumns].c_str();
	lFrame->Caption = GLanguageHandler->Text[kFramexs].c_str();
	lSelectiveOutput->Caption = GLanguageHandler->Text[kRowxs].c_str();
	cbOptimise->Caption = GLanguageHandler->Text[kOptimiseOutputIfPossible].c_str();

	gbEachLine->Caption = GLanguageHandler->Text[kEachLineOfOutput].c_str();
	sbOutputRow->Caption = GLanguageHandler->Text[kRow].c_str();
	sbOutputFrame->Caption = GLanguageHandler->Text[kFrameC].c_str();
	sbOutputBytes->Caption = GLanguageHandler->Text[kBytesC].c_str();

	gbRGB->Caption = GLanguageHandler->Text[kRGBColourFormat].c_str();
	cbChangeBackgroundPixels->Caption = GLanguageHandler->Text[kChangeBackgroundPixels].c_str();
	Label1->Caption = GLanguageHandler->Text[kToC].c_str();
	Label6->Caption = GLanguageHandler->Text[kBrightness].c_str();

	gbLSB->Caption = GLanguageHandler->Text[kLeastSignificantBitLSB].c_str();
	sbLSBLeft->Caption = GLanguageHandler->Text[kLeft].c_str();
	sbLSBRight->Caption = GLanguageHandler->Text[kRight].c_str();

	gbExportFormat->Caption = GLanguageHandler->Text[kExportFormat].c_str();
	cbIncludeExample->Caption = GLanguageHandler->Text[kIncludeExampleCode].c_str();

	gbNumberFormat->Caption = GLanguageHandler->Text[kNumberFormat].c_str();
	sbNumberDecimal->Caption = GLanguageHandler->Text[kDecimal].c_str();
	sbNumberBinary->Caption = GLanguageHandler->Text[kBinary].c_str();
	sbNumberHex->Caption = GLanguageHandler->Text[kHex].c_str();

	gbNumberGroupingRGB->Caption = GLanguageHandler->Text[kNumberGrouping].c_str();
	sbNumberSizeRGB8bits->Caption = GLanguageHandler->Text[k8BitsOneBytePerColour].c_str();
	sbNumberSizeRGB32bits->Caption = GLanguageHandler->Text[k32Bits].c_str();

	gbNumberGrouping->Caption = GLanguageHandler->Text[kNumberGrouping].c_str();
	sbNumberSize8bitSwap->Caption = GLanguageHandler->Text[k8BitSwapNybbles].c_str();
	sbNumberSize16bitSwap->Caption = GLanguageHandler->Text[k16BitSwapBytes].c_str();

    cbLanguageFormat->Items->Clear();
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCommaSeparated].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPICAXEEEPROM].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCppFastLED].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportMicrochip].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPascal].c_str());

	bResetCode->Caption = GLanguageHandler->Text[kResetToDefaults].c_str();

	//

	tsBinary->Caption = GLanguageHandler->Text[kBinary].c_str();

	gbSourceBinary->Caption = GLanguageHandler->Text[kBinary].c_str();
	sbBinaryDataRows->Caption = GLanguageHandler->Text[kRows].c_str();
	sbBinaryDataColumns->Caption = GLanguageHandler->Text[kColumns].c_str();
	Label3->Caption = GLanguageHandler->Text[kFramexs].c_str();
	lBinarySelectiveOutput->Caption = GLanguageHandler->Text[kRowxs].c_str();
	cbBinaryOptimise->Caption = GLanguageHandler->Text[kOptimiseOutputIfPossible].c_str();

	gbLSBBinary->Caption = GLanguageHandler->Text[kLeastSignificantBitLSB].c_str();
	sbBinaryLSBLeft->Caption = GLanguageHandler->Text[kLeft].c_str();
	sbBinaryLSBRight->Caption = GLanguageHandler->Text[kRight].c_str();

	gbBinaryRGB->Caption = GLanguageHandler->Text[kRGBColourFormat].c_str();
	cbBinaryChangeBackgroundPixels->Caption = GLanguageHandler->Text[kChangeBackgroundPixels].c_str();
	Label5->Caption = GLanguageHandler->Text[kToC].c_str();
	Label8->Caption = GLanguageHandler->Text[kBrightness].c_str();

	gbNumberGroupingBinary->Caption = GLanguageHandler->Text[kNumberGrouping].c_str();
	sbBinaryNumberSize8bitSwap->Caption = GLanguageHandler->Text[k8BitSwapNybbles].c_str();

	gbFileContents->Caption = GLanguageHandler->Text[kFileContents].c_str();
	rbSaveAnimation->Caption = GLanguageHandler->Text[kEntireAnimation].c_str();
	rbSaveFrame->Caption = GLanguageHandler->Text[kFrameOnePerFile].c_str();

	gbNumberGroupingBinaryRGB->Caption = GLanguageHandler->Text[kNumberGrouping].c_str();
	sbBinaryNumberSizeRGB8bits->Caption = GLanguageHandler->Text[k8BitsOneBytePerColour].c_str();

	bResetBinary->Caption = GLanguageHandler->Text[kResetToDefaults].c_str();

	//

	gbProfiles->Caption = GLanguageHandler->Text[kProfiles].c_str();

	sbOpen->Caption = GLanguageHandler->Text[kLoad].c_str();
	sbSave->Caption = GLanguageHandler->Text[kSave].c_str();
	bBuildCode->Caption = GLanguageHandler->Text[kBuildCode].c_str();
	cbAutoPreview->Caption = GLanguageHandler->Text[kAutoBuild].c_str();

	GroupBox6->Caption = GLanguageHandler->Text[kOutput].c_str();

	bExport->Caption = GLanguageHandler->Text[kExport].c_str();

	bClose->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


bool TfrmExport::SaveBinaryData(const std::wstring file_name)
{
	int animframe = eBinaryFrameStart->Text.ToInt();
	std::wstring fileprefix = Utility::GetFilePrefix(file_name);
	std::wstring outputfilename = L"";

	if (rbSaveAnimation->Checked)
	{
		outputfilename = file_name;
	}
	else
	{
		std::wstring path = ExtractFilePath(file_name.c_str()).c_str();
		std::wstring ext = ExtractFileExt(file_name.c_str()).c_str();

		outputfilename = path + fileprefix + L"_" + std::to_wstring(animframe) + ext;
	}

	std::ofstream file;

	file.open(file_name, ios::binary);

	MatrixPreset mp;

	if (file)
	{
		char byte;

		for (int t = 0; t < mBinary->Lines->Count; t++)
		{
			if (mBinary->Lines->Strings[t] != L"")
			{
				std::wstring temp = L"";

				std::wstring line = mBinary->Lines->Strings[t].c_str();

				for (int z = 0; z < line.length(); z++)
				{
					if (line[z] != L' ')
					{
						temp += line[z];
					}
					else
					{
						byte = Convert::HexToByte(temp);

						file.write((char*)&byte, 1);

						temp = L"";
					}
				}
			}
			else
			{
				if (rbSaveFrame->Checked && animframe < eBinaryFrameEnd->Text.ToInt())
				{
					file.close();

					animframe++;

					std::wstring path = ExtractFilePath(file_name.c_str()).c_str();
					std::wstring ext = ExtractFileExt(file_name.c_str()).c_str();

					outputfilename = path + fileprefix + L"_" + std::to_wstring(animframe) + ext;

					file.open(outputfilename, ios::binary);
				}
			}
		}

		file.close();

		return true;
	}

	return false;
}


#pragma region Box_BinarySource
void __fastcall TfrmExport::sbBinaryDataRowsClick(TObject *Sender)
{
	cbBinaryDirection->Clear();
	cbBinaryScanDirection->Clear();

	if (sbBinaryDataRows->Down)
	{
		cbBinaryDirection->Items->Add(GLanguageHandler->Text[kTopToBottom].c_str());
		cbBinaryDirection->Items->Add(GLanguageHandler->Text[kBottomToTop].c_str());

		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kLeftToRight].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kRightToLeft].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kAlternateLeftRight].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kAlternateRightLeft].c_str());

		sbBinaryLSBLeft->Caption        = GLanguageHandler->Text[kLeft].c_str();
		sbBinaryLSBRight->Caption       = GLanguageHandler->Text[kRight].c_str();

		lBinarySelectiveOutput->Caption = GLanguageHandler->Text[kRowxs].c_str();
	}
	else
	{
		cbBinaryDirection->Items->Add(GLanguageHandler->Text[kLeftToRight].c_str());
		cbBinaryDirection->Items->Add(GLanguageHandler->Text[kRightToLeft].c_str());
		cbBinaryDirection->Items->Add(GLanguageHandler->Text[kSure24x16].c_str());

		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kTopToBottom].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kBottomToTop].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kAlternateDownUp].c_str());
		cbBinaryScanDirection->Items->Add(GLanguageHandler->Text[kAlternateUpDown].c_str());

		sbBinaryLSBLeft->Caption = GLanguageHandler->Text[kTop].c_str();
		sbBinaryLSBRight->Caption = GLanguageHandler->Text[kBottom].c_str();

		lBinarySelectiveOutput->Caption = GLanguageHandler->Text[kColumnxs].c_str();
	}

	cbBinaryDirection->ItemIndex     = 0;
	cbBinaryScanDirection->ItemIndex = 0;

	if (cbAutoPreview->Checked && !IsBuilding)
	{
		PreviewBinary();
	}
}
#pragma end_region


#pragma region Profiles
void TfrmExport::PopulateProfileList()
{
	GProfileHandler->UpdateAll();

	cbProfileList->Items->Clear();

	switch (Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		for (int t = 0; t < GProfileHandler->Profiles.size(); t++)
		{
			cbProfileList->Items->Add(GProfileHandler->Profiles[t].c_str());
		}
		break;
	case MatrixMode::kRGB:
		for (int t = 0; t < GProfileHandler->ProfilesRGB.size(); t++)
		{
			cbProfileList->Items->Add(GProfileHandler->ProfilesRGB[t].c_str());
		}
		break;
	case MatrixMode::kRGB3BPP:
		for (int t = 0; t < GProfileHandler->ProfilesRGB3BPP.size(); t++)
		{
			cbProfileList->Items->Add(GProfileHandler->ProfilesRGB3BPP[t].c_str());
		}
		break;
	}

	if (cbProfileList->Items->Count == 0)
	{
		cbProfileList->Enabled = false;
	}
	else
	{
		cbProfileList->Enabled = true;
		cbProfileList->ItemIndex = 0;
	}
}


void TfrmExport::LoadProfile(const std::wstring file_name)
{
	std::wstring path = GSystemSettings->App.LMSFilePath + L"export\\" + file_name + L"." + ProfileExtension;

	ExportOptions eeo = GProfileHandler->Load(path);

	if (eeo.Valid)
	{
		BuildFromProfile(eeo);
	}
	else
	{
		MessageDlg(GLanguageHandler->Text[kErrorLoadingProfile].c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
	}
}
#pragma end_region


void TfrmExport::AddExampleCode()
{
	std::wstring s = L"";

	switch (InternalEO.Code.Language)
	{
	case ExportLanguage::kCSV:
	case ExportLanguage::kPICAXE:
	case ExportLanguage::kC1Dim:
	case ExportLanguage::kC2Dim:
		break;
	case ExportLanguage::kCFastLED:
		s += ExampleFastLED::Get(InternalEO.Code.StartFrame, InternalEO.Code.EndFrame, PixelCountFrame);
		break;
	case ExportLanguage::kPython1Dim:
	case ExportLanguage::kPython2Dim:
	case ExportLanguage::kMicrochip:
	case ExportLanguage::kPascal:
		break;
	}

	if (!s.empty())
	{
		Output.push_back(ExportUtility::GetExampleCodeDisclaimer(InternalEO));

		Output.push_back(s);
	}
}


bool TfrmExport::ValidateNumberEdit(TEdit *edit)
{
	std::wstring text = edit->Text.c_str();

	if (!text.empty())
	{
		for (int t = 0; t < text.length(); t++)
		{
			if (!isdigit(text[t]))
			{
				edit->Color = clFuchsia;

				return false;
			}
		}
	}
	else
	{
		edit->Color = clFuchsia;

		return false;
	}

	edit->Color = clWindow;

    return true;
}


void __fastcall TfrmExport::bCloseClick(TObject *Sender)
{
	CreateExportOptions();
}


void __fastcall TfrmExport::bResetCodeClick(TObject *Sender)
{
	ExportOptions reset_code;

	reset_code.Binary = InternalEO.Binary;

	BuildFromProfile(reset_code);
}


void __fastcall TfrmExport::bResetBinaryClick(TObject *Sender)
{
	ExportOptions reset_binary;

	reset_binary.Code = InternalEO.Code;

	BuildFromProfile(reset_binary);
}
