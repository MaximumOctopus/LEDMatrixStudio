// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
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

extern LanguageHandler *GLanguageHandler;
extern PresetHandler *GPresetHandler;
extern ProfileHandler *GProfileHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmExport *frmExport;

ExportOptions OpenExportData(TheMatrix *thematrix, ExportOptions ieo, ExportSource source, MatrixMode mode) // mode = 0 (animation), 1 = (user memories) TO DO (now .exportopions
{
	TfrmExport *frmExport = new TfrmExport(Application);

	frmExport->matrix = thematrix;

	frmExport->Mode = mode;

	frmExport->InternalEO.ExportMode   = source;

	if (frmExport->InternalEO.ExportMode == ExportSource::kNone)
	{
		frmExport->InternalEO.ExportMode = ExportSource::kAnimation;
	}

	frmExport->InternalEO.IncludePreamble = true;

	switch (mode)
	{
	case MatrixMode::kNone:
		break;
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		frmExport->ProfileExtension                  = L"ledsexport";
		frmExport->gbNumberGrouping->Top              = frmExport->gbNumberGroupingRGB->Top;
		break;
	case MatrixMode::kRGB:
		frmExport->gbNumberGrouping->Visible          = false;

		frmExport->gbRGB->Visible                     = true;
		frmExport->gbBinaryRGB->Visible               = true;

		frmExport->gbNumberGroupingRGB->Visible       = true;
		frmExport->gbNumberGroupingBinaryRGB->Visible = true;

		frmExport->gbRGBColourSpace->Visible          = true;
		frmExport->gbRGBColourSpace->Top              = 415;

		frmExport->gbBinaryColourSpaceRGB->Visible    = true;
		frmExport->gbBinaryColourSpaceRGB->Top        = 415;

		frmExport->ProfileExtension                  = L"ledsexportrgb";
		break;
	case MatrixMode::kRGB3BPP:
		frmExport->gbNumberGrouping->Visible          = false;

		frmExport->gbRGB->Visible                     = true;
		frmExport->gbRGB->Height                      = 65;                    // hides background change option
		frmExport->gbBinaryRGB->Visible               = true;

		frmExport->gbNumberGroupingRGB->Visible       = true;
		frmExport->gbNumberGroupingBinaryRGB->Visible = true;

		frmExport->ProfileExtension                  = L"ledsexportrgb3bpp";
		break;
	}

	if (source == ExportSource::kAnimation)
	{
		frmExport->SetMaxFrameCount(thematrix->GetFrameCount()); 	// anim
	}
	else
	{
		frmExport->SetMaxFrameCount(10);						// user memories
	}

	frmExport->eSelectiveStart->Text       = L"1";
	frmExport->eSelectiveEnd->Text         = thematrix->Details.Height;

	frmExport->eBinarySelectiveStart->Text = L"1";
	frmExport->eBinarySelectiveEnd->Text   = thematrix->Details.Height;

	if (ieo.ExportMode != ExportSource::kNone)
	{
		frmExport->BuildFromProfile(ieo);
	}

	ExportOptions eeo;

	frmExport->cbDirectionChange(nullptr);         // these must be executed after the matrix has been assigned (or crash!!)
	frmExport->sbBinaryDataRowsClick(nullptr);


	if (frmExport->ShowModal() == mrOk)
	{
		eeo.Valid = true;

		eeo = frmExport->InternalEO;
	}

	delete frmExport;

	return eeo;
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

	cbLineCount->ItemIndex = 1;

	cbAutoPreview->Checked = true;
}


void TfrmExport::BuildFromProfile(ExportOptions eeo)
{
	if (eeo.Source == ReadSource::kRows)
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

	if (eeo.LSB == LeastSignificantBit::kTopLeft)
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
		switch (eeo.Format)
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
		switch (eeo.Size)
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
		switch  (eeo.Size)
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

	switch (eeo.Content)
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

	cbLineCount->Text = std::to_wstring(eeo.LineCount).c_str();

	// ===========================================================================

	if (gbRGB->Visible)
	{
		switch (eeo.TextRGBMode)
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

		cbChangeBackgroundPixels->Checked  = eeo.RGBChangePixels;
		shapeBackgroundPixels->Brush->Color = TColor(eeo.RGBChangeColour);

		if (eeo.RGBBrightness > 100)
		{
			eeo.RGBBrightness = 100;
		}

		groupBoxRGBBrightness->Text = eeo.RGBBrightness;

		switch (eeo.ColourSpaceRGB)
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

	if (eeo.BinarySource == ReadSource::kRows)
	{
		sbBinaryDataRows->Down = true;
	}
	else
	{
		sbBinaryDataColumns->Down = true;
	}

	// ===========================================================================

	cbBinaryDirection->ItemIndex = eeo.BinaryOrientationToInt();

	// ===========================================================================

	cbBinaryScanDirection->ItemIndex = eeo.BinaryScanDirectionToInt();

	// ===========================================================================

	if (eeo.BinaryLSB == LeastSignificantBit::kTopLeft)
	{
		sbBinaryLSBLeft->Down = true;
	}
	else
	{
		sbBinaryLSBRight->Down = true;
	}

	// ===========================================================================

	switch (eeo.BinarySize)
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

	switch (eeo.BinaryRGBMode)
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

	cbBinaryChangeBackgroundPixels->Checked  = eeo.BinaryRGBChangePixels;
	shapeBinaryBackgroundPixels->Brush->Color = TColor(eeo.BinaryRGBChangeColour);

	groupBoxBinaryRGBBrightness->Text = std::to_wstring(eeo.BinaryRGBBrightness).c_str();

	if (eeo.BinaryColourSpaceRGB == ColourSpace::kRGB32)
	{
		sbBCSRGB32->Down = true;
	}
	else
	{
		sbBCSRGB565->Down = true;
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
		InternalEO.IncludePreamble = false;
		InternalEO.CleanMode       = true;
	}
	else
	{
		InternalEO.IncludePreamble = true;
		InternalEO.CleanMode       = false;
	}

	// =======================================================================

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		InternalEO.StartFrame = eFrameStart->Text.ToInt();
		InternalEO.EndFrame   = eFrameEnd->Text.ToInt();
	}
	else
	{
		InternalEO.StartFrame = eFrameStart->Text.ToInt();
		InternalEO.EndFrame   = eFrameEnd->Text.ToInt();
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

	InternalEO.SelectiveStart = ss;
	InternalEO.SelectiveEnd   = se;

	// =======================================================================

	if (sbDataRows->Down)
	{
		InternalEO.Source = ReadSource::kRows;
	}
	else
	{
		InternalEO.Source = ReadSource::kColumns;
	}

	// =======================================================================

	InternalEO.OrientationFromInt(cbDirection->ItemIndex);

	// =======================================================================

	InternalEO.ScanDirectionFromInt(InternalEO.Source, cbScanDirection->ItemIndex);

	// =======================================================================

	if (sbLSBLeft->Down)
	{
		InternalEO.LSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.LSB = LeastSignificantBit::kBottomRight;
	}

	// =======================================================================

	InternalEO.LanguageFromInt(cbLanguageFormat->ItemIndex);

	InternalEO.Examples = cbIncludeExample->Checked;

	// =======================================================================

	if (gbNumberFormat->Visible)
	{
		if (sbNumberDecimal->Down)
		{
			InternalEO.Format = NumberFormat::kDecimal;
		}
		else if (sbNumberBinary->Down)
		{
			InternalEO.Format = NumberFormat::kBinary;
		}
		else
		{
			InternalEO.Format = NumberFormat::kHex;
		}
	}
	else
	{
		InternalEO.Format = NumberFormat::kHex;
	}

	// =======================================================================

	if (gbNumberGrouping->Visible)
	{
		if (sbNumberSize8bit->Down)
		{
			InternalEO.Size = NumberSize::k8Bit;
		}
		else if (sbNumberSize16bit->Down)
		{
			InternalEO.Size = NumberSize::k16bit;
		}
		else if (sbNumberSize32bit->Down)
		{
			InternalEO.Size = NumberSize::k32bit;
		}
		else if (sbNumberSize8bitSwap->Down)
		{
			InternalEO.Size = NumberSize::k8bitSwap;
		}
		else if (sbNumberSize16bitSwap->Down)
		{
			InternalEO.Size = NumberSize::k16bitSwap;
		}
	}
	else
	{
		if (sbNumberSizeRGB8bits->Down)
		{
			InternalEO.Size = NumberSize::kRGB8bit;
		}
		else if (sbNumberSizeRGB32bits->Down)
		{
			InternalEO.Size = NumberSize::kRGB32bit;
		}
		else
		{
			InternalEO.Size = NumberSize::kRGB32bit;
		}
	}

	// =======================================================================

	if (sbOutputRow->Down)
	{
		InternalEO.Content = LineContent::kRowCol;
	}
	else if (sbOutputFrame->Down)
	{
		InternalEO.Content = LineContent::kFrame;
	}
	else if (sbOutputBytes->Down)
	{
		InternalEO.Content = LineContent::kBytes;
	}

	// =======================================================================

	if (gbRGB->Visible)
	{
		InternalEO.RGBEnabled = true;

		if (sbRGB->Down)
		{
			InternalEO.TextRGBMode = RGBMode::kRGB;
		}
		else if (sbBGR->Down)
		{
			InternalEO.TextRGBMode = RGBMode::kBGR;
		}
		else if (sbGRB->Down)
		{
			InternalEO.TextRGBMode = RGBMode::kGRB;
		}
		else if (sbBRG->Down)
		{
			InternalEO.TextRGBMode = RGBMode::kBRG;
		}

		InternalEO.RGBChangePixels = cbChangeBackgroundPixels->Checked;
		InternalEO.RGBChangeColour = shapeBackgroundPixels->Brush->Color;

		InternalEO.RGBBrightness   = groupBoxRGBBrightness->Text.ToIntDef(100);

		if (sbCSRGB32->Down)
		{
			InternalEO.ColourSpaceRGB = ColourSpace::kRGB32;
			InternalEO.Size     = NumberSize::kRGB32bit;
		}
		else
		{
			InternalEO.ColourSpaceRGB = ColourSpace::kRGB565;
			InternalEO.Size     = NumberSize::kRGB16bit;
		}
	}
	else
	{
		InternalEO.RGBEnabled = false;
	}

	// =======================================================================

	InternalEO.LineCount = cbLineCount->Text.ToInt();

	// =======================================================================
	//   binary file specific options
	// =======================================================================

	if (sbBinaryDataRows->Down)
	{
		InternalEO.BinarySource = ReadSource::kRows;
	}
	else
	{
		InternalEO.BinarySource = ReadSource::kColumns;
	}

	// =======================================================================

	InternalEO.BinaryOrientationFromInt(cbBinaryDirection->ItemIndex);

	// =======================================================================

	InternalEO.BinaryScanDirectionFromInt(InternalEO.BinarySource, cbBinaryScanDirection->ItemIndex);

	// =======================================================================

	if (sbBinaryLSBLeft->Down)
	{
		InternalEO.BinaryLSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.BinaryLSB = LeastSignificantBit::kBottomRight;
	}

	// =======================================================================

	if (gbBinaryRGB->Visible)
	{
		if (sbBinaryRGB->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kRGB;
		}
		else if (sbBinaryBGR->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kBGR;
		}
		else if (sbBinaryGRB->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kGRB;
		}
		else if (sbBinaryBRG->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kBRG;
		}

		InternalEO.BinaryRGBChangePixels = cbBinaryChangeBackgroundPixels->Checked;
		InternalEO.BinaryRGBChangeColour = shapeBinaryBackgroundPixels->Brush->Color;

		InternalEO.BinaryRGBBrightness   = groupBoxBinaryRGBBrightness->Text.ToIntDef(100);

		if (sbBCSRGB32->Down)
		{
			InternalEO.BinaryColourSpaceRGB = ColourSpace::kRGB32;
		}
		else
		{
			InternalEO.BinaryColourSpaceRGB = ColourSpace::kRGB565;
		}
	}

	// =======================================================================

	InternalEO.BinarySize = NumberSize::kRGB8bit;

	// =======================================================================

	if (rbSaveAnimation->Checked)
	{
		InternalEO.BinaryContent = BinaryFileContents::kEntireAnimation;
	}
	else
	{
		InternalEO.BinaryContent = BinaryFileContents::kSingleFrame;
	}
}


void TfrmExport::CreateBinaryExportOptions()
{
	//  eeo.Language     := -1; // none
	InternalEO.Content  = LineContent::kFrame;  // process in frames
	InternalEO.Format = NumberFormat::kHex;   // always in hex format

	// =======================================================================

	if (cbBinaryOptimise->Checked)
	{
		InternalEO.IncludePreamble = false;
		InternalEO.CleanMode       = true;
	}
	else
	{
		InternalEO.IncludePreamble = true;
		InternalEO.CleanMode       = false;
	}

	// =========================================================================

	if (InternalEO.ExportMode == ExportSource::kAnimation)
	{
		InternalEO.StartFrame = eFrameStart->Text.ToInt();
		InternalEO.EndFrame   = eFrameEnd->Text.ToInt();
	}
	else
	{
		InternalEO.StartFrame = eFrameStart->Text.ToInt();
		InternalEO.EndFrame   = eFrameEnd->Text.ToInt();
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

	InternalEO.SelectiveStart = ss;
	InternalEO.SelectiveEnd   = se;

	// =========================================================================

	if (sbBinaryDataRows->Down)
	{
		InternalEO.BinarySource = ReadSource::kRows;
	}
	else
	{
		InternalEO.BinarySource = ReadSource::kColumns;
	}

	// =========================================================================

	InternalEO.BinaryOrientationFromInt(cbBinaryDirection->ItemIndex);

	// =========================================================================

	InternalEO.BinaryScanDirectionFromInt(InternalEO.BinarySource, cbBinaryScanDirection->ItemIndex);

	// =========================================================================

	if (sbBinaryLSBLeft->Down)
	{
		InternalEO.BinaryLSB = LeastSignificantBit::kTopLeft;
	}
	else
	{
		InternalEO.BinaryLSB = LeastSignificantBit::kBottomRight;
	}

	// =========================================================================

	if (gbNumberGrouping->Visible)
	{
		if (sbBinaryNumberSize8bit->Down)
		{
			InternalEO.BinarySize = NumberSize::k8Bit;
		}
		else if (sbBinaryNumberSize8bitSwap->Down)
		{
			InternalEO.BinarySize = NumberSize::k8bitSwap;
		}
		else if (sbBinaryNumberSize16bitSwap->Down)
		{
			InternalEO.BinarySize = NumberSize::k16bitSwap;
		}
		else
		{
			InternalEO.BinarySize = NumberSize::kRGB8bit;
		}
	}

	// =========================================================================

	if (gbBinaryRGB->Visible)
	{
		InternalEO.RGBEnabled = true;

		if (sbBinaryRGB->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kRGB;
		}
		else if (sbBinaryBGR->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kBGR;
		}
		else if (sbBinaryGRB->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kGRB;
		}
		else if (sbBinaryBRG->Down)
		{
			InternalEO.BinaryRGBMode = RGBMode::kBRG;
		}

		InternalEO.BinaryRGBChangePixels = cbBinaryChangeBackgroundPixels->Checked;
		InternalEO.BinaryRGBChangeColour = shapeBinaryBackgroundPixels->Brush->Color;

		InternalEO.BinaryRGBBrightness   = groupBoxBinaryRGBBrightness->Text.ToIntDef(100);

		if (sbBCSRGB32->Down)
		{
			InternalEO.BinaryColourSpaceRGB = ColourSpace::kRGB32;
		}
		else
		{
			InternalEO.BinaryColourSpaceRGB = ColourSpace::kRGB565;
		}
	}
	else
	{
		InternalEO.RGBEnabled = false;
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
		shapeBackgroundPixels->Brush->Color = cdExport->Color;

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

	if (InternalEO.StartFrame <= InternalEO.EndFrame && InternalEO.EndFrame <= endframelimit && InternalEO.StartFrame >= 1)
	{
		std::vector<std::wstring> Unique;
		std::vector<std::wstring> IOutput;
		int entrycount;

		auto ClearForRetry = [&]()
		{
			InternalEO.IncludePreamble = true;
			InternalEO.CleanMode       = false;

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

		// ===================================================================

		mBinary->Lines->Clear();

		mBinary->Lines->BeginUpdate();

		for (int t = 0; t < Output.size(); t++)
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

	if (InternalEO.StartFrame <= InternalEO.EndFrame && InternalEO.EndFrame <= endframelimit && InternalEO.StartFrame >= 1)
	{
		std::vector<std::wstring> Unique;
		Output.clear();

		auto ClearForRetry = [&]()
		{
			InternalEO.IncludePreamble = true;
			InternalEO.CleanMode       = false;

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
			std::wstring path = ExtractFilePath(Application->ExeName).c_str();

			std::wstring name = cbProfileList->Text.c_str();

			path += L"export\\" + name + L"." + ProfileExtension;

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

		std::wstring path = ExtractFilePath(Application->ExeName).c_str();

		path += L"export\\" + s + L"." + ProfileExtension;

		GProfileHandler->Save(path,
							  gbRGB->Visible,
							  InternalEO);

		PopulateProfileList(); // TO DO make sure profile handler refreshes file list
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

			// to do IMPORTANT Output.SaveToFile(sdExport->Filename);

			ModalResult = mrOk;
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
				ModalResult = mrOk;
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
	MinHeight = 740;
	MinWidth  = 688;
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

	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCommaSeparated].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPICAXEEEPROM].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCppFastLED].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportMicrochip].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPascal].c_str());

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

	std::ofstream file(outputfilename);

	MatrixPreset mp;

	if (file)
	{
		for (int t = 0; t < mBinary->Lines->Count; t++)
		{
			if (mBinary->Lines->Strings[t] != L"")
			{
				std::wstring temp = L"";

				for (int z = 0; z < mBinary->Lines->Strings[t].Length(); z++)
				{
					if (mBinary->Lines->Strings[t][z] != L' ')
					{
						temp += mBinary->Lines->Strings[t][z];
					}
					else
					{
						int byte = Convert::HexToByte(temp);

						file << byte;

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

					// TO DO , need to change output file namefile(outputfilename);
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
	if (GProfileHandler->Profiles.size() != 0)
	{
        // to do, refresh profile handler lst

		cbProfileList->Items->Clear();

		for (int t = 0; t < GProfileHandler->Profiles.size(); t++)
		{
			cbProfileList->Items->Add(GProfileHandler->Profiles[t].c_str());
		}
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
	std::wstring path = ExtractFilePath(Application->ExeName).c_str();

	path += L"export\\" + file_name + L"." + ProfileExtension;

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

	switch (InternalEO.Language)
	{
	case ExportLanguage::kCSV:
	case ExportLanguage::kPICAXE:
	case ExportLanguage::kC1Dim:
	case ExportLanguage::kC2Dim:
		break;
	case ExportLanguage::kCFastLED:
		// TO DO s = ExampleFastLED::GetExample(InternalEO.StartFrame, InternalEO.EndFrame, PixelCountFrame);
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
