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

#include <algorithm>
#include <fstream>

#include "main.h"

#include "ColourUtility.h"
#include "DataOut.h"
#include "ExportMonoBi.h"
#include "FontHandler.h"
#include "Formatting.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "PresetHandler.h"
#include "ProfileHandler.h"
#include "SystemConstants.h"
#include "SystemSettings.h"
#include "Utility.h"

#include "FormAbout.h"
#include "FormAutomate.h"
#include "FormCheckVersion.h"
#include "FormColourChange.h"
#include "FormCopyMultiple.h"
#include "FormDeleteMultiple.h"
#include "FormExport.h"
#include "FormExportCode.h"
#include "FormExportGIF.h"
#include "FormFontViewer.h"
#include "FormImportBitmap.h"
#include "FormMerge.h"
#include "FormNewBrush.h"
#include "FormNewProject.h"
#include "FormOptimise.h"
#include "FormPlaybackSpeed.h"
#include "FormPreferences.h"
#include "FormPreviewPopout.h"
#include "FormSaveRange.h"
#include "FormSetIgnoredPixels.h"
#include "FormToggleLockStatus.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;

extern FontHandler *GFontHandler;
extern LanguageHandler *GLanguageHandler;
extern PresetHandler *GPresetHandler;
extern ProfileHandler *GProfileHandler;
extern SystemSettings *GSystemSettings;


__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TForm(Owner)
{
	#if _DEBUG
	miDebug->Visible = true;
	#endif
}


void __fastcall TfrmMain::WmDropFiles(TWMDropFiles& Message)
{
	wchar_t buff[MAX_PATH];
	HDROP hDrop = (HDROP)Message.Drop;
	int count = DragQueryFile(hDrop, -1, NULL, NULL);

	if (count != 0)
	{
		DragQueryFileW(hDrop, 0, buff, sizeof(buff));

        LoadWithWarnings(buff);
	}

	 DragFinish(hDrop);
}


void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
	Caption = __Caption.c_str();
	BackupCaption = Caption;
	LastTick = GetTickCount();

	thematrix = new TheMatrix(this, pCanvas);
	thematrix->OnChange              = std::bind(MatrixOnChange, std::placeholders::_1);
	thematrix->OnLayerChange         = std::bind(MatrixOnLayerChange, std::placeholders::_1);
	thematrix->OnSizeChange          = std::bind(MatrixOnSizeChange, std::placeholders::_1);
	thematrix->OnDisplayBufferCopied = std::bind(MatrixOnDisplayBufferCopied, std::placeholders::_1);
	thematrix->OnNewFrameDisplayed   = std::bind(MatrixOnNewFrameDisplayed, std::placeholders::_1);
	thematrix->OnColourChange        = std::bind(MatrixOnColourChange, std::placeholders::_1);
	thematrix->OnNew3bppColours      = std::bind(MatrixOnNew3bppColours, std::placeholders::_1);
	thematrix->OnMouseOver           = std::bind(MatrixOnMouseOver, std::placeholders::_1, std::placeholders::_2);
	thematrix->OnPreviewMouseDown    = std::bind(MatrixOnPreviewMouseDown, std::placeholders::_1, std::placeholders::_2);
	thematrix->OnDebugEvent          = std::bind(MatrixOnDebug, std::placeholders::_1, std::placeholders::_2);

	std::wstring path = ExtractFilePath(Application->ExeName).c_str();

	GFontHandler = new FontHandler(path + L"fonts\\*.ledsfont");
	GPresetHandler = new PresetHandler(path + L"presets\\*.ledspreset");
	GProfileHandler = new ProfileHandler(path + L"export\\*.");

	InitComponentCache();

	ConfigureControls();

	InitFrames();

	SetGuiLanguageText();

	BuildFontMenu();
	BuildGradientMenu();
	BuildLanguageMenu();
	BuildPresetMenu();
	BuildReOpenMenu();

	GenerateShades(sSelectionLMB->Brush->Color);

	//

	SetFromSettings();
}


void __fastcall TfrmMain::FormClose(TObject *Sender, TCloseAction &Action)
{
	OnResize = nullptr;       // stops the resize firing after the matrix has been freed!

	// =======================================================================

	if (thematrix->GetPreviewPopout())
	{
		frmPreviewPopout->Close();
	}

	// =======================================================================

	for (int t = 0; t < 6; t++)
	{
		GSystemSettings->LEDColoursSingle[t] = thematrix->LEDColoursSingle[t];
	}

	for (int t = 0; t < 6; t++)
	{
		GSystemSettings->LEDColoursBi[t] = thematrix->LEDColoursBi[t];
	}

	GSystemSettings->RGBBackground    = thematrix->RGBBackground;
	GSystemSettings->LEDRGBColours[1] = thematrix->LEDRGBColours[CMouseLeft];
	GSystemSettings->LEDRGBColours[2] = thematrix->LEDRGBColours[CMouseMiddle];
	GSystemSettings->LEDRGBColours[3] = thematrix->LEDRGBColours[CMouseRight];

	if (miPixelAuto->Checked)
	{
		GSystemSettings->Project.PixelSize = CPixelSizeAuto;
	}

	// =======================================================================

	GSystemSettings->SelectionColours[0] = sSelectionLMB->Tag;
	GSystemSettings->SelectionColours[1] = sSelectionMMB->Tag;
	GSystemSettings->SelectionColours[2] = sSelectionRMB->Tag;

	// =======================================================================

	GSystemSettings->Bars.Animation  = miShowAnimationToolbar->Checked;
	GSystemSettings->Bars.RGBPalette = miPaletteGradientToolbar->Checked;

	GSystemSettings->App.AnimSpeed = timerAnimate->Interval;

	// =======================================================================

	GSystemSettings->App.AutoSaveEnabled = miAutoSave->Checked;

	// =======================================================================

	GSystemSettings->Preview.Active    = miPreview->Checked;
	GSystemSettings->Preview.Size      = thematrix->GetPreviewBoxSize();
	GSystemSettings->Preview.View      = thematrix->GetPreviewView();
	GSystemSettings->Preview.Void      = thematrix->GetPreviewVoid();
	GSystemSettings->Preview.Offset    = thematrix->GetRadialOffset();
	GSystemSettings->Preview.Direction = thematrix->GetRadialOffsetDirection();

	// =======================================================================

	for (int t = 0; t < 16; t++)
	{
		GSystemSettings->RGBPalette[t] = _RGBPalette[t]->Brush->Color;
	}

	// =======================================================================
	// =======================================================================

	GSystemSettings->SaveSettings();
}


void __fastcall TfrmMain::FormDestroy(TObject *Sender)
{
    FramePalettePanel->DeInit();

	delete FrameGradientPanel;
	delete FrameLayerPanel;
	delete FramePalettePanel;
	delete FrameQuickData;
	delete FrameUndoPanel;

	delete thematrix;
}


void __fastcall TfrmMain::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
	int tick = GetTickCount();

	if (thematrix->Render.Draw.Mode == DrawMode::kFont && Key == VK_BACK)	// backspace, 1 column
	{
		thematrix->DeleteFontCharacter(GetSelectedFrame());
	}
	else if (thematrix->Render.Draw.Mode == DrawMode::kFont && Key > 31 && tick - LastTick >= 300)
	{
		LastTick = tick;

		thematrix->DrawFontCharacter(Key - 32, GetSelectedFrame());
	}
	else if (Key == VK_RETURN || Key == VK_ESCAPE)
	{
		thematrix->CancelDrawMode();

		sbMouseMode->Down = true;
	}
}


void __fastcall TfrmMain::FormMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
	statusMain->SimpleText = __SimpleTextFull.c_str();
	lPixelColour->Caption  = L"";

	OldMouseX = -1;
	OldMouseY = -1;
}


void __fastcall TfrmMain::FormMouseWheelDown(TObject *Sender, TShiftState Shift, TPoint &MousePos,
		  bool &Handled)
{
	if (Shift.Contains(ssCtrl))
	{
		int sp = thematrix->Render.Draw.Parameter;

		if (sp > 1)                                 // do not allow a value of one or some draw modes will run forever!
		{
			thematrix->SetShapeParameter(sp - 1);
		}
	}
	else
	{
		if (tbFrames->Max != 1)
		{
			if (GetSelectedFrame() == 0)
			{
				tbFrames->Position = tbFrames->Max;
			}
			else
			{
				tbFrames->Position--;
			}

			tbFramesChange(nullptr);

			frmPreviewPopout->tbFrames->Position = tbFrames->Position;
		}
	}

	Handled = true;
}


void __fastcall TfrmMain::FormMouseWheelUp(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled)
{
	if (Shift.Contains(ssCtrl))
	{
		int sp = thematrix->Render.Draw.Parameter;

		thematrix->SetShapeParameter(sp + 1);
	}
	else
	{
		if (tbFrames->Max != 1)
		{
			if (tbFrames->Position == tbFrames->Max)
			{
				tbFrames->Position = 1;
			}
			else
			{
				tbFrames->Position++;
			}

			tbFramesChange(nullptr);

			frmPreviewPopout->tbFrames->Position = tbFrames->Position;
		}
	}

	Handled = true;
}


void __fastcall TfrmMain::FormResize(TObject *Sender)
{
	if (miPixelAuto->Checked && thematrix->Details.Available)
	{
		miPixelTinyClick(miPixelAuto);

		switch (sbGradient->Tag)
		{
		case 0:
			ToggleGradient(GradientOption::kOff, false);
			break;
		case 1:
			ToggleGradient(GradientOption::kVertical, false);
			break;
		case 2:
			ToggleGradient(GradientOption::kHorizontal, false);
			break;
		}
	}
	else if (thematrix->Details.Available)
	{
		thematrix->ChangeZoomUI(GSystemSettings->Project.PixelSize);
	}

	thematrix->Refresh();
}


void __fastcall TfrmMain::FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
		  int &MaxWidth, int &MaxHeight)
{
	MinWidth  = 713;
	MinHeight = 310;
}


void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
	if (thematrix->AnimPlaying)
	{
		if (timerAnimate->Enabled)
		{
			bPlayAnimationClick(bStopAnimation);
		}

		CanClose = false;
	}
	else
	{
		if (sbClear->Enabled)
		{
			if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kExitLMS] +
										  L"\n\n" +
										  GLanguageHandler->Text[kAreYouSure]), mtWarning, mbYesNo, 0) == mrYes)
			{
				CanClose = true;
			}
			else
			{
				CanClose = false;
			}
		}
	}
}


void TfrmMain::InitComponentCache()
{
	_MenuCopyMemory[0]=miMemory1; _MenuCopyMemory[1]=miMemory2; _MenuCopyMemory[2]=miMemory3; _MenuCopyMemory[3]=miMemory4; _MenuCopyMemory[4]=miMemory5;
	_MenuCopyMemory[5]=miMemory6; _MenuCopyMemory[6]=miMemory7; _MenuCopyMemory[7]=miMemory8; _MenuCopyMemory[8]=miMemory9; _MenuCopyMemory[9]=miMemory10;

	_MenuRestoryMemory[0]=miMemoryR1; _MenuRestoryMemory[1]=miMemoryR2; _MenuRestoryMemory[2]=miMemoryR3; _MenuRestoryMemory[3]=miMemoryR4;
	_MenuRestoryMemory[4]=miMemoryR5; _MenuRestoryMemory[5]=miMemoryR6; _MenuRestoryMemory[6]=miMemoryR7; _MenuRestoryMemory[7]=miMemoryR8;
	_MenuRestoryMemory[8]=miMemoryR9; _MenuRestoryMemory[9]=miMemoryR10;

	_RGBPalette[0]  = sRGBPalette1; _RGBPalette[1]  = sRGBPalette2; _RGBPalette[2]  = sRGBPalette3; _RGBPalette[3]  = sRGBPalette4;
	_RGBPalette[4]  = sRGBPalette5; _RGBPalette[5]  = sRGBPalette6; _RGBPalette[6]  = sRGBPalette7; _RGBPalette[7]  = sRGBPalette8;
	_RGBPalette[8]  = sRGBPalette9; _RGBPalette[9]  = sRGBPalette10; _RGBPalette[10] = sRGBPalette11; _RGBPalette[11] = sRGBPalette12;
	_RGBPalette[12] = sRGBPalette13; _RGBPalette[13] = sRGBPalette14; _RGBPalette[14] = sRGBPalette15; _RGBPalette[15] = sRGBPalette16;

	_RGB3ppPalette[0] = sRGB3pp1; _RGB3ppPalette[1] = sRGB3pp2; _RGB3ppPalette[2] = sRGB3pp3; _RGB3ppPalette[3] = sRGB3pp4;
	_RGB3ppPalette[4] = sRGB3pp5; _RGB3ppPalette[5] = sRGB3pp6; _RGB3ppPalette[6] = sRGB3pp7; _RGB3ppPalette[7] = sRGB3pp8;

	_RGBShade[0] = sShade1; _RGBShade[1] = sShade2;  _RGBShade[2]  = sShade3;  _RGBShade[3]  = sShade4;  _RGBShade[4]  = sShade5;  _RGBShade[5]  = sShade6;  _RGBShade[6]  = sShade7;  _RGBShade[7] = sShade8;
	_RGBShade[8] = sShade9; _RGBShade[9] = sShade10; _RGBShade[10] = sShade11; _RGBShade[11] = sShade12; _RGBShade[12] = sShade13; _RGBShade[13] = sShade14; _RGBShade[14] = sShade15; _RGBShade[15] = sShade16;

	_PreviewMenuSize[0][0] = miPreviewx1;   _PreviewMenuSize[0][1] = miPreviewx2;   _PreviewMenuSize[0][2] = miPreviewx3;   _PreviewMenuSize[0][3] = miPreviewx4;
	_PreviewMenuSize[0][4] = miPreviewx5;   _PreviewMenuSize[0][5] = miPreviewx6;   _PreviewMenuSize[0][6] = miPreviewx8;   _PreviewMenuSize[0][7] = miPreviewx10;
	_PreviewMenuSize[0][8] = miPreviewx12;  _PreviewMenuSize[0][9] = miPreviewx15;  _PreviewMenuSize[0][10] = miPreviewx20; _PreviewMenuSize[0][11] = miPreviewx25;
	_PreviewMenuSize[0][12] = miPreviewx30; _PreviewMenuSize[0][13] = miPreviewx40; _PreviewMenuSize[0][14] = miPreviewx50;

	_PreviewMenuSize[1][0] = miPUPreviewx1; _PreviewMenuSize[1][1] = miPUPreviewx2; _PreviewMenuSize[1][2] = miPUPreviewx3; _PreviewMenuSize[1][3] = miPUPreviewx4;
	_PreviewMenuSize[1][4] = miPUPreviewx5; _PreviewMenuSize[1][5] = miPUPreviewx6; _PreviewMenuSize[1][6] = miPUPreviewx8; _PreviewMenuSize[1][7] = miPUPreviewx10;
	_PreviewMenuSize[1][8] = miPUPreviewx12; _PreviewMenuSize[1][9] = miPUPreviewx15; _PreviewMenuSize[1][10] = miPUPreviewx20; _PreviewMenuSize[1][11] = miPUPreviewx25;
	_PreviewMenuSize[1][12] = miPUPreviewx30; _PreviewMenuSize[1][13] = miPUPreviewx40; _PreviewMenuSize[1][14] = miPUPreviewx50;

	_PreviewMenuView[0][0] = miPreviewViewSquare; _PreviewMenuView[0][1] = miPreviewViewRadial; _PreviewMenuView[0][2] = miPreviewViewRadialTQ; _PreviewMenuView[0][3] = miPreviewViewSemiCircle; _PreviewMenuView[0][4] = miPreviewViewSemiCircleInverted;
	_PreviewMenuView[1][0] = miPUPreviewViewSquare; _PreviewMenuView[1][1] = miPUPreviewViewRadial; _PreviewMenuView[1][2] = miPUPreviewViewRadialTQ; _PreviewMenuView[1][3] = miPUPreviewViewSemiCircle; _PreviewMenuView[1][4] = miPUPreviewViewSemiCircleInverted;

	_PreviewMenuVoid[0][0] = miPUPreviewVoid10; _PreviewMenuVoid[0][1] = miPUPreviewVoid15; _PreviewMenuVoid[0][2] = miPUPreviewVoid20; _PreviewMenuVoid[0][3] = miPUPreviewVoid25;
	_PreviewMenuVoid[0][4] = miPUPreviewVoid30; _PreviewMenuVoid[0][5] = miPUPreviewVoid40; _PreviewMenuVoid[0][6] = miPUPreviewVoid50;
	_PreviewMenuVoid[1][0] = miPreviewVoid10; _PreviewMenuVoid[1][1] = miPreviewVoid15; _PreviewMenuVoid[1][2] = miPreviewVoid20; _PreviewMenuVoid[1][3] = miPreviewVoid25;
	_PreviewMenuVoid[1][4] = miPreviewVoid30; _PreviewMenuVoid[1][5] = miPreviewVoid40; _PreviewMenuVoid[1][6] = miPreviewVoid50;
}


void TfrmMain::ConfigureControls()
{
	cbRotateAngle->Items->Add(L"5°");
	cbRotateAngle->Items->Add(L"10°");
	cbRotateAngle->Items->Add(L"15°");
	cbRotateAngle->Items->Add(L"18°");
	cbRotateAngle->Items->Add(L"20°");
	cbRotateAngle->Items->Add(L"22.5°");
	cbRotateAngle->Items->Add(L"25°");
	cbRotateAngle->Items->Add(L"30°");
	cbRotateAngle->Items->Add(L"35°");
	cbRotateAngle->Items->Add(L"36°");
	cbRotateAngle->Items->Add(L"40°");
	cbRotateAngle->Items->Add(L"45°");
	cbRotateAngle->Items->Add(L"50°");
	cbRotateAngle->Items->Add(L"55°");
	cbRotateAngle->Items->Add(L"60°");
	cbRotateAngle->Items->Add(L"65°");
	cbRotateAngle->Items->Add(L"67.5°");
	cbRotateAngle->Items->Add(L"70°");
	cbRotateAngle->Items->Add(L"72°");
	cbRotateAngle->Items->Add(L"75°");
	cbRotateAngle->Items->Add(L"80°");
	cbRotateAngle->Items->Add(L"85°");
	cbRotateAngle->Items->Add(L"90°");
	cbRotateAngle->ItemIndex = 0;

	for (int x = 1; x <= 72; x++)
	{
		cbRotateCount->Items->Add(std::to_wstring(x).c_str());
	}

	cbRotateCount->ItemIndex = 0;

	pRGB_3BPP->Left = 0;

	DrawData dd;

	sbCopy->Tag                       = dd.DrawModeToInt(DrawMode::kCopy);
	sbFilledRectangle->Tag            = dd.DrawModeToInt(DrawMode::kFilledBox);
	sbFrame->Tag                      = dd.DrawModeToInt(DrawMode::kEmptyBox);
	sbEmptyCircle->Tag                = dd.DrawModeToInt(DrawMode::kEmptyCircle);
	sbFilledCircle->Tag               = dd.DrawModeToInt(DrawMode::kFilledCircle);
	sbLine->Tag                       = dd.DrawModeToInt(DrawMode::kLine);
	sbFont->Tag                       = dd.DrawModeToInt(DrawMode::kFont);
	sbGradientBrush->Tag              = dd.DrawModeToInt(DrawMode::kGradientBrush);
	sbMultiDraw->Tag                  = dd.DrawModeToInt(DrawMode::kMulti);
	sbFloodFill->Tag                  = dd.DrawModeToInt(DrawMode::kFloodFill);
	sbRandomDraw->Tag                 = dd.DrawModeToInt(DrawMode::kRandom);
	sbPicker->Tag                     = dd.DrawModeToInt(DrawMode::kPicker);
}


void TfrmMain::InitFrames()
{
	FrameGradientPanel = new TframeGradient(this);
	FrameGradientPanel->Parent       = tsGradients;
	FrameGradientPanel->Align        = alClient;
	FrameGradientPanel->OnCopy       = CopyToGradientBrush;
	FrameGradientPanel->OnFromCustom = CopyFromCustom;
	FrameGradientPanel->OnFromShades = CopyFromShades;
	FrameGradientPanel->SetGuiLanguageText();

	FramePalettePanel = new TframePalette(this);
	FramePalettePanel->Parent        = tsPalette;
	FramePalettePanel->Align         = alClient;
	FramePalettePanel->OnColourClick = std::bind(PaletteColourSelected, std::placeholders::_1, std::placeholders::_2);
	FramePalettePanel->OnColourMove  = std::bind(PaletteColourOver, std::placeholders::_1);

	FramePalettePanel->Init();

	FrameFontPanel = new TframeFont(this);
	FrameFontPanel->Parent  = pbFont;
	FrameFontPanel->Align   = alClient;

	FrameLayerPanel = new TframeLayers(this);
	FrameLayerPanel->Parent  = pLayers;
	FrameLayerPanel->Align   = alClient;
	FrameLayerPanel->OnClose = std::bind(OnLayerPanelClose, std::placeholders::_1);
	FrameLayerPanel->OnMenu  = std::bind(OnLayerMenuItem, std::placeholders::_1);
	FrameLayerPanel->SetGuiLanguageText();
	FrameLayerPanel->ParentMatrix = thematrix;

	FrameUndoPanel = new TframeUndos(this);
	FrameUndoPanel->Parent         = pUndoToolbar;
	FrameUndoPanel->Align          = alClient;
	FrameUndoPanel->OnUndoSelected = std::bind(OnUndoSelected, std::placeholders::_1);

	FrameQuickData = new TframeSimpleExport(this);
	FrameQuickData->Parent         = pQuickData;
	FrameQuickData->Align          = alClient;
	FrameQuickData->OnChange       = std::bind(QuickDataChange, std::placeholders::_1);
	FrameQuickData->SetGuiLanguageText();
}


int TfrmMain::GetSelectedFrame()
{
	return tbFrames->Position - 1;
}


void TfrmMain::ManageUIControls(bool shouldoverride, bool setto)
{
	bool normal_false = false;
	bool normal_true  = true;

	if (shouldoverride)
	{
		normal_false = setto;
		normal_true  = setto;
	}

	if (thematrix->AnimPlaying)
	{
		normal_true = false;
	}

	if (thematrix->Details.Width != thematrix->Details.Height)
	{
		sbRotateL->Enabled = normal_false;
		sbRotateR->Enabled = normal_false;
		miRotateL->Enabled = normal_false;
		miRotateR->Enabled = normal_false;
	}
	else
	{
		sbRotateL->Enabled = normal_true;
		sbRotateR->Enabled = normal_true;
		miRotateL->Enabled = normal_true;
		miRotateR->Enabled = normal_true;
	}

	sbRotateAny->Enabled         = normal_true;
	cbRotateAngle->Enabled       = normal_true;
	cbRotateCount->Enabled       = normal_true;

	bLockFrame->Enabled          = normal_true;

	// ===========================================================================

	sbBuild->Enabled             = normal_true;

	if (thematrix->Details.Available)
	{
		sbSave->Enabled         = normal_true;
		sbExport->Enabled       = normal_true;
		sbGenerateCode->Enabled = normal_true;
	}
	else
	{
		sbSave->Enabled         = normal_false;
		sbExport->Enabled       = normal_false;
		sbGenerateCode->Enabled = normal_false;
	}

	sbOpen->Enabled              = normal_true;
	sbExport->Enabled            = normal_true;
	sbPixelSize->Enabled         = normal_true;
	sbPixelShape->Enabled        = normal_true;
	sbPreset->Enabled            = normal_true;

	miUndo->Enabled              = normal_true;
	miCopy->Enabled              = normal_true;
	miCopyFromPrevious->Enabled  = normal_true;
	miCopyMultiple->Enabled      = normal_true;
	miPaste->Enabled             = normal_true;
	miPasteSpecial->Enabled      = normal_true;
	miBrushActions->Enabled      = normal_true;

	miPopoutPreview->Enabled     = normal_true;

	sbClear->Enabled             = normal_true;
	sbFlip->Enabled              = normal_true;
	sbMirror->Enabled            = normal_true;
	sbInvert->Enabled            = normal_true;
	miFlip->Enabled              = normal_true;
	miMirror->Enabled            = normal_true;
	miInvert->Enabled            = normal_true;
	miFlipAllFrames->Enabled     = normal_true;
	miMirrorAllFrames->Enabled   = normal_true;
	miInvertAllFrames->Enabled   = normal_true;
	sbScrollLeft->Enabled        = normal_true;
	sbScrollRight->Enabled       = normal_true;
	sbScrollUp->Enabled          = normal_true;
	sbScrollDown->Enabled        = normal_true;
	miShiftLeft->Enabled         = normal_true;
	miShiftRight->Enabled        = normal_true;
	miShiftUp->Enabled           = normal_true;
	miShiftDown->Enabled         = normal_true;
	miAddComment->Enabled        = normal_true;

	// bit of hack for when ignored pixel mode active :)
	if (thematrix->GetIgnoredPixelsMode())
	{
		miIgnoredPixels->Enabled            = true;
		miSetIgnoredPixels->Enabled         = true;
		miSetIgnoredFromPattern->Enabled = true;
		miClearAllIgnoredPixels->Enabled    = true;
	}
	else
	{
		miIgnoredPixels->Enabled            = normal_true;
		miSetIgnoredPixels->Enabled         = normal_true;
		miSetIgnoredFromPattern->Enabled = normal_true;
		miClearAllIgnoredPixels->Enabled    = normal_true;
	}

	if (thematrix->AnimPlaying)
	{
		bPlayAnimation->Enabled = false;
		bStopAnimation->Enabled = true;

		frmPreviewPopout->bPlayAnimation->Enabled = false;
		frmPreviewPopout->bStopAnimation->Enabled = true;
	}
	else
	{
		bPlayAnimation->Enabled = true;
		bStopAnimation->Enabled = false;

		frmPreviewPopout->bPlayAnimation->Enabled = true;
		frmPreviewPopout->bStopAnimation->Enabled = false;
	}

	bPreviousFrame->Enabled = normal_true;
	bStartFrame->Enabled = normal_true;
	bEndFrame->Enabled = normal_true;
	bNextFrame->Enabled = normal_true;

	frmPreviewPopout->bStartFrame->Enabled = normal_true;
	frmPreviewPopout->bEndFrame->Enabled = normal_true;
	frmPreviewPopout->bNextFrame->Enabled = normal_true;
	frmPreviewPopout->bPreviousFrame->Enabled = normal_true;

	bAddFrame->Enabled           = normal_true;
	bAddFrameCopy->Enabled       = normal_true;
	bAddFrameMultiple->Enabled   = normal_true;

	miAddFrame->Enabled           = normal_true;
	miAddFrameCopy->Enabled       = normal_true;
	miAddFrameMultiple->Enabled   = normal_true;

	if (thematrix->AnimPlaying)
	{
		bDeleteFrame->Enabled           = false;
		bDeleteMultipleFrames->Enabled  = false;

		miDeleteFrame->Enabled          = false;
		miDeleteMultipleFrames->Enabled = false;
	}
	else
	{
		bDeleteFrame->Enabled           = (thematrix->GetFrameCount() > 1);
		bDeleteMultipleFrames->Enabled  = (thematrix->GetFrameCount() > 1);

		miDeleteFrame->Enabled          = (thematrix->GetFrameCount() > 1);
		miDeleteMultipleFrames->Enabled = (thematrix->GetFrameCount() > 1);
	}

	bLightbox->Enabled                 = normal_true;

	sbMouseMode->Enabled               = normal_true;
	sbCopy->Enabled                    = normal_true;
	sbNewBrush->Enabled                = normal_true;
	sbFilledRectangle->Enabled         = normal_true;
	sbFrame->Enabled                   = normal_true;
	sbEmptyCircle->Enabled             = normal_true;
	sbFilledCircle->Enabled            = normal_true;
	sbLine->Enabled                    = normal_true;
	sbMultiDraw->Enabled               = normal_true;
	sbFloodFill->Enabled               = normal_true;
	sbFont->Enabled                    = normal_true;

	cbMirrorMode->Enabled              = normal_true;

	sbPatternSpiral->Enabled           = normal_true;
	sbPatternCircle->Enabled           = normal_true;
	sbPatternSplitRing->Enabled        = normal_true;
	sbPatternPetals->Enabled           = normal_true;
	sbPatternGrid->Enabled             = normal_true;
	sbPatternPyramid->Enabled          = normal_true;
	sbPatternLeftTriangle->Enabled     = normal_true;
	sbPatternRightTriangle->Enabled    = normal_true;

	miMouseMode->Enabled               = normal_true;
	miNewBrush->Enabled                = normal_true;
	miDrawCopy->Enabled                = normal_true;
	miFilledRectangle->Enabled         = normal_true;
	miFrame->Enabled                   = normal_true;
	miEmptyCircle->Enabled             = normal_true;
	miFilledCircle->Enabled            = normal_true;
	miLine->Enabled                    = normal_true;
	miMultiDraw->Enabled               = normal_true;
	miFloodFill->Enabled               = normal_true;
	miFont->Enabled                    = normal_true;

	miPatternSpiral->Enabled           = normal_true;
	miPatternCircle->Enabled           = normal_true;
	miPatternSplitRing->Enabled        = normal_true;
	miPatternPetals->Enabled           = normal_true;
	miPatternGrid->Enabled             = normal_true;
	miPatternPyramid->Enabled          = normal_true;
	miPatternLeftTriangle->Enabled     = normal_true;
	miPatternRightTriangle->Enabled    = normal_true;

	miAppend->Enabled                  = normal_true;
	miMerge->Enabled                   = normal_true;

	miSave->Enabled                    = normal_true;
	miSaveAs->Enabled                  = normal_true;
	miSaveSingleFrame->Enabled         = normal_true;
	miSaveRange->Enabled               = normal_true;
	miImportInToCurrent->Enabled       = normal_true;
	miExport->Enabled                  = normal_true;
	miExportToBitmap->Enabled          = normal_true;
	miExportAnimationToBitmap->Enabled = normal_true;
	miExportToGIF->Enabled             = normal_true;
	miCodeTemplates->Enabled           = normal_true;

	miLockAll->Enabled                 = normal_true;
	miUnlockAll->Enabled               = normal_true;
	miToggleLockStatus->Enabled        = normal_true;

	miClearLayer->Enabled              = normal_true;
	miFlattenLayers->Enabled           = normal_true;

	sbSave->Enabled                    = normal_true;

	tbFrames->Enabled                  = normal_true;
	frmPreviewPopout->tbFrames->Enabled = normal_true;

	miClearAllFrames->Enabled          = normal_true;
	miClearAllFramesLayer->Enabled     = normal_true;

	miAutomate->Enabled                = normal_true;
	miOptimiseData->Enabled            = normal_true;

	miChangeColoursFrame->Enabled      = normal_true;
	miChangeColoursLayer->Enabled      = normal_true;
	miChangeColoursAll->Enabled        = normal_true;

	miCopyCurrentTo->Enabled           = normal_true;
	miRestoreCurrentFrom->Enabled      = normal_true;
	miExportUserMemories->Enabled      = normal_true;
	miClearAllUserMemories->Enabled    = normal_true;

	miCountColours->Enabled            = normal_true;

	if (thematrix->Details.Mode == MatrixMode::kMono)
	{
		sbGradient->Enabled               = normal_false;
		miClearAllFramesGradient->Enabled = normal_false;
		miGradientFillFrame->Enabled      = normal_false;
		sbRandomDraw->Enabled             = normal_false;
		miGradientAllFrames->Enabled      = normal_false;
		sbPicker->Enabled                 = normal_false;
		sbGradientBrush->Enabled          = normal_false;

		miGradient->Enabled               = normal_false;
		miRandomDraw->Enabled             = normal_false;
		miPicker->Enabled                 = normal_false;
		miGradientBrush->Enabled          = normal_false;
	}
	else
	{
		sbGradient->Enabled               = normal_true;
		miClearAllFramesGradient->Enabled = normal_true;
		miGradientFillFrame->Enabled      = normal_true;
		sbRandomDraw->Enabled             = normal_true;
		miGradientAllFrames->Enabled      = normal_true;
		sbGradientBrush->Enabled          = normal_true;

		miGradient->Enabled               = normal_true;
		miRandomDraw->Enabled             = normal_true;
		miPicker->Enabled                 = normal_true;
		miGradientBrush->Enabled          = normal_true;

		sbPicker->Enabled                 = thematrix->Details.Mode == MatrixMode::kRGB;
	}
}


void TfrmMain::ConfigureOpenDialog(int mode)
{
	switch (mode)
	{
	case CLoadProject:
		odMain->DefaultExt = L".leds";
		odMain->Filter     = Utility::WS2US(GLanguageHandler->Text[kLEDMatrixStudioProjects] + L" (*.leds)|*.leds");
		odMain->InitialDir = GSystemSettings->App.LastLoadLocation.c_str();
		break;
	case CLoadIgnorePixels:
		odMain->DefaultExt = L".ledsip";
		odMain->Filter     = Utility::WS2US(GLanguageHandler->Text[kLEDMatrixStudioIgnorePixelFiles] + L" (*.ledsip)|*.ledsip");
		odMain->InitialDir = GSystemSettings->App.LastLoadLocation.c_str();
		break;
	}
}


void TfrmMain::ConfigureSaveDialog(int mode)
{
	switch (mode)
	{
	case CSaveProject:
		sdMain->InitialDir = GSystemSettings->App.LastSaveLocation.c_str();
		sdMain->Filter     = Utility::WS2US(GLanguageHandler->Text[kLEDMatrixStudioProjects] + L" (*.leds)|*.leds");
		sdMain->DefaultExt = L".leds";
		break;
	case CSaveFont:
		sdMain->DefaultExt = L".ledsfont";
		sdMain->FileName   = Utility::WS2US(L"font_" + std::to_wstring(thematrix->Details.Width) + L"x" + std::to_wstring(thematrix->Details.Height));
		sdMain->Filter     = Utility::WS2US(GLanguageHandler->Text[kLEDMatrixStudioFont] + L" (*.ledsfont)|*.ledsfont");
		sdMain->InitialDir = ExtractFilePath(Application->ExeName) + "fonts\\";
		break;
	case CSaveIgnorePixels:
		sdMain->DefaultExt = L".ledsip";
		sdMain->Filter     = Utility::WS2US(GLanguageHandler->Text[kLEDMatrixStudioIgnorePixelFiles] + L" (*.ledsip)|*.ledsip");
		sdMain->InitialDir = GSystemSettings->App.LastLoadLocation.c_str();
		sdMain->FileName   = Utility::WS2US(L"ignore_" + std::to_wstring(thematrix->Details.Width) + L"x" + std::to_wstring(thematrix->Details.Height));
		break;
	}
}


#pragma region FormPreview_Callbacks
void TfrmMain::PreviewWindowCommand(int command)
{
	PlaybackCommand(command);
}


void TfrmMain::PreviewWindowChangeFrame(int frame)
{
	thematrix->SetAndShowCurrentFrame(frame);

	SetFrameCaption(frame);
}
#pragma end_region


#pragma region FrameGradient_EventCallbacks
void __fastcall TfrmMain::CopyToGradientBrush()
{
	int count = FrameGradientPanel->GetColourCount();

	if (count != 0)
	{
		thematrix->ClearGradient();

		for (int t = 0; t < count; t++)
		{
			thematrix->AddGradient(FrameGradientPanel->GetColour(t));
		}

       	UpdateDrawModeCaption(lSelectedTool->Tag);
	}
}


void __fastcall TfrmMain::CopyFromCustom()
{
	for (int t = 0; t < 16; t++)
	{
		FrameGradientPanel->AddColour(_RGBPalette[t]->Brush->Color);
	}
}


void __fastcall TfrmMain::CopyFromShades()
{
	for (int t = 0; t < 16; t++)
	{
		FrameGradientPanel->AddColour(_RGBShade[t]->Brush->Color);
	}
}
#pragma end_region


#pragma region FrameLayer_EventCallbacks
void __fastcall TfrmMain::OnLayerPanelClose(TframeLayers *Sender)
{
	miToggleLayoutPanelClick(nullptr);
}


void __fastcall TfrmMain::OnLayerMenuItem(int item)
{
	switch (item)
	{
	case 1:
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kClearLayer] + L" \"" +
					   thematrix->GetLayerName(thematrix->GetCurrentLayer()) +
					   L"\"?\n\n" +
					   GLanguageHandler->Text[kThisCannotBeUndone]), mtWarning, mbYesNo, 0) == mrYes)
			thematrix->ClearCurrentLayer();

        break;
	}
}
#pragma end_region


#pragma region FramePalette_EventCallbacks
void __fastcall TfrmMain::PaletteColourOver(int colour)
{
	std::wstring hex = IntToHex(ColourUtility::RGBConvertTo32(colour, RGBMode::kRGB, LeastSignificantBit::kBottomRight, 100), 6).c_str();

	lPixelColour->Caption = Utility::WS2US(GSystemSettings->App.HexPrefix +
							hex +
							L" (" + ColourUtility::RGBConvertToSplit(colour, RGBMode::kRGBSimple, 100, NumberFormat::kDecimal, L"", L" ", ColourSpace::kRGB32) + L")");
}


void __fastcall TfrmMain::PaletteColourSelected(int button, int colour)
{
	if (thematrix->Details.Mode == MatrixMode::kRGB)
	{
		switch (button)
		{
		case 0:
			sSelectionLMB->Brush->Color = TColor(colour);

			thematrix->LEDRGBColours[CMouseLeft]   = colour;
			break;
		case 1:
			sSelectionMMB->Brush->Color = TColor(colour);

			thematrix->LEDRGBColours[CMouseMiddle] = colour;
			break;
		case  2:
			sSelectionRMB->Brush->Color = TColor(colour);

			thematrix->LEDRGBColours[CMouseRight]  = colour;
			break;
		}

		thematrix->SetMouseButtonColours(thematrix->LEDRGBColours[CMouseLeft],
										 thematrix->LEDRGBColours[CMouseMiddle],
										 thematrix->LEDRGBColours[CMouseRight]);

		GenerateShades(colour);
	}
}
#pragma end_region


#pragma region FrameQuickData_EventCallbacks
void __fastcall TfrmMain::QuickDataChange(TframeSimpleExport *Sender)
{
	UpdateData();
}
#pragma end_region


#pragma region FrameUndos_EventCallbacks
void __fastcall TfrmMain::OnUndoSelected(int undo)
{
	thematrix->SetFromUndo(undo);
}
#pragma end_region


#pragma region Matrix_EventCallbacks
void __fastcall TfrmMain::MatrixOnChange(TheMatrix *Sender)
{
	if (thematrix->GetFrameCount() == 0)
	{
	}
	else
	{
		tbFrames->Max = thematrix->GetFrameCount(); // last frame available
		frmPreviewPopout->tbFrames->Max = tbFrames->Max;
	}

	miUndo->Enabled = thematrix->CanUndo;
	miRedo->Enabled = thematrix->CanRedo;

	if (pQuickData->Visible)
	{
		UpdateData();
	}
}


void __fastcall TfrmMain::MatrixOnLayerChange(TheMatrix *Sender)
{
	FrameLayerPanel->UpdateLayerTable();
}


void __fastcall TfrmMain::MatrixOnSizeChange(TheMatrix *Sender)
{
	UpdateMemoryUsage();
}


void __fastcall TfrmMain::MatrixOnDisplayBufferCopied(TheMatrix *Sender)
{
	if (pQuickData->Visible)
	{
		UpdateData();
	}
}


void __fastcall TfrmMain::MatrixOnNewFrameDisplayed(TheMatrix *Sender)
{
	tbFrames->Max                  = thematrix->GetFrameCount();

	bDeleteFrame->Enabled          = (thematrix->GetFrameCount() > 1);
	bDeleteMultipleFrames->Enabled = (thematrix->GetFrameCount() > 1);

	if (thematrix->GetCurrentFrame() >= 0 &&
		tbFrames->Position != thematrix->GetCurrentFrame() + 1)
	{
		tbFrames->Position = thematrix->GetCurrentFrame() + 1;
	}

	frmPreviewPopout->tbFrames->Max      = tbFrames->Max;
	frmPreviewPopout->tbFrames->Position = tbFrames->Position;

	SetFrameCaption(GetSelectedFrame());

	// move to onnewframedisplayed
	if (thematrix->IsLocked())
	{
		bLockFrame->Tag = 1;
	}
	else
	{
		bLockFrame->Tag = 0;
	}

	bLockFrame->ImageIndex = 24 + bLockFrame->Tag;

	miUndo->Enabled = thematrix->CanUndo;
	miRedo->Enabled = thematrix->CanRedo;

	if (pUndoToolbar->Visible)
	{
		FrameUndoPanel->SetUndos(thematrix->GetUndoCount());
	}
}


void __fastcall TfrmMain::MatrixOnColourChange(TheMatrix *Sender)
{
	switch (thematrix->Details.Mode)
	{
	case MatrixMode::kNone:
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		break;
	case MatrixMode::kRGB:
		sSelectionLMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseLeft]);
		sSelectionMMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseMiddle]);
		sSelectionRMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseRight]);
		break;
	case MatrixMode::kRGB3BPP:
		sSelectionLMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[thematrix->LEDRGBColours[CMouseLeft]]);
		sSelectionMMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[thematrix->LEDRGBColours[CMouseMiddle]]);
		sSelectionRMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[thematrix->LEDRGBColours[CMouseRight]]);
		break;
	}
}


void __fastcall TfrmMain::MatrixOnNew3bppColours(TheMatrix *Sender)
{
	for (int i = 0; i < 8; i++)
	{
		_RGB3ppPalette[i]->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[i]);
	}
}


void __fastcall TfrmMain::MatrixOnMouseOver(int x, int y)
{
	OldMouseX = x;
	OldMouseY = y;

	if (x >= 0 && y >= 0 && x < thematrix->Details.Width && y < thematrix->Details.Height)
	{
		switch (thematrix->Details.Mode)
		{
		case MatrixMode::kRGB:
		{
			std::wstring caption = L"X: " + std::to_wstring(x + 1) +
								   L"  Y: " + std::to_wstring(y + 1) +
								   L"  " + GLanguageHandler->Text[kData] +
								   L": " + GSystemSettings->App.HexPrefix;

			 statusMain->SimpleText = caption.c_str() +
									  IntToHex(ColourUtility::RGBConvertTo32(thematrix->MatrixLayers[thematrix->GetCurrentLayer()]->Cells[GetSelectedFrame()]->Grid[y * thematrix->Details.Width + x], RGBMode::kRGB, LeastSignificantBit::kBottomRight, 100), 6);
			 break;
		}
		case MatrixMode::kRGB3BPP:
		{
			std::wstring caption = L"X: " + std::to_wstring(x + 1) +
								   L"  Y: " + std::to_wstring(y + 1) +
								   L"  " + GLanguageHandler->Text[kData] +
								   L": " + GSystemSettings->App.HexPrefix;

			statusMain->SimpleText = caption.c_str() +
									 IntToHex(thematrix->MatrixLayers[thematrix->GetCurrentLayer()]->Cells[GetSelectedFrame()]->Grid[y * thematrix->Details.Width + x], 2);
			break;
		}

		default:
			statusMain->SimpleText = IntToStr(x) + L", " + IntToStr(y);
		}

		if (lPixelColour->Visible)
		{
			if (thematrix->Details.Mode == MatrixMode::kRGB)
			{
				std::wstring caption =  GSystemSettings->App.HexPrefix +
									   IntToHex(ColourUtility::RGBConvertTo32(thematrix->MatrixLayers[thematrix->GetCurrentLayer()]->Cells[GetSelectedFrame()]->Grid[y * thematrix->Details.Width + x], RGBMode::kRGB, LeastSignificantBit::kBottomRight, 100), 6).c_str() +
									   L" (" +
									   ColourUtility::RGBConvertToSplit(thematrix->MatrixLayers[thematrix->GetCurrentLayer()]->Cells[GetSelectedFrame()]->Grid[y * thematrix->Details.Width + x], RGBMode::kRGBSimple, 100, NumberFormat::kDecimal, L"", L" ", ColourSpace::kRGB32) +
									   L")";

				lPixelColour->Caption = caption.c_str();
			}
			else
			{
				lPixelColour->Caption = GSystemSettings->App.HexPrefix.c_str() +
									   IntToHex(thematrix->MatrixLayers[thematrix->GetCurrentLayer()]->Cells[GetSelectedFrame()]->Grid[y * thematrix->Details.Width + x], 2);
			}
		}
	}
}


void __fastcall TfrmMain::MatrixOnPreviewMouseDown(int x, int y)
{
	puPreview->Popup(Left + x + 10, Top + y + 150);
}


void __fastcall TfrmMain::MatrixOnDebug(TheMatrix *sender, const std::wstring s)
{
	Caption = s.c_str();
}
#pragma end_region


#pragma region setlanguagetext
void TfrmMain::SetGuiLanguageText()
{
	DrawModes.push_back(GLanguageHandler->Text[kDraw]);
	DrawModes.push_back(GLanguageHandler->Text[kFilledBox]);
	DrawModes.push_back(GLanguageHandler->Text[kEmptyBox]);
	DrawModes.push_back(GLanguageHandler->Text[kLine]);
	DrawModes.push_back(GLanguageHandler->Text[kFont]);
	DrawModes.push_back(GLanguageHandler->Text[kEmptyCircle]);
	DrawModes.push_back(GLanguageHandler->Text[kFilledCircle]);
	DrawModes.push_back(GLanguageHandler->Text[kRandomBrush]);
	DrawModes.push_back(GLanguageHandler->Text[kMultiDraw]);
	DrawModes.push_back(GLanguageHandler->Text[kColourPicker]);
	DrawModes.push_back(GLanguageHandler->Text[kCopyBrush]);
	DrawModes.push_back(GLanguageHandler->Text[kPasteBrush]);
	DrawModes.push_back(GLanguageHandler->Text[kGradientBrush]);
	DrawModes.push_back(GLanguageHandler->Text[kFloodFill]);
	DrawModes.push_back(GLanguageHandler->Text[kSpiral]);
	DrawModes.push_back(GLanguageHandler->Text[kRing]);
	DrawModes.push_back(GLanguageHandler->Text[kSplitRing]);
	DrawModes.push_back(GLanguageHandler->Text[kPetals]);
	DrawModes.push_back(GLanguageHandler->Text[kGrid]);
	DrawModes.push_back(GLanguageHandler->Text[kPyramid]);
	DrawModes.push_back(GLanguageHandler->Text[kLeftTriangle]);
	DrawModes.push_back(GLanguageHandler->Text[kRightTriangle]);

	lSelectedTool->Caption = GetDrawModeText(0).c_str();

	//

	sbBuild->Caption = GLanguageHandler->Text[kNew].c_str();
	sbOpen->Caption = GLanguageHandler->Text[kOpen].c_str();
	sbSave->Caption = GLanguageHandler->Text[kSave].c_str();
	sbExport->Caption = GLanguageHandler->Text[kExport].c_str();
	sbGenerateCode->Caption = GLanguageHandler->Text[kGenerateCode].c_str();
	sbPreset->Caption = GLanguageHandler->Text[kPreset].c_str();

	sbClear->Caption = GLanguageHandler->Text[kClear].c_str();
	sbMirror->Caption = GLanguageHandler->Text[kMirror].c_str();
	sbFlip->Caption = GLanguageHandler->Text[kFlip].c_str();
	sbInvert->Caption = GLanguageHandler->Text[kInvert].c_str();
	sbRotateAny->Caption = GLanguageHandler->Text[kRotate].c_str();

	cbMirrorMode->Items->Add(GLanguageHandler->Text[kNone].c_str());
	cbMirrorMode->Items->Add(GLanguageHandler->Text[kHorizontal].c_str());
	cbMirrorMode->Items->Add(GLanguageHandler->Text[kVertical].c_str());
	cbMirrorMode->ItemIndex = 0;

	// popup menus

	miGradientColour0->Caption = Utility::WS2US(GLanguageHandler->Text[kColour] + L" 0");
	Colour11->Caption = Utility::WS2US(GLanguageHandler->Text[kColour] + L" 1");
	Colour21->Caption = Utility::WS2US(GLanguageHandler->Text[kColour] + L" 2");
	Colour31->Caption = Utility::WS2US(GLanguageHandler->Text[kColour] + L" 3");

	MenuItem3->Caption = GLanguageHandler->Text[kGradients].c_str();
	miLoadGradients->Caption = GLanguageHandler->Text[kLoad].c_str();
	miSaveGradient->Caption = GLanguageHandler->Text[kSaveCurrent].c_str();

	MenuItem1->Caption = GLanguageHandler->Text[kFonts].c_str();
	miLoadFont->Caption = GLanguageHandler->Text[kLoad].c_str();
	Fontviewer2->Caption = GLanguageHandler->Text[kFontViewer].c_str();
	miFontWrap->Caption = GLanguageHandler->Text[kFontWrap].c_str();

	Presets1->Caption = GLanguageHandler->Text[kPresets].c_str();
	miLoadPreset->Caption = GLanguageHandler->Text[kLoad].c_str();
	miPresetSaveCurrent->Caption = GLanguageHandler->Text[kSaveCurrent].c_str();

	PixelSize1->Caption = GLanguageHandler->Text[kPixelSize].c_str();
	miPixelTiny->Caption = GLanguageHandler->Text[kTiny].c_str();
	miPixelSmall->Caption = GLanguageHandler->Text[kSmall].c_str();
	miPixelMedium->Caption = GLanguageHandler->Text[kMedium].c_str();
	miPixelLarge->Caption = GLanguageHandler->Text[kLarge].c_str();
	miPixelVeryLarge->Caption = GLanguageHandler->Text[kMassive].c_str();
	miPixelUltra->Caption = GLanguageHandler->Text[kUltra].c_str();
	miPixelMegaUltra->Caption = GLanguageHandler->Text[kXUltra].c_str();
	miPixelAuto->Caption = GLanguageHandler->Text[kAuto].c_str();

	Previewsize2->Caption = GLanguageHandler->Text[kPreviewSize].c_str();
	Previewview2->Caption = GLanguageHandler->Text[kPreviewView].c_str();
	miPUPreviewViewSquare->Caption = GLanguageHandler->Text[kSquare].c_str();
	miPUPreviewViewRadial->Caption = GLanguageHandler->Text[kRadial].c_str();
	miPUPreviewViewRadialTQ->Caption = GLanguageHandler->Text[kRadialThreeQuarters].c_str();
	miPUPreviewViewSemiCircle->Caption = GLanguageHandler->Text[kSemicircle].c_str();
	miPUPreviewViewSemiCircleInverted->Caption = GLanguageHandler->Text[kSemicircleIinverted].c_str();
	PreviewvoidRadialSemicircle1->Caption = GLanguageHandler->Text[kPreviewVoidRRadialSsemicircle].c_str();

	MenuItem5->Caption = GLanguageHandler->Text[kPixelShape].c_str();
	miPixelShapeSquare->Caption = GLanguageHandler->Text[kSquare].c_str();
	miPixelShapeRound->Caption = GLanguageHandler->Text[kRound].c_str();
	miPixelShapeRoundRect->Caption = GLanguageHandler->Text[kSquareR].c_str();

	MenuItem10->Caption = GLanguageHandler->Text[kRandomness].c_str();
	miRandomnessTiny->Caption = GLanguageHandler->Text[kTiny].c_str();
	Small2->Caption = GLanguageHandler->Text[kSmall].c_str();
	Medium1->Caption = GLanguageHandler->Text[kMedium].c_str();
	Large2->Caption = GLanguageHandler->Text[kLarge].c_str();
	Massive1->Caption = GLanguageHandler->Text[kMassive].c_str();

	MenuItem8->Caption = GLanguageHandler->Text[kBrushSize].c_str();
	miBrushSizeSmall->Caption = Utility::WS2US(GLanguageHandler->Text[kSmall] + L" (1x1)");
	Large1->Caption = Utility::WS2US(GLanguageHandler->Text[kMedium] + L" (2x2)");
	Large3x3pixels1->Caption = Utility::WS2US(GLanguageHandler->Text[kLarge] + L" (3x3)");

	miGradientSelectRGB->Caption = Utility::WS2US(GLanguageHandler->Text[kSelectColour] + L"...");
	miGradSetRow->Caption = Utility::WS2US(GLanguageHandler->Text[kSetRowToSelectedColour] + L"...");
	miGradFrom->Caption = GLanguageHandler->Text[kGradientFromTopBottom].c_str();
	miGradientBottomTop->Caption = GLanguageHandler->Text[kGradientFlip].c_str();

	Playbackspeed1->Caption = GLanguageHandler->Text[kPlaybackSpeed].c_str();
	miPlaybackSpeedCustom->Caption = GLanguageHandler->Text[kCustom].c_str();
	Setcustomspeed1->Caption = GLanguageHandler->Text[kSetCustomSpeed].c_str();

	//

	tsPalette->Caption = GLanguageHandler->Text[kPalette].c_str();
	tsGradients->Caption = GLanguageHandler->Text[kGradients].c_str();

	lMirror->Caption = GLanguageHandler->Text[kMirrorDraw].c_str();

	// main menu
	File1->Caption = GLanguageHandler->Text[kFile].c_str();
	New1->Caption = GLanguageHandler->Text[kNew].c_str();
	Load1->Caption = Utility::WS2US(GLanguageHandler->Text[kOpen] + L"...");
	miReopenMenu->Caption = GLanguageHandler->Text[kRecentFiles].c_str();
	miImportFromBitmap->Caption = Utility::WS2US(GLanguageHandler->Text[kImportFromBitmap] + L"...");
	miImportFromGIF->Caption = Utility::WS2US(GLanguageHandler->Text[kImportFromGIF] + L"...");
	miImportInToCurrent->Caption = GLanguageHandler->Text[kImportIntoCurrentFrame].c_str();
	miAppend->Caption = Utility::WS2US(GLanguageHandler->Text[kAppendToAnimation] + L"...");
	miMerge->Caption = Utility::WS2US(GLanguageHandler->Text[kMerge] + L"...");
	miSave->Caption = Utility::WS2US(GLanguageHandler->Text[kSave] + L"...");
	miSaveAs->Caption = Utility::WS2US(GLanguageHandler->Text[kSaveAs] + L"...");
	miSaveSingleFrame->Caption = Utility::WS2US(GLanguageHandler->Text[kSaveSingleFrameAs] + L"...");
	miSaveRange->Caption = Utility::WS2US(GLanguageHandler->Text[kSaveRangeAs] + L"...");
	miSaveAsFont->Caption = Utility::WS2US(GLanguageHandler->Text[kSaveLEDMatrixStudioFont] + L"...");
	miExportToBitmap->Caption = Utility::WS2US(GLanguageHandler->Text[kExportToImages] + L"...");
	miExportAnimationToBitmap->Caption = Utility::WS2US(GLanguageHandler->Text[kExportAnimationToBitmap] + L"...");
	miExportToGIF->Caption = Utility::WS2US(GLanguageHandler->Text[kExportAnimationToGIF] + L"...");
	Preferences1->Caption = Utility::WS2US(GLanguageHandler->Text[kPreferences] + L"...");
	Exit1->Caption = Utility::WS2US(GLanguageHandler->Text[kExit]);
	//
	Edit1->Caption = GLanguageHandler->Text[kEdit].c_str();
	miUndo->Caption = GLanguageHandler->Text[kUndo].c_str();
	miRedo->Caption = GLanguageHandler->Text[kRedo].c_str();
	miCopy->Caption = GLanguageHandler->Text[kCopy].c_str();
	miCopyFromPrevious->Caption = GLanguageHandler->Text[kCopyFromPrevious].c_str();
	miCopyMultiple->Caption = Utility::WS2US(GLanguageHandler->Text[kCopyMultiple] + L"...");
	miPaste->Caption = GLanguageHandler->Text[kPaste].c_str();
	miPasteSpecial->Caption = GLanguageHandler->Text[kPasteSpecial].c_str();
	Copyandshiftleft1->Caption = GLanguageHandler->Text[kPasteShiftLeft].c_str();
	Copyandshiftright1->Caption = GLanguageHandler->Text[kPasteShiftRight].c_str();
	Copyandshiftup1->Caption = GLanguageHandler->Text[kPasteShiftUp].c_str();
	Copyandshiftdown1->Caption = GLanguageHandler->Text[kPasteShiftDown].c_str();
	miBrushActions->Caption = GLanguageHandler->Text[kBrushActions].c_str();
	Rotateanticlockwise1->Caption = GLanguageHandler->Text[kRotateAnticlockwise].c_str();
	Rotateclockwise1->Caption = GLanguageHandler->Text[kRotateClockwise].c_str();
	miBrushFlip->Caption = GLanguageHandler->Text[kFlip].c_str();
	Mirror1->Caption = GLanguageHandler->Text[kMirror].c_str();
	Invert1->Caption = GLanguageHandler->Text[kInvert].c_str();
	Pasteintoeveryframe1->Caption = GLanguageHandler->Text[kPasteEveryFrame].c_str();
	Pasteintoeveryframetransparent1->Caption = GLanguageHandler->Text[kPasteEveryFrameTransparent].c_str();
	miShiftLeft->Caption = GLanguageHandler->Text[kShiftLeft].c_str();
	miShiftRight->Caption = GLanguageHandler->Text[kShiftRight].c_str();
	miShiftUp->Caption = GLanguageHandler->Text[kShiftUp].c_str();
	miShiftDown->Caption = GLanguageHandler->Text[kShiftDown].c_str();
	miRotateL->Caption = GLanguageHandler->Text[kRotateAnticlockwise].c_str();
	miRotateR->Caption = GLanguageHandler->Text[kRotateClockwise].c_str();
	miFlip->Caption = GLanguageHandler->Text[kFlip].c_str();
	miMirror->Caption = GLanguageHandler->Text[kMirror].c_str();
	miInvert->Caption = GLanguageHandler->Text[kInvert].c_str();
	miAddComment->Caption = GLanguageHandler->Text[kEditComment].c_str();
	//
	View1->Caption = GLanguageHandler->Text[kView].c_str();
	miShowAnimationToolbar->Caption = GLanguageHandler->Text[kShowAnimationToolbar].c_str();
	miPaletteGradientToolbar->Caption = GLanguageHandler->Text[kPaletteGradientToolbar].c_str();
	miQuickData->Caption = GLanguageHandler->Text[kQuickDataToolbar].c_str();
	miUndoToolbar->Caption = GLanguageHandler->Text[kUndoToolbar].c_str();
	Backgroundcolour1->Caption = GLanguageHandler->Text[kWorkingAreaBackgroundColour].c_str();
	miCustomBackground->Caption = GLanguageHandler->Text[kCustom].c_str();
	Black1->Caption = GLanguageHandler->Text[kBlack].c_str();
	Darkgrey1->Caption = GLanguageHandler->Text[kDarkGreyDefault].c_str();
	Grey1->Caption = GLanguageHandler->Text[kGrey].c_str();
	Green1->Caption = GLanguageHandler->Text[kGreen].c_str();
	Purple1->Caption = GLanguageHandler->Text[kPurple].c_str();
	Red1->Caption = GLanguageHandler->Text[kRed].c_str();
	White1->Caption = GLanguageHandler->Text[kWhite].c_str();
	miFontMode->Caption = GLanguageHandler->Text[kFontMode].c_str();
	miASCIIStartCode->Caption = GLanguageHandler->Text[kChangeStartASCIICode].c_str();
	miPreviousFrame->Caption = GLanguageHandler->Text[kPreviousFrame].c_str();
	miNextFrame->Caption = GLanguageHandler->Text[kNextFrame].c_str();
	miGridToggle->Caption = GLanguageHandler->Text[kGrid].c_str();
	//
	Preview1->Caption = GLanguageHandler->Text[kPreview].c_str();
	miPreview->Caption = GLanguageHandler->Text[kPreview].c_str();
	PreviewSize1->Caption = GLanguageHandler->Text[kPreviewSize].c_str();
	miIncrementRadially->Caption = GLanguageHandler->Text[kIncrementRadially].c_str();
	miPreviewView->Caption = GLanguageHandler->Text[kPreviewView].c_str();
	miPreviewViewSquare->Caption = GLanguageHandler->Text[kSquare].c_str();
	miPreviewViewRadial->Caption = GLanguageHandler->Text[kRadial].c_str();
	miPreviewViewRadialTQ->Caption = GLanguageHandler->Text[kRadialThreeQuarters].c_str();
	miPreviewViewSemiCircle->Caption = GLanguageHandler->Text[kSemicircle].c_str();
	miPreviewViewSemiCircleInverted->Caption = GLanguageHandler->Text[kSemicircleIinverted].c_str();
	PreviewVoidRadial1->Caption = GLanguageHandler->Text[kPreviewVoidRRadialSsemicircle].c_str();
	Previewoffsetradialsemicircle1->Caption = GLanguageHandler->Text[kPreviewOffsetRadialSemicircle].c_str();
	miPreviewOffsetReverse->Caption = GLanguageHandler->Text[kReverse].c_str();
	miPopoutPreview->Caption = GLanguageHandler->Text[kPopoutPreview].c_str();
	miPreviewAllowDrawing->Caption = GLanguageHandler->Text[kAllowDrawing].c_str();
	//
	Project1->Caption = GLanguageHandler->Text[kProject].c_str();
	miClearAllFramesLayer->Caption = GLanguageHandler->Text[kClearAllFramesCurrentLayer].c_str();
	miClearAllFrames->Caption = GLanguageHandler->Text[kClearAllFramesAllLayers].c_str();
	miClearAllFramesGradient->Caption = GLanguageHandler->Text[kClearAllFramesWithGradient].c_str();
	miFlipAllFrames->Caption = GLanguageHandler->Text[kFlipAllFrames].c_str();
	miMirrorAllFrames->Caption = GLanguageHandler->Text[kMirrorAllFrames].c_str();
	miInvertAllFrames->Caption = GLanguageHandler->Text[kInvertAllFrames].c_str();
	miGradientAllFrames->Caption = GLanguageHandler->Text[kApplyGradientToAllFrames].c_str();
	miIgnoredPixels->Caption = GLanguageHandler->Text[kIgnoredPixels].c_str();
	miSetIgnoredPixels->Caption = GLanguageHandler->Text[kSetIgnoredPixels].c_str();
	miSetIgnoredFromPattern->Caption = Utility::WS2US(GLanguageHandler->Text[kSetFromPattern] + L"...");
	miClearAllIgnoredPixels->Caption = GLanguageHandler->Text[kClearAllIgnoredPixels].c_str();
	miSaveIgnoredPixelsAsPattern->Caption = Utility::WS2US(GLanguageHandler->Text[kSavePattern] + L"...");
	miLoadIgnoredPixelsAsPattern->Caption = Utility::WS2US(GLanguageHandler->Text[kLoadPattern] + L"...");
	miHideIgnoredPixels->Caption = Utility::WS2US(GLanguageHandler->Text[kHideIgnoredPixels] + L"...");
	miFadeFirstLast->Caption = GLanguageHandler->Text[kFadeFirstLast].c_str();
	miExport->Caption = Utility::WS2US(GLanguageHandler->Text[kExport] + L"...");
	miCodeTemplates->Caption = Utility::WS2US(GLanguageHandler->Text[kCodeTemplates] + L"...");
	miUnlockAll->Caption = GLanguageHandler->Text[kUnlockAllFrames].c_str();
	miLockAll->Caption = GLanguageHandler->Text[kLockAllFrames].c_str();
	miToggleLockStatus->Caption = Utility::WS2US(GLanguageHandler->Text[kToggleLockStatusRange] + L"...");
	//
	Draw1->Caption = GLanguageHandler->Text[kDraw].c_str();
	miMouseMode->Caption = GLanguageHandler->Text[kFreehandBrush].c_str();
	miNewBrush->Caption = Utility::WS2US(GLanguageHandler->Text[kCustomBrush] + L"...");
	miDrawCopy->Caption = GLanguageHandler->Text[kCopyPaste].c_str();
	miFilledRectangle->Caption = GLanguageHandler->Text[kFilledRectangle].c_str();
	miFrame->Caption = GLanguageHandler->Text[kEmptyRectangle].c_str();
	miFilledCircle->Caption = GLanguageHandler->Text[kFilledCircle].c_str();
	miEmptyCircle->Caption = GLanguageHandler->Text[kEmptyCircle].c_str();
	miLine->Caption = GLanguageHandler->Text[kLine].c_str();
	miMultiDraw->Caption = GLanguageHandler->Text[kMultidrawOnEachFrame].c_str();
	miFloodFill->Caption = GLanguageHandler->Text[kFill].c_str();
	miFont->Caption = GLanguageHandler->Text[kText].c_str();
	miGradientBrush->Caption = GLanguageHandler->Text[kGradientBrush].c_str();
	miGradient->Caption = GLanguageHandler->Text[kGradient].c_str();
	miRandomDraw->Caption = GLanguageHandler->Text[kRandom].c_str();
	miPicker->Caption = GLanguageHandler->Text[kColourPicker].c_str();
	miPatternSpiral->Caption = GLanguageHandler->Text[kPatternSpiral].c_str();
	miPatternCircle->Caption = GLanguageHandler->Text[kPatternCircle].c_str();
	miPatternSplitRing->Caption = GLanguageHandler->Text[kPatternSplitRing].c_str();
	miPatternPetals->Caption = GLanguageHandler->Text[kPatternPetals].c_str();
	miPatternGrid->Caption = GLanguageHandler->Text[kPatternGrid].c_str();
	miPatternPyramid->Caption = GLanguageHandler->Text[kPatternPyramid].c_str();
	miPatternLeftTriangle->Caption = GLanguageHandler->Text[kPatternLeftTriangle].c_str();
	miPatternRightTriangle->Caption = GLanguageHandler->Text[kPatternRightTriangle].c_str();
	//
	Frames1->Caption = GLanguageHandler->Text[kFrames].c_str();
	miAddFrame->Caption = GLanguageHandler->Text[kAddFrame].c_str();
	miAddFrameCopy->Caption = GLanguageHandler->Text[kAddFrameCopy].c_str();
	miAddFrameMultiple->Caption = Utility::WS2US(GLanguageHandler->Text[kAddFrameMultiple] + L"...");
	miDeleteFrame->Caption = GLanguageHandler->Text[kDeleteFrame].c_str();
	miDeleteMultipleFrames->Caption = Utility::WS2US(GLanguageHandler->Text[kDeleteMultipleFrames] + L"...");
	//
	Layers1->Caption = GLanguageHandler->Text[kLayers].c_str();
	miToggleLayoutPanel->Caption = GLanguageHandler->Text[kToggleLayoutPanel].c_str();
	miClearLayer->Caption = GLanguageHandler->Text[kClearLayerAllFrames].c_str();
	miFlattenLayers->Caption = GLanguageHandler->Text[kFlattenAllLayers].c_str();
	//
	Colours1->Caption = GLanguageHandler->Text[kColours].c_str();
	miChangeColoursFrame->Caption = Utility::WS2US(GLanguageHandler->Text[kChangeColoursInTheFrameLayer] + L"...");
	miChangeColoursLayer->Caption = Utility::WS2US(GLanguageHandler->Text[kChangeColoursGloballyCurrentLayer] + L"...");
	miChangeColoursAll->Caption = Utility::WS2US(GLanguageHandler->Text[kChangeColoursGloballyAllLayersFrames] + L"...");
	miCountColours->Caption = GLanguageHandler->Text[kCountColours].c_str();
	Currentframe1->Caption = GLanguageHandler->Text[kCurrentFrame].c_str();
	Animation1->Caption = GLanguageHandler->Text[kAnimation].c_str();
	//
	N34->Caption = GLanguageHandler->Text[kGradients].c_str();
	miGradientFillFrame->Caption = GLanguageHandler->Text[kFillFrame].c_str();
	miGradientLoad->Caption = GLanguageHandler->Text[kLoad].c_str();
	miGradientSave->Caption = Utility::WS2US(GLanguageHandler->Text[kSaveCurrent] + L"...");
	//
	Buffer1->Caption = GLanguageHandler->Text[kMemories].c_str();
	miCopyCurrentTo->Caption = GLanguageHandler->Text[kCopyCurrentTo].c_str();
	miRestoreCurrentFrom->Caption = GLanguageHandler->Text[kRestoreCurrentFrom].c_str();
	miExportUserMemories->Caption = GLanguageHandler->Text[kExportUserMemories].c_str();
	miClearAllUserMemories->Caption = GLanguageHandler->Text[kClearAllUserMemories].c_str();
	//
	ools1->Caption = GLanguageHandler->Text[kTools].c_str();
	miAutoSave->Caption = GLanguageHandler->Text[kAutosave].c_str();
	Autosaveinterval1->Caption = GLanguageHandler->Text[kAutosaveInterval].c_str();
	miAutosave2->Caption = Utility::WS2US(L"2 " + GLanguageHandler->Text[kMinutes]);
	miAutosave5->Caption = Utility::WS2US(L"5 " + GLanguageHandler->Text[kMinutes]);
	miAutosave10->Caption = Utility::WS2US(L"10 " + GLanguageHandler->Text[kMinutes]);
	Openautosavefolder1->Caption = GLanguageHandler->Text[kOpenAutosaveFolder].c_str();
	miAutomate->Caption = GLanguageHandler->Text[kAutomate].c_str();
	miOptimiseData->Caption = GLanguageHandler->Text[kOptimiseData].c_str();
	miFontViewer->Caption = Utility::WS2US(GLanguageHandler->Text[kFontViewer] + L"...");
	//
	miHelp->Caption = GLanguageHandler->Text[kHelp].c_str();
	Help1->Caption = Utility::WS2US(GLanguageHandler->Text[kHelp] + L"...");
	Showshortcutkeys1->Caption = GLanguageHandler->Text[kShowShortcutKeys].c_str();
	miLanguage->Caption = GLanguageHandler->Text[kLanguage].c_str();
	Examples1->Caption = Utility::WS2US(GLanguageHandler->Text[kExampleCode] + L"...");
	Checkforupdates1->Caption = Utility::WS2US(GLanguageHandler->Text[kCheckForUpdates] + L"...");
	Website1->Caption = GLanguageHandler->Text[kWebsite].c_str();
	miAbout->Caption = Utility::WS2US(GLanguageHandler->Text[kAbout] + L" :)");
}


std::wstring TfrmMain::GetDrawModeText(int drawingmode)
{
	switch (drawingmode)
	{
	case 0:
		switch (thematrix->Render.Brush)
		{
		case BrushSize::kSmall:
			return DrawModes[drawingmode] + L" (1x1)";
		case BrushSize::kMedium:
			return DrawModes[drawingmode] + L" (2x2)";
		case BrushSize::kLarge:
			return DrawModes[drawingmode] + L" (3x3)";
		case BrushSize::kBigLarge:
			return DrawModes[drawingmode] + L" (4x4)";
		case BrushSize::kSuperLarge:
			return DrawModes[drawingmode] + L" (5x5)";
		}
		break;

	case 4:
		return DrawModes[drawingmode] + L" (" + L")";

	case 7:
		switch (thematrix->GetRandomCoeff())
		{
		case 20:
			return DrawModes[drawingmode] + L" (" + GLanguageHandler->Text[kTiny] + L")";
		case 30:
			return DrawModes[drawingmode] + L" (" + GLanguageHandler->Text[kSmall] + L")";
		case 40:
			return DrawModes[drawingmode] + L" (" + GLanguageHandler->Text[kMedium] + L")";
		case 50:
			return DrawModes[drawingmode] + L" (" + GLanguageHandler->Text[kLarge] + L")";
		case 60:
			return DrawModes[drawingmode] + L" (" + GLanguageHandler->Text[kMassive] + L")";
		}
		break;

	case 12:
		if (thematrix->GradientBrushCount() == 0)
		{
			return DrawModes[drawingmode] + L" (no gradient loaded)";
		}
		else
		{
			return DrawModes[drawingmode] + L" (" + std::to_wstring(thematrix->GradientBrushCount()) + L" colours)";
		}
		break;

	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
	case 20:
	case 21:
	    return DrawModes[drawingmode] + L" (LEFT CTRL + WHEEL to change)";

	default:
		return DrawModes[drawingmode];
	}

    return L"unknown";
}


void TfrmMain::UpdateDrawModeCaption(int drawingmode)
{
	lSelectedTool->Caption = GetDrawModeText(drawingmode).c_str();
	lSelectedTool->Refresh();

	lSelectedTool->Tag = drawingmode;
}
#pragma end_region


#pragma region animation_playback
void TfrmMain::PlaybackCommand(int command)
{
	switch (command)
	{
	case CAnimPlayStart:
		PlaybackStart();
		break;
	case CAnimPlayStop:
		PlaybackStop();
		break;
	case CAnimFirstFrame:
		PlaybackFirstFrame();
		break;
	case CAnimPreviousFrame:
		PlaybackPreviousFrame();
		break;
	case CAnimNextFrame:
		PlaybackNextFrame();
		break;
	case CAnimLastFrame:
		PlaybackLastFrame();
		break;
	}
}


void  TfrmMain::PlaybackStart()
{
	bPlayAnimation->Enabled        = false;
	bStartFrame->Enabled           = false;
	bEndFrame->Enabled             = false;
	bNextFrame->Enabled            = false;
	bPreviousFrame->Enabled        = false;
	bStopAnimation->Enabled        = true;

	miPreviousFrame->Enabled       = false;
	miNextFrame->Enabled           = false;

	frmPreviewPopout->SetForPlaybackStart();

	bAddFrame->Enabled              = false;
	bAddFrameCopy->Enabled          = false;
	bAddFrameMultiple->Enabled      = false;
	bDeleteFrame->Enabled           = false;
	bDeleteMultipleFrames->Enabled  = false;

	miAddFrame->Enabled             = false;
	miAddFrameCopy->Enabled         = false;
	miAddFrameMultiple->Enabled     = false;
	miDeleteFrame->Enabled          = false;
	miDeleteMultipleFrames->Enabled = false;

	timerAnimate->Tag               = GetSelectedFrame();
	timerAnimate->Enabled           = true;

	thematrix->AnimPlaying         = true;
	thematrix->SetMatrixReadOnly(true);

	ManageUIControls(false, false);
}


void  TfrmMain::PlaybackStop()
{
	timerAnimate->Enabled = false;

	bPlayAnimation->Enabled = true;
	bStartFrame->Enabled = true;
	bEndFrame->Enabled = true;
	bNextFrame->Enabled = true;
	bPreviousFrame->Enabled = true;
	bStopAnimation->Enabled = false;

	miPreviousFrame->Enabled = true;
	miNextFrame->Enabled = true;

	frmPreviewPopout->SetForPlaybackStop();

	bAddFrame->Enabled = true;
	bAddFrameCopy->Enabled = true;
	bAddFrameMultiple->Enabled = true;

	bDeleteFrame->Enabled = tbFrames->Max != 1;
	bDeleteMultipleFrames->Enabled = tbFrames->Max != 1;

	miAddFrame->Enabled = true;
	miAddFrameCopy->Enabled = true;
	miAddFrameMultiple->Enabled = true;

	miDeleteFrame->Enabled = tbFrames->Max != 1;
	miDeleteMultipleFrames->Enabled = tbFrames->Max != 1;

	thematrix->AnimPlaying = false;
	thematrix->SetMatrixReadOnly(false);

	ManageUIControls(false, false);
}


void TfrmMain::PlaybackFirstFrame()
{
	thematrix->SetAndShowCurrentFrame(0);

	SetFrameCaption(0);
}


void TfrmMain::PlaybackPreviousFrame()
{
	int i = tbFrames->Position;

	if (i == 1)
	{
		i = tbFrames->Max;
	}
	else
	{
		i--;
	}

	thematrix->SetAndShowCurrentFrame(i - 1);

	SetFrameCaption(i - 1);
}


void TfrmMain::PlaybackNextFrame()
{
	int i = tbFrames->Position;

	if (i == tbFrames->Max)
	{
		i = 1;
	}
	else
	{
		i++;
	}

	thematrix->SetAndShowCurrentFrame(i - 1);

	SetFrameCaption(i - 1);
}


void TfrmMain::PlaybackLastFrame()
{
	thematrix->SetAndShowCurrentFrame(tbFrames->Max - 1);

	SetFrameCaption(tbFrames->Max - 1);
}
#pragma end_region


#pragma region Menu_Helpers
void TfrmMain::BuildFontMenu()
{
	if (GFontHandler->Fonts.size() != 0)
	{
		for (int t = 0; t < GFontHandler->Fonts.size(); t++)
		{
			TMenuItem *mi = new TMenuItem(miLoadFont);
            mi->Caption = GFontHandler->Fonts[t].c_str();
			mi->Tag = t;
			mi->RadioItem = true;
			mi->Checked = false;
			mi->OnClick = SelectFont;

			miLoadFont->Add(mi);

			if (t == 0)
			{
				SelectFont(mi);
			}
		}
	}
	else
	{
		sbFont->Visible = false;
        miFont->Visible = false;
	}
}


void TfrmMain::BuildGradientMenu()
{
	if (GSystemSettings->Gradients.size() != 0)
	{
		for (int t = 0; t < GSystemSettings->Gradients.size(); t++)
		{
			TMenuItem *mi = new TMenuItem(miLoadGradients);
			mi->Caption = GSystemSettings->Gradients[t].c_str();
			mi->Tag = t;
			mi->OnClick = SelectGradient;

			miLoadGradients->Add(mi);

            // copy to main menu "Gradients"
			TMenuItem *mi2 = new TMenuItem(miGradientLoad);
			mi2->Caption = GSystemSettings->Gradients[t].c_str();
			mi2->Tag = t;
			mi2->OnClick = SelectGradient;

			miGradientLoad->Add(mi2);
		}
	}
}


void TfrmMain::BuildLanguageMenu()
{
	if (GSystemSettings->Languages.size() != 0)
	{
		for (int t = 0; t < GSystemSettings->Languages.size(); t++)
		{
			TMenuItem *mi = new TMenuItem(miLanguage);
			mi->Caption   = GSystemSettings->Languages[t].c_str();
			mi->RadioItem = true;
			mi->AutoCheck = true;
			mi->Tag       = 0;
			mi->OnClick   = LanguageClick;

			miLanguage->Add(mi);

			if (mi->Caption == GSystemSettings->App.Language.c_str())
			{
				mi->Checked = true;
			}
		}
	}
}


void TfrmMain::BuildPresetMenu()
{
	if (GPresetHandler->Presets.size() != 0)
	{
		for (int t = 0; t < GPresetHandler->Presets.size(); t++)
		{
			TMenuItem *mi = new TMenuItem(miLoadPreset);
			mi->Caption = GPresetHandler->Presets[t].c_str();
			mi->Tag = t;
			mi->OnClick = SelectPreset;

			miLoadPreset->Add(mi);
		}
	}
	else
	{
		miLoadPreset->Enabled = false;
    }
}
#pragma end_region


#pragma region Menu_File
void __fastcall TfrmMain::miSaveAsClick(TObject *Sender)
{
	ConfigureSaveDialog(CSaveProject);

	if (sdMain->Execute())
	{
		ImportData tid;

		BuildImportData(tid, 0, thematrix->GetFrameCount() - 1);

		if (thematrix->GetSoftwareMode() == SoftwareMode::kFont)
		{
			thematrix->SaveFont(sdMain->FileName.c_str(), tid, GSystemSettings->App.LastExport);
		}
		else
		{
			ProjectColours colours = GetColours();

			thematrix->SaveAnimation(sdMain->FileName.c_str(), tid, GSystemSettings->App.LastExport, colours);
		}

		SetCurrentProjectFileName(sdMain->FileName.c_str());
	}
}


void __fastcall TfrmMain::ReopenClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	if (sbClear->Enabled && !GSystemSettings->App.IgnoreWarnings)
	{
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kOpeningNewMatrixWillClearCurrentProject] +
					   L"\n\n" +
					   GLanguageHandler->Text[kDoYouWishToContinue]), mtWarning, mbYesNo, 0) != mrYes) return;
	}

	// =======================================================================

	TMenuItem *mi = (TMenuItem*)Sender;

	LoadFromFileName(GSystemSettings->FileHistory[mi->Tag]);

	FormResize(nullptr);

	Application->ProcessMessages();
	thematrix->SetMatrixReadOnly(false);
}


void __fastcall TfrmMain::New1Click(TObject *Sender)
{
	sbBuildClick(nullptr);
}


void __fastcall TfrmMain::miImportFromBitmapClick(TObject *Sender)
{
	if (!thematrix->Details.Available)
	{
		frmImportBitmap->cbCreateNew->Checked = true;
	}

	frmImportBitmap->ShowModal();

	if (frmImportBitmap->Import != ImportMode::kInvalid)
	{
		ImportData ted;

		if (!thematrix->Details.Available || frmImportBitmap->CreateNew)
		{
			GSystemSettings->Project.Width   = frmImportBitmap->FrameWidth;
			GSystemSettings->Project.Height  = frmImportBitmap->FrameHeight;

			GSystemSettings->Project.Special = frmImportBitmap->FrameCount;

			GSystemSettings->Project.Clear   = true;

			switch (frmImportBitmap->ImportMode)
			{
				case ImportColourMode::kMono:
					GSystemSettings->Project.Mode = MatrixMode::kMono;
					break;
				case ImportColourMode::kRGB:
					GSystemSettings->Project.Mode = MatrixMode::kRGB;
					break;
				case ImportColourMode::kRGB3bpp:
					GSystemSettings->Project.Mode = MatrixMode::kRGB3BPP;
					break;
			}

			thematrix->NewMatrix(GSystemSettings->Project.Mode,
								 GSystemSettings->Project.Special, CTopOffset, CLeftOffset,
								 frmImportBitmap->FrameWidth, frmImportBitmap->FrameHeight,
								 GSystemSettings->Project.PixelSize, GSystemSettings->Project.Shape,
								 miGridToggle->Checked, false, true,
								 GSystemSettings->Project.Background);
		}

		switch (frmImportBitmap->Import)
		{
		case ImportMode::kInvalid:
            // already handled above
			break;
		case ImportMode::kSingleImage:
			ted = thematrix->ImportFromBMPSingleImage(frmImportBitmap->ImageFilename,
													  frmImportBitmap->FrameCount,
													  frmImportBitmap->FrameWidth,
													  frmImportBitmap->FrameHeight,
													  frmImportBitmap->ImportMode,
													  frmImportBitmap->CreateNew);
			break;
		case ImportMode::kMultipleImages:
			ted = thematrix->ImportFromBMPMultipleImage(frmImportBitmap->Pattern,
														frmImportBitmap->FirstFrame,
														frmImportBitmap->FrameCount,
														frmImportBitmap->PadLength,
														frmImportBitmap->FrameWidth,
														frmImportBitmap->FrameHeight,
														frmImportBitmap->ImportMode,
														frmImportBitmap->CreateNew);
			break;
		}

		if (frmImportBitmap->FrameCount != -1 && ted.ImportOk)
		{
			ClearCurrentProjectFileName();

			tbFrames->Max = thematrix->GetFrameCount();

			if (thematrix->Details.Width <= 0 || thematrix->Details.Height <= 0)
			{
				thematrix->Details.Width  = ted.NewWidth;
				thematrix->Details.Height = ted.NewHeight;
			}

			frmPreviewPopout->tbFrames->Max = tbFrames->Max;

			bDeleteFrame->Enabled          = (tbFrames->Max > 1);
			bDeleteMultipleFrames->Enabled = (tbFrames->Max > 1);

			SetFrameCaption(thematrix->GetCurrentFrame());

			FormResize(nullptr);

			thematrix->SetAndShowCurrentFrame(GetSelectedFrame());

			ManageUIControls(false, false);

			ChangeMatrixType();
		}
	}
}


void __fastcall TfrmMain::miImportFromGIFClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	if (sbClear->Enabled && !GSystemSettings->App.IgnoreWarnings)
	{
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kOpeningNewMatrixWillClearCurrentProject] +
									 L"\n\n" +
									 GLanguageHandler->Text[kDoYouWishToContinue]),
									 mtWarning, mbYesNo, 0) == mrNo) return;
	}

	// =======================================================================

	opdMain->InitialDir = GSystemSettings->App.LastLoadLocation.c_str();

	if (opdMain->Execute())
	{
		LoadFromGIF(opdMain->FileName.c_str());

		FormResize(nullptr);
	}
}


void __fastcall TfrmMain::miImportInToCurrentClick(TObject *Sender)
{
	 if (timerAnimate->Enabled)
	 {
		bPlayAnimationClick(bStopAnimation);
	 }

	ConfigureOpenDialog(CLoadProject);

	if (odMain->Execute())
	{
		ImportData ted = thematrix->ImportLEDMatrixDataSingleFrame(odMain->FileName.c_str());

		GSystemSettings->Project.Mode = ted.Mode;

		ChangeMatrixType();
	}
}


void __fastcall TfrmMain::miAppendClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	ConfigureOpenDialog(CLoadProject);

	if (odMain->Execute())
	{
		if (AppendFromFileName(odMain->FileName.c_str()))
		{
			UpdateDisplay(-1);

			GSystemSettings->App.LastLoadLocation = ExtractFilePath(odMain->FileName).c_str();
		}
	}
}


void __fastcall TfrmMain::miMergeClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	MergeObject merge = OpenMerge();

	if (merge.Process)
	{
		switch (merge.Mode)
		{
		case MergeMode::kAnimationBottom:
			MergeFromFileName(merge.FileName, merge.StartFrame - 1, LoadMode::kMergeBottomPriority);
			break;
		case MergeMode::kAnimationTop:
			MergeFromFileName(merge.FileName, merge.StartFrame - 1, LoadMode::kMergeTopPriority);
			break;
		case MergeMode::kNewLayer:
			MergeFromFileName(merge.FileName, merge.StartFrame - 1, LoadMode::kMergeNewLayer);
			break;
		case MergeMode::kCurrentFrame:
			MergeFromFileName(merge.FileName, merge.StartFrame - 1, LoadMode::kMergeCurrentLayer);
			break;
		}

		UpdateDisplay(-1);

		GSystemSettings->App.LastLoadLocation = ExtractFilePath(odMain->FileName).c_str();
	}
}


void __fastcall TfrmMain::miSaveSingleFrameClick(TObject *Sender)
{
	ConfigureSaveDialog(CSaveProject);

	if (GSystemSettings->App.DataFilename.empty())
	{
		sdMain->FileName   = Utility::WS2US(GLanguageHandler->Text[kFrame] + L"_" + std::to_wstring(tbFrames->Position));
	}
	else
	{
		std::wstring fn = Utility::GetFileNameNoExt(ExtractFileName(GSystemSettings->App.DataFilename.c_str()).c_str()) +
													L"_" + GLanguageHandler->Text[kFrame] +
													L"_" + std::to_wstring(tbFrames->Position);

		sdMain->FileName   = fn.c_str();
		sdMain->InitialDir = ExtractFilePath(GSystemSettings->App.DataFilename.c_str()).c_str();
	}

	if (sdMain->Execute())
	{
		ImportData ted;

		ted.Mode = GSystemSettings->Project.Mode;

		thematrix->SaveSingleFrame(sdMain->FileName.c_str(), ted, tbFrames->Position);
	}
}


void __fastcall TfrmMain::miSaveRangeClick(TObject *Sender)
{
	SaveFrameRangeObject sfro = OpenFrameRange(thematrix->GetFrameCount());

	if (sfro.Process)
	{
		if ((sbClear->Enabled || !thematrix->AnimPlaying) && thematrix->Details.Available)
		{
			ImportData ted;

			BuildImportData(ted, sfro.StartFrame - 1, sfro.EndFrame - 1);

			// =========================================================================

			std::wstring FileName = Utility::GetAutoSaveName();

			ProjectColours colours = GetColours();

			thematrix->SaveAnimation(GSystemSettings->App.LMSFilePath + L"saves\\autosave\\" + FileName, ted, GSystemSettings->App.LastExport, colours);

			statusMain->SimpleText = Utility::WS2US(GLanguageHandler->Text[kAutosavedCurrentMatrixRange] + L" (" + FileName + L")");
		}
	}
}


void __fastcall TfrmMain::miSaveAsFontClick(TObject *Sender)
{
	ConfigureSaveDialog(CSaveFont);

	// =======================================================================

	if (sdMain->Execute())
	{
		if (thematrix->Details.Mode == MatrixMode::kRGB)
		{
			thematrix->SaveAsRGBFont(sdMain->FileName.c_str());
		}
		else
		{
			thematrix->SaveAsTextToolFont(sdMain->FileName.c_str());
		}
	}
}


void __fastcall TfrmMain::miExportToBitmapClick(TObject *Sender)
{
	spdMain->Filter = GLanguageHandler->Text[kBitmapImages] + L" (*.bmp)|*.bmp";

	if (spdMain->Execute())
	{
		thematrix->ExportToBitmap(spdMain->FileName.c_str());
	}
}


void __fastcall TfrmMain::miExportAnimationToBitmapClick(TObject *Sender)
{
	spdMain->Filter = GLanguageHandler->Text[kBitmapImages] + L" (*.bmp)|*.bmp";

	if (spdMain->Execute())
	{
		thematrix->ExportAnimationToBitmap(spdMain->FileName.c_str());
	}
}


void __fastcall TfrmMain::miExportToGIFClick(TObject *Sender)
{
	ExportGIFSettings ego = OpenExportGIF(GSystemSettings->ExportGIF);

	if (ego.Process)
	{
		thematrix->ExportToGIF(ego.FileName, ego.Background, ego.PixelSize, ego.PixelShape, ego.AnimationSpeed);

		GSystemSettings->ExportGIF.FileName   = ego.FileName;
		GSystemSettings->ExportGIF.PixelSize  = ego.PixelSize;
		GSystemSettings->ExportGIF.PixelShape = ego.PixelShape;
		GSystemSettings->ExportGIF.Background = ego.Background;
	}
}


void __fastcall TfrmMain::Preferences1Click(TObject *Sender)
{
	PrefsMatrixColours pmc;

	pmc.Mono[0] = thematrix->LEDColoursSingle[0];
	pmc.Mono[1] = thematrix->LEDColoursSingle[1];

	pmc.Bi[0] = thematrix->LEDColoursBi[0];
	pmc.Bi[1] = thematrix->LEDColoursBi[1];
	pmc.Bi[2] = thematrix->LEDColoursBi[2];
	pmc.Bi[3] = thematrix->LEDColoursBi[3];

	pmc.Selection = thematrix->LEDColoursBi[4];
	pmc.LightBox = thematrix->LEDColoursBi[5];

	if (OpenPreferences(pmc))
	{
		thematrix->LEDColoursSingle[0] = pmc.Mono[0];
		thematrix->LEDColoursSingle[1] = pmc.Mono[1];

		thematrix->LEDColoursBi[0]     = pmc.Bi[0];
		thematrix->LEDColoursBi[1]     = pmc.Bi[1];
		thematrix->LEDColoursBi[2]     = pmc.Bi[2];
		thematrix->LEDColoursBi[3]     = pmc.Bi[3];

		thematrix->LEDColoursSingle[4] = pmc.Selection;
		thematrix->LEDColoursSingle[5] = pmc.LightBox;
		thematrix->LEDColoursBi[4]     = pmc.Selection;
		thematrix->LEDColoursBi[5]     = pmc.LightBox;

		thematrix->CopyLEDColours();
	}
}


void __fastcall TfrmMain::Exit1Click(TObject *Sender)
{
	Close();
}


void TfrmMain::BuildReOpenMenu()
{
	if (GSystemSettings->FileHistory.size() == 0) return;

	FileHistoryMenus.clear();
	miReopenMenu->Clear();

	for (int t = 0; t < GSystemSettings->FileHistory.size(); t++)
	{
		TMenuItem *mi = new TMenuItem(miReopenMenu);
		mi->Caption = GSystemSettings->FileHistory[t].c_str();
		mi->Tag     = t;
		mi->OnClick = ReopenClick;

		miReopenMenu->Add(mi);

		FileHistoryMenus.push_back(mi);
	}
}
#pragma end_region


#pragma region Menu_Edit
void __fastcall TfrmMain::miUndoClick(TObject *Sender)
{
	thematrix->Undo();
}


void __fastcall TfrmMain::miRedoClick(TObject *Sender)
{
  thematrix->Redo();
}


void __fastcall TfrmMain::miCopyClick(TObject *Sender)
{
	thematrix->CopyCurrentFrame();
}


void __fastcall TfrmMain::miCopyFromPreviousClick(TObject *Sender)
{
	if (sbClear->Enabled && tbFrames->Position != 1)
	{
		thematrix->CopyFromPrevious(GetSelectedFrame());
	}
}


void __fastcall TfrmMain::miCopyMultipleClick(TObject *Sender)
{
	std::vector<std::wstring> Layers;

	for (int t = 0; t < thematrix->GetLayerCount(); t++)
	{
		Layers.push_back(thematrix->GetLayerName(t));
	}

	CopyMultipleObject cpm = OpenCopyMultiple(thematrix->GetFrameCount(), Layers);

	if (cpm.Process)
	{
		thematrix->SetAutomateMode(true);

		for (int t = cpm.StartFrame; t <= cpm.EndFrame; t++)
		{
			if (cpm.AllLayers)
			{
				thematrix->CopyAllLayersFromTo(t, cpm.CopyTo + (t - cpm.StartFrame));
			}
			else
			{
				thematrix->CopyLayerFromTo(cpm.Source, cpm.Destination, t, cpm.CopyTo + (t - cpm.StartFrame));
			}
		}

		thematrix->SetAutomateMode(false);

		thematrix->Refresh();
	}
}


void __fastcall TfrmMain::miPasteClick(TObject *Sender)
{
	thematrix->PasteCurrentFrame();
}


void __fastcall TfrmMain::Copyandshiftleft1Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	thematrix->PasteSpecial(mi->Tag);
}


void __fastcall TfrmMain::Rotateanticlockwise1Click(TObject *Sender)
{
	thematrix->RotateCopyBrush(kEffectRotateACW);
}


void __fastcall TfrmMain::Rotateclockwise1Click(TObject *Sender)
{
	thematrix->RotateCopyBrush(kEffectRotateCW);
}


void __fastcall TfrmMain::miBrushFlipClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	switch (mi->Tag)
	{
	case 0:
		thematrix->PerformEffectOnBrush(kEffectFlip);
		break;
	case 1:
		thematrix->PerformEffectOnBrush(kEffectMirror);
		break;
	case 2:
		thematrix->PerformEffectOnBrush(kEffectInvert);
		break;
	}
}


void __fastcall TfrmMain::Pasteintoeveryframe1Click(TObject *Sender)
{
	thematrix->DrawWithBrushPasteEveryFrame(OldMouseX, OldMouseY, false);
}


void __fastcall TfrmMain::Pasteintoeveryframetransparent1Click(TObject *Sender)
{
	thematrix->DrawWithBrushPasteEveryFrame(OldMouseX, OldMouseY, true);
}


void __fastcall TfrmMain::miShiftLeftClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	ScrollFrame(mi->Tag);
}


void __fastcall TfrmMain::miRotateLClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	RotateFrame(mi->Tag);
}


void __fastcall TfrmMain::miFlipClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	FrameEffect(mi->Tag);
}


void __fastcall TfrmMain::miAddCommentClick(TObject *Sender)
{
	std::wstring s = thematrix->Details.Comment;

	s = InputBox(GLanguageHandler->Text[kMatrixComment].c_str(),
				 GLanguageHandler->Text[kAddCommentMatrix].c_str(), L"Comment").c_str();

	if (!s.empty())
	{
		thematrix->Details.Comment = s;
	}
}
#pragma end_region


#pragma region Menu_View
void __fastcall TfrmMain::miShowAnimationToolbarClick(TObject *Sender)
{
	pAnimationToolbar->Visible = miShowAnimationToolbar->Checked;
}


void __fastcall TfrmMain::miPaletteGradientToolbarClick(TObject *Sender)
{
	miPaletteGradientToolbar->Checked = !miPaletteGradientToolbar->Checked;

	pRGBPalette->Visible = miPaletteGradientToolbar->Checked;

	FormResize(nullptr);
}


void __fastcall TfrmMain::miQuickDataClick(TObject *Sender)
{
	pQuickData->Visible = miQuickData->Checked;

	if (miQuickData->Checked)
	{
		UpdateData();
	}
}


void __fastcall TfrmMain::miUndoToolbarClick(TObject *Sender)
{
	pUndoToolbar->Visible = !pUndoToolbar->Visible;

	if (pUndoToolbar->Visible)
	{
		FrameUndoPanel->SetUndos(thematrix->GetUndoCount());
	}
}


void __fastcall TfrmMain::miCustomBackgroundClick(TObject *Sender)
{
	if (colorDialog->Execute())
	{
		SystemSetBackgroundColour(colorDialog->Color);
	}
}


void __fastcall TfrmMain::Black1Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	SystemSetBackgroundColour(backgroundColours[mi->Tag]);
}


void __fastcall TfrmMain::miASCIIStartCodeClick(TObject *Sender)
{
	UnicodeString s = GSystemSettings->App.ASCIIIndex;

	if (InputQuery(GLanguageHandler->Text[kASCIICode].c_str(), GLanguageHandler->Text[kStartASCIICodeFontMode].c_str(), s))
	{
		GSystemSettings->App.ASCIIIndex = s.ToInt();
	}
}


void __fastcall TfrmMain::miPreviousFrameClick(TObject *Sender)
{
	PlaybackCommand(CAnimPreviousFrame);
}


void __fastcall TfrmMain::miNextFrameClick(TObject *Sender)
{
	PlaybackCommand(CAnimNextFrame);
}


void __fastcall TfrmMain::miGridToggleClick(TObject *Sender)
{
	miGridToggle->Checked = !miGridToggle->Checked;

	thematrix->ChangeGrid(miGridToggle->Checked);
}


void __fastcall TfrmMain::miFontModeClick(TObject *Sender)
{
	pbFont->Visible = miFontMode->Checked;
	miSaveAsFont->Enabled = miFontMode->Checked;

	if (miFontMode->Checked)
	{
		thematrix->SetSoftwareMode(SoftwareMode::kFont);
	}
	else
	{
		thematrix->SetSoftwareMode(SoftwareMode::kAnimation);
	}

	tbFrames->Max = thematrix->GetFrameCount();
	frmPreviewPopout->tbFrames->Max = tbFrames->Max;

	SetFrameCaption(GetSelectedFrame());

	UpdateMemoryUsage();

	FormResize(nullptr);
}
#pragma end_region


#pragma region Menu_Preview
void __fastcall TfrmMain::miPreviewClick(TObject *Sender)
{
	thematrix->SetPreviewActive(miPreview->Checked);

	FormResize(nullptr);
}


void __fastcall TfrmMain::miPreviewx1Click(TObject *Sender)
{
	if (!thematrix->GetPreviewPopout())
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		thematrix->SetPreviewBoxSize(previewSizes[mi->Tag]);

		SyncPreviewSize(mi->Tag);

		FormResize(nullptr);
	}
}


void __fastcall TfrmMain::miPreviewViewSquareClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	thematrix->SetPreviewViewMode(PreviewOptionsHelper::IntToViewShape(mi->Tag));

	SyncPreviewView(mi->Tag);

	FormResize(nullptr);
}


void __fastcall TfrmMain::miPreviewVoid10Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	thematrix->SetPreviewVoid(previewVoids[mi->Tag]);

	SyncPreviewVoid(mi->Tag);

	FormResize(nullptr);
}


void __fastcall TfrmMain::miRadialOffset45Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	int offset = thematrix->GetRadialOffset();

	switch (mi->Tag)
	{
	case 0:
		offset = CZeroDegrees;
		break;
	case 1:
		offset = C45Degrees;
		break;
	case 2:
		offset = C90Degrees;
		break;
	case 3:
		offset = C135Degrees;
		break;
	case 4:
		offset = C180Degrees;
		break;
	case 5:
		offset = C225Degrees;
		break;
	case 6:
		offset = C270Degrees;
		break;
	case 7:
		offset = C315Degrees;
		break;
	}

	thematrix->SetRadialOffset(offset);
}


void __fastcall TfrmMain::miPreviewOffsetReverseClick(TObject *Sender)
{
	thematrix->SetRadialOffsetDirection(miPreviewOffsetReverse->Checked);
}


void __fastcall TfrmMain::miPopoutPreviewClick(TObject *Sender)
{
	if (!thematrix->GetPreviewPopout())
	{
		frmPreviewPopout->Panel1->Color = pCanvas->Color;
		frmPreviewPopout->Color         = pCanvas->Color;

		frmPreviewPopout->Show();

		frmPreviewPopout->OnClose      = &OnPopoutClosed;
		frmPreviewPopout->OnCommand    = std::bind(PreviewWindowCommand, std::placeholders::_1);
		frmPreviewPopout->OnNewFrame   = std::bind(PreviewWindowChangeFrame, std::placeholders::_1);

		thematrix->PreviewOwner = frmPreviewPopout;
		thematrix->PreviewCanvas = frmPreviewPopout->Panel1;

		thematrix->SetPreviewPopout(true);
	}
	else
	{
		frmPreviewPopout->Close();
	}
}


void __fastcall TfrmMain::miPreviewAllowDrawingClick(TObject *Sender)
{
	thematrix->SetPreviewDrawing(miPreviewAllowDrawing->Checked);
}


void __fastcall TfrmMain::miIncrementRadiallyClick(TObject *Sender)
{
	thematrix->SetPreviewIncrementRadially(miIncrementRadially->Checked);
}
#pragma end_region


#pragma region Menu_Project
void __fastcall TfrmMain::miClearAllFramesLayerClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kClearAllFramesFromTheSelectedLayer] +
				  L"\n\n" + GLanguageHandler->Text[kAreYouSure] +
				  L" " +
				  GLanguageHandler->Text[kThisCannotBeUndone]), mtWarning, mbYesNo, 0) == mrYes)
	{
		thematrix->WipeAllFramesCurrentLayer();

		UpdateDisplay(1);

		ClearCurrentProjectFileName();
	}
}


void __fastcall TfrmMain::miClearAllFramesClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kClearAllFramesAndLayers] +
				   L"\n\n" +
				   GLanguageHandler->Text[kAreYouSure] +
				   L" " +
				   GLanguageHandler->Text[kThisCannotBeUndone]), mtWarning, mbYesNo, 0) == mrYes)
	{
		thematrix->WipeAllFramesAllLayers();

		UpdateDisplay(1);

		ClearCurrentProjectFileName();
	}
}


void __fastcall TfrmMain::miClearAllFramesGradientClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kClearAllFramesQ] +
				   L"\n\n" +
				   GLanguageHandler->Text[kAreYouSure]), mtWarning, mbYesNo, 0) == mrYes)
	{
		thematrix->ClearAllFramesGradient(sbGradient->Tag);

		UpdateDisplay(1);

		ClearCurrentProjectFileName();
	}
}


void __fastcall TfrmMain::miFlipAllFramesClick(TObject *Sender)
{
	thematrix->PerformEffectController(kEffectFlipAll, CMOMCurrentLayerFrames);
}


void __fastcall TfrmMain::miMirrorAllFramesClick(TObject *Sender)
{
	thematrix->PerformEffectController(kEffectMirrorAll, CMOMCurrentLayerFrames);
}


void __fastcall TfrmMain::miInvertAllFramesClick(TObject *Sender)
{
	thematrix->PerformEffectController(kEffectInvertAll, CMOMCurrentLayerFrames);
}


void __fastcall TfrmMain::miGradientAllFramesClick(TObject *Sender)
{
	thematrix->PerformEffectController(kEffectGradientAll, CMOMCurrentLayerFrames);
}


void __fastcall TfrmMain::miSetIgnoredPixelsClick(TObject *Sender)
{
	thematrix->SetIgnoredPixelsMode(!thematrix->GetIgnoredPixelsMode());

	if (thematrix->GetIgnoredPixelsMode())
	{
		ManageUIControls(true, false);

		miSetIgnoredPixels->Caption = GLanguageHandler->Text[kAcceptIgnoredPixels].c_str();
	}
	else
	{
		ManageUIControls(false, false);

		miSetIgnoredPixels->Caption = GLanguageHandler->Text[kSetIgnoredPixels].c_str();
	}
}


void __fastcall TfrmMain::miSetIgnoredFromPatternClick(TObject *Sender)
{
	SetIgnoredPixels sipo = OpenIgnoredPixels(thematrix->Details.Width, thematrix->Details.Height);

	if (sipo.Process)
	{
		thematrix->SetIgnoredPixelsFromCustomShape(sipo.Shape, sipo.Parameter);
	}
}


void __fastcall TfrmMain::miClearAllIgnoredPixelsClick(TObject *Sender)
{
	thematrix->SetIgnoredPixels(false);
}


void __fastcall TfrmMain::miSaveIgnoredPixelsAsPatternClick(TObject *Sender)
{
	ConfigureSaveDialog(CSaveIgnorePixels);

	if (sdMain->Execute())
	{
		thematrix->SaveIgnoredPixels(sdMain->FileName.c_str());
	}
}


void __fastcall TfrmMain::miLoadIgnoredPixelsAsPatternClick(TObject *Sender)
{
	ConfigureOpenDialog(CLoadIgnorePixels);

	if (odMain->Execute())
	{
		thematrix->SetIgnoredPixelsFromFileName(odMain->FileName.c_str());
	}
}


void __fastcall TfrmMain::miHideIgnoredPixelsClick(TObject *Sender)
{
	thematrix->ToggleIgnoredPixels(miHideIgnoredPixels->Checked);
}


void __fastcall TfrmMain::miFadeFirstLastClick(TObject *Sender)
{
	thematrix->FadeFirstToLast();
}


void __fastcall TfrmMain::miUnlockAllClick(TObject *Sender)
{
	thematrix->LockUnLockRange(0, thematrix->GetFrameCount(), false);
}


void __fastcall TfrmMain::miLockAllClick(TObject *Sender)
{
	thematrix->LockUnLockRange(0, thematrix->GetFrameCount(), true);
}


void __fastcall TfrmMain::miToggleLockStatusClick(TObject *Sender)
{
	ToggleLockFrameRange tlfro = OpenToggleLockStatus();//thematrix->GetFrameCount());

	if (tlfro.Process)
	{
		thematrix->LockUnLockRange(tlfro.StartFrame, tlfro.EndFrame, tlfro.LockStatus);
	}
}
#pragma end_region


void __fastcall TfrmMain::miMouseModeClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

    SetDrawingMode(mi->Tag);
}



#pragma region Menu_Layers
void __fastcall TfrmMain::miToggleLayoutPanelClick(TObject *Sender)
{
	pLayers->Visible = !pLayers->Visible;

	miToggleLayoutPanel->Checked = pLayers->Visible;

	if (pLayers->Visible)
	{
		FrameLayerPanel->UpdateLayerTable();
	}

	FormResize(nullptr);
}


void __fastcall TfrmMain::miClearLayerClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kClearAllFramesQ] +
				   L"\n\n" +
				   GLanguageHandler->Text[kDoYouWishToContinue]), mtWarning, mbYesNo, 0) == mrYes)
	{
		thematrix->ClearCurrentLayerAllFrames();
	}
}


void __fastcall TfrmMain::miFlattenLayersClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kFlattenAllLayersQ] +
				   L"\n\n" +
				   GLanguageHandler->Text[kThisCannotBeUndone]), mtWarning, mbYesNo, 0) == mrYes)
	{
		thematrix->FlattenAllLayers();
	}
}
#pragma end_region


#pragma region Menu_Colours
void __fastcall TfrmMain::miChangeColoursFrameClick(TObject *Sender)
{
	std::vector<int> Colours;

	thematrix->GetFirst32Colours(Colours);

	ColourChange cco = OpenColourChange(Colours);

	if (cco.Process)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		switch (mi->Tag)
		{
		case 0:
			thematrix->ChangeColourCurrent(cco.ColourFrom, cco.ColourTo);
			break;
		case 1:
			thematrix->ChangeColourCurrentLayer(cco.ColourFrom, cco.ColourTo);
			break;
		case 2:
			thematrix->ChangeColourAll(cco.ColourFrom, cco.ColourTo);
			break;
		}
	}
}


void __fastcall TfrmMain::Currentframe1Click(TObject *Sender)
{
	MessageDlg(Utility::WS2US(GLanguageHandler->Text[kUniqueColoursCurrentFrame] +
			   L": " +
			   std::to_wstring(thematrix->CountColoursFrame())), mtInformation, TMsgDlgButtons() << mbOK, 0);
}


void __fastcall TfrmMain::Animation1Click(TObject *Sender)
{
	MessageDlg(Utility::WS2US(GLanguageHandler->Text[kUniqueColoursAnimation] +
			   L": " +
			   std::to_wstring(thematrix->CountColoursFrame())), mtInformation, TMsgDlgButtons() << mbOK, 0);
}
#pragma end_region


#pragma region Menu_Gradients
void __fastcall TfrmMain::miGradientFillFrameClick(TObject *Sender)
{
	thematrix->GradientFillFrame();
}
#pragma end_region


#pragma region Menu_Memories
void __fastcall TfrmMain::miMemory1Click(TObject *Sender)
{
	if (sbClear->Enabled)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		int memory = mi->Tag;

		thematrix->CopyToUserBuffer(memory);

		_MenuCopyMemory[memory]->ImageIndex = 9;
		_MenuRestoryMemory[memory]->ImageIndex = 9;
	}
}


void __fastcall TfrmMain::miMemoryR1Click(TObject *Sender)
{
	if (sbClear->Enabled)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		int memory = mi->Tag;

		thematrix->RestoreFromUserBuffer(memory);
	}
}


void __fastcall TfrmMain::miExportUserMemoriesClick(TObject *Sender)
{
	OpenExportData(thematrix, GSystemSettings->App.LastExport, ExportSource::kUserMemories, thematrix->Details.Mode);
}


void __fastcall TfrmMain::miClearAllUserMemoriesClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kAreYouSure] + L"\n\n" +
				   GLanguageHandler->Text[kClearAllUserMatrixBuffers] + L"\n\n\n" +
				   L"(" + GLanguageHandler->Text[kDoesNotClearAnimationFrames] + L")"), mtWarning, mbOKCancel, 0) == mrOk)
    {

		thematrix->ClearUserBuffers();

		for (int t = 0; t < 10; t++)
		{
			_MenuCopyMemory[t]->ImageIndex = -1;
			_MenuRestoryMemory[t]->ImageIndex = -1;
		}
	}
}
#pragma end_region


#pragma region Menu_Tools


#pragma region Menu_Help
void __fastcall TfrmMain::Help1Click(TObject *Sender)
{
	if (FileExists(ExtractFilePath(Application->ExeName) + L"help\\en\\help.pdf"))
	{
		Utility::ExecuteFile(GSystemSettings->App.LMSFilePath + L"help\\en\\help.pdf");
	}
	else
	{
		MessageDlg(GLanguageHandler->Text[kHelpFileNotFound].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
	}
}


void __fastcall TfrmMain::Showshortcutkeys1Click(TObject *Sender)
{
	if (FileExists(ExtractFilePath(Application->ExeName) + L"help\\en\\shortcuts.pdf"))
	{
		Utility::ExecuteFile(GSystemSettings->App.LMSFilePath + L"help\\en\\shortcuts.pdf");
	}
	else
	{
		MessageDlg(GLanguageHandler->Text[kShortcutHelpFileNotFound].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
	}
}


void __fastcall TfrmMain::Examples1Click(TObject *Sender)
{
	Utility::ExecuteFile(GSystemSettings->App.LMSFilePath + L"example code\\");
}


void __fastcall TfrmMain::Checkforupdates1Click(TObject *Sender)
{
    OpenCheckForNewVersion(__LEDStudioDate, __LEDStudioVersion, false);
}


void __fastcall TfrmMain::Website1Click(TObject *Sender)
{
	Utility::ExecuteFile(L"https://github.com/MaximumOctopus/LEDMatrixStudio");
}


void __fastcall TfrmMain::miAboutClick(TObject *Sender)
{
	frmAbout->ShowModal();
}


void __fastcall TfrmMain::witter1Click(TObject *Sender)
{
	Utility::ExecuteFile(L"https://maximumoctopus.hashnode.dev/");
}
#pragma end_region


#pragma region Menu_Debug
void __fastcall TfrmMain::RenderMode1Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(ConstantsHelper::MatrixModeAsString(thematrix->Details.Mode).c_str());
	#endif
}


void __fastcall TfrmMain::MonoColours1Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(IntToHex(thematrix->LEDColoursSingle[0]) + L" / " + IntToHex(thematrix->LEDColoursSingle[1]));
	#endif
}


void __fastcall TfrmMain::CurrentLayerFrame1Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(IntToStr(thematrix->GetCurrentLayer()) + L" / " + IntToStr(thematrix->GetCurrentFrame()) + L" (" +
				IntToStr(thematrix->GetLayerCount()) + L" / " + IntToStr(thematrix->GetFrameCount()) + L")");
	#endif
}


void __fastcall TfrmMain::Controls1Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(IntToStr(tbFrames->Max));
	#endif
}


void __fastcall TfrmMain::PaintBox1Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(thematrix->GetPaintBoxDebug().c_str());
	#endif
}


void __fastcall TfrmMain::Preview2Click(TObject *Sender)
{
	#if _DEBUG
	ShowMessage(thematrix->GetPreviewDebug().c_str());
	#endif
}


void __fastcall TfrmMain::miDrawTestPatternClick(TObject *Sender)
{
	#if _DEBUG
    thematrix->TestSignal();
	#endif
}
#pragma end_region


void __fastcall TfrmMain::miAutoSaveClick(TObject *Sender)
{
	miAutoSave->Checked = !miAutoSave->Checked;

	timerAutosave->Enabled = miAutoSave->Checked;
}


void __fastcall TfrmMain::miAutosave2Click(TObject *Sender)
{
	if (Sender != nullptr)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		mi->Checked = true;

		switch (mi->Tag)
		{
		case 0:
			timerAutosave->Interval = 2 * 60 * 1000;
			GSystemSettings->App.AutoSave = AutoSaveInterval::kTwoMinutes;
			break;
		case 1:
			timerAutosave->Interval =  5 * 60 * 1000;
			GSystemSettings->App.AutoSave = AutoSaveInterval::kFiveMinutes;
			break;
		case 2:
			timerAutosave->Interval = 10 * 60 * 1000;
			GSystemSettings->App.AutoSave = AutoSaveInterval::kTenMinutes;
			break;
		}
	}
}


void __fastcall TfrmMain::Openautosavefolder1Click(TObject *Sender)
{
	std::wstring path = GSystemSettings->App.LMSFilePath + L"saves\\autosave\\";

	Utility::ExecuteFile(path);
}


void __fastcall TfrmMain::miAutomateClick(TObject *Sender)
{
	Automation.LastFileName = GSystemSettings->App.LastAutomationFileName;

	std::vector<std::wstring> layers;
	std::vector<int> colours;

	thematrix->GetFirst32Colours(colours);

	AutomationInput ai;

	ai.FrameCurrent = tbFrames->Position;
	ai.FrameMax     = tbFrames->Max;
	ai.Width        = thematrix->Details.Width;
	ai.Height       = thematrix->Details.Height;
	ai.Mode         = thematrix->Details.Mode;

	RGBPaletteColours rgbpc;

	rgbpc.Left   = sSelectionLMB->Brush->Color;
	rgbpc.Middle = sSelectionMMB->Brush->Color;
	rgbpc.Right  = sSelectionRMB->Brush->Color;

	for (int t = 0; t < 28; t++)
	{
		rgbpc.History[t] = FramePalettePanel->RGBPaletteHistory[t]->Brush->Color;
	}

	for (int t = 0; t < thematrix->GetLayerCount(); t++)
	{
		layers.push_back(thematrix->GetLayerName(t));
	}

	if (OpenAutomate(ai, rgbpc, layers, colours, Automation) == mrOk)
	{
		sbMouseModeClick(sbMouseMode);

		if (Automation.ActionList.size() != 0 || Automation.PostProcessList.size() != 0)
		{
			Automation.SetParameterReveal(thematrix->Details.Width - 1, thematrix->Details.Height - 1);

			thematrix->Automate(Automation);

			tbFrames->Position                   = Automation.FrameEnd;
			frmPreviewPopout->tbFrames->Position = Automation.FrameEnd;

			tbFramesChange(nullptr);

            UpdateDisplay(-1);

			GSystemSettings->App.LastAutomationFileName = Automation.LastFileName;
		}
	}
}


void __fastcall TfrmMain::miOptimiseDataClick(TObject *Sender)
{
	OpenOptimise(thematrix);
}


void __fastcall TfrmMain::miFontViewerClick(TObject *Sender)
{
	OpenFontViewer();
}
#pragma end_region


#pragma region Toolbar_Top
void __fastcall TfrmMain::sbBuildClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

  // ===========================================================================
  // ===========================================================================

	if (Sender == Load1)
	{
		GSystemSettings->Project.Clear   = false;
		GSystemSettings->Project.Special = tbFrames->Max;    // preserve the frame count when coming in from Load/Import etc.

		ChangeMatrixType();
	}
	else
	{
		ProjectSettings ps = OpenNewProject(GSystemSettings->Project, sbClear->Enabled);

		if (!ps.Valid) return;

		GSystemSettings->Project.Mode             = ps.Mode;
		GSystemSettings->Project.Width            = ps.Width;
		GSystemSettings->Project.Height           = ps.Height;
		GSystemSettings->Project.Clear            = ps.Clear;
		GSystemSettings->Project.Special          = ps.Special;
		GSystemSettings->Project.SizeType         = ps.SizeType;
		GSystemSettings->Project.Shape      	  = ps.Shape;
		GSystemSettings->Project.CustomShapeParam = ps.CustomShapeParam;
		GSystemSettings->Project.Background       = ps.Background;

		tbFrames->Max                  = ps.Special;
		frmPreviewPopout->tbFrames->Max  = tbFrames->Max;

		GSystemSettings->App.LastExport.clear(GSystemSettings->Project.Mode == MatrixMode::kRGB);

		ChangeMatrixType();
	}

	// ===========================================================================
	// ===========================================================================

	int mw = GSystemSettings->Project.Width;
	int mh = GSystemSettings->Project.Height;

	if (miPixelAuto->Checked)
	{
		GSystemSettings->Project.PixelSize = thematrix->GetAutoPixelSize(pCanvas->Width, pCanvas->Height, sbGradient->Tag);
	}

	if (GSystemSettings->Project.PixelSize <= 0)
	{
		GSystemSettings->Project.PixelSize = thematrix->GetAutoPixelSize(pCanvas->Width, pCanvas->Height, sbGradient->Tag);
	}

	switch (GSystemSettings->Project.Shape)
	{
	case PixelShape::kSquare:
		miPixelShapeSquareClick(miPixelShapeSquare);
		break;
	case PixelShape::kCircle:
		miPixelShapeSquareClick(miPixelShapeRound);
		break;
	case PixelShape::kRoundRect:
		miPixelShapeSquareClick(miPixelShapeRoundRect);
		break;
	}

	sColour0->Brush->Color = TColor(GSystemSettings->Project.Background);

	thematrix->NewMatrix(GSystemSettings->Project.Mode, GSystemSettings->Project.Special, CTopOffset, CLeftOffset, mw, mh,
						 GSystemSettings->Project.PixelSize, GSystemSettings->Project.Shape, miGridToggle->Checked, false, GSystemSettings->Project.Clear,
						 GSystemSettings->Project.Background);

	if (GSystemSettings->Project.ShapeCustom != CustomShape::kNone)
	{
		thematrix->SetIgnoredPixelsFromCustomShape(GSystemSettings->Project.ShapeCustom, GSystemSettings->Project.CustomShapeParam);
	}

	// ===========================================================================

	if (GSystemSettings->Project.Clear)
    {
		if (miFontMode->Checked)
		{
			tbFrames->Max = FontCharacterCount;
		}
		else
		{
			tbFrames->Max = thematrix->GetFrameCount();
		}

		frmPreviewPopout->tbFrames->Max = tbFrames->Max;

		FrameLayerPanel->UpdateLayerTable();
	}

	// ===========================================================================

	ManageUIControls(false, false);

	// ===========================================================================

	OldMatrixMode = GSystemSettings->Project.Mode;

	ClearCurrentProjectFileName();
	GSystemSettings->App.LastAutomationFileName = L"";

	UpdateDisplay(-1);

	UpdateMemoryUsage();

	thematrix->Refresh();

	Screen->Cursor = crDefault;

	GSystemSettings->RecalculatePadding(thematrix->Details.Mode, thematrix->Details.Width, thematrix->Details.Height);
	MatrixOnChange(nullptr);
	MatrixOnLayerChange(nullptr);

	FormResize(nullptr);
}


void __fastcall TfrmMain::sbOpenClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	if (sbClear->Enabled)
	{
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kOpeningNewMatrixWillClearCurrentProject] + L"\n\n" +
									  GLanguageHandler->Text[kDoYouWishToContinue]), mtWarning, mbOKCancel, 0) != mrOk) return;
	}

	// =======================================================================

	ConfigureOpenDialog(CLoadProject);

	if (odMain->Execute())
	{
        Application->ProcessMessages();

		LoadFromFileName(odMain->FileName.c_str());

		FormResize(nullptr);
	}

	Application->ProcessMessages();
	thematrix->SetMatrixReadOnly(false);
}


void __fastcall TfrmMain::sbSaveClick(TObject *Sender)
{
	if (GSystemSettings->App.DataFilename.empty())
	{
		miSaveAsClick(nullptr);
	}
	else
	{
		ImportData tid;

		BuildImportData(tid, 0, thematrix->GetFrameCount() - 1);

		if (thematrix->GetSoftwareMode() == SoftwareMode::kFont)
		{
			thematrix->SaveFont(GSystemSettings->App.DataFilename, tid, GSystemSettings->App.LastExport);
		}
		else
		{
			ProjectColours colours = GetColours();

			thematrix->SaveAnimation(GSystemSettings->App.DataFilename, tid, GSystemSettings->App.LastExport, colours);
		}
	}
}


void __fastcall TfrmMain::sbExportClick(TObject *Sender)
{
	OpenExportData(thematrix, GSystemSettings->App.LastExport, ExportSource::kAnimation, thematrix->Details.Mode);

	if (GSystemSettings->App.LastExport.Valid)
	{
		SetSimpleExport(GSystemSettings->App.LastExport);
	}
}


void __fastcall TfrmMain::sbGenerateCodeClick(TObject *Sender)
{
	OpenExportCode(thematrix);
}


void __fastcall TfrmMain::sbPixelSizeClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	puPixelSize->Popup(Left + sbPixelSize->Left, Top + 80);
}


void __fastcall TfrmMain::sbPixelShapeClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	puPixelShape->Popup(Left + sbPixelShape->Left, Top + 80);
}


void __fastcall TfrmMain::sbPresetClick(TObject *Sender)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	puPresets->Popup(Left + sbPreset->Left, Top + 80);
}

#pragma end_region


#pragma region Toolbar_Middle
void __fastcall TfrmMain::sbClearClick(TObject *Sender)
{
	thematrix->ClearCurrentFrame();
}


void __fastcall TfrmMain::sbMirrorClick(TObject *Sender)
{
	int effect = kEffectFlip;

	TSpeedButton *sb = (TSpeedButton*)Sender;

    FrameEffect(sb->Tag);
}


void TfrmMain::FrameEffect(int id)
{
	int effect = kEffectFlip;

	switch (id)
	{
	case 0:
		effect = kEffectFlip;
		break;
	case 1:
		effect = kEffectMirror;
		break;
	case 2:
		effect = kEffectInvert;
		break;
	}

	if (FrameLayerPanel->GetSyncAll())
	{
		thematrix->PerformEffectController(effect, CMOMCurrentFrameLayers);
	}
	else
	{
		thematrix->PerformEffectController(effect, CMOMCurrentOnly);
	}
}


void __fastcall TfrmMain::sbScrollLeftClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	ScrollFrame(sb->Tag);
}


void TfrmMain::ScrollFrame(int scroll_mode)
{
	int direction = kEffectScrollLeft;

	switch (scroll_mode)
	{
	case 0:
		direction = kEffectScrollLeft;
		break;
	case 1:
		direction = kEffectScrollRight;
		break;
	case 2:
		direction = kEffectScrollUp;
		break;
	case 3:
		direction = kEffectScrollDown;
		break;
	}

	if (FrameLayerPanel->GetSyncAll())
	{
		thematrix->PerformScrollController(direction, CMOMCurrentFrameLayers);
	}
	else
	{
		thematrix->PerformScrollController(direction, CMOMCurrentOnly);
	}
}


void __fastcall TfrmMain::sbRotateLClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	RotateFrame(sb->Tag);
}


void TfrmMain::RotateFrame(int direction)
{
	int rdirection = kEffectRotateCW;

	if (direction == 0)
	{
		rdirection = kEffectRotateACW;
	}
	else
	{
		rdirection = kEffectRotateCW;
	}

	if (FrameLayerPanel->GetSyncAll())
	{
		thematrix->RotateFrameController(rdirection, CMOMCurrentFrameLayers);
	}
	else
	{
		thematrix->RotateFrameController(rdirection, CMOMCurrentOnly);
	}
}


void __fastcall TfrmMain::sbRotateAnyClick(TObject *Sender)
{
	Screen->Cursor = crHourGlass;

	thematrix->BackupMatrix();

	std::wstring angle = cbRotateAngle->Text.c_str();

	double byangle = stod(angle.substr(0, angle.length() - 1));

	int origframe = GetSelectedFrame();

	for (int t = 0; t <= cbRotateCount->ItemIndex + 1; t++)
	{
		if (t + origframe + 1 > thematrix->GetFrameCount())
		{
			thematrix->InsertBlankFrameAt(thematrix->GetFrameCount());
		}

		// ===================================================================

		thematrix->RotateFrameAnyAngle(t * byangle, t + origframe);
	}

	UpdateDisplay(-1);

	Screen->Cursor = crDefault;
}


void __fastcall TfrmMain::bLockFrameClick(TObject *Sender)
{
	if (bLockFrame->Tag == 0)
	{
		bLockFrame->Tag = 1;
		thematrix->LockCurrentFrame();
	}
	else
	{
		bLockFrame->Tag = 0;
		thematrix->UnLockCurrentFrame();
	}

	bLockFrame->ImageIndex = 24 + bLockFrame->Tag;

	//if (pLayers->Visible)
	//{
	//	FrameLayerPanel->UpdateExisting();
	//}
}
#pragma end_region


#pragma region Toolbar_Drawing_Tools
void TfrmMain::SetDrawingMode(int drawingmode)
{
	thematrix->Render.Draw.SetModeFromInt(drawingmode);
	thematrix->Render.Draw.Point       = CDrawPointNone;
	thematrix->Render.Draw.Coords[0].X = -1;
	thematrix->Render.Draw.Coords[0].Y = -1;

	if (thematrix->Render.Draw.IsSinglePointMode(thematrix->Render.Draw.Mode))
	{
		thematrix->Render.Draw.SinglePoint  = true;

		thematrix->Render.Draw.Parameter    = DefaultPatternParameter[drawingmode];
  		thematrix->Render.Draw.ParameterMax = DefaultPatternParameterMax[drawingmode];

		if (thematrix->Details.Mode == MatrixMode::kRGB || thematrix->Details.Mode == MatrixMode::kRGB3BPP)
		{
			thematrix->Render.Draw.Colour = 0xFF8822;  // ensures something is drawn as we move before clicking
		}
		else
		{
			thematrix->Render.Draw.Colour = 1;         // ensures something is drawn as we move before clicking
		}
	}
	else
	{
		thematrix->Render.Draw.SinglePoint  = false;

		thematrix->Render.Draw.Parameter    = 0;
		thematrix->Render.Draw.ParameterMax = 0;
	}

	if (thematrix->Render.Draw.Mode == DrawMode::kGradientBrush)
	{
		iMMBGradient->Visible = true;
	}
	else if (iMMBGradient->Visible)
	{
		ToggleGradient(GradientOption::kOff, false);
	}

	thematrix->Render.Draw.CopyPos.X = 0;
	thematrix->Render.Draw.CopyPos.Y = 0;
	thematrix->Render.Draw.Special   = tbFrames->Max;

	UpdateDrawModeCaption(drawingmode);

	thematrix->RefreshCurrentFrame();
}


void __fastcall TfrmMain::sbMouseModeClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	SetDrawingMode(sb->Tag);
}


void __fastcall TfrmMain::sbNewBrushClick(TObject *Sender)
{
	MatrixSettings lMatrixSettings;
	RGBPaletteColours lRGBPaletteColours;
	std::vector<std::wstring> Brush;
	int lRow;

	lMatrixSettings.Mode = thematrix->Details.Mode;
	lMatrixSettings.Width      = thematrix->Details.Width;
	lMatrixSettings.Height     = thematrix->Details.Height;

	lRGBPaletteColours.Left    = sSelectionLMB->Brush->Color;
	lRGBPaletteColours.Middle  = sSelectionMMB->Brush->Color;
	lRGBPaletteColours.Right   = sSelectionRMB->Brush->Color;

	for (int t = 0; t < 28; t++)
	{
		lRGBPaletteColours.History[t] = FramePalettePanel->RGBPaletteHistory[t]->Brush->Color;
	}

	NewBrush newbrush = OpenNewBrush(Brush, lMatrixSettings, lRGBPaletteColours);

	if (newbrush.Proceed)
	{
		for (int row = 0; row < Brush.size(); row++)
		{
			thematrix->StringToRow(true, Brush[row], -1, row, 0, false);
		}

		thematrix->Render.Draw.Point       = CDrawPointNone;
		thematrix->Render.Draw.Mode        = DrawMode::kPaste;
		thematrix->Render.Draw.Coords[0].X = -1;
		thematrix->Render.Draw.Coords[0].Y = -1;

		thematrix->Render.Draw.CopyPos.X   = newbrush.Width;
		thematrix->Render.Draw.CopyPos.Y   = newbrush.Height;
	}
}


void __fastcall TfrmMain::sbGradientClick(TObject *Sender)
{
	switch (sbGradient->Tag)
	{
	case 0:
		sbGradient->Tag = 1;
		ToggleGradient(GradientOption::kVertical, false); 	// inverses display
		break;
	case 1:
		sbGradient->Tag = 2;
		ToggleGradient(GradientOption::kHorizontal, false); // inverses display
		break;
	case 2:
		sbGradient->Tag = 0;
		ToggleGradient(GradientOption::kOff, false); 		// inverses display
		break;
	}

	FormResize(nullptr);
}


void __fastcall TfrmMain::cbMirrorModeChange(TObject *Sender)
{
	switch (cbMirrorMode->ItemIndex)
	{
	case 0:
		thematrix->SetMirrorMode(MirrorMode::kOff);
		break;
	case 1:
		thematrix->SetMirrorMode(MirrorMode::kHorizontal);
		break;
	case 2:
		thematrix->SetMirrorMode(MirrorMode::kVertical);
        break;
	}
}
#pragma end_region


#pragma region Toolbar_PaletteMonoBi
void __fastcall TfrmMain::sColour3MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	DrawMode dm = thematrix->Render.Draw.Mode;

	thematrix->Render.Draw.Mode = DrawMode::kNone;

	TShape *shape = (TShape*)Sender;

	switch (thematrix->Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		if (Button == mbLeft)
		{
			int colour = shape->Tag;

			sSelectionLMB->Brush->Color = TColor(thematrix->LEDColours[colour]);
			sSelectionLMB->Tag          = shape->Tag;
		}
		else if (Button == mbMiddle)
		{
			int colour = shape->Tag;

			sSelectionMMB->Brush->Color = TColor(thematrix->LEDColours[colour]);
			sSelectionMMB->Tag          = shape->Tag;
		}
		else if (Button == mbRight)
		{
			int colour = shape->Tag;

			sSelectionRMB->Brush->Color = TColor(thematrix->LEDColours[colour]);
			sSelectionRMB->Tag          = shape->Tag;
		}

		thematrix->SetMouseButtonColours(sSelectionLMB->Tag,
										 sSelectionMMB->Tag,
										 sSelectionRMB->Tag);
		break;
	case MatrixMode::kRGB:
	{
		colorDialog->Color = shape->Brush->Color;

		if (colorDialog->Execute())
		{
			int old = thematrix->RGBBackground;

			shape->Brush->Color = colorDialog->Color;

			GenerateShades(colorDialog->Color);

			if (Sender == sColour0)
			{
				thematrix->RGBBackground = shape->Brush->Color;
			}
			else if (Sender == sSelectionLMB)
			{
				thematrix->LEDRGBColours[CMouseLeft] = colorDialog->Color;
			}
			else if (Sender == sSelectionMMB)
			{
				thematrix->LEDRGBColours[CMouseMiddle] = colorDialog->Color;
			}
			else if (Sender == sSelectionRMB)
			{
				thematrix->LEDRGBColours[CMouseRight]  = colorDialog->Color;
			}

			if (Sender == sColour0)
			{
				if (old != colorDialog->Color)
				{
					if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kBackgroundColourHasChanged] +
								  L" \n\n" +
								  GLanguageHandler->Text[kChangeAllBackgroundPixels]), mtInformation, mbOKCancel, 0) == mrOk)
						thematrix->ChangePixels(old, colorDialog->Color);
				}
			}
		}

		thematrix->SetMouseButtonColours(sSelectionLMB->Brush->Color,
										 sSelectionMMB->Brush->Color,
										 sSelectionRMB->Brush->Color);
		break;
	}
	case MatrixMode::kRGB3BPP:
		break;
	}

	thematrix->Render.Draw.Mode = dm;
}
#pragma end_region


#pragma region Toolbar_PaletteRGB
void __fastcall TfrmMain::sRGBPalette1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (thematrix->Details.Mode == MatrixMode::kRGB)
	{
		TShape *shape = (TShape*)Sender;

		int index = shape->Tag;

		if (thematrix->Render.Draw.Mode == DrawMode::kPicker || Shift.Contains(ssCtrl))
		{
		  	colorDialog->Color = shape->Brush->Color;

			if (colorDialog->Execute())
			{
				shape->Brush->Color = colorDialog->Color;
			}
		}
		else
		{
			if (Shift.Contains(ssLeft))
			{
				sSelectionLMB->Brush->Color          = _RGBPalette[index]->Brush->Color;

				thematrix->LEDRGBColours[CMouseLeft] = _RGBPalette[index]->Brush->Color;
			}
			else if (Shift.Contains(ssMiddle))
			{
				sSelectionMMB->Brush->Color            = _RGBPalette[index]->Brush->Color;

				thematrix->LEDRGBColours[CMouseMiddle] = _RGBPalette[index]->Brush->Color;
			}
			else if (Shift.Contains(ssRight))
			{
				sSelectionRMB->Brush->Color            = _RGBPalette[index]->Brush->Color;

				thematrix->LEDRGBColours[CMouseRight]  = _RGBPalette[index]->Brush->Color;
			}

			thematrix->SetMouseButtonColours(thematrix->LEDRGBColours[CMouseLeft],
										   thematrix->LEDRGBColours[CMouseMiddle],
										   thematrix->LEDRGBColours[CMouseRight]);

			FramePalettePanel->AddToHistory(shape->Brush->Color);

			GenerateShades(shape->Brush->Color);
		}
	}
}


void __fastcall TfrmMain::sRGBPalette1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	std::wstring hex = IntToHex(ColourUtility::RGBConvertTo32(shape->Brush->Color, RGBMode::kRGB, LeastSignificantBit::kBottomRight, 100), 6).c_str();

	std::wstring caption = GSystemSettings->App.HexPrefix + hex + L" (" + ColourUtility::RGBConvertToSplit(shape->Brush->Color, RGBMode::kRGBSimple, 100, NumberFormat::kDecimal, L"", L" ", ColourSpace::kRGB32) + L")";

	lPixelColour->Caption = caption.c_str();
}


void __fastcall TfrmMain::sShade1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
	if (thematrix->Details.Mode == MatrixMode::kRGB)
	{
		TShape *shape = (TShape*)Sender;

		TColor colour = shape->Brush->Color;

		if (Shift.Contains(ssLeft))
		{
			sSelectionLMB->Brush->Color = colour;

			thematrix->LEDRGBColours[CMouseLeft] = colour;
		}
		else if (Shift.Contains(ssMiddle))
		{
			sSelectionMMB->Brush->Color = colour;

			thematrix->LEDRGBColours[CMouseMiddle] = colour;
		}
		else if (Shift.Contains(ssRight))
		{
			sSelectionRMB->Brush->Color = colour;

			thematrix->LEDRGBColours[CMouseRight] = colour;
		}

		thematrix->SetMouseButtonColours(thematrix->LEDRGBColours[CMouseLeft],
										 thematrix->LEDRGBColours[CMouseMiddle],
										 thematrix->LEDRGBColours[CMouseRight]);

        FramePalettePanel->AddToHistory(shape->Brush->Color);

		if (shape->Tag != 999)
		{
			GenerateShades(shape->Brush->Color);
		}
	}
}


void __fastcall TfrmMain::sRGB3pp1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
	if (thematrix->Details.Mode == MatrixMode::kRGB3BPP)
	{
		TShape *shape = (TShape*)Sender;

		int colour = shape->Tag;

		if (Shift.Contains(ssLeft))
		{
			sSelectionLMB->Brush->Color = shape->Brush->Color;

			thematrix->LEDRGBColours[CMouseLeft] = colour;
		}
		else if (Shift.Contains(ssMiddle))
		{
			sSelectionMMB->Brush->Color = shape->Brush->Color;

			thematrix->LEDRGBColours[CMouseMiddle] = colour;
		}
		else if (Shift.Contains(ssRight))
		{
			sSelectionRMB->Brush->Color = shape->Brush->Color;

			thematrix->LEDRGBColours[CMouseRight] = colour;
		}

		thematrix->SetMouseButtonColours(thematrix->LEDRGBColours[CMouseLeft],
										 thematrix->LEDRGBColours[CMouseMiddle],
										 thematrix->LEDRGBColours[CMouseRight]);
	}
}


void __fastcall TfrmMain::tbFramesTracking(TObject *Sender)
{
	SetFrameCaption(GetSelectedFrame());

	thematrix->SetAndShowCurrentFrame(GetSelectedFrame());
}
#pragma end_region


#pragma region Toolbar_Animation
void __fastcall TfrmMain::bTimerClick(TObject *Sender)
{
	puAnimationSpeed->Popup(Left + bTimer->Left, pAnimationToolbar->Top - 80);
}


void __fastcall TfrmMain::bPlayAnimationClick(TObject *Sender)
{
	TBitBtn *bb = (TBitBtn*)Sender;

	PlaybackCommand(bb->Tag);
}


void __fastcall TfrmMain::bAddFrameClick(TObject *Sender)
{
	int oldselectedframe = tbFrames->Position;

	thematrix->InsertBlankFrameAt(tbFrames->Position);

	tbFrames->Max = thematrix->GetFrameCount();
	tbFrames->Position  = oldselectedframe + 1;

	thematrix->SetAndShowCurrentFrame(GetSelectedFrame());

	frmPreviewPopout->tbFrames->Max = tbFrames->Max;
	frmPreviewPopout->tbFrames->Position = tbFrames->Position;

	UpdateDisplay(-1);
}


void __fastcall TfrmMain::bAddFrameCopyClick(TObject *Sender)
{
	int oldselectedframe = tbFrames->Position;

	thematrix->InsertCopyFrameAt(tbFrames->Position - 1);

	tbFrames->Max = thematrix->GetFrameCount();
	tbFrames->Position  = oldselectedframe + 1;

	thematrix->SetAndShowCurrentFrame(GetSelectedFrame());

	frmPreviewPopout->tbFrames->Max = tbFrames->Max;
	frmPreviewPopout->tbFrames->Position = tbFrames->Position;

	UpdateDisplay(-1);
}


void __fastcall TfrmMain::bAddFrameMultipleClick(TObject *Sender)
{
	std::wstring s = InputBox(GLanguageHandler->Text[kAddBlankFrames].c_str(),
							  GLanguageHandler->Text[kHowManyFrames].c_str(), L"1").c_str();

	if (!s.empty() && Utility::ValidateNumber(s, 99))
	{
		int sf = stoi(s);

		thematrix->AddFrameMultiple(sf, GetSelectedFrame());

		UpdateDisplay(-1);
	}
	else
	{
		MessageDlg(GLanguageHandler->Text[kInvalidNumberFramesToAdd].c_str(), mtWarning, mbOKCancel, 0);
	}
}


void __fastcall TfrmMain::bDeleteFrameClick(TObject *Sender)
{
	if (tbFrames->Position == 1 && tbFrames->Max == 1)
	{
	}
	else
	{
		thematrix->DeleteFrame(GetSelectedFrame());

		UpdateDisplay(-1);
	}
}


void __fastcall TfrmMain::bDeleteMultipleFramesClick(TObject *Sender)
{
	DeleteMultipleObject dmo = OpenDeleteMultiple(thematrix->GetFrameCount());

	if (dmo.Process)
	{
		int frame = 0;

		for (int t = dmo.StartFrame; t <= dmo.EndFrame; t++)
		{
			thematrix->DeleteFrame(dmo.StartFrame);
		}

		if (tbFrames->Position > thematrix->GetFrameCount())
		{
			frame = thematrix->GetFrameCount();
		}
		else
		{
			frame = -1;
		}

		UpdateDisplay(frame);
	}
}


void __fastcall TfrmMain::bLightboxClick(TObject *Sender)
{
	if (bLightbox->Tag == 0)
	{
		bLightbox->Tag = 1;
		bLightbox->ImageIndex = 38;
	}
	else
	{
		bLightbox->Tag = 0;
		bLightbox->ImageIndex = 76;
	}

	thematrix->SetLightBox(bLightbox->Tag);
}


void __fastcall TfrmMain::tbFramesChange(TObject *Sender)
{
//
}
#pragma end_region


#pragma region Timers
#pragma end_region


#pragma region FileIO
bool TfrmMain::LoadFromFileName(const std::wstring file_name)
{
	// flushes the message queue, stops the mouse double click from the open dialog
	// causing a pixel to be drawm on the first frame
	Application->ProcessMessages();

	// =======================================================================

	GSystemSettings->App.LastExport.clear(false);

	ImportData ted = thematrix->LoadLEDMatrixData(file_name, GSystemSettings->App.LastExport, LoadMode::kNew, -1);

	// =======================================================================

	if (ted.ImportOk)
	{
		GSystemSettings->Project.Mode    = ted.Mode;
		ChangeMatrixType();

		GSystemSettings->Project.Width = ted.NewWidth;
		GSystemSettings->Project.Height = ted.NewHeight;

		tbFrames->Max                  = thematrix->GetFrameCount();
		frmPreviewPopout->tbFrames->Max = tbFrames->Max;

		sbBuildClick(Load1);

		bDeleteFrame->Enabled          = (tbFrames->Max > 1);
		bDeleteMultipleFrames->Enabled = (tbFrames->Max > 1);

		if (ted.Colours.HasData)
		{
			sSelectionLMB->Brush->Color = TColor(ted.Colours.DrawColours[CMouseLeft]);
			sSelectionMMB->Brush->Color = TColor(ted.Colours.DrawColours[CMouseMiddle]);
			sSelectionRMB->Brush->Color = TColor(ted.Colours.DrawColours[CMouseRight]);

			thematrix->LEDRGBColours[CMouseLeft]   = TColor(ted.Colours.DrawColours[CMouseLeft]);
			thematrix->LEDRGBColours[CMouseMiddle] = TColor(ted.Colours.DrawColours[CMouseMiddle]);
			thematrix->LEDRGBColours[CMouseRight]  = TColor(ted.Colours.DrawColours[CMouseRight]);

			thematrix->SetMouseButtonColours(sSelectionLMB->Brush->Color,
										   sSelectionMMB->Brush->Color,
										   sSelectionRMB->Brush->Color);

			for (int i = 0; i < 16; i++)
			{
				_RGBPalette[i]->Brush->Color = TColor(ted.Colours.CustomColours[i]);
			}

			for (int i = 0; i < 28; i++)
			{
				FramePalettePanel->RGBPaletteHistory[i]->Brush->Color = TColor(ted.Colours.PaletteHistory[i]);
			}
		}

		thematrix->SetAndShowCurrentFrame(0);

		SetFrameCaption(0);

		SetCurrentProjectFileName(file_name);

		// ===========================================================================

		switch (ted.Mode)
		{
		case MatrixMode::kNone:
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			break;
		case MatrixMode::kRGB:
		case MatrixMode::kRGB3BPP:
			sColour0->Brush->Color = TColor(ted.BackgroundColour);
            break;
		}

		// ===========================================================================

		if (ted.FontMode)
		{
			if (!miFontMode->Checked)
			{
				miFontMode->Checked = true;
				miFontModeClick(nullptr);
			}

			bPlayAnimationClick(bPlayAnimation);
		}
		else
		{
			if (miFontMode->Checked)
			{
				miFontMode->Checked = false;
				miFontModeClick(nullptr);
			}
		}

		// == preview ================================================================

		miPreview->Checked         = ted.Preview.Enabled;
		thematrix->SetPreviewActive(ted.Preview.Enabled);

		SetPreview(ted.Preview.Size, ted.Preview.View, ted.Preview.Void, ted.Preview.Offset, ted.Preview.OffsetDirection, ted.Preview.Popout);

		// ===========================================================================

		GSystemSettings->App.LastAutomationFileName = ted.AutomationFileName;

		UpdateMemoryUsage();
	}

	if (!ted.ErrorString.empty())
	{
		MessageDlg(Utility::WS2US(ted.ErrorString), mtError, mbOKCancel, 0);

		return false;
	}

	return true;
}


bool TfrmMain::AppendFromFileName(const std::wstring file_name)
{
	// flushes the message queue, stops the mouse double click from the open dialog
	// causing a pixel to be drawm on the first frame
	Application->ProcessMessages();

	GSystemSettings->App.LastExport.clear(false);

	ImportData ted = thematrix->LoadLEDMatrixData(file_name, GSystemSettings->App.LastExport, LoadMode::kAppend, -1);

	if (ted.ImportOk)
	{
		tbFrames->Max = thematrix->GetFrameCount();

		frmPreviewPopout->tbFrames->Max = tbFrames->Max;

		UpdateMemoryUsage();
	}
	else
	{
		if (!ted.ErrorString.empty())
		{
			MessageDlg(ted.ErrorString.c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
		}
	}

	return ted.ImportOk;
}


bool TfrmMain::MergeFromFileName(const std::wstring file_name, int start_frame, LoadMode merge_mode)
{
	// flushes the message queue, stops the mouse double click from the open dialog
	// causing a pixel to be drawm on the first frame
	Application->ProcessMessages();

	GSystemSettings->App.LastExport.clear(false);

	ImportData ted = thematrix->LoadLEDMatrixData(file_name, GSystemSettings->App.LastExport, merge_mode, start_frame);

	if (ted.ImportOk)
	{
		tbFrames->Max = thematrix->GetFrameCount();
		frmPreviewPopout->tbFrames->Max = tbFrames->Max;

		UpdateMemoryUsage();
	}
	else
	{
		if (!ted.ErrorString.empty())
		{
			MessageDlg(ted.ErrorString.c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
		}
	}

	return ted.ImportOk;
}


bool TfrmMain::LoadFromGIF(const std::wstring file_name)
{
	// flushes the message queue, stops the mouse double click from the open dialog
	// causing a pixel to be drawm on the first frame
	Application->ProcessMessages();

	// =======================================================================

	GSystemSettings->App.LastExport.clear(false);

	ImportData ted = thematrix->ImportFromGIF(file_name);

	// =======================================================================

	if (ted.ImportOk)
	{
		GSystemSettings->Project.Mode = ted.Mode;
		ChangeMatrixType();

		GSystemSettings->Project.Width = ted.NewWidth;
		GSystemSettings->Project.Height = ted.NewHeight;

		tbFrames->Max                   = thematrix->GetFrameCount();
		frmPreviewPopout->tbFrames->Max = tbFrames->Max;

		sbBuildClick(Load1);

		bDeleteFrame->Enabled          = (tbFrames->Max > 1);
		bDeleteMultipleFrames->Enabled = (tbFrames->Max > 1);

		thematrix->SetAndShowCurrentFrame(0);

		SetFrameCaption(0);

		ClearCurrentProjectFileName();

		sColour0->Brush->Color = TColor(ted.BackgroundColour);

		// ===================================================================

		if (miFontMode->Checked)
		{
			miFontMode->Checked = false;
			miFontModeClick(nullptr);
		}

		// == preview ========================================================

		miPreview->Checked        = ted.Preview.Enabled;
		thematrix->SetPreviewActive(ted.Preview.Enabled);

		SetPreview(ted.Preview.Size, ted.Preview.View, ted.Preview.Void, ted.Preview.Offset, ted.Preview.OffsetDirection, ted.Preview.Popout);

		// ===================================================================

		GSystemSettings->App.LastLoadLocation = ExtractFilePath(odMain->FileName.c_str());
	}
	else
	{
		if (!ted.ErrorString.empty())
		{
			MessageDlg(ted.ErrorString.c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
		}

		return false;
	}

	UpdateMemoryUsage();

	return true;
}
#pragma end_region


#pragma region Export_Import
void TfrmMain::SetSimpleExport(ExportOptions teo)
{ /* disabled in orginal source
	cbSource.ItemIndex          := Ord(aTEO.Source);

	cbSourceChange(Nil);

	cbSourceLSB.ItemIndex       := Ord(aTEO.LSB);
	cbSourceDirection.ItemIndex := Ord(aTEO.Orientation);

	cbRowsLSBChange(Nil); */
}
#pragma end_region


void TfrmMain::BuildImportData(ImportData &id, int start_frame, int end_frame)
{
	id.PadMode                   = GSystemSettings->App.PadMode;
	id.Mode                      = thematrix->Details.Mode;
	id.ASCIIIndex                = GSystemSettings->App.ASCIIIndex;
	id.MaxFrames                 = tbFrames->Max;
	id.AutomationFileName        = GSystemSettings->App.LastAutomationFileName;

	id.StartFrame                = start_frame;
	id.EndFrame                  = end_frame;

	id.Preview.Enabled           = thematrix->GetPreviewActive();
	id.Preview.Size              = thematrix->GetPreviewBoxSize();
	id.Preview.View              = thematrix->GetPreviewView();
	id.Preview.Void              = thematrix->GetPreviewVoid();
	id.Preview.Offset            = thematrix->GetRadialOffset();
	id.Preview.OffsetDirection   = thematrix->GetRadialOffsetDirection();
	id.Preview.IncrementRadially = thematrix->GetPreviewIncRadially();
}


ProjectColours TfrmMain::GetColours()
{
	ProjectColours pc;

	pc.DrawColours[CMouseLeft]   = sSelectionLMB->Brush->Color;
	pc.DrawColours[CMouseMiddle] = sSelectionMMB->Brush->Color;
	pc.DrawColours[CMouseRight]  = sSelectionRMB->Brush->Color;

	for (int t = 0; t < 16; t++)
	{
		pc.CustomColours[t] = _RGBPalette[t]->Brush->Color;
	}

	for (int t = 0; t < 28; t++)
	{
		pc.PaletteHistory[t] = FramePalettePanel->RGBPaletteHistory[t]->Brush->Color;
	}

    return pc;
}


void TfrmMain::ChangeMatrixType()
{
	bool statusMouseButtonSelect = false;
	bool statusColourSelect0 = false;
	bool statusColourSelect123 = false;
	bool statusBackground = false;

	switch (GSystemSettings->Project.Mode)
	{
	case MatrixMode::kMono:
		pRGB_3BPP->Visible       = false;
		statusBackground        = false;
		statusMouseButtonSelect = false;
		statusColourSelect0     = false;
		statusColourSelect123   = false;
		pCurrentColours->Visible = false;
		panelRGBPalette->Visible = false;
		break;
	case MatrixMode::kBiSequential:
		pRGB_3BPP->Visible       = false;
		statusBackground        = false;
		statusMouseButtonSelect = true;
		statusColourSelect0     = true;
		statusColourSelect123   = true;
		pCurrentColours->Visible = true;
		panelRGBPalette->Visible = false;
		break;
	case MatrixMode::kBiBitplanes:
		pRGB_3BPP->Visible       = false;
		statusBackground        = false;
		statusMouseButtonSelect = true;
		statusColourSelect0     = true;
		statusColourSelect123   = true;
		pCurrentColours->Visible = true;
		panelRGBPalette->Visible = false;
		break;
	case MatrixMode::kRGB:
		pRGB_3BPP->Visible       = false;
		statusBackground        = true;
		statusMouseButtonSelect = true;
		statusColourSelect0     = true;
		statusColourSelect123   = false;
		pCurrentColours->Visible = true;
		panelRGBPalette->Visible = true;
		break;
	case MatrixMode::kRGB3BPP:
		pRGB_3BPP->Visible       = true;
		statusBackground        = true;
		statusMouseButtonSelect = true;
		statusColourSelect0     = true;
		statusColourSelect123   = false;
		pCurrentColours->Visible = true;
		panelRGBPalette->Visible = true;
		break;

	default:
		statusBackground        = false;
		statusMouseButtonSelect = false;
		statusColourSelect0     = false;
		statusColourSelect123   = false;

		GSystemSettings->Project.Mode = MatrixMode::kMono;
	}

	lBackground->Visible     = statusBackground;
	lPixelColour->Visible    = statusBackground;
	miFadeFirstLast->Enabled = statusBackground;

	sColour0->Visible        = statusColourSelect0;
	sColour1->Visible        = statusColourSelect123;
	sColour2->Visible        = statusColourSelect123;
	sColour3->Visible        = statusColourSelect123;

	sSelectionLMB->Visible   = statusMouseButtonSelect;
	sSelectionMMB->Visible   = statusMouseButtonSelect;
	sSelectionRMB->Visible   = statusMouseButtonSelect;
	iColoursLeft->Visible    = statusMouseButtonSelect;
	iColoursMiddle->Visible  = statusMouseButtonSelect;
	iColoursRight->Visible   = statusMouseButtonSelect;

	// ===========================================================================

	thematrix->ChangeMatrixMode(GSystemSettings->Project.Mode);

	SetupMatrixColours();

	GSystemSettings->RecalculatePadding(thematrix->Details.Mode, thematrix->Details.Width, thematrix->Details.Height);

	// ===========================================================================

	switch (thematrix->Details.Mode)
	{
	case MatrixMode::kMono:
		if (sbGradient->Tag == 1)
		{
			ToggleGradient(GradientOption::kOff, true);
		}

		sbGradient->Enabled               = false;
		miClearAllFramesGradient->Enabled = false;
		sbRandomDraw->Enabled             = false;
		miGradientAllFrames->Enabled      = false;
		sbPicker->Enabled                 = false;
		break;
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
	case MatrixMode::kRGB3BPP:
		if (sbGradient->Tag == 1)
		{
			ToggleGradient(GradientOption::kVertical, false);
		}

		sbGradient->Enabled              = true;
		miClearAllFramesGradient->Enabled = true;
		sbRandomDraw->Enabled            = true;
		miGradientAllFrames->Enabled     = true;
		sbPicker->Enabled                = false;
		break;
	case MatrixMode::kRGB:
		if (sbGradient->Tag == 1)
		{
			ToggleGradient(GradientOption::kVertical, true);
		}

		sbPicker->Enabled = true;

		iMMBGradient->Visible = false;
		break;
	}

	FormResize(nullptr);
}


void TfrmMain::SetCurrentProjectFileName(const std::wstring file_name)
{
	GSystemSettings->App.DataFilename = file_name;

	std::wstring efn = ExtractFileName(GSystemSettings->App.DataFilename.c_str()).c_str();

	Caption = Utility::WS2US(BackupCaption + L"  [ " + efn + L" ]");

	GSystemSettings->App.LastSaveLocation = ExtractFilePath(file_name.c_str()).c_str();

	auto it = std::find(GSystemSettings->FileHistory.begin(), GSystemSettings->FileHistory.end(), file_name);

	if (it == GSystemSettings->FileHistory.end())
	{
		GSystemSettings->FileHistory.insert(GSystemSettings->FileHistory.begin(), file_name);

		if (GSystemSettings->FileHistory.size() > 20)
		{
			GSystemSettings->FileHistory.pop_back();
		}

		BuildReOpenMenu();
	}
}


#pragma region GUI
// input MUST be zero-indexed to match matrix->cells storage
// sets the frame caption to i, and the tbframes trackbar to i + 1 (as it starts from 1)
void TfrmMain::SetFrameCaption(int i)
{
	if (thematrix->GetSoftwareMode() == SoftwareMode::kFont)
	{
		lFrame->Caption = Char(i + GSystemSettings->App.ASCIIIndex);

		std::wstring caption = L"ASCII: " + std::to_wstring(i + GSystemSettings->App.ASCIIIndex - 1);

		pASCIICode->Caption = caption.c_str();

		FrameFontPanel->SetFontAscii(i + GSystemSettings->App.ASCIIIndex);
	}
	else
	{
		lFrame->Caption = Utility::WS2US(std::to_wstring(i + 1) + L" / " + std::to_wstring(tbFrames->Max));
	}

	frmPreviewPopout->lFrame->Caption = lFrame->Caption;

	if (tbFrames->Position != i + 1)
	{
		tbFrames->Position = i + 1;
	}

	if (frmPreviewPopout->tbFrames->Position != i + 1)
	{
		frmPreviewPopout->tbFrames->Position = i + 1;
	}

	lFrame->Refresh();
}


void TfrmMain::UpdateMemoryUsage()
{
	int size = thematrix->CalculateMemoryUsage();

	std::wstring dimensions = std::to_wstring(thematrix->Details.Width) + L" x " + std::to_wstring(thematrix->Details.Height);

	std::wstring caption = L"";

	if (size < 32768)
	{
		caption = dimensions + L", " + std::to_wstring(size) + L" " + GLanguageHandler->Text[kBytes];
	}
	else if (size < 1048576)
	{
		caption = dimensions + L", " + FloatToStrF((size / 1024), ffFixed, 7, 3).c_str() + L" KB";
	}
	else
	{
		caption = dimensions + L", " + FloatToStrF((size / 1048576), ffFixed, 7, 3).c_str() + L" MB";
	}

	caption += L" (" + ConstantsHelper::MatrixModeAsString(thematrix->Details.Mode) + L")";

	lMemoryUsage->Caption = caption.c_str();

	lMemoryUsage->Hint = std::to_wstring(size) + L" " + GLanguageHandler->Text[kBytes];
}


void TfrmMain::UpdateData()
{
	if (!sbClear->Enabled || !pQuickData->Visible) return;

	DataOutDisplay dod;

	if (thematrix->Details.Mode != MatrixMode::kRGB &&
		thematrix->Details.Mode != MatrixMode::kRGB3BPP)
	{
		switch (thematrix->Details.Mode)
		{
		case MatrixMode::kMono:
			dod = ExportMonoBi::SimpleExportMono(thematrix,
												 GetSelectedFrame(),
												 FrameQuickData->GetLSB(),
												 FrameQuickData->GetSource(),
												 FrameQuickData->GetDirection(),
												 FrameQuickData->GetHex(),
												 FrameQuickData->GetCombineNybbles(),
												 pQuickData->Visible);
			break;
		case MatrixMode::kBiSequential:	// bicolour, sequential bits
			dod = ExportMonoBi::SimpleExportBiSequential(thematrix,
												 GetSelectedFrame(),
												 FrameQuickData->GetLSB(),
												 FrameQuickData->GetSource(),
												 FrameQuickData->GetDirection(),
												 FrameQuickData->GetHex(),
												 pQuickData->Visible);
			break;
		case MatrixMode::kBiBitplanes:	// bicolour, bitplanes
			dod = ExportMonoBi::SimpleExportBiBitplanes(thematrix,
												 GetSelectedFrame(),
												 FrameQuickData->GetLSB(),
												 FrameQuickData->GetSource(),
												 FrameQuickData->GetDirection(),
												 FrameQuickData->GetHex(),
												 pQuickData->Visible);
			break;
		}

		FrameQuickData->SetText(dod.Text);
	}
}


void TfrmMain::UpdateDisplay(int new_frame_position)
{
	tbFrames->Max = thematrix->GetFrameCount();

	bDeleteFrame->Enabled          = (thematrix->GetFrameCount() > 1);
	bDeleteMultipleFrames->Enabled = bDeleteFrame->Enabled;

	miDeleteFrame->Enabled          = bDeleteFrame->Enabled;
	miDeleteMultipleFrames->Enabled = bDeleteFrame->Enabled;

	if (new_frame_position > 0)
	{
		tbFrames->Position = new_frame_position;
	}

	frmPreviewPopout->tbFrames->Max      = tbFrames->Max;
	frmPreviewPopout->tbFrames->Position = new_frame_position;

	SetFrameCaption(GetSelectedFrame());

	// move to onnewframedisplayed
	if (thematrix->IsLocked())
	{
		bLockFrame->Tag = 1;
	}
	else
	{
		bLockFrame->Tag = 0;
	}

	bLockFrame->ImageIndex = 24 + bLockFrame->Tag;
}
#pragma end_region


void TfrmMain::SetupMatrixColours()
{
	for (int colour = 0; colour < 6; colour++)
	{
		if (GSystemSettings->Project.Mode == MatrixMode::kMono)
		{
			thematrix->LEDColours[colour] = thematrix->LEDColoursSingle[colour];
		}
		else
		{
			thematrix->LEDColours[colour] = thematrix->LEDColoursBi[colour];
		}
	}

	switch (GSystemSettings->Project.Mode)
	{
	case MatrixMode::kRGB:
		sColour0->Brush->Color = TColor(thematrix->RGBBackground);

		if (thematrix->LEDRGBColours[CMouseLeft] < 10 &&
			thematrix->LEDRGBColours[CMouseMiddle] < 10 &&
			thematrix->LEDRGBColours[CMouseRight] < 10)
		{
			thematrix->LEDRGBColours[CMouseLeft]   = 0x000000ff;
			thematrix->LEDRGBColours[CMouseMiddle] = 0x00ff0000;
			thematrix->LEDRGBColours[CMouseRight]  = 0x00000000;
		}

		sSelectionLMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseLeft]);
		sSelectionMMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseMiddle]);
		sSelectionRMB->Brush->Color = TColor(thematrix->LEDRGBColours[CMouseRight]);

		thematrix->SetMouseButtonColours(thematrix->LEDRGBColours[CMouseLeft],
										 thematrix->LEDRGBColours[CMouseMiddle],
										 thematrix->LEDRGBColours[CMouseRight]);
		break;
	case MatrixMode::kRGB3BPP:
		sColour0->Brush->Color      = TColor(thematrix->RGBBackground);

		thematrix->LEDRGBColours[CMouseLeft]   = 4; // red
		thematrix->LEDRGBColours[CMouseMiddle] = 2; // green
		thematrix->LEDRGBColours[CMouseRight]  = 0; // black

		sSelectionLMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[4]);
		sSelectionMMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[2]);
		sSelectionRMB->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[0]);

		thematrix->SetMouseButtonColours(4, 2, 0);
		break;

	default:
		sColour0->Brush->Color      = TColor(thematrix->LEDColours[0]);

		sSelectionLMB->Brush->Color = TColor(thematrix->LEDColours[sSelectionLMB->Tag]);
		sSelectionMMB->Brush->Color = TColor(thematrix->LEDColours[sSelectionMMB->Tag]);
		sSelectionRMB->Brush->Color = TColor(thematrix->LEDColours[sSelectionRMB->Tag]);

		thematrix->SetMouseButtonColours(sSelectionLMB->Tag,
										 sSelectionMMB->Tag,
										 sSelectionRMB->Tag);
	}

	sColour1->Brush->Color = TColor(thematrix->LEDColours[CMouseLeft]);
	sColour2->Brush->Color = TColor(thematrix->LEDColours[CMouseMiddle]);
	sColour3->Brush->Color = TColor(thematrix->LEDColours[CMouseRight]);
}


void TfrmMain::ToggleGradient(GradientOption go, bool clear_gradient)
{
	if (thematrix->Details.Mode == MatrixMode::kMono)
	{
		iMMBGradient->Visible  = false;
		sSelectionMMB->Visible = false;
	}
	else
	{
		switch (go)
		{
		case GradientOption::kOff:
			for (int t = 0; t < __MaxHeight; t++)
			{
				if (MatrixGradient[t] != nullptr)
				{
					MatrixGradient[t]->Visible = false;
				}

				iMMBGradient->Visible  = false;
				sSelectionMMB->Visible = true;

				sbGradient->Tag = 0;
			}
			break;
		case GradientOption::kVertical:
			for (int t = 0; t < thematrix->Details.Height; t++)
			{
				if (MatrixGradient[t] == nullptr)
				{
					TShape *shape = new TShape(this);

					MatrixGradient[t] = shape;
				}

				MatrixGradient[t]->Parent      = pCanvas;
				MatrixGradient[t]->Visible     = true;
				MatrixGradient[t]->Tag         = t;
				MatrixGradient[t]->OnMouseDown = OnGradientClick;

				if (thematrix->GetPreviewActive())
				{
					MatrixGradient[t]->	Left = CLeftOffset + ((thematrix->Details.Width + 1) * GSystemSettings->Project.PixelSize) + (thematrix->Details.Width * thematrix->GetPreviewBoxSize()) + 25;
				}
				else
				{
					MatrixGradient[t]->	Left = CLeftOffset + ((thematrix->Details.Width + 1) * GSystemSettings->Project.PixelSize);
				}

				MatrixGradient[t]->	Top  = CTopOffset + (t * GSystemSettings->Project.PixelSize);

				if (MatrixGradient[t]->Width != GSystemSettings->Project.PixelSize + 1)
				{
					MatrixGradient[t]->Width = GSystemSettings->Project.PixelSize + 1;
				}

				if (MatrixGradient[t]->Height != GSystemSettings->Project.PixelSize + 1)
				{
					MatrixGradient[t]->Height = GSystemSettings->Project.PixelSize + 1;
				}

				MatrixGradient[t]->Brush->Color = TColor(thematrix->Render.Gradient.IY[t]);

				if (clear_gradient)
				{
					if (thematrix->Details.Mode == MatrixMode::kRGB)
					{
						thematrix->Render.Gradient.IY[t] = thematrix->RGBBackground;
					}
					else
					{
						thematrix->Render.Gradient.IY[t] = 0;
					}
				}

				iMMBGradient->Visible  = true;
				sSelectionMMB->Visible = false;

				sbGradient->Tag = 1;
			}
			break;
		case GradientOption::kHorizontal:
			for (int t = 0; t < thematrix->Details.Width; t++)
			{
				if (MatrixGradient[t] == nullptr)
				{
					TShape *shape = new TShape(this);

					MatrixGradient[t] = shape;
				}

				MatrixGradient[t]->Parent      = pCanvas;
				MatrixGradient[t]->Visible     = true;
				MatrixGradient[t]->Tag         = t;
				MatrixGradient[t]->OnMouseDown = OnGradientClick;

				MatrixGradient[t]->Left = CLeftOffset + (t * GSystemSettings->Project.PixelSize);
				MatrixGradient[t]->Top  = CTopOffset + ((thematrix->Details.Height + 1) * GSystemSettings->Project.PixelSize);

				if (MatrixGradient[t]->Width != GSystemSettings->Project.PixelSize + 1)
				{
					MatrixGradient[t]->Width = GSystemSettings->Project.PixelSize + 1;
				}

				if (MatrixGradient[t]->Height != GSystemSettings->Project.PixelSize + 1)
				{
					MatrixGradient[t]->Height = GSystemSettings->Project.PixelSize + 1;
				}

				MatrixGradient[t]->Brush->Color = TColor(thematrix->Render.Gradient.IX[t]);

				if (clear_gradient)
				{
					if (thematrix->Details.Mode == MatrixMode::kRGB)
					{
						thematrix->Render.Gradient.IX[t] = thematrix->RGBBackground;
					}
					else
					{
						thematrix->Render.Gradient.IX[t] = 0;
					}
				}

				iMMBGradient->Visible  = true;
				sSelectionMMB->Visible = false;

                sbGradient->Tag = 2;
			}
			break;
		}
	}

	thematrix->Render.Gradient.Option = go;
}



void TfrmMain::ClearCurrentProjectFileName()
{
	GSystemSettings->App.DataFilename = L"";
	Caption = BackupCaption.c_str();
}


#pragma region PopupMenu_AnimationSpeed
void __fastcall TfrmMain::miPlaybackSpeed3Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	std::wstring hint = GLanguageHandler->Text[kPlayAnimation] + L" ";

	switch (mi->Tag)
	{
	case 0:
		timerAnimate->Interval = 2000;
		hint += L"(2 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 1:
		timerAnimate->Interval = 1500;
		hint += L"(1.5 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 2:
		timerAnimate->Interval = 1000;
		hint += L"(1 " + GLanguageHandler->Text[kSecond] + L")";
		break;
	case 3:
		timerAnimate->Interval = 500;
		hint += L"(0.5 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 4:
		timerAnimate->Interval = 250;
		hint += L"(0.25 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 5:
		timerAnimate->Interval = 200;
		hint += L"(0.20 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 6:
		timerAnimate->Interval = 100;
		hint += L"(0.1 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 7:
		timerAnimate->Interval = 50;
		hint += L"(0.05 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 8:
		timerAnimate->Interval = 25;
		hint += L"(0.025 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 9:
		timerAnimate->Interval = 20;
		hint += L"(0.020 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 10:
		timerAnimate->Interval = 10;
		hint += L"(0.01 " + GLanguageHandler->Text[kSeconds] + L")";
		break;
	case 20:
		timerAnimate->Interval = GSystemSettings->App.CustomSpeed;
		hint += L"(" + std::to_wstring(GSystemSettings->App.CustomSpeed) + L" ms";
		break;
	}

	bPlayAnimation->Hint = hint.c_str();
}


void __fastcall TfrmMain::Setcustomspeed1Click(TObject *Sender)
{
	int speed = OpenCustomPlaybackSpeed(GSystemSettings->App.CustomSpeed);

	if (speed != 0)
	{
		GSystemSettings->App.CustomSpeed = speed;

		std::wstring newcaption = GLanguageHandler->Text[kCustom] + L" (" + std::to_wstring(speed) + L" ms)";

		miPlaybackSpeedCustom->Caption = newcaption.c_str();
	}
}

#pragma end_region


#pragma region PopupMenu_BrushSizee
void __fastcall TfrmMain::miBrushSizeSmallClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	puBrushSize->Tag = mi->Tag;

	thematrix->SetPixelBrush(ConstantsHelper::BrushFromInt(puBrushSize->Tag));

	UpdateDrawModeCaption(lSelectedTool->Tag);
}
#pragma end_region


#pragma region Popup_Fonts
void __fastcall TfrmMain::miFontWrapClick(TObject *Sender)
{
	thematrix->SetFontWrap(miFontWrap->Checked);
}
#pragma end_region


#pragma region Popup_Gradient
void __fastcall TfrmMain::miGradientColour0Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	for (int column = 0; column < thematrix->Details.Width; column++)
	{
		if (thematrix->MatrixLayers[0]->Cells[GetSelectedFrame()]->Grid[puGradient->Tag * thematrix->Details.Width + column] != 0)
		{
			thematrix->MatrixLayers[0]->Cells[GetSelectedFrame()]->Grid[puGradient->Tag * thematrix->Details.Width + column] = mi->Tag;
		}
	}

	MatrixGradient[puGradient->Tag]->Brush->Color = TColor(thematrix->LEDColours[mi->Tag]);
	thematrix->Render.Gradient.IY[puGradient->Tag] = mi->Tag;
}


void __fastcall TfrmMain::SelectGradient(TObject *Sender)
{
	if (MessageDlg(GLanguageHandler->Text[kReallyLoadThisGradient].c_str(), mtWarning, mbYesNo, 0) == mrYes)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		std::wstring name = GSystemSettings->Gradients[mi->Tag];

		std::wstring path = GSystemSettings->App.LMSFilePath + L"gradients\\"  + name + L".ledsgradient";

		if (FileExists(path.c_str()))
		{
			thematrix->Render.Gradient.Load(path);

			UpdateGradientColours();
		}
		else
		{
			std::wstring message = path + L"\n\n \"" + name + L"\"";

			MessageDlg(message.c_str(), mtError, TMsgDlgButtons() << mbOK, 0);
		}
	}
}


void TfrmMain::UpdateGradientColours()
{
	switch (thematrix->Render.Gradient.Option)
	{
	case GradientOption::kOff:
		break;
	case GradientOption::kVertical:
		for (int t = 0; t < thematrix->Details.Height; t++)
		{
			if (thematrix->Details.Mode == MatrixMode::kBiSequential ||
				thematrix->Details.Mode == MatrixMode::kBiBitplanes)
			{
				MatrixGradient[t]->Brush->Color = TColor(thematrix->LEDColours[thematrix->Render.Gradient.IY[t]]);
			}
			else
			{
				MatrixGradient[t]->Brush->Color = TColor(thematrix->Render.Gradient.IY[t]);
			}
		}
		break;
	case GradientOption::kHorizontal:
		for (int t = 0; t < thematrix->Details.Width; t++)
		{
			if (thematrix->Details.Mode == MatrixMode::kBiSequential ||
				thematrix->Details.Mode == MatrixMode::kBiBitplanes)
			{
				MatrixGradient[t]->Brush->Color = TColor(thematrix->LEDColours[thematrix->Render.Gradient.IX[t]]);
			}
			else
			{
				MatrixGradient[t]->Brush->Color = TColor(thematrix->Render.Gradient.IX[t]);
			}
		}
		break;
	}
}


void __fastcall TfrmMain::miSaveGradientClick(TObject *Sender)
{
	std::wstring s = InputBox(GLanguageHandler->Text[kSaveGradient].c_str(),
							  GLanguageHandler->Text[kName].c_str(),
							  GLanguageHandler->Text[kMyGradient].c_str()).c_str();

	if (!s.empty())
	{
		std::wstring file_name = GSystemSettings->App.LMSFilePath + L"gradients\\" + s + L".ledsgradient";

		thematrix->Render.Gradient.Save(file_name);
	}
}
#pragma end_region


#pragma region PopupMenu_GradientRGB
void __fastcall TfrmMain::puGradientRGBPopup(TObject *Sender)
{
	TShape *shape = (TShape*)Sender;

	miGradSetRow->Tag = shape->Tag;

	if (sbGradient->Tag == 1)
	{
		miGradSetRow->Caption = GLanguageHandler->Text[kSetRowToSelectedColour].c_str();
		miGradFrom->Caption   = GLanguageHandler->Text[kGradientFromTopBottom].c_str();
	}
	else
	{
		miGradSetRow->Caption = GLanguageHandler->Text[kSetColumnToSelectedColour].c_str();
		miGradFrom->Caption   = GLanguageHandler->Text[kGradientFromLeftRight].c_str();
	}
}
#pragma end_region


#pragma region PopupMenu_PixelShape
void __fastcall TfrmMain::miPixelShapeSquareClick(TObject *Sender)
{
	Screen->Cursor = crHourGlass;

	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	sbPixelShape->Caption          = mi->Caption;
	sbPixelShape->Tag              = mi->Tag;
	GSystemSettings->Project.Shape = ConstantsHelper::PixelShapeFromInt(mi->Tag);

	thematrix->ChangePixelShape(GSystemSettings->Project.Shape);

	Screen->Cursor = crDefault;
}
#pragma end_region


#pragma region PopupMenu_PixelSize
void __fastcall TfrmMain::miPixelTinyClick(TObject *Sender)
{
	Screen->Cursor = crHourGlass;

	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	switch (mi->Tag)
	{
	case  0:
		GSystemSettings->Project.PixelSize = CPixelSize10;
        break;
	case  1:
		GSystemSettings->Project.PixelSize = CPixelSize15;
		break;
	case  2:
		GSystemSettings->Project.PixelSize = CPixelSize20;
		break;
	case  3:
		GSystemSettings->Project.PixelSize = CPixelSize25;
		break;
	case  4:
		GSystemSettings->Project.PixelSize = CPixelSize30;
		break;
	case  5:
		GSystemSettings->Project.PixelSize = CPixelSize40;
		break;
	case  6:
		GSystemSettings->Project.PixelSize = CPixelSize50;
		break;
	case 99:
		GSystemSettings->Project.PixelSize = thematrix->GetAutoPixelSize(pCanvas->Width, pCanvas->Height, sbGradient->Tag);
		break;
	}

	sbPixelSize->Caption = mi->Caption;
	sbPixelSize->Tag     = mi->Tag;

	thematrix->ChangePixelSize(GSystemSettings->Project.PixelSize);

	Screen->Cursor = crDefault;
}
#pragma end_region


#pragma region Popup_Random
void __fastcall TfrmMain::miRandomnessTinyClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	mi->Checked = true;

	thematrix->SetRandomCoeff(mi->Tag);

	UpdateDrawModeCaption(lSelectedTool->Tag);
}
#pragma end_region


void __fastcall TfrmMain::OnGradientClick(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	switch (thematrix->Details.Mode)
	{
	case MatrixMode::kRGB:
		puGradientRGB->Tag = shape->Tag;

		puGradientRGB->Popup(Left + shape->Left + 10, Top + pCanvas->Top + shape->Top + 20);
		break;
	case MatrixMode::kRGB3BPP:
		puGradientRGB_3BPP->Tag = shape->Tag;

		puGradientRGB_3BPP->Popup(Left + shape->Left + 10, Top + pCanvas->Top + shape->Top + 20);
		break;

	default:
		puGradient->Tag = shape->Tag;

		puGradient->Popup(Left + shape->Left + 10, Top + pCanvas->Top + shape->Top + 20);
	}
}


void TfrmMain::SetPreview(int size, ViewShape view, int voidsize, int offset, bool direction, bool popout)
{
	switch (size)
	{
	case 1:
		miPreviewx1Click(miPreviewx1);
		break;
	case 2:
		miPreviewx1Click(miPreviewx2);
		break;
	case 3:
		miPreviewx1Click(miPreviewx3);
		break;
	case 4:
		miPreviewx1Click(miPreviewx4);
		break;
	case 5:
		miPreviewx1Click(miPreviewx5);
		break;
	case 6:
		miPreviewx1Click(miPreviewx6);
		break;
	case 8:
		miPreviewx1Click(miPreviewx8);
		break;
	case 10:
		miPreviewx1Click(miPreviewx10);
		break;
	case 12:
		miPreviewx1Click(miPreviewx12);
		break;
	case 15:
		miPreviewx1Click(miPreviewx15);
		break;
	case 20:
		miPreviewx1Click(miPreviewx20);
		break;
	case 25:
		miPreviewx1Click(miPreviewx25);
		break;
	case 30:
		miPreviewx1Click(miPreviewx30);
		break;
	case 40:
		miPreviewx1Click(miPreviewx40);
		break;
	case 50:
		miPreviewx1Click(miPreviewx50);
		break;

	default:
		miPreviewx1Click(miPreviewx1);
	}

	switch (view)
	{
	case ViewShape::kSquare:
		miPreviewViewSquareClick(miPreviewViewSquare);
		break;
	case ViewShape::kRadial:
		miPreviewViewSquareClick(miPreviewViewRadial);
		break;
	case ViewShape::kRadial3Q:
		miPreviewViewSquareClick(miPreviewViewRadialTQ);
		break;
	case ViewShape::kSemiCircle:
		miPreviewViewSquareClick(miPreviewViewSemiCircle);
		break;
	case ViewShape::kSemiCircleInverted:
		miPreviewViewSquareClick(miPreviewViewSemiCircleInverted);
		break;

	default:
		miPreviewViewSquareClick(miPreviewViewSquare);
	}

	switch (voidsize)
	{
	case 10:
		miPreviewVoid10Click(miPreviewVoid10);
		break;
	case 15:
		miPreviewVoid10Click(miPreviewVoid15);
		break;
	case 20:
		miPreviewVoid10Click(miPreviewVoid20);
		break;
	case 25:
		miPreviewVoid10Click(miPreviewVoid25);
		break;
	case 30:
		miPreviewVoid10Click(miPreviewVoid30);
		break;
	case 40:
		miPreviewVoid10Click(miPreviewVoid40);
		break;
	case 50:
		miPreviewVoid10Click(miPreviewVoid50);
		break;

	default:
		miPreviewVoid10Click(miPreviewVoid10);
	}

	switch (offset)
	{
	case CZeroDegrees:
		miRadialOffset45Click(miRadialOffset0);
		break;
	case 1:
	case C45Degrees:
		miRadialOffset45Click(miRadialOffset45);
		break;
	case 2:
	case C90Degrees:
		miRadialOffset45Click(miRadialOffset90);
		break;
	case 3:
	case C135Degrees:
		miRadialOffset45Click(miRadialOffset135);
		break;
	case 4:
	case C180Degrees:
		miRadialOffset45Click(miRadialOffset180);
		break;
	case 5:
	case C225Degrees:
		miRadialOffset45Click(miRadialOffset225);
		break;
	case 6:
	case C270Degrees:
		miRadialOffset45Click(miRadialOffset270);
		break;
	case 7:
	case C315Degrees:
		miRadialOffset45Click(miRadialOffset315);
		break;

	default:
		miRadialOffset45Click(miRadialOffset0);
	}

	if (direction)
	{
		miPreviewOffsetReverse->Checked = true;
		miPreviewOffsetReverseClick(miPreviewOffsetReverse);
	}
}


void TfrmMain::SyncPreviewSize(int size)
{
	_PreviewMenuSize[0][size]->Checked = true;
	_PreviewMenuSize[1][size]->Checked = true;
}


void TfrmMain::SyncPreviewView(int view)
{
	_PreviewMenuView[0][view]->Checked = true;
	_PreviewMenuView[1][view]->Checked = true;
}


void TfrmMain::SyncPreviewVoid(int v)
{
	_PreviewMenuVoid[0][v]->Checked = true;
	_PreviewMenuVoid[1][v]->Checked = true;
}


// settings already loaded, put them where they are needed
void TfrmMain::SetFromSettings()
{
	for (int t = 0; t < 6; t++)
	{
		thematrix->LEDColoursSingle[t] = GSystemSettings->LEDColoursSingle[t];
	}

	for (int t = 0; t < 6; t++)
	{
		thematrix->LEDColoursBi[t] = GSystemSettings->LEDColoursBi[t];
	}

	thematrix->RGBBackground               = GSystemSettings->RGBBackground;
	thematrix->LEDRGBColours[CMouseLeft]   = GSystemSettings->LEDRGBColours[1];
	thematrix->LEDRGBColours[CMouseMiddle] = GSystemSettings->LEDRGBColours[2];
	thematrix->LEDRGBColours[CMouseRight]  = GSystemSettings->LEDRGBColours[3];

	// ===========================================================================

	sSelectionLMB->Tag = GSystemSettings->SelectionColours[0];
	sSelectionMMB->Tag = GSystemSettings->SelectionColours[1];
	sSelectionRMB->Tag = GSystemSettings->SelectionColours[2];

	// ===========================================================================

	SetupMatrixColours();

	// ===========================================================================

	SystemSetBackgroundColour(GSystemSettings->App.BackgroundColour);

	// ===========================================================================

	odMain->InitialDir = GSystemSettings->App.LastLoadLocation.c_str();
	sdMain->InitialDir = GSystemSettings->App.LastSaveLocation.c_str();

	miPlaybackSpeedCustom->Caption = Utility::WS2US(GLanguageHandler->Text[kCustom] + L" (" + std::to_wstring(GSystemSettings->App.CustomSpeed) + L" ms)");

	if (GSystemSettings->App.CustomSpeed <= 0)
	{
		GSystemSettings->App.CustomSpeed = 1000;
	}

	// ===========================================================================

	switch (GSystemSettings->Project.PixelSize)
	{
	case CPixelSizeAuto:
		miPixelTinyClick(miPixelAuto);
		break;
	case CPixelSize10:
		miPixelTinyClick(miPixelTiny);
		break;
	case CPixelSize15:
		miPixelTinyClick(miPixelSmall);
		break;
	case CPixelSize20:
		miPixelTinyClick(miPixelMedium);
		break;
	case CPixelSize25:
		miPixelTinyClick(miPixelLarge);
		break;
	case CPixelSize30:
		miPixelTinyClick(miPixelVeryLarge);
		break;
	case CPixelSize40:
		miPixelTinyClick(miPixelUltra);
		break;
	case CPixelSize50:
		miPixelTinyClick(miPixelMegaUltra);
		break;

	default:
		miPixelTinyClick(miPixelAuto);
	}

	// ===========================================================================

	switch (GSystemSettings->Project.Shape)
	{
	case PixelShape::kSquare:
		miPixelShapeSquareClick(miPixelShapeSquare);
		break;
	case PixelShape::kCircle:
		miPixelShapeSquareClick(miPixelShapeRound);
		break;
	case PixelShape::kRoundRect:
		miPixelShapeSquareClick(miPixelShapeRoundRect);
		break;

	default:
		miPixelShapeSquareClick(miPixelShapeSquare);
	}

	// ===========================================================================

	miShowAnimationToolbar->Checked = GSystemSettings->Bars.Animation;

	switch (GSystemSettings->App.AnimSpeed)
	{
	case   10:
		miPlaybackSpeed3Click(miPlaybackSpeed11);
		break;
	case   20:
		miPlaybackSpeed3Click(miPlaybackSpeed10);
		break;
	case   25:
		miPlaybackSpeed3Click(miPlaybackSpeed9);
		break;
	case   50:
		miPlaybackSpeed3Click(miPlaybackSpeed8);
		break;
	case  100:
		miPlaybackSpeed3Click(miPlaybackSpeed7);
		break;
	case  200:
		miPlaybackSpeed3Click(miPlaybackSpeed6);
		break;
	case  250:
		miPlaybackSpeed3Click(miPlaybackSpeed5);
		break;
	case  500:
		miPlaybackSpeed3Click(miPlaybackSpeed4);
		break;
	case 1000:
		miPlaybackSpeed3Click(miPlaybackSpeed3);
        break;
	case 1500:
		miPlaybackSpeed3Click(miPlaybackSpeed2);
		break;
	case 2000:
		miPlaybackSpeed3Click(miPlaybackSpeed1);
		break;

	default:
		if (GSystemSettings->App.AnimSpeed > 0)
		{
			SetPlaybackCustom(GSystemSettings->App.AnimSpeed);
		}
		else
		{
			miPlaybackSpeed3Click(miPlaybackSpeed5);
		}
	}

	miShowAnimationToolbarClick(nullptr);

	// ===========================================================================

	miPaletteGradientToolbar->Checked = GSystemSettings->Bars.RGBPalette;
	pRGBPalette->Visible              = miPaletteGradientToolbar->Checked;

	// ===========================================================================

	if (GSystemSettings->App.AutoSaveEnabled)
	{
		miAutoSave->Checked    = true;
		timerAutosave->Enabled = true;
	}

	switch (GSystemSettings->App.AutoSave)
	{
	case AutoSaveInterval::kTwoMinutes:
		miAutosave2Click(miAutosave2);
		break;
	case AutoSaveInterval::kFiveMinutes:
		miAutosave2Click(miAutosave5);
		break;
	case AutoSaveInterval::kTenMinutes:
		miAutosave2Click(miAutosave10);
		break;

	default:
		miAutosave2Click(miAutosave2);
	}

	// ===========================================================================

	miPreview->Checked = GSystemSettings->Preview.Active;

	thematrix->SetPreviewActive(miPreview->Checked);

	SetPreview(GSystemSettings->Preview.Size,
			   GSystemSettings->Preview.View,
			   GSystemSettings->Preview.Void,
			   GSystemSettings->Preview.Offset,
			   GSystemSettings->Preview.Direction,
			   GSystemSettings->Preview.Popout);

	// =======================================================================

	for (int t = 0; t < 16; t++)
	{
		_RGBPalette[t]->Brush->Color = TColor(GSystemSettings->RGBPalette[t]);
	}
}


void TfrmMain::SystemSetBackgroundColour(int new_colour)
{
	frmMain->Color = TColor(new_colour);
	pCanvas->Color = TColor(new_colour);

	if (frmPreviewPopout != nullptr)
	{
		frmPreviewPopout->Panel1->Color = TColor(new_colour);
		frmPreviewPopout->Color = TColor(new_colour);
	}

	thematrix->SetBackgroundColour(new_colour);

	GSystemSettings->App.BackgroundColour = new_colour;
}


void TfrmMain::SetPlaybackCustom(int value)
{
	miPlaybackSpeedCustom->Checked = true;
	timerAnimate->Interval         = value;
	bPlayAnimation->Hint           = GLanguageHandler->Text[kPlayAnimation] + L" (" + GLanguageHandler->Text[kCustom] + L" " + std::to_wstring(value) + L" ms";
	miPlaybackSpeedCustom->Caption = Utility::WS2US(GLanguageHandler->Text[kCustom] + L" (" + std::to_wstring(value) + L" ms)");
}


void TfrmMain::DisplayFrame(int frame)
{
	SetFrameCaption(frame);

	thematrix->SetAndShowCurrentFrame(frame);
}


void TfrmMain::GenerateShades(int colour)
{
	int xR = (colour & 0x0000ff);         // Windows colour structure = BGR
	int xB = (colour & 0xff0000) >> 16;
	int xG = (colour & 0x00ff00) >> 8;

	double xMaxRPos = std::round((double)std::max(xR, std::max(xG, xB)) / 255);

	double dR = std::round((double)(xR * xMaxRPos) / 16);
	double dG = std::round((double)(xG * xMaxRPos) / 16);
	double dB = std::round((double)(xB * xMaxRPos) / 16);

	for (int t = 0; t < 16; t++)
	{
		xR = (t * dR);
		xG = (t * dG);
		xB = (t * dB);

		if (t * dR > 255) xR = 255;
		if (t * dG > 255) xG = 255;
		if (t * dB > 255) xB = 255;

		// windows format is BGR
		_RGBShade[t]->Brush->Color = TColor(xR + (xB << 16) + (xG << 8));
	}
}


#pragma region Timers
void __fastcall TfrmMain::timerAnimateTimer(TObject *Sender)
{
	SetFrameCaption(timerAnimate->Tag);

	thematrix->SetAndShowCurrentFrame(timerAnimate->Tag);

	if (timerAnimate->Tag == tbFrames->Max - 1)
	{
		timerAnimate->Tag = 0;
	}
	else
	{
		timerAnimate->Tag++;
	}
}


void __fastcall TfrmMain::timerAutosaveTimer(TObject *Sender)
{
	if ((sbClear->Enabled || !thematrix->AnimPlaying) && thematrix->Details.Available)
	{
		ImportData ted;

		BuildImportData(ted, 0, thematrix->GetFrameCount() - 1);

		// ===================================================================

		std::wstring FileName = Utility::GetAutoSaveName();

		std::wstring FullPath = GSystemSettings->App.LMSFilePath + L"saves\\autosave\\" + FileName;

		ProjectColours colours = GetColours();

		thematrix->SaveAnimation(FullPath, ted, GSystemSettings->App.LastExport, colours);

		statusMain->SimpleText = Utility::WS2US(GLanguageHandler->Text[kAutosavedCurrentMatrix] + L" (" + FileName + L")");
	}
}
#pragma end_region


#pragma region Popup_Presests
void __fastcall TfrmMain::miPresetSaveCurrentClick(TObject *Sender)
{
	MatrixPreset mpp;

	std::wstring s = InputBox(GLanguageHandler->Text[kPresetFileName].c_str(),
							  GLanguageHandler->Text[kName].c_str(),
							  Utility::WS2US(std::to_wstring(thematrix->Details.Width) + L" x " + std::to_wstring(thematrix->Details.Height))).c_str();

	if (s != L"")
	{
		mpp.Width      = thematrix->Details.Width;
		mpp.Height     = thematrix->Details.Height;
		mpp.PixelSize  = sbPixelSize->Tag;
		mpp.Mode       = thematrix->Details.Mode;
		mpp.PixelShape = sbPixelShape->Tag;

		GPresetHandler->Save(GSystemSettings->App.LMSFilePath + L"presets\\" + s + L".ledspreset", mpp);
	}
}
#pragma end_region


#pragma region Popup_GradientRGB_3BPP
void __fastcall TfrmMain::miGradientRGB3BPP1Click(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	for (int column = 0; column < thematrix->Details.Width; column++)
	{
		if (thematrix->MatrixLayers[0]->Cells[GetSelectedFrame()]->Grid[puGradient->Tag * thematrix->Details.Width + column] != 0)
		{
			thematrix->MatrixLayers[0]->Cells[GetSelectedFrame()]->Grid[puGradient->Tag * thematrix->Details.Width + column] = mi->Tag;
		}
	}

	MatrixGradient[puGradientRGB_3BPP->Tag]->Brush->Color = TColor(thematrix->LEDRGB3BPPColours[mi->Tag]);

	if (sbGradient->Tag == 1)
	{
		thematrix->Render.Gradient.IY[puGradientRGB_3BPP->Tag] = mi->Tag;
	}
	else
	{
		thematrix->Render.Gradient.IX[puGradientRGB_3BPP->Tag] = mi->Tag;
	}
}
#pragma end_region


#pragma region Popup_GradientRGB
void __fastcall TfrmMain::miGradientSelectRGBClick(TObject *Sender)
{
	colorDialog->Color = MatrixGradient[puGradientRGB->Tag]->Brush->Color;

	if (colorDialog->Execute())
	{
		MatrixGradient[puGradientRGB->Tag]->Brush->Color  = colorDialog->Color;

		if (sbGradient->Tag == 1)
		{
			thematrix->Render.Gradient.IY[puGradientRGB->Tag] = colorDialog->Color;
		}
		else
		{
			thematrix->Render.Gradient.IX[puGradientRGB->Tag] = colorDialog->Color;
		}
	}
}


void __fastcall TfrmMain::miGradSetRowClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	if (sbGradient->Tag == 1)
	{
		for (int x = 0; x < thematrix->Details.Width; x++)
		{
			thematrix->PlotPixelMatrix(x, mi->Tag, thematrix->Render.Gradient.IY[mi->Tag]);
		}
	}
	else
	{
		for (int y = 0; y < thematrix->Details.Height; y++)
		{
			thematrix->PlotPixelMatrix(mi->Tag, y, thematrix->Render.Gradient.IX[mi->Tag]);
		}
	}

	thematrix->Refresh();
}


void __fastcall TfrmMain::miGradFromClick(TObject *Sender)
{
	int colstart = 0;
	int colend = 0;
	int end = 0;

	if (sbGradient->Tag == 1)
	{
		colstart   = thematrix->Render.Gradient.IY[0];
		colend     = thematrix->Render.Gradient.IY[thematrix->Details.Height - 1];

		end        = thematrix->Details.Height - 1;
	}
	else
	{
		colstart   = thematrix->Render.Gradient.IX[0];
		colend     = thematrix->Render.Gradient.IX[thematrix->Details.Width - 1];

		end       = thematrix->Details.Width - 1;
	}

	int rdy  = (colend & 0x0000FF) - (colstart & 0x0000FF);
	int gdy  = ((colend & 0x00FF00) >> 8) - ((colstart & 0x00FF00) >> 8);
	int bdy  = ((colend & 0xFF0000) >> 16) - ((colstart & 0xFF0000) >> 16);

	double newr = (colstart & 0x0000FF);
	double newg = (colstart & 0x00FF00) >> 8;
	double newb = (colstart & 0xFF0000) >> 16;

	double rdx  = rdy / end;
	double gdx  = gdy / end;
	double bdx  = bdy / end;

	for (int y = 1; y < end; y++)
	{
		newr  = newr + rdx;
		newg  = newg + gdx;
		newb  = newb + bdx;

		int newri = std::floor(newr);
		int newgi = std::floor(newg);
		int newbi = std::floor(newb);

		MatrixGradient[y]->Brush->Color  = TColor((newbi << 16) + (newgi << 8) + newri);

		if (sbGradient->Tag == 1)
		{
			thematrix->Render.Gradient.IY[y] = (newbi << 16) + (newgi << 8) + newri;
		}
		else
		{
			thematrix->Render.Gradient.IX[y] = (newbi << 16) + (newgi << 8) + newri;
		}
	}
}


void __fastcall TfrmMain::miGradientBottomTopClick(TObject *Sender)
{
	std::vector<int> colours;

	int end = 0;
	int tempcolour = 0;

	if (sbGradient->Tag == 1)
	{
		end = thematrix->Details.Height;
	}
	else
	{
		end = thematrix->Details.Width;
	}

	for (int y = 0; y < end; y++)
	{
		colours.push_back(MatrixGradient[y]->Brush->Color);
	}

	int index = colours.size() - 1;

	for (int y = 0; y < end; y++)
	{
		tempcolour = colours[index];

		MatrixGradient[y]->Brush->Color  = TColor(tempcolour);

		if (sbGradient->Tag == 1)
		{
			thematrix->Render.Gradient.IY[y] = tempcolour;
		}
		else
		{
			thematrix->Render.Gradient.IX[y] = tempcolour;
		}

		index--;
	}
}
#pragma end_region


void __fastcall TfrmMain::pCanvasMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
	if (Shift.Contains(ssRight))
	{
		puMainCanvas->Popup(Left + X - 10, Top + pCanvas->Top + 48 + Y);
	}
}


void __fastcall TfrmMain::pCanvasMouseMove(TObject *Sender, TShiftState Shift, int X,
		  int Y)
{
	FormMouseMove(nullptr, {}, 0, 0);
}


void __fastcall TfrmMain::LanguageClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	GSystemSettings->App.Language = mi->Caption;
}


void __fastcall TfrmMain::OnPopoutClosed(TObject *Sender, TCloseAction &Action)
{
	thematrix->SetPreviewPopout(false);

	FormResize(nullptr);
}


void __fastcall TfrmMain::SelectFont(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	std::wstring name = GFontHandler->Fonts[mi->Tag] + L".ledsfont";

	std::wstring path = GSystemSettings->App.LMSFilePath + L"fonts\\" + name;

	if (FileExists(path.c_str()))
	{
		thematrix->LoadTextToolFont(path, GFontHandler->Fonts[mi->Tag]);

		UpdateDrawModeCaption(lSelectedTool->Tag);

		mi->Checked = true;
	}
	else
	{
		MessageDlg(Utility::WS2US(GLanguageHandler->Text[kCannotFindFont] + L"\n\n\"" + name + L"\""), mtError, TMsgDlgButtons() << mbOK, 0);
	}
}


void __fastcall TfrmMain::SelectPreset(TObject *Sender)
{
	if (MessageDlg(GLanguageHandler->Text[kReallyLoadThisPreset].c_str(), mtWarning, mbYesNo, 0) == mrYes)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		std::wstring name = GPresetHandler->Presets[mi->Tag] + L".ledspreset";

		std::wstring path = GSystemSettings->App.LMSFilePath + L"presets\\" + name;

		if (FileExists(path.c_str()))
		{
			LoadPreset(path);
		}
		else
		{
			MessageDlg(Utility::WS2US(GLanguageHandler->Text[kCannotFindPresetFile] + L"\n\n" + name + L"\""), mtError, TMsgDlgButtons() << mbOK, 0);
		}
	}
}


void TfrmMain::LoadPreset(const std::wstring file_name)
{
	MatrixMode mode = MatrixMode::kMono; // default matrix type if none specified in file

	// ===========================================================================

	MatrixPreset mpp = GPresetHandler->Load(file_name);

	GSystemSettings->Project.Width  = mpp.Width;
	GSystemSettings->Project.Height = mpp.Height;

	switch (mpp.PixelSize)
	{
	case 0:
		miPixelTinyClick(miPixelTiny);
		break;
	case 1:
		miPixelTinyClick(miPixelSmall);
		break;
	case 2:
		miPixelTinyClick(miPixelMedium);
		break;
	case 3:
		miPixelTinyClick(miPixelLarge);
		break;
	case 4:
		miPixelTinyClick(miPixelVeryLarge);
		break;
	}

	mode = mpp.Mode;

	// =======================================================================

	GSystemSettings->Project.Mode = mode;
	ChangeMatrixType();

	// =======================================================================

	sbBuildClick(Load1);
}


void TfrmMain::LoadWithWarnings(const std::wstring file_name)
{
	if (timerAnimate->Enabled)
	{
		bPlayAnimationClick(bStopAnimation);
	}

	// =======================================================================

	if (sbClear->Enabled)
	{
		if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kOpeningNewProjectWillClearCurrentProject] +
					   L"\n\n" +
					   GLanguageHandler->Text[kDoYouWishToContinue]), mtWarning, mbYesNo, 0) != mrYes) return;
	}

	// =======================================================================

	LoadFromFileName(file_name);

	FormResize(nullptr);

	Application->ProcessMessages();
	thematrix->SetMatrixReadOnly(false);
}
