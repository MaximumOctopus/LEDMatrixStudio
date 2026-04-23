// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#pragma once

#include <System.Classes.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "ActionObject.h"
#include "Colours.h"
#include "DrawingData.h"
#include "ExportOptions.h"
#include "FileConstants.h"
#include "Font.h"
#include "FreeformHandler.h"
#include "Gradient.h"
#include "ImportData.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Layer.h"
#include "LayerHandler.h"
#include "MatrixDetails.h"
#include "MatrixGrid.h"
#include "MatrixConstants.h"
#include "MatrixIgnored.h"
#include "PreviewSettings.h"

#define _FrameTimer 0

extern LanguageHandler *GLanguageHandler;


typedef void __fastcall (__closure *MouseOverEvent)(int, int);
typedef void __fastcall (__closure *DebugEvent)(std::wstring);


class TheMatrix
{
private:

	bool Busy = false;

	MatrixGrid *DisplayBuffer = nullptr;

	TPaintBox *PaintBox = nullptr;
	TPaintBox *PreviewBox = nullptr;

	Font *TextFont = nullptr;

	TComponent *Owner = nullptr;
	TWinControl *Canvas = nullptr;

	int CanvasBackground;

	int LastMouseButton = 0;

	bool PreviewPopout = false;

	bool AutomateMode = false;

	int CurrentFrame = 0;
	int CurrentLayer = 0;
	int CurrentPixel = -1;
	int LightBox = 0;
	int RandomCoeff = 30;
	bool IgnoredPixelsMode = false;
    bool HideIgnoredPixels = true;
	bool MatrixReadOnly = false;
	SoftwareMode Software = SoftwareMode::kAnimation;

	int RadialOffsetDegrees = 0; // combination of the two below
	int RadialOffset = 0;
	bool RadialOffsetDirection = false;

	PreviewOptions Preview;

	MirrorMode Mirror;

	std::vector<int> Gradient;

	TScrollBar *ScrollHorizontal;
	TScrollBar *ScrollVertical;

	bool FontWrap = false;

	void InitPreviewBox(TComponent*, TWinControl*, bool);

	ImportData LoadProjectFreeform(const std::wstring, ExportOptions&, LoadMode, int);

	int GetPixelFrom(MatrixColourMode MatrixFormat, MatrixColourMode ImportFormat, int Pixel, int Background);

	void CopyCurrentFrameToDrawBuffer();
	void CopyDrawBufferToCurrentFrame();

	void __fastcall OnPreviewBoxMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	LoadData LoadDataParameterType(const std::wstring, bool, bool, bool, bool, bool);

	void __fastcall ClickPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	void __fastcall Shape1MouseUpBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall ClickPixelBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseMoveBiColour(TObject *Sender, TShiftState Shift, int X, int Y);

	void __fastcall ClickPixelRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseMoveRGB(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseUpRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	void __fastcall ClickPixelRGBFF(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseMoveRGBFF(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseUpRGBFF(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	void __fastcall ClickPixelIgnoredPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseMoveIgnoredPixel(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Shape1MouseUpIgnoredPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	int GetPreviewPixelSize(int);

	void __fastcall pbPreviewPaint(TObject *Sender);
	void __fastcall pbPreviewPaintRadial(TObject *Sender);
	void __fastcall pbPreviewPaintRadialThreeQuarters(TObject *Sender);
	void __fastcall pbPreviewPaintSemiCircle(TObject *Sender);
	void __fastcall pbPreviewPaintSemiCircleInverted(TObject *Sender);

	void ConfigurePaintboxDrawing();

	void BuildMonoBiRenderFrame();
	void BuildRGBRenderFrame();
	void BuildRGB3BPPRenderFrame();

	void __fastcall PaintBoxUpdate(TObject *Sender);
	void __fastcall PaintBoxUpdateRGB(TObject *Sender);
	void __fastcall PaintBoxUpdateRGBFF(TObject *Sender);
	void __fastcall PaintBoxUpdateRGB_3BPP(TObject *Sender);
	void __fastcall PaintBoxUpdateIgnoredPixel(TObject *Sender);

	void ColourPixel(int);
	void ColourPixelMulti(int);

	void DrawWithBrush(int Index, int x, int y);
	void DrawWithBrushMulti(int Index, int x, int y);
	void DrawWithGradientBrush(int x, int y);
	void DrawWithBrushPaste(int x1, int y1, bool Transparent);

	void CopyShape();
	void UpdateDrawTool(int SetX, int SetY, int SetColour, bool IsGradient);
	void PlotInBounds(int X, int Y, int Colour);

	void SimpleLine(int, int, int, int, int, bool);
	void DrawShape(bool RealTime, int Colour, bool IsGradient);

	void ChangeSelectionColour(int SelectionLMB, int SelectionMMB, int SelectionRMB);

	void __fastcall ScrollBarHorizontalChange(TObject *Sender);
	void __fastcall ScrollBarVerticalChange(TObject *Sender);

public:

	TComponent *PreviewOwner;
	TWinControl *PreviewCanvas;

	// public events

	std::function<void(TheMatrix*)> OnChange;
	std::function<void(TheMatrix*)> OnLayerChange;
	std::function<void(TheMatrix*)> OnSizeChange;
	std::function<void(TheMatrix*)> OnDisplayBufferCopied;
	std::function<void(TheMatrix*)> OnNewFrameDisplayed;
	std::function<void(TheMatrix*)> OnColourChange;
	std::function<void(TheMatrix*)> OnNew3bppColours;
	std::function<void(int, int)> OnMouseOver;              // X and Y refer to the grid x/y when in kGrid draw mode, otherwise x/y pixel positions on the canvas
	std::function<void(int, int, int)> OnMouseOverPixel;          // X and Y refer to the grid x/y when in kGrid draw mode, otherwise x/y pixel positions on the canvas
	std::function<void(int, int)> OnPreviewMouseDown;
	// use this to send debug data from the component to screen or file based on your needs
	std::function<void(TheMatrix*, const std::wstring)> OnDebugEvent;

	//

	MatrixDetails Details;
	MatrixRendering Render;

	int LastX = -1;
	int LastY = -1;

	bool AnimPlaying = false;

	int SelectionLMB = 0;
	int SelectionMMB = 1;
	int SelectionRMB = 2;

	int LEDColoursSingle[6]; 	// used as backups only
	int LEDColoursBi[6];	 	// used as backups only

	int LEDRGBColours[4];		// background, lmb, mmb, rmb
	int LEDColours[6] = {0xffffff, 0, 0, 0, 0};			// currently being displayed
	int LEDRGB3BPPColours[8] = { 0x00000000, // 000
								 0x00FF0000, // 001
								 0x0000FF00, // 010
								 0x00FFFF00, // 011
								 0x000000FF, // 100
								 0x00FF00FF, // 101
								 0x0000FFFF, // 110
								 0x00FFFFFF }; // 111

	int RGBBackground = 0x000000;

	LayerHandler *Data;

	std::vector<MatrixGrid*> MatrixUser;
	std::vector<int> MatrixUserFF[10];

	MatrixGrid *MatrixBackup;
	MatrixGrid *MatrixCopy;
	MatrixIgnored *MatrixIgnoredLayout;
	MatrixGrid *MatrixRender;
	MatrixGrid *MatrixMerge;

	std::vector<int> PixelFrameColours;

	TheMatrix(TComponent*, TWinControl*);
	~TheMatrix();

	int NewOrder = 0;

   	void SetMatrixReadOnly(bool);

	void NewMatrix(MatrixDrawMode, MatrixColourMode,
				   int, int, int, int, int, int,
				   PixelShape,
				   bool, bool, bool,
				   int);

	void SetYPos(int);
	void SetBackgroundColour(int);

	void CancelDrawMode();

	void ChangePixelSize(int);
	void ChangeZoomUI(int);
	void ChangePixelShape(PixelShape);
	void SetPixelBrush(BrushSize);
	void ChangeMatrixMode(MatrixDrawMode, MatrixColourMode);

	void SetAutomateMode(bool);
	void SetSoftwareMode(SoftwareMode);
	void SetRadialOffset(int);
	void SetRadialOffsetDirection(bool);
	void SetShapeParameter(int);
	void SetMirrorMode(MirrorMode);

	void SetPreviewActive(bool);
	void SetPreviewBoxSize(int);
	void SetPreviewVoid(int);
	void SetPreviewViewMode(ViewShape);
	void SetPreviewPopout(bool);
	void SetPreviewIncrementRadially(bool);
	void SetPreviewDrawing(bool);                   // sets the ability to draw on the preview canvas

    std::wstring GetFontName();
	void SetFontWrap(bool);

	void SetRandomCoeff(int);

	void SetMouseButtonColours(int, int, int);

	void PlotPixelMatrix(int, int, int); 			// use only this function (or PlotPixelMatrixFrame) to draw on the matrix outside of this class
	void PlotPixelMatrixFrame(int, int, int, int);  //

	std::wstring RowToString(int, int);
	void StringToRow(bool, std::wstring, int, int, int, bool);

	void BackupMatrix(int, int);
	void BackupMatrix();            				// backs up current frame, current layer

	void SetIgnoredPixels(int);
	void SetIgnoredPixelsFromCustomShape(CustomShape, int);
	void SetIgnoredPixelsFromFileName(const std::wstring);
	void SaveIgnoredPixels(const std::wstring);
	void ToggleIgnoredPixels(bool);

	void AddPixel(int, int);
	void DeletePixel();
	void SelectInGroup(int);
	void PerformFreeformEffect(int);
	void AutoOrderPixels(int);

	void ClearCurrentFrame();
	void ClearCurrentLayer();
	void ClearFrame(int);
	void ClearAllMatrixData(bool, int, int);
    void RemoveAllPixels();
	void WipeAllFramesCurrentLayer();
	void WipeAllFramesAllLayers();

	void ClearAllFramesGradient(int);
	void GradientFillFrame();

	// =========================================================================

	void AddPixelShape(int, int, int, int, int, int, int, int);

	// =========================================================================

	void PerformEffectController(int, int);

	void PerformScrollController(int, int);

	void RotateFrameController(int, int);

	void PerformWipeOnCurrentFrame(int, bool );
	void PerformRevealOnCurrentFrame(int, int, int &);
	void PerformColumnScrollOnCurrentFrame(int , int , bool);
	void PerformRowScrollOnCurrentFrame(int , int , bool);

	// =========================================================================

	void RotateCopyBrush(int);
	void PerformEffectOnBrush(int);

	void CopyCurrentFrame();
	void CopyBackupToCurrentFrame();
	void PasteCurrentFrame();
	void PasteSpecial(int Mode);

	void DrawWithBrushPasteEveryFrame(int, int, bool);

	void InsertBlankFrameAt(int);
	void InsertCopyFrameAt(int);
	void AddFrameMultiple(int, int);

	void DeleteFrame(int);

//	void SetCurrentFrame(int);
	void SetAndShowCurrentFrame(int);
	void SetCurrentLayer(int);
	void SetLightBox(int);
	void ChangeGrid(bool);
    void SetGroupOrderDisplay(bool, bool);
	void SetIgnoredPixelsMode(bool);

	void RefreshCurrentFrame();

	void ChangePixels(int, int);

	void FadeFirstToLast();

    void AddFontCharacter(int, int);
	void DrawFontCharacter(int, int);
	void DeleteFontCharacter(int);
	void LoadTextToolFont(const std::wstring, const std::wstring);

	void ImportRowData(bool, int, int, const std::wstring);
	void ImportColumnData(bool, int, int, const std::wstring);
	ImportData ImportLEDMatrixDataSingleFrame(const std::wstring);
    bool ProcessRGB3bppColours(TCanvas*, std::vector<int> &, int, int);
	void ImportFromFrame(TCanvas*, ImportColourMode, int, int, int, int, std::vector<int> &);
	ImportData ImportFromBMPSingleImage(const std::wstring, int, int, int, ImportColourMode, bool);
	ImportData ImportFromBMPMultipleImage(const std::wstring, int, int, int, int, int, ImportColourMode, bool);

	bool ExportToBitmap(const std::wstring);
	bool ExportAnimationToBitmap(const std::wstring);

	bool SaveAnimation(const std::wstring, ImportData&, ExportOptions&, ProjectColours&);

	ImportData LoadProject(const std::wstring, ExportOptions&, LoadMode, int);
	ImportData LoadProjectGrid(const std::wstring, ExportOptions&, LoadMode, int);

	ImportData ImportFromGIF(const std::wstring);
	void ExportToGIF(const std::wstring, int, int, int, int);
	
	void ClearUserBuffers();
	void CopyToUserBuffer(int);
	void RestoreFromUserBuffer(int);

	void CopyFromPrevious(int);
	void CopyAllLayersFromTo(int, int);
	void CopyLayerFromTo(int, int, int, int);

	void CopyLEDColours();

	void Undo();
	void Redo();
	void SetFromUndo(int Undo);

	bool CanUndo();
	bool CanRedo();

	void Refresh();

	void Automate(ActionObject&);
	void AutomationActionExecute(ActionObject&, int);
	void AutomationPostProcessExecute(ActionObject&, int);

	void ChangeColourCurrent(int, int);
	void ChangeColourCurrentLayer(int, int);
	void ChangeColourAll(int, int);

	void FloodFill(int, int, int);
	void DoFill(int, int, int);

	bool AddLayer(const std::wstring);
	bool AddLayerAsCopy(const std::wstring, int);
	bool DeleteLayer(int);
	void ClearCurrentLayerAllFrames();
	void FlattenAllLayers();
	bool IsVisible(int);

	void SetVisibility(int, bool);
	void MoveUp(int);
	void MoveDown(int);

	void __fastcall OnPreviewBoxCanvasResize(TObject *Sender);

	void BuildMergedFrame(int, MergeFrameMode);

	void ClearGradient();
	void AddGradient(int);
    int GradientBrushCount();

	bool GetIgnoredPixelsMode();
	SoftwareMode GetSoftwareMode();

	int GetAutoPixelSize(int, int, int);

	int GetCurrentLayer();
	int GetCurrentFrame();
	int GetCurrentPixel();

	bool GetPreviewActive();
	int GetPreviewBoxSize();
	bool GetPreviewIncRadially();
	ViewShape GetPreviewView();
	int GetPreviewVoid();
	bool GetPreviewPopout();
	int GetRadialOffset();
	int GetRadialOffsetDirection();
	bool GetAutomateMode();

	int GetRandomCoeff();

	#if _DEBUG
	std::wstring GetPaintBoxDebug();
	std::wstring GetPreviewDebug();
	void TestSignal();
	#endif
};
