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

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.ImageList.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Menus.hpp>

#include "ProjectSettings.h"
#include "TheMatrix.h"

#include "FrameFontPanel.h"
#include "FrameGradientPanel.h"
#include "FrameLayerPanel.h"
#include "FramePalettePanel.h"
#include "FrameQuickData.h"
#include "FrameUndoPanel.h"

//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel20;
	TPanel *pAnimationToolbar;
	TLabel *lFrame;
	TBevel *Bevel5;
	TBevel *Bevel7;
	TBevel *Bevel8;
	TBevel *Bevel9;
	TBevel *Bevel11;
	TBitBtn *bPlayAnimation;
	TBitBtn *bStopAnimation;
	TBitBtn *bPreviousFrame;
	TBitBtn *bNextFrame;
	TBitBtn *bAddFrame;
	TBitBtn *bLightbox;
	TBitBtn *bDeleteFrame;
	TBitBtn *bAddFrameCopy;
	TTrackBar *tbFrames;
	TBitBtn *bStartFrame;
	TBitBtn *bEndFrame;
	TStatusBar *statusMain;
	TPanel *pbFont;
	TPanel *pASCIICode;
	TPanel *pRGBPalette;
	TPageControl *PageControl1;
	TTabSheet *tsPalette;
	TTabSheet *tsGradients;
	TPanel *panelTop;
	TSpeedButton *sbBuild;
	TBevel *Bevel3;
	TSpeedButton *sbPreset;
	TSpeedButton *sbPixelSize;
	TBevel *Bevel17;
	TLabel *lMemoryUsage;
	TSpeedButton *sbPixelShape;
	TBevel *Bevel19;
	TSpeedButton *sbSave;
	TBevel *Bevel10;
	TSpeedButton *sbExport;
	TBevel *Bevel4;
	TBevel *Bevel16;
	TSpeedButton *sbGenerateCode;
	TPanel *panelMiddle;
	TSpeedButton *sbClear;
	TBevel *Bevel1;
	TSpeedButton *sbMirror;
	TSpeedButton *sbFlip;
	TBevel *Bevel2;
	TSpeedButton *sbScrollLeft;
	TSpeedButton *sbScrollRight;
	TSpeedButton *sbScrollUp;
	TSpeedButton *sbScrollDown;
	TSpeedButton *sbInvert;
	TBevel *Bevel6;
	TSpeedButton *sbRotateL;
	TSpeedButton *sbRotateR;
	TSpeedButton *sbRotateAny;
	TBevel *Bevel13;
	TBevel *Bevel14;
	TComboBox *cbRotateAngle;
	TComboBox *cbRotateCount;
	TBitBtn *bLockFrame;
	TPanel *paneTools;
	TSpeedButton *sbMouseMode;
	TSpeedButton *sbFilledRectangle;
	TSpeedButton *sbLine;
	TSpeedButton *sbFrame;
	TSpeedButton *sbCopy;
	TBevel *Bevel15;
	TSpeedButton *sbGradient;
	TSpeedButton *sbRandomDraw;
	TSpeedButton *sbEmptyCircle;
	TSpeedButton *sbMultiDraw;
	TSpeedButton *sbPicker;
	TSpeedButton *sbFont;
	TSpeedButton *sbFilledCircle;
	TSpeedButton *sbNewBrush;
	TBevel *Bevel18;
	TSpeedButton *sbGradientBrush;
	TSpeedButton *sbFloodFill;
	TSpeedButton *sbPatternSpiral;
	TSpeedButton *sbPatternCircle;
	TSpeedButton *sbPatternSplitRing;
	TSpeedButton *sbPatternPetals;
	TSpeedButton *sbPatternGrid;
	TSpeedButton *sbPatternPyramid;
	TSpeedButton *sbPatternRightTriangle;
	TSpeedButton *sbPatternLeftTriangle;
	TLabel *lSelectedTool;
	TComboBox *cbMirrorMode;
	TPanel *pCanvas;
	TPanel *pLayers;
	TPanel *pCurrentColours;
	TShape *sSelectionMMB;
	TImage *iMMBGradient;
	TLabel *lPixelColour;
	TShape *sSelectionRMB;
	TShape *sSelectionLMB;
	TShape *sColour0;
	TShape *sColour1;
	TLabel *lBackground;
	TShape *sColour3;
	TShape *sColour2;
	TBevel *Bevel12;
	TPanel *panelRGBPalette;
	TShape *sRGBPalette1;
	TShape *sRGBPalette2;
	TShape *sRGBPalette3;
	TShape *sRGBPalette4;
	TShape *sRGBPalette5;
	TShape *sRGBPalette6;
	TShape *sRGBPalette7;
	TShape *sRGBPalette8;
	TShape *sRGBPalette9;
	TShape *sRGBPalette10;
	TShape *sRGBPalette11;
	TShape *sRGBPalette12;
	TShape *sRGBPalette13;
	TShape *sRGBPalette14;
	TShape *sRGBPalette15;
	TShape *sRGBPalette16;
	TShape *Shape37;
	TShape *Shape38;
	TShape *Shape39;
	TShape *Shape40;
	TShape *Shape41;
	TShape *Shape42;
	TShape *Shape43;
	TShape *Shape44;
	TShape *Shape45;
	TShape *Shape46;
	TShape *sShade1;
	TShape *sShade2;
	TShape *sShade3;
	TShape *sShade4;
	TShape *sShade5;
	TShape *sShade6;
	TShape *sShade7;
	TShape *sShade8;
	TShape *sShade9;
	TShape *sShade10;
	TShape *sShade16;
	TShape *sShade15;
	TShape *sShade14;
	TShape *sShade13;
	TShape *sShade12;
	TShape *sShade11;
	TLabel *Label1;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label9;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label12;
	TLabel *Label13;
	TLabel *Label14;
	TLabel *Label15;
	TLabel *Label16;
	TLabel *Label17;
	TPanel *pRGB_3BPP;
	TShape *sRGB3pp1;
	TShape *sRGB3pp2;
	TShape *sRGB3pp3;
	TShape *sRGB3pp4;
	TShape *sRGB3pp5;
	TShape *sRGB3pp6;
	TShape *sRGB3pp7;
	TShape *sRGB3pp8;
	TPanel *pUndoToolbar;
	TPanel *pQuickData;
	TMainMenu *miMain;
	TMenuItem *File1;
	TMenuItem *New1;
	TMenuItem *N8;
	TMenuItem *Load1;
	TMenuItem *miReopenMenu;
	TMenuItem *N55;
	TMenuItem *miImportFromBitmap;
	TMenuItem *miImportFromGIF;
	TMenuItem *miImportInToCurrent;
	TMenuItem *N31;
	TMenuItem *miAppend;
	TMenuItem *miMerge;
	TMenuItem *N5;
	TMenuItem *miSave;
	TMenuItem *miSaveAs;
	TMenuItem *miSaveSingleFrame;
	TMenuItem *miSaveRange;
	TMenuItem *miSaveAsFont;
	TMenuItem *N41;
	TMenuItem *miExportToBitmap;
	TMenuItem *miExportAnimationToBitmap;
	TMenuItem *miExportToGIF;
	TMenuItem *N25;
	TMenuItem *Preferences1;
	TMenuItem *N4;
	TMenuItem *Exit1;
	TMenuItem *Edit1;
	TMenuItem *miUndo;
	TMenuItem *miRedo;
	TMenuItem *N15;
	TMenuItem *miCopy;
	TMenuItem *miCopyFromPrevious;
	TMenuItem *miCopyMultiple;
	TMenuItem *miPaste;
	TMenuItem *miPasteSpecial;
	TMenuItem *Copyandshiftleft1;
	TMenuItem *Copyandshiftright1;
	TMenuItem *Copyandshiftup1;
	TMenuItem *Copyandshiftdown1;
	TMenuItem *N16;
	TMenuItem *miBrushActions;
	TMenuItem *Rotateanticlockwise1;
	TMenuItem *Rotateclockwise1;
	TMenuItem *N45;
	TMenuItem *miBrushFlip;
	TMenuItem *Mirror1;
	TMenuItem *Invert1;
	TMenuItem *N46;
	TMenuItem *Pasteintoeveryframe1;
	TMenuItem *Pasteintoeveryframetransparent1;
	TMenuItem *N37;
	TMenuItem *miShiftLeft;
	TMenuItem *miShiftRight;
	TMenuItem *miShiftUp;
	TMenuItem *miShiftDown;
	TMenuItem *N14;
	TMenuItem *miRotateL;
	TMenuItem *miRotateR;
	TMenuItem *N13;
	TMenuItem *miFlip;
	TMenuItem *miMirror;
	TMenuItem *miInvert;
	TMenuItem *N26;
	TMenuItem *miAddComment;
	TMenuItem *View1;
	TMenuItem *miShowAnimationToolbar;
	TMenuItem *miPaletteGradientToolbar;
	TMenuItem *miQuickData;
	TMenuItem *miUndoToolbar;
	TMenuItem *N57;
	TMenuItem *Backgroundcolour1;
	TMenuItem *miCustomBackground;
	TMenuItem *N42;
	TMenuItem *Black1;
	TMenuItem *Darkgrey1;
	TMenuItem *Grey1;
	TMenuItem *Green1;
	TMenuItem *Purple1;
	TMenuItem *Red1;
	TMenuItem *White1;
	TMenuItem *N18;
	TMenuItem *miFontMode;
	TMenuItem *miASCIIStartCode;
	TMenuItem *N7;
	TMenuItem *miPreviousFrame;
	TMenuItem *miNextFrame;
	TMenuItem *N23;
	TMenuItem *miGridToggle;
	TMenuItem *Preview1;
	TMenuItem *miPreview;
	TMenuItem *PreviewSize1;
	TMenuItem *miPreviewx1;
	TMenuItem *miPreviewx2;
	TMenuItem *miPreviewx3;
	TMenuItem *miPreviewx4;
	TMenuItem *miPreviewx5;
	TMenuItem *miPreviewx6;
	TMenuItem *miPreviewx8;
	TMenuItem *miPreviewx10;
	TMenuItem *miPreviewx12;
	TMenuItem *miPreviewx15;
	TMenuItem *miPreviewx20;
	TMenuItem *miPreviewx25;
	TMenuItem *miPreviewx30;
	TMenuItem *miPreviewx40;
	TMenuItem *miPreviewx50;
	TMenuItem *N51;
	TMenuItem *miIncrementRadially;
	TMenuItem *miPreviewView;
	TMenuItem *miPreviewViewSquare;
	TMenuItem *miPreviewViewRadial;
	TMenuItem *miPreviewViewRadialTQ;
	TMenuItem *miPreviewViewSemiCircle;
	TMenuItem *miPreviewViewSemiCircleInverted;
	TMenuItem *PreviewVoidRadial1;
	TMenuItem *miPreviewVoid10;
	TMenuItem *miPreviewVoid15;
	TMenuItem *miPreviewVoid20;
	TMenuItem *miPreviewVoid25;
	TMenuItem *miPreviewVoid30;
	TMenuItem *miPreviewVoid40;
	TMenuItem *miPreviewVoid50;
	TMenuItem *Previewoffsetradialsemicircle1;
	TMenuItem *miRadialOffset0;
	TMenuItem *miRadialOffset45;
	TMenuItem *miRadialOffset90;
	TMenuItem *miRadialOffset135;
	TMenuItem *miRadialOffset180;
	TMenuItem *miRadialOffset225;
	TMenuItem *miRadialOffset270;
	TMenuItem *miRadialOffset315;
	TMenuItem *N48;
	TMenuItem *miPreviewOffsetReverse;
	TMenuItem *N6;
	TMenuItem *miPopoutPreview;
	TMenuItem *Project1;
	TMenuItem *miClearAllFramesLayer;
	TMenuItem *miClearAllFrames;
	TMenuItem *miClearAllFramesGradient;
	TMenuItem *N22;
	TMenuItem *miFlipAllFrames;
	TMenuItem *miMirrorAllFrames;
	TMenuItem *miInvertAllFrames;
	TMenuItem *miGradientAllFrames;
	TMenuItem *N36;
	TMenuItem *miIgnoredPixels;
	TMenuItem *miSetIgnoredPixels;
	TMenuItem *miSetIgnoredFromPattern;
	TMenuItem *N44;
	TMenuItem *miClearAllIgnoredPixels;
	TMenuItem *N56;
	TMenuItem *miSaveIgnoredPixelsAsPattern;
	TMenuItem *miLoadIgnoredPixelsAsPattern;
	TMenuItem *N43;
	TMenuItem *miFadeFirstLast;
	TMenuItem *N35;
	TMenuItem *miExport;
	TMenuItem *miCodeTemplates;
	TMenuItem *N50;
	TMenuItem *miUnlockAll;
	TMenuItem *miLockAll;
	TMenuItem *miToggleLockStatus;
	TMenuItem *Draw1;
	TMenuItem *miMouseMode;
	TMenuItem *miNewBrush;
	TMenuItem *miDrawCopy;
	TMenuItem *N59;
	TMenuItem *miFilledRectangle;
	TMenuItem *miFrame;
	TMenuItem *miFilledCircle;
	TMenuItem *miEmptyCircle;
	TMenuItem *N60;
	TMenuItem *miLine;
	TMenuItem *miMultiDraw;
	TMenuItem *miFloodFill;
	TMenuItem *miFont;
	TMenuItem *miGradientBrush;
	TMenuItem *miGradient;
	TMenuItem *miRandomDraw;
	TMenuItem *miPicker;
	TMenuItem *N58;
	TMenuItem *miPatternSpiral;
	TMenuItem *miPatternCircle;
	TMenuItem *miPatternSplitRing;
	TMenuItem *miPatternPetals;
	TMenuItem *miPatternGrid;
	TMenuItem *miPatternPyramid;
	TMenuItem *miPatternLeftTriangle;
	TMenuItem *miPatternRightTriangle;
	TMenuItem *Frames1;
	TMenuItem *miAddFrame;
	TMenuItem *miAddFrameCopy;
	TMenuItem *miAddFrameMultiple;
	TMenuItem *N61;
	TMenuItem *miDeleteFrame;
	TMenuItem *miDeleteMultipleFrames;
	TMenuItem *Layers1;
	TMenuItem *miToggleLayoutPanel;
	TMenuItem *N53;
	TMenuItem *miClearLayer;
	TMenuItem *N54;
	TMenuItem *miFlattenLayers;
	TMenuItem *Colours1;
	TMenuItem *miChangeColoursFrame;
	TMenuItem *miChangeColoursLayer;
	TMenuItem *miChangeColoursAll;
	TMenuItem *N29;
	TMenuItem *miCountColours;
	TMenuItem *Currentframe1;
	TMenuItem *Animation1;
	TMenuItem *Buffer1;
	TMenuItem *miCopyCurrentTo;
	TMenuItem *miMemory1;
	TMenuItem *miMemory2;
	TMenuItem *miMemory3;
	TMenuItem *miMemory4;
	TMenuItem *miMemory5;
	TMenuItem *miMemory6;
	TMenuItem *miMemory7;
	TMenuItem *miMemory8;
	TMenuItem *miMemory9;
	TMenuItem *miMemory10;
	TMenuItem *miRestoreCurrentFrom;
	TMenuItem *miMemoryR1;
	TMenuItem *miMemoryR2;
	TMenuItem *miMemoryR3;
	TMenuItem *miMemoryR4;
	TMenuItem *miMemoryR5;
	TMenuItem *miMemoryR6;
	TMenuItem *miMemoryR7;
	TMenuItem *miMemoryR8;
	TMenuItem *miMemoryR9;
	TMenuItem *miMemoryR10;
	TMenuItem *N2;
	TMenuItem *miExportUserMemories;
	TMenuItem *N3;
	TMenuItem *miClearAllUserMemories;
	TMenuItem *ools1;
	TMenuItem *miAutoSave;
	TMenuItem *Autosaveinterval1;
	TMenuItem *miAutosave2;
	TMenuItem *miAutosave5;
	TMenuItem *miAutosave10;
	TMenuItem *N32;
	TMenuItem *Openautosavefolder1;
	TMenuItem *N38;
	TMenuItem *miAutomate;
	TMenuItem *N27;
	TMenuItem *miOptimiseData;
	TMenuItem *N52;
	TMenuItem *miFontViewer;
	TMenuItem *miHelp;
	TMenuItem *Help1;
	TMenuItem *Showshortcutkeys1;
	TMenuItem *N24;
	TMenuItem *miLanguage;
	TMenuItem *N12;
	TMenuItem *Examples1;
	TMenuItem *N21;
	TMenuItem *Checkforupdates1;
	TMenuItem *N19;
	TMenuItem *witter1;
	TMenuItem *Website1;
	TMenuItem *N17;
	TMenuItem *miAbout;
	TColorDialog *colorDialog;
	TTimer *timerAnimate;
	TImageList *ilMain;
	TSaveDialog *sdMain;
	TOpenDialog *odMain;
	TImageList *ilMenu;
	TPopupMenu *puPresets;
	TMenuItem *Presets1;
	TMenuItem *N9;
	TMenuItem *miLoadPreset;
	TMenuItem *N10;
	TMenuItem *miPresetSaveCurrent;
	TPopupMenu *puPixelSize;
	TMenuItem *PixelSize1;
	TMenuItem *N11;
	TMenuItem *miPixelTiny;
	TMenuItem *miPixelSmall;
	TMenuItem *miPixelMedium;
	TMenuItem *miPixelLarge;
	TMenuItem *miPixelVeryLarge;
	TMenuItem *miPixelUltra;
	TMenuItem *miPixelMegaUltra;
	TMenuItem *N30;
	TMenuItem *miPixelAuto;
	TPopupMenu *puFonts;
	TMenuItem *MenuItem1;
	TMenuItem *MenuItem2;
	TMenuItem *miLoadFont;
	TMenuItem *N47;
	TMenuItem *Fontviewer2;
	TMenuItem *N28;
	TMenuItem *miFontWrap;
	TPopupMenu *puAnimationSpeed;
	TMenuItem *Playbackspeed1;
	TMenuItem *N20;
	TMenuItem *miPlaybackSpeed1;
	TMenuItem *miPlaybackSpeed2;
	TMenuItem *miPlaybackSpeed3;
	TMenuItem *miPlaybackSpeed4;
	TMenuItem *miPlaybackSpeed5;
	TMenuItem *miPlaybackSpeed6;
	TMenuItem *miPlaybackSpeed7;
	TMenuItem *miPlaybackSpeed8;
	TMenuItem *miPlaybackSpeed9;
	TMenuItem *miPlaybackSpeed10;
	TMenuItem *miPlaybackSpeed11;
	TMenuItem *N39;
	TMenuItem *miPlaybackSpeedCustom;
	TMenuItem *N40;
	TMenuItem *Setcustomspeed1;
	TTimer *timerAutosave;
	TPopupMenu *puGradient;
	TMenuItem *miGradientColour0;
	TMenuItem *Colour11;
	TMenuItem *Colour21;
	TMenuItem *Colour31;
	TPopupMenu *puGradients;
	TMenuItem *MenuItem3;
	TMenuItem *MenuItem4;
	TMenuItem *miLoadGradients;
	TMenuItem *MenuItem6;
	TMenuItem *miSaveGradient;
	TPopupMenu *puPixelShape;
	TMenuItem *MenuItem5;
	TMenuItem *MenuItem7;
	TMenuItem *miPixelShapeSquare;
	TMenuItem *miPixelShapeRound;
	TMenuItem *miPixelShapeRoundRect;
	TPopupMenu *puBrushSize;
	TMenuItem *MenuItem8;
	TMenuItem *MenuItem9;
	TMenuItem *miBrushSizeSmall;
	TMenuItem *Large1;
	TMenuItem *Large3x3pixels1;
	TPopupMenu *puGradientRGB;
	TMenuItem *miGradientSelectRGB;
	TMenuItem *N33;
	TMenuItem *miGradSetRow;
	TMenuItem *miGradFrom;
	TMenuItem *miGradientBottomTop;
	TPopupMenu *puRandom;
	TMenuItem *MenuItem10;
	TMenuItem *MenuItem11;
	TMenuItem *miRandomnessTiny;
	TMenuItem *Small2;
	TMenuItem *Medium1;
	TMenuItem *Large2;
	TMenuItem *Massive1;
	TSavePictureDialog *spdMain;
	TPopupMenu *puPreview;
	TMenuItem *Previewsize2;
	TMenuItem *miPUPreviewx1;
	TMenuItem *miPUPreviewx2;
	TMenuItem *miPUPreviewx3;
	TMenuItem *miPUPreviewx4;
	TMenuItem *miPUPreviewx5;
	TMenuItem *miPUPreviewx6;
	TMenuItem *miPUPreviewx8;
	TMenuItem *miPUPreviewx10;
	TMenuItem *miPUPreviewx12;
	TMenuItem *miPUPreviewx15;
	TMenuItem *miPUPreviewx20;
	TMenuItem *miPUPreviewx25;
	TMenuItem *miPUPreviewx30;
	TMenuItem *miPUPreviewx40;
	TMenuItem *miPUPreviewx50;
	TMenuItem *Previewview2;
	TMenuItem *miPUPreviewViewSquare;
	TMenuItem *miPUPreviewViewRadial;
	TMenuItem *miPUPreviewViewRadialTQ;
	TMenuItem *miPUPreviewViewSemiCircle;
	TMenuItem *miPUPreviewViewSemiCircleInverted;
	TMenuItem *PreviewvoidRadialSemicircle1;
	TMenuItem *miPUPreviewVoid10;
	TMenuItem *miPUPreviewVoid15;
	TMenuItem *miPUPreviewVoid20;
	TMenuItem *miPUPreviewVoid25;
	TMenuItem *miPUPreviewVoid30;
	TMenuItem *miPUPreviewVoid40;
	TMenuItem *miPUPreviewVoid50;
	TOpenPictureDialog *opdMain;
	TPopupMenu *puGradientRGB_3BPP;
	TMenuItem *miGradientRGB3BPP1;
	TMenuItem *MenuItem15;
	TMenuItem *MenuItem13;
	TMenuItem *MenuItem14;
	TMenuItem *Red1001;
	TMenuItem *Magenta1011;
	TMenuItem *Yellow1101;
	TMenuItem *White1111;
	TPopupMenu *puMainCanvas;
	TMenuItem *Workingareabackgroundcolour1;
	TMenuItem *Custom1;
	TMenuItem *N1;
	TMenuItem *Black2;
	TMenuItem *Darkgreydefault1;
	TMenuItem *Grey2;
	TMenuItem *Green2;
	TMenuItem *Purple2;
	TMenuItem *Red2;
	TMenuItem *White2;
	TBitBtn *bAddFrameMultiple;
	TBitBtn *bDeleteMultipleFrames;
	TMenuItem *miDebug;
	TMenuItem *RenderMode1;
	TMenuItem *MonoColours1;
	TMenuItem *CurrentLayerFrame1;
	TMenuItem *Controls1;
	TImageList *ilActive;
	TSpeedButton *sbOpen;
	TImage *iColoursLeft;
	TImage *iColoursMiddle;
	TImage *iColoursRight;
	TBitBtn *bTimer;
	TMenuItem *N34;
	TMenuItem *miGradientFillFrame;
	TMenuItem *N49;
	TMenuItem *N62;
	TMenuItem *miGradientLoad;
	TMenuItem *miGradientSave;
	TMenuItem *Preview2;
	TMenuItem *N63;
	TMenuItem *PaintBox1;
	TLabel *lMirror;
	TBevel *Bevel21;
	TMenuItem *N4x41;
	TMenuItem *N5x51;
	TMenuItem *N64;
	TMenuItem *miDrawTestPattern;
	TMenuItem *N65;
	TMenuItem *miPreviewAllowDrawing;
	TMenuItem *N66;
	TMenuItem *miHideIgnoredPixels;
	void __fastcall sbBuildClick(TObject *Sender);
	void __fastcall FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
          int &MaxWidth, int &MaxHeight);
	void __fastcall miMemoryR1Click(TObject *Sender);
	void __fastcall miMemory1Click(TObject *Sender);
	void __fastcall miClearAllUserMemoriesClick(TObject *Sender);
	void __fastcall miExportUserMemoriesClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall sbOpenClick(TObject *Sender);
	void __fastcall sbSaveClick(TObject *Sender);
	void __fastcall sbExportClick(TObject *Sender);
	void __fastcall sbGenerateCodeClick(TObject *Sender);
	void __fastcall sbPixelSizeClick(TObject *Sender);
	void __fastcall sbPixelShapeClick(TObject *Sender);
	void __fastcall sbPresetClick(TObject *Sender);
	void __fastcall miSaveAsClick(TObject *Sender);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall bPlayAnimationClick(TObject *Sender);
	void __fastcall cbMirrorModeChange(TObject *Sender);
	void __fastcall miPixelShapeSquareClick(TObject *Sender);
	void __fastcall puGradientRGBPopup(TObject *Sender);
	void __fastcall miFontModeClick(TObject *Sender);
	void __fastcall miPreviewx1Click(TObject *Sender);
	void __fastcall miPreviewViewSquareClick(TObject *Sender);
	void __fastcall miPreviewVoid10Click(TObject *Sender);
	void __fastcall miRadialOffset45Click(TObject *Sender);
	void __fastcall miPreviewOffsetReverseClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall miPixelTinyClick(TObject *Sender);
	void __fastcall miPlaybackSpeed3Click(TObject *Sender);
	void __fastcall sbClearClick(TObject *Sender);
	void __fastcall sbMirrorClick(TObject *Sender);
	void __fastcall sbScrollLeftClick(TObject *Sender);
	void __fastcall sbRotateLClick(TObject *Sender);
	void __fastcall sbRotateAnyClick(TObject *Sender);
	void __fastcall bLockFrameClick(TObject *Sender);
	void __fastcall sbMouseModeClick(TObject *Sender);
	void __fastcall sbNewBrushClick(TObject *Sender);
	void __fastcall sbGradientClick(TObject *Sender);
	void __fastcall sRGBPalette1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall sRGBPalette1MouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y);
	void __fastcall sShade1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall bAddFrameClick(TObject *Sender);
	void __fastcall bAddFrameCopyClick(TObject *Sender);
	void __fastcall bAddFrameMultipleClick(TObject *Sender);
	void __fastcall bDeleteFrameClick(TObject *Sender);
	void __fastcall bDeleteMultipleFramesClick(TObject *Sender);
	void __fastcall bLightboxClick(TObject *Sender);
	void __fastcall tbFramesChange(TObject *Sender);
	void __fastcall miAutoSaveClick(TObject *Sender);
	void __fastcall miAutosave2Click(TObject *Sender);
	void __fastcall Openautosavefolder1Click(TObject *Sender);
	void __fastcall miAutomateClick(TObject *Sender);
	void __fastcall miOptimiseDataClick(TObject *Sender);
	void __fastcall miFontViewerClick(TObject *Sender);
	void __fastcall New1Click(TObject *Sender);
	void __fastcall miImportFromBitmapClick(TObject *Sender);
	void __fastcall miImportFromGIFClick(TObject *Sender);
	void __fastcall miImportInToCurrentClick(TObject *Sender);
	void __fastcall miAppendClick(TObject *Sender);
	void __fastcall miMergeClick(TObject *Sender);
	void __fastcall miSaveSingleFrameClick(TObject *Sender);
	void __fastcall miSaveRangeClick(TObject *Sender);
	void __fastcall miSaveAsFontClick(TObject *Sender);
	void __fastcall miExportToBitmapClick(TObject *Sender);
	void __fastcall miExportAnimationToBitmapClick(TObject *Sender);
	void __fastcall miExportToGIFClick(TObject *Sender);
	void __fastcall Preferences1Click(TObject *Sender);
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall FormMouseWheelDown(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall FormMouseWheelUp(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall FormMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall timerAutosaveTimer(TObject *Sender);
	void __fastcall timerAnimateTimer(TObject *Sender);
	void __fastcall Website1Click(TObject *Sender);
	void __fastcall miAboutClick(TObject *Sender);
	void __fastcall witter1Click(TObject *Sender);
	void __fastcall miToggleLayoutPanelClick(TObject *Sender);
	void __fastcall miClearLayerClick(TObject *Sender);
	void __fastcall miUndoClick(TObject *Sender);
	void __fastcall miRedoClick(TObject *Sender);
	void __fastcall miCopyClick(TObject *Sender);
	void __fastcall miCopyFromPreviousClick(TObject *Sender);
	void __fastcall miCopyMultipleClick(TObject *Sender);
	void __fastcall miPasteClick(TObject *Sender);
	void __fastcall Copyandshiftleft1Click(TObject *Sender);
	void __fastcall Rotateanticlockwise1Click(TObject *Sender);
	void __fastcall Rotateclockwise1Click(TObject *Sender);
	void __fastcall miBrushFlipClick(TObject *Sender);
	void __fastcall Pasteintoeveryframe1Click(TObject *Sender);
	void __fastcall Pasteintoeveryframetransparent1Click(TObject *Sender);
	void __fastcall miShiftLeftClick(TObject *Sender);
	void __fastcall miFlipClick(TObject *Sender);
	void __fastcall miRotateLClick(TObject *Sender);
	void __fastcall miChangeColoursFrameClick(TObject *Sender);
	void __fastcall Currentframe1Click(TObject *Sender);
	void __fastcall Animation1Click(TObject *Sender);
	void __fastcall miPreviewClick(TObject *Sender);
	void __fastcall miPopoutPreviewClick(TObject *Sender);
	void __fastcall miClearAllFramesLayerClick(TObject *Sender);
	void __fastcall miClearAllFramesClick(TObject *Sender);
	void __fastcall miClearAllFramesGradientClick(TObject *Sender);
	void __fastcall miFlipAllFramesClick(TObject *Sender);
	void __fastcall miMirrorAllFramesClick(TObject *Sender);
	void __fastcall miInvertAllFramesClick(TObject *Sender);
	void __fastcall miGradientAllFramesClick(TObject *Sender);
	void __fastcall miSetIgnoredPixelsClick(TObject *Sender);
	void __fastcall miSetIgnoredFromPatternClick(TObject *Sender);
	void __fastcall miClearAllIgnoredPixelsClick(TObject *Sender);
	void __fastcall miSaveIgnoredPixelsAsPatternClick(TObject *Sender);
	void __fastcall miLoadIgnoredPixelsAsPatternClick(TObject *Sender);
	void __fastcall miFadeFirstLastClick(TObject *Sender);
	void __fastcall miUnlockAllClick(TObject *Sender);
	void __fastcall miLockAllClick(TObject *Sender);
	void __fastcall miToggleLockStatusClick(TObject *Sender);
	void __fastcall miShowAnimationToolbarClick(TObject *Sender);
	void __fastcall miPaletteGradientToolbarClick(TObject *Sender);
	void __fastcall miQuickDataClick(TObject *Sender);
	void __fastcall miUndoToolbarClick(TObject *Sender);
	void __fastcall miCustomBackgroundClick(TObject *Sender);
	void __fastcall Black1Click(TObject *Sender);
	void __fastcall miASCIIStartCodeClick(TObject *Sender);
	void __fastcall miPreviousFrameClick(TObject *Sender);
	void __fastcall miNextFrameClick(TObject *Sender);
	void __fastcall miGridToggleClick(TObject *Sender);
	void __fastcall Showshortcutkeys1Click(TObject *Sender);
	void __fastcall Help1Click(TObject *Sender);
	void __fastcall miBrushSizeSmallClick(TObject *Sender);
	void __fastcall miRandomnessTinyClick(TObject *Sender);
	void __fastcall miPresetSaveCurrentClick(TObject *Sender);
	void __fastcall miFontWrapClick(TObject *Sender);
	void __fastcall miGradientColour0Click(TObject *Sender);
	void __fastcall miGradientRGB3BPP1Click(TObject *Sender);
	void __fastcall miGradientSelectRGBClick(TObject *Sender);
	void __fastcall miGradSetRowClick(TObject *Sender);
	void __fastcall miGradFromClick(TObject *Sender);
	void __fastcall miGradientBottomTopClick(TObject *Sender);
	void __fastcall pCanvasMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall pCanvasMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
	void __fastcall Examples1Click(TObject *Sender);
	void __fastcall sColour3MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall Setcustomspeed1Click(TObject *Sender);
	void __fastcall miSaveGradientClick(TObject *Sender);
	void __fastcall sRGB3pp1MouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall miIncrementRadiallyClick(TObject *Sender);
	void __fastcall miAddCommentClick(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall RenderMode1Click(TObject *Sender);
	void __fastcall miFlattenLayersClick(TObject *Sender);
	void __fastcall MonoColours1Click(TObject *Sender);
	void __fastcall CurrentLayerFrame1Click(TObject *Sender);
	void __fastcall Controls1Click(TObject *Sender);
	void __fastcall miMouseModeClick(TObject *Sender);
	void __fastcall tbFramesTracking(TObject *Sender);
	void __fastcall bTimerClick(TObject *Sender);
	void __fastcall miGradientFillFrameClick(TObject *Sender);
	void __fastcall Checkforupdates1Click(TObject *Sender);
	void __fastcall Preview2Click(TObject *Sender);
	void __fastcall PaintBox1Click(TObject *Sender);
	void __fastcall miDrawTestPatternClick(TObject *Sender);
	void __fastcall miPreviewAllowDrawingClick(TObject *Sender);
	void __fastcall miHideIgnoredPixelsClick(TObject *Sender);

private:

	void __fastcall WmDropFiles(TWMDropFiles& Message);

	static const int CAnimPlayStart     = 0;
	static const int CAnimPlayStop      = 1;
	static const int CAnimFirstFrame    = 2;
	static const int CAnimPreviousFrame = 3;
	static const int CAnimNextFrame     = 4;
	static const int CAnimLastFrame     = 5;

	static const int CLoadProject       = 0;
	static const int CLoadIgnorePixels  = 1;

	static const int CSaveProject       = 0;
	static const int CSaveFont          = 1;
	static const int CSaveIgnorePixels  = 2;

	TheMatrix *thematrix = nullptr;

	TframeFont *FrameFontPanel;
	TframeGradient *FrameGradientPanel;
	TframeLayers *FrameLayerPanel;
	TframePalette *FramePalettePanel;
	TframeSimpleExport *FrameQuickData;
	TframeUndos *FrameUndoPanel;

	MatrixMode OldMatrixMode = MatrixMode::kNone;
	int OldMouseX = -1;
	int OldMouseY = -1;

    int LastTick = 0;

	std::wstring BackupCaption = L"";

	std::vector<std::wstring> DrawModes;

    ActionObject Automation;

	// -- component cache (makes iteration much easier) ----------------------
    TShape *MatrixGradient[__MaxHeight];

	TMenuItem *_MenuCopyMemory[10];
	TMenuItem *_MenuRestoryMemory[10];
	TMenuItem *_PreviewMenuSize[2][15];
	TMenuItem *_PreviewMenuView[2][5];
	TMenuItem *_PreviewMenuVoid[2][7];

	TShape *_RGBPalette[16];
    TShape *_RGB3ppPalette[8];
	TShape *_RGBShade[16];

    std::vector<TMenuItem*> FileHistoryMenus;

	ProjectSettings UserProjectSettings;

	void InitComponentCache();
	void ConfigureControls();
	void InitFrames();
	void ManageUIControls(bool, bool);

	int GetSelectedFrame();

	// -- language -----------------------------------------------------------

	void SetGuiLanguageText();
	std::wstring GetDrawModeText(int);
    void UpdateDrawModeCaption(int);

	// -- dialogs ------------------------------------------------------------

	void ConfigureOpenDialog(int);
	void ConfigureSaveDialog(int);

	// -- playback -----------------------------------------------------------
	void PlaybackCommand(int);
	void PlaybackStart();
	void PlaybackStop();
	void PlaybackFirstFrame();
	void PlaybackPreviousFrame();
	void PlaybackNextFrame();
	void PlaybackLastFrame();

	// -- file io ------------------------------------------------------------
	bool LoadFromFileName(const std::wstring);
	bool LoadFromGIF(const std::wstring);

    // -- menu helper --------------------------------------------------------

	void BuildFontMenu();
	void BuildGradientMenu();
	void BuildLanguageMenu();
	void BuildPresetMenu();

	// -- menu file ----------------------------------------------------------
	void __fastcall ReopenClick(TObject*);
	void BuildReOpenMenu();

	// -- menu draw ----------------------------------------------------------
	void SetDrawingMode(int);

	// -- menu gradient ------------------------------------------------------
	void __fastcall SelectGradient(TObject*);


	// import/export
	void SetSimpleExport(ExportOptions);
	bool AppendFromFileName(const std::wstring);
	bool MergeFromFileName(const std::wstring, int, LoadMode);

	// -- gui ----
	void UpdateMemoryUsage();
	void UpdateData();
	void SetFrameCaption(int);
	void UpdateDisplay(int);

	// -- misc, to organise

	void BuildImportData(ImportData&, int, int);
	ProjectColours GetColours();
	void ChangeMatrixType();
	void SetCurrentProjectFileName(const std::wstring);
	void ClearCurrentProjectFileName();
	void SetupMatrixColours();
	void ToggleGradient(GradientOption, bool);
	void SetPreview(int, ViewShape, int, int, bool, bool);
	void SetFromSettings();
	void SystemSetBackgroundColour(int);
	void SetPlaybackCustom(int);
	void DisplayFrame(int);
	void GenerateShades(int);
	void ScrollFrame(int);
	void RotateFrame(int);
	void FrameEffect(int);
	void UpdateGradientColours();
	void LoadPreset(const std::wstring);
	void LoadWithWarnings(const std::wstring);

	void __fastcall LanguageClick(TObject *);
	void __fastcall SelectFont(TObject *);
	void __fastcall SelectPreset(TObject*);
	void __fastcall PaletteColourSelected(int button, int);
	void __fastcall OnPopoutClosed(TObject*, TCloseAction &);

	void SyncPreviewSize(int);
	void SyncPreviewView(int);
	void SyncPreviewVoid(int);


    void __fastcall OnGradientClick(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y);

	// -----

	// formreview callbacks
	void PreviewWindowCommand(int);
	void PreviewWindowChangeFrame(int);

	// framegradient callbacks
	void __fastcall CopyToGradientBrush();
	void __fastcall CopyFromCustom();
	void __fastcall CopyFromShades();

	// framelayer callbacks
	void __fastcall OnLayerPanelClose(TframeLayers*);
	void __fastcall OnLayerMenuItem(int);

	// framepalette callbacks
	void __fastcall PaletteColourOver(int);

	// framequickdata callbacks
	void __fastcall QuickDataChange(TframeSimpleExport*);

	// frameundos callbacks
	void __fastcall OnUndoSelected(int);

	// matrix callbacks
	void __fastcall MatrixOnChange(TheMatrix*);
	void __fastcall MatrixOnLayerChange(TheMatrix*);
	void __fastcall MatrixOnSizeChange(TheMatrix*);
	void __fastcall MatrixOnDisplayBufferCopied(TheMatrix*);
	void __fastcall MatrixOnNewFrameDisplayed(TheMatrix*);
	void __fastcall MatrixOnColourChange(TheMatrix*);
	void __fastcall MatrixOnNew3bppColours(TheMatrix*);
	void __fastcall MatrixOnMouseOver(int, int);
	void __fastcall MatrixOnPreviewMouseDown(int, int);
	void __fastcall MatrixOnDebug(TheMatrix*, const std::wstring s);

public:
	__fastcall TfrmMain(TComponent* Owner);

BEGIN_MESSAGE_MAP
   MESSAGE_HANDLER(WM_DROPFILES, TWMDropFiles, WmDropFiles)
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
