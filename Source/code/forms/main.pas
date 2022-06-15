// ===================================================================
//
// (c) Paul Alan Freshney 2012-2022
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit main;


interface


uses
  System.UITypes, System.Contnrs, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ToolWin, ComCtrls, Menus, ImgList, System.Generics.Collections,
  ExtDlgs, ActionObject, System.ImageList, Vcl.CheckLst, typinfo,

  formCopyMultiple, formDeleteMultipl, formExportGIF, formAutomate, formPlaybackSpeed, formSaveRange, formPreviewPopout,
  formToggleLockStatus, formMerge, formNewBrush, formColourChange, formOptimise,

  fileconstants, matrixconstants,

  languagehandler,

  exportOptions, exportoptions_monobi, importdata, systemsettings, formSetIgnoredPixels, presethandler,

  xglobal,

  colours,

  utility, projectsettings, datadisplay,

  frameGradientPanel, framePalettePanel, frameFontPanel, frameLayerPanel, frameUndoPanel, frameQuickData,

  thematrix, matrix, matrixdead, Vcl.Grids;


type
  TfrmMain = class(TForm)
    File1: TMenuItem;
    Exit1: TMenuItem;
    colorDialog: TColorDialog;
    Buffer1: TMenuItem;
    miCopyCurrentTo: TMenuItem;
    miMemory1: TMenuItem;
    miMemory2: TMenuItem;
    miMemory3: TMenuItem;
    miMemory4: TMenuItem;
    miMemory5: TMenuItem;
    miMemory6: TMenuItem;
    miMemory7: TMenuItem;
    timerAnimate: TTimer;
    miMemory8: TMenuItem;
    miMemory9: TMenuItem;
    miMemory10: TMenuItem;
    miRestoreCurrentFrom: TMenuItem;
    miMemoryR10: TMenuItem;
    miMemoryR9: TMenuItem;
    miMemoryR8: TMenuItem;
    miMemoryR7: TMenuItem;
    miMemoryR6: TMenuItem;
    miMemoryR5: TMenuItem;
    miMemoryR4: TMenuItem;
    miMemoryR3: TMenuItem;
    miMemoryR2: TMenuItem;
    miMemoryR1: TMenuItem;
    View1: TMenuItem;
    miShowAnimationToolbar: TMenuItem;
    pAnimationToolbar: TPanel;
    bPlayAnimation: TBitBtn;
    bStopAnimation: TBitBtn;
    bPreviousFrame: TBitBtn;
    bNextFrame: TBitBtn;
    lFrame: TLabel;
    N2: TMenuItem;
    miClearAllUserMemories: TMenuItem;
    Bevel5: TBevel;
    N3: TMenuItem;
    miExportUserMemories: TMenuItem;
    Edit1: TMenuItem;
    miUndo: TMenuItem;
    Bevel7: TBevel;
    bAddFrame: TBitBtn;
    About1: TMenuItem;
    miAbout: TMenuItem;
    bLightbox: TBitBtn;
    ilMain: TImageList;
    Bevel8: TBevel;
    N4: TMenuItem;
    miSaveSingleFrame: TMenuItem;
    miSaveAs: TMenuItem;
    N5: TMenuItem;
    Load1: TMenuItem;
    sdMain: TSaveDialog;
    odMain: TOpenDialog;
    ilMenu: TImageList;
    N7: TMenuItem;
    Bevel9: TBevel;
    bDeleteFrame: TBitBtn;
    miSave: TMenuItem;
    New1: TMenuItem;
    N8: TMenuItem;
    puPresets: TPopupMenu;
    Presets1: TMenuItem;
    N9: TMenuItem;
    miLoadPreset: TMenuItem;
    N10: TMenuItem;
    miPresetSaveCurrent: TMenuItem;
    statusMain: TStatusBar;
    puPixelSize: TPopupMenu;
    PixelSize1: TMenuItem;
    N11: TMenuItem;
    miPixelTiny: TMenuItem;
    miPixelSmall: TMenuItem;
    miPixelMedium: TMenuItem;
    miPixelLarge: TMenuItem;
    puFonts: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miLoadFont: TMenuItem;
    bAddFrameCopy: TBitBtn;
    Bevel11: TBevel;
    miPreviousFrame: TMenuItem;
    miNextFrame: TMenuItem;
    miShiftLeft: TMenuItem;
    miShiftRight: TMenuItem;
    miShiftUp: TMenuItem;
    miShiftDown: TMenuItem;
    N14: TMenuItem;
    miRotateL: TMenuItem;
    miRotateR: TMenuItem;
    N13: TMenuItem;
    miFlip: TMenuItem;
    miMirror: TMenuItem;
    miInvert: TMenuItem;
    N16: TMenuItem;
    miPixelVeryLarge: TMenuItem;
    pbFont: TPanel;
    miFontMode: TMenuItem;
    pASCIICode: TPanel;
    miCopyFromPrevious: TMenuItem;
    N15: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSaveAsFont: TMenuItem;
    tbFrames: TTrackBar;
    miPixelUltra: TMenuItem;
    bStartFrame: TBitBtn;
    bEndFrame: TBitBtn;
    N17: TMenuItem;
    Checkforupdates1: TMenuItem;
    N19: TMenuItem;
    Website1: TMenuItem;
    puAnimationSpeed: TPopupMenu;
    Playbackspeed1: TMenuItem;
    N20: TMenuItem;
    miPlaybackSpeed3: TMenuItem;
    miPlaybackSpeed4: TMenuItem;
    miPlaybackSpeed5: TMenuItem;
    miPlaybackSpeed7: TMenuItem;
    Help1: TMenuItem;
    N21: TMenuItem;
    miImportInToCurrent: TMenuItem;
    miFlipAllFrames: TMenuItem;
    miMirrorAllFrames: TMenuItem;
    miInvertAllFrames: TMenuItem;
    N22: TMenuItem;
    bAddFrameMultiple: TBitBtn;
    N23: TMenuItem;
    miGridToggle: TMenuItem;
    Project1: TMenuItem;
    miAutoSave: TMenuItem;
    Autosaveinterval1: TMenuItem;
    miAutosave2: TMenuItem;
    miAutosave5: TMenuItem;
    miAutosave10: TMenuItem;
    timerAutosave: TTimer;
    N25: TMenuItem;
    Importfrombitmap1: TMenuItem;
    miPlaybackSpeed8: TMenuItem;
    Examples1: TMenuItem;
    puGradient: TPopupMenu;
    Colour11: TMenuItem;
    Colour21: TMenuItem;
    Colour31: TMenuItem;
    Colour01: TMenuItem;
    puGradients: TPopupMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLoadGradients: TMenuItem;
    MenuItem6: TMenuItem;
    miSaveGradient: TMenuItem;
    miGradientAllFrames: TMenuItem;
    puPixelShape: TPopupMenu;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miPixelShapeSquare: TMenuItem;
    miPixelShapeRound: TMenuItem;
    miASCIIStartCode: TMenuItem;
    miExport: TMenuItem;
    N18: TMenuItem;
    miPreview: TMenuItem;
    PreviewSize1: TMenuItem;
    miPreviewx1: TMenuItem;
    miPreviewx2: TMenuItem;
    miPreviewx3: TMenuItem;
    miPreviewx4: TMenuItem;
    N12: TMenuItem;
    N26: TMenuItem;
    miAddComment: TMenuItem;
    miPreviewx5: TMenuItem;
    puBrushSize: TPopupMenu;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Small1: TMenuItem;
    Large1: TMenuItem;
    Large3x3pixels1: TMenuItem;
    miFontViewer: TMenuItem;
    N30: TMenuItem;
    miPixelAuto: TMenuItem;
    ools1: TMenuItem;
    miClearAllFramesLayer: TMenuItem;
    N31: TMenuItem;
    miAppend: TMenuItem;
    N32: TMenuItem;
    Openautosavefolder1: TMenuItem;
    miCodeTemplates: TMenuItem;
    puGradientRGB: TPopupMenu;
    miGradientSelectRGB: TMenuItem;
    N33: TMenuItem;
    miGradFrom: TMenuItem;
    miGradSetRow: TMenuItem;
    miClearAllFramesGradient: TMenuItem;
    puRandom: TPopupMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    miRandomnessTiny: TMenuItem;
    Small2: TMenuItem;
    Medium1: TMenuItem;
    Large2: TMenuItem;
    Massive1: TMenuItem;
    miSetDeadPixels: TMenuItem;
    N35: TMenuItem;
    Preferences1: TMenuItem;
    N36: TMenuItem;
    miFadeFirstLast: TMenuItem;
    miExportToBitmap: TMenuItem;
    spdMain: TSavePictureDialog;
    miPreviewx6: TMenuItem;
    miRedo: TMenuItem;
    N38: TMenuItem;
    miAutomate: TMenuItem;
    miPasteSpecial: TMenuItem;
    Copyandshiftleft1: TMenuItem;
    Copyandshiftright1: TMenuItem;
    Copyandshiftup1: TMenuItem;
    Copyandshiftdown1: TMenuItem;
    miPlaybackSpeed2: TMenuItem;
    miPlaybackSpeed1: TMenuItem;
    miPlaybackSpeed6: TMenuItem;
    miPlaybackSpeed9: TMenuItem;
    miPlaybackSpeed10: TMenuItem;
    miPlaybackSpeed11: TMenuItem;
    N39: TMenuItem;
    miPlaybackSpeedCustom: TMenuItem;
    N40: TMenuItem;
    Setcustomspeed1: TMenuItem;
    miPreviewViewSquare: TMenuItem;
    miPreviewViewRadial: TMenuItem;
    PreviewVoidRadial1: TMenuItem;
    miPreviewVoid10: TMenuItem;
    miPreviewVoid20: TMenuItem;
    N27: TMenuItem;
    Optimisedata1: TMenuItem;
    Backgroundcolour1: TMenuItem;
    miCustomBackground: TMenuItem;
    N42: TMenuItem;
    Black1: TMenuItem;
    Grey1: TMenuItem;
    Red1: TMenuItem;
    Green1: TMenuItem;
    White1: TMenuItem;
    Purple1: TMenuItem;
    N43: TMenuItem;
    miDeadPixels: TMenuItem;
    N44: TMenuItem;
    miClearAllDeadPixels: TMenuItem;
    miPreviewViewSemiCircle: TMenuItem;
    miPreviewx8: TMenuItem;
    miPreviewx10: TMenuItem;
    miPreviewx12: TMenuItem;
    miPreviewx15: TMenuItem;
    miPreviewx20: TMenuItem;
    miPreviewVoid25: TMenuItem;
    Darkgrey1: TMenuItem;
    miPreviewx25: TMenuItem;
    miPreviewx30: TMenuItem;
    miPreviewx50: TMenuItem;
    miMain: TMainMenu;
    miPreviewVoid30: TMenuItem;
    miPreviewVoid40: TMenuItem;
    miPreviewVoid50: TMenuItem;
    miPreviewVoid15: TMenuItem;
    miPreviewViewSemiCircleInverted: TMenuItem;
    pRGBPalette: TPanel;
    miPreviewViewRadialTQ: TMenuItem;
    N41: TMenuItem;
    miPreviewx40: TMenuItem;
    puPreview: TPopupMenu;
    Previewsize2: TMenuItem;
    miPUPreviewx50: TMenuItem;
    miPUPreviewx40: TMenuItem;
    miPUPreviewx30: TMenuItem;
    miPUPreviewx25: TMenuItem;
    miPUPreviewx20: TMenuItem;
    miPUPreviewx15: TMenuItem;
    miPUPreviewx12: TMenuItem;
    miPUPreviewx10: TMenuItem;
    miPUPreviewx8: TMenuItem;
    miPUPreviewx6: TMenuItem;
    miPUPreviewx5: TMenuItem;
    miPUPreviewx4: TMenuItem;
    miPUPreviewx3: TMenuItem;
    miPUPreviewx2: TMenuItem;
    miPUPreviewx1: TMenuItem;
    Previewview2: TMenuItem;
    miPUPreviewViewSemiCircleInverted: TMenuItem;
    miPUPreviewViewSemiCircle: TMenuItem;
    miPUPreviewViewRadialTQ: TMenuItem;
    miPUPreviewViewRadial: TMenuItem;
    miPUPreviewViewSquare: TMenuItem;
    PreviewvoidRadialSemicircle1: TMenuItem;
    miPUPreviewVoid50: TMenuItem;
    miPUPreviewVoid40: TMenuItem;
    miPUPreviewVoid30: TMenuItem;
    miPUPreviewVoid25: TMenuItem;
    miPUPreviewVoid20: TMenuItem;
    miPUPreviewVoid15: TMenuItem;
    miPUPreviewVoid10: TMenuItem;
    miBrushActions: TMenuItem;
    N37: TMenuItem;
    Rotateanticlockwise1: TMenuItem;
    Rotateclockwise1: TMenuItem;
    N45: TMenuItem;
    miBrushFlip: TMenuItem;
    Mirror1: TMenuItem;
    Invert1: TMenuItem;
    N46: TMenuItem;
    Pasteintoeveryframe1: TMenuItem;
    Pasteintoeveryframetransparent1: TMenuItem;
    witter1: TMenuItem;
    N47: TMenuItem;
    Fontviewer2: TMenuItem;
    miExportAnimationToBitmap: TMenuItem;
    miCopyMultiple: TMenuItem;
    bDeleteMultipleFrames: TBitBtn;
    Previewoffsetradialsemicircle1: TMenuItem;
    miRadialOffset45: TMenuItem;
    miRadialOffset90: TMenuItem;
    miRadialOffset135: TMenuItem;
    miRadialOffset180: TMenuItem;
    miRadialOffset225: TMenuItem;
    miRadialOffset270: TMenuItem;
    miRadialOffset315: TMenuItem;
    miRadialOffset0: TMenuItem;
    miPreviewOffsetReverse: TMenuItem;
    N48: TMenuItem;
    miPixelShapeRoundRect: TMenuItem;
    panelTop: TPanel;
    sbBuild: TSpeedButton;
    Bevel3: TBevel;
    sbPreset: TSpeedButton;
    sbPixelSize: TSpeedButton;
    Bevel17: TBevel;
    lMemoryUsage: TLabel;
    sbPixelShape: TSpeedButton;
    Bevel19: TBevel;
    sbSave: TSpeedButton;
    sbOpen: TSpeedButton;
    Bevel10: TBevel;
    sbExport: TSpeedButton;
    Bevel4: TBevel;
    Bevel16: TBevel;
    sbGenerateCode: TSpeedButton;
    panelMiddle: TPanel;
    sbClear: TSpeedButton;
    Bevel1: TBevel;
    sbMirror: TSpeedButton;
    sbFlip: TSpeedButton;
    Bevel2: TBevel;
    sbScrollLeft: TSpeedButton;
    sbScrollRight: TSpeedButton;
    sbScrollUp: TSpeedButton;
    sbScrollDown: TSpeedButton;
    sbInvert: TSpeedButton;
    Bevel6: TBevel;
    sbRotateL: TSpeedButton;
    sbRotateR: TSpeedButton;
    sbRotateAny: TSpeedButton;
    Bevel13: TBevel;
    Bevel14: TBevel;
    cbRotateAngle: TComboBox;
    cbRotateCount: TComboBox;
    paneTools: TPanel;
    sbMouseMode: TSpeedButton;
    sbFilledRectangle: TSpeedButton;
    sbLine: TSpeedButton;
    sbFrame: TSpeedButton;
    sbCopy: TSpeedButton;
    Bevel15: TBevel;
    sbGradient: TSpeedButton;
    sbRandomDraw: TSpeedButton;
    sbEmptyCircle: TSpeedButton;
    sbMultiDraw: TSpeedButton;
    sbPicker: TSpeedButton;
    sbFont: TSpeedButton;
    sbFilledCircle: TSpeedButton;
    sbNewBrush: TSpeedButton;
    pCanvas: TPanel;
    miSaveRange: TMenuItem;
    miPopoutPreview: TMenuItem;
    bLockFrame: TBitBtn;
    miUnlockAll: TMenuItem;
    miLockAll: TMenuItem;
    miToggleLockStatus: TMenuItem;
    N50: TMenuItem;
    N51: TMenuItem;
    miIncrementRadially: TMenuItem;
    pLayers: TPanel;
    Layers1: TMenuItem;
    miFlattenLayers: TMenuItem;
    pCurrentColours: TPanel;
    Colours1: TMenuItem;
    miChangeColoursFrame: TMenuItem;
    miChangeColoursAll: TMenuItem;
    N52: TMenuItem;
    Preview1: TMenuItem;
    N6: TMenuItem;
    cbMirrorMode: TComboBox;
    Bevel18: TBevel;
    ImportfromGIF1: TMenuItem;
    opdMain: TOpenPictureDialog;
    puGradientRGB_3BPP: TPopupMenu;
    miGradientRGB3BPP1: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    Red1001: TMenuItem;
    Magenta1011: TMenuItem;
    Yellow1101: TMenuItem;
    White1111: TMenuItem;
    N29: TMenuItem;
    miCountColours: TMenuItem;
    Currentframe1: TMenuItem;
    Animation1: TMenuItem;
    miExportToGIF: TMenuItem;
    miToggleLayoutPanel: TMenuItem;
    N53: TMenuItem;
    miClearLayer: TMenuItem;
    N54: TMenuItem;
    PageControl1: TPageControl;
    tsPalette: TTabSheet;
    tsGradients: TTabSheet;
    sbGradientBrush: TSpeedButton;
    miClearAllFrames: TMenuItem;
    panelRGBPalette: TPanel;
    sRGBPalette1: TShape;
    sRGBPalette2: TShape;
    sRGBPalette3: TShape;
    sRGBPalette4: TShape;
    sRGBPalette5: TShape;
    sRGBPalette6: TShape;
    sRGBPalette7: TShape;
    sRGBPalette8: TShape;
    sRGBPalette9: TShape;
    sRGBPalette10: TShape;
    sRGBPalette11: TShape;
    sRGBPalette12: TShape;
    sRGBPalette13: TShape;
    sRGBPalette14: TShape;
    sRGBPalette15: TShape;
    sRGBPalette16: TShape;
    Shape37: TShape;
    Shape38: TShape;
    Shape39: TShape;
    Shape40: TShape;
    Shape41: TShape;
    Shape42: TShape;
    Shape43: TShape;
    Shape44: TShape;
    Shape45: TShape;
    Shape46: TShape;
    sShade1: TShape;
    sShade2: TShape;
    sShade3: TShape;
    sShade4: TShape;
    sShade5: TShape;
    sShade6: TShape;
    sShade7: TShape;
    sShade8: TShape;
    sShade9: TShape;
    sShade10: TShape;
    sShade16: TShape;
    sShade15: TShape;
    sShade14: TShape;
    sShade13: TShape;
    sShade12: TShape;
    sShade11: TShape;
    pRGB_3BPP: TPanel;
    Shape47: TShape;
    Shape48: TShape;
    Shape49: TShape;
    Shape50: TShape;
    Shape51: TShape;
    Shape52: TShape;
    Shape53: TShape;
    Shape54: TShape;
    sbFloodFill: TSpeedButton;
    miMerge: TMenuItem;
    miChangeColoursLayer: TMenuItem;
    Bevel20: TBevel;
    N55: TMenuItem;
    miReopenMenu: TMenuItem;
    sbPatternSpiral: TSpeedButton;
    sbPatternCircle: TSpeedButton;
    sbPatternSplitRing: TSpeedButton;
    sbPatternPetals: TSpeedButton;
    sbPatternGrid: TSpeedButton;
    sbPatternPyramid: TSpeedButton;
    sbPatternRightTriangle: TSpeedButton;
    sbPatternLeftTriangle: TSpeedButton;
    lPixelColour: TLabel;
    sSelectionRMB: TShape;
    lColoursRight: TLabel;
    iMMBGradient: TImage;
    lColoursMiddle: TLabel;
    sSelectionLMB: TShape;
    lColoursLeft: TLabel;
    sColour0: TShape;
    sColour1: TShape;
    lBackground: TLabel;
    sColour3: TShape;
    sColour2: TShape;
    Bevel12: TBevel;
    sSelectionMMB: TShape;
    lSelectedTool: TLabel;
    miPaletteGradientToolbar: TMenuItem;
    miUndoToolbar: TMenuItem;
    pUndoToolbar: TPanel;
    miSetIgnoredFromPattern: TMenuItem;
    N56: TMenuItem;
    Savepattern1: TMenuItem;
    Loadpattern1: TMenuItem;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    N57: TMenuItem;
    Showshortcutkeys1: TMenuItem;
    Draw1: TMenuItem;
    miMouseMode: TMenuItem;
    miNewBrush: TMenuItem;
    miDrawCopy: TMenuItem;
    miFilledRectangle: TMenuItem;
    miFrame: TMenuItem;
    miFilledCircle: TMenuItem;
    miEmptyCircle: TMenuItem;
    miLine: TMenuItem;
    miMultiDraw: TMenuItem;
    miFloodFill: TMenuItem;
    miFont: TMenuItem;
    miGradientBrush: TMenuItem;
    miGradient: TMenuItem;
    miRandomDraw: TMenuItem;
    miPicker: TMenuItem;
    N58: TMenuItem;
    N59: TMenuItem;
    N60: TMenuItem;
    miPatternSpiral: TMenuItem;
    miPatternCircle: TMenuItem;
    miPatternSplitRing: TMenuItem;
    miPatternPetals: TMenuItem;
    miPatternGrid: TMenuItem;
    miPatternLeftTriangle: TMenuItem;
    miPatternRightTriangle: TMenuItem;
    miPatternPyramid: TMenuItem;
    Frames1: TMenuItem;
    miAddFrame: TMenuItem;
    miAddFrameCopy: TMenuItem;
    miAddFrameMultiple: TMenuItem;
    N61: TMenuItem;
    miDeleteFrame: TMenuItem;
    miDeleteMultipleFrames: TMenuItem;
    miPixelMegaUltra: TMenuItem;
    puMainCanvas: TPopupMenu;
    Workingareabackgroundcolour1: TMenuItem;
    White2: TMenuItem;
    Red2: TMenuItem;
    Purple2: TMenuItem;
    Green2: TMenuItem;
    Grey2: TMenuItem;
    Darkgreydefault1: TMenuItem;
    Black2: TMenuItem;
    N1: TMenuItem;
    Custom1: TMenuItem;
    miPreviewView: TMenuItem;
    miLanguage: TMenuItem;
    N24: TMenuItem;
    pQuickData: TPanel;
    miQuickData: TMenuItem;
    miGradientBottomTop: TMenuItem;
    procedure sbBuildClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbScrollLeftClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbUndoClick(Sender: TObject);
    procedure miMemory1Click(Sender: TObject);
    procedure bPlayAnimationClick(Sender: TObject);
    procedure timerAnimateTimer(Sender: TObject);
    procedure miMemoryR1Click(Sender: TObject);
    procedure miShowAnimationToolbarClick(Sender: TObject);
    procedure miClearAllUserMemoriesClick(Sender: TObject);
    procedure sbMirrorClick(Sender: TObject);
    procedure sbRotateLClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure bAddFrameClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure SetButtonImage(button : TBitbtn; imageidx : integer);
    procedure bLightboxClick(Sender: TObject);
    procedure miSaveSingleFrameClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure DisplayFrame(frameno: integer);
    procedure miSaveClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure bDeleteFrameClick(Sender: TObject);
    procedure LoadPreset(filename : string);
    procedure miPresetSaveCurrentClick(Sender: TObject);
    procedure sbPresetClick(Sender: TObject);
    procedure BuildPresetList;
    procedure BuildGradientList;
    procedure BuildFontList;
    procedure SelectPreset(Sender : TObject);
    procedure SelectFont(Sender : TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure miPixelTinyClick(Sender: TObject);
    procedure sbPixelSizeClick(Sender: TObject);
    procedure sbMouseModeClick(Sender: TObject);
    procedure bAddFrameCopyClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure miFontModeClick(Sender: TObject);
    procedure SetFrameCaption(i : integer);
    procedure UpdateMemoryUsage;
    procedure miCopyFromPreviousClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miSaveAsFontClick(Sender: TObject);
    procedure tbFramesChange(Sender: TObject);
    procedure Checkforupdates1Click(Sender: TObject);
    procedure sbRotateAnyClick(Sender: TObject);
    procedure Website1Click(Sender: TObject);
    procedure SetPlaybackcustom(aValue : integer);
    procedure miPlaybackSpeed3Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure miImportInToCurrentClick(Sender: TObject);
    procedure miFlipAllFramesClick(Sender: TObject);
    procedure miMirrorAllFramesClick(Sender: TObject);
    procedure miInvertAllFramesClick(Sender: TObject);
    procedure bAddFrameMultipleClick(Sender: TObject);
    procedure RecalculatePadding;
    procedure miGridToggleClick(Sender: TObject);
    procedure miAutoSaveClick(Sender: TObject);
    procedure miAutosave2Click(Sender: TObject);
    procedure timerAutosaveTimer(Sender: TObject);
    procedure Importfrombitmap1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
    procedure sColour3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbMatrixTypeChange(Sender: TObject);
    procedure SetupMatrixColours;
    procedure Examples1Click(Sender: TObject);
    procedure sbGradientClick(Sender: TObject);
    procedure OnGradientClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToggleGradient(aGradientMode : TGradientOption; cleargradient : boolean);
    procedure Colour01Click(Sender: TObject);
    procedure SelectGradient(Sender : TObject);
    procedure miSaveGradientClick(Sender: TObject);
    procedure miGradientAllFramesClick(Sender: TObject);
    procedure sbPixelShapeClick(Sender: TObject);
    procedure miPixelShapeSquareClick(Sender: TObject);
    procedure miASCIIStartCodeClick(Sender: TObject);
    procedure miExportClick(Sender: TObject);
    procedure miExportUserMemoriesClick(Sender: TObject);
    procedure miPreviewClick(Sender: TObject);
    procedure miPreviewx1Click(Sender: TObject);
    procedure Donate1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miAddCommentClick(Sender: TObject);
    procedure Small1Click(Sender: TObject);
    procedure miFontViewerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miClearAllFramesLayerClick(Sender: TObject);
    procedure miAppendClick(Sender: TObject);
    procedure Openautosavefolder1Click(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
    procedure miCodeTemplatesClick(Sender: TObject);
    procedure miGradFromClick(Sender: TObject);
    procedure miGradientSelectRGBClick(Sender: TObject);
    procedure miClearAllFramesGradientClick(Sender: TObject);
    procedure miRandomnessTinyClick(Sender: TObject);
    procedure miSetDeadPixelsClick(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    function  GetAutoPixelSize: integer;

    procedure MatrixOnChange(Sender : TObject);
    procedure MatrixOnLayerChange(Sender : TObject);
    procedure MatrixOnSizeChange(Sender : TObject);
    procedure MatrixOnDisplayBufferCopied(Sender : TObject);
    procedure MatrixOnNewFrameDisplayed(Sender : TObject);
    procedure MatrixOnColourChange(Sender : TObject);
    procedure MatrixOnMouseOver(const  x, y : integer);
    procedure MatrixOnPreviewMouseDown(const x, y : integer);
    procedure MatrixOnDebug(const s : string);
    procedure SetPreview(aSize : integer; aView : TViewShape; aVoid : integer; aOffSet : integer; aDirection, aPopout : boolean);
    procedure miFadeFirstLastClick(Sender: TObject);
    procedure miExportToBitmapClick(Sender: TObject);

    procedure ManageUIControls(aOverride : boolean; aSetTo : boolean);
    procedure UpdateDisplay(aNewFramePosition : integer);
    procedure puGradientShapePopup(Sender: TObject);
    procedure miGradSetRowClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miAutomateClick(Sender: TObject);
    procedure Copyandshiftleft1Click(Sender: TObject);
    procedure Setcustomspeed1Click(Sender: TObject);
    procedure SetSimpleExport(aTEO : TExportOptions);
    procedure miPreviewViewSquareClick(Sender: TObject);
    procedure miPreviewVoid10Click(Sender: TObject);
    procedure Optimisedata1Click(Sender: TObject);
    procedure sRGBPalette1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miRGBPaletteToolbarClick(Sender: TObject);
    procedure Black1Click(Sender: TObject);
    procedure miCustomBackgroundClick(Sender: TObject);
    procedure SyncPreviewSize(aSize : integer);
    procedure SyncPreviewView(aView : integer);
    procedure SyncPreviewVoid(aVoid : integer);

    procedure SystemSetBackgroundColour(aNewColour : integer);
    procedure miClearAllDeadPixelsClick(Sender: TObject);
    procedure sRGBPaletteColourMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sRGBPalette1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Rotateanticlockwise1Click(Sender: TObject);
    procedure Rotateclockwise1Click(Sender: TObject);
    procedure miBrushFlipClick(Sender: TObject);
    procedure Pasteintoeveryframe1Click(Sender: TObject);
    procedure Pasteintoeveryframetransparent1Click(Sender: TObject);
    procedure sbNewBrushClick(Sender: TObject);
    procedure witter1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miExportAnimationToBitmapClick(Sender: TObject);
    procedure miCopyMultipleClick(Sender: TObject);
    procedure bDeleteMultipleFramesClick(Sender: TObject);
    procedure miRadialOffset45Click(Sender: TObject);
    procedure miPreviewOffsetReverseClick(Sender: TObject);
    procedure Shape16MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miSaveRangeClick(Sender: TObject);
    procedure miPopoutPreviewClick(Sender: TObject);
    procedure bLockFrameClick(Sender: TObject);
    procedure miMergeBottomClick(Sender: TObject);
    procedure miUnlockAllClick(Sender: TObject);
    procedure miLockAllClick(Sender: TObject);
    procedure miToggleLockStatusClick(Sender: TObject);
    procedure miIncrementRadiallyClick(Sender: TObject);
    procedure miChangeColoursFrameClick(Sender: TObject);
    procedure cbMirrorModeChange(Sender: TObject);
    procedure ImportfromGIF1Click(Sender: TObject);
    procedure pCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Shape47MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miGradientRGB3BPP1Click(Sender: TObject);
    procedure Currentframe1Click(Sender: TObject);
    procedure Animation1Click(Sender: TObject);
    procedure miExportToGIFClick(Sender: TObject);
    procedure miToggleLayoutPanelClick(Sender: TObject);
    procedure miFlattenLayersClick(Sender: TObject);
    procedure miClearLayerClick(Sender: TObject);
    procedure miClearAllFramesClick(Sender: TObject);
    procedure miPreviousFrameClick(Sender: TObject);
    procedure miNextFrameClick(Sender: TObject);
    procedure miPaletteGradientToolbarClick(Sender: TObject);
    procedure miUndoToolbarClick(Sender: TObject);
    procedure miSetIgnoredFromPatternClick(Sender: TObject);
    procedure Savepattern1Click(Sender: TObject);
    procedure Loadpattern1Click(Sender: TObject);
    procedure miShiftLeftClick(Sender: TObject);
    procedure Showshortcutkeys1Click(Sender: TObject);
    procedure pCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miQuickDataClick(Sender: TObject);
    procedure miGradientBottomTopClick(Sender: TObject);
  public
    procedure PreviewWindowChangeFrame(aNewFrame : integer);
    procedure PreviewWindowCommand(aCommandID : integer);
    procedure OnPopoutClosed(Sender: TObject; var Action: TCloseAction);
    procedure OnLayerPanelClose(Sender : TObject);
    procedure OnLayerMenuItem(aItem : integer);
    procedure OnUndoSelected(aUndo : integer);
    procedure QuickDataChange(Sender : TObject);

    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
  private
    MatrixGradient : array[0.._MaxHeight] of TShape;

    FActionObject : TActionObject; // used to store users interactions with automation dialog

    FOldMouseX : integer;
    FOldMouseY : integer;

    OldMatrixMode : TMatrixMode;

    BackupCaption  : string;

    FLastTick : cardinal;

    FFileHistoryMenus : TObjectList<TMenuItem>;

    FFrameGradientPanel : TframeGradient;
    FFramePalettePanel  : TframePalette;
    FFrameFontPanel     : TframeFont;
    FFrameLayerPanel    : TframeLayers;
    FFrameUndoPanel     : TframeUndos;
    FFrameQuickData     : TframeSimpleExport;

    MenuCopyMemory    : array[0..9] of TMenuItem;
    MenuRestoryMemory : array[0..9] of TMenuItem;
    PreviewMenuSize   : array[0..1, 0..14] of TMenuItem;
    PreviewMenuView   : array[0..1, 0..4] of TMenuItem;
    PreviewMenuVoid   : array[0..1, 0..6] of TMenuItem;

    RGBPalette        : array[0..15] of TShape;
    RGBShade          : array[0..15] of TShape;

    procedure SetGUILanguageText;

    procedure InitFrames;

    procedure BuildReOpenMenu;
    procedure BuildLanguageMenu;
    procedure ReopenClick(Sender: TObject);
    procedure LanguageClick(Sender: TObject);

    procedure ClearCurrentProjectFileName;
    procedure SetCurrentProjectFileName(aFileName : string);

    function  AppendFromFileName(aFileName : string): boolean;
    function  MergeFromFileName(aFileName : string; aStartFrame : integer; aMergeMode : TLoadMode): boolean;
    function  LoadFromFileName(aFilename : string): boolean;
    function  LoadFromGIF(aFilename : string): boolean;
    procedure SetFromSettings;

    procedure LoadWithWarnings(aFileName : string);

    procedure PlaybackCommand(aCommandID : integer);
    procedure PlaybackStart;
    procedure PlaybackStop;
    procedure PlaybackFirstFrame;
    procedure PlaybackPreviousFrame;
    procedure PlaybackNextFrame;
    procedure PlaybackLastFrame;

    procedure UpdateGradientColours;

    procedure GenerateShades(aColour : integer);

    function  BuildImportData(aStartFrame, aEndFrame : integer): TImportData;

    procedure CopyToGradientBrush(Sender : TObject);
    procedure CopyFromCustom(Sender : TObject);
    procedure CopyFromShades(Sender : TObject);
    procedure PaletteColourSelected(aMouseButton, aColour : integer);
    procedure PaletteColourOver(aColour : integer);

    function  GetColours: TColours;

    procedure ConfigureOpenDialog(aMode : integer);
    procedure ConfigureSaveDialog(aMode : integer);

    procedure UpdateData;
  end;


var
  frmMain: TfrmMain;


implementation


{$R *.dfm}


uses ShellAPI,
     formAbout, formCheckVersion, formImportBitmap, formExport,
     formFontViewer, math, formNewProject, formExportCode, formPreferences, layer,

     drawingdata;


const
  CAnimPlayStart     = 0;
  CAnimPlayStop      = 1;
  CAnimFirstFrame    = 2;
  CAnimPreviousFrame = 3;
  CAnimNextFrame     = 4;
  CAnimLastFrame     = 5;

  CLoadProject       = 0;
  CLoadIgnorePixels  = 1;

  CSaveProject       = 0;
  CSaveFont          = 1;
  CSaveIgnorePixels  = 2;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  x : integer;

begin
  DragAcceptFiles(Handle, True);

  Caption                  := Caption + ' ' + LEDStudioVersion;
  BackupCaption            := Caption;
  FLastTick                := GetTickCount();
  DoubleBuffered           := True;

  FFileHistoryMenus        := TObjectList<TMenuItem>.Create;
  FFileHistoryMenus.OwnsObjects := True;

  // ===========================================================================

  TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);

  // ===========================================================================

  FActionObject := TActionObject.Create;

  MatrixMain := TTheMatrix.Create(Self, pCanvas);
  MatrixMain.OnChange              := MatrixOnChange;
  MatrixMain.OnLayerChange         := MatrixOnLayerChange;
  MatrixMain.OnSizeChange          := MatrixOnSizeChange;
  MatrixMain.OnDisplayBufferCopied := MatrixOnDisplayBufferCopied;
  MatrixMain.OnNewFrameDisplayed   := MatrixOnNewFrameDisplayed;
  MatrixMain.OnColourChange        := MatrixOnColourChange;
  MatrixMain.OnMouseOver           := MatrixOnMouseOver;
  MatrixMain.OnPreviewMouseDown    := MatrixOnPreviewMouseDown;
  MatrixMain.OnDebug               := MatrixOnDebug;

  sbCopy.Tag                       := Ord(dmCopy);
  sbFilledRectangle.Tag            := Ord(dmFilledBox);
  sbFrame.Tag                      := Ord(dmEmptyBox);
  sbEmptyCircle.Tag                := Ord(dmEmptyCircle);
  sbFilledCircle.Tag               := Ord(dmFilledCircle);
  sbLine.Tag                       := Ord(dmLine);
  sbFont.Tag                       := Ord(dmFont);
  sbGradientBrush.Tag              := Ord(dmGradientBrush);
  sbMultiDraw.Tag                  := Ord(dmMulti);
  sbFloodFill.Tag                  := Ord(dmFloodFill);
  sbRandomDraw.Tag                 := Ord(dmRandom);
  sbPicker.Tag                     := Ord(dmPicker);
  
  // ===========================================================================

  MenuCopyMemory[0]:=miMemory1; MenuCopyMemory[1]:=miMemory2; MenuCopyMemory[2]:=miMemory3; MenuCopyMemory[3]:=miMemory4; MenuCopyMemory[4]:=miMemory5;
  MenuCopyMemory[5]:=miMemory6; MenuCopyMemory[6]:=miMemory7; MenuCopyMemory[7]:=miMemory8; MenuCopyMemory[8]:=miMemory9; MenuCopyMemory[9]:=miMemory10;

  MenuRestoryMemory[0]:=miMemoryR1; MenuRestoryMemory[1]:=miMemoryR2; MenuRestoryMemory[2]:=miMemoryR3; MenuRestoryMemory[3]:=miMemoryR4;
  MenuRestoryMemory[4]:=miMemoryR5; MenuRestoryMemory[5]:=miMemoryR6; MenuRestoryMemory[6]:=miMemoryR7; MenuRestoryMemory[7]:=miMemoryR8;
  MenuRestoryMemory[8]:=miMemoryR9; MenuRestoryMemory[9]:=miMemoryR10;

  RGBPalette[0]  := sRGBPalette1; RGBPalette[1]  := sRGBPalette2; RGBPalette[2]  := sRGBPalette3; RGBPalette[3]  := sRGBPalette4;
  RGBPalette[4]  := sRGBPalette5; RGBPalette[5]  := sRGBPalette6; RGBPalette[6]  := sRGBPalette7; RGBPalette[7]  := sRGBPalette8;
  RGBPalette[8]  := sRGBPalette9; RGBPalette[9]  := sRGBPalette10; RGBPalette[10] := sRGBPalette11; RGBPalette[11] := sRGBPalette12;
  RGBPalette[12] := sRGBPalette13; RGBPalette[13] := sRGBPalette14; RGBPalette[14] := sRGBPalette15; RGBPalette[15] := sRGBPalette16;

  RGBShade[0] := sShade1; RGBShade[1] := sShade2;  RGBShade[2]  := sShade3;  RGBShade[3]  := sShade4;  RGBShade[4]  := sShade5;  RGBShade[5]  := sShade6;  RGBShade[6]  := sShade7;  RGBShade[7] := sShade8;
  RGBShade[8] := sShade9; RGBShade[9] := sShade10; RGBShade[10] := sShade11; RGBShade[11] := sShade12; RGBShade[12] := sShade13; RGBShade[13] := sShade14; RGBShade[14] := sShade15; RGBShade[15] := sShade16;

  PreviewMenuSize[0, 0] := miPreviewx1;   PreviewMenuSize[0, 1] := miPreviewx2;   PreviewMenuSize[0, 2] := miPreviewx3;   PreviewMenuSize[0, 3] := miPreviewx4;
  PreviewMenuSize[0, 4] := miPreviewx5;   PreviewMenuSize[0, 5] := miPreviewx6;   PreviewMenuSize[0, 6] := miPreviewx8;   PreviewMenuSize[0, 7] := miPreviewx10;
  PreviewMenuSize[0, 8] := miPreviewx12;  PreviewMenuSize[0, 9] := miPreviewx15;  PreviewMenuSize[0, 10] := miPreviewx20; PreviewMenuSize[0, 11] := miPreviewx25;
  PreviewMenuSize[0, 12] := miPreviewx30; PreviewMenuSize[0, 13] := miPreviewx40; PreviewMenuSize[0, 14] := miPreviewx50;

  PreviewMenuSize[1, 0] := miPUPreviewx1; PreviewMenuSize[1, 1] := miPUPreviewx2; PreviewMenuSize[1, 2] := miPUPreviewx3; PreviewMenuSize[1, 3] := miPUPreviewx4;
  PreviewMenuSize[1, 4] := miPUPreviewx5; PreviewMenuSize[1, 5] := miPUPreviewx6; PreviewMenuSize[1, 6] := miPUPreviewx8; PreviewMenuSize[1, 7] := miPUPreviewx10;
  PreviewMenuSize[1, 8] := miPUPreviewx12; PreviewMenuSize[1, 9] := miPUPreviewx15; PreviewMenuSize[1, 10] := miPUPreviewx20; PreviewMenuSize[1, 11] := miPUPreviewx25;
  PreviewMenuSize[1, 12] := miPUPreviewx30; PreviewMenuSize[1, 13] := miPUPreviewx40; PreviewMenuSize[1, 14] := miPUPreviewx50;

  PreviewMenuView[0, 0] := miPreviewViewSquare; PreviewMenuView[0, 1] := miPreviewViewRadial; PreviewMenuView[0, 2] := miPreviewViewRadialTQ; PreviewMenuView[0, 3] := miPreviewViewSemiCircle; PreviewMenuView[0, 4] := miPreviewViewSemiCircleInverted;
  PreviewMenuView[1, 0] := miPUPreviewViewSquare; PreviewMenuView[1, 1] := miPUPreviewViewRadial; PreviewMenuView[1, 2] := miPUPreviewViewRadialTQ; PreviewMenuView[1, 3] := miPUPreviewViewSemiCircle; PreviewMenuView[1, 4] := miPUPreviewViewSemiCircleInverted;

  PreviewMenuVoid[0, 0] := miPUPreviewVoid10; PreviewMenuVoid[0, 1] := miPUPreviewVoid15; PreviewMenuVoid[0, 2] := miPUPreviewVoid20; PreviewMenuVoid[0, 3] := miPUPreviewVoid25;
  PreviewMenuVoid[0, 4] := miPUPreviewVoid30; PreviewMenuVoid[0, 5] := miPUPreviewVoid40; PreviewMenuVoid[0, 6] := miPUPreviewVoid50;
  PreviewMenuVoid[1, 0] := miPreviewVoid10; PreviewMenuVoid[1, 1] := miPreviewVoid15; PreviewMenuVoid[1, 2] := miPreviewVoid20; PreviewMenuVoid[1, 3] := miPreviewVoid25;
  PreviewMenuVoid[1, 4] := miPreviewVoid30; PreviewMenuVoid[1, 5] := miPreviewVoid40; PreviewMenuVoid[1, 6] := miPreviewVoid50;

  // ===========================================================================

  SetGUILanguageText;

  // ===========================================================================

  cbRotateAngle.Items.Add('5°');
  cbRotateAngle.Items.Add('10°');
  cbRotateAngle.Items.Add('15°');
  cbRotateAngle.Items.Add('18°');
  cbRotateAngle.Items.Add('20°');
  cbRotateAngle.Items.Add('22.5°');
  cbRotateAngle.Items.Add('25°');
  cbRotateAngle.Items.Add('30°');
  cbRotateAngle.Items.Add('35°');
  cbRotateAngle.Items.Add('36°');
  cbRotateAngle.Items.Add('40°');
  cbRotateAngle.Items.Add('45°');
  cbRotateAngle.Items.Add('50°');
  cbRotateAngle.Items.Add('55°');
  cbRotateAngle.Items.Add('60°');
  cbRotateAngle.Items.Add('65°');
  cbRotateAngle.Items.Add('67.5°');
  cbRotateAngle.Items.Add('70°');
  cbRotateAngle.Items.Add('72°');
  cbRotateAngle.Items.Add('75°');
  cbRotateAngle.Items.Add('80°');
  cbRotateAngle.Items.Add('85°');
  cbRotateAngle.Items.Add('90°');
  cbRotateAngle.ItemIndex := 0;

  for x := 1 to 72 do
    cbRotateCount.Items.Add(IntToStr(x));

  cbRotateCount.ItemIndex := 0;

  // ===========================================================================

  pRGB_3BPP.Left := 0;

  // ===========================================================================

  SetFromSettings;

  BuildReopenMenu;
  BuildLanguageMenu;

  // ===========================================================================

  SetButtonImage(bLightbox, 0);

  // ===========================================================================

  BuildPresetList;
  BuildGradientList;

  GenerateShades(sSelectionLMB.Brush.Color);

  // ===========================================================================

  BuildFontList;

  if FileExists((Application.ExeName) + 'fonts\5x7.ledsfont') then
    MatrixMain.LoadFont(ExtractFilePath(Application.ExeName) + 'fonts\5x7.ledsfont');

  // ===========================================================================

  InitFrames;

  // ===========================================================================

  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
      LoadFromFileName(ParamStr(1));
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFramePalettePanel.DeInit;
  FFramePalettePanel.Free;

  FFrameGradientPanel.Free;

  FFrameLayerPanel.Free;

  FFrameQuickData.Free;

  FActionObject.Free;

  FFileHistoryMenus.Free;

  LMSSettings.Free;
end;


procedure TfrmMain.AcceptFiles( var msg : TMessage );
const
  cnMaxFileNameLen = 255;

var
  lCount : integer;
  lFileName : array [0..cnMaxFileNameLen] of char;

begin
  lCount := DragQueryFile(msg.WParam,
                          $FFFFFFFF,
                          lFileName,
                          cnMaxFileNameLen );

  if (lCount <> 0) then begin
    DragQueryFile(msg.WParam, 0, lFileName, cnMaxFileNameLen );

    LoadWithWarnings(lFileName);
  end;

  DragFinish(msg.WParam);
end;


procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  lTick : cardinal;

begin
  lTick := GetTickCount();

  if (MatrixMain.Render.DrawData.Mode = dmFont) and (key = #8) then begin    // backspace, 1 column
    MatrixMain.DeleteFontCharacter(tbFrames.Position);
  end
  else if (MatrixMain.Render.DrawData.Mode = dmFont) and (ord(key) > 31) and (lTick - FLastTick >= 400) then begin
    FLastTick := lTick;

    MatrixMain.DrawFontCharacter(Ord(Key) - 32, tbFrames.Position);
  end
  else if (Key = #27) then begin
    MatrixMain.CancelDrawMode;

    sbMouseMode.Down := True;
  end
end;


procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  statusMain.SimpleText := '(c) Paul Alan Freshney :: ' + LEDStudioDate + ' :: www.MaximumOctopus.com';
  lPixelColour.Caption  := '';

  FOldMouseX := -1;
  FOldMouseY := -1;
end;


procedure TfrmMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  lSP : integer;

begin
  if (ssCtrl in Shift) then begin
    lSP := MatrixMain.ShapeParameter;

    if lSP > 1 then                                 // do not allow a value of one or some draw modes will run forever!
      MatrixMain.ShapeParameter := lSP - 1;
  end
  else begin
    if (tbFrames.Max <> 1) then begin
      if tbFrames.Position = 1 then
        tbFrames.Position := tbFrames.Max
      else
        tbFrames.Position := tbFrames.Position - 1;

      tbFramesChange(Nil);

      frmPreviewPopout.tbFrames.Position := tbFrames.Position;
    end;
  end;

  Handled := True;
end;


procedure TfrmMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  lSP : integer;

begin
  if (ssCtrl in Shift) then begin
    lSP := MatrixMain.ShapeParameter;

    MatrixMain.ShapeParameter := lSP + 1;
  end
  else begin
    if (tbFrames.Max <> 1) then begin
      if tbFrames.Position = tbFrames.Max then
        tbFrames.Position := 1
      else
        tbFrames.Position := tbFrames.Position + 1;

      tbFramesChange(Nil);

      frmPreviewPopout.tbFrames.Position := tbFrames.Position;
    end;
  end;

  Handled := True;
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin
  if (miPixelAuto.Checked) and (MatrixMain.Matrix.Available) then begin
    miPixelTinyClick(miPixelAuto);

    ToggleGradient(TGradientOption(sbGradient.Tag), False);
  end
  else if (MatrixMain.Matrix.Available) then
    MatrixMain.ChangeZoomUI(LMSSettings.PixelSize);

  MatrixMain.Refresh;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  t : integer;

begin
  OnResize := Nil;       // stops the resize firing after the matrix has been freed!

  // ===========================================================================

  for t := 0 to 5 do
    LMSSettings.LEDColoursSingle[t] := MatrixMain.LEDColoursSingle[t];

  for t := 0 to 5 do
    LMSSettings.LEDColoursBi[t] := MatrixMain.LEDColoursBi[t];

  LMSSettings.RGBBackground    := MatrixMain.RGBBackground;
  LMSSettings.LEDRGBColours[1] := MatrixMain.LEDRGBColours[CMouseLeft];
  LMSSettings.LEDRGBColours[2] := MatrixMain.LEDRGBColours[CMouseMiddle];
  LMSSettings.LEDRGBColours[3] := MatrixMain.LEDRGBColours[CMouseRight];

  if miPixelAuto.Checked then
    LMSSettings.PixelSize := CPixelSizeAuto;

  // ===========================================================================

  LMSSettings.SelectionColours[1] := sSelectionLMB.Tag;
  LMSSettings.SelectionColours[2] := sSelectionMMB.Tag;
  LMSSettings.SelectionColours[3] := sSelectionRMB.Tag;

  // ===========================================================================

  LMSSettings.Toolbars.Animation  := miShowAnimationToolbar.Checked;
  LMSSettings.Toolbars.RGBPalette := miPaletteGradientToolbar.Checked;

  LMSSettings.AnimSpeed           := timerAnimate.Interval;

  // ===========================================================================

  LMSSettings.AutoSaveEnabled  := miAutosave.Checked;

  // ===========================================================================

  LMSSettings.PreviewOptions.Enabled   := miPreview.Checked;
  LMSSettings.PreviewOptions.Size      := MatrixMain.PreviewBoxSize;
  LMSSettings.PreviewOptions.View      := MatrixMain.PreviewView;
  LMSSettings.PreviewOptions.Void      := MatrixMain.PreviewVoid;
  LMSSettings.PreviewOptions.Offset    := MatrixMain.RadialOffset;
  LMSSettings.PreviewOptions.Direction := MatrixMain.RadialOffsetDirection;

  // ===========================================================================

  for t := 0 to 15 do
    LMSSettings.RGBPalette[t] :=  RGBPalette[t].Brush.Color;

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================

  LMSSettings.SaveSettings;

  MatrixMain.Free;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if MatrixMain.AnimPlaying then begin
    if timerAnimate.Enabled then
      bPlayAnimationClick(bStopAnimation);

    CanClose := False;
  end
  else begin
    if sbClear.Enabled then begin
      if MessageDlg(GLanguageHandler.Text[kExitLMS] + #13#13 + GLanguageHandler.Text[kAreYouSure], mtWarning, [mbYes, mbNo], 0)=mrYes then
        CanClose := True
      else
        CanClose := False;
    end;
  end;
end;


procedure TfrmMain.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinWidth  := 713;
  MinHeight := 310;
end;


{$REGION setlanguagetext}

procedure TfrmMain.SetGUILanguageText;
begin
  DrawModes[0]  := GLanguageHandler.Text[kDraw];
  DrawModes[1]  := GLanguageHandler.Text[kFilledBox];
  DrawModes[2]  := GLanguageHandler.Text[kEmptyBox];
  DrawModes[3]  := GLanguageHandler.Text[kLine];
  DrawModes[4]  := GLanguageHandler.Text[kFont];
  DrawModes[5]  := GLanguageHandler.Text[kEmptyCircle];
  DrawModes[6]  := GLanguageHandler.Text[kFilledCircle];
  DrawModes[7]  := GLanguageHandler.Text[kRandomBrush];
  DrawModes[8]  := GLanguageHandler.Text[kMultiDraw];
  DrawModes[9]  := GLanguageHandler.Text[kColourPicker];
  DrawModes[10] := GLanguageHandler.Text[kCopyBrush];
  DrawModes[11] := GLanguageHandler.Text[kPasteBrush];
  DrawModes[12] := GLanguageHandler.Text[kGradientBrush];
  DrawModes[13] := GLanguageHandler.Text[kFloodFill];
  DrawModes[14] := GLanguageHandler.Text[kSpiral];
  DrawModes[15] := GLanguageHandler.Text[kRing];
  DrawModes[16] := GLanguageHandler.Text[kSplitRing];
  DrawModes[17] := GLanguageHandler.Text[kPetals];
  DrawModes[18] := GLanguageHandler.Text[kGrid];
  DrawModes[19] := GLanguageHandler.Text[kPyramid];
  DrawModes[20] := GLanguageHandler.Text[kLeftTriangle];
  DrawModes[21] := GLanguageHandler.Text[kRightTriangle];

  lSelectedTool.Caption := DrawModes[0];

  //

  sbBuild.Caption := GLanguageHandler.Text[kNew];
  sbOpen.Caption := GLanguageHandler.Text[kOpen];
  sbSave.Caption := GLanguageHandler.Text[kSave];
  sbExport.Caption := GLanguageHandler.Text[kExport];
  sbGenerateCode.Caption := GLanguageHandler.Text[kGenerateCode];
  sbPreset.Caption := GLanguageHandler.Text[kPreset];

  sbClear.Caption := GLanguageHandler.Text[kClear];
  sbMirror.Caption := GLanguageHandler.Text[kMirror];
  sbFlip.Caption := GLanguageHandler.Text[kFlip];
  sbInvert.Caption := GLanguageHandler.Text[kInvert];
  sbRotateAny.Caption := GLanguageHandler.Text[kRotate];

  cbMirrorMode.Items.Add(GLanguageHandler.Text[kNone]);
  cbMirrorMode.Items.Add(GLanguageHandler.Text[kHorizontal]);
  cbMirrorMode.Items.Add(GLanguageHandler.Text[kVertical]);
  cbMirrorMode.ItemIndex := 0;

  // popup menus

  Colour01.Caption := GLanguageHandler.Text[kColour] + ' 0';
  Colour11.Caption := GLanguageHandler.Text[kColour] + ' 1';
  Colour21.Caption := GLanguageHandler.Text[kColour] + ' 2';
  Colour31.Caption := GLanguageHandler.Text[kColour] + ' 3';

  MenuItem3.Caption := GLanguageHandler.Text[kGradients];
  miLoadGradients.Caption := GLanguageHandler.Text[kLoad];
  miSaveGradient.Caption := GLanguageHandler.Text[kSaveCurrent];

  MenuItem1.Caption := GLanguageHandler.Text[kFonts];
  miLoadFont.Caption := GLanguageHandler.Text[kLoad];
  Fontviewer2.Caption := GLanguageHandler.Text[kFontViewer];

  Presets1.Caption := GLanguageHandler.Text[kPresets];
  miLoadPreset.Caption := GLanguageHandler.Text[kLoad];
  miPresetSaveCurrent.Caption := GLanguageHandler.Text[kSaveCurrent];

  PixelSize1.Caption := GLanguageHandler.Text[kPixelSize];
  miPixelTiny.Caption := GLanguageHandler.Text[kTiny];
  miPixelSmall.Caption := GLanguageHandler.Text[kSmall];
  miPixelMedium.Caption := GLanguageHandler.Text[kMedium];
  miPixelLarge.Caption := GLanguageHandler.Text[kLarge];
  miPixelVeryLarge.Caption := GLanguageHandler.Text[kMassive];
  miPixelUltra.Caption := GLanguageHandler.Text[kUltra];
  miPixelMegaUltra.Caption := GLanguageHandler.Text[kXUltra];
  miPixelAuto.Caption := GLanguageHandler.Text[kAuto];

  Previewsize2.Caption := GLanguageHandler.Text[kPreviewSize];
  Previewview2.Caption := GLanguageHandler.Text[kPreviewView];
  miPUPreviewViewSquare.Caption := GLanguageHandler.Text[kSquare];
  miPUPreviewViewRadial.Caption := GLanguageHandler.Text[kRadial];
  miPUPreviewViewRadialTQ.Caption := GLanguageHandler.Text[kRadialThreeQuarters];
  miPUPreviewViewSemiCircle.Caption := GLanguageHandler.Text[kSemicircle];
  miPUPreviewViewSemiCircleInverted.Caption := GLanguageHandler.Text[kSemicircleIinverted];
  PreviewvoidRadialSemicircle1.Caption := GLanguageHandler.Text[kPreviewVoidRRadialSsemicircle];

  MenuItem5.Caption := GLanguageHandler.Text[kPixelShape];
  miPixelShapeSquare.Caption := GLanguageHandler.Text[kSquare];
  miPixelShapeRound.Caption := GLanguageHandler.Text[kRound];
  miPixelShapeRoundRect.Caption := GLanguageHandler.Text[kSquareR];

  MenuItem10.Caption := GLanguageHandler.Text[kRandomness];
  miRandomnessTiny.Caption := GLanguageHandler.Text[kTiny];
  Small2.Caption := GLanguageHandler.Text[kSmall];
  Medium1.Caption := GLanguageHandler.Text[kMedium];
  Large2.Caption := GLanguageHandler.Text[kLarge];
  Massive1.Caption := GLanguageHandler.Text[kMassive];

  MenuItem8.Caption := GLanguageHandler.Text[kBrushSize];
  Small1.Caption := GLanguageHandler.Text[kSmall] + ' (1x1)';
  Large1.Caption := GLanguageHandler.Text[kMedium] + ' (2x2)';
  Large3x3pixels1.Caption := GLanguageHandler.Text[kLarge] + ' (3x3)';

  miGradientSelectRGB.Caption := GLanguageHandler.Text[kSelectColour] + '...';
  miGradSetRow.Caption := GLanguageHandler.Text[kSetRowToSelectedColour] + '...';
  miGradFrom.Caption := GLanguageHandler.Text[kGradientFromTopBottom];
  miGradientBottomTop.Caption := GLanguageHandler.Text[kGradientFlip];

  Playbackspeed1.Caption := GLanguageHandler.Text[kPlaybackSpeed];
  miPlaybackSpeedCustom.Caption := GLanguageHandler.Text[kCustom];
  Setcustomspeed1.Caption := GLanguageHandler.Text[kSetCustomSpeed];

  //

  tsPalette.Caption := GLanguageHandler.Text[kPalette];
  tsGradients.Caption := GLanguageHandler.Text[kGradients];


  // main menu
  File1.Caption := GLanguageHandler.Text[kFile];
  New1.Caption := GLanguageHandler.Text[kNew];
  Load1.Caption := GLanguageHandler.Text[kOpen] + '...';
  miReopenMenu.Caption := GLanguageHandler.Text[kRecentFiles];
  Importfrombitmap1.Caption := GLanguageHandler.Text[kImportFromBitmap] + '...';
  ImportfromGIF1.Caption := GLanguageHandler.Text[kImportFromGIF] + '...';
  miImportInToCurrent.Caption := GLanguageHandler.Text[kImportIntoCurrentFrame];
  miAppend.Caption := GLanguageHandler.Text[kAppendToAnimation] + '...';
  miMerge.Caption := GLanguageHandler.Text[kMerge] + '...';
  miSave.Caption := GLanguageHandler.Text[kSave] + '...';
  miSaveAs.Caption := GLanguageHandler.Text[kSaveAs] + '...';
  miSaveSingleFrame.Caption := GLanguageHandler.Text[kSaveSingleFrameAs] + '...';
  miSaveRange.Caption := GLanguageHandler.Text[kSaveRangeAs] + '...';
  miSaveAsFont.Caption := GLanguageHandler.Text[kSaveLEDMatrixStudioFont] + '...';
  miExportToBitmap.Caption := GLanguageHandler.Text[kExportToImages] + '...';
  miExportAnimationToBitmap.Caption := GLanguageHandler.Text[kExportAnimationToBitmap] + '...';
  miExportToGIF.Caption := GLanguageHandler.Text[kExportAnimationToGIF] + '...';
  Preferences1.Caption := GLanguageHandler.Text[kPreferences] + '...';
  Exit1.Caption := GLanguageHandler.Text[kExit];
  //
  Edit1.Caption := GLanguageHandler.Text[kEdit];
  miUndo.Caption := GLanguageHandler.Text[kUndo];
  miRedo.Caption := GLanguageHandler.Text[kRedo];
  miCopy.Caption := GLanguageHandler.Text[kCopy];
  miCopyFromPrevious.Caption := GLanguageHandler.Text[kCopyFromPrevious];
  miCopyMultiple.Caption := GLanguageHandler.Text[kCopyMultiple] + '...';
  miPaste.Caption := GLanguageHandler.Text[kPaste];
  miPasteSpecial.Caption := GLanguageHandler.Text[kPasteSpecial];
  Copyandshiftleft1.Caption := GLanguageHandler.Text[kPasteShiftLeft];
  Copyandshiftright1.Caption := GLanguageHandler.Text[kPasteShiftRight];
  Copyandshiftup1.Caption := GLanguageHandler.Text[kPasteShiftUp];
  Copyandshiftdown1.Caption := GLanguageHandler.Text[kPasteShiftDown];
  miBrushActions.Caption := GLanguageHandler.Text[kBrushActions];
  Rotateanticlockwise1.Caption := GLanguageHandler.Text[kRotateAnticlockwise];
  Rotateclockwise1.Caption := GLanguageHandler.Text[kRotateClockwise];
  miBrushFlip.Caption := GLanguageHandler.Text[kFlip];
  Mirror1.Caption := GLanguageHandler.Text[kMirror];
  Invert1.Caption := GLanguageHandler.Text[kInvert];
  Pasteintoeveryframe1.Caption := GLanguageHandler.Text[kPasteEveryFrame];
  Pasteintoeveryframetransparent1.Caption := GLanguageHandler.Text[kPasteEveryFrameTransparent];
  miShiftLeft.Caption := GLanguageHandler.Text[kShiftLeft];
  miShiftRight.Caption := GLanguageHandler.Text[kShiftRight];
  miShiftUp.Caption := GLanguageHandler.Text[kShiftUp];
  miShiftDown.Caption := GLanguageHandler.Text[kShiftDown];
  miRotateL.Caption := GLanguageHandler.Text[kRotateAnticlockwise];
  miRotateR.Caption := GLanguageHandler.Text[kRotateClockwise];
  miFlip.Caption := GLanguageHandler.Text[kFlip];
  miMirror.Caption := GLanguageHandler.Text[kMirror];
  miInvert.Caption := GLanguageHandler.Text[kInvert];
  miAddComment.Caption := GLanguageHandler.Text[kEditComment];
  //
  View1.Caption := GLanguageHandler.Text[kView];
  miShowAnimationToolbar.Caption := GLanguageHandler.Text[kShowAnimationToolbar];
  miPaletteGradientToolbar.Caption := GLanguageHandler.Text[kPaletteGradientToolbar];
  miQuickData.Caption := GLanguageHandler.Text[kQuickDataToolbar];
  miUndoToolbar.Caption := GLanguageHandler.Text[kUndoToolbar];
  Backgroundcolour1.Caption := GLanguageHandler.Text[kWorkingAreaBackgroundColour];
  miCustomBackground.Caption := GLanguageHandler.Text[kCustom];
  Black1.Caption := GLanguageHandler.Text[kBlack];
  Darkgrey1.Caption := GLanguageHandler.Text[kDarkGreyDefault];
  Grey1.Caption := GLanguageHandler.Text[kGrey];
  Green1.Caption := GLanguageHandler.Text[kGreen];
  Purple1.Caption := GLanguageHandler.Text[kPurple];
  Red1.Caption := GLanguageHandler.Text[kRed];
  White1.Caption := GLanguageHandler.Text[kWhite];
  miFontMode.Caption := GLanguageHandler.Text[kFontMode];
  miASCIIStartCode.Caption := GLanguageHandler.Text[kChangeStartASCIICode];
  miPreviousFrame.Caption := GLanguageHandler.Text[kPreviousFrame];
  miNextFrame.Caption := GLanguageHandler.Text[kNextFrame];
  miGridToggle.Caption := GLanguageHandler.Text[kGrid];
  //
  Preview1.Caption := GLanguageHandler.Text[kPreview];
  miPreview.Caption := GLanguageHandler.Text[kPreview];
  PreviewSize1.Caption := GLanguageHandler.Text[kPreviewSize];
  miIncrementRadially.Caption := GLanguageHandler.Text[kIncrementRadially];
  miPreviewView.Caption := GLanguageHandler.Text[kPreviewView];
  miPreviewViewSquare.Caption := GLanguageHandler.Text[kSquare];
  miPreviewViewRadial.Caption := GLanguageHandler.Text[kRadial];
  miPreviewViewRadialTQ.Caption := GLanguageHandler.Text[kRadialThreeQuarters];
  miPreviewViewSemiCircle.Caption := GLanguageHandler.Text[kSemicircle];
  miPreviewViewSemiCircleInverted.Caption := GLanguageHandler.Text[kSemicircleIinverted];
  PreviewVoidRadial1.Caption := GLanguageHandler.Text[kPreviewVoidRRadialSsemicircle];
  Previewoffsetradialsemicircle1.Caption := GLanguageHandler.Text[kPreviewOffsetRadialSemicircle];
  miPreviewOffsetReverse.Caption := GLanguageHandler.Text[kReverse];
  miPopoutPreview.Caption := GLanguageHandler.Text[kPopoutPreview];
  //
  Project1.Caption := GLanguageHandler.Text[kProject];
  miClearAllFramesLayer.Caption := GLanguageHandler.Text[kClearAllFramesCurrentLayer];
  miClearAllFrames.Caption := GLanguageHandler.Text[kClearAllFramesAllLayers];
  miClearAllFramesGradient.Caption := GLanguageHandler.Text[kClearAllFramesWithGradient];
  miFlipAllFrames.Caption := GLanguageHandler.Text[kFlipAllFrames];
  miMirrorAllFrames.Caption := GLanguageHandler.Text[kMirrorAllFrames];
  miInvertAllFrames.Caption := GLanguageHandler.Text[kInvertAllFrames];
  miGradientAllFrames.Caption := GLanguageHandler.Text[kApplyGradientToAllFrames];
  miDeadPixels.Caption := GLanguageHandler.Text[kIgnoredPixels];
  miSetDeadPixels.Caption := GLanguageHandler.Text[kSetIgnoredPixels];
  miSetIgnoredFromPattern.Caption := GLanguageHandler.Text[kSetFromPattern] + '...';
  miClearAllDeadPixels.Caption := GLanguageHandler.Text[kClearAllIgnoredPixels];
  Savepattern1.Caption := GLanguageHandler.Text[kSavePattern] + '...';
  Loadpattern1.Caption := GLanguageHandler.Text[kLoadPattern] + '...';
  miFadeFirstLast.Caption := GLanguageHandler.Text[kFadeFirstLast];
  miExport.Caption := GLanguageHandler.Text[kExport] + '...';
  miCodeTemplates.Caption := GLanguageHandler.Text[kCodeTemplates] + '...';
  miUnlockAll.Caption := GLanguageHandler.Text[kUnlockAllFrames];
  miLockAll.Caption := GLanguageHandler.Text[kLockAllFrames];
  miToggleLockStatus.Caption := GLanguageHandler.Text[kToggleLockStatusRange] + '...';
  //
  Draw1.Caption := GLanguageHandler.Text[kDraw];
  miMouseMode.Caption := GLanguageHandler.Text[kFreehandBrush];
  miNewBrush.Caption := GLanguageHandler.Text[kCustomBrush];
  miDrawCopy.Caption := GLanguageHandler.Text[kCopyPaste];
  miFilledRectangle.Caption := GLanguageHandler.Text[kFilledRectangle];
  miFrame.Caption := GLanguageHandler.Text[kEmptyRectangle];
  miFilledCircle.Caption := GLanguageHandler.Text[kFilledCircle];
  miEmptyCircle.Caption := GLanguageHandler.Text[kEmptyCircle];
  miLine.Caption := GLanguageHandler.Text[kLine];
  miMultiDraw.Caption := GLanguageHandler.Text[kMultidrawOnEachFrame];
  miFloodFill.Caption := GLanguageHandler.Text[kFill];
  miFont.Caption := GLanguageHandler.Text[kText];
  miGradientBrush.Caption := GLanguageHandler.Text[kGradientBrush];
  miGradient.Caption := GLanguageHandler.Text[kGradient];
  miRandomDraw.Caption := GLanguageHandler.Text[kRandom];
  miPicker.Caption := GLanguageHandler.Text[kColourPicker];
  miPatternSpiral.Caption := GLanguageHandler.Text[kPatternSpiral];
  miPatternCircle.Caption := GLanguageHandler.Text[kPatternCircle];
  miPatternSplitRing.Caption := GLanguageHandler.Text[kPatternSplitRing];
  miPatternPetals.Caption := GLanguageHandler.Text[kPatternPetals];
  miPatternGrid.Caption := GLanguageHandler.Text[kPatternGrid];
  miPatternPyramid.Caption := GLanguageHandler.Text[kPatternPyramid];
  miPatternLeftTriangle.Caption := GLanguageHandler.Text[kPatternLeftTriangle];
  miPatternRightTriangle.Caption := GLanguageHandler.Text[kPatternRightTriangle];

  //
  Frames1.Caption := GLanguageHandler.Text[kFrames];
  miAddFrame.Caption := GLanguageHandler.Text[kAddFrame];
  miAddFrameCopy.Caption := GLanguageHandler.Text[kAddFrameCopy];
  miAddFrameMultiple.Caption := GLanguageHandler.Text[kAddFrameMultiple] + '...';
  miDeleteFrame.Caption := GLanguageHandler.Text[kDeleteFrame];
  miDeleteMultipleFrames.Caption := GLanguageHandler.Text[kDeleteMultipleFrames] + '...';
  //
  Layers1.Caption := GLanguageHandler.Text[kLayers];
  miToggleLayoutPanel.Caption := GLanguageHandler.Text[kToggleLayoutPanel];
  miClearLayer.Caption := GLanguageHandler.Text[kClearLayerAllFrames];
  miFlattenLayers.Caption := GLanguageHandler.Text[kFlattenAllLayers];

  //
  Colours1.Caption := GLanguageHandler.Text[kColours];
  miChangeColoursFrame.Caption := GLanguageHandler.Text[kChangeColoursInTheFrameLayer] + '...';
  miChangeColoursLayer.Caption := GLanguageHandler.Text[kChangeColoursGloballyCurrentLayer] + '...';
  miChangeColoursAll.Caption := GLanguageHandler.Text[kChangeColoursGloballyAllLayersFrames] + '...';
  miCountColours.Caption := GLanguageHandler.Text[kCountColours];
  Currentframe1.Caption := GLanguageHandler.Text[kCurrentFrame];
  Animation1.Caption := GLanguageHandler.Text[kAnimation];
  //
  Buffer1.Caption := GLanguageHandler.Text[kMemories];
  miCopyCurrentTo.Caption := GLanguageHandler.Text[kCopyCurrentTo];
  miRestoreCurrentFrom.Caption := GLanguageHandler.Text[kRestoreCurrentFrom];
  miExportUserMemories.Caption := GLanguageHandler.Text[kExportUserMemories];
  miClearAllUserMemories.Caption := GLanguageHandler.Text[kClearAllUserMemories];
  //
  ools1.Caption := GLanguageHandler.Text[kTools];
  miAutoSave.Caption := GLanguageHandler.Text[kAutosave];
  Autosaveinterval1.Caption := GLanguageHandler.Text[kAutosaveInterval];
  miAutosave2.Caption := '2 ' + GLanguageHandler.Text[kMinutes];
  miAutosave5.Caption := '5 ' + GLanguageHandler.Text[kMinutes];
  miAutosave10.Caption := '10 ' + GLanguageHandler.Text[kMinutes];
  Openautosavefolder1.Caption := GLanguageHandler.Text[kOpenAutosaveFolder];
  miAutomate.Caption := GLanguageHandler.Text[kAutomate];
  Optimisedata1.Caption := GLanguageHandler.Text[kOptimiseData];
  miFontViewer.Caption := GLanguageHandler.Text[kFontViewer] + '...';
  //
  About1.Caption := GLanguageHandler.Text[kAbout];
  Help1.Caption := GLanguageHandler.Text[kHelp] + '...';
  Showshortcutkeys1.Caption := GLanguageHandler.Text[kShowShortcutKeys];
  miLanguage.Caption := GLanguageHandler.Text[kLanguage];
  Examples1.Caption := GLanguageHandler.Text[kExampleCode] + '...';
  Checkforupdates1.Caption := GLanguageHandler.Text[kCheckForUpdates] + '...';
  Website1.Caption := GLanguageHandler.Text[kWebsite];
  miAbout.Caption := GLanguageHandler.Text[kAbout] + ' :)';
end;

{$ENDREGION}


procedure TfrmMain.InitFrames;
begin
  FFrameGradientPanel := TframeGradient.Create(Self);
  FFrameGradientPanel.Parent       := tsGradients;
  FFrameGradientPanel.Align        := alClient;
  FFrameGradientPanel.OnCopy       := CopyToGradientBrush;
  FFrameGradientPanel.OnFromCustom := CopyFromCustom;
  FFrameGradientPanel.OnFromShades := CopyFromShades;
  FFrameGradientPanel.SetGUILanguageText;

  FFramePalettePanel := TframePalette.Create(Self);
  FFramePalettePanel.Parent        := tsPalette;
  FFramePalettePanel.Align         := alClient;
  FFramePalettePanel.OnColourClick := PaletteColourSelected;
  FFramePalettePanel.OnColourMove  := PaletteColourOver;

  FFramePalettePanel.Init;

  FFrameFontPanel := TframeFont.Create(Self);
  FFrameFontPanel.Parent  := pbFont;
  FFrameFontPanel.Align   := alClient;

  FFrameLayerPanel := TframeLayers.Create(Self);
  FFrameLayerPanel.Parent  := pLayers;
  FFrameLayerPanel.Align   := alClient;
  FFrameLayerPanel.OnClose := OnLayerPanelClose;
  FFrameLayerPanel.OnMenu  := OnLayerMenuItem;
  FFrameLayerPanel.SetGUILanguageText;

  FFrameUndoPanel := TframeUndos.Create(Self);
  FFrameUndoPanel.Parent         := pUndoToolbar;
  FFrameUndoPanel.Align          := alClient;
  FFrameUndoPanel.OnUndoSelected := OnUndoSelected;

  FFrameQuickData := TframeSimpleExport.Create(Self);
  FFrameQuickData.Parent         := pQuickData;
  FFrameQuickData.Align          := alClient;
  FFrameQuickData.OnChange       := QuickDataChange;
  FFrameQuickData.SetGUILanguageText;
end;


// =============================================================================


procedure TfrmMain.BuildReOpenMenu;
var
  t : integer;
  lMenuItem : TMenuItem;

begin
  if LMSSettings.FileHistory.Count = 0 then Exit;

  FFileHistoryMenus.Clear;
  miReopenMenu.Clear;

  for t := 0 to LMSSettings.FileHistory.Count - 1 do begin
    lMenuItem := TMenuItem.Create(miReopenMenu);
    lMenuItem.Caption := LMSSettings.FileHistory[t];
    lMenuItem.Tag     := t;
    lMenuItem.OnClick := ReopenClick;

    miReopenMenu.Add(lMenuItem);

    FFileHistoryMenus.Add(lMenuItem);
  end;
end;


procedure TfrmMain.BuildLanguageMenu;
var
  lMenuItem : TMenuItem;
  lSearchResult : TSearchRec;

begin
  miLanguage.Clear;

  if FindFirst(ExtractFilePath(Application.ExeName) + 'language\*.txt', faAnyFile, lSearchResult) = 0 then begin
    repeat
      if (lSearchResult.Name <> '.') and (lSearchResult.Name <> '..')  then begin

         lMenuItem := TMenuItem.Create(miLanguage);
         lMenuItem.Caption   := TUtility.GetFileNameNoExt(lSearchResult.Name);
         lMenuItem.RadioItem := True;
         lMenuItem.AutoCheck := True;
         lMenuItem.Tag       := 0;
         lMenuItem.OnClick   := LanguageClick;

         miLanguage.Add(lMenuItem);

         if (lMenuItem.Caption = LMSSettings.App.Language) then
           lMenuItem.Checked := True;
       end;
    until FindNext(lSearchResult) <> 0;

    FindClose(lSearchResult);
  end;
end;


procedure TfrmMain.ClearCurrentProjectFileName;
begin
  LMSSettings.App.DataFilename := '';
  Caption                      := BackupCaption;
end;


procedure TfrmMain.miClearLayerClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kClearAllFramesQ] + #13#13 +
                GLanguageHandler.Text[kDoYouWishToContinue], mtWarning, [mbYes, mbNo], 0) = mrYes then
    MatrixMain.ClearCurrentLayerAllFrames;
end;


procedure TfrmMain.SetCurrentProjectFileName(aFileName : string);
begin
  LMSSettings.App.DataFilename     := aFileName;
  Caption                          := BackupCaption + '  [ ' + ExtractFilename(LMSSettings.App.DataFilename) + ' ]';

  LMSSettings.App.LastSaveLocation := ExtractFilePath(aFileName);

  if LMSSettings.FileHistory.IndexOf(aFileName) = -1 then begin
    LMSSettings.FileHistory.Insert(0, aFileName);

    if (LMSSettings.FileHistory.Count > 20) then
      LMSSettings.FileHistory.Delete(20);

    BuildReOpenMenu;
  end;
end;


function TfrmMain.GetAutoPixelSize: integer;
var
  xc, yc, pxc, pyc: integer;

begin
  if MatrixMain.Matrix.Available then begin
    xc := pCanvas.Width - 70;
    yc := pCanvas.Height - 14;

    case sbGradient.Tag of
      0 : begin
            pxc := Floor(xc / MatrixMain.Matrix.Width);
            pyc := Floor(yc / MatrixMain.Matrix.Height);
          end;
      1 : begin
            pxc := Floor(xc / (MatrixMain.Matrix.Width + 2));
            pyc := Floor(yc / MatrixMain.Matrix.Height);
          end;
      2 : begin
            pxc := Floor(xc / MatrixMain.Matrix.Width);
            pyc := Floor(yc / (MatrixMain.Matrix.Height + 2));
          end;
    else
      pxc := 10;
      pyc := 10;
    end;

    if pxc > pyc then   // use the smallest value
      Result := pyc
    else
      Result := pxc;
  end
  else
    Result := 1;
end;


procedure TfrmMain.PreviewWindowChangeFrame(aNewFrame : integer);
begin
  MatrixMain.CurrentFrame := aNewFrame;

  SetFrameCaption(aNewFrame);
end;


procedure TfrmMain.PreviewWindowCommand(aCommandID : integer);
begin
  PlaybackCommand(aCommandID);
end;


procedure TfrmMain.miPreviousFrameClick(Sender: TObject);
begin
  PlaybackCommand(CAnimPreviousFrame);
end;


procedure TfrmMain.miQuickDataClick(Sender: TObject);
begin
  pQuickData.Visible := miQuickData.Checked;

  if (miQuickData.Checked) then
    UpdateData;
end;


procedure TfrmMain.PlaybackCommand(aCommandID : integer);
begin
  case aCommandID of
    CAnimPlayStart     : PlaybackStart;
    CAnimPlayStop      : PlaybackStop;
    CAnimFirstFrame    : PlaybackFirstFrame;
    CAnimPreviousFrame : PlaybackPreviousFrame;
    CAnimNextFrame     : PlaybackNextFrame;
    CAnimLastFrame     : PlaybackLastFrame;
  end;
end;


procedure TfrmMain.PlaybackStart;
begin
  bPlayAnimation.Enabled        := False;
  bStartFrame.Enabled           := False;
  bEndFrame.Enabled             := False;
  bNextFrame.Enabled            := False;
  bPreviousFrame.Enabled        := False;
  bStopAnimation.Enabled        := True;

  miPreviousFrame.Enabled       := False;
  miNextFrame.Enabled           := False;

  frmPreviewPopout.bPlayAnimation.Enabled := False;
  frmPreviewPopout.bStartFrame.Enabled    := False;
  frmPreviewPopout.bEndFrame.Enabled      := False;
  frmPreviewPopout.bNextFrame.Enabled     := False;
  frmPreviewPopout.bPreviousFrame.Enabled := False;
  frmPreviewPopout.bStopAnimation.Enabled := True;

  bAddFrame.Enabled              := False;
  bAddFrameCopy.Enabled          := False;
  bAddFrameMultiple.Enabled      := False;
  bDeleteFrame.Enabled           := False;
  bDeleteMultipleFrames.Enabled  := False;

  miAddFrame.Enabled             := False;
  miAddFrameCopy.Enabled         := False;
  miAddFrameMultiple.Enabled     := False;
  miDeleteFrame.Enabled          := False;
  miDeleteMultipleFrames.Enabled := False;

  timerAnimate.Tag               := tbFrames.Position; // 1
  timerAnimate.Enabled           := True;

  MatrixMain.AnimPlaying         := True;
  MatrixMain.MatrixReadOnly      := True;

  ManageUIControls(false, false);
end;


procedure TfrmMain.PlaybackStop;
begin
  timerAnimate.Enabled      := False;

  bPlayAnimation.Enabled    := True;
  bStartFrame.Enabled       := True;
  bEndFrame.Enabled         := True;
  bNextFrame.Enabled        := True;
  bPreviousFrame.Enabled    := True;
  bStopAnimation.Enabled    := False;

  miPreviousFrame.Enabled   := True;
  miNextFrame.Enabled       := True;

  frmPreviewPopout.bPlayAnimation.Enabled        := True;
  frmPreviewPopout.bStartFrame.Enabled           := True;
  frmPreviewPopout.bEndFrame.Enabled             := True;
  frmPreviewPopout.bNextFrame.Enabled            := True;
  frmPreviewPopout.bPreviousFrame.Enabled        := True;
  frmPreviewPopout.bStopAnimation.Enabled        := False;

  bAddFrame.Enabled             := True;
  bAddFrameCopy.Enabled         := True;
  bAddFrameMultiple.Enabled     := True;

  bDeleteFrame.Enabled          := tbFrames.Max <> 1;
  bDeleteMultipleFrames.Enabled := tbFrames.Max <> 1;

  miAddFrame.Enabled             := True;
  miAddFrameCopy.Enabled         := True;
  miAddFrameMultiple.Enabled     := True;

  miDeleteFrame.Enabled          := tbFrames.Max <> 1;
  miDeleteMultipleFrames.Enabled := tbFrames.Max <> 1;

  MatrixMain.AnimPlaying    := False;
  MatrixMain.MatrixReadOnly := False;

  ManageUIControls(false, false);
end;


procedure TfrmMain.PlaybackFirstFrame;
begin
  MatrixMain.CurrentFrame := 1;

  SetFrameCaption(1);
end;


procedure TfrmMain.PlaybackPreviousFrame;
var
  i : integer;

begin
  i := tbFrames.Position;

  if i = 1 then
    i := tbFrames.Max
  else
    dec(i);

  SetFrameCaption(i);

  MatrixMain.CurrentFrame := i;
end;


procedure TfrmMain.PlaybackNextFrame;
var
  i : integer;

begin
  i := tbFrames.Position;

  if i = tbFrames.Max then
    i := 1
  else
    inc(i);

  SetFrameCaption(i);

  MatrixMain.CurrentFrame := i;
end;


procedure TfrmMain.PlaybackLastFrame;
begin
  MatrixMain.CurrentFrame := tbFrames.Max;

  SetFrameCaption(tbFrames.Max);
end;


// =============================================================================


procedure TfrmMain.miFadeFirstLastClick(Sender: TObject);
begin
  MatrixMain.FadeFirstToLast;
end;


procedure TfrmMain.miFontViewerClick(Sender: TObject);
begin
  ShowFontViewer;
end;


procedure TfrmMain.UpdateDisplay(aNewFramePosition : integer);
begin
  tbFrames.Max                  := MatrixMain.FrameCount;

  bDeleteFrame.Enabled          := (MatrixMain.FrameCount > 1);
  bDeleteMultipleFrames.Enabled := (MatrixMain.FrameCount > 1);

  if (aNewFramePosition <> -1) then
    tbFrames.Position := aNewFramePosition;

  frmPreviewPopout.tbFrames.Max      := tbFrames.Max;
  frmPreviewPopout.tbFrames.Position := aNewFramePosition;

  SetFrameCaption(tbFrames.Position);

  // move to onnewframedisplayed
  if MatrixMain.IsLocked then
    bLockFrame.Tag := 1
  else
    bLockFrame.Tag := 0;

  SetButtonImage(bLockFrame, 6 + bLockFrame.Tag);

end;


procedure TfrmMain.bDeleteFrameClick(Sender: TObject);
begin
  if (tbFrames.Position = 1) and (tbFrames.Max = 1) then begin
  end
  else begin
    MatrixMain.DeleteFrame(tbFrames.Position);

   // if tbFrames.Position >= tbFrames.Max then
     // tbFrames.Position := tbFrames.Max - 1;

    UpdateDisplay(-1);
  end;
end;


procedure TfrmMain.bDeleteMultipleFramesClick(Sender: TObject);
var
  dmo : TDeleteMultipleObject;
  t, newFrame : integer;

begin
  dmo := DoDeleteMultiple;

  if dmo.Process then begin
    for t := dmo.StartFrame to dmo.EndFrame do begin
      MatrixMain.DeleteFrame(dmo.StartFrame);
    end;

    if tbFrames.Position > MatrixMain.FrameCount then
      newFrame := MatrixMain.FrameCount
    else
      newFrame := -1;

    UpdateDisplay(newFrame);
  end;
end;


procedure TfrmMain.Black1Click(Sender: TObject);
var
  lIndex : integer;

begin
  lIndex := TMenuItem(Sender).Tag;

  SystemSetBackgroundColour(backgroundColours[lIndex]);
end;


procedure TfrmMain.SystemSetBackgroundColour(aNewColour : integer);
begin
  frmMain.Color := aNewColour;
  pCanvas.Color := aNewColour;

  if Assigned(frmPreviewPopout) then begin
    frmPreviewPopout.Panel1.Color := aNewColour;
    frmPreviewPopout.Color        := aNewColour;
  end;

  MatrixMain.SetBackgroundColour(aNewColour);

  LMSSettings.App.BackgroundColour := aNewColour;
end;


procedure TfrmMain.miAutomateClick(Sender: TObject);
var
  lAI : TAutomationInput;
  lRGBPaletteColours : TRGBPaletteColours;
  t : integer;
  lLayers, lColours : TStringList;
  lResult : word;

begin
  FActionObject.LastFileName := LMSSettings.App.LastAutomationFileName;

  lLayers  := TStringList.Create;
  lColours := TStringList.Create;

  MatrixMain.GetFirst32Colours(lColours);

  lAI.FrameCurrent := tbFrames.Position;
  lAI.FrameMax     := tbFrames.Max;
  lAI.Width        := MatrixMain.Matrix.Width;
  lAI.Height       := MatrixMain.Matrix.Height;
  lAI.MatrixMode := MatrixMain.Matrix.Mode;

  lRGBPaletteColours.Left   := sSelectionLMB.Brush.Color;
  lRGBPaletteColours.Middle := sSelectionMMB.Brush.Color;
  lRGBPaletteColours.Right  := sSelectionRMB.Brush.Color;

  for t := 0 to 27 do
    lRGBPaletteColours.History[t] := FFramePalettePanel.RGBPaletteHistory[t].Brush.Color;

  for t := 0 to MatrixMain.GetLayerCount - 1 do
    lLayers.Add(MatrixMain.GetLayerName(t));

  lResult := DoAutomate(lAI, lRGBPaletteColours, lLayers, lColours, FActionObject);

  if (lResult = mrOK) then begin
    sbMouseModeClick(sbMouseMode);

    if (FActionObject.ActionList.Count <> 0) or (FActionObject.PostProcessList.Count <> 0) then begin
      FActionObject.SetParameterReveal(MatrixMain.Matrix.Width - 1, MatrixMain.Matrix.Height - 1);

      MatrixMain.Automate(FActionObject);

      tbFrames.Position                  := FActionObject.FrameEnd;
      frmPreviewPopout.tbFrames.Position := FActionObject.FrameEnd;

      tbFramesChange(Nil);

      LMSSettings.App.LastAutomationFileName := FActionObject.LastFileName;
    end;
  end;

  lLayers.Free;
end;


procedure TfrmMain.bAddFrameClick(Sender: TObject);
begin
  MatrixMain.InsertBlankFrameAt(tbFrames.Position);

  tbFrames.Max                       := MatrixMain.FrameCount;
  tbFrames.Position                  := tbFrames.Max;

  MatrixMain.CurrentFrame            := tbFrames.Position;

  frmPreviewPopout.tbFrames.Max      := tbFrames.Max;
  frmPreviewPopout.tbFrames.Position := tbFrames.Position;

  UpdateDisplay(-1);
end;


procedure TfrmMain.bAddFrameCopyClick(Sender: TObject);
begin
  MatrixMain.InsertCopyFrameAt(tbFrames.Position);

  tbFrames.Max                       := MatrixMain.FrameCount;
  tbFrames.Position                  := tbFrames.Position + 1;

  MatrixMain.CurrentFrame            := tbFrames.Position;

  frmPreviewPopout.tbFrames.Max      := tbFrames.Max;
  frmPreviewPopout.tbFrames.Position := tbFrames.Position;

  UpdateDisplay(-1);
end;


procedure TfrmMain.bAddFrameMultipleClick(Sender: TObject);
var
  s : string;
  sf : integer;

  function ValidIntegerInput(s : string): boolean;
   var
    t : integer;

   begin
    Result := True;

    for t := 1 to length(s) do begin
      if not((Ord(s[t]) >= 48) and (Ord(s[t]) <= 57)) then
        Result := False;
    end;
  end;

begin
  s := InputBox(GLanguageHandler.Text[kAddBlankFrames], GLanguageHandler.Text[kHowManyFrames], '1');

  if (s <> '') and (ValidIntegerInput(s)) then begin
    sf := StrToInt(s);

    MatrixMain.AddFrameMultiple(sf, tbFrames.Position);

    UpdateDisplay(-1);
  end
  else
    MessageDlg(GLanguageHandler.Text[kInvalidNumberFramesToAdd], mtWarning, [mbOK], 0);
end;


procedure TfrmMain.SetButtonImage(button : TBitbtn; imageidx : integer);
var
  bmp : TBitmap;

begin
  bmp := TBitmap.Create;
  bmp.Width  := 16;
  bmp.Height := 16;

  ilMain.GetBitmap(imageidx, bmp);

  button.Glyph.Assign(bmp);

  bmp.Free;
end;


procedure TfrmMain.Setcustomspeed1Click(Sender: TObject);
var
  lCustomSpeed : integer;

begin
  lCustomSpeed := DoCustomPlaybackSpeed(LMSSettings.App.CustomSpeed);

  if (lCustomSpeed <> 0) then begin
    LMSSettings.App.CustomSpeed := lCustomSpeed;

    miPlaybackSpeedCustom.Caption := GLanguageHandler.Text[kCustom] + ' (' + IntToStr(lCustomSpeed) + ' ms)';
  end;
end;


procedure TfrmMain.miSetDeadPixelsClick(Sender: TObject);
begin
  MatrixMain.DeadPixelsMode := not MatrixMain.DeadPixelsMode;

  if (MatrixMain.DeadPixelsMode) then begin
    ManageUIControls(true, false);

    miSetDeadPixels.Caption := GLanguageHandler.Text[kAcceptDeadPixels];
  end
  else begin
    ManageUIControls(false, false);

    miSetDeadPixels.Caption := GLanguageHandler.Text[kSetDeadPixels];
  end;
end;


procedure TfrmMain.miSetIgnoredFromPatternClick(Sender: TObject);
var
  sipo : TSetIgnoredPixelsObject;

begin
  sipo := DoIgnoredPixels(MatrixMain.Matrix.Width, MatrixMain.Matrix.Height);

  if (sipo.Process) then begin
    MatrixMain.SetDeadPixelsFromCustomShape(sipo.Shape, sipo.Parameter);
  end;
end;


procedure TfrmMain.bPlayAnimationClick(Sender: TObject);
begin
  PlaybackCommand(TBitBtn(Sender).Tag);
end;


procedure TfrmMain.bLightboxClick(Sender: TObject);
begin
  if bLightbox.Tag = 0 then
    bLightbox.Tag := 1
  else
    bLightbox.Tag := 0;

  SetButtonImage(bLightbox, bLightbox.Tag);

  MatrixMain.LightBox := bLightbox.Tag;
end;


procedure TfrmMain.bLockFrameClick(Sender: TObject);
begin
  if bLockFrame.Tag = 0 then
    bLockFrame.Tag := 1
  else
    bLockFrame.Tag := 0;

  SetButtonImage(bLockFrame, 6 + bLockFrame.Tag);

  if bLockFrame.Tag = 0 then
    MatrixMain.UnLockCurrentFrame
  else
    MatrixMain.LockCurrentFrame;
end;


procedure TfrmMain.DisplayFrame(frameno: integer);
begin
  SetFrameCaption(frameno);

  MatrixMain.CurrentFrame := frameno;
end;


procedure TfrmMain.Donate1Click(Sender: TObject);
begin
  TUtility.ExecuteFile(0, 'http://www.maximumoctopus.com/donate.htm', '', '');
end;


procedure TfrmMain.cbMatrixTypeChange(Sender: TObject);
var
  statusMouseButtonSelect : boolean;
  statusColourSelect0     : boolean;
  statusColourSelect123   : boolean;
  statusBackground        : boolean;

begin
  case LMSSettings.Project.MatrixMode of
    mtMono         : begin
                       pRGB_3BPP.Visible       := False;
                       statusBackground        := False;
                       statusMouseButtonSelect := False;
                       statusColourSelect0     := False;
                       statusColourSelect123   := False;
                       pCurrentColours.Visible := False;
                       panelRGBPalette.Visible := False;
                     end;
    mtBiSequential : begin
                       pRGB_3BPP.Visible       := False;
                       statusBackground        := False;
                       statusMouseButtonSelect := True;
                       statusColourSelect0     := True;
                       statusColourSelect123   := True;
                       pCurrentColours.Visible := False;
                       panelRGBPalette.Visible := False;
                     end;
    mtBiBitPlanes  : begin
                       pRGB_3BPP.Visible       := False;
                       statusBackground        := False;
                       statusMouseButtonSelect := True;
                       statusColourSelect0     := True;
                       statusColourSelect123   := True;
                       pCurrentColours.Visible := False;
                       panelRGBPalette.Visible := False;
                     end;
    mtRGB          : begin
                       pRGB_3BPP.Visible       := False;
                       statusBackground        := True;
                       statusMouseButtonSelect := True;
                       statusColourSelect0     := True;
                       statusColourSelect123   := False;
                       pCurrentColours.Visible := True;
                       panelRGBPalette.Visible := True;
                     end;
    mtRGB3BPP      : begin
                       pRGB_3BPP.Visible       := True;
                       statusBackground        := True;
                       statusMouseButtonSelect := True;
                       statusColourSelect0     := True;
                       statusColourSelect123   := False;
                       pCurrentColours.Visible := True;
                       panelRGBPalette.Visible := True;
                     end;
  else
    statusBackground        := False;
    statusMouseButtonSelect := False;
    statusColourSelect0     := False;
    statusColourSelect123   := False;

    LMSSettings.Project.MatrixMode := mtMono;
  end;
  
  lBackground.Visible     := statusBackground;
  lPixelColour.Visible    := statusBackground;
  miFadeFirstLast.Enabled := statusBackground;

  sColour0.Visible        := statusColourSelect0;
  sColour1.Visible        := statusColourSelect123;
  sColour2.Visible        := statusColourSelect123;
  sColour3.Visible        := statusColourSelect123;

  sSelectionLMB.Visible   := statusMouseButtonSelect;
  sSelectionMMB.Visible   := statusMouseButtonSelect;
  sSelectionRMB.Visible   := statusMouseButtonSelect;
  lColoursLeft.Visible    := statusMouseButtonSelect;
  lColoursMiddle.Visible  := statusMouseButtonSelect;
  lColoursRight.Visible   := statusMouseButtonSelect;
                        
  // ===========================================================================

  MatrixMain.ChangeMatrixMode(LMSSettings.Project.MatrixMode);

  SetupMatrixColours;

  RecalculatePadding;

  // ===========================================================================  

  case MatrixMain.Matrix.Mode of
     mtMono        : begin
                       if sbGradient.Tag = 1 then
                         ToggleGradient(goOff, True);

                        sbGradient.Enabled               := False;
                        miClearAllFramesGradient.Enabled := False;
                        sbRandomDraw.Enabled             := False;
                        miGradientAllFrames.Enabled      := False;
                       sbPicker.Enabled                 := False;
                     end;
    mtBiSequential,
    mtBiBitPlanes,
    mtRGB3BPP      : begin
                       if sbGradient.Tag = 1 then
                         ToggleGradient(goVertical, False);

                       sbGradient.Enabled               := True;
                       miClearAllFramesGradient.Enabled := True;
                       sbRandomDraw.Enabled             := True;
                       miGradientAllFrames.Enabled      := True;
                       sbPicker.Enabled                 := False;
                     end;
    mtRGB          : begin
                       if sbGradient.Tag = 1 then
                         ToggleGradient(goVertical, True);

                       sbPicker.Enabled                 := True;

                       iMMBGradient.Visible             := False;
                     end;
  end;

  FormResize(Nil);
end;


procedure TfrmMain.cbMirrorModeChange(Sender: TObject);
begin
  MatrixMain.MirrorMode := TMirrorMode(cbMirrorMode.ItemIndex);
end;


procedure TfrmMain.Load1Click(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  if sbClear.Enabled then begin
    if MessageDlg(GLanguageHandler.Text[kOpeningNewMatrixWillClearCurrentProject] + #13#13 +
                  GLanguageHandler.Text[kDoYouWishToContinue], mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  // =======================================================================

  ConfigureOpenDialog(CLoadProject);

  if odMain.Execute then begin
    LoadFromFileName(odMain.FileName);

    FormResize(Nil);
  end;
end;


procedure TfrmMain.LoadWithWarnings(aFileName : string);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  if sbClear.Enabled then begin
    if MessageDlg(GLanguageHandler.Text[kOpeningNewProjectWillClearCurrentProject] + #13#13 +
                  GLanguageHandler.Text[kDoYouWishToContinue], mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  // =======================================================================

  LoadFromFileName(aFileName);

  FormResize(Nil);
end;


procedure TfrmMain.miMemoryR1Click(Sender: TObject);
begin
  if sbClear.Enabled then
    MatrixMain.RestoreFromUserBuffer(TMenuItem(Sender).Tag);
end;


procedure TfrmMain.miMergeBottomClick(Sender: TObject);
var
  lMerge : TMergeObject;

begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  lMerge := DoMerge;

  if (lMerge.Process) then begin

    case (lMerge.Mode) of
      moAnimationBottom : MergeFromFileName(lMerge.FileName, lMerge.StartFrame, lmMergeBottomPriority);
      moAnimationTop    : MergeFromFileName(lMerge.FileName, lMerge.StartFrame, lmMergeTopPriority);
      moNewLayer        : MergeFromFileName(lMerge.FileName, lMerge.StartFrame, lmMergeNewLayer);
      moCurrentFrame    : MergeFromFileName(lMerge.FileName, lMerge.StartFrame, lmMergeCurrentLayer);
    end;

    UpdateDisplay(-1);

    LMSSettings.App.LastLoadLocation := ExtractFilePath(odMain.Filename);
  end;
end;


procedure TfrmMain.miAppendClick(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  ConfigureOpenDialog(CLoadProject);

  if odMain.Execute then begin

    if AppendFromFileName(odMain.FileName) then begin
      UpdateDisplay(-1);

      LMSSettings.App.LastLoadLocation := ExtractFilePath(odMain.Filename);
    end;
  end;
end;


procedure TfrmMain.miAutoSaveClick(Sender: TObject);
begin
  miAutoSave.Checked    := not(miAutoSave.Checked);

  timerAutoSave.Enabled := miAutoSave.Checked;
end;


procedure TfrmMain.miBrushFlipClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0 : MatrixMain.PerformEffectOnBrush(modeFlip);
    1 : MatrixMain.PerformEffectOnBrush(modeMirror);
    2 : MatrixMain.PerformEffectOnBrush(modeInvert);
  end;
end;

procedure TfrmMain.miCopyClick(Sender: TObject);
begin
  MatrixMain.CopyCurrentFrame;
end;


procedure TfrmMain.miCopyFromPreviousClick(Sender: TObject);
begin
  if (sbClear.Enabled) and (tbFrames.Position <> 1) then begin
    MatrixMain.CopyFromPrevious(tbFrames.Position);
  end;
end;


procedure TfrmMain.miCopyMultipleClick(Sender: TObject);
var
  cpm : TCopyMultipleObject;
  lLayers : TStringList;
  t   : integer;

begin
  lLayers := TStringList.Create;

  for t := 0 to MatrixMain.GetLayerCount - 1 do
    lLayers.Add(MatrixMain.GetLayerName(t));

  cpm := DoCopyMultiple(MatrixMain.FrameCount, lLayers);

  if (cpm.Process) then begin
    MatrixMain.AutomateMode := True;

    for t := cpm.StartFrame to cpm.EndFrame do begin

      if cpm.AllLayers then
        MatrixMain.CopyAllLayersFromTo(t, cpm.CopyTo + (t - cpm.StartFrame))
      else
        MatrixMain.CopyLayerFromTo(cpm.Source, cpm.Destination, t, cpm.CopyTo + (t - cpm.StartFrame));
    end;

    MatrixMain.AutomateMode := False;

    MatrixMain.Refresh;
  end;

  lLayers.Free;
end;


procedure TfrmMain.miCustomBackgroundClick(Sender: TObject);
begin
  if colorDialog.Execute then begin
    SystemSetBackgroundColour(colorDialog.Color);
  end;
end;


procedure TfrmMain.miFontModeClick(Sender: TObject);
begin
  pbFont.Visible       := miFontMode.Checked;
  miSaveAsFont.Enabled := miFontMode.Checked;

  if miFontMode.Checked then
    MatrixMain.SoftwareMode := smFont
  else
    MatrixMain.SoftwareMode := smAnimation;

  tbFrames.Max                  := MatrixMain.FrameCount;
  frmPreviewPopout.tbFrames.Max := tbFrames.Max;

  SetFrameCaption(tbFrames.Position);

  UpdateMemoryUsage;

  FormResize(Nil);
end;


procedure TfrmMain.miImportInToCurrentClick(Sender: TObject);
var
  ted : TImportData;

begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  ConfigureOpenDialog(CLoadProject);

  if odMain.Execute then begin
    ted := MatrixMain.ImportLEDMatrixDataSingleFrame(odMain.FileName);

    LMSSettings.Project.MatrixMode := ted.MatrixMode;

    cbMatrixTypeChange(Nil);
  end;
end;


procedure TfrmMain.miMemory1Click(Sender: TObject);
begin
  if sbClear.Enabled then begin
    MatrixMain.CopyToUserBuffer(TMenuItem(Sender).Tag);

    MenuCopyMemory[TMenuItem(Sender).Tag].ImageIndex    := 9;
    MenuRestoryMemory[TMenuItem(Sender).Tag].ImageIndex := 9;
  end;
end;


procedure TfrmMain.miPresetSaveCurrentClick(Sender: TObject);
var
  s : string;
  lMPP : TMatrixPreset;

begin
  s := InputBox(GLanguageHandler.Text[kPresetFileName], GLanguageHandler.Text[kName], IntToStr(MatrixMain.Matrix.Width) + ' x ' + IntToStr(MatrixMain.Matrix.Height));

  if s <> '' then begin
    lMPP.Width      := MatrixMain.Matrix.Width;
    lMPP.Height     := MatrixMain.Matrix.Height;
    lMPP.PixelSize  := sbPixelSize.Tag;
    lMPP.MatrixMode := MatrixMain.Matrix.Mode;
    lMPP.PixelShape := sbPixelShape.Tag;

    TPresetHandler.SaveMatrixPreset(ExtractFilePath(Application.ExeName) + 'presets\' + s + '.ledspreset', lMPP);
  end;
end;


procedure TfrmMain.miPreviewClick(Sender: TObject);
begin
  MatrixMain.PreviewActive := miPreview.Checked;

  FormResize(Nil);  
end;


procedure TfrmMain.miRandomnessTinyClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;

  MatrixMain.RandomCoeff := TMenuItem(Sender).Tag;
end;


procedure TfrmMain.BuildFontList;
var
  searchResult : TSearchRec;
  mi : TMenuItem;
  i : integer;

begin
  if FindFirst(ExtractFilePath(Application.ExeName) + 'fonts\*.ledsfont', faAnyFile, searchResult) = 0 then begin
    i := 0;

    repeat
      mi := TMenuItem.Create(Self);

      with mi do begin
        Caption   := TUtility.RemoveExtension(searchResult.Name);
        Tag       := i;

        RadioItem := True;
        Checked   := False;

        OnClick   := SelectFont;
      end;

      miLoadFont.Add(mi);

      if i = 0 then
        SelectFont(mi); // loads first font in list

      inc(i);
    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end
  else
    sbFont.Visible := False;
end;


procedure TfrmMain.BuildPresetList;
var
  searchResult : TSearchRec;
  mi : TMenuItem;
  i : integer;

begin
  // Try to find regular files matching Unit1.d* in the current dir
  if FindFirst(ExtractFilePath(Application.ExeName) + 'presets\*.ledspreset', faAnyFile, searchResult) = 0 then begin
    i := 0;

    repeat
      mi := TMenuItem.Create(Self);

      with mi do begin
        Caption := TUtility.RemoveExtension(searchResult.Name);
        Tag     := i;

        OnClick := SelectPreset;
      end;

      miLoadPreset.Add(mi);

      inc(i);
    until FindNext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);
  end;
end;


procedure TfrmMain.BuildGradientList;
var
  searchResult : TSearchRec;
  mi : TMenuItem;
  i : integer;

begin
  // Try to find regular files matching *.ledsgradient in the current dir
  if FindFirst(ExtractFilePath(Application.ExeName) + 'gradients\*.ledsgradient', faAnyFile, searchResult) = 0 then begin
    i := 0;

    repeat
      mi := TMenuItem.Create(Self);
      with mi do begin
        Caption := TUtility.RemoveExtension(searchResult.Name);
        Tag     := i;

        OnClick := SelectGradient;
      end;

      miLoadGradients.Add(mi);

      inc(i);
    until FindNext(searchResult) <> 0;

    // Must free up resources used by these successful finds
    FindClose(searchResult);
  end;
end;


procedure TfrmMain.SelectFont(Sender : TObject);
var
  s,temp : string;
  t : integer;

begin
  temp := ExtractFilePath(Application.ExeName) + 'fonts\' + TMenuItem(Sender).Caption + '.ledsfont';
  s    := '';

  for t := 1 to length(temp) do
    if temp[t] <> '&' then
      s := s + temp[t];

  if FileExists(s) then begin
    MatrixMain.LoadFont(s);

    TMenuItem(Sender).Checked   := True;
  end
  else
    MessageDlg(GLanguageHandler.Text[kCannotFindFont] + #13#10 + #13#10 + '"' + s + '"', mtError, [mbOK], 0);
end;


procedure TfrmMain.SelectPreset(Sender : TObject);
var
  s, temp : string;
  t : integer;

begin
  if MessageDlg(GLanguageHandler.Text[kReallyLoadThisPreset], mtWarning, [mbYes, mbNo], 0) = mrYes then begin

    temp := ExtractFilePath(Application.ExeName) + 'presets\' + TMenuItem(Sender).Caption + '.ledspreset';
    s    := '';

    for t := 1 to length(temp) do
      if temp[t] <> '&' then
        s := s + temp[t];

    if FileExists(s) then begin
      LoadPreset(s);
    end
    else
      MessageDlg(GLanguageHandler.Text[kCannotFindPresetFile] + #13#10 + #13#10 + '"' + s + '"', mtError, [mbOK], 0);
  end;
end;


procedure TfrmMain.LoadPreset(filename : string);
var
  lMatrixMode : TMatrixMode;
  lMPP : TMatrixPreset;

begin
  lMatrixMode := mtMono; // default matrix type if none specified in file

  // ===========================================================================

  lMPP := TPresetHandler.LoadMatrixPreset(filename);

  LMSSettings.Project.width  := lMPP.Width;
  LMSSettings.Project.height := lMPP.Height;
  case (lMPP.PixelSize) of
    0 : miPixelTinyClick(miPixelTiny);
    1 : miPixelTinyClick(miPixelSmall);
    2 : miPixelTinyClick(miPixelMedium);
    3 : miPixelTinyClick(miPixelLarge);
    4 : miPixelTinyClick(miPixelVeryLarge);
  end;

  lMatrixMode := lMPP.MatrixMode;

  // ===========================================================================

  LMSSettings.Project.MatrixMode := lMatrixMode;
  cbMatrixTypeChange(Nil);

  // ===========================================================================

  sbBuildClick(Load1);
end;


procedure TfrmMain.Small1Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;

  puBrushSize.Tag := TMenuItem(Sender).Tag;

  MatrixMain.ChangePixelBrush(TBrushSize(puBrushSize.Tag));
end;


procedure TfrmMain.sRGBPalette1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lIndex : integer;

begin
  if (MatrixMain.Matrix.Mode = mtRGB) then begin
    lIndex := TShape(Sender).Tag;

    if (MatrixMain.Render.DrawData.Mode = dmPicker) or (ssCtrl in Shift) then begin
      colorDialog.Color := TShape(Sender).Brush.Color;

      if colorDialog.Execute then
        TShape(Sender).Brush.Color := colorDialog.Color;
    end
    else begin
      if ssLeft in Shift then begin
        sSelectionLMB.Brush.Color              := RGBPalette[lIndex].Brush.Color;

        MatrixMain.LEDRGBColours[CMouseLeft]   := RGBPalette[lIndex].Brush.Color;
      end
      else if ssMiddle in Shift then begin
        sSelectionMMB.Brush.Color              := RGBPalette[lIndex].Brush.Color;

        MatrixMain.LEDRGBColours[CMouseMiddle] := RGBPalette[lIndex].Brush.Color;
      end
      else if ssRight in Shift then begin
        sSelectionRMB.Brush.Color              := RGBPalette[lIndex].Brush.Color;

        MatrixMain.LEDRGBColours[CMouseRight]  := RGBPalette[lIndex].Brush.Color;
      end;

      MatrixMain.SetMouseButtonColours(MatrixMain.LEDRGBColours[CMouseLeft],
                                       MatrixMain.LEDRGBColours[CMouseMiddle],
                                       MatrixMain.LEDRGBColours[CMouseRight]);

      GenerateShades(TShape(Sender).Brush.Color);
    end;
  end;
end;


procedure TfrmMain.sRGBPalette1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lPixelColour.Caption := LMSSettings.App.HexPrefix + IntToHex(TUtility.RGBConvertTo(TShape(Sender).Brush.Color, cmRGB, llBottomRight, 100), 6) + ' (' + TUtility.RGBConvertToSplit(TShape(Sender).Brush.Color, cmRGBSimple, 100, nfDecimal, '', ' ') + ')';
end;


procedure TfrmMain.sRGBPaletteColourMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (MatrixMain.Matrix.Mode = mtRGB) then begin
    if ssLeft in Shift then begin
      PaletteColourSelected(0, TShape(Sender).Brush.Color);
    end
    else if ssMiddle in Shift then begin
      PaletteColourSelected(1, TShape(Sender).Brush.Color);
    end
    else if ssRight in Shift then begin
      PaletteColourSelected(2, TShape(Sender).Brush.Color);
    end;
  end;
end;


procedure TfrmMain.PaletteColourSelected(aMouseButton, aColour : integer);
begin
  if (MatrixMain.Matrix.Mode = mtRGB) then begin
    case aMouseButton of
      0 : begin
            sSelectionLMB.Brush.Color              := aColour;

            MatrixMain.LEDRGBColours[CMouseLeft]   := aColour;
          end;
      1 : begin
            sSelectionMMB.Brush.Color              := aColour;

            MatrixMain.LEDRGBColours[CMouseMiddle] := aColour;
          end;
      2 : begin
            sSelectionRMB.Brush.Color              := aColour;

            MatrixMain.LEDRGBColours[CMouseRight]  := aColour;
          end;
    end;

    MatrixMain.SetMouseButtonColours(MatrixMain.LEDRGBColours[CMouseLeft],
                                     MatrixMain.LEDRGBColours[CMouseMiddle],
                                     MatrixMain.LEDRGBColours[CMouseRight]);

    FFramePalettePanel.AddToHistory(aColour); // to do, palette clicks only

    GenerateShades(aColour);
  end;
end;


procedure TfrmMain.miPaletteGradientToolbarClick(Sender: TObject);
begin
  miPaletteGradientToolbar.Checked := not(miPaletteGradientToolbar.Checked);

  pRGBPalette.Visible      := miPaletteGradientToolbar.Checked;

  FormResize(Nil);
end;

procedure TfrmMain.miPreviewViewSquareClick(Sender: TObject);
begin
  MatrixMain.PreviewView := TViewShape(TMenuItem(Sender).Tag);

  SyncPreviewView(TMenuItem(Sender).Tag);

  FormResize(Nil);
end;


procedure TfrmMain.miPixelShapeSquareClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;

  TMenuItem(Sender).Checked := True;

  sbPixelShape.Caption   := TMenuItem(Sender).Caption;
  sbPixelShape.Tag       := TMenuItem(Sender).Tag;
  LMSSettings.PixelShape := TPixelShape(TMenuItem(Sender).Tag);

  if sbClear.Enabled then begin
    MatrixMain.ChangePixelShape(TPixelShape(LMSSettings.PixelShape));
  end;

  Screen.Cursor := crDefault;
end;


// this should all be handled by the matrix component // to do
procedure TfrmMain.miPixelTinyClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;

  TMenuItem(Sender).Checked := True;

  case TMenuItem(Sender).Tag of
     0 : LMSSettings.PixelSize := CPixelSize10;
     1 : LMSSettings.PixelSize := CPixelSize15;
     2 : LMSSettings.PixelSize := CPixelSize20;
     3 : LMSSettings.PixelSize := CPixelSize25;
     4 : LMSSettings.PixelSize := CPixelSize30;
     5 : LMSSettings.PixelSize := CPixelSize40;
     6 : LMSSettings.PixelSize := CPixelSize50;

    99 : LMSSettings.PixelSize := GetAutoPixelSize;
  end;

  sbPixelSize.Caption := TMenuItem(Sender).Caption;
  sbPixelSize.Tag     := TMenuItem(Sender).Tag;

  MatrixMain.ChangePixelSize(LMSSettings.PixelSize);

  Screen.Cursor := crDefault;
end;


procedure TfrmMain.sbPresetClick(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  puPresets.Popup(Left + sbPreset.Left, Top + 80);
end;


procedure TfrmMain.sbRotateAnyClick(Sender: TObject);
var
  t, origframe : integer;
  byangle : real;

begin
  Screen.Cursor := crHourGlass;

  MatrixMain.BackupMatrix(MatrixMain.CurrentLayer, MatrixMain.CurrentFrame);

  byangle   := StrToFloat(Copy(cbRotateAngle.Text, 1, Length(cbRotateAngle.Text) - 1));
  origframe := tbFrames.Position;

  for t := 1 to cbRotateCount.ItemIndex + 1 do begin

    if (t + origframe > MatrixMain.FrameCount) then
      MatrixMain.InsertBlankFrameAt(MatrixMain.FrameCount);

    // =========================================================================

    MatrixMain.RotateFrameAnyAngle(t * byangle, t + origframe);
  end;

  UpdateDisplay(-1);

  Screen.Cursor := crDefault;
end;


procedure TfrmMain.tbFramesChange(Sender: TObject);
begin
  MatrixMain.CurrentFrame := tbFrames.Position;

  SetFrameCaption(tbFrames.Position);
end;


procedure TfrmMain.sbPixelShapeClick(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  puPixelShape.Popup(Left + sbPixelShape.Left, Top + 80);
end;


procedure TfrmMain.sbPixelSizeClick(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  puPixelSize.Popup(Left + sbPixelSize.Left, Top + 80);
end;


procedure TfrmMain.sbMirrorClick(Sender: TObject);
var
  lEffect : integer;

begin
  case TSpeedButton(Sender).Tag of
    0 : lEffect := modeFlip;
    1 : lEffect := modeMirror;
    2 : lEffect := modeInvert;
  else
    lEffect := modeFlip;
  end;

  if (FFrameLayerPanel.SyncAll) then
    MatrixMain.PerformEffectController(lEffect, CMOMCurrentFrameLayers)
  else
    MatrixMain.PerformEffectController(lEffect, CMOMCurrentOnly);
end;


procedure TfrmMain.miFlipAllFramesClick(Sender: TObject);
begin
  MatrixMain.PerformEffectController(modeFlipAll, CMOMCurrentLayerFrames);
end;


procedure TfrmMain.sbMouseModeClick(Sender: TObject);
begin
  MatrixMain.Render.DrawData.Mode        := TDrawMode(TSpeedButton(Sender).Tag);
  MatrixMain.Render.DrawData.Point       := CDrawPointNone;
  MatrixMain.Render.DrawData.Coords[0].X := -1;
  MatrixMain.Render.DrawData.Coords[0].Y := -1;

  if (MatrixMain.Render.DrawData.Mode >= dmFloodFill) then begin
    MatrixMain.Render.DrawData.SinglePoint  := True;

    MatrixMain.Render.DrawData.Parameter    := DefaultPatternParameter[Ord(MatrixMain.Render.DrawData.Mode)];
    MatrixMain.Render.DrawData.ParameterMax := DefaultPatternParameterMax[Ord(MatrixMain.Render.DrawData.Mode)];

    if (MatrixMain.Matrix.Mode = mtRGB) or (MatrixMain.Matrix.Mode = mtRGB3BPP) then
      MatrixMain.Render.DrawData.Colour      := $FF8822  // ensures something is drawn as we move before clicking
    else
      MatrixMain.Render.DrawData.Colour      := 1        // ensures something is drawn as we move before clicking
  end
  else begin
    MatrixMain.Render.DrawData.SinglePoint  := False;

    MatrixMain.Render.DrawData.Parameter    := 0;
    MatrixMain.Render.DrawData.ParameterMax := 0;
  end;

  if (MatrixMain.Render.DrawData.Mode = dmGradientBrush) then
    iMMBGradient.Visible := True
  else if iMMBGradient.Visible then
    iMMBGradient.Visible := False;

  MatrixMain.Render.DrawData.CopyPos.X := 0;
  MatrixMain.Render.DrawData.CopyPos.Y := 0;
  MatrixMain.Render.DrawData.Special   := tbFrames.Max;

  lSelectedTool.Caption           := DrawModes[TSpeedButton(Sender).Tag];

  DisplayFrame(tbFrames.Position);
end;


procedure TfrmMain.sbNewBrushClick(Sender: TObject);
var
  lMatrixSettings : TMatrixSettings;
  lNewBrush : TNewBrush;
  lRGBPaletteColours : TRGBPaletteColours;
  lBrush : TStringList;
  lRow, t : integer;

begin
  lMatrixSettings.MatrixMode := MatrixMain.Matrix.Mode;
  lMatrixSettings.Width      := MatrixMain.Matrix.Width;
  lMatrixSettings.Height     := MatrixMain.Matrix.Height;

  lRGBPaletteColours.Left    := sSelectionLMB.Brush.Color;
  lRGBPaletteColours.Middle  := sSelectionMMB.Brush.Color;
  lRGBPaletteColours.Right   := sSelectionRMB.Brush.Color;

  for t := 0 to 27 do
    lRGBPaletteColours.History[t] := FFramePalettePanel.RGBPaletteHistory[t].Brush.Color;

  lBrush := TStringList.Create;

  lNewBrush := DoNewBrush(lBrush, lMatrixSettings, lRGBPaletteColours);

  if (lNewBrush.Proceed) then begin
    for lRow := 0 to lBrush.Count - 1 do begin
      MatrixMain.StringToRow(True, lBrush[lRow], -1, lRow, 0, False);
    end;

    MatrixMain.Render.DrawData.Point       := CDrawPointNone;
    MatrixMain.Render.DrawData.Mode        := dmPaste;
    MatrixMain.Render.DrawData.Coords[0].X := -1;
    MatrixMain.Render.DrawData.Coords[0].Y := -1;

    MatrixMain.Render.DrawData.CopyPos.X   := lNewBrush.Width;
    MatrixMain.Render.DrawData.CopyPos.Y   := lNewBrush.Height;
  end;

  lBrush.Free;
end;


procedure TfrmMain.sbRotateLClick(Sender: TObject);
var
  lRDirection : integer;

begin
  if TSpeedButton(Sender).Tag = 0 then
    lRDirection := modeRotateACW
  else
    lRDirection := modeRotateCW;

  if (FFrameLayerPanel.SyncAll) then
    MatrixMain.RotateFrameController(lRDirection, CMOMCurrentFrameLayers)
  else
    MatrixMain.RotateFrameController(lRDirection, CMOMCurrentOnly);
end;


procedure TfrmMain.miShiftLeftClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0 : sbScrollLeftClick(sbScrollLeft);
    1 : sbScrollLeftClick(sbScrollRight);
    2 : sbScrollLeftClick(sbScrollUp);
    3 : sbScrollLeftClick(sbScrollDown);
  end;
end;


procedure TfrmMain.miShowAnimationToolbarClick(Sender: TObject);
begin
  pAnimationToolbar.Visible := miShowAnimationToolbar.Checked;
end;


procedure TfrmMain.SetPlaybackCustom(aValue : integer);
begin
  miPlaybackSpeedCustom.Checked := True;
  timerAnimate.Interval         := aValue;
  bPlayAnimation.Hint           := GLanguageHandler.Text[kPlayAnimation] + ' (' + GLanguageHandler.Text[kCustom] + ' ' + IntToStr(aValue) + ' ms';
  miPlaybackSpeedCustom.Caption := GLanguageHandler.Text[kCustom] + ' (' + IntToStr(aValue) + ' ms)';
end;


procedure TfrmMain.miPlaybackSpeed3Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;

  case TMenuItem(Sender).Tag of
    0 : begin
          timerAnimate.Interval := 2000;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (2 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    1 : begin
          timerAnimate.Interval := 1500;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (1.5 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    2 : begin
          timerAnimate.Interval := 1000;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (1 ' + GLanguageHandler.Text[kSecond] + ')';
        end;
    3 : begin
          timerAnimate.Interval := 500;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.5 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    4 : begin
          timerAnimate.Interval := 250;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.25 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    5 : begin
          timerAnimate.Interval := 200;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.20 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    6 : begin
          timerAnimate.Interval := 100;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.1 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    7 : begin
          timerAnimate.Interval := 50;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.05 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    8 : begin
          timerAnimate.Interval := 25;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.025 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
    9 : begin
          timerAnimate.Interval := 20;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.020 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
   10 : begin
          timerAnimate.Interval := 10;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (0.01 ' + GLanguageHandler.Text[kSeconds] + ')';
        end;
   20 : begin
          timerAnimate.Interval := LMSSettings.App.CustomSpeed;
          bPlayAnimation.Hint   := GLanguageHandler.Text[kPlayAnimation] + ' (' + IntToStr(LMSSettings.App.CustomSpeed) + ' ms';
        end;
  end;
end;


procedure TfrmMain.miAddCommentClick(Sender: TObject);
var
  s : string;

begin
  s := MatrixMain.Matrix.Comment;

  if InputQuery(GLanguageHandler.Text[kMatrixComment], GLanguageHandler.Text[kAddCommentMatrix], s) then begin
    MatrixMain.Matrix.Comment := s;
  end;
end;


procedure TfrmMain.miASCIIStartCodeClick(Sender: TObject);
var
  s : string;

begin
  s := IntToStr(LMSSettings.App.ASCIIIndex);

  if InputQuery(GLanguageHandler.Text[kASCIICode], GLanguageHandler.Text[kStartASCIICodeFontMode], s) then begin
    LMSSettings.App.ASCIIIndex := StrToInt(s);
  end;
end;


procedure TfrmMain.miAutosave2Click(Sender: TObject);
begin
  if (Sender <> Nil) then begin
    TMenuItem(Sender).Checked := True;

    case TAutoSaveInterval(TMenuItem(Sender).Tag) of
      asTwoMinutes  : timerAutosave.Interval :=  2 * 60 * 1000;
      asFiveMinutes : timerAutosave.Interval :=  5 * 60 * 1000;
      asTenMinutes  : timerAutosave.Interval := 10 * 60 * 1000;
    end;

    LMSsettings.AutoSaveInterval := TAutoSaveInterval(TMenuItem(Sender).Tag);
  end;
end;


procedure TfrmMain.miPreviewVoid10Click(Sender: TObject);
begin
  MatrixMain.PreviewVoid := previewVoids[TMenuItem(Sender).Tag];

  SyncPreviewVoid(TMenuItem(Sender).Tag);

  FormResize(Nil);
end;


procedure TfrmMain.miRadialOffset45Click(Sender: TObject);
var
  lOffset : integer;
begin
  TMenuItem(Sender).Checked := True;

  case TMenuItem(Sender).Tag of
    0 : lOffset :=   0;
    1 : lOffset :=  45;
    2 : lOffset :=  90;
    3 : lOffset := 135;
    4 : lOffset := 180;
    5 : lOffset := 225;
    6 : lOffset := 270;
    7 : lOffset := 315;
  else
    lOffset := MatrixMain.RadialOffset;
  end;

  MatrixMain.RadialOffset := lOffset;
end;


procedure TfrmMain.miPreviewOffsetReverseClick(Sender: TObject);
begin
  MatrixMain.RadialOffsetDirection := miPreviewOffsetReverse.Checked;
end;


procedure TfrmMain.New1Click(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  if MessageDlg(GLanguageHandler.Text[kReallyClearEverything], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    sbBuildClick(Nil);
  end;
end;


procedure TfrmMain.miNextFrameClick(Sender: TObject);
begin
  PlaybackCommand(CAnimNextFrame);
end;


procedure TfrmMain.miToggleLayoutPanelClick(Sender: TObject);
begin
  pLayers.Visible := not pLayers.Visible;

  miToggleLayoutPanel.Checked := pLayers.Visible;

  if pLayers.Visible then
    FFrameLayerPanel.UpdateLayerTable;

  FormResize(Nil);
end;


procedure TfrmMain.miToggleLockStatusClick(Sender: TObject);
var
  lTLFRO : TToggleLockFrameRangeObject;

begin
  lTLFRO := DoToggleLockFrameRange(MatrixMain.FrameCount);

  if lTLFRO.Process then begin
    MatrixMain.LockUnLockRange(lTLFRO.StartFrame, lTLFRO.EndFrame, lTLFRO.LockStatus);
  end;
end;


procedure TfrmMain.miPasteClick(Sender: TObject);
begin
  MatrixMain.PasteCurrentFrame;
end;


procedure TfrmMain.sbUndoClick(Sender: TObject);
begin
  MatrixMain.Undo;
end;


procedure TfrmMain.miRedoClick(Sender: TObject);
begin
  MatrixMain.Redo;
end;


procedure TfrmMain.ReopenClick(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  if sbClear.Enabled then begin
    if MessageDlg(GLanguageHandler.Text[kOpeningNewMatrixWillClearCurrentProject] + #13#13 +
                  GLanguageHandler.Text[kDoYouWishToContinue], mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  // =======================================================================

  LoadFromFileName(LMSSettings.FileHistory[TMenuItem(Sender).Tag]);

  FormResize(Nil);
end;


procedure TfrmMain.LanguageClick(Sender: TObject);
begin
  LMSSettings.App.Language := TMenuItem(Sender).Caption;
end;


procedure TfrmMain.sColour3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  old, lColour : integer;
  lTempDrawMode : TDrawMode; // for some reason the colordialog crashes when in floodfill mode... interim fix... :(     to do

begin
  lTempDrawMode := MatrixMain.Render.DrawData.Mode;

  MatrixMain.Render.DrawData.Mode := dmNone;

  case MatrixMain.Matrix.Mode of
    mtMono, mtBiSequential, mtBiBitPlanes : begin
                   if button = mbLeft then begin
                     lColour                   := TShape(Sender).Tag;

                     sSelectionLMB.Brush.Color := MatrixMain.LEDColours[lColour];
                     sSelectionLMB.Tag         := TShape(Sender).Tag;
                   end
                   else if button = mbMiddle then begin
                     lColour                   := TShape(Sender).Tag;

                     sSelectionMMB.Brush.Color := MatrixMain.LEDColours[lColour];
                     sSelectionMMB.Tag         := TShape(Sender).Tag;
                   end
                   else if button = mbRight then begin
                     lColour                   := TShape(Sender).Tag;

                     sSelectionRMB.Brush.Color := MatrixMain.LEDColours[lColour];
                     sSelectionRMB.Tag         := TShape(Sender).Tag;
                   end;

                   MatrixMain.SetMouseButtonColours(sSelectionLMB.Tag,
                                                    sSelectionMMB.Tag,
                                                    sSelectionRMB.Tag);
                 end;
    mtRGB      : begin // == RGB ===============================================
                   colorDialog.Color := TShape(Sender).Brush.Color;

                   if colorDialog.Execute then begin
                     old := MatrixMain.RGBBackground;

                     TShape(Sender).Brush.Color := colorDialog.Color;

                     GenerateShades(colorDialog.Color);

                     if Sender = sColour0 then
                       MatrixMain.RGBBackground := TShape(Sender).Brush.Color
                     else if Sender = sSelectionLMB then
                       MatrixMain.LEDRGBColours[CMouseLeft]   := colorDialog.Color
                     else if Sender = sSelectionMMB then
                       MatrixMain.LEDRGBColours[CMouseMiddle] := colorDialog.Color
                     else if Sender = sSelectionRMB then
                       MatrixMain.LEDRGBColours[CMouseRight]  := colorDialog.Color;

                     if (Sender = sColour0) then begin
                       if old <> colorDialog.Color then begin
                         if MessageDlg(GLanguageHandler.Text[kBackgroundColourHasChanged] + ' ' + #13#13 +
                                       GLanguageHandler.Text[kChangeAllBackgroundPixels], mtInformation, [mbOK, mbCancel], 0) = mrOK then
                           MatrixMain.ChangePixels(old, colorDialog.Color);
                       end;
                     end;
                   end;

                   MatrixMain.SetMouseButtonColours(sSelectionLMB.Brush.Color,
                                                    sSelectionMMB.Brush.Color,
                                                    sSelectionRMB.Brush.Color);
                 end;
    mtRGB3BPP  : {};
  end;

  MatrixMain.Render.DrawData.Mode := lTempDrawMode;
end;


procedure TfrmMain.sbBuildClick(Sender: TObject);
var
  mw, mh : integer;
  ps : TProjectSettings;

begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // ===========================================================================
  // ===========================================================================

  if (Sender = Load1) then begin
    LMSSettings.Project.clear   := False;
    LMSSettings.Project.special := tbFrames.Max;    // preserve the frame count when coming in from Load/Import etc.

    cbMatrixTypeChange(Nil);
  end
  else begin
    ps := DoNewProject(LMSSettings.Project, sbClear.Enabled);

    if not(ps.Valid) then Exit;

    LMSSettings.Project.MatrixMode       := ps.MatrixMode;
    LMSSettings.Project.Width            := ps.width;
    LMSSettings.Project.Height           := ps.height;
    LMSSettings.Project.Clear            := ps.clear;
    LMSSettings.Project.Special          := ps.special;
    LMSSettings.Project.SizeType         := ps.sizetype;
    LMSSettings.Project.CustomShape      := ps.CustomShape;
    LMSSettings.Project.CustomShapeParam := ps.CustomShapeParam;
    LMSSettings.Project.Pixel            := ps.pixel;
    LMSSettings.Project.Background       := ps.Background;

    tbFrames.Max                   := ps.special;
    frmPreviewPopout.tbFrames.Max  := tbFrames.Max;

    if LMSSettings.Project.MatrixMode = mtRGB then begin
      TUtility.ClearTExportOptions(True, LMSSettings.App.LastExport);
    end
    else begin
      TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);
    end;

    cbMatrixTypeChange(Nil);
  end;

  // ===========================================================================
  // ===========================================================================      

  mw := LMSSettings.Project.width;
  mh := LMSSettings.Project.height;

  if miPixelAuto.Checked then
    LMSSettings.PixelSize := GetAutoPixelSize;

  case LMSSettings.Project.pixel of
    psSquare    : miPixelShapeSquareClick(miPixelShapeSquare);
    psCircle    : miPixelShapeSquareClick(miPixelShapeRound);
    psRoundRect : miPixelShapeSquareClick(miPixelShapeRoundRect);
  end;

  sColour0.Brush.Color := LMSSettings.Project.Background;

  MatrixMain.NewMatrix(LMSSettings.Project.MatrixMode, LMSSettings.Project.special, CTopOffSet, CLeftOffset, mw, mh,
                       LMSSettings.PixelSize, TPixelShape(LMSSettings.PixelShape), miGridToggle.Checked, False, LMSSettings.Project.Clear,
                       LMSSettings.Project.Background);

  if (LMSSettings.Project.CustomShape <> csNone) then
    MatrixMain.SetDeadPixelsFromCustomShape(LMSSettings.Project.CustomShape, LMSSettings.Project.CustomShapeParam);

  // ===========================================================================

  if (LMSSettings.Project.clear) then begin
    if miFontMode.Checked then begin
      tbFrames.Max := FontCharacterCount;
    end
    else begin
      tbFrames.Max := MatrixMain.FrameCount;
    end;

    frmPreviewPopout.tbFrames.Max      := tbFrames.Max;
  end;

  // ===========================================================================

  ManageUIControls(false, false);

  // ===========================================================================

  OldMatrixMode := LMSSettings.Project.MatrixMode;

  ClearCurrentProjectFileName;
  LMSSettings.App.LastAutomationFileName := '';

  UpdateDisplay(-1);

  UpdateMemoryUsage;

  MatrixMain.Refresh;

  Screen.Cursor := crDefault;

  RecalculatePadding;
  MatrixOnChange(Nil);
  MatrixOnLayerChange(Nil);

  FormResize(Nil);
end;



procedure TfrmMain.sbClearClick(Sender: TObject);
begin
  MatrixMain.ClearCurrentFrame;
end;


procedure TfrmMain.sbGradientClick(Sender: TObject);
begin
  case sbGradient.Tag of
    0 : sbGradient.Tag := 1;
    1 : sbGradient.Tag := 2;
    2 : sbGradient.Tag := 0;
  end;

  ToggleGradient(TGradientOption(sbGradient.Tag), False); // inverses display

  FormResize(Nil);
end;


procedure TfrmMain.miMirrorAllFramesClick(Sender: TObject);
begin
  MatrixMain.PerformEffectController(modeMirrorAll, CMOMCurrentLayerFrames);
end;


procedure TfrmMain.sbScrollLeftClick(Sender: TObject);
var
  lDirection : integer;

begin
 case TSpeedButton(Sender).Tag of
    0 : lDirection := modeScrollLeft;
    1 : lDirection := modeScrollRight;
    2 : lDirection := modeScrollUp;
    3 : lDirection := modeScrollDown;
  else
    lDirection := modeScrollLeft;
  end;

  if (FFrameLayerPanel.SyncAll) then
    MatrixMain.PerformScrollController(lDirection, CMOMCurrentFrameLayers)
  else
    MatrixMain.PerformScrollController(lDirection, CMOMCurrentOnly);
end;


procedure TfrmMain.miIncrementRadiallyClick(Sender: TObject);
begin
  MatrixMain.PreviewIncRadially := miIncrementRadially.Checked;
end;


procedure TfrmMain.miInvertAllFramesClick(Sender: TObject);
begin
  MatrixMain.PerformEffectController(modeInvertAll, CMOMCurrentLayerFrames);
end;


function TfrmMain.GetColours: TColours;
var
  t : integer;

begin
  Result.DrawColours[CMouseLeft]   := sSelectionLMB.Brush.Color;
  Result.DrawColours[CMouseMiddle] := sSelectionMMB.Brush.Color;
  Result.DrawColours[CMouseRight]  := sSelectionRMB.Brush.Color;

  for t := 0 to 15 do
    Result.CustomColours[t] := RGBPalette[t].Brush.Color;

  for t := 0 to 27 do
    Result.PaletteHistory[t] := FFramePalettePanel.RGBPaletteHistory[t].Brush.Color;
end;


procedure TfrmMain.timerAnimateTimer(Sender: TObject);
begin
  SetFrameCaption(timerAnimate.Tag);

  MatrixMain.CurrentFrame := timerAnimate.Tag;

  if timerAnimate.Tag = tbFrames.Max then
    timerAnimate.Tag := 1
  else
    timerAnimate.Tag := timerAnimate.Tag + 1;
end;


function TfrmMain.BuildImportData(aStartFrame, aEndFrame : integer): TImportData;
begin
  Result.PadMode                   := LMSSettings.App.PadMode;
  Result.MatrixMode                := MatrixMain.Matrix.Mode;
  Result.ASCIIIndex                := LMSSettings.App.ASCIIIndex;
  Result.MaxFrames                 := tbFrames.Max;
  Result.AutomationFileName        := LMSSettings.App.LastAutomationFileName;

  Result.StartFrame                := aStartFrame;
  Result.EndFrame                  := aEndFrame;

  Result.Preview.Enabled           := MatrixMain.PreviewActive;
  Result.Preview.Size              := MatrixMain.PreviewBoxSize;
  Result.Preview.View              := MatrixMain.PreviewView;
  Result.Preview.Void              := MatrixMain.PreviewVoid;
  Result.Preview.Offset            := MatrixMain.RadialOffset;
  Result.Preview.OffsetDirection   := MatrixMain.RadialOffsetDirection;
  Result.Preview.IncrementRadially := MatrixMain.PreviewIncRadially;
end;


procedure TfrmMain.timerAutosaveTimer(Sender: TObject);
var
  ted : TImportData;
  lFileName : string;

begin
  if ((sbClear.Enabled) or (not MatrixMain.AnimPlaying)) and (MatrixMain.Matrix.Available) then begin

    ted := BuildImportData(1, MatrixMain.FrameCount);

    // =========================================================================

    lFileName := TUtility.GetAutoSaveName;

    MatrixMain.SaveAnimation(ExtractFilePath(Application.ExeName) + 'saves\autosave\' + lFileName, ted, LMSSettings.App.LastExport, GetColours);

    statusMain.SimpleText := GLanguageHandler.Text[kAutosavedCurrentMatrix] + ' (' + lFileName + ')';
  end;
end;


procedure TfrmMain.miClearAllUserMemoriesClick(Sender: TObject);
var
  t : integer;

begin
  if MessageDlg(GLanguageHandler.Text[kAreYouSure] + #13#13 +
                GLanguageHandler.Text[kClearAllUserMatrixBuffers] + #13#13#13 +
                '(' + GLanguageHandler.Text[kDoesNotClearAnimationFrames] + ')', mtWarning, [mbYes, mbNo], 0) = mrYes then begin

    MatrixMain.ClearUserBuffers;

    for t := 0 to 9 do begin
      MenuCopyMemory[t].ImageIndex    := -1;
      MenuRestoryMemory[t].ImageIndex := -1;
    end;
  end;
end;


procedure TfrmMain.miClearAllFramesLayerClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kClearAllFramesFromTheSelectedLayer] + #13#13 + GLanguageHandler.Text[kAreYouSure] + ' ' + GLanguageHandler.Text[kThisCannotBeUndone], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    MatrixMain.WipeAllFramesCurrentLayer;

    UpdateDisplay(1);

    ClearCurrentProjectFileName;
  end;
end;


procedure TfrmMain.miClearAllFramesClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kClearAllFramesAndLayers] + #13#13 + GLanguageHandler.Text[kAreYouSure] + ' ' + GLanguageHandler.Text[kThisCannotBeUndone], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    MatrixMain.WipeAllFramesAllLayers;

    UpdateDisplay(1);

    ClearCurrentProjectFileName;
  end;
end;


procedure TfrmMain.Importfrombitmap1Click(Sender: TObject);
var
  ted : TImportData;

begin
  if (not MatrixMain.Matrix.Available) then
    frmImportBitmap.cbCreateNew.Checked := True;

  frmImportBitmap.ShowModal;

  if frmImportBitmap.ImportMode <> ImportModeInvalid then begin

    if (not MatrixMain.Matrix.Available) or (frmImportBitmap.CreateNew) then begin
      LMSSettings.Project.width   := frmImportBitmap.FrameWidth;
      LMSSettings.Project.height  := frmImportBitmap.FrameHeight;

      LMSSettings.Project.Special := frmImportBitmap.FrameCount;

      LMSSettings.Project.Clear   := True;

      if frmImportBitmap.RGBImport then
        LMSSettings.Project.MatrixMode := mtRGB
      else
        LMSSettings.Project.MatrixMode := mtMono;

      MatrixMain.NewMatrix(LMSSettings.Project.MatrixMode, LMSSettings.Project.special, CTopOffSet, CLeftOffset, frmImportBitmap.FrameWidth, frmImportBitmap.FrameHeight,
                           LMSSettings.PixelSize, TPixelShape(LMSSettings.PixelShape), miGridToggle.Checked, False, True,
                           LMSSettings.Project.Background);
    end;

    case frmImportBitmap.ImportMode of
      ImportModeSingleImage    : begin
                                   ted := MatrixMain.ImportFromBMPSingleImage(frmImportBitmap.ImageFilename,
                                                                              frmImportBitmap.FrameCount,
                                                                              frmImportBitmap.FrameWidth,
                                                                              frmImportBitmap.FrameHeight,
                                                                              frmImportBitmap.RGBImport,
                                                                              frmImportBitmap.CreateNew);
                                 end;
      ImportModeMultipleImages : begin
                                   ted := MatrixMain.ImportFromBMPMultipleImage(frmImportBitmap.Pattern,
                                                                                frmImportBitmap.FirstFrame,
                                                                                frmImportBitmap.FrameCount,
                                                                                frmImportBitmap.PadLength,
                                                                                frmImportBitmap.FrameWidth,
                                                                                frmImportBitmap.FrameHeight,
                                                                                frmImportBitmap.RGBImport,
                                                                                frmImportBitmap.CreateNew);
                                 end;
    end;

    if (frmImportBitmap.FrameCount <> -1) then begin

      ClearCurrentProjectFileName;

      tbFrames.Max             := ted.NewFrames;

      if (MatrixMain.Matrix.Width <= 0) or (MatrixMain.Matrix.Height <= 0) then begin
        MatrixMain.Matrix.Width  := ted.NewWidth;
        MatrixMain.Matrix.Height := ted.NewHeight;
      end;

      frmPreviewPopout.tbFrames.Max := tbFrames.Max;

      bDeleteFrame.Enabled          := (tbFrames.Max > 1);
      bDeleteMultipleFrames.Enabled := (tbFrames.Max > 1);

      SetFrameCaption(MatrixMain.CurrentFrame);

      FormResize(Nil);

      MatrixMain.CurrentFrame := tbFrames.Position;

      ManageUIControls(False, False);
    end;
  end;
end;


procedure TfrmMain.ImportfromGIF1Click(Sender: TObject);
begin
  if timerAnimate.Enabled then
    bPlayAnimationClick(bStopAnimation);

  // =======================================================================

  if sbClear.Enabled then begin
    if MessageDlg(GLanguageHandler.Text[kOpeningNewMatrixWillClearCurrentProject] + #13#13 +
                  GLanguageHandler.Text[kDoYouWishToContinue], mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  // =======================================================================

  opdMain.InitialDir := LMSSettings.App.LastLoadLocation;

  if opdMain.Execute then begin
    LoadFromGIF(opdMain.FileName);

    FormResize(Nil);
  end;
end;


procedure TfrmMain.miUndoToolbarClick(Sender: TObject);
begin
  pUndoToolbar.Visible := not(pUndoToolbar.Visible);

  if (pUndoToolbar.Visible) then
    FFrameUndoPanel.SetUndos(MatrixMain.GetUndoCount);
end;


procedure TfrmMain.miUnlockAllClick(Sender: TObject);
begin
  MatrixMain.LockUnLockRange(1, MatrixMain.FrameCount, False);
end;


procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;


procedure TfrmMain.miChangeColoursFrameClick(Sender: TObject);
var
  lCCO : TColourChangeObject;
  lColours : TStringList;

begin
  lColours := TStringList.Create;

  MatrixMain.GetFirst32Colours(lColours);

  lCCO := DoColourChange(lColours);

  if lCCO.Process then begin
    case TMenuItem(Sender).Tag of
      0 : MatrixMain.ChangeColourCurrent(lCCO.ColourFrom, lCCO.ColourTO);
      1 : MatrixMain.ChangeColourCurrentLayer(lCCO.ColourFrom, lCCO.ColourTO);
      2 : MatrixMain.ChangeColourAll(lCCO.ColourFrom, lCCO.ColourTO);
    end;
  end;

  lColours.Free;
end;


procedure TfrmMain.Checkforupdates1Click(Sender: TObject);
begin
  CheckForNewVersion(LEDStudioVersion, LEDStudioDate, 'led.dat', false);
end;


procedure TfrmMain.miClearAllDeadPixelsClick(Sender: TObject);
begin
  MatrixMain.SetDeadPixels(ptNormal);
end;


procedure TfrmMain.miClearAllFramesGradientClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kClearAllFramesQ] + #13#13 + GLanguageHandler.Text[kAreYouSure], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    MatrixMain.ClearAllFramesGradient(sbGradient.Tag);

    UpdateDisplay(1);

    ClearCurrentProjectFileName;
  end;
end;


procedure TfrmMain.miCodeTemplatesClick(Sender: TObject);
begin
  DoExportCode(MatrixMain.Matrix.Mode);
end;


procedure TfrmMain.Colour01Click(Sender: TObject);
var
  lColumn : integer;

begin
  for lColumn := 0 to MatrixMain.Matrix.Width - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[tbFrames.Position].Grid[lColumn, puGradient.Tag] <> 0 then begin
      MatrixMain.MatrixLayers[0].Frames[tbFrames.Position].Grid[lColumn, puGradient.Tag] := TMenuItem(Sender).Tag; // to do, no idea what this does
    end;
  end;

  MatrixGradient[puGradient.Tag].Brush.Color   := MatrixMain.LEDColours[TMenuItem(Sender).Tag];
  MatrixMain.Render.GradientIY[puGradient.Tag] := TMenuItem(Sender).Tag;
end;


procedure TfrmMain.Copyandshiftleft1Click(Sender: TObject);
begin
  MatrixMain.PasteSpecial(TMenuItem(Sender).Tag);
end;


procedure TfrmMain.Examples1Click(Sender: TObject);
begin
  TUtility.ExecuteFile(0, '"' + ExtractFilePath(Application.ExeName) + 'example code\' + '"', '', '')
end;


procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;


procedure TfrmMain.miExportToGIFClick(Sender: TObject);
var
  ego : TExportGIFObject;

begin
  ego := DoExportGIF(LMSSettings.ExportGIFSettings);

  if (ego.Process) then begin
    MatrixMain.ExportToGIF(ego.Background, ego.PixelSize, ego.PixelShape, ego.AnimationSpeed, ego.FileName);

    LMSSettings.ExportGIFSettings.FileName   := ego.FileName;
    LMSSettings.ExportGIFSettings.PixelSize  := ego.PixelSize;
    LMSSettings.ExportGIFSettings.PixelShape := ego.PixelShape;
    LMSSettings.ExportGIFSettings.Background := ego.Background;
  end;
end;


procedure TfrmMain.miExportToBitmapClick(Sender: TObject);
begin
   spdMain.Filter := GLanguageHandler.Text[kBitmapImages] + ' (*.bmp)|*.bmp';

  if spdMain.Execute then
    MatrixMain.ExportToBitmap(spdMain.FileName);
end;


procedure TfrmMain.miSaveSingleFrameClick(Sender: TObject);
var
  ted : TImportData;

begin
  ConfigureSaveDialog(CSaveProject);

  if LMSSettings.App.DataFilename = '' then begin
    sdMain.FileName   := GLanguageHandler.Text[kFrame] + '_' + IntToStr(tbFrames.Position);
  end
  else begin
    sdMain.FileName   := TUtility.GetFilenameNoExt(ExtractFilename(LMSSettings.App.DataFilename)) + '_' + GLanguageHandler.Text[kFrame] + '_' + IntToStr(tbFrames.Position);
    sdMain.InitialDir := ExtractFilePath(LMSSettings.App.DataFilename);
  end;

  if sdMain.Execute then begin
    ted.MatrixMode := LMSSettings.Project.MatrixMode;

    MatrixMain.SaveSingleFrame(sdMain.FileName, ted, tbFrames.Position);
  end;
end;


procedure TfrmMain.miSaveClick(Sender: TObject);
var
  ted : TImportData;

begin
  if LMSSettings.App.DataFilename = '' then
    miSaveAsClick(Nil)
  else begin
    ted := BuildImportData(1, MatrixMain.FrameCount);

    if MatrixMain.SoftwareMode = smFont then
      MatrixMain.SaveFont(LMSSettings.App.DataFilename, ted, LMSSettings.App.LastExport)
    else
      MatrixMain.SaveAnimation(LMSSettings.App.DataFilename, ted, LMSSettings.App.LastExport, GetColours);
  end;
end;


procedure TfrmMain.miSaveGradientClick(Sender: TObject);
var
  s,g : string;
  t : integer;
  tf : TextFile;

begin
  s := InputBox(GLanguageHandler.Text[kSaveGradient], GLanguageHandler.Text[kName], GLanguageHandler.Text[kMyGradient]);

  if (s <> '') then begin
    AssignFile(tf, ExtractFilePath(Application.ExeName) + 'gradients\' + s + '.ledsgradient');
    Rewrite(tf);

    Writeln(tf, '{' + kGradientFileHeader);

    g := '';
    for t := 0 to MatrixMain.Matrix.Height - 1 do
      g := g + IntToStr(MatrixMain.Render.GradientIY[t]) + ' ';

    Writeln(tf, kGradientColour + ':' + g);
    Writeln(tf, kDataBlockEnd);

    CloseFile(tf);
  end;
end;


procedure TfrmMain.miSaveRangeClick(Sender: TObject);
var
  lSFRO : TSaveFrameRangeObject;
  ted : TImportData;
  lFileName : string;

begin
  lSFRO := DoSaveFrameRange(MatrixMain.FrameCount);

  if lSFRO.Process then begin
    if ((sbClear.Enabled) or (not MatrixMain.AnimPlaying)) and (MatrixMain.Matrix.Available) then begin

      ted := BuildImportData(lSFRO.StartFrame, lSFRO.EndFrame);

      // =========================================================================

      lFileName := TUtility.GetAutoSaveName;

      MatrixMain.SaveAnimation(ExtractFilePath(Application.ExeName) + 'saves\autosave\' + lFileName, ted, LMSSettings.App.LastExport, GetColours);

      statusMain.SimpleText := GLanguageHandler.Text[kAutosavedCurrentMatrixRange] + ' (' + lFileName + ')';
    end;
  end;
end;


procedure TfrmMain.miSaveAsClick(Sender: TObject);
var
  ted : TImportData;

begin
  ConfigureSaveDialog(CSaveProject);

  if sdMain.Execute then begin

    ted := BuildImportData(1, MatrixMain.FrameCount);

    if (MatrixMain.SoftwareMode = smFont) then
      MatrixMain.SaveFont(sdMain.Filename, ted, LMSSettings.App.LastExport)
    else
      MatrixMain.SaveAnimation(sdMain.Filename, ted, LMSSettings.App.LastExport, GetColours);

    SetCurrentProjectFileName(sdMain.FileName);
  end;
end;


procedure TfrmMain.miSaveAsFontClick(Sender: TObject);
begin
  ConfigureSaveDialog(CSaveFont);

  // ===========================================================================

  if sdMain.Execute then begin
    if MatrixMain.Matrix.Mode = mtRGB then
      MatrixMain.SaveAsRGBFont(sdMain.Filename)
    else
      MatrixMain.SaveAsFont(sdMain.Filename);
  end;
end;


procedure TfrmMain.miExportUserMemoriesClick(Sender: TObject);
begin
  ExportData(LMSSettings.App.LastExport, esUserMemories, MatrixMain.Matrix.Mode);
end;


procedure TfrmMain.miGradientAllFramesClick(Sender: TObject);
begin
  MatrixMain.PerformEffectController(modeGradientAll, CMOMCurrentLayerFrames);
end;


procedure TfrmMain.miGradientRGB3BPP1Click(Sender: TObject);
var
  lColumn : integer;

begin
  for lColumn := 0 to MatrixMain.Matrix.Width - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[tbFrames.Position].Grid[lColumn, puGradient.Tag] <> 0 then begin
      MatrixMain.MatrixLayers[0].Frames[tbFrames.Position].Grid[lColumn, puGradient.Tag] := TMenuItem(Sender).Tag; // to do no idea what this does
    end;
  end;

  MatrixGradient[puGradientRGB_3BPP.Tag].Brush.Color  := MatrixMain.LEDRGB3BPPColours[TMenuItem(Sender).Tag];

  if sbGradient.Tag = 1 then
    MatrixMain.Render.GradientIY[puGradientRGB_3BPP.Tag] := TMenuItem(Sender).Tag
  else
    MatrixMain.Render.GradientIX[puGradientRGB_3BPP.Tag] := TMenuItem(Sender).Tag;
end;


procedure TfrmMain.miGridToggleClick(Sender: TObject);
begin
  miGridToggle.Checked := not(miGridToggle.Checked);

  MatrixMain.ChangeGrid(miGridToggle.Checked);
end;


procedure TfrmMain.Help1Click(Sender: TObject);
begin
  if FileExists(ExtractFilePath(Application.ExeName) + 'help\en\help.pdf') then
    TUtility.ExecuteFile(0, ExtractFilePath(Application.ExeName) + 'help\en\help.pdf', '', '')
  else
    MessageDlg(GLanguageHandler.Text[kHelpFileNotFound], mtWarning, [mbOK], 0);
end;


procedure TfrmMain.Showshortcutkeys1Click(Sender: TObject);
begin
  if FileExists(ExtractFilePath(Application.ExeName) + 'help\en\shortcuts.pdf') then
    TUtility.ExecuteFile(0, ExtractFilePath(Application.ExeName) + 'help\en\shortcuts.pdf', '', '')
  else
    MessageDlg(GLanguageHandler.Text[kShortcutHelpFileNotFound], mtWarning, [mbOK], 0);
end;


procedure TfrmMain.miExportAnimationToBitmapClick(Sender: TObject);
begin
  spdMain.Filter := GLanguageHandler.Text[kBitmapImages] + ' (*.bmp)|*.bmp';

  if spdMain.Execute then
    MatrixMain.ExportAnimationToBitmap(spdMain.FileName);
end;


procedure TfrmMain.miExportClick(Sender: TObject);
var
  teo : TExportOptions;

begin
  teo := ExportData(LMSSettings.App.LastExport, esAnimation, MatrixMain.Matrix.Mode);

  if teo.Valid then begin
    LMSSettings.App.LastExport.ExportMode      := teo.ExportMode;

    LMSSettings.App.LastExport.StartFrame      := teo.StartFrame;
    LMSSettings.App.LastExport.EndFrame        := teo.EndFrame;
    LMSSettings.App.LastExport.Source          := teo.Source;
    LMSSettings.App.LastExport.Orientation     := teo.Orientation;
    LMSSettings.App.LastExport.ScanDirection   := teo.ScanDirection;
    LMSSettings.App.LastExport.LSB             := teo.LSB;
    LMSSettings.App.LastExport.Language        := teo.Language;
    LMSSettings.App.LastExport.NumberFormat    := teo.NumberFormat;
    LMSSettings.App.LastExport.NumberSize      := teo.NumberSize;
    LMSSettings.App.LastExport.LineContent     := teo.LineContent;
    LMSSettings.App.LastExport.LineCount       := teo.LineCount;
    LMSSettings.App.LastExport.RGBEnabled      := teo.RGBEnabled;
    LMSSettings.App.LastExport.RGBMode         := teo.RGBMode;
    LMSSettings.App.LastExport.RGBChangePixels := teo.RGBChangePixels;
    LMSSettings.App.LastExport.RGBChangeColour := teo.RGBChangeColour;
    LMSSettings.App.LastExport.RGBBrightness   := teo.RGBBrightness;

    SetSimpleExport(teo);
  end;
end;


procedure TfrmMain.SetSimpleExport(aTEO : TExportOptions);
begin
{  cbSource.ItemIndex          := Ord(aTEO.Source); //TO DO

  cbSourceChange(Nil);

  cbSourceLSB.ItemIndex       := Ord(aTEO.LSB);
  cbSourceDirection.ItemIndex := Ord(aTEO.Orientation);

  cbRowsLSBChange(Nil);}
end;


procedure TfrmMain.SetFrameCaption(i : integer);
begin
  if (MatrixMain.SoftwareMode = smFont) then begin
    lFrame.Caption     := Char(i + LMSSettings.App.ASCIIIndex - 1);
    pASCIICode.Caption := 'ASCII: ' + IntToStr(i + LMSSettings.App.ASCIIIndex - 1);

    FFrameFontPanel.SetFont(i + LMSSettings.App.ASCIIIndex - 1);
  end
  else begin
    lFrame.Caption     := IntToStr(i) + ' / ' + IntToStr(tbFrames.Max);
  end;

  frmPreviewPopout.lFrame.Caption := lFrame.Caption;

  if tbFrames.Position <> i then
    tbFrames.Position := i;

  if frmPreviewPopout.tbFrames.Position <> i then
    frmPreviewPopout.tbFrames.Position := i;

  lFrame.Refresh;
end;


procedure TfrmMain.UpdateMemoryUsage;
var
  lSize : integer;
  lDimensions : string;

begin
  lSize       := MatrixMain.CalculateMemoryUsage;

  lDimensions := IntToStr(MatrixMain.Matrix.Width) + ' x ' + IntToStr(MatrixMain.Matrix.Height);

  if (lSize < 32768) then
    lMemoryUsage.Caption := lDimensions + ', ' + GLanguageHandler.Text[kBytes] + ' ' + IntToStr(lSize) + ' ' + GLanguageHandler.Text[kBytes]
  else if (lSize < 1048576) then
    lMemoryUsage.Caption := lDimensions + ', ' + GLanguageHandler.Text[kBytes] + ' ' + FloatToStrF((lSize / 1024), ffFixed, 7, 3) + ' KB'
  else
    lMemoryUsage.Caption := lDimensions + ', ' + GLanguageHandler.Text[kBytes] + ' ' + FloatToStrF((lSize / 1048576), ffFixed, 7, 3) + ' MB';

  lMemoryUsage.Caption := lMemoryUsage.Caption + ' (' + TUtility.GetTypeName(MatrixMain.Matrix.Mode) + ')';

  lMemoryUsage.Hint := IntToStr(lSize) + ' ' + GLanguageHandler.Text[kBytes];
end;


procedure TfrmMain.Website1Click(Sender: TObject);
begin
  TUtility.ExecuteFile(0, 'https://github.com/MaximumOctopus/LEDMatrixStudio', '', '');
end;


procedure TfrmMain.witter1Click(Sender: TObject);
begin
  TUtility.ExecuteFile(0, 'http://www.twitter.com/maximumoctopus', '', '');
end;


procedure TfrmMain.miPreviewx1Click(Sender: TObject);
begin
  if (not MatrixMain.PreviewPopout) then begin
    MatrixMain.PreviewBoxSize := previewSizes[TMenuItem(Sender).Tag];

    SyncPreviewSize(TMenuItem(Sender).Tag);

    FormResize(Nil);
  end;
end;


procedure TfrmMain.SyncPreviewSize(aSize : integer);
begin
  PreviewMenuSize[0, aSize].Checked := True;
  PreviewMenuSize[1, aSize].Checked := True;
end;


procedure TfrmMain.SyncPreviewView(aView : integer);
begin
  PreviewMenuView[0, aView].Checked := True;
  PreviewMenuView[1, aView].Checked := True;
end;


procedure TfrmMain.SyncPreviewVoid(aVoid : integer);
begin
  PreviewMenuVoid[0, aVoid].Checked := True;
  PreviewMenuVoid[1, aVoid].Checked := True;
end;


procedure TfrmMain.RecalculatePadding;
begin
  case LMSSettings.App.PadMode of
    pfAuto : begin
          case MatrixMain.Matrix.Width of
            1..8   : LMSSettings.App.PadModeHexRow := 2;
            9..16  : LMSSettings.App.PadModeHexRow := 4;
            17..24 : LMSSettings.App.PadModeHexRow := 6;
            25..32 : LMSSettings.App.PadModeHexRow := 8;
            33..40 : LMSSettings.App.PadModeHexRow := 10;
            41..48 : LMSSettings.App.PadModeHexRow := 12;
            49..56 : LMSSettings.App.PadModeHexRow := 14;
            57..64 : LMSSettings.App.PadModeHexRow := 16;
          end;

          case MatrixMain.Matrix.Height of
            1..8   : LMSSettings.App.PadModeHexCol := 2;
            9..16  : LMSSettings.App.PadModeHexCol := 4;
            17..24 : LMSSettings.App.PadModeHexCol := 6;
            25..32 : LMSSettings.App.PadModeHexCol := 8;
            33..40 : LMSSettings.App.PadModeHexCol := 10;
            41..48 : LMSSettings.App.PadModeHexCol := 12;
            49..56 : LMSSettings.App.PadModeHexCol := 14;
            57..64 : LMSSettings.App.PadModeHexCol := 16;
          end;

          if MatrixMain.Matrix.Mode > mtMono then begin
            LMSSettings.App.PadModeHexRow := LMSSettings.App.PadModeHexRow * 2;
            LMSSettings.App.PadModeHexCol := LMSSettings.App.PadModeHexCol * 2;
          end;
        end;
    pf8Bits : begin
          LMSSettings.App.PadModeHexRow := 2;
          LMSSettings.App.PadModeHexCol := 2;
        end;
    pf16Bits : begin
          LMSSettings.App.PadModeHexRow := 4;
          LMSSettings.App.PadModeHexCol := 4;
        end;
    pf24Bits : begin
          LMSSettings.App.PadModeHexRow := 6;
          LMSSettings.App.PadModeHexCol := 6;
        end;
    pf32Bits : begin
          LMSSettings.App.PadModeHexRow := 8;
          LMSSettings.App.PadModeHexCol := 8;
        end;
    pf40Bits : begin
          LMSSettings.App.PadModeHexRow := 10;
          LMSSettings.App.PadModeHexCol := 10;
        end;
    pf48Bits : begin
          LMSSettings.App.PadModeHexRow := 12;
          LMSSettings.App.PadModeHexCol := 12;
        end;
    pf56Bits : begin
          LMSSettings.App.PadModeHexRow := 14;
          LMSSettings.App.PadModeHexCol := 14;
        end;
    pf64Bits : begin
          LMSSettings.App.PadModeHexRow := 16;
          LMSSettings.App.PadModeHexCol := 16;
        end;
{    9 : begin // no idea what this is :(
          LMSSettings.App.PadModeHexRow := 1;
          LMSSettings.App.PadModeHexCol := 1;
        end;    }
  end;
end;


procedure TfrmMain.Rotateanticlockwise1Click(Sender: TObject);
begin
  MatrixMain.RotateCopyBrush(modeRotateACW);
end;


procedure TfrmMain.Rotateclockwise1Click(Sender: TObject);
begin
  MatrixMain.RotateCopyBrush(modeRotateCW);
end;


procedure TfrmMain.Loadpattern1Click(Sender: TObject);
begin
  ConfigureOpenDialog(CLoadIgnorePixels);

  if odMain.Execute then begin
    MatrixMain.SetDeadPixelsFromFileName(odMain.FileName);
  end;
end;


procedure TfrmMain.Savepattern1Click(Sender: TObject);
begin
  ConfigureSaveDialog(CSaveIgnorePixels);

  if sdMain.Execute then begin
    MatrixMain.SaveDeadPixels(sdMain.FileName);
  end;
end;


procedure TfrmMain.miRGBPaletteToolbarClick(Sender: TObject);
//var
//  i : integer;

begin
{   FormResize(Nil);
  panelRGBPalette.Visible := not(panelRGBPalette.Visible);

  LMSSettings.App.TopOffset := 2; // to do, get rid

  MatrixMain.SetYPos(LMSSettings.App.TopOffset);

  for i := 0 to MatrixMain.Matrix.Width - 1 do
    ColLabels[i].Top := LMSSettings.App.TopOffset + 4 + ((MatrixMain.Matrix.Height) * MatrixMain.MatrixPixelSize);

  for i := 0 to MatrixMain.Matrix.Height - 1 do
    RowLabels[i].Top := LMSSettings.App.TopOffset + Round((LMSSettings.PixelSize - 2 - RowLabels[i].Height) / 2) + (i * MatrixMain.MatrixPixelSize);}
end;


procedure TfrmMain.SetupMatrixColours;
var
  lColour : integer;

begin
  for lColour := 0 to 5 do begin
    if LMSSettings.Project.MatrixMode = mtMono then
      MatrixMain.LEDColours[lColour] := MatrixMain.LEDColoursSingle[lColour]
    else
      MatrixMain.LEDColours[lColour] := MatrixMain.LEDColoursBi[lColour];
  end;

  case LMSSettings.Project.MatrixMode of
    mtRGB      : begin
                   sColour0.Brush.Color      := MatrixMain.RGBBackground;

                   if (MatrixMain.LEDRGBColours[CMouseLeft] < 10) and (MatrixMain.LEDRGBColours[CMouseMiddle] < 10) and (MatrixMain.LEDRGBColours[CMouseRight] < 10) then begin
                     MatrixMain.LEDRGBColours[CMouseLeft]   := $000000ff;
                     MatrixMain.LEDRGBColours[CMouseMiddle] := $00ff0000;
                     MatrixMain.LEDRGBColours[CMouseRight]  := $00000000;
                   end;

                   sSelectionLMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseLeft];
                   sSelectionMMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseMiddle];
                   sSelectionRMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseRight];

                   MatrixMain.SetMouseButtonColours(MatrixMain.LEDRGBColours[CMouseLeft],
                                                    MatrixMain.LEDRGBColours[CMouseMiddle],
                                                    MatrixMain.LEDRGBColours[CMouseRight]);
                 end;
    mtRGB3BPP  : begin
                   sColour0.Brush.Color      := MatrixMain.RGBBackground;

                   MatrixMain.LEDRGBColours[CMouseLeft]   := 4; // red
                   MatrixMain.LEDRGBColours[CMouseMiddle] := 2; // green
                   MatrixMain.LEDRGBColours[CMouseRight]  := 0; // black

                   sSelectionLMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[4];
                   sSelectionMMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[2];
                   sSelectionRMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[0];

                   MatrixMain.SetMouseButtonColours(4, 2, 0);
                 end;
  else
    sColour0.Brush.Color      := MatrixMain.LEDColours[0];

    sSelectionLMB.Brush.Color := MatrixMain.LEDColours[sSelectionLMB.Tag];
    sSelectionMMB.Brush.Color := MatrixMain.LEDColours[sSelectionMMB.Tag];
    sSelectionRMB.Brush.Color := MatrixMain.LEDColours[sSelectionRMB.Tag];

    MatrixMain.SetMouseButtonColours(sSelectionLMB.Tag,
                                     sSelectionMMB.Tag,
                                     sSelectionRMB.Tag);
  end;

  sColour1.Brush.Color       := MatrixMain.LEDColours[CMouseLeft];
  sColour2.Brush.Color       := MatrixMain.LEDColours[CMouseMiddle];
  sColour3.Brush.Color       := MatrixMain.LEDColours[CMouseRight];
end;


procedure TfrmMain.Shape16MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lColour : integer;

begin
  if (MatrixMain.Matrix.Mode = mtRGB) then begin
    lColour := TShape(Sender).Brush.Color;

    if ssLeft in Shift then begin
      sSelectionLMB.Brush.Color              := lColour;

      MatrixMain.LEDRGBColours[CMouseLeft]   := lColour;
    end
    else if ssMiddle in Shift then begin
      sSelectionMMB.Brush.Color              := lColour;

      MatrixMain.LEDRGBColours[CMouseMiddle] := lColour;
    end
    else if ssRight in Shift then begin
      sSelectionRMB.Brush.Color              := lColour;

      MatrixMain.LEDRGBColours[CMouseRight]  := lColour;
    end;

    MatrixMain.SetMouseButtonColours(MatrixMain.LEDRGBColours[CMouseLeft],
                                     MatrixMain.LEDRGBColours[CMouseMiddle],
                                     MatrixMain.LEDRGBColours[CMouseRight]);

    if (TShape(Sender).Tag <> 999) then
      GenerateShades(TShape(Sender).Brush.Color);
  end;
end;


procedure TfrmMain.Shape47MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lColour : integer;

begin
  if (MatrixMain.Matrix.Mode = mtRGB3BPP) then begin
    lColour := TShape(Sender).Tag;

    if ssLeft in Shift then begin
      sSelectionLMB.Brush.Color              := TShape(Sender).Brush.Color;

      MatrixMain.LEDRGBColours[CMouseLeft]   := lColour;
    end
    else if ssMiddle in Shift then begin
      sSelectionMMB.Brush.Color              := TShape(Sender).Brush.Color;

      MatrixMain.LEDRGBColours[CMouseMiddle] := lColour;
    end
    else if ssRight in Shift then begin
      sSelectionRMB.Brush.Color              := TShape(Sender).Brush.Color;

      MatrixMain.LEDRGBColours[CMouseRight]  := lColour;
    end;

    MatrixMain.SetMouseButtonColours(MatrixMain.LEDRGBColours[CMouseLeft],
                                     MatrixMain.LEDRGBColours[CMouseMiddle],
                                     MatrixMain.LEDRGBColours[CMouseRight]);
  end;
end;


procedure TfrmMain.OnGradientClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 begin
  case MatrixMain.Matrix.Mode of
    mtRGB     : begin
                  puGradientRGB.Tag := TShape(Sender).Tag;

                  puGradientRGB.Popup(Left + TShape(Sender).Left + 10, Top + pCanvas.Top + TShape(Sender).Top + 20);
                end;
    mtRGB3BPP : begin
                  puGradientRGB_3BPP.Tag := TShape(Sender).Tag;

                  puGradientRGB_3BPP.Popup(Left + TShape(Sender).Left + 10, Top + pCanvas.Top + TShape(Sender).Top + 20);
                end;
  else
    puGradient.Tag := TShape(Sender).Tag;

    puGradient.Popup(Left + TShape(Sender).Left + 10, Top + pCanvas.Top + TShape(Sender).Top + 20);
  end;
end;


procedure TfrmMain.Openautosavefolder1Click(Sender: TObject);
 begin
  TUtility.ExecuteFile(0, ExtractFilePath(Application.ExeName) + 'saves\autosave\', '', '')
end;


procedure TfrmMain.Optimisedata1Click(Sender: TObject);
begin
  DoOptimise;
end;


procedure TfrmMain.ToggleGradient(aGradientMode : TGradientOption; cleargradient : boolean);
var
  t : integer;

begin
  if MatrixMain.Matrix.Mode = mtMono then begin
    iMMBGradient.Visible  := False;
    sSelectionMMB.Visible := False;
  end
  else begin
    case aGradientMode of
      goOff        : for t := 0 to _MaxHeight do begin
                       if MatrixGradient[t] <> Nil then
                       MatrixGradient[t].Visible := False;

                       iMMBGradient.Visible  := False;
                       sSelectionMMB.Visible := True;
                     end;
      goVertical   : for t := 0 to MatrixMain.Matrix.Height - 1 do begin
                       if MatrixGradient[t] = Nil then begin
                         MatrixGradient[t] := TShape.Create(Self);
                       end;

                       with MatrixGradient[t] do begin
                         Parent      := pCanvas;
                         Visible     := True;
                         Tag         := t;
                         OnMouseDown := OnGradientClick;

                         if MatrixMain.PreviewActive then
                           Left := CLeftOffset + ((MatrixMain.Matrix.Width + 1) * LMSSettings.PixelSize) + (MatrixMain.Matrix.Width * MatrixMain.PreviewBoxSize) + 25
                         else
                           Left := CLeftOffset + ((MatrixMain.Matrix.Width + 1) * LMSSettings.PixelSize);

                         Top  := CTopOffset + (t * LMSSettings.PixelSize);

                         if Width <> LMSSettings.PixelSize + 1 then
                           Width := LMSSettings.PixelSize + 1;

                         if Height <> LMSSettings.PixelSize + 1 then
                           Height := LMSSettings.PixelSize + 1;

                         Brush.Color := MatrixMain.Render.GradientIY[t];

                         if (cleargradient) then begin
                           if MatrixMain.Matrix.Mode = mtRGB then
                             MatrixMain.Render.GradientIY[t] := MatrixMain.RGBBackground
                           else
                             MatrixMain.Render.GradientIY[t] := 0;
                         end;
                       end;

                       iMMBGradient.Visible  := True;
                       sSelectionMMB.Visible := False
                     end;
      goHorizontal : for t := 0 to MatrixMain.Matrix.Width - 1 do begin
                       if MatrixGradient[t] = Nil then begin
                         MatrixGradient[t] := TShape.Create(Self);
                       end;

                       with MatrixGradient[t] do begin
                         Parent      := pCanvas;
                         Visible     := True;
                         Tag         := t;
                         OnMouseDown := OnGradientClick;

                         Left := CLeftOffset + (t * LMSSettings.PixelSize);
                         Top  := CTopOffset + ((MatrixMain.Matrix.Height + 1) * LMSSettings.PixelSize);

                         if Width <> LMSSettings.PixelSize + 1 then
                           Width := LMSSettings.PixelSize + 1;

                         if Height <> LMSSettings.PixelSize + 1 then
                           Height := LMSSettings.PixelSize + 1;

                         Brush.Color := MatrixMain.Render.GradientIX[t];

                         if (cleargradient) then begin
                           if MatrixMain.Matrix.Mode = mtRGB then
                             MatrixMain.Render.GradientIX[t] := MatrixMain.RGBBackground
                           else
                             MatrixMain.Render.GradientIX[t] := 0;
                         end;
                       end;

                       iMMBGradient.Visible  := True;
                       sSelectionMMB.Visible := False
                     end;
    end;
  end;

  sbGradient.Tag             := Ord(aGradientMode);

  MatrixMain.Render.Gradient := aGradientMode;
end;


procedure TfrmMain.SelectGradient(Sender : TObject);
var
  s, temp : string;
  t : integer;

begin
  if MessageDlg(GLanguageHandler.Text[kReallyLoadThisGradient], mtWarning, [mbYes, mbNo], 0) = mrYes then begin

    temp := ExtractFilePath(Application.ExeName) + 'gradients\' + TMenuItem(Sender).Caption + '.ledsgradient';
    s    := '';

    for t := 1 to length(temp) do
      if temp[t] <> '&' then
        s := s + temp[t];

    if FileExists(s) then begin
      MatrixMain.LoadGradient(s);

      UpdateGradientColours;
    end
    else
      MessageDlg(GLanguageHandler.Text[kCannotFindGradientFile] + #13#10 + #13#10 + '"' + s + '"', mtError, [mbOK], 0);
  end;
end;


procedure TfrmMain.UpdateGradientColours;
var
  t : integer;

begin
  case MatrixMain.Render.Gradient of
    goVertical   : begin
                     for t := 0 to MatrixMain.Matrix.Height - 1 do begin
                       if (MatrixMain.Matrix.Mode = mtBiSequential) or
                          (MatrixMain.Matrix.Mode = mtBiBitPlanes) then
                         MatrixGradient[t].Brush.Color := MatrixMain.LEDColours[MatrixMain.Render.GradientIY[t]]
                       else
                         MatrixGradient[t].Brush.Color := MatrixMain.Render.GradientIY[t];
                     end;
                   end;
    goHorizontal : begin
                     for t := 0 to MatrixMain.Matrix.Width - 1 do begin
                       if (MatrixMain.Matrix.Mode = mtBiSequential) or
                          (MatrixMain.Matrix.Mode = mtBiBitPlanes) then
                         MatrixGradient[t].Brush.Color := MatrixMain.LEDColours[MatrixMain.Render.GradientIX[t]]
                       else
                         MatrixGradient[t].Brush.Color := MatrixMain.Render.GradientIX[t];
                     end;
                   end;
  end;
end;


procedure TfrmMain.miGradientSelectRGBClick(Sender: TObject);
begin
  colorDialog.Color := MatrixGradient[puGradientRGB.Tag].Brush.Color;

  if colorDialog.Execute then begin
    MatrixGradient[puGradientRGB.Tag].Brush.Color  := colorDialog.Color;

    if sbGradient.Tag = 1 then
      MatrixMain.Render.GradientIY[puGradientRGB.Tag] := colorDialog.Color
    else
      MatrixMain.Render.GradientIX[puGradientRGB.Tag] := colorDialog.Color;
  end;
end;


procedure TfrmMain.miGradFromClick(Sender: TObject);
var
  rdy, gdy, bdy : integer;
  rdx, gdx, bdx : double;
  newr, newg, newb : double;
  y : integer;
  colstart, colend : integer;
  lEnd : integer;
  newri, newgi, newbi : integer;

begin
  if sbGradient.Tag = 1 then begin
    colstart   := MatrixMain.Render.GradientIY[0];
    colend     := MatrixMain.Render.GradientIY[MatrixMain.Matrix.Height - 1];

    lEnd       := MatrixMain.Matrix.Height - 1;
  end
  else begin
    colstart   := MatrixMain.Render.GradientIX[0];
    colend     := MatrixMain.Render.GradientIX[MatrixMain.Matrix.Width - 1];

    lEnd       := MatrixMain.Matrix.Width - 1;
  end;

  rdy  := (colend and $0000FF) - (colstart and $0000FF);
  gdy  := ((colend and $00FF00) shr 8) - ((colstart and $00FF00) shr 8);
  bdy  := ((colend and $FF0000) shr 16) - ((colstart and $FF0000) shr 16);

  newr := (colstart and $0000FF);
  newg := (colstart and $00FF00) shr 8;
  newb := (colstart and $FF0000) shr 16;

  rdx  := rdy / lEnd;
  gdx  := gdy / lEnd;
  bdx  := bdy / lEnd;

  for y := 1 to lEnd - 1 do begin
    newr  := newr + rdx;
    newg  := newg + gdx;
    newb  := newb + bdx;

    newri := Floor(newr);
    newgi := Floor(newg);
    newbi := Floor(newb);

    MatrixGradient[y].Brush.Color  := (newbi shl 16) + (newgi shl 8) + newri;

    if sbGradient.Tag = 1 then
      MatrixMain.Render.GradientIY[y] := (newbi shl 16) + (newgi shl 8) + newri
    else
      MatrixMain.Render.GradientIX[y] := (newbi shl 16) + (newgi shl 8) + newri;
  end;
end;


procedure TfrmMain.miGradientBottomTopClick(Sender: TObject);
var
  y : integer;
  lEnd, lTempColour, lIndex : integer;
  lColours : TStringList;

begin
  lColours := TStringList.Create;

  if sbGradient.Tag = 1 then begin
    lEnd       := MatrixMain.Matrix.Height - 1;
  end
  else begin
    lEnd       := MatrixMain.Matrix.Width - 1;
  end;

  for y := 0 to lEnd do
    lColours.Add(IntToStr(MatrixGradient[y].Brush.Color));

  lIndex := lColours.Count - 1;

  for y := 0 to lEnd do begin
    lTempColour := StrToInt(lColours[lIndex]);

    MatrixGradient[y].Brush.Color  := lTempColour;

    if sbGradient.Tag = 1 then
      MatrixMain.Render.GradientIY[y] := lTempColour
    else
      MatrixMain.Render.GradientIX[y] := lTempColour;

    dec(lIndex);
  end;

  FreeAndNil(lColours);
end;


procedure TfrmMain.miGradSetRowClick(Sender: TObject);
var
  x : integer;

begin
  if sbGradient.Tag = 1 then begin
    for x := 0 to MatrixMain.Matrix.Width do begin
      MatrixMain.PlotPixelMatrix(x, TMenuItem(Sender).Tag, MatrixMain.Render.GradientIY[TMenuItem(Sender).Tag]);
    end;
  end
  else begin
    for x := 0 to MatrixMain.Matrix.Height do begin
      MatrixMain.PlotPixelMatrix(TMenuItem(Sender).Tag, x, MatrixMain.Render.GradientIX[TMenuItem(Sender).Tag]);
    end;
  end;

  MatrixMain.Refresh;
end;


procedure TfrmMain.Pasteintoeveryframe1Click(Sender: TObject);
begin
  MatrixMain.DrawWithBrushPasteEveryFrame(FOldMouseX, FOldMouseY, False);
end;


procedure TfrmMain.Pasteintoeveryframetransparent1Click(Sender: TObject);
begin
  MatrixMain.DrawWithBrushPasteEveryFrame(FOldMouseX, FOldMouseY, True);
end;


procedure TfrmMain.pCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in shift) then
    puMainCanvas.Popup(frmMain.Left + X - 10, frmMain.Top + pCanvas.Top + 48 + Y);
end;


procedure TfrmMain.pCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FormMouseMove(nil, [], 0, 0);
end;


procedure TfrmMain.miPopoutPreviewClick(Sender: TObject);
begin
  if not(MatrixMain.PreviewPopout) then begin
    frmPreviewPopout.Panel1.Color := pCanvas.Color;
    frmPreviewPopout.Color        := pCanvas.Color;

    frmPreviewPopout.Show;

    frmPreviewPopout.OnClose      := OnPopoutClosed;
    frmPreviewPopout.OnCommand    := PreviewWindowCommand;
    frmPreviewPopout.OnNewFrame   := PreviewWindowChangeFrame;

    MatrixMain.PreviewOwner       := frmPreviewPopout;
    MatrixMain.PreviewComponent   := frmPreviewPopout.Panel1;

    MatrixMain.PreviewPopout      := True;
  end
  else
    frmPreviewPopout.Close;
end;


procedure TfrmMain.OnPopoutClosed(Sender: TObject; var Action: TCloseAction);
begin
  MatrixMain.PreviewPopout := False;

  FormResize(Nil);
end;


procedure TfrmMain.Preferences1Click(Sender: TObject);
begin
  if DoPrefs = mrOK then begin
     MatrixMain.CurrentFrame := MatrixMain.CurrentFrame;
  end;
end;


procedure TfrmMain.puGradientShapePopup(Sender: TObject);
begin
  miGradSetRow.Tag := TShape(Sender).Tag;

  if sbGradient.Tag = 1 then begin
    miGradSetRow.Caption := GLanguageHandler.Text[kSetRowToSelectedColour];
    miGradFrom.Caption   := GLanguageHandler.Text[kGradientFromTopBottom];
  end
  else begin
    miGradSetRow.Caption := GLanguageHandler.Text[kSetColumnToSelectedColour];
    miGradFrom.Caption   := GLanguageHandler.Text[kGradientFromLeftRight];
  end;
end;


procedure TfrmMain.MatrixOnChange(Sender : TObject);
begin
  tbFrames.Max                  := MatrixMain.FrameCount; // last frame available
  frmPreviewPopout.tbFrames.Max := tbFrames.Max;

  miUndo.Enabled := MatrixMain.CanUndo;
  miRedo.Enabled := MatrixMain.CanRedo;

//  if (pQuickData.Visible) then
 //   UpdateData;
end;


procedure TfrmMain.MatrixOnLayerChange(Sender : TObject);
begin
  FFrameLayerPanel.UpdateLayerTable;
end;


procedure TfrmMain.MatrixOnSizeChange(Sender : TObject);
begin
  updateMemoryUsage;
end;


procedure TfrmMain.MatrixOnDisplayBufferCopied(Sender : TObject);
begin
  if (pQuickData.Visible) then
    UpdateData;
end;


procedure TfrmMain.MatrixOnNewFrameDisplayed(Sender : TObject);
begin
  tbFrames.Max                  := MatrixMain.FrameCount;

  bDeleteFrame.Enabled          := (MatrixMain.FrameCount > 1);
  bDeleteMultipleFrames.Enabled := (MatrixMain.FrameCount > 1);

  if (MatrixMain.CurrentFrame <> -1) then
    tbFrames.Position := MatrixMain.CurrentFrame;

  frmPreviewPopout.tbFrames.Max      := tbFrames.Max;
  frmPreviewPopout.tbFrames.Position := tbFrames.Position;

  SetFrameCaption(tbFrames.Position);

  // move to onnewframedisplayed
  if MatrixMain.IsLocked then
    bLockFrame.Tag := 1
  else
    bLockFrame.Tag := 0;

  SetButtonImage(bLockFrame, 6 + bLockFrame.Tag);

  miUndo.Enabled := MatrixMain.CanUndo;
  miRedo.Enabled := MatrixMain.CanRedo;

  if pUndoToolbar.Visible then
    FFrameUndoPanel.SetUndos(MatrixMain.GetUndoCount);
end;


procedure TfrmMain.MatrixOnColourChange(Sender : TObject);
begin
  case MatrixMain.Matrix.Mode of
    mtRGB     : begin
                  sSelectionLMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseLeft];
                  sSelectionMMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseMiddle];
                  sSelectionRMB.Brush.Color := MatrixMain.LEDRGBColours[CMouseRight];
                end;
    mtRGB3BPP : begin
                  sSelectionLMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[MatrixMain.LEDRGBColours[CMouseLeft]];
                  sSelectionMMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[MatrixMain.LEDRGBColours[CMouseMiddle]];
                  sSelectionRMB.Brush.Color := MatrixMain.LEDRGB3BPPColours[MatrixMain.LEDRGBColours[CMouseRight]];
                end;
  end;
end;


procedure TfrmMain.MatrixOnMouseOver(const x, y : integer);
begin
  FOldMouseX := X;
  FOldMouseY := Y;

  if (x >= 0) and (y >= 0) and (x < MatrixMain.Matrix.Width) and (y < MatrixMain.Matrix.Height) then begin

    case MatrixMain.Matrix.Mode of
      mtRGB     : statusMain.SimpleText := 'X: ' + IntToStr(x + 1) +
                                         '  Y: ' + IntToStr(y + 1) +
                                         '  ' + GLanguageHandler.Text[kData] + ': ' + LMSSettings.App.HexPrefix + IntToHex(TUtility.RGBConvertTo(MatrixMain.MatrixLayers[MatrixMain.CurrentLayer].Frames[tbFrames.Position].Grid[x, y], cmRGB, llBottomRight, 100), 6);
      mtRGB3BPP : statusMain.SimpleText := 'X: ' + IntToStr(x + 1) +
                                         '  Y: ' + IntToStr(y + 1) +
                                         '  ' + GLanguageHandler.Text[kData] + ': ' + LMSSettings.App.HexPrefix + IntToHex(MatrixMain.MatrixLayers[MatrixMain.CurrentLayer].Frames[tbFrames.Position].Grid[x, y], 2);
    end;

    if (lPixelColour.Visible) then begin
      if MatrixMain.Matrix.Mode = mtRGB then
        lPixelColour.Caption := LMSSettings.App.HexPrefix + IntToHex(TUtility.RGBConvertTo(MatrixMain.MatrixLayers[MatrixMain.CurrentLayer].Frames[tbFrames.Position].Grid[x, y], cmRGB, llBottomRight, 100), 6) + ' (' + TUtility.RGBConvertToSplit(MatrixMain.MatrixLayers[MatrixMain.CurrentLayer].Frames[tbFrames.Position].Grid[x, y], cmRGBSimple, 100, nfDecimal, '', ' ') + ')'
      else
        lPixelColour.Caption := LMSSettings.App.HexPrefix + IntToHex(MatrixMain.MatrixLayers[MatrixMain.CurrentLayer].Frames[tbFrames.Position].Grid[x, y], 2);
    end;
  end;
end;


procedure TfrmMain.MatrixOnPreviewMouseDown(const x, y : integer);
begin
  puPreview.Popup(Left + X + 10, Top + Y + 10);
end;


procedure TfrmMain.MatrixOnDebug(const s : string);
begin
  Caption := s;
end;


function TfrmMain.AppendFromFileName(aFileName : string): boolean;
var
  ted : TImportData;

begin
  // flushes the message queue, stops the mouse double click from the open dialog
  // causing a pixel to be drawm on the first frame
  Application.ProcessMessages;

  TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);

  ted := MatrixMain.LoadLEDMatrixData(aFileName, LMSSettings.App.LastExport, lmAppend, -1);

  if (ted.ImportOk) then begin
    tbFrames.Max                  := ted.MaxFrames;

    frmPreviewPopout.tbFrames.Max := tbFrames.Max;

    UpdateMemoryUsage;
  end
  else begin
    if ted.ErrorString <> '' then
      MessageDlg(ted.ErrorString, mtError, [mbOK], 0);
  end;

  Result := ted.ImportOk;
end;


procedure TfrmMain.miFlattenLayersClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kFlattenAllLayersQ] + #13#13 + GLanguageHandler.Text[kThisCannotBeUndone], mtWarning, mbYesNo, 0) = mrYes then begin
    MatrixMain.FlattenAllLayers;
  end;
end;


function TfrmMain.MergeFromFileName(aFileName : string; aStartFrame : integer; aMergeMode : TLoadMode): boolean;
var
  ted : TImportData;

begin
  // flushes the message queue, stops the mouse double click from the open dialog
  // causing a pixel to be drawm on the first frame
  Application.ProcessMessages;

  TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);

  ted := MatrixMain.LoadLEDMatrixData(aFileName, LMSSettings.App.LastExport, aMergeMode, aStartFrame);

  if (ted.ImportOk) then begin
    tbFrames.Max                  := ted.MaxFrames;
    frmPreviewPopout.tbFrames.Max := tbFrames.Max;

    UpdateMemoryUsage;
  end
  else begin
    if (ted.ErrorString <> '') then
      MessageDlg(ted.ErrorString, mtError, [mbOK], 0);
  end;

  Result := ted.ImportOk;
end;


function TfrmMain.LoadFromGIF(aFilename : string): boolean;
var
  ted : TImportData;

begin
  Result := True;

  // flushes the message queue, stops the mouse double click from the open dialog
  // causing a pixel to be drawm on the first frame
  Application.ProcessMessages;

  // =======================================================================

  TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);

  ted := MatrixMain.ImportFromGIF(aFileName);

  // =======================================================================

  if (ted.ImportOk) then begin
    LMSSettings.Project.MatrixMode    := ted.MatrixMode;
    cbMatrixTypeChange(Nil);

    LMSSettings.Project.width         := ted.NewWidth;
    LMSSettings.Project.height        := ted.NewHeight;

    tbFrames.Max                  := ted.MaxFrames;
    frmPreviewPopout.tbFrames.Max := tbFrames.Max;

    sbBuildClick(Load1);

    bDeleteFrame.Enabled          := (tbFrames.Max > 1);
    bDeleteMultipleFrames.Enabled := (tbFrames.Max > 1);

    MatrixMain.CurrentFrame  := 1;

    SetFrameCaption(1);

    ClearCurrentProjectFileName;

    sColour0.Brush.Color := ted.BackgroundColour;

    // =========================================================================

    if miFontMode.Checked = True then begin
      miFontMode.Checked := False;
      miFontModeClick(Nil);
    end;

    // == preview ==============================================================

    miPreview.Checked         := ted.Preview.Enabled;
    MatrixMain.PreviewActive  := ted.Preview.Enabled;

    SetPreview(ted.Preview.Size, ted.Preview.View, ted.Preview.Void, ted.Preview.Offset, ted.Preview.OffsetDirection, ted.Preview.Popout);

    // =========================================================================

    LMSSettings.App.LastLoadLocation       := ExtractFilePath(odMain.Filename);
  end
  else begin
    if (ted.ErrorString <> '') then
      MessageDlg(ted.ErrorString, mtError, [mbOK], 0);
  end;

  UpdateMemoryUsage;
end;


function TfrmMain.LoadFromFileName(aFilename : string): boolean;
var
  ted : TImportData;
  i : integer;

begin
  Result := True;

  // flushes the message queue, stops the mouse double click from the open dialog
  // causing a pixel to be drawm on the first frame
  Application.ProcessMessages;

  // =======================================================================

  TUtility.ClearTExportOptions(False, LMSSettings.App.LastExport);

  ted := MatrixMain.LoadLEDMatrixData(aFileName, LMSSettings.App.LastExport, lmNew, -1);

  // =======================================================================

  if (ted.ImportOk) then begin
    LMSSettings.Project.MatrixMode    := ted.MatrixMode;
    cbMatrixTypeChange(Nil);

    LMSSettings.Project.width         := ted.NewWidth;
    LMSSettings.Project.height        := ted.NewHeight;

    tbFrames.Max                  := ted.MaxFrames;
    frmPreviewPopout.tbFrames.Max := tbFrames.Max;

    sbBuildClick(Load1);

    bDeleteFrame.Enabled          := (tbFrames.Max > 1);
    bDeleteMultipleFrames.Enabled := (tbFrames.Max > 1);

    if (ted.Colours.HasData) then begin
      sSelectionLMB.Brush.Color := ted.Colours.DrawColours[CMouseLeft];
      sSelectionMMB.Brush.Color := ted.Colours.DrawColours[CMouseMiddle];
      sSelectionRMB.Brush.Color := ted.Colours.DrawColours[CMouseRight];

      MatrixMain.LEDRGBColours[CMouseLeft]   := ted.Colours.DrawColours[CMouseLeft];
      MatrixMain.LEDRGBColours[CMouseMiddle] := ted.Colours.DrawColours[CMouseMiddle];
      MatrixMain.LEDRGBColours[CMouseRight]  := ted.Colours.DrawColours[CMouseRight];

      MatrixMain.SetMouseButtonColours(sSelectionLMB.Brush.Color,
                                       sSelectionMMB.Brush.Color,
                                       sSelectionRMB.Brush.Color);

      for i := 0 to 15 do
        RGBPalette[i].Brush.Color := ted.Colours.CustomColours[i];

      for i := 0 to 27 do
        FFramePalettePanel.RGBPaletteHistory[i].Brush.Color := ted.Colours.PaletteHistory[i];
    end;

    MatrixMain.CurrentFrame  := 1;

    SetFrameCaption(1);

    SetCurrentProjectFileName(aFileName);

    // ===========================================================================

    case ted.MatrixMode of
      mtRGB,
      mtRGB3BPP : sColour0.Brush.Color := ted.BackgroundColour;
    end;

    // ===========================================================================

    if ted.FontMode then begin
      if miFontMode.Checked = False then begin
        miFontMode.Checked := True;
        miFontModeClick(Nil);
      end;

      bPlayAnimationClick(bPlayAnimation);
    end
    else begin
      if miFontMode.Checked = True then begin
        miFontMode.Checked := False;
        miFontModeClick(Nil);
      end;
    end;

    // == preview ================================================================

    miPreview.Checked         := ted.Preview.Enabled;
    MatrixMain.PreviewActive  := ted.Preview.Enabled;

    SetPreview(ted.Preview.Size, ted.Preview.View, ted.Preview.Void, ted.Preview.Offset, ted.Preview.OffsetDirection, ted.Preview.Popout);

    // ===========================================================================

    LMSSettings.App.LastAutomationFileName := ted.AutomationFileName;

    UpdateMemoryUsage;
  end
    else begin
    if (ted.ErrorString <> '') then
      MessageDlg(ted.ErrorString, mtError, [mbOK], 0);
  end;
end;


// settings already loaded, put them where they are needed
procedure TfrmMain.SetFromSettings;
var
  t : integer;

begin
  for t := 0 to 5 do
    MatrixMain.LEDColoursSingle[t] := LMSSettings.LEDColoursSingle[t];

  for t := 0 to 5 do
    MatrixMain.LEDColoursBi[t] := LMSSettings.LEDColoursBi[t];

  MatrixMain.RGBBackground               := LMSSettings.RGBBackground;
  MatrixMain.LEDRGBColours[CMouseLeft]   := LMSSettings.LEDRGBColours[1];
  MatrixMain.LEDRGBColours[CMouseMiddle] := LMSSettings.LEDRGBColours[2];
  MatrixMain.LEDRGBColours[CMouseRight]  := LMSSettings.LEDRGBColours[3];

  // ===========================================================================

  sSelectionLMB.Tag := LMSSettings.SelectionColours[1];
  sSelectionMMB.Tag := LMSSettings.SelectionColours[2];
  sSelectionRMB.Tag := LMSSettings.SelectionColours[3];

  // ===========================================================================

  SetupMatrixColours;

  // ===========================================================================

  SystemSetBackgroundColour(LMSSettings.App.BackgroundColour);

  // ===========================================================================

  odMain.InitialDir := LMSSettings.App.LastLoadLocation;
  sdMain.InitialDir := LMSSettings.App.LastSaveLocation;

  miPlaybackSpeedCustom.Caption := GLanguageHandler.Text[kCustom] + ' (' + IntToStr(LMSSettings.App.CustomSpeed) + ' ms)';

  if (LMSSettings.App.CustomSpeed <= 0) then
    LMSSettings.App.CustomSpeed := 1000;

  // ===========================================================================

  case LMSSettings.PixelSize of
    CPixelSizeAuto : miPixelTinyClick(miPixelAuto);
    CPixelSize10   : miPixelTinyClick(miPixelTiny);
    CPixelSize15   : miPixelTinyClick(miPixelSmall);
    CPixelSize20   : miPixelTinyClick(miPixelMedium);
    CPixelSize25   : miPixelTinyClick(miPixelLarge);
    CPixelSize30   : miPixelTinyClick(miPixelVeryLarge);
    CPixelSize40   : miPixelTinyClick(miPixelUltra);
    CPixelSize50   : miPixelTinyClick(miPixelMegaUltra);
  else
    miPixelTinyClick(miPixelAuto);
  end;

  // ===========================================================================

  case LMSSettings.PixelShape of
    psSquare    : miPixelShapeSquareClick(miPixelShapeSquare);
    psCircle    : miPixelShapeSquareClick(miPixelShapeRound);
    psRoundRect : miPixelShapeSquareClick(miPixelShapeRoundRect);
  else
    miPixelShapeSquareClick(miPixelShapeSquare);
  end;

  // ===========================================================================

  miShowAnimationToolbar.Checked := LMSSettings.Toolbars.Animation;

  case LMSSettings.AnimSpeed of
      10 : miPlaybackSpeed3Click(miPlaybackSpeed11);
      20 : miPlaybackSpeed3Click(miPlaybackSpeed10);
      25 : miPlaybackSpeed3Click(miPlaybackSpeed9);
      50 : miPlaybackSpeed3Click(miPlaybackSpeed8);
     100 : miPlaybackSpeed3Click(miPlaybackSpeed7);
     200 : miPlaybackSpeed3Click(miPlaybackSpeed6);
     250 : miPlaybackSpeed3Click(miPlaybackSpeed5);
     500 : miPlaybackSpeed3Click(miPlaybackSpeed4);
    1000 : miPlaybackSpeed3Click(miPlaybackSpeed3);
    1500 : miPlaybackSpeed3Click(miPlaybackSpeed2);
    2000 : miPlaybackSpeed3Click(miPlaybackSpeed1);
  else
    if LMSSettings.AnimSpeed > 0 then begin
      SetPlaybackCustom(LMSSettings.AnimSpeed);
    end
    else
      miPlaybackSpeed3Click(miPlaybackSpeed5);
  end;

  miShowAnimationToolbarClick(Nil);

  // ===========================================================================

  miPaletteGradientToolbar.Checked := LMSSettings.Toolbars.RGBPalette;
  pRGBPalette.Visible              := miPaletteGradientToolbar.Checked;

  // ===========================================================================

  if (LMSSettings.AutoSaveEnabled) then begin
    miAutosave.Checked    := True;
    timerAutosave.Enabled := True;
  end;

  case LMSSettings.AutoSaveInterval of
    asTwoMinutes  : miAutosave2Click(miAutosave2);
    asFiveMinutes : miAutosave2Click(miAutosave5);
    asTenMinutes  : miAutosave2Click(miAutosave10);
  else
    miAutosave2Click(miAutosave2);
  end;

  // ===========================================================================

  miPreview.Checked         := LMSSettings.PreviewOptions.Enabled;

  MatrixMain.PreviewActive  := miPreview.Checked;

  SetPreview(LMSSettings.PreviewOptions.Size,
             LMSSettings.PreviewOptions.View,
             LMSSettings.PreviewOptions.Void,
             LMSSettings.PreviewOptions.OffSet,
             LMSSettings.PreviewOptions.Direction,
             LMSSettings.PreviewOptions.Popout);

  // ===========================================================================

  for t := 0 to 15 do begin
    RGBPalette[t].Brush.Color := LMSSettings.RGBPalette[t];
  end;

  // ===========================================================================
end;


procedure TfrmMain.miLockAllClick(Sender: TObject);
begin
  MatrixMain.LockUnLockRange(1, MatrixMain.FrameCount, True);
end;


procedure TfrmMain.SetPreview(aSize : integer; aView : TViewShape; aVoid : integer; aOffSet : integer; aDirection, aPopout : boolean);
begin
  case aSize of
    1  : miPreviewx1Click(miPreviewx1);
    2  : miPreviewx1Click(miPreviewx2);
    3  : miPreviewx1Click(miPreviewx3);
    4  : miPreviewx1Click(miPreviewx4);
    5  : miPreviewx1Click(miPreviewx5);
    6  : miPreviewx1Click(miPreviewx6);
    8  : miPreviewx1Click(miPreviewx8);
    10 : miPreviewx1Click(miPreviewx10);
    12 : miPreviewx1Click(miPreviewx12);
    15 : miPreviewx1Click(miPreviewx15);
    20 : miPreviewx1Click(miPreviewx20);
    25 : miPreviewx1Click(miPreviewx25);
    30 : miPreviewx1Click(miPreviewx30);
    40 : miPreviewx1Click(miPreviewx40);
    50 : miPreviewx1Click(miPreviewx50);
  else
    miPreviewx1Click(miPreviewx1);
  end;

  case aView of
    vsSquare             : miPreviewViewSquareClick(miPreviewViewSquare);
    vsRadial             : miPreviewViewSquareClick(miPreviewViewRadial);
    vsRadial3Q           : miPreviewViewSquareClick(miPreviewViewRadialTQ);
    vsSemiCircle         : miPreviewViewSquareClick(miPreviewViewSemiCircle);
    vsSemiCircleInverted : miPreviewViewSquareClick(miPreviewViewSemiCircleInverted);
  else
    miPreviewViewSquareClick(miPreviewViewSquare);
  end;

  case aVoid of
    10 : miPreviewVoid10Click(miPreviewVoid10);
    15 : miPreviewVoid10Click(miPreviewVoid15);
    20 : miPreviewVoid10Click(miPreviewVoid20);
    25 : miPreviewVoid10Click(miPreviewVoid25);
    30 : miPreviewVoid10Click(miPreviewVoid30);
    40 : miPreviewVoid10Click(miPreviewVoid40);
    50 : miPreviewVoid10Click(miPreviewVoid50);
  else
    miPreviewVoid10Click(miPreviewVoid10);
  end;

  case aOffSet of
    0      : miRadialOffset45Click(miRadialOffset0);
    1,  45 : miRadialOffset45Click(miRadialOffset45);
    2,  90 : miRadialOffset45Click(miRadialOffset90);
    3, 135 : miRadialOffset45Click(miRadialOffset135);
    4, 180 : miRadialOffset45Click(miRadialOffset180);
    5, 225 : miRadialOffset45Click(miRadialOffset225);
    6, 270 : miRadialOffset45Click(miRadialOffset270);
    7, 315 : miRadialOffset45Click(miRadialOffset315);
  else
    miRadialOffset45Click(miRadialOffset0);
  end;

  if aDirection then begin
    miPreviewOffsetReverse.Checked := True;
    miPreviewOffsetReverseClick(miPreviewOffsetReverse);
  end;
end;


procedure TfrmMain.ManageUIControls(aOverride : boolean; aSetTo : boolean);
var
  lNormalFalse : boolean;
  lNormalTrue  : boolean;

begin
  lNormalFalse := False;
  lNormalTrue  := True;

  if (aOverride) then begin
    lNormalFalse := aSetTo;
    lNormalTrue  := aSetTo;
  end;

  if MatrixMain.AnimPlaying then begin
    lNormalTrue := False;
  end;

  if MatrixMain.Matrix.Width <> MatrixMain.Matrix.Height then begin
    sbRotateL.Enabled := lNormalFalse;
    sbRotateR.Enabled := lNormalFalse;
    miRotateL.Enabled := lNormalFalse;
    miRotateR.Enabled := lNormalFalse;
  end
  else begin
    sbRotateL.Enabled := lNormalTrue;
    sbRotateR.Enabled := lNormalTrue;
    miRotateL.Enabled := lNormalTrue;
    miRotateR.Enabled := lNormalTrue;
  end;

  sbRotateAny.Enabled         := lNormalTrue;
  cbRotateAngle.Enabled       := lNormalTrue;
  cbRotateCount.Enabled       := lNormalTrue;

  bLockFrame.Enabled          := lNormalTrue;

  // ===========================================================================

  sbBuild.Enabled             := lNormalTrue;

  if MatrixMain.Matrix.Available then begin
    sbSave.Enabled         := lNormalTrue;
    sbExport.Enabled       := lNormalTrue;
    sbGenerateCode.Enabled := lNormalTrue;
  end
  else begin
    sbSave.Enabled         := lNormalFalse;
    sbExport.Enabled       := lNormalFalse;
    sbGenerateCode.Enabled := lNormalFalse;
  end;

  sbOpen.Enabled              := lNormalTrue;
  sbExport.Enabled            := lNormalTrue;
  sbPixelSize.Enabled         := lNormalTrue;
  sbPixelShape.Enabled        := lNormalTrue;
  sbPreset.Enabled            := lNormalTrue;

  miUndo.Enabled              := lNormalTrue;
  miCopy.Enabled              := lNormalTrue;
  miCopyFromPrevious.Enabled  := lNormalTrue;
  miCopyMultiple.Enabled      := lNormalTrue;
  miPaste.Enabled             := lNormalTrue;
  miPasteSpecial.Enabled      := lNormalTrue;
  miBrushActions.Enabled      := lNormalTrue;

  miPopoutPreview.Enabled     := lNormalTrue;

  sbClear.Enabled             := lNormalTrue;
  sbFlip.Enabled              := lNormalTrue;
  sbMirror.Enabled            := lNormalTrue;
  sbInvert.Enabled            := lNormalTrue;
  miFlip.Enabled              := lNormalTrue;
  miMirror.Enabled            := lNormalTrue;
  miInvert.Enabled            := lNormalTrue;
  miFlipAllFrames.Enabled     := lNormalTrue;
  miMirrorAllFrames.Enabled   := lNormalTrue;
  miInvertAllFrames.Enabled   := lNormalTrue;
  sbScrollLeft.Enabled        := lNormalTrue;
  sbScrollRight.Enabled       := lNormalTrue;
  sbScrollUp.Enabled          := lNormalTrue;
  sbScrollDown.Enabled        := lNormalTrue;
  miShiftLeft.Enabled         := lNormalTrue;
  miShiftRight.Enabled        := lNormalTrue;
  miShiftUp.Enabled           := lNormalTrue;
  miShiftDown.Enabled         := lNormalTrue;
  miAddComment.Enabled        := lNormalTrue;

  // bit of hack for when dead pixel mode active :)
  if (MatrixMain.DeadPixelsMode) then begin
    miDeadPixels.Enabled            := True;
    miSetDeadPixels.Enabled         := True;
    miSetIgnoredFromPattern.Enabled := True;
    miClearAllDeadPixels.Enabled    := True;
  end
  else begin
    miDeadPixels.Enabled            := lNormalTrue;
    miSetDeadPixels.Enabled         := lNormalTrue;
    miSetIgnoredFromPattern.Enabled := lNormalTrue;
    miClearAllDeadPixels.Enabled    := lNormalTrue;
  end;

  if MatrixMain.AnimPlaying then begin
    bPlayAnimation.Enabled                  := False;
    bStopAnimation.Enabled                  := True;

    frmPreviewPopout.bPlayAnimation.Enabled := False;
    frmPreviewPopout.bStopAnimation.Enabled := True;
  end
  else begin
    bPlayAnimation.Enabled                 := True;
    bStopAnimation.Enabled                 := False;

    frmPreviewPopout.bPlayAnimation.Enabled := True;
    frmPreviewPopout.bStopAnimation.Enabled := False;
  end;

  bPreviousFrame.Enabled                  := lNormalTrue;
  bStartFrame.Enabled                     := lNormalTrue;
  bEndFrame.Enabled                       := lNormalTrue;
  bNextFrame.Enabled                      := lNormalTrue;

  frmPreviewPopout.bStartFrame.Enabled    := lNormalTrue;
  frmPreviewPopout.bEndFrame.Enabled      := lNormalTrue;
  frmPreviewPopout.bNextFrame.Enabled     := lNormalTrue;
  frmPreviewPopout.bPreviousFrame.Enabled := lNormalTrue;

  bAddFrame.Enabled           := lNormalTrue;
  bAddFrameCopy.Enabled       := lNormalTrue;
  bAddFrameMultiple.Enabled   := lNormalTrue;

  miAddFrame.Enabled           := lNormalTrue;
  miAddFrameCopy.Enabled       := lNormalTrue;
  miAddFrameMultiple.Enabled   := lNormalTrue;

  if MatrixMain.AnimPlaying then begin
    bDeleteFrame.Enabled           := False;
    bDeleteMultipleFrames.Enabled  := False;

    miDeleteFrame.Enabled          := False;
    miDeleteMultipleFrames.Enabled := False;
  end
  else begin
    bDeleteFrame.Enabled           := (MatrixMain.FrameCount > 2);
    bDeleteMultipleFrames.Enabled  := (MatrixMain.FrameCount > 2);

    miDeleteFrame.Enabled          := (MatrixMain.FrameCount > 2);
    miDeleteMultipleFrames.Enabled := (MatrixMain.FrameCount > 2);
  end;

  bLightBox.Enabled                 := lNormalTrue;

  sbMouseMode.Enabled               := lNormalTrue;
  sbCopy.Enabled                    := lNormalTrue;
  sbNewBrush.Enabled                := lNormalTrue;
  sbFilledRectangle.Enabled         := lNormalTrue;
  sbFrame.Enabled                   := lNormalTrue;
  sbEmptyCircle.Enabled             := lNormalTrue;
  sbFilledCircle.Enabled            := lNormalTrue;
  sbLine.Enabled                    := lNormalTrue;
  sbMultiDraw.Enabled               := lNormalTrue;
  sbFloodFill.Enabled               := lNormalTrue;
  sbFont.Enabled                    := lNormalTrue;

  cbMirrorMode.Enabled              := lNormalTrue;

  sbPatternSpiral.Enabled           := lNormalTrue;
  sbPatternCircle.Enabled           := lNormalTrue;
  sbPatternSplitRing.Enabled        := lNormalTrue;
  sbPatternPetals.Enabled           := lNormalTrue;
  sbPatternGrid.Enabled             := lNormalTrue;
  sbPatternPyramid.Enabled          := lNormalTrue;
  sbPatternLeftTriangle.Enabled     := lNormalTrue;
  sbPatternRightTriangle.Enabled    := lNormalTrue;

  miMouseMode.Enabled               := lNormalTrue;
  miNewBrush.Enabled                := lNormalTrue;
  miDrawCopy.Enabled                := lNormalTrue;
  miFilledRectangle.Enabled         := lNormalTrue;
  miFrame.Enabled                   := lNormalTrue;
  miEmptyCircle.Enabled             := lNormalTrue;
  miFilledCircle.Enabled            := lNormalTrue;
  miLine.Enabled                    := lNormalTrue;
  miMultiDraw.Enabled               := lNormalTrue;
  miFloodFill.Enabled               := lNormalTrue;
  miFont.Enabled                    := lNormalTrue;

  miPatternSpiral.Enabled           := lNormalTrue;
  miPatternCircle.Enabled           := lNormalTrue;
  miPatternSplitRing.Enabled        := lNormalTrue;
  miPatternPetals.Enabled           := lNormalTrue;
  miPatternGrid.Enabled             := lNormalTrue;
  miPatternPyramid.Enabled          := lNormalTrue;
  miPatternLeftTriangle.Enabled     := lNormalTrue;
  miPatternRightTriangle.Enabled    := lNormalTrue;


  miAppend.Enabled                  := lNormalTrue;
  miMerge.Enabled                   := lNormalTrue;

  miSave.Enabled                    := lNormalTrue;
  miSaveAs.Enabled                  := lNormalTrue;
  miSaveSingleFrame.Enabled         := lNormalTrue;
  miSaveRange.Enabled               := lNormalTrue;
  miImportInToCurrent.Enabled       := lNormalTrue;
  miExport.Enabled                  := lNormalTrue;
  miExportToBitmap.Enabled          := lNormalTrue;
  miExportAnimationToBitmap.Enabled := lNormalTrue;
  miExportToGIF.Enabled             := lNormalTrue;
  miCodeTemplates.Enabled           := lNormalTrue;

  miLockAll.Enabled                 := lNormalTrue;
  miUnlockAll.Enabled               := lNormalTrue;
  miToggleLockStatus.Enabled        := lNormalTrue;

  miClearLayer.Enabled              := lNormalTrue;
  miFlattenLayers.Enabled           := lNormalTrue;

  sbSave.Enabled                    := lNormalTrue;

  tbFrames.Enabled                  := lNormalTrue;
  frmPreviewPopout.tbFrames.Enabled := lNormalTrue;

  miClearAllFrames.Enabled          := lNormalTrue;
  miClearAllFramesLayer.Enabled     := lNormalTrue;

  miAutomate.Enabled                := lNormalTrue;

  miChangeColoursFrame.Enabled      := lNormalTrue;
  miChangeColoursLayer.Enabled      := lNormalTrue;
  miChangeColoursAll.Enabled        := lNormalTrue;

  miCopyCurrentTo.Enabled           := lNormalTrue;
  miRestoreCurrentFrom.Enabled      := lNormalTrue;
  miExportUserMemories.Enabled      := lNormalTrue;
  miClearAllUserMemories.Enabled    := lNormalTrue;

  miCountColours.Enabled            := lNormalTrue;

  if MatrixMain.Matrix.Mode = mtMono then begin
    sbGradient.Enabled               := lNormalFalse;
    miClearAllFramesGradient.Enabled := lNormalFalse;
    sbRandomDraw.Enabled             := lNormalFalse;
    miGradientAllFrames.Enabled      := lNormalFalse;
    sbPicker.Enabled                 := lNormalFalse;
    sbGradientBrush.Enabled          := lNormalFalse;

    miGradient.Enabled               := lNormalFalse;
    miRandomDraw.Enabled             := lNormalFalse;
    miPicker.Enabled                 := lNormalFalse;
    miGradientBrush.Enabled          := lNormalFalse;
  end
  else begin
    sbGradient.Enabled               := lNormalTrue;
    miClearAllFramesGradient.Enabled := lNormalTrue;
    sbRandomDraw.Enabled             := lNormalTrue;
    miGradientAllFrames.Enabled      := lNormalTrue;
    sbGradientBrush.Enabled          := lNormalTrue;

    miGradient.Enabled               := lNormalTrue;
    miRandomDraw.Enabled             := lNormalTrue;
    miPicker.Enabled                 := lNormalTrue;
    miGradientBrush.Enabled          := lNormalTrue;

    sbPicker.Enabled                 := MatrixMain.Matrix.Mode = mtRGB;
  end;
end;


procedure TfrmMain.Currentframe1Click(Sender: TObject);
begin
  MessageDlg(GLanguageHandler.Text[kUniqueColoursCurrentFrame] + ': ' + IntToStr(MatrixMain.CountColoursFrame), mtInformation, [mbOK], 0);
end;


procedure TfrmMain.Animation1Click(Sender: TObject);
begin
  MessageDlg(GLanguageHandler.Text[kUniqueColoursAnimation] + ': ' + IntToStr(MatrixMain.CountColoursFrame), mtInformation, [mbOK], 0);
end;


procedure TfrmMain.GenerateShades(aColour : integer);
var
  xR : LongWord;
  xG : LongWord;
  xB : LongWord;
  t : LongWord;
  xMaxRPos : LongWord;
  dR : LongWord;
  dG : LongWord;
  dB : LongWord;

begin
  xR := (aColour and $0000ff);         // Windows colour structure = BGR
  xB := (aColour and $ff0000) shr 16;
  xG := (aColour and $00ff00) shr 8;

  xMaxRPos := Round(Max(xR, Max(xG, xB)) / 255);

  dR       := Round((xR * xMaxRPos) / 16);
  dG       := Round((xG * xMaxRPos) / 16);
  dB       := Round((xB * xMaxRPos) / 16);

  for t := 0 to 15 do begin
    xR := (t * dR);
    xG := (t * dG);
    xB := (t * dB);

    if (t * dR > 255) then xR := 255;
    if (t * dG > 255) then xG := 255;
    if (t * dB > 255) then xB := 255;

    RGBShade[t].Brush.Color := xR + (xB shl 16) + (xG shl 8); // windows format is BGR
  end;
end;


procedure TfrmMain.CopyToGradientBrush(Sender : TObject);
var
  lCount, t : integer;

begin
  lCount := FFrameGradientPanel.Count;

  if lCount <> 0 then begin
    MatrixMain.ClearGradient;

    for t := 0 to lCount - 1 do
      MatrixMain.AddGradient(FFrameGradientPanel.GetColour(t));
  end;
end;


procedure TfrmMain.CopyFromCustom(Sender : TObject);
var
  i : integer;

begin
  for i := 0 to 15 do
    FFrameGradientPanel.AddColour(RGBPalette[i].Brush.Color);
end;


procedure TfrmMain.CopyFromShades(Sender : TObject);
var
  i : integer;

begin
  for i := 0 to 15 do
    FFrameGradientPanel.AddColour(RGBShade[i].Brush.Color);
end;


procedure TfrmMain.PaletteColourOver(aColour : integer);
begin
  lPixelColour.Caption := LMSSettings.App.HexPrefix + IntToHex(TUtility.RGBConvertTo(aColour, cmRGB, llBottomRight, 100), 6) + ' (' + TUtility.RGBConvertToSplit(aColour, cmRGBSimple, 100, nfDecimal, '', ' ') + ')';
end;


procedure TfrmMain.OnLayerPanelClose(Sender : TObject);
begin
  miToggleLayoutPanelClick(Nil);
end;


procedure TfrmMain.OnLayerMenuItem(aItem : integer);
begin
  case aItem of
    1 : if MessageDlg(GLanguageHandler.Text[kClearLayer] + ' "' + MatrixMain.GetLayerName(MatrixMain.CurrentLayer) + '"?' + #13#13 + GLanguageHandler.Text[kThisCannotBeUndone], mtWarning, [mbYes, mbNo], 0) = mrYes then
          MatrixMain.ClearCurrentLayer;
  end;
end;


procedure TfrmMain.OnUndoSelected(aUndo : integer);
begin
  MatrixMain.SetFromUndo(aUndo);
end;


procedure TfrmMain.QuickDataChange(Sender : TObject);
begin
  UpdateData;
end;


procedure TfrmMain.ConfigureOpenDialog(aMode : integer);
begin
  case aMode of
    CLoadProject      : begin
                          odMain.DefaultExt := '.leds';
                          odMain.Filter     := GLanguageHandler.Text[kLEDMatrixStudioProjects] + ' (*.leds)|*.leds';
                          odMain.InitialDir := LMSSettings.App.LastLoadLocation;
                        end;
    CLoadIgnorePixels : begin
                          odMain.DefaultExt := '.ledsip';
                          odMain.Filter     := GLanguageHandler.Text[kLEDMatrixStudioIgnorePixelFiles] + ' (*.ledsip)|*.ledsip';
                          odMain.InitialDir := LMSSettings.App.LastLoadLocation;
                        end;
  end;
end;


procedure TfrmMain.ConfigureSaveDialog(aMode : integer);
begin
  case aMode of
    CSaveProject      : begin
                          sdMain.InitialDir := LMSSettings.App.LastSaveLocation;
                          sdMain.Filter     := GLanguageHandler.Text[kLEDMatrixStudioProjects] + ' (*.leds)|*.leds';
                          sdMain.DefaultExt := '.leds';
                        end;
    CSaveFont         : begin
                          sdMain.DefaultExt := '.ledsfont';
                          sdMain.Filename   := 'font_' + IntToStr(MatrixMain.Matrix.Width) + 'x' + IntToStr(MatrixMain.Matrix.Height);
                          sdMain.Filter     := GLanguageHandler.Text[kLEDMatrixStudioFont] + ' (*.ledsfont)|*.ledsfont';
                          sdMain.InitialDir := ExtractFilepath(Application.Exename) + 'fonts\';
                        end;
    CSaveIgnorePixels : begin
                          sdMain.DefaultExt := '.ledsip';
                          sdMain.Filter     := GLanguageHandler.Text[kLEDMatrixStudioIgnorePixelFiles] + ' (*.ledsip)|*.ledsip';
                          sdMain.InitialDir := LMSSettings.App.LastLoadLocation;
                          sdMain.Filename   := 'ignore_' + IntToStr(MatrixMain.Matrix.Width) + 'x' + IntToStr(MatrixMain.Matrix.Height);
                        end;
  end;
end;


procedure TfrmMain.UpdateData;
var
 tdod     : TDataOutDisplay;

 function BinToInt(s : string): int64;
 var
   i, t : integer;

  begin
   result := 0;

   i := 0;

   for t := length(s) downto 1 do begin
     if s[t] = '1' then
       result := result + powers[i];

     inc(i);
   end;
 end;


begin
  if (not sbClear.Enabled) or (not pQuickData.Visible) then
    Exit;

  if (MatrixMain.Matrix.Mode <> mtRGB) and
     (MatrixMain.Matrix.Mode <> mtRGB3BPP) then begin

    case MatrixMain.Matrix.Mode of
      mtMono         : begin
                         tdod := TExportMonoBi.SimpleExportMono(tbFrames.Position, FFrameQuickData.LSB, FFrameQuickData.Source, FFrameQuickData.Direction, FFrameQuickData.Hex, FFrameQuickData.CombineNybbles, pQuickData.Visible);
                       end;
      mtBiSequential : begin // bicolour, sequential bits
                         tdod := TExportMonoBi.SimpleExportBiSequential(tbFrames.Position, FFrameQuickData.LSB, FFrameQuickData.Source, FFrameQuickData.Direction, FFrameQuickData.Hex, pQuickData.Visible);
                       end;
      mtBiBitPlanes  : begin // bicolour, bitplanes
                         tdod := TExportMonoBi.SimpleExportBiBitplanes(tbFrames.Position, FFrameQuickData.LSB, FFrameQuickData.Source, FFrameQuickData.Direction, FFrameQuickData.Hex, pQuickData.Visible);
                       end;
    end;

    FFrameQuickData.SetText(tdod.Text);
  end;
end;


end.
