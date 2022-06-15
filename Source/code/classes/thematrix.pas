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

unit thematrix;


interface


uses System.UITypes, ExtCtrls, classes, controls, types, sysutils, graphics, math,
     contnrs, System.Generics.Collections, Vcl.Imaging.GIFImg, Vcl.StdCtrls, Vcl.Forms,

     fileconstants, matrixconstants, importdata, drawingdata, languagehandler,

     exportoptions, layer,

     matrix, matrixdead,

     xglobal, colours,

     ActionObject;


type
  TMouseOverEvent = procedure(const x, y : integer) of object;
  TDebugEvent     = procedure(const s : string) of object;

  TPreviewOptions = record
                      Active            : boolean;
                      Size              : integer; // 1...50
                      View              : TViewShape;

                      RPixel            : integer; // size of pixel in radial mode
                      ROffSet           : integer; // size of the inner void (pixels, radius) for radial/semi-circle

                      OldSize           : integer;
                      IncrementRadially : boolean;
                    end;

  TMatrixDetails   = record
                        Available   : boolean;

                        Mode        : TMatrixMode;

                        Width       : integer; // actual width of matrix in pixels
                        Height      : integer; // actual height of matrix in pixels

                        Grid        : boolean;
                        Comment     : string;
                     end;


  TMatrixRendering = record
                       PixelSize      : integer;
                       PixelSizeZ     : integer;
                       PixelShape     : TPixelShape;
                       BrushSize      : TBrushSize;
                       TopLeft        : TPoint; // index of the top left pixel (on screen) in x and y direction
                                                // used when matrix is larger than display
                       BottomRight    : TPoint; //
                                                // used when matrix is larger than display

                       ViewWindow     : TPoint; // width and height, in pixels, of the display

                       Gradient       : TGradientOption;
                       GradientIY     : array[0.._MaxHeight] of integer;
                       GradientIX     : array[0.._MaxWidth] of integer;

                       DrawData       : TDrawData;
  end;


  TTheMatrix = class
  private
    FBusy                  : boolean;

    FDisplayBuffer         : TMatrix;

    FPaintBox              : TPaintBox;
    PreviewBox             : TPaintBox;
    FOnChange              : TNotifyEvent;
    FOnLayerChange         : TNotifyEvent;
    FOnSizeChange          : TNotifyEvent;
    FOnDisplayBufferCopied : TNotifyEvent;
    FOnNewFrameDisplayed   : TNotifyEvent;
    FOnColourChange        : TNotifyEvent;
    FOnMouseOver           : TMouseOverEvent;
    FOnPreviewMouseDown    : TMouseOverEvent;
    FOnDebugEvent          : TDebugEvent;

    FCanvasBackground      : integer;

    FLastMouseButton       : integer;

    FPreviewPopout         : boolean;
    FPreviewOwner          : TComponent;
    FPreviewCanvas         : TWinControl;

    FOwner                 : TComponent;
    FCanvas                : TWinControl;

    FAutomateMode          : boolean;

    FFrameCount            : integer;
    FCurrentFrame          : integer;
    FCurrentLayer          : integer;
    FLightBox              : integer;
    FRGBBackground         : integer;
    FRandomCoeff           : integer;
    FDeadPixelsMode        : boolean;
    FMatrixReadOnly        : boolean;
    FSoftwareMode          : TSoftwareMode;

    FRadialOffsetDegrees   : integer; // combination of the two below
    FRadialOffset          : integer;
    FRadialOffsetDirection : boolean;

    FPreviewOptions        : TPreviewOptions;

    FMirrorMode            : TMirrorMode;

    FMatrixMerge           : TMatrix;

    FGradient              : TList<Integer>;

    FScrollHorizontal      : TScrollBar;
    FScrollVertical        : TScrollBar;

    procedure InitPreviewBox(aOwner : TComponent; aWinControl : TWinControl; aVisible : boolean);
    procedure InitMatrix;

    function  GetPixelFrom(aMatrixFormat, aImportFormat : TMatrixMode; aPixel, aBackground : integer): integer;

    procedure EnsureLayerCoherence;
    function  AreLayersIdentical(aLayer1, aLayer2, aFrame : integer): boolean;

    procedure CopyCurrentFrameToDrawBuffer;
    procedure CopyDrawBufferToCurrentFrame;

    procedure OnPreviewBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function  LoadDataParameterType(const s : string; aHeaderMode, aMatrixMode, aDeadPixelMode, aLayerMode, aColoursMode : boolean): TLoadData;

    procedure ClickPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure Shape1MouseUpBiColour(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClickPixelBiColour(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMoveBiColour(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure ClickPixelRGB(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMoveRGB(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseUpRGB(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

//    procedure ClickPixelRGB_3BPP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//    procedure Shape1MouseMoveRGB_3BPP(Sender: TObject; Shift: TShiftState; X, Y: Integer);
//    procedure Shape1MouseUpRGB_3BPP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ClickPixelDeadPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseMoveDeadPixel(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseUpDeadPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function  CurrentFrameCount: integer;
    function  GetPreviewPixelSize(aROffset : integer): integer;
    procedure SetPreviewBoxSize(aSize : integer);
    procedure SetPreviewIncrementRadially(aIncrement : boolean);
    procedure SetPreviewActive(aActive : boolean);
    procedure SetPreviewViewMode(aMode : TViewShape);
    procedure SetPreviewVoid(aOffset : integer);
    procedure SetPreviewPopout(aPopout : boolean);

    //function  GetColourFromXY(x, y : integer): integer;

    procedure pbPreviewPaint(Sender: TObject);
    procedure pbPreviewPaintRadial(Sender: TObject);
    procedure pbPreviewPaintRadialThreeQuarters(Sender: TObject);
    procedure pbPreviewPaintSemiCircle(Sender: TObject);
    procedure pbPreviewPaintSemiCircleInverted(Sender: TObject);

    procedure ConfigurePaintboxDrawing;

    procedure BuildMonoBiRenderFrame;
    procedure BuildRGBRenderFrame;
    procedure BuildRGB3BPPRenderFrame;

    procedure PaintBoxUpdate(Sender: TObject);
    procedure PaintBoxUpdateRGB(Sender: TObject);
    procedure PaintBoxUpdateRGB_3BPP(Sender: TObject);
    procedure PaintBoxUpdateDeadPixel(Sender: TObject);

    procedure DrawWithBrush(aIndex, x, y : integer);
    procedure DrawWithBrushMulti(aIndex, x, y : integer);
    procedure DrawWithGradientBrush(x, y : integer);
    procedure DrawWithBrushPaste(x1, y1 : integer; aTransparent : boolean);

    procedure CopyShape;
    procedure UpdateDrawTool(aSetX, aSetY, aSetColour : integer; aIsGradient : boolean);
    procedure PlotInBounds(aX, aY, aColour : integer);

    procedure DrawShape(aRealTime : boolean; aColour : integer; aIsGradient : boolean);

    function  HexToInt(const s : string): integer;
    function  BrightenRGB(aRGB : integer): integer;
    function  RandomColour(aRGB : integer): integer;

    procedure ChangeSelectionColour(aSelectionLMB, aSelectionMMB, aSelectionRMB : integer);

    procedure ScrollBarHorizontalChange(Sender: TObject);
    procedure ScrollBarVerticalChange(Sender: TObject);

  public

    Matrix            : TMatrixDetails;
    Render            : TMatrixRendering;

    LastX, LastY      : integer;

    AnimPlaying       : boolean;

    SelectionLMB      : integer;
    SelectionMMB      : integer;
    SelectionRMB      : integer;

    LEDColoursSingle  : array[0..5] of integer; // used as backups only
    LEDColoursBi      : array[0..5] of integer; // used as backups only

    LEDColours        : array[0..5] of integer; // currently being displayed
    LEDRGBColours     : array[0..3] of integer; // background, lmb, mmb, rmb
    LEDRGB3BPPColours : array[0..7] of integer;

    MatrixLayers      : TObjectList<TLayer>;
    MatrixUser        : TObjectList<TMatrix>;
    MatrixBackup      : TMatrix;
    MatrixCopy        : TMatrix;
    MatrixDead        : TMatrixDead;
    MatrixRender      : TMatrix;

    FontMatrixMode    : TMatrixMode; // mirrors Matrix.Mode, but shows what the font was loaded as
    FontMatrix        : array[0..95, 0..7, 0..7] of integer;
    FontMatrixStart   : array[0..95] of integer;
    FontMatrixEnd     : array[0..95] of integer;

    constructor Create(AOwner: TComponent; Zig : TWinControl);
    destructor  Destroy; Override;

    procedure   CreateMatrixMerge;
    procedure   FreeMatrixMerge;

    procedure   NewMatrix(aMatrixMode : TMatrixMode;
                          aFrameCount, aTop, aLeft, aWidth, aHeight, aPixelSize : integer;
                          aPixelShape : TPixelShape;
                          aGrid, aReadonly, aClearAll : boolean;
                          aBackgroundColour : integer);

    procedure   SetYPos(aNewYPos : integer);
    procedure   SetBackgroundColour(aNewColour : integer);

    procedure   CancelDrawMode;

    procedure   ChangePixelSize(aNewPixelSize : integer);
    procedure   ChangeZoomUI(aNewPixelSize : integer);
    procedure   ChangePixelShape(aNewPixelShape : TPixelShape);
    procedure   ChangePixelBrush(aNewBrushSize : TBrushSize);
    procedure   ChangeMatrixMode(aNewMatrixMode : TMatrixMode);
    procedure   ChangeSoftwareMode(aSoftwareMode : TSoftwareMode);
    procedure   SetRadialOffset(aRadialOffset : integer);
    procedure   SetRadialOffsetDirection(aRadialOffsetDirection : boolean);
    procedure   SetShapeParameter(aParameter : integer);
    procedure   SetMirrorMode(aNewMode : TMirrorMode);

    procedure   SetMouseButtonColours(aLMB, aMMB, aRMB : integer);

    procedure   PlotPixelMatrix(x, y, aDefaultColour : integer); // use only this function (or PlotPixelMatrixFrame) to draw on the matrix outside of this class
    procedure   PlotPixelMatrixFrame(aFrame, x, y, aDefaultColour : integer);

    function    RowToString(aFrame, aRow : integer): string;
    procedure   StringToRow(aCopyBrush : boolean; aString : string; aFrame, aRow, aTransparentColour : integer; aTransparent : boolean);

    procedure   BackupMatrix(aLayer, aFrame : integer);

    procedure   SetDeadPixels(aDeadness : TPixelType);
    procedure   SetDeadPixelsFromCustomShape(aShape : TCustomShape; aParameter : integer);
    procedure   SetDeadPixelsFromFileName(aFileName : string);
    procedure   SaveDeadPixels(aFileName : string);

    procedure   ClearGradients;
    procedure   ClearCurrentFrame;
    procedure   ClearCurrentLayer;
    procedure   ClearFrame(aFrame : integer);
    procedure   ClearAllFrames;
    procedure   WipeAllFramesCurrentLayer;
    procedure   WipeAllFramesAllLayers;
    procedure   ClearAllFramesGradient(aMode : integer);
    procedure   ClearFont;

    // =========================================================================

    procedure   PerformEffectController(aMode, aMultipleOptionMode : integer);
    procedure   PerformEffect(aMode, aLayer, aFrame : integer);

    procedure   PerformScrollController(aMode, aMultipleOptionMode : integer);
    procedure   PerformScroll(aMode, aLayer, aFrame : Integer);

    procedure   PerformSplitScroll(aMode, aLayer, aFrame : integer);

    procedure   PerformAlternateScroll(aMode, aLayer, aFrame : integer);

    procedure   RotateFrameController(aMode, aMultipleOptionMode : integer);
    procedure   RotateFrame(aMode, aLayer, aFrame : integer);

    procedure   PerformWipeOnCurrentFrame(aMode : integer; aClear : boolean);
    procedure   PerformRevealOnCurrentFrame(aMode, aColour : integer; var aParameter : integer);
    procedure   PerformScrollOnCopyFrame(aMode : integer);
    procedure   PerformColumnScrollOnCurrentFrame(aMode, aColumn : integer; aClear : boolean);
    procedure   PerformRowScrollOnCurrentFrame(aMode, aRow : integer; aClear : boolean);
    procedure   RotateFrameAnyAngle(aNewAngle : real; aToFrame : integer);

    // =========================================================================

    procedure   ScrollRow(aLayer, aFrame, aMode, aRow : integer);
    procedure   ScrollColumn(aLayer, aFrame, aMode, aColumn : integer);

    procedure   RotateCopyBrush(aMode : integer);
    procedure   PerformEffectOnBrush(aMode : integer);

    procedure   CopyCurrentFrame;
    procedure   CopyBackupToCopyFrame;
    procedure   PasteCurrentFrame;
    procedure   PasteSpecial(aMode : integer);

    procedure   DrawWithBrushPasteEveryFrame(x1, y1 : integer; aTransparent : boolean);

    procedure   InsertBlankFrameAt(aInsertAt : integer);
    procedure   InsertCopyFrameAt(aInsertAt : integer);
    procedure   AddFrameMultiple(aFrameCount, aFramecurrent : integer);

    procedure   DeleteFrame(aFrame : integer);

    function    IsThisFrameLocked(aLayer, aFrame : integer): boolean;
    function    IsLocked: boolean;
    procedure   UnLockCurrentFrame;
    procedure   LockCurrentFrame;
    procedure   LockUnLockRange(aStart, aEnd : integer; aNewLockStatus : boolean);
    procedure   LockLayer(aLayer : integer);
    procedure   UnlockLayer(aLayer : integer);
    function    IsLayerLocked(aLayer : integer): boolean;

    procedure   ChangeCurrentFrame(aFrame : integer);
    procedure   ChangeCurrentLayer(aLayer : integer);
    procedure   ChangeLightBox(aLightBoxMode : integer);
    procedure   ChangeGrid(aGrid : boolean);
    procedure   ChangeDeadPixelsMode(aMode : boolean);
    procedure   ChangeMatrixReadOnly(aMode : boolean);

    procedure   ChangePixels(aFrom, aTo : integer);

    procedure   FadeFirstToLast;

    procedure   DrawFontCharacter(aASCIICode, aFrame : integer);
    procedure   DeleteFontCharacter(aFrame : integer);
    procedure   LoadFont(filename : string);

    procedure   ImportRowData(aHex : boolean; aSourceDirection, aSourceLSB : integer; s : string);
    procedure   ImportColumnData(aHex : boolean; aSourceDirection, aSourceLSB : integer; s : string);
    function    ImportLEDMatrixDataSingleFrame(aFileName : string): TImportData;
    function    ImportFromBMPSingleImage(aFileName : string; aFCount, aFWidth, aFHeight : integer; aRGBImport, aCreateNew : boolean): TImportData;
    function    ImportFromBMPMultipleImage(aPattern : string; aStartFrame, aCount, aPadLength, aFWidth, aFHeight : integer; aRGBImport, aCreateNew : boolean): TImportData;
    function    ExportToBitmap(aFileName : string): boolean;
    function    ExportAnimationToBitmap(aFileName : string): boolean;

    procedure   SaveAnimation(aFilename : string; aTED : TImportData; aEEO : TExportOptions; aColours : TColours);
    procedure   SaveFont(aFilename : string; aTED : TImportData; aEEO : TExportOptions);
    procedure   SaveAsFont(aFilename : string);
    procedure   SaveAsRGBFont(aFilename : string);
    procedure   SaveSingleFrame(aFilename : string; aTED : TImportData; aFrame : integer);

    procedure   LoadGradient(aFilename : string);

    function    LoadLEDMatrixData(aFileName : string; var aEEO : TExportOptions; aLoadMode : TLoadMode; aStartFrame : integer): TImportData;

    function    ImportFromGIF(aFileName : string): TImportData;
    procedure   ExportToGIF(aBackground, aPixelSize, aPixelShape, aAnimationSpeed : integer; aFileName : string);

    procedure   ClearUserBuffers;
    procedure   CopyToUserBuffer(aFrame : integer);
    procedure   RestoreFromUserBuffer(aFrame : integer);

    procedure   CopyFromPrevious(toframe : integer);
    procedure   CopyAllLayersFromTo(aFromFrame, aToFrame : integer);
    procedure   CopyLayerFromTo(aSourceLayer, aDestinationLayer, aFromFrame, aToFrame : integer);

    procedure   CopyLEDColours;

    procedure   Undo;
    procedure   Redo;
    function    GetUndoCount: integer;
    procedure   SetFromUndo(aUndo : integer);

    function    CanUndo: boolean;
    function    CanRedo: boolean;

    function    CalculateMemoryUsage: integer;
    function    DataSizeBytes: integer;

    procedure   Refresh;

    function    GetTotalUndos: integer;

    procedure   Automate(var aAO : TActionObject);
    procedure   AutomationActionExecute(var aAO : TActionObject; aActionID : integer);
    procedure   AutomationPostProcessExecute(var aAO : TActionObject; aActionID : integer);

    procedure   ChangeColourCurrent(aFrom, aTo : integer);
    procedure   ChangeColourCurrentLayer(aFrom, aTo : integer);
    procedure   ChangeColourAll(aFrom, aTo : integer);

    procedure   FloodFill(x, y, aFillColour : integer);
    procedure   DoFill(x, y, aFillColour : integer);

    function    GetLayerCount: integer;
    function    GetLayerName(aLayerIndex : integer): string;
    procedure   SetLayerName(aLayerIndex : integer; aNewName : string);
    function    AddLayer(aName : string): boolean;
    function    AddLayerAsCopy(aName : string; aCopyLayer : integer): boolean;
    function    DeleteLayer(aIndex : integer): boolean;
    procedure   ClearCurrentLayerAllFrames;
    procedure   FlattenAllLayers;
    function    IsVisible(aLayerIndex : integer): boolean;

    procedure   SetVisibility(aLayerIndex : integer; aVisibility : boolean);
    procedure   MoveUp(aLayerIndex : integer);
    procedure   MoveDown(aLayerIndex : integer);

    procedure   OnPreviewBoxCanvasResize(Sender: TObject);

    function    RightBounds: integer;
    function    BottomBounds: integer;

    function    CountColoursFrame: integer;
    function    CountColoursAnimation: integer;
    procedure   GetFirst32Colours(var aColourList : TStringList);

    procedure   BuildMergedFrame(aFrame, aColours : integer);

    procedure   ClearGradient;
    procedure   AddGradient(aColour : integer);
  published
    property    AutomateMode          : boolean         read FAutomateMode                     write FAutomateMode;
    property    FrameCount            : integer         read CurrentFrameCount;
    property    CurrentFrame          : integer         read FCurrentFrame                     write ChangeCurrentFrame;
    property    CurrentLayer          : integer         read FCurrentLayer                     write ChangeCurrentLayer;
    property    LightBox              : integer         read FLightBox                         write ChangeLightBox;
    property    RGBBackground         : integer         read FRGBBackground                    write FRGBBackground;
    property    RandomCoeff           : integer         read FRandomCoeff                      write FRandomCoeff;
    property    DeadPixelsMode        : boolean         read FDeadPixelsMode                   write ChangeDeadPixelsMode;
    property    MatrixReadOnly        : boolean         read FMatrixReadOnly                   write ChangeMatrixReadOnly;
    property    SoftwareMode          : TSoftwareMode   read FSoftwareMode                     write ChangeSoftwareMode;
    property    RadialOffset          : integer         read FRadialOffset                     write SetRadialOffset;
    property    RadialOffsetDirection : boolean         read FRadialOffsetDirection            write SetRadialOffsetDirection;
    property    ShapeParameter        : integer         read Render.DrawData.Parameter         write SetShapeParameter;
    property    MirrorMode            : TMirrorMode     read FMirrorMode                       write SetMirrorMode;

    property    PreviewActive         : boolean         read FPreviewOptions.Active            write SetPreviewActive;
    property    PreviewBoxSize        : integer         read FPreviewOptions.Size              write SetPreviewBoxSize;
    property    PreviewIncRadially    : boolean         read FPreviewOptions.IncrementRadially write SetPreviewIncrementRadially;
    property    PreviewView           : TViewShape      read FPreviewOptions.View              write SetPreviewViewMode;
    property    PreviewVoid           : integer         read FPreviewOptions.ROffset           write SetPreviewVoid;
    property    PreviewPopout         : boolean         read FPreviewPopout                    write SetPreviewPopout;
    property    PreviewOwner          : TComponent      read FPreviewOwner                     write FPreviewOwner;
    property    PreviewComponent      : TWinControl     read FPreviewCanvas                    write FPreviewCanvas;

    property    OnNewFrameDisplayed   : TNotifyEvent    read FOnNewFrameDisplayed              write FOnNewFrameDisplayed;
    property    OnDisplayBufferCopied : TNotifyEvent    read FOnDisplayBufferCopied            write FOnDisplayBufferCopied;
    property    OnChange              : TNotifyEvent    read FOnChange                         write FOnChange;
    property    OnLayerChange         : TNotifyEvent    read FOnLayerChange                    write FOnLayerChange;
    property    OnSizeChange          : TNotifyEvent    read FOnSizeChange                     write FOnSizeChange;
    property    OnMouseOver           : TMouseOverEvent read FOnMouseOver                      write FOnMouseOver;
    property    OnColourChange        : TNotifyEvent    read FOnColourChange                   write FOnColourChange;
    property    OnPreviewMouseDown    : TMouseOverEvent read FOnPreviewMouseDown               write FOnPreviewMouseDown;
    property    OnDebug               : TDebugEvent     read FOnDebugEvent                     write FOnDebugEvent;

    property    MatrixMerge           : TMatrix         read FMatrixMerge;
  protected
    procedure   MatrixChange; dynamic;
    procedure   MatrixLayerChange; dynamic;
    procedure   MatrixSizeChanged;
    procedure   MatrixNewFrameDisplayed; dynamic;
    procedure   ColourChange; dynamic;
    procedure   MouseOver; dynamic;

    procedure   Debug(const s : string); dynamic;
  end;


const
  DefaultPatternParameter    : array[14..21] of integer = ( 4,  0,  3,  8,  4,  1,  1,  1);
  DefaultPatternParameterMin : array[14..21] of integer = ( 1,  0,  1,  1,  2,  1,  1,  1);
  DefaultPatternParameterMax : array[14..21] of integer = (64,  0, 64, 64, 64, 64, 64, 64);

  CDisplayClear  = 0;
  CMouseLeft     = 1;
  CMouseMiddle   = 2;
  CMouseRight    = 3;
  CDisplayMarker = 4;
  CLightBoxShade = 5;

  CPermanentLayer = 0;

  CMOMCurrentOnly        = 0;
  CMOMCurrentFrameLayers = 1;
  CMOMCurrentLayerFrames = 2;
  CMOMAll                = 3;


implementation


uses dialogs, utility;


constructor TTheMatrix.Create(AOwner: TComponent; Zig : TWinControl);
var
  lLayer  : TLayer;
  lMatrix : TMatrix;
  x : integer;

begin
  FOwner                       := AOwner; // cache it for later!
  FCanvas                      := Zig;

  FBusy                        := False;

  FPaintBox                    := TPaintBox.Create(aOwner);
  FPaintBox.Parent             := FCanvas;
  FPaintBox.OnPaint            := PaintBoxUpdate;

  InitPreviewBox(aOwner, Zig, False);

  FPreviewOptions.Active       := False;
  FPreviewOptions.Size         := 1;
  FPreviewOptions.View         := vsSquare;
  FPreviewOptions.ROffset      := 15;

  Matrix.Available             := False;

  FAutomateMode                := False;
  FFrameCount                  := 1;
  FCurrentFrame                := 1;
  FCurrentLayer                := 0;
  FLightBox                    := 0;
  FDeadPixelsMode              := False;

  Render.DrawData.Mode         := dmNone;
  Render.DrawData.Point        := CDrawPointNone;
  Render.DrawData.Colour       := 1;
  Render.DrawData.Coords[0].X  := -1;
  Render.DrawData.Coords[0].Y  := -1;

  FRandomCoeff                 := 30;

  FRadialOffsetDegrees         := 0;
  FRadialOffset                := 0;
  FRadialOffsetDirection       := False;

  Render.Gradient              := goOff;

  Render.BrushSize             := bsSmall;
  Render.PixelSize             := 1;

  Matrix.Comment               := '';

  LEDRGB3BPPColours[0]         := $00000000; // 000
  LEDRGB3BPPColours[1]         := $00FF0000; // 001
  LEDRGB3BPPColours[2]         := $0000FF00; // 010
  LEDRGB3BPPColours[3]         := $00FFFF00; // 011
  LEDRGB3BPPColours[4]         := $000000FF; // 100
  LEDRGB3BPPColours[5]         := $00FF00FF; // 101
  LEDRGB3BPPColours[6]         := $0000FFFF; // 110
  LEDRGB3BPPColours[7]         := $00FFFFFF; // 111

  // ===========================================================================

  FScrollHorizontal := TScrollBar.Create(FOwner);
  FScrollHorizontal.Parent   := FCanvas ;
  FScrollHorizontal.Align    := alBottom;
  FScrollHorizontal.Kind     := sbHorizontal;
  FScrollHorizontal.Name     := 'FSH';
  FScrollHorizontal.Min      := 0;
  FScrollHorizontal.OnChange := ScrollBarHorizontalChange;
  FScrollHorizontal.Visible  := False;

  FScrollVertical := TScrollBar.Create(FOwner);
  FScrollVertical.Parent   := FCanvas;
  FScrollVertical.Align    := alRight;
  FScrollVertical.Kind     := sbVertical;
  FScrollVertical.Name     := 'FSV';
  FScrollVertical.Min      := 0;
  FScrollVertical.OnChange := ScrollBarVerticalChange;
  FScrollVertical.Visible  := False;

  // ===========================================================================

  MatrixLayers            := TObjectList<TLayer>.Create;

  lLayer := TLayer.Create(GLanguageHandler.Text[kBottomLayer]);
  MatrixLayers.Add(lLayer);

  // ===========================================================================

  FLastMouseButton        := 0;

  FDisplayBuffer          := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);

  MatrixBackup            := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);
  MatrixCopy              := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);
  MatrixRender            := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);

  MatrixDead              := TMatrixDead.Create;

  MatrixUser              := TObjectList<TMatrix>.Create;

  for x := 1 to 10 do begin
    lMatrix := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);
    MatrixUser.Add(lMatrix);
  end;

  // ===========================================================================

  FGradient := TList<Integer>.Create;

  // ===========================================================================

  with FPaintBox do begin
    OnMouseDown := ClickPixel;
    OnMouseMove := Shape1MouseMove;
    OnMouseUp   := Shape1MouseUp;
  end;

  // ===========================================================================

  ClearAllFrames;
end;


destructor TTheMatrix.Destroy;
begin
  FGradient.Free;
  FPaintBox.Free;
  PreviewBox.Free;
  MatrixLayers.Free;
  MatrixBackup.Free;
  MatrixCopy.Free;
  MatrixUser.Free;

  inherited Destroy;
end;


procedure TTheMatrix.CreateMatrixMerge;
begin
  FMatrixMerge := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);
end;


procedure TTheMatrix.FreeMatrixMerge;
begin
  if Assigned(FMatrixMerge) then
    FMatrixMerge.Free;
end;


procedure TTheMatrix.InitPreviewBox(aOwner : TComponent; aWinControl : TWinControl; aVisible : boolean);
begin
  PreviewBox                  := TPaintBox.Create(aOwner);
  PreviewBox.Parent           := aWinControl;
  PreviewBox.Visible          := aVisible;
  PreviewBox.Top              := 0;
  PreviewBox.Left             := 0;
  PreviewBox.OnMouseDown      := OnPreviewBoxMouseDown;

  PreviewBox.Canvas.Pen.Color := clBtnFace;

  FPreviewCanvas              := aWinControl;

  SetPreviewBoxSize(FPreviewOptions.Size);
end;


procedure TTheMatrix.CopyCurrentFrameToDrawBuffer;
var
  x, y : integer;

begin
  if FBusy then Exit;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      FDisplayBuffer.Grid[x, y] := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y];
    end;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.CopyDrawBufferToCurrentFrame;
var
  x, y : integer;

begin
  if FBusy then Exit;

  if not(MatrixLayers[FCurrentLayer].Visible) then Exit;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := FDisplayBuffer.Grid[x, y];
    end;
  end;

  FPaintBox.Invalidate;

  if (Assigned(FOnDisplayBufferCopied)) and (Matrix.Available) then
    FOnDisplayBufferCopied(Self);
end;


procedure TTheMatrix.InitMatrix;
var
  lMatrix : TMatrix;

begin
  lMatrix := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);
  MatrixLayers[CPermanentLayer].Frames.Add(lMatrix);

  lMatrix := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground);
  MatrixLayers[CPermanentLayer].Frames.Add(lMatrix);
end;


// frame and/or layer count has changed
procedure TTheMatrix.MatrixSizeChanged;
begin
  if (Assigned(FOnSizeChange)) and (Matrix.Available) then
    FOnSizeChange(Self);
end;


// the contents have the current matrix have changed
procedure TTheMatrix.MatrixChange;
begin
  if (Assigned(FOnChange)) and (Matrix.Available) then
    FOnChange(Self);
end;


// informs the owner app that it needs to update the layer table (etc.)
procedure TTheMatrix.MatrixLayerChange;
begin
  if (Assigned(FOnLayerChange)) and (Matrix.Available) then
    FOnLayerChange(Self);
end;


// a new frame is now being displayed
procedure TTheMatrix.MatrixNewFrameDisplayed;
begin
  if (Assigned(FOnNewFrameDisplayed)) and (Matrix.Available) then
    FOnNewFrameDisplayed(Self);
end;


procedure TTheMatrix.ColourChange;
begin
  if (Assigned(FOnColourChange)) and (Matrix.Available) then
    FOnColourChange(Self);
end;


// sends debug text to the parent application
procedure TTheMatrix.Debug(const s : string);
begin
  if (Assigned(FOnDebugEvent)) then
    FOnDebugEvent(s);
end;


procedure TTheMatrix.OnPreviewBoxCanvasResize(Sender: TObject);
begin
  SetPreviewBoxSize(previewPixelSizeAuto);
end;


procedure TTheMatrix.OnPreviewBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssRight in shift then begin
    if (Assigned(FOnPreviewMouseDown)) then
      FOnPreviewMouseDown(PreviewBox.Left + X, PreviewBox.Top + Y);
  end;
end;


procedure TTheMatrix.MouseOver;
begin
//  if Assigned(FOnMouseOver) then
//    FOnMouseOver();
end;


// radial, semi-circle only
function TTheMatrix.GetPreviewPixelSize(aROffset : integer): integer;
begin
  Result := 1;

  // calculate circumference at ROffSet pixels from centre = 2 * pi * ROffSet
  // this is the circumference of the smallest part of the display, we need
  // to make sure that the pixels fit in this gap!
  // Divide this by the number of pixels and we get the maximium pixel size.

  case FPreviewOptions.View of
    vsSquare             : {n / a};
    vsRadial             : begin
                             Result := Round((2 * Pi * aROffset) / Matrix.Width);

                             if Result = 0 then
                               Result := 1;
                           end;
    vsRadial3Q           : begin
                             Result := Round((2 * Pi * aROffset) / Matrix.Width);

                            if Result = 0 then
                              Result := 1;
                           end;
    vsSemiCircle         : begin
                             Result := Round((2 * Pi * aROffset) / (2 * Matrix.Width));

                             if Result = 0 then
                               Result := 1;
                           end;
    vsSemiCircleInverted : begin
                             Result   := Round((2 * Pi * aROffset) / (2 * Matrix.Width));

                             if Result = 0 then
                               Result := 1;
                           end;
  end;
end;


procedure TTheMatrix.SetPreviewBoxSize(aSize : integer);
var
  lMD : integer;

begin
  if (FPreviewPopout) then begin
    PreviewBox.Width     := TPanel(PreviewBox.Parent).Width;
    PreviewBox.Height    := TPanel(PreviewBox.Parent).Height;

    lMD := Min(Round(PreviewBox.Width / Matrix.Width), Round(PreviewBox.Height / Matrix.Height));

    FPreviewOptions.Size := lMD;
  end
  else begin
    FPreviewOptions.Size := aSize;

    PreviewBox.Width     := (Matrix.Width) * FPreviewOptions.Size;
    PreviewBox.Height    := (Matrix.Height) * FPreviewOptions.Size;
  end;

  if not(Matrix.Available) then
    Exit;

  case FPreviewOptions.View of
    vsSquare             : begin
                             PreviewBox.OnPaint     := pbPreviewPaint
                           end;
    vsRadial             : begin
                             PreviewBox.OnPaint     := pbPreviewPaintRadial;

                             FPreviewOptions.RPixel := GetPreviewPixelSize(FPreviewOptions.ROffSet);
                           end;
    vsRadial3Q           : begin
                             PreviewBox.OnPaint     := pbPreviewPaintRadialThreeQuarters;

                             FPreviewOptions.RPixel := GetPreviewPixelSize(FPreviewOptions.ROffSet);
                           end;
    vsSemiCircle         : begin
                             PreviewBox.OnPaint     := pbPreviewPaintSemiCircle;

                             FPreviewOptions.RPixel := GetPreviewPixelSize(FPreviewOptions.ROffSet);
                           end;
    vsSemiCircleInverted : begin
                             PreviewBox.OnPaint     := pbPreviewPaintSemiCircleInverted;

                             FPreviewOptions.RPixel := GetPreviewPixelSize(FPreviewOptions.ROffSet);
                           end;
  end;

  if (FPreviewOptions.View <> vsSquare) and (not(FPreviewPopout)) then begin
    lMD := Max(PreviewBox.Width, PreviewBox.Height);

    PreviewBox.Width  := lMD;
    PreviewBox.Height := lMD;
  end;

  if (FPreviewPopout) then
    PreviewBox.Left := 0
  else
    PreviewBox.Left := CLeftOffset + (Render.PixelSize * (Matrix.Width)) + 20;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.SetPreviewIncrementRadially(aIncrement : boolean);
begin
  FPreviewOptions.IncrementRadially := aIncrement;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.SetPreviewActive(aActive : boolean);
begin
  FPreviewOptions.Active := aActive;
  PreviewBox.Visible     := aActive;

  if (aActive) then
    SetPreviewBoxSize(FPreviewOptions.Size);

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.SetPreviewViewMode(aMode : TViewShape);
begin
  FPreviewOptions.View := aMode;

  SetPreviewBoxSize(FPreviewOptions.Size);
end;


procedure TTheMatrix.SetPreviewVoid(aOffset : integer);
begin
  FPreviewOptions.ROffset := aOffset;

  SetPreviewBoxSize(FPreviewOptions.Size);
end;


procedure TTheMatrix.SetPreviewPopout(aPopout : boolean);
begin
  FPreviewPopout := aPopout;

  PreviewBox.Free;

  if (aPopout) then begin
    FPreviewOptions.OldSize := FPreviewOptions.Size;

    InitPreviewBox(FPreviewOwner, FPreviewCanvas, True);

    TPanel(FPreviewCanvas).OnResize := OnPreviewBoxCanvasResize;
  end
  else begin
    InitPreviewBox(FOwner, FCanvas, FPreviewOptions.Active);

    SetPreviewBoxSize(FPreviewOptions.OldSize);
  end;
end;


// value should be the same for all layers, so just return those of layer 0
function TTheMatrix.CurrentFrameCount: integer;
begin
  Result := MatrixLayers[CPermanentLayer].Frames.Count - 1;
end;


procedure TTheMatrix.CancelDrawMode;
begin
  MatrixMain.Render.DrawData.Mode        := dmNone;
  MatrixMain.Render.DrawData.Point       := CDrawPointNone;
  MatrixMain.Render.DrawData.Coords[0].X := -1;
  MatrixMain.Render.DrawData.Coords[0].Y := -1;
  MatrixMain.Render.DrawData.CopyPos.X   := 0;
  MatrixMain.Render.DrawData.CopyPos.Y   := 0;

  CopyCurrentFrameToDrawBuffer;
end;


// merges all layers to a single layer. pixels "rain" down from top (highest index)
// to bottom (lowest index)
// colours = 0 ; retain grid value
// colours = 1 ; convert to colour for render
// colours = 2 ; convert for file output
procedure TTheMatrix.BuildMergedFrame(aFrame, aColours : integer);
var
  x, y, l : integer;

begin
  FMatrixMerge.ClearColour(FRGBBackground); // to do check that works for single colour

  for l := 0 to MatrixLayers.Count - 1 do begin
    if MatrixLayers[l].Visible then begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          case Matrix.Mode of
            mtMono,
            mtBiSequential,
            mtBiBitplanes   : case MatrixLayers[l].Frames[aFrame].Grid[x, y] of
                                0       : begin
                                          end;
                                1, 2, 3 : case (aColours) of
                                            0 : FMatrixMerge.Grid[x, y] := MatrixLayers[l].Frames[aFrame].Grid[x, y];
                                            1 : FMatrixMerge.Grid[x, y] := LEDColours[MatrixLayers[l].Frames[aFrame].Grid[x, y]];
                                            2 : case (MatrixLayers[l].Frames[aFrame].Grid[x, y]) of
                                                  0 : FMatrixMerge.Grid[x, y] := $00000000;
                                                  1 : FMatrixMerge.Grid[x, y] := $00ffffff;
                                                  2 : FMatrixMerge.Grid[x, y] := LEDColours[CMouseMiddle];
                                                  3 : FMatrixMerge.Grid[x, y] := LEDColours[CMouseRight];
                                                end;
                                          end;
                              end;
            mtRGB           : begin
                                if (MatrixLayers[l].Frames[aFrame].Grid[x, y] <> FRGBBackground) then
                                  FMatrixMerge.Grid[x, y] := MatrixLayers[l].Frames[aFrame].Grid[x, y];
                                end;
            mtRGB3BPP       : begin
                                if (MatrixLayers[l].Frames[aFrame].Grid[x, y] <> FRGBBackground) then
                                   case (aColours) of
                                     0    : FMatrixMerge.Grid[x, y] := MatrixLayers[l].Frames[aFrame].Grid[x, y];
                                     1, 2 : FMatrixMerge.Grid[x, y] := LEDRGB3BPPColours[MatrixLayers[l].Frames[aFrame].Grid[x, y]];
                                   end;
                              end;
           end;
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.BuildMonoBiRenderFrame;
var
  x, y, l : integer;

begin
  MatrixRender.ClearColour(LEDColours[CDisplayClear]);

  for l := 0 to MatrixLayers.Count - 1 do begin
    if MatrixLayers[l].Visible then begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if MatrixDead.Grid[x, y]  <> ptNormal then
            MatrixRender.Grid[x, y] := FCanvasBackground
          else begin
            if (l = FCurrentLayer) then begin
              case FDisplayBuffer.Grid[x, y] of
                0 : begin
                      if (MatrixRender.Grid[x, y] = LEDColours[0]) then begin
                        if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                          if MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y] = 1 then
                            MatrixRender.Grid[x, y] := LEDColours[CLightBoxShade];
                        end;
                      end;
                    end;
                1 : MatrixRender.Grid[x, y] := LEDColours[CMouseLeft];
                2 : MatrixRender.Grid[x, y] := LEDColours[CMouseMiddle];
                3 : MatrixRender.Grid[x, y] := LEDColours[CMouseRight];
              end;
            end
            else begin
              case MatrixLayers[l].Frames[FCurrentFrame].Grid[x, y] of
                0 : begin
                      if (MatrixRender.Grid[x, y] = LEDColours[CDisplayClear]) then begin
                        if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                          if MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y] = 1 then
                            MatrixRender.Grid[x, y] := LEDColours[CLightBoxShade];
                        end;
                      end;
                    end;
                1 : MatrixRender.Grid[x, y] := LEDColours[CMouseLeft];
                2 : MatrixRender.Grid[x, y] := LEDColours[CMouseMiddle];
                3 : MatrixRender.Grid[x, y] := LEDColours[CMouseRight];
               end;
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.PaintBoxUpdate(Sender: TObject);
var
  x, y : integer;

begin
  BuildMonoBiRenderFrame;

  for x := 0 to Render.ViewWindow.X  do begin
    for y := 0 to Render.ViewWindow.Y do begin
      FPaintBox.Canvas.Brush.Color := MatrixRender.Grid[Render.TopLeft.X + x, Render.TopLeft.Y + y];

      case Render.PixelShape of
          psSquare    : FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                                       y * Render.PixelSize,
                                                      (x * Render.PixelSize) + Render.PixelSizeZ,
                                                      (y * Render.PixelSize) + Render.PixelSizeZ));
          psCircle    : FPaintBox.Canvas.Ellipse(x * Render.PixelSize,
                                                 y * Render.PixelSize,
                                                (x * Render.PixelSize) + Render.PixelSizeZ,
                                                (y * Render.PixelSize) + Render.PixelSizeZ);
          psRoundRect : FPaintBox.Canvas.RoundRect(x * Render.PixelSize,
                                                   y * Render.PixelSize,
                                                  (x * Render.PixelSize) + Render.PixelSizeZ,
                                                  (y * Render.PixelSize) + Render.PixelSizeZ,
                                                   Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                   Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
        else
          FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                         y * Render.PixelSize,
                                        (x * Render.PixelSize) + Render.PixelSizeZ,
                                        (y * Render.PixelSize) + Render.PixelSizeZ));
        end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.Mode <> dmNone then begin
    if ((Render.DrawData.SinglePoint) or (Render.DrawData.Coords[0].X <> - 1)) then begin
      FPaintBox.Canvas.Brush.Color := LEDColours[Render.DrawData.Colour];
      DrawShape(True, 1, False);

      // =======================================================================

      // single point modes don't require "first click" marker
      if (Render.DrawData.SinglePoint) then begin
        FPaintBox.Canvas.Brush.Color := LEDColours[CDisplayMarker];

        case Render.PixelShape of
          psSquare    : FPaintBox.Canvas.FillRect(Rect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                         Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                        (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                        (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
          psCircle    : FPaintBox.Canvas.Ellipse(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                   Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                  (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                  (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
          psRoundRect : FPaintBox.Canvas.RoundRect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                     Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                    (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                    (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
                                                    Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                    Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
        end;
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.CopyPos.X <> 0 then begin
    for x := 0 to Render.DrawData.CopyPos.X do begin
      for y := 0 to Render.DrawData.CopyPos.Y do begin
        if (x + lastx >= 0) and (x + lastx <= Matrix.Width - 1) and
          (y + lasty >= 0) and (y + lasty <= Matrix.Height - 1) then begin

           if MatrixDead.Grid[x + lastx, y + lasty] = ptNormal then
             FPaintBox.Canvas.Brush.Color := LEDColours[MatrixCopy.Grid[x, y]]      // ? to do
           else
             FPaintBox.Canvas.Brush.Color := FCanvasBackground;


           case Render.PixelShape of
             psSquare    : FPaintBox.Canvas.FillRect(Rect((x + lastx) * Render.PixelSize,
                                                          (y + lasty) * Render.PixelSize,
                                                         ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                         ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ));
             psCircle    : FPaintBox.Canvas.Ellipse((x + lastx) * Render.PixelSize,
                                                    (y + lasty) * Render.PixelSize,
                                                   ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                   ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ);
             psRoundRect : FPaintBox.Canvas.RoundRect((x + lastx) * Render.PixelSize,
                                                      (y + lasty) * Render.PixelSize,
                                                     ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                     ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ,
                                                       Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                       Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
           end;
        end;
      end;
    end;
  end;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.BuildRGBRenderFrame;
var
  x, y, l : integer;

begin
  MatrixRender.ClearColour(FRGBBackground);

  for l := 0 to MatrixLayers.Count - 1 do begin
    if MatrixLayers[l].Visible then begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if MatrixDead.Grid[x, y]  <> ptNormal then
            MatrixRender.Grid[x, y] := FCanvasBackground
          else begin
            if (l = FCurrentLayer) then begin
              if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                if (FDisplayBuffer.Grid[x, y] = FRGBBackground) then begin
                  MatrixRender.Grid[x, y] := BrightenRGB(MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y])
                end
              end
              else if (FDisplayBuffer.Grid[x, y] <> FRGBBackground) then
                MatrixRender.Grid[x, y] := FDisplayBuffer.Grid[x, y];
            end
            else begin
              if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                if (FDisplayBuffer.Grid[x, y] = FRGBBackground) then begin
                  MatrixRender.Grid[x, y] := BrightenRGB(MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y])
                end
              end
              else if (MatrixLayers[l].Frames[FCurrentFrame].Grid[x, y] <> FRGBBackground) then
                MatrixRender.Grid[x, y] := MatrixLayers[l].Frames[FCurrentFrame].Grid[x, y];
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.PaintBoxUpdateRGB(Sender: TObject);
var
  x, y : integer;

begin
  BuildRGBRenderFrame;

  for x := 0 to Render.ViewWindow.X  do begin
    for y := 0 to Render.ViewWindow.Y do begin
      FPaintBox.Canvas.Brush.Color := MatrixRender.Grid[Render.TopLeft.X + x, Render.TopLeft.Y + y];

      case Render.PixelShape of
        psSquare    : FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                                     y * Render.PixelSize,
                                                    (x * Render.PixelSize) + Render.PixelSizeZ,
                                                    (y * Render.PixelSize) + Render.PixelSizeZ));
        psCircle    : FPaintBox.Canvas.Ellipse(x * Render.PixelSize,
                                               y * Render.PixelSize,
                                              (x * Render.PixelSize) + Render.PixelSizeZ,
                                              (y * Render.PixelSize) + Render.PixelSizeZ);
        psRoundRect : FPaintBox.Canvas.RoundRect(x * Render.PixelSize,
                                                 y * Render.PixelSize,
                                                (x * Render.PixelSize) + Render.PixelSizeZ,
                                                (y * Render.PixelSize) + Render.PixelSizeZ,
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
      else
        FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                       y * Render.PixelSize,
                                      (x * Render.PixelSize) + Render.PixelSizeZ,
                                      (y * Render.PixelSize) + Render.PixelSizeZ));
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.Mode <> dmNone then begin
    if ((Render.DrawData.SinglePoint) or (Render.DrawData.Coords[0].X <> - 1)) then begin
      FPaintBox.Canvas.Brush.Color := Render.DrawData.Colour;
      DrawShape(True, Render.DrawData.Colour, False);

      // =======================================================================

      FPaintBox.Canvas.Brush.Color := LEDColours[CDisplayMarker];

      case Render.PixelShape of
        psSquare    : FPaintBox.Canvas.FillRect(Rect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                     Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                    (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                    (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
        psCircle    : FPaintBox.Canvas.Ellipse(Render.DrawData.Coords[0].X * Render.PixelSize,
                                               Render.DrawData.Coords[0].Y * Render.PixelSize,
                                              (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                              (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
        psRoundRect : FPaintBox.Canvas.RoundRect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                 Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.CopyPos.X <> 0 then begin
    for x := 0 to Render.DrawData.CopyPos.X do begin
      for y := 0 to Render.DrawData.CopyPos.Y do begin
        if (x + lastx >= 0) and (x + lastx <= Matrix.Width - 1) and
          (y + lasty >= 0) and (y + lasty <= Matrix.Height - 1) then begin

           if MatrixDead.Grid[x + lastx, y + lasty] = ptNormal then
             FPaintBox.Canvas.Brush.Color := MatrixCopy.Grid[x, y]  // ? to do
           else
             FPaintBox.Canvas.Brush.Color := FCanvasBackground;

           case Render.PixelShape of
             psSquare    : FPaintBox.Canvas.FillRect(Rect((x + lastx) * Render.PixelSize,
                                                          (y + lasty) * Render.PixelSize,
                                                         ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                         ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ));
             psCircle    : FPaintBox.Canvas.Ellipse((x + lastx) * Render.PixelSize,
                                                    (y + lasty) * Render.PixelSize,
                                                   ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                   ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ);
             psRoundRect : FPaintBox.Canvas.RoundRect((x + lastx) * Render.PixelSize,
                                                      (y + lasty) * Render.PixelSize,
                                                     ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                     ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ,
                                                      Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                      Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
           end;
        end;
      end;
    end;
  end;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.BuildRGB3BPPRenderFrame;
var
  x, y, l : integer;

begin
  MatrixRender.ClearColour(FRGBBackground);

  for l := 0 to MatrixLayers.Count - 1 do begin
    if MatrixLayers[l].Visible then begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if MatrixDead.Grid[x, y]  <> ptNormal then
            MatrixRender.Grid[x, y] := FCanvasBackground
          else begin
            if (l = FCurrentLayer) then begin
              if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                if (FDisplayBuffer.Grid[x, y] = FRGBBackground) then begin
                  if (FDisplayBuffer.Grid[x, y] <> FRGBBackground) then
                    MatrixRender.Grid[x, y] := BrightenRGB(LEDRGB3BPPColours[MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y]])
                end
              end
              else
                MatrixRender.Grid[x, y] := LEDRGB3BPPColours[FDisplayBuffer.Grid[x, y]];
            end
            else begin
              if (fLightbox = 1) and (FCurrentFrame <> 1) then begin
                if (FDisplayBuffer.Grid[x, y] = FRGBBackground) then begin
                  if (FDisplayBuffer.Grid[x, y] <> FRGBBackground) then
                    MatrixRender.Grid[x, y] := BrightenRGB(MatrixLayers[l].Frames[FCurrentFrame - 1].Grid[x, y])
                end
              end
              else
                MatrixRender.Grid[x, y] := LEDRGB3BPPColours[MatrixLayers[l].Frames[FCurrentFrame].Grid[x, y]];
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.PaintBoxUpdateRGB_3BPP(Sender: TObject);
var
  x, y : integer;

begin
  BuildRGB3BPPRenderFrame;

  for x := 0 to Render.ViewWindow.X  do begin
    for y := 0 to Render.ViewWindow.Y do begin
      FPaintBox.Canvas.Brush.Color := MatrixRender.Grid[Render.TopLeft.X + x, Render.TopLeft.Y + y];

      case Render.PixelShape of
          psSquare    : FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                                       y * Render.PixelSize,
                                                      (x * Render.PixelSize) + Render.PixelSizeZ,
                                                      (y * Render.PixelSize) + Render.PixelSizeZ));
          psCircle    : FPaintBox.Canvas.Ellipse(x * Render.PixelSize,
                                                 y * Render.PixelSize,
                                                (x * Render.PixelSize) + Render.PixelSizeZ,
                                                (y * Render.PixelSize) + Render.PixelSizeZ);
          psRoundRect : FPaintBox.Canvas.RoundRect(x * Render.PixelSize,
                                                   y * Render.PixelSize,
                                                  (x * Render.PixelSize) + Render.PixelSizeZ,
                                                  (y * Render.PixelSize) + Render.PixelSizeZ,
                                                   Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                   Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
        else
          FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                         y * Render.PixelSize,
                                        (x * Render.PixelSize) + Render.PixelSizeZ,
                                        (y * Render.PixelSize) + Render.PixelSizeZ));
        end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.Mode <> dmNone then begin
    if ((Render.DrawData.SinglePoint) or (Render.DrawData.Coords[0].X <> - 1)) then begin
      FPaintBox.Canvas.Brush.Color := Render.DrawData.Colour;
      DrawShape(True, Render.DrawData.Colour, False);

      // =======================================================================

      FPaintBox.Canvas.Brush.Color := LEDColours[CDisplayMarker];

      case Render.PixelShape of
        psSquare    : FPaintBox.Canvas.FillRect(Rect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                     Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                    (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                    (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
        psCircle    : FPaintBox.Canvas.Ellipse(Render.DrawData.Coords[0].X * Render.PixelSize,
                                               Render.DrawData.Coords[0].Y * Render.PixelSize,
                                              (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                              (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
        psRoundRect : FPaintBox.Canvas.RoundRect(Render.DrawData.Coords[0].X * Render.PixelSize,
                                                 Render.DrawData.Coords[0].Y * Render.PixelSize,
                                                (Render.DrawData.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
                                                (Render.DrawData.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.CopyPos.X <> 0 then begin
    for x := 0 to Render.DrawData.CopyPos.X do begin
      for y := 0 to Render.DrawData.CopyPos.Y do begin
        if (x + lastx >= 0) and (x + lastx <= Matrix.Width - 1) and
          (y + lasty >= 0) and (y + lasty <= Matrix.Height - 1) then begin

          if MatrixDead.Grid[x + lastx, y + lasty] = ptNormal then
            FPaintBox.Canvas.Brush.Color := LEDRGB3BPPColours[MatrixCopy.Grid[x, y]] // ? to do
          else
            FPaintBox.Canvas.Brush.Color := FCanvasBackground;

          case Render.PixelShape of
            psSquare    : FPaintBox.Canvas.FillRect(Rect((x + lastx) * Render.PixelSize,
                                                         (y + lasty) * Render.PixelSize,
                                                        ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                        ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ));
            psCircle    : FPaintBox.Canvas.Ellipse((x + lastx) * Render.PixelSize,
                                                   (y + lasty) * Render.PixelSize,
                                                  ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                  ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ);
            psRoundRect : FPaintBox.Canvas.RoundRect((x + lastx) * Render.PixelSize,
                                                     (y + lasty) * Render.PixelSize,
                                                    ((x + lastx) * Render.PixelSize) + Render.PixelSizeZ,
                                                    ((y + lasty) * Render.PixelSize) + Render.PixelSizeZ,
                                                      Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                      Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
          end;
        end;
      end;
    end;
  end;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.PaintBoxUpdateDeadPixel(Sender: TObject);
var
  x, y : integer;

begin
  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      if MatrixDead.Grid[Render.TopLeft.X + x, Render.TopLeft.Y + y] = ptNormal then
        FPaintBox.Canvas.Brush.Color := $000000
      else
        FPaintBox.Canvas.Brush.Color := $FFFFFF;

      case Render.PixelShape of
        psSquare    : FPaintBox.Canvas.FillRect(Rect(x * Render.PixelSize,
                                                     y * Render.PixelSize,
                                                    (x * Render.PixelSize) + Render.PixelSizeZ,
                                                    (y * Render.PixelSize) + Render.PixelSizeZ));
        psCircle    : FPaintBox.Canvas.Ellipse(x * Render.PixelSize,
                                               y * Render.PixelSize,
                                              (x * Render.PixelSize) + Render.PixelSizeZ,
                                              (y * Render.PixelSize) + Render.PixelSizeZ);
        psRoundRect : FPaintBox.Canvas.RoundRect(x * Render.PixelSize,
                                                 y * Render.PixelSize,
                                                (x * Render.PixelSize) + Render.PixelSizeZ,
                                                (y * Render.PixelSize) + Render.PixelSizeZ,
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)),
                                                 Render.PixelSize - (Round(Render.PixelSize / CRoundRectCoeff)));
      end;
    end;
  end;
end;


// =============================================================================
// =============================================================================


procedure TTheMatrix.SetDeadPixels(aDeadness : TPixelType);
var
  x, y : integer;

begin
  for x := 0 to _MaxWidth - 1 do begin
    for y := 0 to _MaxHeight - 1 do begin
      MatrixDead.Grid[x, y] := aDeadness;
    end;
  end;
end;


procedure TTheMatrix.SetDeadPixelsFromCustomShape(aShape : TCustomShape; aParameter : integer);
begin
  MatrixDead.SetFromCustomShape(Matrix.Width, Matrix.Height, aShape, aParameter);

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.SetDeadPixelsFromFileName(aFileName : string);
begin
  MatrixDead.Load(aFileName);

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.SaveDeadPixels(aFileName : string);
begin
  MatrixDead.Save(aFileName, Matrix.Width, Matrix.Height);
end;


// =============================================================================
// =============================================================================


procedure TTheMatrix.ClearAllFrames;
begin
  FDisplayBuffer.Clear(Matrix.Mode, FRGBBackground);

  while MatrixLayers.Count > 1 do
    MatrixLayers.Delete(1);

  MatrixLayers[CPermanentLayer].Frames.Clear;
  MatrixLayers[CPermanentLayer].Name := GLanguageHandler.Text[kBottomLayer];

  InitMatrix;

  FCurrentFrame := 1;
  FCurrentLayer := 0;

  MatrixLayers[CPermanentLayer].Frames[1].History.Clear;
  MatrixLayers[CPermanentLayer].Frames[1].AddToHistory;

  MatrixLayers[CPermanentLayer].Frames[1].Locked := False;

  MatrixChange;

  MatrixLayerChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.WipeAllFramesCurrentLayer;
var
  lFrame : integer;

begin
  for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin

    if not(IsThisFrameLocked(FCurrentLayer, lFrame)) then begin

      MatrixLayers[FCurrentLayer].Frames[lFrame].Clear(Matrix.Mode, FRGBBackground);

    end;
  end;

  MatrixChange;

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.WipeAllFramesAllLayers;
var
  lFrame, lLayer : integer;

begin
  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin

      if not(IsThisFrameLocked(lLayer, lFrame)) then begin

        MatrixLayers[lLayer].Frames[lFrame].Clear(Matrix.Mode, FRGBBackground);

      end;
    end;
  end;

  MatrixChange;

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ClearAllFramesGradient(aMode : integer);
var
  x, y, lFrame : integer;

begin
  for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin

    if not(IsThisFrameLocked(FCurrentLayer, lFrame)) then begin

      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if aMode = 1 then begin
            if (Matrix.Mode = mtRGB) or (Matrix.Mode = mtRGB3BPP) then
              MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[x, y] := Render.GradientIY[y]
            else
              MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[x, y] := LEDColours[Render.GradientIY[y]];
          end
           else begin
             if (Matrix.Mode = mtRGB) or (Matrix.Mode = mtRGB3BPP) then
               MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[x, y] := Render.GradientIX[x]
            else
               MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[x, y] := LEDColours[Render.GradientIX[x]];
          end;
        end;
      end;

    end;
  end;

  MatrixChange;

  FPaintBox.Invalidate;
end;


// =============================================================================
// =============================================================================


procedure TTheMatrix.ClearFont;
var
  i, x, y : integer;

begin
  for i := 0 to 95 do begin
    for x := 0 to 7 do begin
      for y:= 0 to 7 do begin
        FontMatrix[i, x, y] := -1;
      end;
    end;

    FontMatrixStart[i] := 0;
    FontMatrixEnd[i]   := 0;
  end;
end;


procedure TTheMatrix.SetMouseButtonColours(aLMB, aMMB, aRMB : integer);
begin
  SelectionLMB := aLMB;
  SelectionMMB := aMMB;
  SelectionRMB := aRMB;
end;


procedure TTheMatrix.ChangePixels(aFrom, aTo : integer);
var
  lFrame : integer;

begin
  for lFrame := 0 to FrameCount do begin
    MatrixLayers[FCurrentLayer].Frames[lFrame].ChangePixels(aFrom, aTo);
  end;

  MatrixChange;

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


function TTheMatrix.HexToInt(const s : string): integer;
var
  i : integer;
  digit : integer;

begin
  Result := 0;

  for i:=1 to length(s) do begin
    case Ord(s[i]) of
      48..57 : digit := StrToInt(s[i]);
      65..70 : digit := Ord(s[i]) - 55;
    else
      MessageDlg(GLanguageHandler.Text[kError] + ': ' + s[i], mtError, [mbOK], 0);
      digit := 0;
    end;

    Result := Result + (digit * powers16[length(s) - i]);
  end;
end;


function TTheMatrix.BrightenRGB(aRGB : integer): integer;
var
  xR : integer;
  xG : integer;
  xB : integer;

begin
  xR := (aRGB and $0000ff);         // Windows colour structure = BGR
  xB := (aRGB and $ff0000) shr 16;
  xG := (aRGB and $00ff00) shr 8;

  xR := Round(xR * 0.8);
  xG := Round(xG * 0.8);
  xB := Round(xB * 0.8);

{  if xR > 255 then
    xR := 255;
  if xG > 255 then
    xG := 255;
  if xB > 255 then
    xB := 255;}

  Result := (xB shl 16) + (xG shl 8) + xR;
end;


function TTheMatrix.RandomColour(aRGB : integer): integer;
var
  xR, xG, xB : integer;

begin
  xR := (aRGB and $0000ff);         // Windows colour structure = BGR
  xB := (aRGB and $ff0000) shr 16;
  xG := (aRGB and $00ff00) shr 8;

  xR := (xR - RandomCoeff) + (random(2 * RandomCoeff));
  xG := (xG - RandomCoeff) + (random(2 * RandomCoeff));
  xB := (xB - RandomCoeff) + (random(2 * RandomCoeff));

  if xR > 255 then xR := 255;
  if xG > 255 then xG := 255;
  if xB > 255 then xB := 255;

  if xR < 0 then xR := 0;
  if xG < 0 then xG := 0;
  if xB < 0 then xB := 0;

  Result := (xB shl 16) + (xG shl 8) + xR;
end;


procedure TTheMatrix.NewMatrix(aMatrixMode : TMatrixMode; aFrameCount, aTop, aLeft, aWidth, aHeight, aPixelSize : integer;
                               aPixelShape : TPixelShape; aGrid, aReadonly, aClearAll : boolean;
                               aBackgroundColour : integer);
begin
  FCurrentFrame        := 1;
  FLightBox            := 0;
  FDeadPixelsMode      := False;
  FFrameCount          := aFramecount;

  AnimPlaying          := False;

  Render.TopLeft.X      := 0;
  Render.TopLeft.Y      := 0;
  Render.BottomRight.X  := aWidth - 1;
  Render.BottomRight.Y  := aHeight - 1;
  Render.ViewWindow.X   := aWidth - 1;
  Render.ViewWindow.Y   := aHeight - 1;

  Render.DrawData.Mode        := dmNone;
  Render.DrawData.Point       := CDrawPointNone;
  Render.DrawData.Colour      := 0;
  Render.DrawData.Coords[0].X := -1;
  Render.DrawData.Coords[0].Y := -1;
  Render.DrawData.CopyPos.X   := 0;
  Render.DrawData.CopyPos.Y   := 0;

  LastX                := -1;
  LastY                := -1;

  FPaintBox.Top        := aTop;
  FPaintBox.Left       := aLeft;
  FPaintBox.Width      := aWidth * aPixelSize;
  FPaintBox.Height     := aHeight * aPixelSize;

  PreviewBox.Top       := aTop;

  Matrix.Width         := aWidth;
  Matrix.Height        := aHeight;
  Matrix.Mode          := aMatrixMode;
  Render.Gradient      := goOff;
  Render.PixelShape    := aPixelShape;
  Render.PixelSize     := aPixelSize;
  FMatrixReadOnly      := aReadOnly;
  FRGBBackground       := aBackgroundColour;

  Matrix.Grid          := aGrid;

  if aGrid then
    Render.PixelSizeZ := Render.PixelSize - 1
  else
    Render.PixelSizeZ := Render.PixelSize;

  // ===========================================================================

  if (aClearAll) then
    SetDeadPixels(ptNormal);

  // ===========================================================================

  ClearGradients;

  // ===========================================================================

  ConfigurePaintboxDrawing;

  // ===========================================================================

  if aClearAll then begin
    ClearAllFrames;

    Matrix.Comment := '';
  end;

  if aFrameCount <> 1 then begin
    while MatrixLayers[CPermanentLayer].Frames.Count <= aFrameCount do
      InsertBlankFrameAt(0);
  end;

  Matrix.Available := True;

  SetPreviewBoxSize(FPreviewOptions.Size);

  MatrixChange;
end;


procedure TTheMatrix.SetYPos(aNewYPos : integer);
begin
  FPaintBox.Top  := aNewYPos;
  PreviewBox.Top := aNewYPos;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.SetBackgroundColour(aNewColour : integer);
begin
  FCanvasBackground := aNewColour;

  FPaintBox.Canvas.Brush.Color := aNewColour;
  FPaintBox.Canvas.FillRect(Rect(0, 0, FPaintBox.Width, FPaintBox.Height));

  PreviewBox.Canvas.Brush.Color := aNewColour;
  PreviewBox.Canvas.FillRect(Rect(0, 0, FPaintBox.Width, FPaintBox.Height));

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ChangePixelSize(aNewPixelSize : integer);
begin
  Render.PixelSize := aNewPixelSize;

  FPaintBox.Width  := Matrix.Width * Render.PixelSize;
  FPaintBox.Height := Matrix.Height * Render.PixelSize;

  if Matrix.Grid then
    Render.PixelSizeZ := Render.PixelSize - 1
  else
    Render.PixelSizeZ := Render.PixelSize;

  if PreviewActive then begin
    if (FPreviewPopout) then
      PreviewBox.Left := 0
    else
      PreviewBox.Left := CLeftOffset + (Render.PixelSize * (Matrix.Width)) + 20;
  end;

  // ===========================================================================

  ChangeZoomUI(aNewPixelSize);

  // ===========================================================================

  FPaintBox.Invalidate;
end;


// configures the render engine for scrolling the view window
procedure TTheMatrix.ChangeZoomUI(aNewPixelSize : integer);
var
  lContainerWidth, lContainerHeight : integer;

begin
  lContainerWidth  := TPanel(FCanvas).Width - 95 - 25;
  lContainerHeight := TPanel(FCanvas).Height - 40;

  if (aNewPixelSize * Matrix.Width > lContainerWidth) then begin
    if not(FScrollHorizontal.Visible) then
      FScrollHorizontal.Visible := True;

    Render.ViewWindow.X        := Floor(lContainerWidth / aNewPixelSize);

    Render.TopLeft.X           := 0;
    Render.BottomRight.X       := Render.TopLeft.X + Render.ViewWindow.X - 1;

    FScrollHorizontal.Max      := Matrix.Width - Render.ViewWindow.X - 1;
    FScrollHorizontal.Position := 0;
  end
  else begin
    if (FScrollHorizontal.Visible) then
      FScrollHorizontal.Visible := False;
  end;


  if (aNewPixelSize * Matrix.Height > lContainerHeight) then begin
    if not(FScrollVertical.Visible) then
      FScrollVertical.Visible := True;

    Render.ViewWindow.Y      := Floor(lContainerHeight / aNewPixelSize);

    Render.TopLeft.Y         := 0;
    Render.BottomRight.Y     := Render.TopLeft.Y + Render.ViewWindow.Y - 1;

    FScrollVertical.Max      := Matrix.Height - Render.ViewWindow.Y - 1;
    FScrollVertical.Position := 0;
  end
  else begin
    if (FScrollVertical.Visible) then
      FScrollVertical.Visible := False;
  end;
end;


procedure TTheMatrix.ChangePixelShape(aNewPixelShape : TPixelShape);
begin
  Render.PixelShape := aNewPixelShape;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ChangePixelBrush(aNewBrushSize : TBrushSize);
begin
  Render.BrushSize := aNewBrushSize;
end;


procedure TTheMatrix.ChangeMatrixMode(aNewMatrixMode : TMatrixMode);       // to do
var
  lFrame, lLayer, x, y : integer;

begin
  if Matrix.Width <> -1 then begin

    Matrix.Mode := aNewMatrixMode;

    ConfigurePaintboxDrawing;
  end;

  // if we're moving to single colour matrix
  // make sure the matrix data fits!

  if aNewMatrixMode = mtMono then begin
    for lLayer := 0 to MatrixLayers.Count - 1 do begin
      for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin

        for x := 0 to Matrix.Width - 1 do begin
          for y := 0 to Matrix.Height - 1 do begin
            if MatrixLayers[lLayer].Frames[lFrame].Grid[x, y] > 0 then // TO DO
              MatrixLayers[lLayer].Frames[lFrame].Grid[x, y] := 1;
          end;
        end;
      end;
    end;
  end;

  // ===========================================================================

  ClearGradients;

  // ===========================================================================

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ChangeSoftwareMode(aSoftwareMode : TSoftwareMode);   // to do
var
  t : integer;
  lMatrix : TMatrix;

begin
  case aSoftwareMode of
    smAnimation : begin
                    ClearAllFrames;
                  end;
    smFont      : begin
                    ClearAllFrames;

                    for t := 1 to 95 do begin
                      lMatrix := TMatrix.Create(_MaxWidth, _MaxHeight, Matrix.Mode, FRGBBackground); // TO DO

                      MatrixLayers[CPermanentLayer].Frames.Add(lMatrix);
                    end;
                  end;
  end;

  FSoftwareMode := aSoftwareMode;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.SetRadialOffset(aRadialOffset : integer);
begin
  FRadialOffset := aRadialOffset;

  if FRadialOffsetDirection then
    FRadialOffsetDegrees := -FRadialOffset
  else
    FRadialOffsetDegrees := FRadialOffset;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.SetRadialOffsetDirection(aRadialOffsetDirection : boolean);
begin
  FRadialOffsetDirection := aRadialOffsetDirection;

  if FRadialOffsetDirection then
    FRadialOffsetDegrees := -FRadialOffset
  else
    FRadialOffsetDegrees := FRadialOffset;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.SetShapeParameter(aParameter : integer);
begin
  Render.DrawData.Parameter := aParameter;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.SetMirrorMode(aNewMode : TMirrorMode);
begin
  FMirrorMode := aNewMode;
end;


procedure TTheMatrix.DrawWithBrush(aIndex, x, y : integer);
var
  a, b : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
     not(MatrixLayers[FCurrentLayer].Visible) then
    Exit;

  case Render.BrushSize of
    bsSmall  : begin
                 PlotPixelMatrix(x, y, aIndex);

                 MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
               end;
    bsMedium : begin
                 PlotPixelMatrix(x,     y,     aIndex);
                 PlotPixelMatrix(x + 1, y,     aIndex);
                 PlotPixelMatrix(x,     y + 1, aIndex);
                 PlotPixelMatrix(x + 1, y + 1, aIndex);

                 MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
               end;
    bsLarge  : begin
                 for a := 0 to 2 do begin
                   for b := 0 to 2 do begin
                     PlotPixelMatrix(x + a, y + b, aIndex);
                   end;
                 end;

                 MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
               end;
  end;
end;

// draws identical pixels on every frame
procedure TTheMatrix.DrawWithBrushMulti(aIndex, x, y : integer);
var
  a, b, lFrame : integer;

begin
  for lFrame := 1 to Render.DrawData.Special do begin
    if not(IsThisFrameLocked(FCurrentLayer, lFrame)) and
      (MatrixLayers[FCurrentLayer].Visible) then begin

      case Render.BrushSize of
        bsSmall  : begin
                     PlotPixelMatrixFrame(lFrame, x, y, aIndex);

                     MatrixLayers[FCurrentLayer].Frames[lFrame].AddToHistory(FDisplayBuffer);
                   end;
        bsMedium : begin
                     PlotPixelMatrixFrame(lFrame, x, y,         aIndex);
                     PlotPixelMatrixFrame(lFrame, x + 1, y,     aIndex);
                     PlotPixelMatrixFrame(lFrame, x, y + 1,     aIndex);
                     PlotPixelMatrixFrame(lFrame, x + 1, y + 1, aIndex);

                     MatrixLayers[FCurrentLayer].Frames[lFrame].AddToHistory(FDisplayBuffer);
                   end;
        bsLarge  : begin
                     for a := 0 to 2 do begin
                       for b := 0 to 2 do begin
                         PlotPixelMatrixFrame(lFrame, x + a, y + b, aIndex);
                       end;
                     end;

                     MatrixLayers[FCurrentLayer].Frames[lFrame].AddToHistory(FDisplayBuffer);
                   end;
      end;
    end;
  end;
end;


procedure TTheMatrix.DrawWithGradientBrush(x, y : integer);
begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) or
    (FGradient.Count = 0) then
      Exit;

  PlotPixelMatrixFrame(FCurrentFrame, x, y, FGradient[Render.DrawData.Parameter]);

  if (Render.DrawData.Parameter = FGradient.Count - 1) then
    Render.DrawData.Parameter := 0
  else
    inc(Render.DrawData.Parameter);

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
end;


procedure TTheMatrix.DrawWithBrushPaste(x1, y1 : integer; aTransparent : boolean);
var
  x2, y2 : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  case Matrix.Mode of
    mtRGB,
    mtRGB3BPP : for x2 := 0 to Render.DrawData.CopyPos.X do begin
                  for y2 := 0 to Render.DrawData.CopyPos.Y do begin
                    if (x2 + x1 >= 0) and (x2 + x1 <= Matrix.Width - 1) and
                      (y2 + y1 >= 0) and (y2 + y1 <= Matrix.Height - 1) then begin

                      if MatrixCopy.Grid[x2, y2] <> FRGBBackground then begin
                        PlotPixelMatrix(x2 + x1, y2 + y1, MatrixCopy.Grid[x2, y2]);
                      end
                      else
                        if aTransparent then
                          PlotPixelMatrix(x2 + x1, y2 + y1, FRGBBackground);
                    end;
                  end;
                end;
  else
    for x2 := 0 to Render.DrawData.CopyPos.X do begin
      for y2 := 0 to Render.DrawData.CopyPos.Y do begin
        if (x2 + x1 >= 0) and (x2 + x1 <= Matrix.Width - 1) and
           (y2 + y1 >= 0) and (y2 + y1 <= Matrix.Height - 1) then begin

          if MatrixCopy.Grid[x2, y2] = 1 then begin
            PlotPixelMatrix(x2 + x1, y2 + y1, 1);
          end
          else
            if aTransparent then
              PlotPixelMatrix(x2 + x1, y2 + y1, 0);
        end;
      end;
    end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
end;


procedure TTheMatrix.DrawWithBrushPasteEveryFrame(x1, y1 : integer; aTransparent : boolean);
var
  x2, y2, lFrame : integer;

begin
  case Matrix.Mode of
    mtRGB,
    mtRGB3BPP : for x2 := 0 to Render.DrawData.CopyPos.X do begin
                  for y2 := 0 to Render.DrawData.CopyPos.Y do begin
                    if (x2 + x1 >= 0) and (x2 + x1 <= Matrix.Width - 1) and
                      (y2 + y1 >= 0) and (y2 + y1 <= Matrix.Height - 1) then begin

                      for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
                        if not(IsThisFrameLocked(FCurrentLayer, lFrame)) then begin

                          if MatrixCopy.Grid[x2, y2] <> FRGBBackground then begin
                            PlotPixelMatrixFrame(lFrame, x2 + x1, y2 + y1, MatrixCopy.Grid[x2, y2]);
                          end
                          else
                            if not(aTransparent) then
                              PlotPixelMatrixFrame(lFrame, x2 + x1, y2 + y1, FRGBBackground);
                        end;
                      end;
                    end;
                  end;
                end;
  else
    for x2 := 0 to Render.DrawData.CopyPos.X do begin
      for y2 := 0 to Render.DrawData.CopyPos.Y do begin
        if (x2 + x1 >= 0) and (x2 + x1 <= Matrix.Width - 1) and
           (y2 + y1 >= 0) and (y2 + y1 <= Matrix.Height - 1) then begin


          for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
            if not(IsThisFrameLocked(FCurrentLayer, lFrame)) then begin

              if MatrixCopy.Grid[x2, y2] = 1 then begin
                PlotPixelMatrixFrame(lFrame, x2 + x1, y2 + y1, 1);
              end
              else
                if not(aTransparent) then
                  PlotPixelMatrixFrame(lFrame, x2 + x1, y2 + y1, 0);
            end;

          end;
        end;
      end;
    end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);
end;


procedure TTheMatrix.ClickPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if ssleft in shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone  : begin
                  DrawWithBrush(1, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(1, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmPaste : begin
                  DrawWithBrushPaste(x1, y1, not(ssShift in shift));

                  MatrixChange;
                end;
    else
      UpdateDrawTool(x1, y1, 1, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone  : begin
                  DrawWithBrush(0, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(0, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    else
      UpdateDrawTool(x1, y1, 0, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end;

  PreviewBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if Assigned(FOnMouseOver) then
    FOnMouseOver(x1, y1);

  // ===========================================================================
  // ===========================================================================

  if ssLeft in Shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone  : begin
                  if not((LastX = x1) and (LastY = y1)) then begin
                    DrawWithBrush(1, x1, y1);
                  end;

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  if not((LastX = x1) and (LastY = y1)) then begin
                     DrawWithBrushMulti(1, x1, y1);
                  end;

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    end;
  end
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone  : begin
                  if not((LastX = x1) and (LastY = y1)) then begin
                    DrawWithBrush(0, x1, y1);
                  end;

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  if not((LastX = x1) and (LastY = y1)) then begin
                    DrawWithBrushMulti(0, x1, y1);
                  end;

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    end;
  end;

  LastX := x1;
  LastY := y1;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Render.DrawData.Mode = dmNone) then
    CopyDrawBufferToCurrentFrame;
end;


procedure TTheMatrix.Shape1MouseUpBiColour(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case (Render.DrawData.Mode) of
    dmNone,
    dmGradientBrush,
    dmMulti,
    dmRandom         : CopyDrawBufferToCurrentFrame;
  end;
end;


procedure TTheMatrix.Shape1MouseUpRGB(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 begin
  case (Render.DrawData.Mode) of
    dmNone,
    dmGradientBrush,
    dmMulti,
    dmRandom         : CopyDrawBufferToCurrentFrame;
  end;
end;


procedure TTheMatrix.ClickPixelBiColour(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1, y1, i : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;


  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if ssleft in shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone   : begin
                   DrawWithBrush(SelectionLMB, x1, y1);

                   LastX := x1;
                   LastY := y1;

                   MatrixChange;
                 end;
      dmMulti  : begin
                   DrawWithBrushMulti(SelectionLMB, x1, y1);

                   LastX := x1;
                   LastY := y1;

                   MatrixChange;
                 end;
      dmPaste  : begin
                   DrawWithBrushPaste(x1, y1, not(ssShift in shift));

                   MatrixChange;
                 end;
      dmRandom : begin
                   i := 1 + random(3);

                   DrawWithBrush(i, x1, y1);

                   MatrixChange;
                 end
    else
      UpdateDrawTool(x1, y1, SelectionLMB, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end
  else if ssMiddle in Shift then begin
    FLastMouseButton := CMouseMiddle;

    case Render.DrawData.Mode of
      dmNone  : begin
                  DrawWithBrush(SelectionMMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(SelectionMMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    else
      UpdateDrawTool(x1, y1, SelectionMMB, True);
    end;

    CopyDrawBufferToCurrentFrame;
  end
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone  : begin
                   DrawWithBrush(SelectionRMB, x1, y1);

                   LastX := x1;
                   LastY := y1;

                   MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(SelectionRMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    else
      UpdateDrawTool(x1, y1, SelectionRMB, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseMoveBiColour(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if Assigned(FOnMouseOver) then
    FOnMouseOver(x1, y1);

  // ===========================================================================
  // ===========================================================================

  if ssLeft in Shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone  : begin
                  DrawWithBrush(SelectionLMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(SelectionLMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    end;
  end
  else if ssMiddle in Shift then begin
    FLastMouseButton := CMouseMiddle;

    case Render.DrawData.Mode of
      dmNone  : begin
                  if (Render.Gradient = goVertical) and (Render.GradientIY[y1] <> 0) and (SelectionMMB <> 0) then begin
                    DrawWithBrush(Render.GradientIY[y1], x1, y1);
                  end
                  else if (Render.Gradient = goHorizontal) and (Render.GradientIX[x1] <> 0) and (SelectionMMB <> 0) then begin
                    DrawWithBrush(Render.GradientIX[x1], x1, y1);
                  end
                  else begin
                    DrawWithBrush(SelectionMMB, x1, y1);
                  end;

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(SelectionMMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    end;
  end
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone  : begin
                  DrawWithBrush(SelectionRMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
      dmMulti : begin
                  DrawWithBrushMulti(SelectionRMB, x1, y1);

                  LastX := x1;
                  LastY := y1;

                  MatrixChange;
                end;
    end;
  end;

  LastX := x1;
  LastY := y1;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ClickPixelRGB(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if ssleft in shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone,
      dmGradientBrush : begin
                          DrawWithBrush(SelectionLMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom        : begin
                          DrawWithBrush(RandomColour(SelectionLMB), x1, y1);

                          MatrixChange;
                        end;
      dmMulti         : begin
                          DrawWithBrushMulti(SelectionLMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmPicker        : ChangeSelectionColour(MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x1, y1], SelectionMMB, SelectionRMB);
      dmPaste         : begin
                          DrawWithBrushPaste(x1, y1, not(ssShift in shift));

                          MatrixChange;
                        end;
    else
      UpdateDrawTool(x1, y1, SelectionLMB, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end
  else if ssMiddle in Shift then begin
    FLastMouseButton := CMouseMiddle;

    case Render.DrawData.Mode of
      dmNone          : begin
                          case Render.Gradient of
                            goOff        : DrawWithBrush(SelectionMMB, x1, y1);
                            goVertical   : DrawWithBrush(Render.GradientIY[y1], x1, y1);
                            goHorizontal : DrawWithBrush(Render.GradientIX[x1], x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom        : begin
                          DrawWithBrush(RandomColour(SelectionMMB), x1, y1);
                        end;
      dmMulti         : begin
                          DrawWithBrushMulti(SelectionMMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmPicker        : ChangeSelectionColour(SelectionLMB, MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x1, y1], SelectionRMB);
      dmGradientBrush : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithGradientBrush(x1, y1);

                            MatrixChange;

                            LastX := x1;
                            LastY := y1;
                          end;
                        end;

    else
      UpdateDrawTool(x1, y1, SelectionMMB, True);
    end;

    CopyDrawBufferToCurrentFrame;
  end  
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone,
      dmGradientBrush : begin
                          DrawWithBrush(SelectionRMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom        : begin
                          DrawWithBrush(RandomColour(SelectionRMB), x1, y1);
                        end;
      dmMulti         : begin
                          DrawWithBrushMulti(SelectionRMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmPicker        : ChangeSelectionColour(SelectionLMB, SelectionMMB, MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x1, y1]);
    else
      UpdateDrawTool(x1, y1, SelectionRMB, False);
    end;

    CopyDrawBufferToCurrentFrame;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseMoveRGB(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if Assigned(FOnMouseOver) then
    FOnMouseOver(x1, y1);

  // ===========================================================================
  // ===========================================================================

  if ssLeft in Shift then begin
    FLastMouseButton := CMouseLeft;

    case Render.DrawData.Mode of
      dmNone,
      dmGradientBrush : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithBrush(SelectionLMB, x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmMulti         : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithBrushMulti(SelectionLMB, x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom        : begin
                          DrawWithBrush(RandomColour(SelectionLMB), x1, y1);
                        end
    end;
  end
  else if ssMiddle in Shift then begin
    FLastMouseButton := CMouseMiddle;

    case Render.DrawData.Mode of
      dmNone          : begin
                          case Render.Gradient of
                            goOff        : DrawWithBrush(SelectionMMB, x1, y1);
                            goVertical   : DrawWithBrush(Render.GradientIY[y1], x1, y1);
                            goHorizontal : DrawWithBrush(Render.GradientIX[x1], x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmMulti         : begin
                          DrawWithBrushMulti(SelectionMMB, x1, y1);

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom        : begin
                          DrawWithBrush(RandomColour(SelectionMMB), x1, y1);
                        end;
      dmGradientBrush : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithGradientBrush(x1, y1);

                            MatrixChange;

                            LastX := x1;
                            LastY := y1;
                          end;
                        end;
    end;
  end
  else if ssRight in Shift then begin
    FLastMouseButton := CMouseRight;

    case Render.DrawData.Mode of
      dmNone,
      dmGradientBrush : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithBrush(SelectionRMB, x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmMulti         : begin
                          if not((LastX = x1) and (LastY = y1)) then begin
                            DrawWithBrushMulti(SelectionRMB, x1, y1);
                          end;

                          LastX := x1;
                          LastY := y1;

                          MatrixChange;
                        end;
      dmRandom :        begin
                          DrawWithBrush(RandomColour(SelectionRMB), x1, y1);
                        end;
    end;
  end;

  LastX := x1;
  LastY := y1;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ClickPixelDeadPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);    // to do
var
  x1, y1 : integer;

begin
  if (IsThisFrameLocked(0, FCurrentFrame)) then
    Exit;

  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if ssleft in shift then begin
    if (MatrixDead.Grid[x1, y1] = ptNormal) then
      MatrixDead.Grid[x1, y1] := ptDead
    else
      MatrixDead.Grid[x1, y1] := ptNormal;

    LastX := x1;
    LastY := y1;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseMoveDeadPixel(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  x1, y1 : integer;

begin
  x1 := Floor(x / Render.PixelSize);
  y1 := Floor(y / Render.PixelSize);

  if (x1 < 0) or (y1 < 0) then Exit;

  x1 := Floor(x / Render.PixelSize) + Render.TopLeft.X;
  y1 := Floor(y / Render.PixelSize) + Render.TopLeft.Y;

  // ===========================================================================

  if Assigned(FOnMouseOver) then
    FOnMouseOver(x1, y1);

  // ===========================================================================
  // ===========================================================================

  if ssLeft in Shift then begin
    if not((LastX = x1) and (LastY = y1)) then begin
      if (MatrixDead.Grid[x1, y1] = ptNormal) then
        MatrixDead.Grid[x1, y1] := ptDead
      else
        MatrixDead.Grid[x1, y1] := ptNormal;
    end;
  end;

  LastX := x1;
  LastY := y1;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Shape1MouseUpDeadPixel(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 begin
  {}
end;


// =============================================================================
// =============================================================================
// =============================================================================


procedure TTheMatrix.BackupMatrix(aLayer, aFrame : integer);
var
  x, y : integer;

begin
  if (aFrame > 0) then begin
    for y := 0 to Matrix.Height - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        MatrixBackup.Grid[x, y] := MatrixLayers[aLayer].Frames[aFrame].Grid[x, y];
      end;
    end;
  end
  else begin
    for y := 0 to Matrix.Height - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        MatrixBackup.Grid[x, y] := MatrixCopy.Grid[x, y];
      end;
    end;
  end;
end;


procedure TTheMatrix.ClearGradients;
var
  t : integer;

  begin
  for t := 0 to Matrix.Width do begin
    if Matrix.Mode = mtRGB then
      Render.GradientIX[t] := FRGBBackground
    else
      Render.GradientIX[t] := 0;
  end;

  for t := 0 to Matrix.Height do begin
    if Matrix.Mode = mtRGB then
      Render.GradientIY[t] := FRGBBackground
    else
      Render.GradientIY[t] := 0;
  end;
end;


procedure TTheMatrix.ClearCurrentFrame;
var
  l : integer;

begin
  for l := 0 to MatrixLayers.Count - 1 do begin
    if not(IsThisFrameLocked(l, FCurrentFrame)) then begin
      if l = FCurrentLayer then
        FDisplayBuffer.Clear(Matrix.Mode, FRGBBackground);

      MatrixLayers[l].Frames[FCurrentFrame].Clear(Matrix.Mode, FRGBBackground);

      MatrixLayers[l].Frames[FCurrentFrame].AddToHistory;
    end;
  end;

  FPaintBox.Invalidate;

  MatrixChange;
end;


procedure TTheMatrix.ClearCurrentLayer;
begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) then
    Exit;

  FDisplayBuffer.Clear(Matrix.Mode, FRGBBackground);

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Clear(Matrix.Mode, FRGBBackground);

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  FPaintBox.Invalidate;

  MatrixChange;
end;


procedure TTheMatrix.ClearFrame(aFrame : integer);
var
  l : integer;

begin
  for l := 0 to MatrixLayers.Count - 1 do begin
    if not(IsThisFrameLocked(l, aFrame)) then begin
      if (l = FCurrentLayer) and (aFrame = FCurrentFrame) then
        FDisplayBuffer.Clear(Matrix.Mode, FRGBBackground);

      MatrixLayers[l].Frames[aFrame].Clear(Matrix.Mode, FRGBBackground);

      MatrixLayers[l].Frames[aFrame].AddToHistory;
    end;
  end;

  FPaintBox.Invalidate;

  MatrixChange;
end;


procedure TTheMatrix.PerformEffectController(aMode, aMultipleOptionMode : integer);
var
  lLayer, lFrame : integer;

begin
  case aMultipleOptionMode of
    CMOMCurrentOnly        : PerformEffect(aMode, FCurrentLayer, FCurrentFrame);
    CMOMCurrentFrameLayers : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               PerformEffect(aMode, lLayer, FCurrentFrame);
                             end;
    CMOMCurrentLayerFrames : for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
                               PerformEffect(aMode, FCurrentLayer, lFrame);
                             end;
    CMOMAll                : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
                                 PerformEffect(aMode, lLayer, lFrame);
                               end;
                             end;
  end;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformEffect(aMode, aLayer, aFrame : integer);
var
  x, y : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  BackupMatrix(aLayer, aFrame);

  case aMode of
    modeFlip           : begin
                           for x := 0 to Matrix.Width - 1 do begin
                             for y := 0 to Matrix.Height - 1 do begin
                               MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[Matrix.Width - x - 1, y];
                             end;
                           end;
                         end;
    modeMirror         : begin
                           for y := 0 to Matrix.Height - 1 do begin
                             for x := 0 to Matrix.Width - 1 do begin
                               MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[x, Matrix.Height - y - 1];
                             end;
                           end;
                         end;
    modeInvert         : begin
                         for x := 0 to Matrix.Width - 1 do begin
                            for y := 0 to Matrix.Height - 1 do begin
                             case Matrix.Mode of
                               mtMono         : MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := 1 - MatrixLayers[aLayer].Frames[aFrame].Grid[x, y];
                               mtBiSequential,
                               mtBiBitPlanes  : MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := 3 - MatrixLayers[aLayer].Frames[aFrame].Grid[x, y];
                               mtRGB          : MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := $FFFFFF - MatrixLayers[aLayer].Frames[aFrame].Grid[x, y];
                               mtRGB3BPP      : MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := $4 - MatrixLayers[aLayer].Frames[aFrame].Grid[x, y];
                             end;
                           end;
                         end;
                       end;
    modeGradientAll  : begin
                         for x := 0 to Matrix.Width - 1 do begin
                           for y := 0 to Matrix.Height - 1 do begin
                             if (Render.Gradient = goVertical) and (Render.GradientIY[y] <> 0) then begin
                               if MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] <> 0 then
                                 MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := Render.GradientIY[y];
                             end;
                           end;
                         end;
                       end;
  end;

  MatrixLayers[aLayer].Frames[aFrame].AddToHistory;
end;


procedure TTheMatrix.PerformScrollController(aMode, aMultipleOptionMode : integer);
var
  lLayer, lFrame : integer;

begin
  case aMultipleOptionMode of
    CMOMCurrentOnly        : PerformScroll(aMode, FCurrentLayer, FCurrentFrame);
    CMOMCurrentFrameLayers : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               PerformScroll(aMode, lLayer, FCurrentFrame);
                             end;
    CMOMCurrentLayerFrames : for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
                               PerformScroll(aMode, FCurrentLayer, lFrame);
                             end;
    CMOMAll                : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
                                 PerformScroll(aMode, lLayer, lFrame);
                               end;
                             end;
  end;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformScroll(aMode, aLayer, aFrame : Integer);
var
  x, y : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  BackupMatrix(aLayer, aFrame);

  case aMode of
     modeScrollLeft          : begin
                                 for x := 0 to Matrix.Width - 2 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[x + 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   MatrixLayers[aLayer].Frames[aFrame].Grid[Matrix.Width - 1, y] := MatrixBackup.Grid[0, y];
                                 end;
                               end;
     modeScrollRight         : begin
                                 for x := 1 to Matrix.Width - 1 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[x - 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   MatrixLayers[aLayer].Frames[aFrame].Grid[0, y] := MatrixBackup.Grid[Matrix.Width - 1, y];
                                 end;
                               end;
     modeScrollUp            : begin
                                 for y := 0 to Matrix.Height - 2 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[x, y + 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   MatrixLayers[aLayer].Frames[aFrame].Grid[x, Matrix.Height - 1] := MatrixBackup.Grid[x, 0];
                                 end;
                               end;
     modeScrollDown          : begin
                                 for y := 1 to Matrix.Height - 1 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[x, y - 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   MatrixLayers[aLayer].Frames[aFrame].Grid[x, 0] := MatrixBackup.Grid[x, Matrix.Height - 1];
                                 end;
                               end;
  end;

  MatrixLayers[aLayer].Frames[aFrame].AddToHistory;
end;


procedure TTheMatrix.PerformSplitScroll(aMode, aLayer, aFrame : integer);
var
  lMid, lThing, a, b : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  case aMode of
    modeSplitScrollLeftRight,
    modeSplitScrollRightLeft  : begin
                                  lMid := Round(Matrix.Height / 2) - 1;

                                  case aMode of
                                    modeSplitScrollLeftRight : begin
                                                                 a := modeScrollRowLeft;
                                                                 b := modeScrollRowRight;
                                                               end;
                                    modeSplitScrollRightLeft : begin
                                                                 a := modeScrollRowRight;
                                                                 b := modeScrollRowLeft;
                                                               end;
                                  else
                                    a := modeScrollRowLeft;
                                    b := modeScrollRowRight;
                                  end;

                                 for lThing := 0 to lMid do
                                   ScrollRow(aLayer, aFrame, a, lThing);

                                 for lThing := lMid + 1 to Matrix.Height - 1 do
                                   ScrollRow(aLayer, aFrame, b, lThing);
                                end;
    modeSplitScrollUpDown,
    modeSplitSCrollDownUp     : begin
                                  lMid := Round(Matrix.Width / 2) - 1;

                                  case aMode of
                                    modeSplitScrollUpDown : begin
                                                              a := modeScrollColumnUp;
                                                              b := modeScrollColumnDown;
                                                            end;
                                    modeSplitSCrollDownUp : begin
                                                              a := modeScrollColumnDown;
                                                              b := modeScrollColumnUp;
                                                            end;
                                  else
                                    a := modeScrollColumnUp;
                                    b := modeScrollColumnDown;
                                  end;

                                  for lThing := 0 to lMid do
                                    ScrollColumn(aLayer, aFrame, a, lThing);

                                  for lThing := lMid + 1 to Matrix.Height - 1 do
                                    ScrollColumn(aLayer, aFrame, b, lThing);
                                end;
  end;
end;


procedure TTheMatrix.PerformAlternateScroll(aMode, aLayer, aFrame : integer);
var
  lCoEff, lCount, t, lMode : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  case aMode of
    modeAlternateScrollUpDown,
    modeAlternateScrollDownUp  : begin
                                   lCoEff := Round(Matrix.Width / 4);

                                   lCount := 0;
                                   lMode  := modeScrollColumnUp;

                                   for t := 0 to Matrix.Width - 1 do begin
                                     ScrollColumn(aLayer, aFrame, lMode, t);

                                     inc(lCount);

                                     if lCount = lCoEff then begin
                                       lCount := 0;

                                       if lMode = modeScrollColumnUp then
                                         lMode := modeScrollColumnDown
                                       else
                                         lMode := modeScrollColumnUp;
                                     end;
                                   end;

                                 end;
  end;
end;


procedure TTheMatrix.PerformWipeOnCurrentFrame(aMode : integer; aClear : boolean);
var
  x, y, z : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;
 
  BackupMatrix(FCurrentLayer, FCurrentFrame);

  case aMode of
     modeWipeVerticalOut     : begin
                                 z := Round(Matrix.Width / 2);

                                 for x := 0 to z - 2 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x + 1, y];
                                   end;
                                 end;

                                 for x := Matrix.Width - 1 downto z  + 1 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x - 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[z - 1, y] := FRGBBackground;
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[z, y]     := FRGBBackground;
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[z - 1, y] := MatrixBackup.Grid[0, y];
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[z, y]     := MatrixBackup.Grid[Matrix.Width - 1, y];
                                   end;
                                 end;
                               end;
     modeWipeVerticalIn      : begin
                                 z := Round(Matrix.Width / 2);

                                 for x := 1 to z - 1 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x - 1, y];
                                   end;
                                 end;

                                 for x := Matrix.Width - 1 downto z do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x + 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, y]               := FRGBBackground;
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, y] := FRGBBackground;
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, y]               := MatrixBackup.Grid[z - 1, y];
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, y] := MatrixBackup.Grid[z, y];
                                   end;
                                 end;
                               end;
     modeWipeHorizontalOut   : begin
                                 z := Round(Matrix.Height / 2);

                                 for y := 0 to z - 2 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y + 1];
                                   end;
                                 end;

                                 for y := Matrix.Height - 1 downto z + 1 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y - 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, z - 1] := FRGBBackground;
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, z]     := FRGBBackground;
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, z - 1] := MatrixBackup.Grid[x, 0];
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, z]     := MatrixBackup.Grid[x, Matrix.Height - 1];
                                   end;
                                 end;
                               end;
     modeWipeHorizontalIn    : begin
                                 z := Round(Matrix.Height / 2);

                                 for y := 1 to z - 1 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y - 1];
                                   end;
                                 end;

                                 for y := Matrix.Height - 1 downto z  do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y + 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, 0]                := FRGBBackground;
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, Matrix.Height - 1] := FRGBBackground;
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, 0]                := MatrixBackup.Grid[x, z - 1];
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, Matrix.Height - 1] := MatrixBackup.Grid[x, z];
                                   end;
                                 end;
                               end;
     modeWipeLeftToRight     : begin
                                 for x := 0 to Matrix.Width - 2 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x + 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, y] := FRGBBackground
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, y] := MatrixBackup.Grid[0, y];
                                   end;
                                 end;
                               end;
     modeWipeRightToLeft     : begin
                                 for x := 1 to Matrix.Width - 1 do begin
                                   for y := 0 to Matrix.Height - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x - 1, y];
                                   end;
                                 end;

                                 for y := 0 to Matrix.Height - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, y] := FRGBBackground
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, y] := MatrixBackup.Grid[Matrix.Width - 1, y];
                                   end;
                                 end;
                               end;
     modeWipeUpToDown        : begin
                                 for y := 0 to Matrix.Height - 2 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y + 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, Matrix.Height - 1] := FRGBBackground
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, Matrix.Height - 1] := MatrixBackup.Grid[x, 0];
                                   end;
                                 end;
                               end;
     modeWipeDownToUp        : begin
                                 for y := 1 to Matrix.Height - 1 do begin
                                   for x := 0 to Matrix.Width - 1 do begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixBackup.Grid[x, y - 1];
                                   end;
                                 end;

                                 for x := 0 to Matrix.Width - 1 do begin
                                   if (aClear) then begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, 0] := FRGBBackground
                                   end
                                   else begin
                                     MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, 0] := MatrixBackup.Grid[x, Matrix.Height - 1];
                                   end;
                                 end;
                               end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformRevealOnCurrentFrame(aMode, aColour : integer; var aParameter : integer);
var
  x, y : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  BackupMatrix(FCurrentLayer, FCurrentFrame);

  case aMode of
     modeRevealLeftRight     : begin
                                 if aParameter <= Matrix.Width - 1 then begin
                                   for x:= aParameter to Matrix.Width - 1 do begin
                                     for y := 0 to Matrix.Height - 1 do begin
                                       MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := aColour;
                                     end;
                                   end;

                                   inc(aParameter);
                                 end;
                               end;
     modeRevealRightLeft     : begin
                                 if aParameter >= 0 then begin
                                   for x := aParameter downto 0 do begin
                                     for y := 0 to Matrix.Height - 1 do begin
                                       MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := aColour;
                                     end;
                                   end;

                                   dec(aParameter);
                                 end;
                               end;
     modeRevealTopBottom     : begin
                                 if aParameter <= Matrix.Height - 1 then begin
                                   for y:= aParameter to Matrix.Height - 1 do begin
                                     for x := 0 to Matrix.Width - 1 do begin
                                       MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := aColour;
                                     end;
                                   end;

                                   inc(aParameter);
                                 end;
                               end;
     modeRevealBottomTop     : begin
                                 if aParameter >= 0 then begin
                                   for y := aParameter downto 0 do begin
                                     for x := 0 to Matrix.Width - 1 do begin
                                       MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := aColour;
                                     end;
                                   end;

                                   dec(aParameter);
                                 end;
                               end;
     modeRevealCentreIn      : begin
                               end;
     modeRevealCentreOut     : begin
                               end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformScrollOnCopyFrame(aMode : integer);
var
  x, y : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  BackupMatrix(-1, -1);

  case aMode of
     modeScrollLeft  : begin
                         for x := 0 to Matrix.Width - 2 do begin
                           for y := 0 to Matrix.Height - 1 do begin
                             MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x + 1, y];
                           end;
                         end;

                         for y := 0 to Matrix.Height - 1 do begin
                           MatrixCopy.Grid[Matrix.Width - 1, y] := MatrixBackup.Grid[0, y];
                         end;
                       end;
     modeScrollRight : begin
                         for x := 1 to Matrix.Width - 1 do begin
                           for y := 0 to Matrix.Height - 1 do begin
                             MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x - 1, y];
                           end;
                         end;

                         for y := 0 to Matrix.Height - 1 do begin
                           MatrixCopy.Grid[0, y] := MatrixBackup.Grid[Matrix.Width - 1, y];
                         end;
                       end;
     modeScrollUp    : begin
                         for y := 0 to Matrix.Height - 2 do begin
                           for x := 0 to Matrix.Width - 1 do begin
                             MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x, y + 1];
                           end;
                         end;

                         for x := 0 to Matrix.Width - 1 do begin
                           MatrixCopy.Grid[x, Matrix.Height - 1] := MatrixBackup.Grid[x, 0];
                         end;
                       end;
     modeScrollDown  : begin
                         for y := 1 to Matrix.Height - 1 do begin
                           for x := 0 to Matrix.Width - 1 do begin
                             MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x, y - 1];
                           end;
                         end;

                         for x := 0 to Matrix.Width - 1 do begin
                           MatrixCopy.Grid[x, 0] := MatrixBackup.Grid[x, Matrix.Height - 1];
                         end;
                       end;
  end;

  CopyBackupToCopyFrame;
end;


procedure TTheMatrix.PerformColumnScrollOnCurrentFrame(aMode, aColumn : integer; aClear : boolean);
var
  y : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  BackupMatrix(FCurrentLayer, FCurrentFrame);

  case aMode of
     modeScrollUp            : begin
                                 for y := 0 to Matrix.Height - 2 do begin
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, y] := MatrixBackup.Grid[aColumn, y + 1];
                                 end;

                                 if aClear then
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, Matrix.Height - 1] := 0
                                 else
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, Matrix.Height - 1] := MatrixBackup.Grid[aColumn, 0];
                               end;
     modeScrollDown          : begin
                                 for y := 1 to Matrix.Height - 1 do begin
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, y] := MatrixBackup.Grid[aColumn, y - 1];
                                 end;

                                 if aClear then
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, 0] := 0
                                 else
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[aColumn, 0] := MatrixBackup.Grid[aColumn, Matrix.Height - 1];
                               end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformRowScrollOnCurrentFrame(aMode, aRow : integer; aClear : boolean);
var
  x : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  BackupMatrix(FCurrentLayer, FCurrentFrame);

  case aMode of
     modeScrollLeft          : begin
                                 for x := 0 to Matrix.Width - 2 do begin
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, aRow] := MatrixBackup.Grid[x + 1, aRow];
                                 end;

                                 if aClear then
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, aRow] := 0
                                 else
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - 1, aRow] := MatrixBackup.Grid[0, aRow];
                               end;
     modeScrollRight         : begin
                                 for x := 1 to Matrix.Width - 1 do begin
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, aRow] := MatrixBackup.Grid[x - 1, aRow];
                                 end;

                                 if aClear then
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, aRow] := 0
                                 else
                                   MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[0, aRow] := MatrixBackup.Grid[Matrix.Width - 1, aRow];
                               end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  if not(FAutomateMode) then begin
    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.RotateFrameController(aMode, aMultipleOptionMode : integer);
var
  lLayer, lFrame : integer;

begin
 // CopyDrawBufferToCurrentFrame;

  FBusy := True;

  case aMultipleOptionMode of
    CMOMCurrentOnly        : RotateFrame(aMode, FCurrentLayer, FCurrentFrame);
    CMOMCurrentFrameLayers : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               RotateFrame(aMode, lLayer, FCurrentFrame);
                             end;
    CMOMCurrentLayerFrames : for lFrame := 1 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
                               RotateFrame(aMode, FCurrentLayer, lFrame);
                             end;
    CMOMAll                : for lLayer := 0 to MatrixLayers.Count - 1 do begin
                               for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
                                 RotateFrame(aMode, lLayer, lFrame);
                               end;
                             end;
  end;

  FBusy := False;

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.RotateFrame(aMode, aLayer, aFrame : integer);
var
  x, y : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  BackupMatrix(aLayer, aFrame);

  case aMode of
    modeRotateCW  : begin
                      for x := 0 to Matrix.Width - 1 do begin
                        for y := 0 to Matrix.Height - 1 do begin
                          MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[y, Matrix.Width - x - 1];
                        end;
                      end;
                    end;
    modeRotateACW : begin
                      for x := 0 to Matrix.Width - 1 do begin
                        for y := 0 to Matrix.Height - 1 do begin
                          MatrixLayers[aLayer].Frames[aFrame].Grid[x, y] := MatrixBackup.Grid[Matrix.Height - y - 1, x];
                        end;
                      end;
                    end;
  end;

  MatrixLayers[aLayer].Frames[aFrame].AddToHistory;
end;


procedure TTheMatrix.RotateFrameAnyAngle(aNewAngle : real; aToFrame : integer);   // to do for multilayer
var
  newx, newy, x, y, ox, oy, hx, hy: integer;
  myangle : real;

begin
  if (IsThisFrameLocked(FCurrentLayer, aToFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  MatrixLayers[FCurrentLayer].Frames[aToframe].Clear(Matrix.Mode, FRGBBackground);

  myangle := (pi * aNewAngle) / 180;
  hx      := Round((Matrix.Width - 1) / 2);
  hy      := Round((Matrix.Height - 1) / 2);

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      ox   := x - hx;
      oy   := y - hy;

      newx := hx + Round((ox * cos(myangle)) - (oy * sin(myangle)));
      newy := hy + Round((ox * sin(myangle)) + (oy * cos(myangle)));

      case Matrix.Mode of
        mtRGB,
        mtRGB3BPP : if ((newx >= 0) and (newx <= Matrix.Width - 1) and (newy >= 0) and (newy <= Matrix.Height - 1)) then
                        MatrixLayers[FCurrentLayer].Frames[aToFrame].Grid[newx, newy] := MatrixBackup.Grid[x, y];
      else
        if MatrixBackup.Grid[x, y] > 0 then begin
          if ((newx >= 0) and (newx <= Matrix.Width - 1) and (newy >= 0) and (newy <= Matrix.Height - 1)) then
            MatrixLayers[FCurrentLayer].Frames[aToFrame].Grid[newx, newy] := MatrixBackup.Grid[x, y];
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.ScrollRow(aLayer, aFrame, aMode, aRow : integer);
var
  x, lPixel : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  case aMode of
    modeScrollRowLeft  : begin
                           lPixel := MatrixLayers[aLayer].Frames[aFrame].Grid[0, aRow];

                           for x := 0 to Matrix.Width - 2 do
                             MatrixLayers[aLayer].Frames[aFrame].Grid[x, aRow] := MatrixLayers[aLayer].Frames[aFrame].Grid[x + 1, aRow];

                           MatrixLayers[aLayer].Frames[aFrame].Grid[Matrix.Width - 1, aRow] := lPixel;
                         end;
    modeScrollRowRight : begin
                           lPixel := MatrixLayers[aLayer].Frames[aFrame].Grid[Matrix.Width - 1, aRow];

                           for x := Matrix.Width - 1 downto 1 do
                             MatrixLayers[aLayer].Frames[aFrame].Grid[x, aRow] := MatrixLayers[aLayer].Frames[aFrame].Grid[x - 1, aRow];

                           MatrixLayers[aLayer].Frames[aFrame].Grid[0, aRow] := lPixel;
                         end;
  end;
end;


procedure TTheMatrix.ScrollColumn(aLayer, aFrame, aMode, aColumn : integer);
var
  y, lPixel : integer;

begin
  if (IsThisFrameLocked(aLayer, aFrame)) or
    not(MatrixLayers[aLayer].Visible) then
      Exit;

  case aMode of
    modeScrollColumnUp   : begin
                             lPixel := MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, 0];

                             for y := 0 to Matrix.Height - 1 do
                               MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, y] := MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, y + 1];

                             MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, Matrix.Height - 1] := lPixel;
                           end;

    modeScrollColumnDown : begin
                             lPixel := MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, Matrix.Height - 1];

                             for y := Matrix.Height - 1 downto 1 do
                               MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, y] := MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, y - 1];

                             MatrixLayers[aLayer].Frames[aFrame].Grid[aColumn, 0] := lPixel;
                           end;
  end;
end;


// =============================================================================


procedure TTheMatrix.RotateCopyBrush(aMode : integer);
var
  x, y : integer;

begin
  if Render.DrawData.CopyPos.X = Render.DrawData.CopyPos.Y then begin
    BackupMatrix(-1, -1);

    case aMode of
      modeRotateCW  : begin
                        for x := 0 to Render.DrawData.CopyPos.X do begin
                          for y := 0 to Render.DrawData.CopyPos.Y do begin
                            MatrixCopy.Grid[x, y] := MatrixBackup.Grid[y, Render.DrawData.CopyPos.X - x];
                          end;
                        end;
                      end;
      modeRotateACW : begin
                        for x := 0 to Render.DrawData.CopyPos.X do begin
                          for y := 0 to Render.DrawData.CopyPos.Y do begin
                            MatrixCopy.Grid[x, y] := MatrixBackup.Grid[Render.DrawData.CopyPos.Y - y, x];
                          end;
                        end;
                      end;
    end;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.PerformEffectOnBrush(aMode : integer);
var
  x,y : integer;

begin
  BackupMatrix(-1, -1);

  case aMode of
    modeFlip   : begin
                   for x := 0 to Render.DrawData.CopyPos.X do begin
                     for y := 0 to Render.DrawData.CopyPos.Y do begin
                       MatrixCopy.Grid[x, y] := MatrixBackup.Grid[Render.DrawData.CopyPos.X - x, y];
                     end;
                   end;
                 end;
    modeMirror : begin
                   for y := 0 to Render.DrawData.CopyPos.X do begin
                     for x := 0 to Render.DrawData.CopyPos.Y do begin
                       MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x, Render.DrawData.CopyPos.Y - y];
                     end;
                   end;
                 end;
    modeInvert : begin
                   for x := 0 to Render.DrawData.CopyPos.X do begin
                     for y := 0 to Render.DrawData.CopyPos.Y do begin
                       case Matrix.Mode of
                         mtMono         : MatrixCopy.Grid[x, y] := 1 - MatrixBackup.Grid[x, y];
                         mtBiSequential,
                         mtBiBitPlanes  : MatrixCopy.Grid[x, y] := 3 - MatrixBackup.Grid[x, y];
                         mtRGB          : MatrixCopy.Grid[x, y] := $FFFFFF - MatrixBackup.Grid[x, y];
                         mtRGB3BPP      : MatrixCopy.Grid[x, y] := $000004 - MatrixBackup.Grid[x, y];
                       end;
                     end;
                   end;
                 end;
  end;

  FPaintBox.Invalidate;
end;


// =============================================================================


procedure TTheMatrix.ChangeCurrentFrame(aFrame : integer);
 begin
  FCurrentFrame := aFrame;

//  PaintBox.Invalidate;

  CopyCurrentFrameToDrawBuffer;

  MatrixNewFrameDisplayed;
end;


procedure TTheMatrix.ChangeCurrentLayer(aLayer : integer);
begin
  CopyDrawBufferToCurrentFrame;

  FCurrentLayer := aLayer;

  CopyCurrentFrameToDrawBuffer;
end;


procedure TTheMatrix.ChangeLightBox(aLightBoxMode : integer);
 begin
  FLightBox := aLightBoxMode;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ChangeGrid(aGrid : boolean);
 begin
  Matrix.Grid := aGrid;

 if Matrix.Grid then
    Render.PixelSizeZ := Render.PixelSize - 1
  else
    Render.PixelSizeZ := Render.PixelSize;
end;


procedure TTheMatrix.ChangeDeadPixelsMode(aMode : boolean);
 begin
  FDeadPixelsMode := aMode;

  ConfigurePaintboxDrawing;
end;


procedure TTheMatrix.ChangeMatrixReadOnly(aMode : boolean);
 begin
  FMatrixReadOnly := aMode;

  ConfigurePaintboxDrawing;
end;


// =============================================================================


procedure TTheMatrix.CopyCurrentFrame;
var
  x,y : integer;

begin
  for y := 0 to Matrix.Height - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      MatrixCopy.Grid[x, y] := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y];
    end;
  end;
end;


procedure TTheMatrix.CopyBackupToCopyFrame;
var
  x,y : integer;

begin
  for y := 0 to Matrix.Height - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      MatrixCopy.Grid[x, y] := MatrixBackup.Grid[x, y];
    end;
  end;
end;


procedure TTheMatrix.PasteSpecial(aMode : integer);
begin
  PerformScrollOnCopyFrame(aMode);

  PasteCurrentFrame;
end;


procedure TTheMatrix.PasteCurrentFrame;
var
  x, y : integer;

begin
  if (IsThisFrameLocked(FCurrentLayer, FCurrentFrame)) or
    not(MatrixLayers[FCurrentLayer].Visible) then
      Exit;

  BackupMatrix(FCurrentLayer, FCurrentFrame);

  for y := 0 to Matrix.Height - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixCopy.Grid[x, y];
    end;
  end;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.InsertBlankFrameAt(aInsertAt : integer);
var
  lMatrix : TMatrix;
  lLayer  : integer;

begin
  if not(FAutomateMode) then
    FBusy := True;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

    if aInsertAt >= MatrixLayers[lLayer].Frames.Count - 1 then
      MatrixLayers[lLayer].Frames.Add(lMatrix)
    else
      MatrixLayers[lLayer].Frames.Insert(aInsertAt + 1, lMatrix);
  end;

  if not(FAutomateMode) then begin
    MatrixSizeChanged;

    FBusy := False;
  end;
end;


procedure TTheMatrix.InsertCopyFrameAt(aInsertAt : integer);
var
  x, y, lLayer : integer;
  lMatrix : TMatrix;

begin
  if not(FAutomateMode) then
    FBusy := True;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

    for x := 0 to Matrix.Width - 1 do begin
      for y := 0 to Matrix.Height - 1 do begin
        lMatrix.Grid[x, y] := MatrixLayers[lLayer].Frames[FCurrentFrame].Grid[x, y];
      end;
    end;

    if aInsertAt = MatrixLayers[lLayer].Frames.Count - 1 then
      MatrixLayers[lLayer].Frames.Add(lMatrix)
    else
      MatrixLayers[lLayer].Frames.Insert(aInsertAt + 1, lMatrix);
  end;

  if not(FAutomateMode) then begin
    MatrixSizeChanged;

    FBusy := False;
  end;
end;


procedure TTheMatrix.AddFrameMultiple(aFrameCount, aFrameCurrent : integer);
var
  oldframe, lFrame : integer;

begin
  oldframe := aFrameCurrent;

  for lFrame := 1 to aFrameCount do begin
    InsertBlankFrameAt(oldframe);

    inc(oldframe);
  end;
end;


procedure TTheMatrix.CopyShape;
var
  x, y, tc : integer;

begin
  for x := 0 to Matrix.Width do begin
    for y := 0 to Matrix.Height do begin
      if Matrix.Mode = mtRGB then
        MatrixCopy.Grid[x, y] := FRGBBackground
      else
        MatrixCopy.Grid[x, y] := 0;
    end;
  end;

  if (Render.DrawData.Coords[0].X > Render.DrawData.Coords[1].X) then begin
    tc := Render.DrawData.Coords[0].X;

    Render.DrawData.Coords[0].X := Render.DrawData.Coords[1].X;
    Render.DrawData.Coords[1].X := tc;
  end;

  if (Render.DrawData.Coords[0].Y > Render.DrawData.Coords[1].Y) then begin
    tc := Render.DrawData.Coords[0].Y;

    Render.DrawData.Coords[0].Y := Render.DrawData.Coords[1].Y;
    Render.DrawData.Coords[1].Y := tc;
  end;

  Render.DrawData.CopyPos.X := Render.DrawData.Coords[1].X - Render.DrawData.Coords[0].X;
  Render.DrawData.CopyPos.Y := Render.DrawData.Coords[1].Y - Render.DrawData.Coords[0].Y;

  for x := Render.DrawData.Coords[0].X to Render.DrawData.Coords[1].X do begin
    for y := Render.DrawData.Coords[0].Y to Render.DrawData.Coords[1].Y do begin
      MatrixCopy.Grid[x - Render.DrawData.Coords[0].X, y - Render.DrawData.Coords[0].Y] := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y];
    end;
  end;

  Render.DrawData.Point       := CDrawPointNone;
  Render.DrawData.Mode        := dmPaste;
  Render.DrawData.Coords[0].X := -1;
  Render.DrawData.Coords[0].Y := -1;
end;


procedure TTheMatrix.UpdateDrawTool(aSetX, aSetY, aSetColour : integer; aIsGradient : boolean);
 begin
  Render.DrawData.Coords[Render.DrawData.Point].X := aSetX;
  Render.DrawData.Coords[Render.DrawData.Point].Y := aSetY;

  if (Render.DrawData.Point = CDrawPointNone) then
    Render.DrawData.Colour := aSetColour;

  BackupMatrix(FCurrentLayer, FCurrentFrame);

  case Render.DrawData.Mode of
    dmFilledBox,
    dmEmptyBox,
    dmLine,
    dmEmptyCircle,
    dmFilledCircle  : begin
                        inc(Render.DrawData.Point);

                        if (Render.DrawData.Point = CDrawPointLast) then begin
                          DrawShape(False, Render.DrawData.Colour, aIsGradient);

                          CopyDrawBufferToCurrentFrame;
                        end;
                      end;
    dmCopy          : begin
                        inc(Render.DrawData.Point);

                        if (Render.DrawData.Point = CDrawPointLast) then begin
                          CopyCurrentFrameToDrawBuffer;

                          CopyShape;
                        end;
                      end;
    dmFloodFill     : FloodFill(aSetX, aSetY, Render.DrawData.Colour);
    dmSpiral,
    dmRing,
    dmSplitRing,
    dmPetals,
    dmGrid,
    dmPyramid,
    dmLeftTriangle,
    dmRightTriangle : begin
                        DrawShape(False, Render.DrawData.Colour, aIsGradient);

                        CopyDrawBufferToCurrentFrame;
                      end;
  end;
end;


procedure TTheMatrix.PlotInBounds(aX, aY, aColour : integer);
begin
  if (aX >= 0) and (aX <= Matrix.Width - 1) and
     (aY >= 0) and (aY <= Matrix.Height - 1) then
    PlotPixelMatrix(aX, aY, aColour);
end;


// this and PlotPixelMatrixFrame() are the only two safe methods of drawing on the matrix
// this takes in to account the gradient status and allows for the drawing buffer and
// various other drawing modes.
procedure TTheMatrix.PlotPixelMatrix(x, y, aDefaultColour : integer);
var
  lColour : integer;
  lNewCoord : integer;

begin
  lColour := aDefaultColour;

  if (FLastMouseButton = CMouseMiddle) then begin
    case (Render.Gradient) of
     // goOff        : lColour := aDefaultColour;
      goVertical   : lColour := Render.GradientIY[y];
      goHorizontal : lColour := Render.GradientIX[x];
    end;
  end;

  case FMirrorMode of
    mmHorizontal : lNewCoord := Matrix.Height - y - 1;
    mmVertical   : lNewCoord := Matrix.Width - x - 1;
  else
    lNewCoord := Matrix.Height - y - 1;
  end;

  FDisplayBuffer.Grid[x, y] := lColour;

  case FMirrorMode of
    mmHorizontal : FDisplayBuffer.Grid[x, lNewCoord] := lColour;
    mmVertical   : FDisplayBuffer.Grid[lNewCoord, y] := lColour;
  end;
end;


// this and PlotPixelMatrix() are the only two safe methods of drawing on the matrix
// this takes in to account the gradient status and allows for the drawing buffer and
// various other drawing modes.
procedure TTheMatrix.PlotPixelMatrixFrame(aFrame, x, y, aDefaultColour : integer); // check currentlayer is okay to o
var
  lColour : integer;
  lNewCoord : integer;

begin
  lColour := aDefaultColour;

  case (Render.Gradient) of
    goVertical   : lColour := Render.GradientIY[y];
    goHorizontal : lColour := Render.GradientIX[x];
  end;

  case FMirrorMode of
    mmHorizontal : lNewCoord := Matrix.Height - y - 1;
    mmVertical   : lNewCoord := Matrix.Width - x - 1;
  else
    lNewCoord := Matrix.Height - y - 1;
  end;

  if (aFrame = FCurrentFrame) then begin
    FDisplayBuffer.Grid[x, y] := lColour;

    case FMirrorMode of
      mmHorizontal : FDisplayBuffer.Grid[x, lNewCoord] := lColour;
      mmVertical   : FDisplayBuffer.Grid[lNewCoord, y] := lColour;
    end;
  end
  else begin
    MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[x, y] := lColour;

    case FMirrorMode of
      mmHorizontal : MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[x, lNewCoord] := lColour;
      mmVertical   : MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[lNewCoord, y] := lColour;
    end;
  end;
end;


procedure TTheMatrix.DrawShape(aRealTime : boolean; aColour : integer; aIsGradient : boolean);
var
  x, y, tc       : integer;
  a, b           : integer;  // displacements in x and y
  d              : integer;  // decision variable
  diag_inc       : integer;  // d's increment for diagonal steps
  dx_diag        : integer;  // diagonal x step for next pixel
  dx_nondiag     : integer;  // nondiagonal x step for next pixel
  dy_diag        : integer;  // diagonal y step for next pixel
  dy_nondiag     : integer;  // nondiagonal y step for next pixel
  i, j           : integer;  // loop index
  nondiag_inc    : integer;  // d's increment for nondiagonal steps
  x1, y1, x2, y2 : integer;

  procedure SimpleLine(x1, y1, x2, y2 : integer);
  var
    lColumn : integer;
    lColour : integer;

  begin
    lColumn := x1;
    lColour := aColour;

    if aIsGradient then begin
      case (Render.Gradient) of
        goVertical   : lColour := Render.GradientIY[y];
        goHorizontal : lColour := Render.GradientIX[x];
      end;
    end;

    FPaintBox.Canvas.Brush.Color := lColour;

    while lColumn <= x2 do begin
      PlotInBounds(lColumn, y1, lColour);

      inc(lColumn);
    end;
  end;

 begin
  CopyCurrentFrameToDrawBuffer;

  if not(aRealTime) then
    BackupMatrix(FCurrentLayer, FCurrentFrame)
  else begin
    Render.DrawData.Coords[1].X := lastx;
    Render.DrawData.Coords[1].Y := lasty;
  end;

  x1 := Render.DrawData.Coords[0].X;
  y1 := Render.DrawData.Coords[0].Y;
  x2 := Render.DrawData.Coords[1].X;
  y2 := Render.DrawData.Coords[1].Y;

  // ===========================================================================

  case Render.DrawData.Mode of
    // =========================================================================
    // == Filled Box ===========================================================
    // =========================================================================
    dmFilledBox             : begin
                                if (x1 > x2) then begin
                                  tc := x1;
                                  x1 := x2;
                                  x2 := tc;
                                end;

                                if (y1 > y2) then begin
                                  tc := y1;
                                  y1 := y2;
                                  y2 := tc;
                                end;

                                for x := x1 to x2 do begin
                                  for y := y1 to y2 do begin
                                    PlotPixelMatrix(x, y, aColour);
                                  end;
                                end;
                              end;

    // =========================================================================
    // == Empty Box ============================================================
    // =========================================================================
    dmEmptyBox              : begin
                                if (x1 > x2) then begin
                                  tc := x1;
                                  x1 := x2;
                                  x2 := tc;
                                end;

                                if (y1 > y2) then begin
                                  tc := y1;

                                  y1 := y2;
                                  y2 := tc;
                                end;

                                for x := x1 to x2 do begin
                                  for y := y1 to y2 do begin
                                    if ((x = x1) or (x = x2)) or ((y = y1) or (y = y2))  then begin
                                      PlotPixelMatrix(x, y, aColour);
                                    end;
                                  end;
                                end;
                              end;

    // =========================================================================
    // == Straight Line ========================================================
    // =========================================================================
    dmLine                  : begin
                                x := x1;              // line starting point
                                y := y1;

                                // Determine drawing direction and step to the next pixel.
                                a := x2 - x1;       // difference in x dimension
                                b := y2 - y1;       // difference in y dimension

                                // Determine whether end point lies to right or left of start point.
                                if   a < 0  then begin
                                  a := -a;                // make 'a' positive
                                  dx_diag := -1
                                end
                                else
                                  dx_diag := 1;

                                // Determine whether end point lies above or below start point.
                                if   b < 0 then begin
                                  b := -b;                // make 'a' positive
                                  dy_diag := -1
                                end
                                else
                                  dy_diag := 1;

                                // Identify octant containing end point.
                                if   a < b then begin
                                  tc := a;
                                  a := b;
                                  b := tc;
                                  dx_nondiag := 0;
                                  dy_nondiag := dy_diag
                                end
                                else begin
                                  dx_nondiag := dx_diag;
                                  dy_nondiag := 0
                                end;

                                d := b + b - a;            // initial value for d is 2*b - a
                                nondiag_inc := b + b;      // set initial d increment values
                                diag_inc    := b + b - a - a;

                                for i := 0 to a do begin   // draw the a+1 pixels

                                  PlotPixelMatrix(x, y, aColour);

                                  if d < 0 then begin               // step nondiagonally
                                    x := x + dx_nondiag;
                                    y := y + dy_nondiag;
                                    d := d + nondiag_inc   // update decision variable
                                  end
                                  else begin               // midpoint is above the line; step diagonally
                                    x := x + dx_diag;
                                    y := y + dy_diag;
                                    d := d + diag_inc
                                  end
                                end
                              end;

    // =========================================================================
    // == Empty Circle =========================================================
    // =========================================================================
    dmEmptyCircle           :  begin
                                 // c^2 = a^2 + b^2
                                 tc := round(sqrt(sqr(abs(x1 - x2)) + sqr(abs(y1 - y2)))); // radius of circle

                                 // midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
                                 a := 0;
                                 b := 1 - tc;

                                 while(tc >= a) do begin
                                   PlotInBounds( tc + x1,   a + y1, aColour);
                                   PlotInBounds(  a + x1,  tc + y1, aColour);
                                   PlotInBounds(-tc + x1,   a + y1, aColour);
                                   PlotInBounds( -a + x1,  tc + y1, aColour);
                                   PlotInBounds(-tc + x1,  -a + y1, aColour);
                                   PlotInBounds( -a + x1, -tc + y1, aColour);
                                   PlotInBounds( tc + x1,  -a + y1, aColour);
                                   PlotInBounds(  a + x1, -tc + y1, aColour);

                                   inc(a);

                                   if (b < 0) then
                                     b := b + 2 * a + 1
                                   else begin
                                     dec(tc);
                                     b := b + 2 * (a - tc + 1);
                                   end;
                                 end;
                               end;

    // =========================================================================
    // == Filled Circle ========================================================
    // =========================================================================
    dmFilledCircle           : begin
                                 // c^2 = a^2 + b^2
                                 tc := round(sqrt(sqr(abs(x1 - x2)) + sqr(abs(y1 - y2)))); // radius of circle

                                 // midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
                                 a := 0;
                                 b := 1 - tc;
                                 while(tc >= a) do begin
                                   if aRealTime then begin
                                     SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1);
                                     SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1);
                                     SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1);
                                     SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1);
                                   end
                                   else begin
                                     SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1);
                                     SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1);
                                     SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1);
                                     SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1);
                                   end;

                                   inc(a);

                                   if (b < 0) then
                                     b := b+ 2 * a + 1
                                   else begin
                                     dec(tc);
                                     b := b + 2 * (a - tc + 1);
                                   end;
                                 end;
                               end;

     // =========================================================================
     // == Copy Lasso thing =====================================================
     // =========================================================================
     dmCopy               : begin
                             if not(aRealTime) then exit;

                             if (x1 > x2) then begin
                               tc := x1;
                               x1 := x2;
                               x2 := tc;
                              end;

                              if (y1 > y2) then begin
                                tc := y1;

                                y1 := y2;
                                y2 := tc;
                              end;

                              FPaintBox.Canvas.Brush.Color := LEDColours[CDisplayMarker];

                              for x := x1 to x2 do begin
                                for y := y1 to y2 do begin
                                  if (((x = x1) or (x = x2)) or ((y = y1) or (y = y2))) then begin
                                    PlotPixelMatrix(x, y, aColour);
                                  end;
                                end;
                              end;
                            end;

     // =========================================================================
     // == Patterns: Spiral =====================================================
     // =========================================================================
     dmSpiral             : begin
                              a := LastX;
                              b := 0;

                              while b <= Matrix.Height - 1 do begin

                                PlotPixelMatrix(a, b, aColour);

                                if a = Matrix.Width - 1 then
                                  a := 0
                                else
                                  inc(a);

                                inc(b, Render.DrawData.Parameter);
                              end;
                            end;

     dmRing               : begin
                              y := LastY;

                              for x := 0 to Matrix.Width - 1 do begin
                                PlotPixelMatrix(x, y, aColour);
                              end;
                            end;

     dmSplitRing          : begin
                              x := LastX;
                              y := LastY;

                              if x = 0 then
                                a := Matrix.Width - 1
                              else
                                a := x - 1;

                              d := Render.DrawData.Parameter; // count between pixels X000X = 4

                              while x <> a do begin

                                if d = Render.DrawData.Parameter then begin

                                  PlotPixelMatrix(x, y, aColour);

                                  d := 0;
                                end
                                else
                                  inc(d);

                                if (x = Matrix.Width - 1) then
                                  x := 0
                                else
                                  inc(x);
                              end
                            end;

    dmPetals              : begin
                              x := LastX;

                              if x = 0 then
                                a := Matrix.Width - 1
                              else
                                a := x - 1;

                              d := Render.DrawData.Parameter;

                              while x <> a do begin
                                if (d = Render.DrawData.Parameter) then begin

                                  i := x; // left part
                                  j := x; // right part

                                  for y := Matrix.Height - 1 downto 0 do begin
                                    PlotPixelMatrix(i, y, aColour);
                                    PlotPixelMatrix(j, y, aColour);

                                    if i = 0 then
                                      i := Matrix.Width - 1
                                    else
                                      dec(i);

                                    if j = Matrix.Width - 1 then
                                      j := 0
                                    else
                                      inc(j);
                                  end;

                                  d := 1;
                                end
                                else
                                  inc(d);

                                if (x = Matrix.Width - 1) then
                                  x := 0
                                else
                                  inc(x);
                              end;
                            end;

    dmGrid                : begin
                              x := LastX;
                              y := LastY;

                              if x = 0 then
                                a := Matrix.Width - 1
                              else
                                a := x - 1;

                              if y = 0 then
                                b := Matrix.Height - 1
                              else
                                b := y - 1;

                              d := Render.DrawData.Parameter; // count between pixels X000X = 4

                              while x <> a do begin

                                if d = Render.DrawData.Parameter then begin

                                  for i := 0 to Matrix.Height - 1 do begin
                                    PlotPixelMatrix(x, i, aColour);
                                  end;

                                  d := 0;
                                end
                                else
                                  inc(d);

                                if (x = Matrix.Width - 1) then
                                  x := 0
                                else
                                  inc(x);
                              end;

                              d := Render.DrawData.Parameter;

                              while y <> b do begin

                                if d = Render.DrawData.Parameter then begin

                                  for i := 0 to Matrix.Width - 1 do begin
                                    PlotPixelMatrix(i, y, aColour);
                                  end;

                                  d := 0;
                                end
                                else
                                  inc(d);

                                if (y = Matrix.Height - 1) then
                                  y := 0
                                else
                                  inc(y);
                              end
                            end;
    dmPyramid             : begin
                              i := 1;
                              x := LastX - 1;
                              y := LastY;

                              while y <= Matrix.Height - 1 do begin
                                for a := 1 to i do
                                  PlotInBounds(x + a, y, aColour);

                                dec(x);
                                inc(i, 2);
                                inc(y, Render.DrawData.Parameter);
                              end;
                            end;
    dmLeftTriangle        : begin
                              i := 1;
                              x := LastX - 1;
                              y := LastY;

                              while y <= Matrix.Height - 1 do begin
                                for a := 1 to i do
                                  PlotInBounds(x + a, y, aColour);

                                inc(i, 1);
                                inc(y, Render.DrawData.Parameter);
                              end;

                            end;
    dmRightTriangle       : begin
                              i := 1;
                              x := LastX - 1;
                              y := LastY;

                              while y <= Matrix.Height - 1 do begin
                                for a := 1 to i do
                                  PlotInBounds(x + a, y, aColour);

                                dec(x);
                                inc(i, 1);
                                inc(y, Render.DrawData.Parameter);
                              end;
                            end;
  end;

  if not(aRealTime) then begin
    MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory(FDisplayBuffer);

    MatrixChange;

    Render.DrawData.Point       := CDrawPointNone;
    Render.DrawData.Coords[0].X := -1;
    Render.DrawData.Coords[0].Y := -1;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.DrawFontCharacter(aASCIICode, aFrame : integer);
var
  x, startY, y : integer;
  ts : string;

begin
  startY := Render.DrawData.Coords[0].Y;
  ts     := '';

  for x := FontMatrixStart[aASCIICode] to FontMatrixEnd[aASCIICode] do begin
    for y := 0 to 7 do begin
      if (Render.DrawData.Coords[0].X >= 0) and (Render.DrawData.Coords[0].X <= Matrix.Width - 1) and
         (y >= 0) and (y <= Matrix.Height - 1) then begin

        if Matrix.Mode = mtRGB then begin // to do
          case FontMatrixMode of
            mtMono : if FontMatrix[aASCIICode, x, y] = 1 then
                       MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[Render.DrawData.Coords[0].X, startY - y] := Render.DrawData.Colour;
            mtRGB  : if FontMatrix[aASCIICode, x, y] <> -1 then
                       MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[Render.DrawData.Coords[0].X, startY - y] := FontMatrix[aASCIICode, x, y];
          end;
        end
        else begin
          case FontMatrixMode of
            mtMono : if FontMatrix[aASCIICode, x, y] = 1 then
                       MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[Render.DrawData.Coords[0].X, startY - y] := Render.DrawData.Colour;
            mtRGB  : if FontMatrix[aASCIICode, x, y] <> -1 then
                       MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[Render.DrawData.Coords[0].X, startY - y] := Render.DrawData.Colour;
          end;
        end;
      end
    end;

    inc(Render.DrawData.Coords[0].X);
  end;

  inc(Render.DrawData.Coords[0].X); // adds single column spacing between chars

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.DeleteFontCharacter(aFrame : integer);
var
  y : integer;

begin
  dec(Render.DrawData.Coords[0].X);

  for y := Render.DrawData.Coords[0].Y downto Render.DrawData.Coords[0].Y - 7 do begin
    if (Render.DrawData.Coords[0].X >= 0) and (Render.DrawData.Coords[0].X <= Matrix.Width - 1) and
       (y >= 0) and (y <= Matrix.Height - 1) then begin

      MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[Render.DrawData.Coords[0].X, y] := 0;
    end;
  end;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.LoadFont(filename : string);
var
  tf : TextFile;
  s, lInput : string;
  t,p,lByte, lFrame, rowid, colid, lHeight : integer;
  headermode, haddata : boolean;
  lFirstData, lLastData : integer;

begin
  ClearFont;

  lFrame     := 0;
  colid      := 0;
  lFirstData := -1;
  lLastData  := -1;  

  // ===========================================================================

  AssignFile(tf, filename);
  Reset(tf);

  Readln(tf, s);

  if s = '{' + kFileHeaderFontRGB then begin
    FontMatrixMode := mtRGB;

    headermode := True;

    while not(eof(tf)) do begin
      Readln(tf, s);

      case s[1] of
        kDataBlockStart : begin
                            if headermode then begin
                              headermode := False;
                            end
                            else begin
                              if pos(kFontPrefixChar, s) <> 0 then begin
                                inc(lFrame);

                                colid := 0;

                                lFirstData := -1;
                                lLastData := -1;
                              end;
                            end;
                          end;
        kDataBlockEnd   : begin
                            if not headermode then begin
                              if lFirstData <> -1 then
                                FontMatrixStart[lFrame] := lFirstData
                              else
                                FontMatrixStart[lFrame] := 0;

                              if lLastData <> -1 then
                                FontMatrixEnd[lFrame] := lLastData
                              else
                                FontMatrixEnd[lFrame] := FontMatrixStart[lFrame];
                            end;
                          end;
        kRGBFontData    : begin
                            lInput  := '';
                            rowid   := lHeight - 1;
                            haddata := False;

                            for t := 3 to length(s) do begin
                              if s[t] = ' ' then begin
                                if lInput = '-1' then
                                  FontMatrix[lFrame, colid, rowid] := -1
                                else begin
                                  FontMatrix[lFrame, colid, rowid] := HexToInt(lInput);

                                  haddata := True;
                                end;

                                dec(rowid);

                                lInput := '';
                              end
                              else
                                lInput := lInput + s[t];
                            end;

                            if haddata then begin
                              if lFirstData = -1 then
                                lFirstData := colid
                              else
                                lLastData := colid;
                            end;

                            inc(colid);
                          end;
        kRGBFontHeight  : if headermode then begin
                            lHeight := StrToInt(Copy(s, 3, length(s) - 2));
                          end;

      end;
    end;
  end
  else begin  // could do with seeking to beginning of file
    FontMatrixMode := mtMono;

    while not(eof(tf)) do begin
      Readln(tf, s);

      if s[1] <> '/' then begin
        lInput := '';
        t      := 1;
        colid  := 0;

        while (s[t] <> '/') and (t <= length(s)) do begin
          if (ord(s[t]) = 32) then begin
            if lInput <> '' then begin
              lByte := StrToInt(lInput);

              for p := 0 to 7 do begin
                if (lByte and powers[p]) = powers[p] then
                  FontMatrix[lFrame, colid, p] := 1;
              end;

              lInput := '';
              inc(colid);
            end;
          end
          else if ((ord(s[t]) >= 48) and (ord(s[t]) <= 57)) then begin
            lInput := lInput + s[t];
          end;

          inc(t);
        end;

        FontMatrixStart[lFrame] := 0;
        FontMatrixEnd[lFrame]   := colid - 1;

        inc(lFrame);
      end;
    end;
  end;

  CloseFile(tf);
end;


// =============================================================================


procedure TTheMatrix.DeleteFrame(aFrame : integer);
var
  lLayer : integer;

begin
  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    MatrixLayers[lLayer].Frames.Delete(aFrame);
  end;

  if aFrame >= MatrixLayers[CPermanentLayer].Frames.Count then
    FCurrentFrame := MatrixLayers[CPermanentLayer].Frames.Count - 1;

  MatrixNewFrameDisplayed;

  MatrixSizeChanged;

  FPaintBox.Invalidate;
end;


// =============================================================================
// =============================================================================
// == Saving / Loading =========================================================
// =============================================================================
// =============================================================================


procedure TTheMatrix.ImportRowData(aHex : boolean; aSourceDirection, aSourceLSB : integer; s : string);
var
  t, x, rowindex : integer;
  zig, rowvalue : int64;
  temp : string;

begin
  if aSourceDirection = 0 then
    rowindex := 0
  else
    rowindex := Matrix.Height - 1;

  temp     := '';

  s := UpperCase(s);
  s := StringReplace(s, '0X', '$', [rfReplaceAll]);
  s := StringReplace(s, ',',  ' ', [rfReplaceAll]);
  s := StringReplace(s, '[',  '',  [rfReplaceAll]);
  s := StringReplace(s, ']',  '',  [rfReplaceAll]);
  s := StringReplace(s, '{',  '',  [rfReplaceAll]);
  s := StringReplace(s, '}',  '',  [rfReplaceAll]);
  s := StringReplace(s, '(',  '',  [rfReplaceAll]);
  s := StringReplace(s, ')',  '',  [rfReplaceAll]);
  s := s + ' ';

  for t := 1 to length(s) do begin
    if s[t] = ' ' then begin
      if temp <> '' then begin

        if aHex then
          rowvalue := StrToInt64('$' + temp)
        else
          rowvalue := StrToInt64(temp);

        if (rowindex >= 0) and (rowindex <= Matrix.Height - 1) then begin
          for x := 0 to Matrix.Width - 1 do begin
            case aSourceLSB of
              lsbLeft  : begin
                           zig := (rowvalue and Powers[x]);

                           if zig = Powers[x] then begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, rowindex] := 1;
                           end
                           else begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, rowindex] := 0;
                           end;
                         end;
              lsbRight : begin
                           zig:=(rowvalue and Powers[x]);

                           if zig = Powers[x] then begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - x - 1, rowindex] := 1;
                           end
                           else begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[Matrix.Width - x - 1, rowindex] := 0;
                           end;
                         end;
            end;
          end;
        end;

        temp := '';

        if aSourceDirection = 0 then
          inc(rowindex)
        else
          dec(rowindex);
      end;
    end
    else if (ord(s[t]) >= 48) and (ord(s[t]) <= 57) then begin
      temp := temp + s[t];
    end
    else if (ord(s[t]) >= 65) and (ord(s[t]) <= 90) then begin
      temp := temp + s[t];
    end
    else if (s[t] = '$') then begin
      aHex := True;
    end;

    MatrixChange;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ImportColumnData(aHex : boolean; aSourceDirection, aSourceLSB : integer; s : string);
var
  t, y, colindex  : integer;
  zig, colvalue : int64;
  temp : string;

begin
  if aSourceDirection = 0 then
    colindex := 0
  else
    colindex := Matrix.Width - 1;

  temp := '';

  s := UpperCase(s);
  s := StringReplace(s, '0X', '$', [rfReplaceAll]);
  s := StringReplace(s, ',',  ' ', [rfReplaceAll]);
  s := StringReplace(s, '[',  '', [rfReplaceAll]);
  s := StringReplace(s, ']',  '', [rfReplaceAll]);
  s := StringReplace(s, '{',  '', [rfReplaceAll]);
  s := StringReplace(s, '}',  '', [rfReplaceAll]);
  s := StringReplace(s, '(',  '', [rfReplaceAll]);
  s := StringReplace(s, ')',  '', [rfReplaceAll]);
  s := s + ' ';

  for t := 1 to length(s) do begin
    if s[t] = ' ' then begin
      if temp <> '' then begin

        if aHex then
          colvalue := StrToInt64('$' + temp)
        else
          colvalue := StrToInt64(temp);

        if (colindex >= 0) and (colindex <= Matrix.Width - 1) then begin
          for y := 0 to Matrix.Height - 1 do begin
            case aSourceLSB of
              lsbLeft  : begin
                           zig := (colvalue and Powers[y]);

                          if zig = Powers[y] then begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[colindex, y] := 1;
                           end
                           else begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[colindex, y] := 0;
                           end;
                         end;
              lsbRight : begin
                           zig := (colvalue and Powers[y]);

                           if zig = Powers[y] then begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[colindex, Matrix.Height - y] := 1;
                           end
                           else begin
                             MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[colindex, Matrix.Height - y] := 0;
                           end;
                         end;
            end;
          end;
        end;

        temp := '';

        if aSourceDirection = 0 then
          inc(colindex)
        else
          dec(colindex);
      end;
    end
    else if (ord(s[t]) >= 48) and (ord(s[t]) <= 57) then begin
      temp := temp + s[t];
    end
    else if (ord(s[t]) >= 65) and (ord(s[t]) <= 90) then begin
      temp := temp + s[t];
    end
    else if (s[t] = '$') then begin
      aHex := True;
    end;

    MatrixChange;
  end;

  FPaintBox.Invalidate;
end;


function TTheMatrix.LoadDataParameterType(const s : string; aHeaderMode, aMatrixMode, aDeadPixelMode, aLayerMode, aColoursMode : boolean): TLoadData;
begin
  Result := ldUnknown;

  if Pos('header', s) <> 0 then
    Result := ldLoadBlockStartHeader
  else if Pos('deadpixel', s) <> 0 then
    Result := ldLoadBlockStartDeadPixel
  else if Pos('colours', s) <> 0 then
    Result := ldLoadBlockStartColours
  else if s[1] = kDataBlockStart then
    Result := ldLoadBlockBegin
  else if s[1] = kDataBlockEnd then
    Result := ldLoadBlockEnd
  else if s[1] = '[' then
    Result := ldLoadBlockBeginLayout
  else if s[1] = ']' then
    Result := ldLoadBlockEndLayout
  else begin
    if aHeaderMode then begin
      case s[1] of
        kAnimDataSource         : Result := ldLoadHeaderSource;
        kAnimSourceLSB          : Result := ldLoadHeaderSourceLSB;
        kAnimSourceDirection    : Result := ldLoadHeaderSourceDirection;
        kAnimPadMode            : Result := ldLoadHeaderPadMode;
        kAnimHexFormat          : Result := ldLoadHeaderHexFormat;
        kAnimHexOutput          : Result := ldLoadHeaderHexOutput;
        kAnimBrackets           : Result := ldLoadHeaderBrackets;
        kAnimSource             : Result := ldLoadHeaderDataSource;
        kAnimOrientation        : Result := ldLoadHeaderOrientation;
        kAnimScanDirection      : Result := ldLoadHeaderScanDirection;
        kAnimLSB                : Result := ldLoadHeaderLSB;
        kAnimLanguage           : Result := ldLoadHeaderLanguage;
        kAnimNumberFormat       : Result := ldLoadHeaderNumberFormat;
        kAnimNumberSize         : Result := ldLoadHeaderNumberSize;
        kAnimLineContent        : Result := ldLoadHeaderLineContent;
        kAnimLineCount          : Result := ldLoadHeaderLineCount;
        kAnimRGBMode            : Result := ldLoadHeaderRGBMode;
        kAnimRGBChangePixels    : Result := ldLoadHeaderRGBChangePixels;
        kAnimRGBChangeColour    : Result := ldLoadHeaderRGBChangeColour;
        kAnimOptimise           : Result := ldLoadHeaderOptimise;
        kAnimRGBBrightness      : Result := ldLoadHeaderRGBBrightness;

        kAnimAutomationFileName : Result := ldLoadHeaderAutomationFile;
        kAnimComment            : Result := ldLoadHeaderMatrixComment;
	    	kAnimASCIIIndex         : Result := ldLoadHeaderASCIIIndex;
        kAnimRGBBackground      : Result := ldLoadHeaderRGBBackground;

        kAnimPreviewEnabled     : Result := ldLoadHeaderPreviewEnabled;
        kAnimPreviewSize        : Result := ldLoadHeaderPreviewSize;
        kAnimPreviewView        : Result := ldLoadHeaderPreviewView;
        kAnimPreviewVoid        : Result := ldLoadHeaderPreviewVoid;
        kAnimPreviewOffset      : Result := ldLoadHeaderPreviewOffset;
        kAnimPreviewDirection   : Result := ldLoadHeaderPreviewOffsetDir;
        kAnimPreviewIncRadially : Result := ldLoadHeaderPreviewIncRadially;
        kAnimLayerCount         : Result := ldLoadHeaderLayerCount;

        kAnimBlockEnd           : Result := ldLoadHeaderEnd;
      end;
    end
    else if aDeadPixelMode then begin
      case s[1] of
        kAnimDeadPixelData : Result := ldLoadDeadPixelData;
      end;
    end
    else if aMatrixMode then begin
      case s[1] of
        kAnimWidth       : Result := ldLoadMatrixWidth;
        kAnimHeight      : Result := ldLoadMatrixHeight;
        kAnimRowData     : Result := ldLoadMatrixData;
        kAnimFrameLocked : Result := ldLoadMatrixLocked;
      end
    end
    else if aLayerMode then begin
      case s[1] of
        kAnimLayerName   : Result := ldLoadLayoutName;
        kAnimLayerWidth  : Result := ldLoadLayoutWidth;
        kAnimLayerHeight : Result := ldLoadLayoutHeight;
        kAnimLayerLocked : Result := ldLoadLayoutLocked;
      end;
    end
    else if aColoursMode then begin
      case s[1] of
        kAnimColoursCustom         : Result := ldLoadColoursCustom;
        kAnimColoursLeft           : Result := ldLoadColoursDraw0;
        kAnimColoursMiddle         : Result := ldLoadColoursDraw1;
        kAnimColoursRight          : Result := ldLoadColoursDraw2;
        kAnimColoursPaletteHistory : Result := ldLoadColoursPaletteHistory;
      end;
    end;
  end;
end;


function TTheMatrix.ImportFromBMPSingleImage(aFileName : string; aFCount, aFWidth, aFHeight : integer; aRGBImport, aCreateNew : boolean): TImportData;
var
  ibmp : TBitmap;
  lFrame, w, h, wo : integer;
  lFrameStart, lFrameEnd : integer;
  lMatrix : TMatrix;

begin
  ibmp := TBitmap.Create;
  ibmp.LoadFromFile(aFileName);

  if aCreateNew then begin
    lFrameStart := 1;
    lFrameEnd   := aFCount;

    FFrameCount := aFCount;
  end
  else begin
    lFrameStart := FCurrentFrame;
    lFrameEnd   := FCurrentFrame + aFCount - 1;

    if lFrameEnd > FFrameCount then
      FFrameCount := lFrameEnd;
  end;

  // ===========================================================================

  for lFrame := lFrameStart to lFrameEnd do begin
    wo := (lFrame - lFrameStart) * aFWidth;

    if lFrame > MatrixLayers[FCurrentLayer].Frames.Count - 1 then begin
      lMatrix := TMatrix.Create(aFWidth, aFHeight, Matrix.Mode, FRGBBackground);
      MatrixLayers[FCurrentLayer].Frames.Add(lMatrix);
    end;

    for w := 0 to aFWidth - 1 do begin

      for h := 0 to aFHeight - 1 do begin
        if not aRGBImport then begin
          if ibmp.Canvas.Pixels[wo + w, h] = clBlack then
            MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := 0
          else
            MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := 1;
        end
        else begin
          MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := ibmp.Canvas.Pixels[wo + w, h];
        end;
      end;

    end;

    if lFrame = FCurrentFrame then
      CopyCurrentFrameToDrawBuffer;
  end;

  // ===========================================================================

  Result.NewWidth    := aFWidth;
  Result.NewHeight   := aFHeight;
  Result.NewFrames   := FFrameCount;
  Result.RGBImport   := aRGBImport;

  FPaintBox.Invalidate;

  Matrix.Available := True;

  MatrixChange;

  MatrixLayerChange;

  // ===========================================================================

  ibmp.Free;
end;


function TTheMatrix.ImportFromBMPMultipleImage(aPattern : string; aStartFrame, aCount, aPadLength, aFWidth, aFHeight : integer; aRGBImport, aCreateNew : boolean): TImportData;
var
  ibmp : TBitmap;
  lFileName : string;
  lFrame, i, w, h : integer;
  lMatrix : TMatrix;

begin
  for i := 0 to aCount - 1 do begin

    if aPadLength = 0 then
      lFileName := StringReplace(aPattern, '$$', IntToStr(aStartFrame + i), [rfReplaceAll])
    else
      lFileName := StringReplace(aPattern, '$$', TUtility.PadZeroes(IntToStr(aStartFrame + i), aPadLength), [rfReplaceAll]);

    ibmp := TBitmap.Create;
    ibmp.LoadFromFile(lFileName);

    if aCreateNew then begin
      lFrame := aStartFrame + i;
    end
    else begin
      lFrame := FCurrentFrame + i;
    end;

    // =========================================================================

    if lFrame > MatrixLayers[FCurrentLayer].Frames.Count - 1 then begin
      lMatrix := TMatrix.Create(aFWidth, aFHeight, Matrix.Mode, FRGBBackground); // to do
      MatrixLayers[FCurrentLayer].Frames.Add(lMatrix);
    end;

    for w := 0 to aFWidth - 1 do begin

      for h := 0 to aFHeight - 1 do begin

        if not aRGBImport then begin
          if ibmp.Canvas.Pixels[w, h] = clBlack then
            MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := 0
          else
            MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := 1;
        end
        else begin
          MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[w, h] := ibmp.Canvas.Pixels[w, h];
        end;
      end;
    end;

    ibmp.Free;
  end;

  // ===========================================================================

  Result.NewWidth    := aFWidth;
  Result.NewHeight   := aFHeight;
  Result.NewFrames   := FFrameCount;
  Result.RGBImport   := aRGBImport;

  FPaintBox.Invalidate;

  Matrix.Available := True;

  MatrixLayerChange;

  MatrixChange;
end;


function TTheMatrix.RowToString(aFrame, aRow : integer): string; // to do
var
  x : integer;

begin
  Result := '';

  for x := 0 to Matrix.Width - 1 do begin
    Result := Result + Copy(IntToHex(MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[x, aRow], 6), 1, 6) + ' ';
  end;
end;


procedure TTheMatrix.StringToRow(aCopyBrush : boolean; aString : string; aFrame, aRow, aTransparentColour : integer; aTransparent : boolean);
var
  x, i, lColour : integer;
  lInput : string;

begin
  x     := 0;
  lInput := '';

  for i := 1 to length(aString) do begin
    if (aString[i] = ' ') or (i = length(aString)) then begin

      lColour := HexToInt(lInput);

      if aCopyBrush then
        MatrixCopy.Grid[x, aRow] := lColour
      else begin
        if aTransparent then begin
          if lColour <> aTransparentColour then
            MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[x, aRow] := lColour
        end
        else
          MatrixLayers[FCurrentLayer].Frames[aFrame].Grid[x, aRow] := lColour
      end;

      inc(x);

      lInput := '';
    end
    else
      lInput := lInput + aString[i];
  end;
end;


function TTheMatrix.RightBounds: integer;
var
  x, y, lBound : integer;

begin
  lBound := 0;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      if (MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] <> FRGBBackground) then
        if (x > lBound) then
          lBound := x;
    end;
  end;

  Result := lBound;
end;


function TTheMatrix.BottomBounds: integer;
var
  x, y, lBound : integer;

begin
  lBound := 0;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      if (MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] <> FRGBBackground) then
        if (y > lBound) then
          lBound := y;
    end;
  end;

  Result := lBound;
end;


procedure TTheMatrix.SaveAnimation(aFileName : string; aTED : TImportData; aEEO : TExportOptions; aColours : TColours);
var
  s : string;
  tf : TextFile;
  x, y, i, lLayer : integer;

begin
  AssignFile(tf, aFileName);

  Rewrite(tf);

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderHeader);

  writeln(tf, kAnimPadMode +            ':' + IntToStr(Ord(aTED.PadMode)));
  writeln(tf, kAnimHexFormat +          ':' + IntToStr(Ord(aTED.HexFormat)));
  writeln(tf, kAnimHexOutput +          ':' + IntToStr(Ord(aTED.HexOutput)));
  writeln(tf, kAnimBrackets +           ':' + IntToStr(Ord(aTED.Brackets)));

  writeln(tf, kAnimPreviewEnabled +     ':' + BoolToStr(aTED.Preview.Enabled));
  writeln(tf, kAnimPreviewSize +        ':' + IntToStr(aTED.Preview.Size));
  writeln(tf, kAnimPreviewView +        ':' + IntToStr(Ord(aTED.Preview.View)));
  writeln(tf, kAnimPreviewVoid +        ':' + IntToStr(aTED.Preview.Void));
  writeln(tf, kAnimPreviewOffset +      ':' + IntToStr(aTED.Preview.Offset));
  writeln(tf, kAnimPreviewDirection +   ':' + BoolToStr(aTED.Preview.OffsetDirection));
  writeln(tf, kAnimPreviewIncRadially + ':' + BoolToStr(aTED.Preview.IncrementRadially));

  if (aEEO.ExportMode <> esNone) then begin // export options haven't been modified TO DO
    writeln(tf, kAnimSource +           ':' + IntToStr(Ord(aEEO.Source)));
    writeln(tf, kAnimOrientation +      ':' + IntToStr(Ord(aEEO.Orientation)));
    writeln(tf, kAnimScanDirection +    ':' + IntToStr(aEEO.ScanDirection));
    writeln(tf, kAnimLSB +              ':' + IntToStr(Ord(aEEO.LSB)));
    writeln(tf, kAnimLanguage +         ':' + IntToStr(Ord(aEEO.Language)));
    writeln(tf, kAnimNumberFormat +     ':' + IntToStr(Ord(aEEO.NumberFormat)));
    writeln(tf, kAnimNumberSize +       ':' + IntToStr(Ord(aEEO.NumberSize)));
    writeln(tf, kAnimLineContent +      ':' + IntToStr(Ord(aEEO.LineContent)));
    writeln(tf, kAnimLineCount +        ':' + IntToStr(aEEO.LineCount));

    writeln(tf, kAnimRGBMode +          ':' + IntToStr(Ord(aEEO.RGBMode)));
    writeln(tf, kAnimRGBChangePixels +  ':' + BoolToStr(aEEO.RGBChangePixels));
    writeln(tf, kAnimRGBChangeColour +  ':' + IntToStr(aEEO.RGBChangeColour));
    writeln(tf, kAnimRGBBrightness +    ':' + IntToStr(aEEO.RGBBrightness));

    writeln(tf, kAnimOptimise +         ':' + BoolToStr(aEEO.Optimise));
  end;

  writeln(tf, kAnimAutomationFileName + ':' + aTED.AutomationFileName);
  writeln(tf, kAnimComment +            ':' + Matrix.Comment);
  writeln(tf, kAnimRGBBackground +      ':' + IntToStr(FRGBBackground));
  writeln(tf, kAnimFrameRange +         ':' + IntToStr(aTED.StartFrame) + ',' + IntToStr(aTED.EndFrame));
  writeln(tf, kAnimLayerCount +         ':' + IntToStr(MatrixLayers.Count));
  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  if (aTED.MatrixMode = mtRGB) then begin
    writeln(tf, '{' + kFileHeaderColours);

    for i := 0 to 15 do
      writeln(tf, kAnimColoursCustom + ':' + IntToStr(aColours.CustomColours[i]));

    for i := 0 to 27 do
      writeln(tf, kAnimColoursPaletteHistory + ':' + IntToStr(aColours.PaletteHistory[i]));

    writeln(tf, kAnimColoursLeft +   ':' + IntToStr(aColours.DrawColours[CMouseLeft]));
    writeln(tf, kAnimColoursMiddle + ':' + IntToStr(aColours.DrawColours[CMouseMiddle]));
    writeln(tf, kAnimColoursRight +  ':' + IntToStr(aColours.DrawColours[CMouseRight]));

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    writeln(tf, '[' + kFileHeaderLayer);
    writeln(tf, kAnimLayerName +   ':' + MatrixLayers[lLayer].Name);
    writeln(tf, kAnimLayerWidth +  ':' + IntToStr(Matrix.Width));
    writeln(tf, kAnimLayerHeight + ':' + IntToStr(Matrix.Height));
    writeln(tf, kAnimLayerLocked + ':' + BoolToStr(MatrixLayers[lLayer].Locked));
    writeln(tf, ']');

    // =========================================================================

    for i := aTED.StartFrame to aTED.EndFrame do begin
      case aTED.MatrixMode of
        mtMono         : writeln(tf, '{' + kFilePrefixMono);
        mtBiSequential : writeln(tf, '{' + kFilePrefixBiSequential);
        mtBiBitPlanes  : writeln(tf, '{' + kFilePrefixBiBitPlanes);
        mtRGB          : writeln(tf, '{' + kFilePrefixRGB);
        mtRGB3BPP      : writeln(tf, '{' + kFilePrefixRGB3BPP);
      end;

      writeln(tf, kAnimWidth +  ':' + IntToStr(Matrix.Width));
      writeln(tf, kAnimHeight + ':' + IntToStr(Matrix.Height));

      for y := 0 to Matrix.Height - 1 do begin
        s := '';

        for x := 0 to Matrix.Width - 1 do begin
          s := s + Copy(IntToHex(MatrixLayers[lLayer].Frames[i].Grid[x, y], 6), 1, 6) + ' ';
        end;

        writeln(tf, kAnimRowData + ':' + s);
      end;

      writeln(tf, kAnimFrameLocked + ':' + BoolToStr(MatrixLayers[lLayer].Frames[i].Locked));

      writeln(tf, kDataBlockEnd);
    end;
  end;

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderDeadPixel);

  for y := 0 to Matrix.Height - 1 do begin
    s := '';

    for x := 0 to Matrix.Width - 1 do begin
      s := s + IntToStr(Ord(MatrixDead.Grid[x, y])) + ' ';
    end;

    writeln(tf, kAnimDeadPixelData + ':' + s);
  end;

  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  CloseFile(tf);
end;


procedure TTheMatrix.SaveFont(aFileName : string; aTED : TImportData; aEEO : TExportOptions);
var
  s : string;
  tf : TextFile;
  x, y, i, lLayer : integer;

begin
  AssignFile(tf, aFileName);

  Rewrite(tf);

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderFontHeader);

  writeln(tf, kAnimPreviewEnabled +   ':' + BoolToStr(aTED.Preview.Enabled));
  writeln(tf, kAnimPreviewSize +      ':' + IntToStr(aTED.Preview.Size));
  writeln(tf, kAnimPreviewView +      ':' + IntToStr(Ord(aTED.Preview.View)));
  writeln(tf, kAnimPreviewVoid +      ':' + IntToStr(aTED.Preview.Void));
  writeln(tf, kAnimPreviewOffset +    ':' + IntToStr(aTED.Preview.Offset));
  writeln(tf, kAnimPreviewDirection + ':' + BoolToStr(aTED.Preview.OffsetDirection));

  if aEEO.ExportMode <> esNone then begin // export options haven't been modified
    writeln(tf, kAnimSource +           ':' + IntToStr(Ord(aEEO.Source)));
    writeln(tf, kAnimOrientation +      ':' + IntToStr(Ord(aEEO.Orientation)));
    writeln(tf, kAnimScanDirection +    ':' + IntToStr(aEEO.ScanDirection));
    writeln(tf, kAnimLSB +              ':' + IntToStr(Ord(aEEO.LSB)));
    writeln(tf, kAnimLanguage +         ':' + IntToStr(Ord(aEEO.Language)));
    writeln(tf, kAnimNumberFormat +     ':' + IntToStr(Ord(aEEO.NumberFormat)));
    writeln(tf, kAnimNumberSize +       ':' + IntToStr(Ord(aEEO.NumberSize)));
    writeln(tf, kAnimLineContent +      ':' + IntToStr(Ord(aEEO.LineContent)));
    writeln(tf, kAnimLineCount +        ':' + IntToStr(aEEO.LineCount));

    writeln(tf, kAnimRGBMode +          ':' + IntToStr(Ord(aEEO.RGBMode)));
    writeln(tf, kAnimRGBChangePixels +  ':' + BoolToStr(aEEO.RGBChangePixels));
    writeln(tf, kAnimRGBChangeColour +  ':' + IntToStr(aEEO.RGBChangeColour));
  end;

  writeln(tf, kAnimAutomationFileName + ':' + aTED.AutomationFileName);
  writeln(tf, kAnimComment +            ':' + Matrix.Comment);
  writeln(tf, kAnimASCIIIndex +         ':' + IntToStr(aTED.ASCIIIndex));
  writeln(tf, kAnimRGBBackground +      ':' + IntToStr(FRGBBackground));
  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    writeln(tf, '[' + kFileHeaderLayer);
    writeln(tf, kAnimLayerName +   ':' + MatrixLayers[lLayer].Name);
    writeln(tf, kAnimLayerWidth +  ':' + IntToStr(Matrix.Width));
    writeln(tf, kAnimLayerHeight + ':' + IntToStr(Matrix.Height));
    writeln(tf, kAnimLayerLocked + ':' + BoolToStr(MatrixLayers[lLayer].Locked));
    writeln(tf, ']');

    for i := 1 to FontCharacterCount do begin
      case aTED.MatrixMode of
        mtMono         : writeln(tf, '{' + kFontPrefixMono);
        mtBiSequential : writeln(tf, '{' + kFontPrefixBiSequential);
        mtBiBitPlanes  : writeln(tf, '{' + kFontPrefixBiBitPlanes);
        mtRGB          : writeln(tf, '{' + kFontPrefixRGB);
        mtRGB3BPP      : writeln(tf, '{' + kFontPrefixRGB3BPP);
      end;

      writeln(tf, kAnimWidth +  ':' + IntToStr(Matrix.Width));
      writeln(tf, kAnimHeight + ':' + IntToStr(Matrix.Height));

      for y := 0 to Matrix.Height - 1 do begin
        s := '';

        for x := 0 to Matrix.Width - 1 do begin
          s := s + IntToHex(MatrixLayers[lLayer].Frames[i].Grid[x, y], 4) + ' ';
        end;

        writeln(tf, kAnimRowData + ':' + s);
      end;

      writeln(tf, kDataBlockEnd);
    end;
  end;

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderDeadPixel);

  for y := 0 to Matrix.Height - 1 do begin
    s := '';

    for x := 0 to Matrix.Width - 1 do begin
      s := s + IntToStr(Ord(MatrixDead.Grid[x, y])) + ' ';
    end;

    writeln(tf, kAnimDeadPixelData + ':' + s);
  end;

  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  CloseFile(tf);
end;


procedure TTheMatrix.SaveSingleFrame(aFileName : string; aTED : TImportData; aFrame : integer);
var
  s : string;
  tf : TextFile;
  x, y, lLayer : integer;

begin
  AssignFile(tf, aFileName);

  Rewrite(tf);

  case aTED.MatrixMode of
    mtMono         : writeln(tf, '{' + kFramePrefixMono);
    mtBiSequential : writeln(tf, '{' + kFramePrefixBiSequential);
    mtBiBitPlanes  : writeln(tf, '{' + kFramePrefixBiBitPlanes);
    mtRGB          : writeln(tf, '{' + kFramePrefixRGB);
    mtRGB3BPP      : writeln(tf, '{' + kFramePrefixRGB3BPP);
  end;

  writeln(tf, kAnimWidth +         ':' + IntToStr(Matrix.Width));
  writeln(tf, kAnimHeight +        ':' + IntToStr(Matrix.Height));
  writeln(tf, kAnimComment +       ':' + Matrix.Comment);
  writeln(tf, kAnimRGBBackground + ':' + IntToStr(FRGBBackground));

  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    writeln(tf, '[' + kFileHeaderLayer);
    writeln(tf, kAnimLayerName +   ':' + MatrixLayers[lLayer].Name);
    writeln(tf, kAnimLayerWidth +  ':' + IntToStr(Matrix.Width));
    writeln(tf, kAnimLayerHeight + ':' + IntToStr(Matrix.Height));
    writeln(tf, ']');

    writeln(tf, kDataBlockStart);

    for y := 0 to Matrix.Height - 1 do begin
      s := '';

      for x := 0 to Matrix.Width - 1 do begin
        s := s + IntToHex(MatrixLayers[lLayer].Frames[aFrame].Grid[x, y], 6) + ' ';
      end;

      writeln(tf, kAnimRowData + ':' + s);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderDeadPixel);

  for y := 0 to Matrix.Height - 1 do begin
    s := '';

    for x := 0 to Matrix.Width - 1 do begin
      s := s + IntToStr(Ord(MatrixDead.Grid[x, y])) + ' ';
    end;

    writeln(tf, kAnimDeadPixelData + ':' + s);
  end;

  writeln(tf, kDataBlockEnd);

  // ===========================================================================  

  CloseFile(tf);
end;


procedure TTheMatrix.SaveAsFont(aFileName : string); // to do
var
  tf : textfile;
  t, x, y, mydata : integer;
  s : string;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  for t := 1 to FontCharacterCount do begin
    s := '';

    for x := 0 to Matrix.Width - 1 do begin
      mydata := 0;

      for y := 0 to Matrix.Height - 1 do begin
        if MatrixLayers[CPermanentLayer].Frames[t].Grid[x, y] = 1 then
          mydata := mydata + (powers[Matrix.Height - y - 1]);
      end;

      if x <> Matrix.Width - 1 then
        s := s + IntToStr(mydata) + ', '
      else
        s := s + IntToStr(mydata);
    end;

    Writeln(tf, s + ' // ' + Char(32 + t));
  end;

  CloseFile(tf);
end;


procedure TTheMatrix.SaveAsRGBFont(aFileName : string);  // to do
var
  tf : textfile;
  t, x, y  : integer;
  mydata : string;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  writeln(tf, '{' + kFileHeaderFontRGB);
  writeln(tf, kRGBFontWidth  + ':' + IntToStr(Matrix.Width));
  writeln(tf, kRGBFontHeight + ':' + IntToStr(Matrix.Height));
  writeln(tf, kDataBlockEnd);

  for t := 1 to FontCharacterCount do begin
    writeln(tf, '{' + kFontPrefixChar);

    for x := 0 to Matrix.Width - 1 do begin
      mydata := '';

      for y := 0 to Matrix.Height - 1 do begin
        if MatrixLayers[CPermanentLayer].Frames[t].Grid[x, y] <> FRGBBackground then
          mydata := mydata + IntTohex(MatrixLayers[CPermanentLayer].Frames[t].Grid[x, y], 6) + ' '
        else
          mydata := mydata + '-1 ';
      end;

      writeln(tf, kRGBFontData + ':' + mydata);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  CloseFile(tf);
end;


// =============================================================================
// =============================================================================


function TTheMatrix.ExportToBitmap(aFileName : string): boolean;
var
  ibmp : TBitmap;
  lFrame, x, y : integer;

begin
  Result := True;

  try
    ibmp         := TBitmap.Create;
    FMatrixMerge := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

    ibmp.Width  := CurrentFrameCount * Matrix.Width;
    ibmp.Height := Matrix.Height;

    for lFrame := 1 to CurrentFrameCount do begin
      for x:= 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin

          BuildMergedFrame(lFrame, 2);

          ibmp.Canvas.Pixels[((lFrame - 1) * Matrix.Width) + x, y] := FMatrixMerge.Grid[x, y];
        end;
      end;
    end;

    ibmp.SaveToFile(aFileName);
  finally
    if (Assigned(ibmp)) then
      ibmp.Free;

    if (Assigned(FMatrixMerge)) then
      FMatrixMerge.Free;
  end;
end;


function TTheMatrix.ExportAnimationToBitmap(aFileName : string): boolean;
var
  lFilePrefix : string;
  ibmp : TBitmap;
  lFrame, x, y : integer;

begin
  Result := True;

  lFilePrefix := TUtility.GetFileNameNoExt(aFileName);

  try
    ibmp := TBitmap.Create;
    FMatrixMerge := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

    for lFrame := 1 to CurrentFrameCount do begin

      ibmp.Width  := Matrix.Width;
      ibmp.Height := Matrix.Height;

      for x:= 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin

          BuildMergedFrame(lFrame, 2);

          ibmp.Canvas.Pixels[x, y] := FMatrixMerge.Grid[x, y];
        end;
      end;

      ibmp.SaveToFile(lFilePrefix + '_' + TUtility.PadZeroes(IntToStr(lFrame), 6) + '.bmp');
    end;

  finally
    ibmp.Free;
    FMatrixMerge.Free;
  end;
end;


// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


// ensures that all layers have the same number of frames, crashes will occur
// if this is not the case!
procedure TTheMatrix.EnsureLayerCoherence;
var
  t, lFrame, lMax : integer;
  lMatrix : TMatrix;

begin
  lMax := 0;

  for t := 0 to MatrixLayers.Count - 1 do
    if (MatrixLayers[t].Frames.Count > lMax) then
      lMax := MatrixLayers[t].Frames.Count;

  for t := 0 to MatrixLayers.Count - 1 do begin
    if (MatrixLayers[t].Frames.Count <> lMax) then begin
      for lFrame := MatrixLayers[t].Frames.Count + 1 to lMax do begin
        lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

        MatrixLayers[t].Frames.Add(lMatrix);
      end;
    end;
  end;
end;


function TTheMatrix.AreLayersIdentical(aLayer1, aLayer2, aFrame : integer): boolean;
var
  x, y : integer;

begin
  Result := True;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      if (MatrixLayers[aLayer1].Frames[aFrame].Grid[x, y] <> MatrixLayers[aLayer2].Frames[aFrame].Grid[x, y]) then begin
        Result := False;

        Exit;
      end;
    end;
  end;
end;


function TTheMatrix.GetPixelFrom(aMatrixFormat, aImportFormat : TMatrixMode; aPixel, aBackground : integer): integer;
var
  t : integer;

begin
  Result := 0;

  case aMatrixFormat of
    mtMono          : case aImportFormat of
                        mtMono          : Result := aPixel;
                        mtBiSequential,
                        mtBiBitPlanes,
                        mtRGB,
                        mtRGB3BPP       : if not(aPixel = aBackground) then
                                            Result := 1;
                      end;
    mtBiSequential,
    mtBiBitPlanes   : case aImportFormat of
                        mtMono,
                        mtBiSequential,
                        mtBiBitPlanes   : Result := aPixel;
                        mtRGB,
                        mtRGB3BPP       : if not(aPixel = aBackground) then
                                            Result := 1;
                      end;
    mtRGB           : case aImportFormat of
                        mtMono,
                        mtBiSequential,
                        mtBiBitPlanes   : if not(aPixel = aBackground) then
                                            Result := $00ffffff;
                        mtRGB           : Result := aPixel;
                        mtRGB3BPP       : Result := LEDRGB3BPPColours[aPixel];
                      end;
    mtRGB3BPP       : case aImportFormat of
                        mtMono,
                        mtBiSequential,
                        mtBiBitPlanes    : if not(aPixel = aBackground) then
                                             Result := $00ffffff;
                        mtRGB            : for t := 0 to 7 do
                                             if LEDRGB3BPPColours[t] = aPixel then
                                               Result := LEDRGB3BPPColours[t];
                        mtRGB3BPP        : Result := aPixel;
                      end;
  end;
end;


function TTheMatrix.LoadLEDMatrixData(aFileName : string; var aEEO : TExportOptions; aLoadMode : TLoadMode; aStartFrame : integer): TImportData;
var
  tf : TextFile;
  pixel : string;
  x, lRow, i, lRGBBackground, lCurrentFrame, lCurrentLayer, lLayerCount : integer;
  lMatrixMode : TMatrixMode;
  tempMaxWidth, tempMaxHeight, lNewWidth, lNewHeight, lColour, lPalette, lInitialFrame : integer;
  s,v : string;
  lHeaderMode, fontmode, deadpixelmode, lMatrixDataMode, lLayerMode, lColoursMode : boolean;
  lMatrix : TMatrix;

  function SafeStringToBool(v : string): boolean;
  begin
    if (v = '') or (v = '0') then
      Result := False
    else
      Result := True;
  end;


begin
  // ===========================================================================

  lCurrentLayer := FCurrentLayer;
  lCurrentFrame := FCurrentFrame;

  case aLoadMode of
    lmNew                   : begin
                                ClearAllFrames;

                                lCurrentFrame := 1;
                                lCurrentLayer := 0;
                              end;
    lmMergeBottomPriority,
    lmMergeTopPriority      : lCurrentFrame := aStartFrame;
    lmAppend                : lCurrentFrame := MatrixLayers[CPermanentLayer].Frames.Count;
    lmMergeNewLayer         : begin
                                lCurrentFrame := aStartFrame;

                                AddLayer('Merge from ' + ExtractFileName(aFileName));

                                lCurrentLayer := MatrixLayers.Count - 1;
                              end;
    lmMergeCurrentLayer     : begin
                                lCurrentLayer := FCurrentLayer;
                                lCurrentFrame := 1;
                              end;
  end;

  lInitialFrame := lCurrentFrame;

  // ===========================================================================
  // ===========================================================================

  try
    AssignFile(tf, aFileName);
    Reset(tf);

    lHeaderMode            := False;
    fontmode               := False;
    deadpixelmode          := False;
    lMatrixDataMode        := False;
    lLayerMode             := False;
    lColoursMode           := False;

    lRow                   := 0;
    lMatrixMode            := mtMono;
    lRGBBackground         := -1; // was -1 !!!!
    lColour                := 0;
    lPalette               := 0;

    Result.ImportOk        := True;
    Result.MatrixMode      := mtMono;
    Result.RGBImport       := False;
    Result.RGBBrightness   := 100;

    Result.Colours.HasData := False;

    // clear rest of preview data?
    Result.Preview.Enabled           := MatrixMain.PreviewActive;
    Result.Preview.IncrementRadially := False;

    tempMaxWidth           := -1;
    tempMaxHeight          := -1;

    if aLoadMode = lmNew then
      SetDeadPixels(ptNormal);

    if (aLoadMode = lmAppend) then begin
      for i := 0 to MatrixLayers.Count - 1 do begin
        lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);
        MatrixLayers[i].Frames.Add(lMatrix);
      end;
    end;

    // ===========================================================================
    // ===========================================================================

    while not(eof(tf)) do begin
      readln(tf, s);

      if s <> '' then begin
        v := Copy(s, 3, length(s) - 2);

        case LoadDataParameterType(LowerCase(s), lHeaderMode, lMatrixDataMode, deadpixelmode, lLayerMode, lColoursMode) of
          ldLoadBlockStartHeader        : begin
                                           if UpperCase(s) = '{' + UpperCase(kFileHeaderFontHeader) then
                                             fontmode := True
                                           else
                                             fontmode := False;

                                           lHeaderMode := True;
                                         end;
          ldLoadBlockStartDeadPixel     : begin // dead pixel mode
                                           deadpixelmode   := True;
                                           lMatrixDataMode := False;

                                           lRow := 0;
                                         end;
          ldLoadBlockBegin              : begin
                                           lRow := 0;

                                           case v[length(v)] of
                                             '2' : lMatrixMode := mtBiSequential;
                                             '3' : lMatrixMode := mtBiBitPlanes;
                                             '4' : lMatrixMode := mtRGB;
                                             '5' : lMatrixMode := mtRGB3BPP;
                                           else
                                             lMatrixMode := mtMono;
                                           end;

                                           if aLoadMode = lmNew then
                                             Matrix.Mode := lMatrixMode;

                                           lHeaderMode     := False;
                                           lMatrixDataMode := True;
                                         end;
          ldLoadBlockEnd                : begin
                                           if (lMatrixDataMode) then
                                             inc(lCurrentFrame);
                                         end;
          ldLoadBlockBeginLayout        : begin
                                           lColoursMode    := False;
                                           lLayerMode      := True;
                                           lMatrixDataMode := False;
                                           lHeaderMode     := False;

                                           inc(lCurrentLayer);

                                           lCurrentFrame := lInitialFrame;
                                         end;
          ldLoadBlockEndLayout          : begin
                                           lLayerMode := False;

                                           case aLoadMode of
                                             lmNew : begin
                                                       Matrix.Height := tempMaxHeight;
                                                       Matrix.Width  := tempMaxWidth;
                                                     end;
                                           end;

                                           if (lLayerCount > 0) then
                                             for i := 1 to lLayerCount - 1 do
                                               AddLayer('');

                                           lLayerCount := -1;
                                         end;
          ldLoadBlockStartColours       : begin
                                           lHeaderMode            := False;
                                           lColoursMode           := True;

                                           Result.Colours.HasData := True;
                                         end;

         // ====================================================================

         ldLoadHeaderSource             : {};
         ldLoadHeaderSourceLSB          : {};
         ldLoadHeaderSourceDirection    : {};
         ldLoadHeaderPadMode            : Result.PadMode          := TPadFormat(StrToInt(v));
         ldLoadHeaderHexFormat          : Result.HexFormat        := THexFormat(StrToInt(v));
         ldLoadHeaderHexOutput          : Result.HexOutput        := THexPrefix(StrToInt(v));
         ldLoadHeaderBrackets           : Result.Brackets         := TBracketStyle(StrToInt(v));

         ldLoadHeaderDataSource         : aEEO.Source             := TReadSource(StrToInt(v));
         ldLoadHeaderOrientation        : aEEO.Orientation        := TInputOrientation(StrToInt(v));
         ldLoadHeaderScanDirection      : aEEO.ScanDirection      := StrToInt(v);
         ldLoadHeaderLSB                : aEEO.LSB                := TLSB(StrToInt(v));
         ldLoadHeaderLanguage           : aEEO.Language           := TExportLanguage(StrToInt(v));
         ldLoadHeaderNumberFormat       : aEEO.NumberFormat       := TNumberFormat(StrToInt(v));
         ldLoadHeaderNumberSize         : aEEO.NumberSize         := TNumberSize(StrToInt(v));
         ldLoadHeaderLineContent        : aEEO.LineContent        := TLineContent(StrToInt(v));
         ldLoadHeaderLineCount          : aEEO.LineCount          := StrToInt(v);
         ldLoadHeaderRGBMode            : aEEO.RGBMode            := TRGBMode(StrToInt(v));
         ldLoadHeaderRGBChangePixels    : aEEO.RGBChangePixels    := SafeStringToBool(v);
         ldLoadHeaderRGBChangeColour    : aEEO.RGBChangeColour    := StrToInt(v);

         ldLoadHeaderOptimise           : aEEO.Optimise           := SafeStringToBool(v);

         ldLoadHeaderMatrixComment      : Matrix.Comment          := v;
         ldLoadHeaderRGBBackground      : lRGBBackground          := StrToInt(v);

         ldLoadHeaderASCIIIndex         : Result.ASCIIIndex       := StrToInt(v);

         ldLoadHeaderAutomationFile     : Result.AutomationFileName := v;

         ldLoadHeaderRGBBrightness      : begin
                                           aEEO.RGBBrightness    := StrToInt(v);
                                           Result.RGBBrightness  := aEEO.RGBBrightness;
                                         end;

         // ======================================================================

         ldLoadHeaderPreviewEnabled     : Result.Preview.Enabled           := SafeStringToBool(v);
         ldLoadHeaderPreviewSize        : Result.Preview.Size              := StrToInt(v);
         ldLoadHeaderPreviewView        : Result.Preview.View              := TViewShape(StrToInt(v));
         ldLoadHeaderPreviewVoid        : Result.Preview.Void              := StrToInt(v);
         ldLoadHeaderPreviewOffset      : Result.Preview.Offset            := StrToInt(v);
         ldLoadHeaderPreviewOffsetDir   : Result.Preview.OffsetDirection   := SafeStringToBool(v);
         ldLoadHeaderPreviewIncRadially : Result.Preview.IncrementRadially := SafeStringToBool(v);

         ldLoadHeaderLayerCount : begin
                lLayerCount   := StrToInt(v);

                // layers have been saved in the file, so we know the first will be 0
                // set this to -1 so that when the [layer data is reached we increment from -1 to 0 ;)
                lCurrentLayer := -1;
              end;

         // ======================================================================

         ldLoadMatrixWidth              : tempMaxWidth  := StrToInt(v);
         ldLoadMatrixHeight             : tempMaxHeight := StrToInt(v);
         ldLoadMatrixData               : begin
                                           if (lRow = 0) and (MatrixLayers[lCurrentLayer].Frames.Count < lCurrentFrame + 1) then begin
                                             case (aLoadMode) of
                                               lmNew                 : begin
                                                                         lNewWidth  := tempMaxWidth;
                                                                         lNewHeight := tempMaxHeight;
                                                                       end;
                                               lmAppend,
                                               lmMergeBottomPriority,
                                               lmMergeTopPriority    : begin
                                                                         lNewWidth  := Matrix.Width;
                                                                         lNewHeight := Matrix.Height;
                                                                       end;
                                             end;

                                             lMatrix := TMatrix.Create(lNewWidth, lNewHeight, Matrix.Mode, FRGBBackground);
                                             MatrixLayers[lCurrentLayer].Frames.Add(lMatrix);
                                           end;

                                           x     := 0;
                                           pixel := '';

                                           if (lRGBBackground = -1) then
                                             lRGBBackground := FRGBBackground;

                                           for i := 1 to length(v) do begin

                                             if (v[i] = ' ') or (i = length(v)) then begin

                                               case aLoadMode of
                                                 lmMergeBottomPriority : begin
                                                                           if lMatrixMode = mtRGB then begin
                                                                             if MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].Grid[x, lRow] = lRGBBackground then
                                                                               MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].SafePlot(x, lRow, HexToInt(pixel));
                                                                             end
                                                                           else
                                                                             if MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].Grid[x, lRow] = 0 then
                                                                               MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].SafePlot(x, lRow, HexToInt(pixel));
                                                                         end;
                                                 lmMergeTopPriority    : begin
                                                                           if lMatrixMode = mtRGB then begin
                                                                             if HexToInt(pixel) <> lRGBBackground then
                                                                               MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].SafePlot(x, lRow, HexToInt(pixel));
                                                                             end
                                                                             else
                                                                               if HexToInt(pixel) <> 0 then
                                                                                 MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].SafePlot(x, lRow, HexToInt(pixel));
                                                                         end;
                                               else
                                                 MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].SafePlot(x, lRow, GetPixelFrom(Matrix.Mode, lMatrixMode, HexToInt(pixel), lRGBBackground));
                                               end;

                                               inc(x);

                                               pixel := '';
                                             end
                                             else
                                               pixel := pixel + v[i];
                                           end;

                                           inc(lRow);
                                         end;
         ldLoadMatrixLocked             : MatrixLayers[lCurrentLayer].Frames[lCurrentFrame].Locked := StrToBool(v);

         // ======================================================================

         ldLoadDeadPixelData            : begin
                                           x     := 0;
                                           pixel := '';

                                           for i := 1 to length(v) do begin
                                             if (v[i] = ' ') or (i = length(v)) then begin
                                               if (pixel = '0') then
                                                 MatrixDead.Grid[x, lRow] := ptNormal
                                               else
                                                 MatrixDead.Grid[x, lRow] := ptDead;

                                               inc(x);

                                               pixel := '';
                                             end
                                             else
                                               pixel := pixel + v[i];
                                           end;

                                           inc(lRow);
                                         end;

         // ====================================================================

         ldLoadLayoutName               : MatrixLayers[lCurrentLayer].Name := v;
         ldLoadLayoutWidth              : tempMaxWidth  := StrToInt(v);
         ldLoadLayoutHeight             : tempMaxHeight := StrToInt(v);
         ldLoadLayoutLocked             : MatrixLayers[lCurrentLayer].Locked := StrToBool(v);

         // ====================================================================

         ldLoadColoursCustom            : begin
                                           Result.Colours.CustomColours[lColour] := StrToInt(v);

                                           inc(lColour);
                                         end;
         ldLoadColoursDraw0             : Result.Colours.DrawColours[CMouseLeft]   := StrToInt(v);
         ldLoadColoursDraw1             : Result.Colours.DrawColours[CMouseMiddle] := StrToInt(v);
         ldLoadColoursDraw2             : Result.Colours.DrawColours[CMouseRight]  := StrToInt(v);
         ldLoadColoursPaletteHistory    : begin
                                           Result.Colours.PaletteHistory[lPalette] := StrToInt(v);

                                           inc(lPalette);
                                         end;
        end;
      end;
    end;

    CloseFile(tf);

    //for i := 0 to MatrixLayers.Count - 1 do begin
//      MatrixLayers[i].Frames.Delete(MatrixLayers[0].Frames.Count - 1); // do all
//    end;

    EnsureLayerCoherence;

    Matrix.Height := tempMaxHeight;
    Matrix.Width  := tempMaxWidth;

    Matrix.Available   := True;

    FCurrentFrame := 1;

    CopyCurrentFrameToDrawBuffer;

    if (aLoadMode = lmNew) then begin
      Result.MatrixMode       := lMatrixMode;
      Result.NewWidth         := tempMaxWidth;
      Result.NewHeight        := tempMaxHeight;
      Result.BackgroundColour := lRGBBackground;
    end;

    Result.MaxFrames        := MatrixLayers[0].Frames.Count - 1;
    Result.FontMode         := fontmode;

    aEEO.ExportMode   := esAnimation;
  except
    on E: Exception do begin
      Matrix.Available         := False;

      Result.ImportOk    := False;
      Result.ErrorString := GLanguageHandler.Text[kErrorLoadingProject] + ': "' + E.Message + '"';
    end;
  end;

  MatrixLayerChange;

  FPaintBox.Invalidate;
end;


function TTheMatrix.ImportLEDMatrixDataSingleFrame(aFileName : string): TImportData; // to do or cull ;)
var
  tf : TextFile;
  x, Row, MemSlot, i, lRGBBackground, lCurrentLayer : integer;
  lMatrixMode : TMatrixMode;
  s, v, pixel : string;
  addedSingleFrame, headerMode, deadpixelmode, lMatrixDataMode, fontmode, lLayerMode, lColoursMode : boolean;

begin
  BackupMatrix(FCurrentLayer, FCurrentFrame);

  addedSingleFrame       := False;
  lMatrixMode            := mtMono;
  headermode             := False;
  deadpixelmode          := False;
  fontmode               := False;
  lMatrixDataMode        := False;
  lLayerMode             := False;
  lColoursMode           := False;
  lRGBBackground         := -1;

  lCurrentLayer          := 0;

  Result.Source          := -1;
  Result.SourceLSB       := -1;
//  Result.SourceDirection := -1;
  Result.MatrixMode      := mtMono;
  Result.RGBImport       := False;

  // ===========================================================================
  // ===========================================================================

  AssignFile(tf, aFileName);
  Reset(tf);

  MemSlot    := FCurrentFrame;
  Row        := 0;

  while not(eof(tf)) and (addedSingleFrame = False) do begin
    readln(tf, s);

    if s<>'' then begin
      v  := Copy(s, 3, length(s) - 2);

      case LoadDataParameterType(s, headermode, lMatrixDataMode, deadpixelmode, lLayerMode, lColoursMode) of // TO DO layermode
        ldLoadBlockStartHeader      : begin
                                        if UpperCase(s) = '{' + UpperCase(kFileHeaderFontHeader) then
                                          fontmode := True
                                        else
                                          fontmode := False;

                                        headerMode := True;
                                      end;
        ldLoadBlockStartDeadPixel   : begin // dead pixel mode
                                        deadpixelmode   := True;
                                        lMatrixDataMode := False;

                                        Row := 0;
                                      end;
        ldLoadBlockBegin            : begin
                                        Row := 0;

                                        case v[length(v)] of
                                          '2' : lMatrixMode := mtBiSequential;
                                          '3' : lMatrixMode := mtBiBitPlanes;
                                          '4' : lMatrixMode := mtRGB;
                                          '5' : lMatrixMode := mtRGB3BPP;
                                        else
                                          lMatrixMode := mtMono;
                                        end;

                                        headerMode     := False;
                                        lMatrixDataMode := True;
                                      end;
         ldLoadBlockEnd             : begin
                                        if lMatrixDataMode then
                                          inc(MemSlot);
                                      end;
         ldLoadBlockBeginLayout     : lLayerMode := True;
         ldLoadBlockEndLayout       : lLayerMode := False;

       // ======================================================================

         ldLoadHeaderSource          : Result.Source          := StrToInt(v);
         ldLoadHeaderSourceLSB       : Result.SourceLSB       := StrToInt(v);
      //   ldLoadHeaderSourceDirection : Result.SourceDirection := StrToInt(v);
         ldLoadHeaderPadMode         : Result.PadMode         := TPadFormat(StrToInt(v));
         ldLoadHeaderHexFormat       : Result.HexFormat       := THexFormat(StrToInt(v));
         ldLoadHeaderHexOutput       : Result.HexOutput       := THexPrefix(StrToInt(v));
         ldLoadHeaderBrackets        : Result.Brackets        := TBracketStyle(StrToInt(v));

       // ======================================================================

//       50 : tempMaxWidth  := StrToInt(v);
//       51 : tempMaxHeight := StrToInt(v);
        ldLoadMatrixData            : begin
                                        x     := 0;
                                        pixel := '';

                                        for i := 1 to length(v) do begin
                                          if (v[i] = ' ') or (i = length(v)) then begin
                                            if Result.RGBImport then begin
                                              if lRGBBackground <> -1 then begin
                                                if HexToInt(pixel) = lRGBBackground then
                                                  MatrixLayers[lCurrentLayer].Frames[MemSlot].Grid[x, Row] := RGBBackground;
                                              end;
                                            end
                                            else
                                              MatrixLayers[lCurrentLayer].Frames[MemSlot].Grid[x, Row] := HexToInt(pixel);

                                            inc(x);

                                            pixel := '';
                                          end
                                          else
                                            pixel := pixel + v[i];
                                        end;

                                        inc(Row);
                                      end;

       // ======================================================================

        ldLoadDeadPixelData         : begin
                                        x     := 0;
                                        pixel := '';

                                        for i := 1 to length(v) do begin
                                          if (v[i] = ' ') or (i = length(v)) then begin
                                            if (pixel = '0') then
                                              MatrixDead.Grid[x, Row] := ptNormal
                                            else
                                              MatrixDead.Grid[x, Row] := ptDead;

                                            inc(x);

                                            pixel := '';
                                          end
                                          else
                                            pixel := pixel + v[i];
                                        end;

                                        inc(Row);
                                      end;

       // ======================================================================

        ldLoadLayoutName            : MatrixLayers[lCurrentLayer].Name := v;
      end;
    end;
  end;

  CloseFile(tf);

  Result.MatrixMode := lMatrixMode;

  MatrixChange;

  FPaintBox.Invalidate;
end;


// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


procedure TTheMatrix.LoadGradient(aFileName : string);
var
  tf : TextFile;
  s, v, lColour : string;
  t, idx, x, y, slot : integer;

 function parameterType(s : string): integer;
 begin
   if s[1] = kDataBlockStart then
     Result := 0
   else if s[1] = kDataBlockEnd then
     Result := 1
   else if s[1] = kGradientColour then
     Result := 2
   else
     Result := -1;
 end;

begin
  // ===========================================================================

  AssignFile(tf, aFileName);
  Reset(tf);

  while not(eof(tf)) do begin
    readln(tf, s);

    if s <> '' then begin
      v := Copy(s, 3, length(s) - 2);

      case parameterType(s) of
        2 : begin
              idx     := 0;
              lColour := '';

              for t := 1 to length(v) do begin
                if v[t] = ' ' then begin
                  if Render.Gradient = goVertical then
                    Render.GradientIY[idx] := StrToInt(lColour)
                  else
                    Render.GradientIX[idx] := StrToInt(lColour);

                  lColour := '';

                  inc(idx);
                end
                else
                  lColour := lColour + v[t];
              end;
            end;
      end;
    end;
  end;

  CloseFile(tf);

  // ===========================================================================

  for slot := 1 to MatrixLayers[0].Frames.Count - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      for y := 0 to Matrix.Height - 1 do begin
        if MatrixLayers[0].Frames[slot].Grid[x, y] <> FRGBBackground then
          MatrixLayers[0].Frames[slot].Grid[x, y] := Render.GradientIY[y];
      end;
    end;
  end;

  // ===========================================================================

  FPaintBox.Invalidate;
end;


// =============================================================================
// =============================================================================
// =============================================================================


procedure TTheMatrix.ClearUserBuffers;
var
  lBuffer : integer;

begin
  for lBuffer := 0 to 9 do begin
    MatrixUser[lBuffer].Clear(Matrix.Mode, FRGBBackground);
  end;
end;


procedure TTheMatrix.CopyToUserBuffer(aFrame : integer);
var
  x, y : integer;

begin
  for y := 0 to Matrix.Height - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      MatrixUser[aFrame].Grid[x, y] := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y];
    end;
  end;
end;


procedure TTheMatrix.RestoreFromUserBuffer(aFrame : integer);
var
  x, y : integer;

begin
  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      if Matrix.Mode = mtRGB then
        MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := MatrixUser[aFrame].Grid[x, y]
      else begin
        if MatrixUser[aFrame].Grid[x, y] = 1 then begin
          MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := 1;
        end
        else begin
          MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Grid[x, y] := 0;
        end;
      end;
    end;
  end;

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.CopyFromPrevious(toframe : integer);
var
  x, y, lLayer : integer;

begin
  if (toFrame <> 1) then begin

    for lLayer := 0 to MatrixLayers.Count - 1 do begin

      for y := 0 to Matrix.Height - 1 do begin
        for x := 0 to Matrix.Width - 1 do begin
          MatrixLayers[lLayer].Frames[toframe].Grid[x, y] := MatrixLayers[lLayer].Frames[toframe - 1].Grid[x, y];
        end;
      end;
    end;

    CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.CopyAllLayersFromTo(aFromFrame, aToFrame : integer);
var
  x, y, lLayer : integer;

begin
  if not(FAutomateMode) then begin
    if (aFromFrame = FCurrentFrame) then
      CopyDrawBufferToCurrentFrame;

    FBusy := True;
  end;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    for y := 0 to Matrix.Height - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        MatrixLayers[lLayer].Frames[aToFrame].Grid[x, y] := MatrixLayers[lLayer].Frames[aFromFrame].Grid[x, y];
      end;
    end;
  end;

  if not(FAutomateMode) then begin
    FBusy := False;

    if (aFromFrame = FCurrentFrame) then
      CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.CopyLayerFromTo(aSourceLayer, aDestinationLayer, aFromFrame, aToFrame : integer);
var
  x, y : integer;

begin
  if not(FAutomateMode) then begin
    if (aFromFrame = FCurrentFrame) then
      CopyDrawBufferToCurrentFrame;

    FBusy := True;
  end;

  for y := 0 to Matrix.Height - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      MatrixLayers[aDestinationLayer].Frames[aToFrame].Grid[x, y] := MatrixLayers[aSourceLayer].Frames[aFromFrame].Grid[x, y];
    end;
  end;

  if not(FAutomateMode) then begin
    FBusy := False;

    if (aFromFrame = FCurrentFrame) then
      CopyCurrentFrameToDrawBuffer;

    MatrixChange;

    FPaintBox.Invalidate;
  end;
end;


procedure TTheMatrix.Undo;
begin
  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Undo;

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Redo;
begin
  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Redo;

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


function TTheMatrix.GetUndoCount: integer;
begin
  Result := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].History.Count;
end;


procedure TTheMatrix.SetFromUndo(aUndo : integer);
begin
  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].SetFromUndo(aUndo);

  CopyCurrentFrameToDrawBuffer;

  MatrixChange;

  FPaintBox.Invalidate;
end;


function  TTheMatrix.CanUndo: boolean;
begin
  Result := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].HistoryOffset <> 0;
end;


function  TTheMatrix.CanRedo: boolean;
begin
  Result := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].HistoryOffset <> MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].History.Count - 1;
end;


procedure TTheMatrix.ConfigurePaintboxDrawing;
begin
  if FMatrixReadOnly then begin
    FPaintBox.OnMouseDown := Nil;
    FPaintBox.OnMouseMove := Nil;
    FPaintBox.OnMouseUp   := Nil;

    case Matrix.Mode of
      mtMono         : FPaintBox.OnPaint := PaintBoxUpdate;
      mtBiSequential : FPaintBox.OnPaint := PaintBoxUpdate;
      mtBiBitPlanes  : FPaintBox.OnPaint := PaintBoxUpdate;
      mtRGB          : FPaintBox.OnPaint := PaintBoxUpdateRGB;
      mtRGB3BPP      : FPaintBox.OnPaint := PaintBoxUpdateRGB_3BPP;
    end;
  end
  else begin
    if FDeadPixelsMode then begin
      FPaintBox.OnMouseDown := ClickPixelDeadPixel;
      FPaintBox.OnMouseMove := Shape1MouseMoveDeadPixel;
      FPaintBox.OnMouseUp   := Shape1MouseUpDeadPixel;

      FPaintBox.OnPaint     := PaintBoxUpdateDeadPixel;
    end
    else begin
      case Matrix.Mode of
        mtMono         : begin
                           FPaintBox.OnMouseDown := ClickPixel;
                           FPaintBox.OnMouseMove := Shape1MouseMove;
                           FPaintBox.OnMouseUp   := Shape1MouseUp;

                           FPaintBox.OnPaint     := PaintBoxUpdate;
                         end;
        mtBiSequential : begin
                           FPaintBox.OnMouseDown := ClickPixelBiColour;
                           FPaintBox.OnMouseMove := Shape1MouseMoveBiColour;
                           FPaintBox.OnMouseUp   := Shape1MouseUpBiColour;

                           FPaintBox.OnPaint     := PaintBoxUpdate;
                         end;
        mtBiBitPlanes  : begin
                           FPaintBox.OnMouseDown := ClickPixelBiColour;
                           FPaintBox.OnMouseMove := Shape1MouseMoveBiColour;
                           FPaintBox.OnMouseUp   := Shape1MouseUpBiColour;

                           FPaintBox.OnPaint     := PaintBoxUpdate;
                         end;
        mtRGB          : begin
                           FPaintBox.OnMouseDown := ClickPixelRGB;
                           FPaintBox.OnMouseMove := Shape1MouseMoveRGB;
                           FPaintBox.OnMouseUp   := Shape1MouseUpRGB;

                           FPaintBox.OnPaint     := PaintBoxUpdateRGB;
                         end;
        mtRGB3BPP      : begin
                           FPaintBox.OnMouseDown := ClickPixelRGB;
                           FPaintBox.OnMouseMove := Shape1MouseMoveRGB;
                           FPaintBox.OnMouseUp   := Shape1MouseUpRGB;

                           FPaintBox.OnPaint     := PaintBoxUpdateRGB_3BPP;
                         end;
      end;
    end;
  end;
end;


procedure TTheMatrix.CopyLEDColours;
var
  t : integer;

begin
  for t := 0 to 5 do begin
    case Matrix.Mode of
      mtMono         : LEDColours[t] := LEDColoursSingle[t];
      mtBiSequential,
      mtBiBitPlanes  : LEDColours[t] := LEDColoursBi[t];
    end;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ChangeSelectionColour(aSelectionLMB, aSelectionMMB, aSelectionRMB : integer);
 begin
  SetMouseButtonColours(aSelectionLMB, aSelectionMMB, aSelectionRMB);

  LEDRGBColours[CMouseLeft]   := aSelectionLMB;
  LEDRGBColours[CMouseMiddle] := aSelectionMMB;
  LEDRGBColours[CMouseRight]  := aSelectionRMB;

  ColourChange;
end;


function TTheMatrix.CalculateMemoryUsage: integer;
var
  a, b : integer;

begin
  case Matrix.Mode  of
    mtRGB      : Result := Matrix.Width * Matrix.Height * 4 * FrameCount;             // 4 bytes per pixel
    mtRGB3BPP  : Result := Ceil((Matrix.Width * Matrix.Height * 3 * FrameCount) / 8); // 3 bits per pixel
  else
    if Matrix.Height >= Matrix.Width then begin
      a := (Matrix.Height + 1) div 8;
      b := (Matrix.Width);
    end
    else begin
      a := (Matrix.Width + 1) div 8;
      b := (Matrix.Height);
    end;

    if FSoftwareMode = smFont then begin
      Result := (a * b) * (FontCharacterCount);         // always 96 frames in font mode
    end
    else begin
      Result := (a * b) * (FrameCount);
    end;

    // if using any of the bicolour modes then double requirements
    if (Matrix.Mode > mtMono) then
      Result := Result * 2;
  end;
end;


function TTheMatrix.DataSizeBytes: integer;
begin
  case Matrix.Mode of
    mtRGB      : Result := 4;
    mtRGB3BPP  : Result := Ceil(Matrix.Height / 8) * 3;
  else
    case Matrix.Height - 1 of
       0..7  : Result := 1;
       8..15 : Result := 2;
      16..23 : Result := 4;
      24..31 : Result := 4;
      32..39 : Result := 8;
      40..47 : Result := 8;
      48..55 : Result := 8;
      56..63 : Result := 8;
    else
      Result := 0;
    end;
  end;
end;


procedure TTheMatrix.FadeFirstToLast;
var
  x, y, lFrame : integer;
  rdy, gdy, bdy : integer;
  rdx, gdx, bdx : double;
  newr, newg, newb : double;
  colstart, colend : integer;
  gradheight : integer;
  newri, newgi, newbi : integer;

begin
  for y := 0 to Matrix.Height - 1 do begin
    for x:= 0 to Matrix.Width - 1 do begin

      colstart   := MatrixLayers[FCurrentLayer].Frames[1].Grid[x, y];
      colend     := MatrixLayers[FCurrentLayer].Frames[framecount].Grid[x, y];

      gradheight := FrameCount;

      rdy  := (colend and $0000FF) - (colstart and $0000FF);
      gdy  := ((colend and $00FF00) shr 8) - ((colstart and $00FF00) shr 8);
      bdy  := ((colend and $FF0000) shr 16) - ((colstart and $FF0000) shr 16);

      newr := (colstart and $0000FF);
      newg := (colstart and $00FF00) shr 8;
      newb := (colstart and $FF0000) shr 16;

      rdx  := rdy / gradheight;
      gdx  := gdy / gradheight;
      bdx  := bdy / gradheight;

      for lFrame := 2 to FrameCount do begin
        newr  := newr + rdx;
        newg  := newg + gdx;
        newb  := newb + bdx;

        newri := Floor(newr);
        newgi := Floor(newg);
        newbi := Floor(newb);

        MatrixLayers[FCurrentLayer].Frames[lFrame].Grid[x, y] := (newbi shl 16) + (newgi shl 8) + newri;
      end;
    end;
  end;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.Refresh;
 begin
  FPaintBox.Invalidate;
end;


function TTheMatrix.GetTotalUndos: integer;
var
  t, lLayer : integer;

begin
  Result := 0;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin

    for t:= 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
      Result := Result + MatrixLayers[lLayer].Frames[t].History.Count;
    end;
  end;
end;

     {
function TTheMatrix.GetColourFromXY(x, y : integer): integer;
begin
  case Matrix.Mode of
    psTypeRGB      : Result := FDisplayBuffer.Grid[x, y];
    psTypeRGB_3BPP : Result := LEDRGB3BPPColours[FDisplayBuffer.Grid[x, y]];
  else
    case FDisplayBuffer.Grid[x, y] of
      0 : Result := LEDColours[0];
      1 : Result := LEDColours[1];
      2 : Result := LEDColours[2];
      3 : Result := LEDColours[3];
    else
      Result := LEDColours[0];
    end;
  end;
end; }


procedure TTheMatrix.pbPreviewPaint(Sender: TObject);
var
  x, y : integer;

begin
  case Matrix.Mode of
    mtMono,
    mtBiSequential,
    mtBiBitPlanes   : BuildMonoBiRenderFrame;
    mtRGB           : BuildRGBRenderFrame;
    mtRGB3BPP       : BuildRGB3BPPRenderFrame;
  end;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      PreviewBox.Canvas.Brush.Color := MatrixRender.Grid[x, y];

      PreviewBox.Canvas.Pen.Color := PreviewBox.Canvas.Brush.Color;

      case Render.PixelShape of
        psSquare    : PreviewBox.Canvas.FillRect(Rect(x * FPreviewOptions.Size,
                                                      y * FPreviewOptions.Size,
                                                     (x * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                     (y * FPreviewOptions.Size) + FPreviewOptions.Size));
        psCircle    : PreviewBox.Canvas.Ellipse(x * FPreviewOptions.Size,
                                                y * FPreviewOptions.Size,
                                               (x * FPreviewOptions.Size) + FPreviewOptions.Size,
                                               (y * FPreviewOptions.Size) + FPreviewOptions.Size);
        psRoundRect : PreviewBox.Canvas.RoundRect(x * FPreviewOptions.Size,
                                                  y * FPreviewOptions.Size,
                                                 (x * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                 (y * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                  FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)),
                                                  FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)));
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================      

  if Render.DrawData.Mode <> dmNone then begin
    if Render.DrawData.Coords[0].X <> - 1 then begin
      case Matrix.Mode of
        mtRGB,
        mtRGB3BPP : PreviewBox.Canvas.Brush.Color := Render.DrawData.Colour;
      else
        PreviewBox.Canvas.Brush.Color := LEDColours[Render.DrawData.Colour];
      end;

      // need a preview version of draw shape
      //    DrawShape(True, PreviewBox.Canvas, FPreviewOptions.Size, FPreviewOptions.Size, 1, False);

      // =======================================================================

      PreviewBox.Canvas.Brush.Color := LEDColours[CDisplayMarker];

      case Render.PixelShape of
        psSquare    : PreviewBox.Canvas.FillRect(Rect(Render.DrawData.Coords[0].X * FPreviewOptions.Size,
                                                      Render.DrawData.Coords[0].Y * FPreviewOptions.Size.Size,
                                                     (Render.DrawData.Coords[0].X * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                     (Render.DrawData.Coords[0].Y * FPreviewOptions.Size) + FPreviewOptions.Size));
        psCircle    : PreviewBox.Canvas.Ellipse(Render.DrawData.Coords[0].X * FPreviewOptions.Size,
                                                Render.DrawData.Coords[0].Y * FPreviewOptions.Size,
                                               (Render.DrawData.Coords[0].X * FPreviewOptions.Size) + FPreviewOptions.Size,
                                               (Render.DrawData.Coords[0].Y * FPreviewOptions.Size) + FPreviewOptions.Size);
        psRoundRect : PreviewBox.Canvas.RoundRect(Render.DrawData.Coords[0].X * FPreviewOptions.Size,
                                                  Render.DrawData.Coords[0].Y * FPreviewOptions.Size,
                                                 (Render.DrawData.Coords[0].X * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                 (Render.DrawData.Coords[0].Y * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                  FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)),
                                                  FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)));
      end;
    end;
  end;

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================

  if Render.DrawData.CopyPos.X <> 0 then begin
    for x := 0 to Render.DrawData.CopyPos.X do begin
      for y := 0 to Render.DrawData.CopyPos.Y do begin
        if (x + lastx >= 0) and (x + lastx <= Matrix.Width - 1) and
          (y + lasty >= 0) and (y + lasty <= Matrix.Height - 1) then begin

           if Matrix.Mode = mtRGB then begin
             if MatrixDead.Grid[x + lastx, y + lasty] = ptNormal then
               PreviewBox.Canvas.Brush.Color := MatrixCopy.Grid[x, y]
             else
               PreviewBox.Canvas.Brush.Color := FRGBBackground;
           end
           else begin
             if MatrixDead.Grid[x + lastx, y + lasty] = ptNormal then
               PreviewBox.Canvas.Brush.Color := LEDColours[MatrixCopy.Grid[x, y]]
             else
               PreviewBox.Canvas.Brush.Color := clBtnFace;
           end;

           case Render.PixelShape of
             psSquare    : PreviewBox.Canvas.FillRect(Rect((x + lastx) * FPreviewOptions.Size,
                                                           (y + lasty) * FPreviewOptions.Size,
                                                          ((x + lastx) * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                          ((y + lasty) * FPreviewOptions.Size) + FPreviewOptions.Size));
             psCircle    : PreviewBox.Canvas.Ellipse((x + lastx) * FPreviewOptions.Size,
                                                     (y + lasty) * FPreviewOptions.Size,
                                                    ((x + lastx) * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                    ((y + lasty) * FPreviewOptions.Size) + FPreviewOptions.Size);
             psRoundRect : PreviewBox.Canvas.RoundRect((x + lastx) * FPreviewOptions.Size,
                                                       (y + lasty) * FPreviewOptions.Size,
                                                      ((x + lastx) * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                      ((y + lasty) * FPreviewOptions.Size) + FPreviewOptions.Size,
                                                       FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)),
                                                       FPreviewOptions.Size - (Round(FPreviewOptions.Size / CRoundRectCoeff)));
           end;
        end;
      end;
    end;
  end;
end;


procedure TTheMatrix.pbPreviewPaintRadial(Sender: TObject);
var
  x, y : integer;
  xp, yp : integer;
  lAC  : double;
  lAS  : double;
  lD  : double;
  lCX : integer;
  lCY : integer;

begin
  lCX := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);
  lCY := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);

  case Matrix.Mode of
    mtMono,
    mtBiSequential,
    mtBiBitPlanes   : BuildMonoBiRenderFrame;
    mtRGB           : BuildRGBRenderFrame;
    mtRGB3BPP       : BuildRGB3BPPRenderFrame;
  end;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      PreviewBox.Canvas.Brush.Color := MatrixRender.Grid[x, y];

      PreviewBox.Canvas.Pen.Color := PreviewBox.Canvas.Brush.Color;

      lAC :=  Cos(DegToRad(FRadialOffsetDegrees + ((Matrix.Width - 1 - x) / Matrix.Width - 1) * 360));
      lAS :=  Sin(DegToRad(FRadialOffsetDegrees + ((Matrix.Width - 1 - x) / Matrix.Width - 1) * 360));

      lD :=  (lCX - FPreviewOptions.ROffSet) / Matrix.Height;

      xp :=  lCX + Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAC);
      yp :=  lCY - Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAS);

      if FPreviewOptions.IncrementRadially then begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y),
                                  yp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y));
      end
      else begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel,
                                  yp + FPreviewOptions.RPixel);
      end;
    end;
  end;
end;


procedure TTheMatrix.pbPreviewPaintRadialThreeQuarters(Sender: TObject);
var
  x, y : integer;
  xp, yp : integer;
  lAC  : double;
  lAS  : double;
  lD  : double;
  lCX : integer;
  lCY : integer;

begin
  lCX := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);
  lCY := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);

  case Matrix.Mode of
    mtMono,
    mtBiSequential,
    mtBiBitPlanes   : BuildMonoBiRenderFrame;
    mtRGB           : BuildRGBRenderFrame;
    mtRGB3BPP       : BuildRGB3BPPRenderFrame;
  end;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      PreviewBox.Canvas.Brush.Color := MatrixRender.Grid[x, y];

      PreviewBox.Canvas.Pen.Color := PreviewBox.Canvas.Brush.Color;

      lAC :=  Cos(DegToRad(FRadialOffsetDegrees + 225 - (x / (Matrix.Width - 1)) * 270));
      lAS :=  Sin(DegToRad(FRadialOffsetDegrees + 225 - (x / (Matrix.Width - 1)) * 270));

      lD :=  (lCX - FPreviewOptions.ROffSet) / Matrix.Height;

      xp :=  lCX + Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAC);
      yp :=  lCY - Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAS);

      if FPreviewOptions.IncrementRadially then begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y),
                                  yp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y));
      end
      else begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel,
                                  yp + FPreviewOptions.RPixel);
      end;
    end;
  end;
end;


procedure TTheMatrix.pbPreviewPaintSemiCircle(Sender: TObject);
var
  x, y : integer;
  xp, yp : integer;
  lAC  : double;
  lAS  : double;
  lD  : double;
  lCX : integer;
  lCY : integer;

begin
  lCX := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);
  lCY := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);

  case Matrix.Mode of
    mtMono,
    mtBiSequential,
    mtBiBitPlanes   : BuildMonoBiRenderFrame;
    mtRGB           : BuildRGBRenderFrame;
    mtRGB3BPP       : BuildRGB3BPPRenderFrame;
  end;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      PreviewBox.Canvas.Brush.Color := MatrixRender.Grid[x, y];

      PreviewBox.Canvas.Pen.Color := PreviewBox.Canvas.Brush.Color;

      lAC :=  Cos(DegToRad(FRadialOffsetDegrees + 180 - (x / (Matrix.Width - 1)) * 180));
      lAS :=  Sin(DegToRad(FRadialOffsetDegrees + 180 - (x / (Matrix.Width - 1)) * 180));

      lD :=  (lCX - FPreviewOptions.ROffSet) / Matrix.Height;

      xp :=  lCX + Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAC);
      yp :=  lCY - Round((FPreviewOptions.ROffSet + (lD * (Matrix.Height - 1 - y))) * lAS);

      if FPreviewOptions.IncrementRadially then begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y),
                                  yp + FPreviewOptions.RPixel + (Matrix.Height - 1 - y));
      end
      else begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel,
                                  yp + FPreviewOptions.RPixel);
      end;
    end;
  end;
end;


procedure TTheMatrix.pbPreviewPaintSemiCircleInverted(Sender: TObject);
var
  x, y : integer;
  xp, yp : integer;
  lAC  : double;
  lAS  : double;
  lD  : double;
  lCX : integer;
  lCY : integer;

begin
  lCX := Round(Min(PreviewBox.Width, PreviewBox.Height) / 2);
  lCY := 4;

  case Matrix.Mode of
    mtMono,
    mtBiSequential,
    mtBiBitPlanes   : BuildMonoBiRenderFrame;
    mtRGB           : BuildRGBRenderFrame;
    mtRGB3BPP       : BuildRGB3BPPRenderFrame;
  end;

  for x := 0 to Matrix.Width - 1 do begin
    for y := 0 to Matrix.Height - 1 do begin
      PreviewBox.Canvas.Brush.Color := MatrixRender.Grid[x, y];

      PreviewBox.Canvas.Pen.Color := PreviewBox.Canvas.Brush.Color;

      lAC :=  Cos(DegToRad(FRadialOffsetDegrees + 180 + (x / (Matrix.Width - 1)) * 180));
      lAS :=  Sin(DegToRad(FRadialOffsetDegrees + 180 + (x / (Matrix.Width - 1)) * 180));

      lD :=  (lCX - FPreviewOptions.ROffSet) / Matrix.Height;

      xp :=  lCX + Round((FPreviewOptions.ROffSet + (lD * (y))) * lAC);
      yp :=  lCY - Round((FPreviewOptions.ROffSet + (lD * (y))) * lAS);

      if FPreviewOptions.IncrementRadially then begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel + y,
                                  yp + FPreviewOptions.RPixel + y);
      end
      else begin
        PreviewBox.Canvas.Ellipse(xp,
                                  yp,
                                  xp + FPreviewOptions.RPixel,
                                  yp + FPreviewOptions.RPixel);
      end;
    end;
  end;
end;


procedure TTheMatrix.AutomationPostProcessExecute(var aAO : TActionObject; aActionID : integer);
var
  x, lIndex : integer;
  lDirection : TCyclingDirection;

begin
  if (aAO.Layer < 0) or (aAO.layer > MatrixLayers.Count - 1)  then
    Exit;

  case aActionID of
          // == colour cycling =================================================

     27 : begin  // linear
            lIndex     := aAO.CCTargetIndex;

            for x := 0 to aAO.SourceColours.Count - 1 do begin
              MatrixLayers[aAO.Layer].Frames[FCurrentFrame].ChangePixels(StrToInt(aAO.SourceColours[x]),
                                                                         StrToInt(aAO.TargetColours[lIndex]));

              if (lIndex = aAO.TargetColours.Count - 1) then
                lIndex := 0
              else
                inc(lIndex);
            end;

            if (aAO.TargetSkipIndex = 0) then begin
              if (aAO.CCTargetIndex = aAO.TargetColours.Count - 1) then
                aAO.CCTargetIndex := 0
              else
               inc(aAO.CCTargetIndex);
            end;
          end;

     28 : begin  // bounceybouncey
            lIndex     := aAO.CCTargetIndex;
            lDirection := aAO.CCDirection;

            for x := 0 to aAO.SourceColours.Count - 1 do begin
              MatrixLayers[aAO.Layer].Frames[FCurrentFrame].ChangePixels(StrToInt(aAO.SourceColours[x]),
                                                                           StrToInt(aAO.TargetColours[lIndex]));
              if lDirection = cdForwards then begin
                if (lIndex = aAO.TargetColours.Count - 1) then begin
                  lIndex := aAO.TargetColours.Count - 2;

                  lDirection := cdBackwards;
                end
                else
                 inc(lIndex);
              end
              else begin
                if (lIndex = 0) then begin
                  lIndex := 1;

                  lDirection := cdForwards;
                end
                else
                  dec(lIndex);
              end;
            end;

            if (aAO.TargetSkipIndex = 0) then begin

              if aAO.CCDirection = cdForwards then begin
                if (aAO.CCTargetIndex = aAO.TargetColours.Count - 1) then begin
                  aAO.CCTargetIndex := aAO.TargetColours.Count - 2;

                  aAO.CCDirection := cdBackwards;
                end
                else
                  inc(aAO.CCTargetIndex);
              end
              else begin
                if (aAO.CCTargetIndex = 0) then begin
                  aAO.CCTargetIndex := 1;

                  aAO.CCDirection := cdForwards;
                end
                else
                  dec(aAO.CCTargetIndex);
              end;
            end;
          end

  else
    MessageDlg('Error: unknown action ID "' + IntToStr(aActionID) + '".', mtError, [mbOK], 0);
  end;
end;


procedure TTheMatrix.AutomationActionExecute(var aAO : TActionObject; aActionID : integer);
var
  x : integer;

begin
  case aActionID of
     0 : PerformEffect(modeMirror, aAO.Layer, FCurrentFrame);
     1 : PerformEffect(modeFlip,   aAO.Layer, FCurrentFrame);
     2 : PerformEffect(modeInvert, aAO.Layer, FCurrentFrame);

     3 : PerformScroll(modeScrollLeft,  aAO.Layer, FCurrentFrame);
     4 : PerformScroll(modeScrollRight, aAO.Layer, FCurrentFrame);
     5 : PerformScroll(modeScrollUp,    aAO.Layer, FCurrentFrame);
     6 : PerformScroll(modeScrollDown,  aAO.Layer, FCurrentFrame);

     7 : RotateFrame(modeRotateACW, aAO.Layer, FCurrentFrame);
     8 : RotateFrame(modeRotateCW,  aAO.Layer, FCurrentFrame);

     9 : PerformWipeOnCurrentFrame(modeWipeVerticalOut,   aAO.EraseBehind);
    10 : PerformWipeOnCurrentFrame(modeWipeVerticalIn,    aAO.EraseBehind);
    11 : PerformWipeOnCurrentFrame(modeWipeHorizontalOut, aAO.EraseBehind);
    12 : PerformWipeOnCurrentFrame(modeWipeHorizontalIn,  aAO.EraseBehind);
    36 : PerformWipeOnCurrentFrame(modeWipeLeftToRight,   aAO.EraseBehind);
    37 : PerformWipeOnCurrentFrame(modeWipeRightToLeft,   aAO.EraseBehind);
    38 : PerformWipeOnCurrentFrame(modeWipeUpToDown,      aAO.EraseBehind);
    39 : PerformWipeOnCurrentFrame(modeWipeDownToUp,      aAO.EraseBehind);

    13 : if (aAO.ProcesingStage < Matrix.Height) then begin
           for x := 0 to aAO.ProcesingStage mod Matrix.Height do
             PerformRowScrollOnCurrentFrame(modeScrollLeft, x, aAO.EraseBehind)
           end
         else
           for x := 0 to Matrix.Height - 1 do
             PerformRowScrollOnCurrentFrame(modeScrollLeft, x, aAO.EraseBehind);

    14 : if (aAO.ProcesingStage < Matrix.Height) then begin
           for x := 0 to aAO.ProcesingStage mod Matrix.Height do
             PerformRowScrollOnCurrentFrame(modeScrollRight, x, aAO.EraseBehind)
           end
         else
           for x := 0 to Matrix.Height - 1 do
             PerformRowScrollOnCurrentFrame(modeScrollRight, x, aAO.EraseBehind);

    15 : if (aAO.ProcesingStage < Matrix.Width) then begin
            for x := 0 to aAO.ProcesingStage mod Matrix.Width do
              PerformColumnScrollOnCurrentFrame(modeScrollUp, x, aAO.EraseBehind)
            end
          else
            for x := 0 to Matrix.Width - 1 do
              PerformColumnScrollOnCurrentFrame(modeScrollUp, x, aAO.EraseBehind);

    16 : if (aAO.ProcesingStage < Matrix.Width) then begin
           for x := 0 to aAO.ProcesingStage mod Matrix.Width do
             PerformColumnScrollOnCurrentFrame(modeScrollDown, x, aAO.EraseBehind)
           end
         else
           for x := 0 to Matrix.Width - 1 do
             PerformColumnScrollOnCurrentFrame(modeScrollDown, x, aAO.EraseBehind);

          // parameter1 is scroll count
          // parameter2 is direction: 0 = right, 1 = left
    17 :  begin
            if aAO.Parameter2 = 0 then
              PerformScroll(modeScrollRight, aAO.Layer, FCurrentFrame)
            else
              PerformScroll(modeScrollLeft, aAO.Layer, FCurrentFrame);

            if aAO.Parameter1 = Matrix.Width - 1 then begin
              aAO.Parameter1 := 0;

              if (aAO.Parameter2 = 0) then
                aAO.Parameter2 := 1
              else
                aAO.Parameter2 := 0;
            end
            else
              inc(aAO.Parameter1);
          end;

          // parameter1 is scroll count
          // parameter2 is direction: 0 = right, 1 = left
    18 :  begin
            if aAO.Parameter2 = 0 then
              PerformScroll(modeScrollUp, aAO.Layer, FCurrentFrame)
            else
              PerformScroll(modeScrollDown, aAO.Layer, FCurrentFrame);

            if aAO.Parameter1 = Matrix.Width - 1 then begin
              aAO.Parameter1 := 0;

              if (aAO.Parameter2 = 0) then
                aAO.Parameter2 := 1
              else
                aAO.Parameter2 := 0;
            end
            else
              inc(aAO.Parameter1);
          end;

          // == paste brush in to every frame ==================================
     19 : begin
            for x:= 0 to aAO.Brushes[0].BrushData.Count - 1 do begin
              StringToRow(False, aAO.Brushes[0].BrushData[x], FCurrentFrame, x,
                          aAO.Brushes[0].TransparentColour,
                          aAO.Brushes[0].Transparent);
            end;
          end;

     20 : begin
             if FCurrentFrame = aAO.FrameStart then begin
               for x:= 0 to aAO.Brushes[0].BrushData.Count - 1 do begin
                 StringToRow(False, aAO.Brushes[0].BrushData[x], FCurrentFrame, x,
                             aAO.Brushes[0].TransparentColour,
                             aAO.Brushes[0].Transparent);
               end;
             end;
          end;

     21 : begin
            for x:= 0 to aAO.Brushes[1].BrushData.Count - 1 do begin
              StringToRow(False, aAO.Brushes[1].BrushData[x], FCurrentFrame, x,
                          aAO.Brushes[1].TransparentColour,
                          aAO.Brushes[1].Transparent);
            end;
          end;

     22 : begin
             if FCurrentFrame = aAO.FrameStart then begin
               for x:= 0 to aAO.Brushes[1].BrushData.Count - 1 do begin
                 StringToRow(False, aAO.Brushes[1].BrushData[x], FCurrentFrame, x,
                             aAO.Brushes[1].TransparentColour,
                             aAO.Brushes[1].Transparent);
               end;
             end;
          end;

          // == split scroll
     23 : PerformSplitScroll(modeSplitScrollLeftRight, aAO.Layer, FCurrentFrame);
     24 : PerformSplitScroll(modeSplitScrollRightLeft, aAO.Layer, FCurrentFrame);
     25 : PerformSplitScroll(modeSplitScrollUpDown,    aAO.Layer, FCurrentFrame);
     26 : PerformSplitScroll(modeSplitScrollDownUp,    aAO.Layer, FCurrentFrame);

          // alternate scrolls
     29 : PerformAlternateScroll(modeAlternateScrollUpDown, aAO.Layer, FCurrentFrame);

     30 : PerformRevealOnCurrentFrame(modeRevealLeftRight, aAO.ParameterRevealColour, aAO.ParameterReveal);
     31 : PerformRevealOnCurrentFrame(modeRevealRightLeft, aAO.ParameterRevealColour, aAO.ParameterReveal);
     32 : PerformRevealOnCurrentFrame(modeRevealTopBottom, aAO.ParameterRevealColour, aAO.ParameterReveal);
     33 : PerformRevealOnCurrentFrame(modeRevealBottomTop, aAO.ParameterRevealColour, aAO.ParameterReveal);
     34 : PerformRevealOnCurrentFrame(modeRevealCentreIn,  aAO.ParameterRevealColour, aAO.ParameterReveal);
     35 : PerformRevealOnCurrentFrame(modeRevealCentreOut, aAO.ParameterRevealColour, aAO.ParameterReveal);
  else
    MessageDlg('Error: unknown action ID "' + IntToStr(aActionID) + '".', mtError, [mbOK], 0);
  end;
end;


procedure TTheMatrix.Automate(var aAO : TActionObject);
var
  lFrame, a, i : integer;
  lAction, lIterationCount, lOldLayer : integer;

begin
  FAutomateMode := True;

  // ===========================================================================

  aAO.ProcesingStage := 0;
  aAO.CCSourceIndex  := 0;
  aAO.CCTargetIndex  := 0;
  aAO.CCDirection    := cdForwards;
  lIterationCount    := 1;

  // ===========================================================================

  lOldLayer     := FCurrentLayer;

  FCurrentLayer := aAO.Layer;

  // ===========================================================================

  FBusy := True;

  case aAO.Source of
    acFirstFrame   : begin // first frame is source
                       FCurrentFrame := aAO.FrameStart;

                       for a := 0 to aAO.ActionList.Count - 1 do begin
                         lAction := StrToIntDef(aAO.ActionList[a], -1);

                         if (lAction >= 19) or (lAction <= 22) then
                           AutomationActionExecute(aAO, lAction);
                       end;

                       for lFrame := aAO.FrameStart + 1 to aAO.FrameEnd do begin

                         if lFrame > CurrentFrameCount then
                           InsertBlankFrameAt(lFrame);

                         CopyLayerFromTo(aAO.Layer, aAO.Layer, lFrame - 1, lFrame);

                         FCurrentFrame := lFrame;

                         for a := 0 to aAO.ActionList.Count - 1 do begin
                           lAction := StrToIntDef(aAO.ActionList[a], -1);

                           AutomationActionExecute(aAO, lAction);
                         end;

                         inc(aAO.ProcesingStage);
                       end;
                     end;
    acEachFrame    : begin // previous frame is source
                       for lFrame:= aAO.FrameStart to aAO.FrameEnd do begin

                        if lFrame > CurrentFrameCount then
                          InsertBlankFrameAt(lFrame);

                         FCurrentFrame := lFrame;

                         for a := 0 to aAO.ActionList.Count - 1 do begin
                           lAction := StrToIntDef(aAO.ActionList[a], -1);

                           AutomationActionExecute(aAO, lAction);
                         end;

                         inc(aAO.ProcesingStage);
                       end;
                     end;
    acEachFrameInc : begin
                       for lFrame:= aAO.FrameStart to aAO.FrameEnd do begin

                         if lFrame > CurrentFrameCount then
                           InsertBlankFrameAt(lFrame);

                         FCurrentFrame := lFrame;

                         for a := 0 to aAO.ActionList.Count - 1 do begin
                           lAction := StrToIntDef(aAO.ActionList[a], -1);

                           for i := 1 to lIterationCount do
                             AutomationActionExecute(aAO, lAction);
                         end;

                         inc(aAO.ProcesingStage);
                         inc(lIterationCount);
                       end;
        end;
  end;

  // ===========================================================================

  if aAO.PostProcessList.Count <> 0 then begin

    aAO.TargetSkipIndex := 0;

    for lFrame := aAO.FrameStart to aAO.FrameEnd do begin

      FCurrentFrame := lFrame;

      for a := 0 to aAO.PostProcessList.Count - 1 do begin
        lAction := StrToIntDef(aAO.PostProcessList[a], -1);

        AutomationPostProcessExecute(aAO, lAction);
      end;

      inc(aAO.TargetSkipIndex);

      if (aAO.TargetSkipIndex > aAO.TargetSkip) then
        aAO.TargetSkipIndex := 0;
    end;
  end;

  // ===========================================================================

  FCurrentLayer := lOldLayer;

  FBusy := False;

  // ===========================================================================

  FAutomateMode := False;
end;


// change colours in the current layer of the currently frame only
procedure TTheMatrix.ChangeColourCurrent(aFrom, aTo : integer);
begin
  CopyDrawBufferToCurrentFrame;

  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].ChangePixels(aFrom, aTo);

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


// change colours in all frames of the current layer
procedure TTheMatrix.ChangeColourCurrentLayer(aFrom, aTo : integer);
var
  t : integer;

begin
  CopyDrawBufferToCurrentFrame;

  for t := 0 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do
    MatrixLayers[FCurrentLayer].Frames[t].ChangePixels(aFrom, aTo);

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


// change the colours in all layers and all frames
procedure TTheMatrix.ChangeColourAll(aFrom, aTo : integer);
var
  lLayer, lFrame : integer;

begin
  CopyDrawBufferToCurrentFrame;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for lFrame := 0 to MatrixLayers[lLayer].Frames.Count - 1 do
      MatrixLayers[lLayer].Frames[lFrame].ChangePixels(aFrom, aTo);
  end;

  CopyCurrentFrameToDrawBuffer;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.FloodFill(x, y, aFillColour : integer);
begin
  if (aFillColour <> FDisplayBuffer.Grid[x, y]) then begin

    FBusy := True;

    DoFill(x, y, aFillColour);

    FBusy := False;

    Render.DrawData.Coords[0].X := - 1;

    CopyDrawBufferToCurrentFrame;

    MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].AddToHistory;

    FPaintBox.Invalidate;
  end;
end;


// based on code from here:
// https://stackoverflow.com/questions/53247243/how-should-i-implement-a-flood-fill-function-to-my-c-program
procedure TTheMatrix.DoFill(x, y, aFillColour : integer);
var
  lInitialColour : integer;

begin
  lInitialColour            := FDisplayBuffer.Grid[x, y];

  FDisplayBuffer.Grid[x, y] := aFillColour;

  if (x > 0) and (FDisplayBuffer.Grid[x - 1, y] = lInitialColour) then
    DoFill(x - 1, y, aFillColour);
  if (x + 1 < Matrix.Width) and (FDisplayBuffer.Grid[x + 1, y] = lInitialColour) then
    DoFill(x + 1, y, aFillColour);
  if (y > 0) and (FDisplayBuffer.Grid[x, y - 1] = lInitialColour) then
    DoFill(x, y - 1, aFillColour);
  if (y + 1 < Matrix.Height) and (FDisplayBuffer.Grid[x, y + 1] = lInitialColour) then
    DoFill(x, y + 1, aFillColour);
end;


// =============================================================================


function TTheMatrix.IsThisFrameLocked(aLayer, aFrame : integer): boolean;
begin
  if (IsLayerLocked(aLayer)) or (MatrixLayers[aLayer].Frames[aFrame].Locked) then
    Result := True
  else
    Result := False;
end;


function TTheMatrix.IsLocked: boolean;
begin
  Result := MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Locked;
end;


procedure TTheMatrix.UnLockCurrentFrame;
begin
  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Locked := False;
end;


procedure TTheMatrix.LockCurrentFrame;
begin
  MatrixLayers[FCurrentLayer].Frames[FCurrentFrame].Locked := True;
end;


procedure TTheMatrix.LockUnLockRange(aStart, aEnd : integer; aNewLockStatus : boolean);
var
  lFrame : integer;

begin
  for lFrame := aStart to aEnd do
    MatrixLayers[FCurrentLayer].Frames[lFrame].Locked := aNewLockStatus;
end;


procedure TTheMatrix.UnlockLayer(aLayer : integer);
begin
  MatrixLayers[aLayer].Locked := False;
end;


procedure TTheMatrix.LockLayer(aLayer : integer);
begin

  MatrixLayers[aLayer].Locked := True;
end;


function TTheMatrix.IsLayerLocked(aLayer : integer): boolean;
begin
  Result := MatrixLayers[aLayer].Locked;
end;


// =============================================================================
// =============================================================================


// based on code from this stack overflow question
// https://stackoverflow.com/questions/36444024/how-to-extract-frames-from-this-gif-image-access-violation-in-tgifrenderer-dra
function TTheMatrix.ImportFromGIF(aFileName : string): TImportData;
type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..4095] of TRGBTriple;


var
  lGIF : TGIFImage;
  lTempFrame: TBitmap;
  t, x, y, lHeight, lWidth, lLayer: integer;
  lGR: TGIFRenderer;
  lMatrix : TMatrix;
  scanLine : PRGBTripleArray;


begin
  ClearAllFrames;

  // ===========================================================================

  Result.ImportOk        := True;
  Result.Source          := -1;
  Result.SourceLSB       := -1;
//  Result.SourceDirection := -1;
  Result.MatrixMode      := mtMono;
  Result.RGBImport       := False;
  Result.Preview.Enabled := MatrixMain.PreviewActive;

  // ===========================================================================

  lGIF := TGIFImage.Create;
  try
    try
      lGIF.Animate := FALSE;
      lGIF.LoadFromFile(aFileName);
    except
      Result.ImportOk    := False;
      Result.ErrorString := GLanguageHandler.Text[kErrorWhileLoadingThisGIF];

      Exit;
    end;

    lTempFrame := TBitmap.Create;
    lTempFrame.PixelFormat := pf24Bit;
    lGR        := TGIFRenderer.Create(lGIF);

    lHeight    := lGIF.Height;
    lWidth     := lGIF.Width;

    if (lHeight > 256) or (lWidth > 256) then begin
      Result.ImportOk    := False;
      Result.ErrorString := GLanguageHandler.Text[kGIFDimensionsAreTooLarge] + ' ' + IntToStr(lWidth) + ' x ' + IntToStr(lHeight) + ').';

      Exit;
    end;

    Matrix.Width  := lGIF.Width;
    Matrix.Height := lGIF.Height;

    try
      lTempFrame.SetSize(lGIF.Width, lGIF.Height);

      for t := 0 to lGIF.Images.Count - 1 do begin

        if lGIF.Images[t].Empty then                                            // ignore bad frames
          Continue;

        lGR.Draw(lTempFrame.Canvas, lTempFrame.Canvas.ClipRect);

        for y := 0 to lGIF.Height - 1 do begin
          scanLine := lTempFrame.ScanLine[y];

          for x := 0 to lGIF.Width - 1 do
            MatrixLayers[FCurrentLayer].Frames.Last.Grid[x, y] := (scanline[x].rgbtBlue shl 16) + (scanline[x].rgbtGreen shl 8) + (scanline[x].rgbtred);
        end;

        for lLayer := 0 to MatrixLayers.Count - 1 do begin
          lMatrix := TMatrix.Create(lGIF.Width, lGIF.Height, mtRGB, FRGBBackground);
          MatrixLayers[lLayer].Frames.Add(lMatrix);
        end;

        lGR.NextFrame;
      end;

    finally
     FreeAndNil(lGR);
     FreeAndNil(lTempFrame);
    end;

  finally
    FreeAndNil(lGIF);
  end;


  for lLayer := 0 to MatrixLayers.Count - 1 do
    MatrixLayers[lLayer].Frames.Delete(MatrixLayers[lLayer].Frames.Count - 1);

  FCurrentFrame := 1;

  CopyCurrentFrameToDrawBuffer;

  Result.MatrixMode       := mtRGB;
  Result.NewWidth         := lWidth;
  Result.NewHeight        := lHeight;
  Result.BackgroundColour := FRGBBackground;

  Result.MaxFrames        := MatrixLayers[CPermanentLayer].Frames.Count - 1;
  Result.FontMode         := False;

  Matrix.Available := True;

  MatrixLayerChange;

  FPaintBox.Invalidate;
end;


// if you decide to tweak the export yourself then don't bother with the Embarcadero docs, they are worse
// than useless. open Vcl.Imaging.GIFImg and examine the code to see how things are done!
procedure TTheMatrix.ExportToGIF(aBackground, aPixelSize, aPixelShape, aAnimationSpeed : integer; aFileName : string);
var
  lGIF, lTGI : TGIFImage;
  lTempFrame : TBitmap;
  lFrame, lColumn, lRow : integer;

begin
  lGIF := TGIFImage.Create;

  lGIF.Animate        := True;
  lGIF.AnimateLoop    := glContinously;

  FMatrixMerge := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

  try
    for lFrame := 1 to MatrixLayers[CPermanentLayer].Frames.Count - 1 do begin
      lTempFrame := TBitmap.Create;

      lTempFrame.Width  := Matrix.Width * aPixelSize;
      lTempFrame.Height := Matrix.Height * aPixelSize;

      lTempFrame.Canvas.Brush.Color := aBackground;
      lTempFrame.Canvas.FillRect(Rect(0, 0, lTempFrame.Width, lTempFrame.Height));

      BuildMergedFrame(lFrame, 2);

      if (aPixelSize = 1) then begin
        for lColumn := 0 to Matrix.Width - 1 do begin
          for lRow := 0 to Matrix.Height - 1 do begin
            lTempFrame.Canvas.Pixels[lColumn, lRow] := FMatrixMerge.Grid[lColumn, lRow];
          end;
        end;
      end
      else begin
        for lColumn := 0 to Matrix.Width - 1 do begin
          for lRow := 0 to Matrix.Height - 1 do begin
            lTempFrame.Canvas.Brush.Color := FMatrixMerge.Grid[lColumn, lRow];

            if (aPixelShape = 0) then begin
              lTempFrame.Canvas.FillRect(Rect(lColumn * aPixelSize,
                                              lRow * aPixelSize,
                                             (lColumn * aPixelSize) + aPixelSize,
                                             (lRow * aPixelSize) + aPixelSize));
            end
            else begin
              lTempFrame.Canvas.RoundRect(lColumn * aPixelSize,
                                          lRow * aPixelSize,
                                         (lColumn * aPixelSize) + aPixelSize,
                                         (lRow * aPixelSize) + aPixelSize,
                                          aPixelSize - (Round(aPixelSize / CRoundRectCoeff)),
                                          aPixelSize - (Round(aPixelSize / CRoundRectCoeff)));
            end;
          end;
        end;
      end;

      lTGI := TGIFImage.Create;
      lTGI.Assign(lTempFrame);

      if (aAnimationSpeed <> 0) then
        TGIFGraphicControlExtension.Create(lTGI.Images.Frames[0]).Delay := aAnimationSpeed;

      lGIF.Add(lTGI);

      lTGI.Free;
      lTempFrame.Free;
    end;

    TGIFAppExtNSLoop.Create(lGIF.Images.Frames[0]).Loops := 0;

    lGIF.SaveToFile(aFileName);

  finally
    FreeAndNil(lGIF);
    FreeAndNil(FMatrixMerge);
  end;
end;


// =============================================================================
//
// =============================================================================


procedure TTheMatrix.ScrollBarHorizontalChange(Sender: TObject);
begin
  Render.TopLeft.X     := FScrollHorizontal.Position;
  Render.BottomRight.X := Render.TopLeft.X + Render.ViewWindow.X - 1;

  FPaintBox.Invalidate;
end;


procedure TTheMatrix.ScrollBarVerticalChange(Sender: TObject);
begin
  Render.TopLeft.Y     := FScrollVertical.Position;
  Render.BottomRight.Y := Render.TopLeft.Y + Render.ViewWindow.Y - 1;

  FPaintBox.Invalidate;
end;


// =============================================================================
// add proper layer handling object at some point...
// =============================================================================


function TTheMatrix.AddLayer(aName : string): boolean;
var
  lLayer  : TLayer;
  lMatrix : TMatrix;
  t : integer;

begin
  Result := False;

  if (FSoftwareMode = smAnimation) and (Matrix.Width > 0) and (Matrix.Height > 0) then begin
    FBusy := True;

    lLayer := TLayer.Create(aName);

    MatrixLayers.Add(lLayer);

    for t := 0 to MatrixLayers[CPermanentLayer].Frames.Count - 1 do begin
      lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);
      MatrixLayers.Last.Frames.Add(lMatrix);
    end;

    FBusy := False;

    ChangeCurrentLayer(MatrixLayers.Count - 1);

    MatrixLayerChange;

    Result := True;
  end;
end;


function TTheMatrix.AddLayerAsCopy(aName : string; aCopyLayer : integer): boolean;
var
  lLayer  : TLayer;
  lMatrix : TMatrix;
  t, x, y : integer;

begin
  Result := False;

  if (FSoftwareMode = smAnimation) and (Matrix.Width > 0) and (Matrix.Height > 0) then begin
    FBusy := True;

    lLayer := TLayer.Create(aName);

    MatrixLayers.Add(lLayer);

    for t := 0 to MatrixLayers[CPermanentLayer].Frames.Count - 1 do begin
      lMatrix := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          lMatrix.Grid[x, y] := MatrixLayers[aCopyLayer].Frames[t].Grid[x, y];
        end;
      end;

      MatrixLayers.Last.Frames.Add(lMatrix);
    end;

    FBusy := False;

    ChangeCurrentLayer(MatrixLayers.Count - 1);

    MatrixLayerChange;

    Result := True;
  end;
end;


function TTheMatrix.DeleteLayer(aIndex : integer): boolean;
begin
  Result := False;

  if MatrixLayers.Count > 1 then begin
    CopyDrawBufferToCurrentFrame;

    MatrixLayers.Delete(aIndex);

    FCurrentLayer := 0;

    MatrixLayerChange;

    Result := True;
  end;
end;


procedure TTheMatrix.ClearCurrentLayerAllFrames;
var
  lFrame : integer;

begin
  FDisplayBuffer.Clear(Matrix.Mode, FRGBBackground);

  for lFrame := 0 to MatrixLayers[FCurrentLayer].Frames.Count - 1 do begin
    if not(IsThisFrameLocked(FCurrentLayer, lFrame)) then begin
      MatrixLayers[FCurrentLayer].Frames[lFrame].Clear(Matrix.Mode, FRGBBackground);

      MatrixLayers[FCurrentLayer].Frames[lFrame].AddToHistory;
    end;
  end;

  FPaintBox.Invalidate;

  MatrixChange;
end;


procedure TTheMatrix.FlattenAllLayers;
var
  f, l, x, y : integer;

begin
  CopyDrawBufferToCurrentFrame;

  FBusy := True;

  FMatrixMerge := TMatrix.Create(Matrix.Width, Matrix.Height, Matrix.Mode, FRGBBackground);

  for l := 0 to MatrixLayers.Count - 1 do
    MatrixLayers[l].Visible := True;

  for f := 1 to MatrixLayers[CPermanentLayer].Frames.Count - 1 do begin

    BuildMergedFrame(f, 0);

    for x := 0 to Matrix.Width - 1 do begin
      for y := 0 to Matrix.Height - 1 do begin
        MatrixLayers[CPermanentLayer].Frames[f].Grid[x, y] := FMatrixMerge.Grid[x, y];
      end;
    end;
  end;

  FMatrixMerge.Free;

  while MatrixLayers.Count > 1  do
    MatrixLayers.Delete(1);

  FCurrentFrame := 1;
  FCurrentLayer := 0;

  FBusy := False;

  CopyCurrentFrameToDrawBuffer;

  MatrixLayerChange;
end;


function TTheMatrix.GetLayerCount: integer;
begin
  Result := MatrixLayers.Count;
end;


function TTheMatrix.GetLayerName(aLayerIndex : integer): string;
begin
  Result := MatrixLayers[aLayerIndex].Name;
end;


procedure TTheMatrix.SetLayerName(aLayerIndex : integer; aNewName : string);
begin
  MatrixLayers[aLayerIndex].Name := aNewName;
end;


function TTheMatrix.IsVisible(aLayerIndex : integer): boolean;
begin
  Result := MatrixLayers[aLayerIndex].Visible;
end;


procedure TTheMatrix.SetVisibility(aLayerIndex : integer; aVisibility : boolean);
begin
  MatrixLayers[aLayerIndex].Visible := aVisibility;
end;


procedure TTheMatrix.MoveUp(aLayerIndex : integer);
begin
  if (aLayerIndex = FCurrentFrame) then
    CopyCurrentFrameToDrawBuffer;

  FBusy := True;

  MatrixLayers.Move(aLayerIndex, aLayerIndex + 1);

  FCurrentLayer := aLayerIndex + 1;

  FBusy := False;

  FPaintBox.Invalidate;

  MatrixLayerChange;
end;


procedure TTheMatrix.MoveDown(aLayerIndex : integer);
begin
  if (aLayerIndex = FCurrentFrame) then
    CopyCurrentFrameToDrawBuffer;

  FBusy := True;

  MatrixLayers.Move(aLayerIndex, aLayerIndex - 1);

  FCurrentLayer := aLayerIndex - 1;

  FBusy := False;

  FPaintBox.Invalidate;

  MatrixLayerChange;
end;


// =============================================================================
// =============================================================================


function TTheMatrix.CountColoursFrame: integer;
var
  x, y, lLayer : integer;
  lColours : TList<Integer>;

begin
  lColours := TList<Integer>.Create;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for x := 0 to Matrix.Width - 1 do begin
      for y := 0 to Matrix.Height - 1 do begin
        if (lColours.IndexOf(MatrixLayers[lLayer].Frames[FCurrentFrame].Grid[x, y]) = -1) then
          lColours.Add(MatrixLayers[lLayer].Frames[FCurrentFrame].Grid[x, y]);
      end;
    end;
  end;

  Result := lColours.Count;

  lColours.Free;
end;


function TTheMatrix.CountColoursAnimation: integer;
var
  x, y, lFrame, lLayer : integer;
//  lColours : TList<Integer>;
  lColours : TStringList;

begin
  {lColours := TList<Integer>.Create;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if (lColours.IndexOf(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y]) = -1) then
            lColours.Add(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y]);
        end;
      end;
    end;
  end;    }

  lColours := TStringLIst.Create;
  lColours.Sorted := True;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if (lColours.IndexOf(IntToStr(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y])) = -1) then
            lColours.Add(IntToStr(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y]));
        end;
      end;
    end;
  end;

  lColours.Free;

  Result := lColours.Count;
end;


procedure TTheMatrix.GetFirst32Colours(var aColourList : TStringList);
var
  x, y, lFrame, lLayer : integer;

begin
  aColourList.Duplicates := dupIgnore;
  aColourList.Sorted     := True;

  for lLayer := 0 to MatrixLayers.Count - 1 do begin
    for lFrame := 1 to MatrixLayers[lLayer].Frames.Count - 1 do begin
      for x := 0 to Matrix.Width - 1 do begin
        for y := 0 to Matrix.Height - 1 do begin
          if (aColourList.IndexOf(IntToStr(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y])) = -1) then begin
            aColourList.Add(IntTOStr(MatrixLayers[lLayer].Frames[lFrame].Grid[x, y]));

            if aColourList.Count = 32 then
              Exit;
          end;
        end;
      end;
    end;
  end;
end;


// =============================================================================
// =============================================================================


procedure TTheMatrix.ClearGradient;
begin
  FGradient.Clear;
end;


procedure TTheMatrix.AddGradient(aColour : integer);
begin
  FGradient.Add(aColour);
end;


end.
