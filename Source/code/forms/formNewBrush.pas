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

unit formNewBrush;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Math, System.UITypes,

  utility, xglobal, languagehandler,

  thematrix,

  matrixconstants, exportoptions;

type
  TMatrixSettings = record
    Width        : integer;
    Height       : integer;

    MatrixMode   : TMatrixMode;
  end;

  TNewBrush = record
    Proceed : boolean;

    Width   : integer;
    Height  : integer;
  end;


  TRGBPaletteColours = record
    Left   : integer;
    Middle : integer;
    Right  : integer;

    History : array[0..27] of integer;
  end;

  TfrmNewBrush = class(TForm)
    pMain: TPanel;
    cbAvailableTypes: TComboBox;
    Label1: TLabel;
    bCreate: TButton;
    pColours: TPanel;
    lColours: TLabel;
    cdNewBrush: TColorDialog;
    clbMain: TColorListBox;
    bAddColour: TBitBtn;
    BitBtn1: TBitBtn;
    bOpenColours: TBitBtn;
    bSaveColours: TBitBtn;
    sdSaveBrush: TSaveDialog;
    odLoadBrush: TOpenDialog;
    sbSave: TBitBtn;
    sbCancel: TBitBtn;
    pRGBPalette: TPanel;
    sRGBPaletteColour: TShape;
    sRGBP1: TShape;
    sRGBP2: TShape;
    sRGBP3: TShape;
    sRGBP4: TShape;
    sRGBP5: TShape;
    sRGBP6: TShape;
    sRGBP7: TShape;
    sRGBP8: TShape;
    sRGBP9: TShape;
    sRGBP10: TShape;
    sRGBP11: TShape;
    sRGBP12: TShape;
    sRGBP13: TShape;
    sRGBP14: TShape;
    sRGBP15: TShape;
    sRGBP16: TShape;
    sRGBP17: TShape;
    sRGBP18: TShape;
    sRGBP19: TShape;
    sRGBP20: TShape;
    sRGBP21: TShape;
    lPaletteColourText: TLabel;
    tbRed: TTrackBar;
    eRed: TEdit;
    eGreen: TEdit;
    tbGreen: TTrackBar;
    eBlue: TEdit;
    tbBlue: TTrackBar;
    lPixelColour: TLabel;
    lColoursLeft: TLabel;
    sSelectionLMB: TShape;
    lColoursRight: TLabel;
    sSelectionRMB: TShape;
    lColoursMiddle: TLabel;
    sSelectionMMB: TShape;
    Bevel1: TBevel;
    bSaveBrush: TButton;
    bLoadBrush: TButton;
    bColourUp: TBitBtn;
    bColourDown: TBitBtn;
    pCanvas: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure bAddColourClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bOpenColoursClick(Sender: TObject);
    procedure bSaveColoursClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure sRGBPaletteColourMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbRedChange(Sender: TObject);
    procedure sRGBPaletteColourMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure eRedKeyPress(Sender: TObject; var Key: Char);
    procedure sSelectionLMBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure bSaveBrushClick(Sender: TObject);
    procedure bLoadBrushClick(Sender: TObject);
    procedure bColourUpClick(Sender: TObject);
    procedure bColourDownClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
  private
    FMatrixSettings : TMatrixSettings;
    FMatrixAutomate : TTheMatrix;

    RGBPaletteHistory : array[0..20] of TShape;
    RGBPaletteHistoryIndex : integer;

    procedure SetGUILanguageText;

    procedure SetCaption(aPath : string);

    procedure AddToPaletteHistory(aColour : integer);

    procedure GradientHorizontal(aMode : integer);
    procedure GradientVertical(aMode : integer);
    procedure GradientDiagonal(aMode : integer);
    procedure GenerateChevron(aMode : integer);
    procedure GenerateCheckerboard(aMode : integer);

    procedure SaveColours(aFileName : string);
    procedure LoadColours(aFileName : string);

    function  LoadDataParameterType(s : string; aHeaderMode, aMatrixMode, aColoursMode : boolean): TLoadData;

    procedure LoadBrush(aFileName : string);
    procedure SaveBrush(aFileName : string);
  public
  end;


var
  frmNewBrush: TfrmNewBrush;


const
  CTGradientHorizontalUp      = 0;
  CTGradientHorizontalDown    = 1;
  CTGradientVerticalRight     = 2;
  CTGradientVerticalLeft      = 3;
  CTGradientDiagonalUpRight   = 4;
  CTGradientDiagonalDownRight = 5;
  CTChevronUp                 = 6;
  CTChevronDown               = 7;
  CTChevronRight              = 8;
  CTChevronLeft               = 9;
  CTCheckerboard1x1           = 10;
  CTCheckerboard2x2           = 11;
  CTCheckerboard3x3           = 12;
  CTCheckerboard4x4           = 13;


function DoNewBrush(var aBrushData : TStringList; aNewBrush : TMatrixSettings; aRGBPaletteColours : TRGBPaletteColours): TNewBrush;


implementation


{$R *.dfm}

uses systemsettings;


function DoNewBrush(var aBrushData : TStringList; aNewBrush : TMatrixSettings; aRGBPaletteColours : TRGBPaletteColours): TNewBrush;
var
  t : integer;

begin
  with TfrmNewBrush.Create(Application) do
    try
      FMatrixSettings := aNewBrush;

      Result.Proceed  := False;

      // =======================================================================

      sSelectionLMB.Brush.Color := aRGBPaletteColours.Left;
      sSelectionMMB.Brush.Color := aRGBPaletteColours.Middle;
      sSelectionRMB.Brush.Color := aRGBPaletteColours.Right;

      for t:= 0 to 20 do
        RGBPaletteHistory[t].Brush.Color := aRGBPaletteColours.History[t];

      // =======================================================================

      if FMatrixSettings.MatrixMode <> mtRGB then
        pRGBPalette.Visible := False;

      FMatrixAutomate.NewMatrix(FMatrixSettings.MatrixMode, 1,
                                4, 4,
                                FMatrixSettings.Width, FMatrixSettings.Height,
                                25, psSquare, True, False, True, $00000000);

      if aBrushData.Count <> 0 then begin
        for t := 0 to aBrushData.Count - 1 do begin
          FMatrixAutomate.StringToRow(False, aBrushData[t], 1, t, 0, False);
        end;
      end;

      FMatrixAutomate.SetMouseButtonColours(sSelectionLMB.Brush.Color,
                                            sSelectionMMB.Brush.Color,
                                            sSelectionRMB.Brush.Color);

      // =======================================================================

      FMatrixAutomate.LEDColours[0]   := $00ffffff;
      FMatrixAutomate.LEDColours[1]   := $00000000;

      FMatrixAutomate.Render.DrawData.Colour := 1;
      FMatrixAutomate.RGBBackground   := $00FFFFFF;

      // =======================================================================

      ShowModal;

      // =======================================================================

      if ModalResult = mrOK then begin
        Result.Proceed := True;

        Result.Width   := FMatrixAutomate.RightBounds;
        Result.Height  := FMatrixAutomate.BottomBounds;

        for t := 0 to FMatrixSettings.Height - 1 do
          aBrushData.Add(FMatrixAutomate.RowToString(1, t));
      end;
    finally
      FMatrixAutomate.Free;

      Free;
    end;
end;


procedure TfrmNewBrush.FormCreate(Sender: TObject);
begin
  RGBPaletteHistoryIndex := 0;

  FMatrixAutomate := TTheMatrix.Create(self, pCanvas);
  FMatrixAutomate.OnChange           := Nil;
  FMatrixAutomate.OnColourChange     := Nil;
  FMatrixAutomate.OnMouseOver        := Nil;
  FMatrixAutomate.OnPreviewMouseDown := Nil;

  RGBPaletteHistory[0]  := sRGBP1;  RGBPaletteHistory[1]  := sRGBP2;  RGBPaletteHistory[2]  := sRGBP3;  RGBPaletteHistory[3]  := sRGBP4;
  RGBPaletteHistory[4]  := sRGBP5;  RGBPaletteHistory[5]  := sRGBP6;  RGBPaletteHistory[6]  := sRGBP7;  RGBPaletteHistory[7]  := sRGBP8;
  RGBPaletteHistory[8]  := sRGBP9;  RGBPaletteHistory[9]  := sRGBP10; RGBPaletteHistory[10] := sRGBP11; RGBPaletteHistory[11] := sRGBP12;
  RGBPaletteHistory[12] := sRGBP13; RGBPaletteHistory[13] := sRGBP14; RGBPaletteHistory[14] := sRGBP15; RGBPaletteHistory[15] := sRGBP16;
  RGBPaletteHistory[16] := sRGBP17; RGBPaletteHistory[17] := sRGBP18; RGBPaletteHistory[18] := sRGBP19; RGBPaletteHistory[19] := sRGBP20;
  RGBPaletteHistory[20] := sRGBP21;

  SetGUILanguageText;
end;


procedure TfrmNewBrush.FormShow(Sender: TObject);
begin
  if FMatrixAutomate.Matrix.Mode <> mtRGB then begin
    clbMain.Enabled    := False;
    bAddColour.Enabled := False;
  end;
end;


procedure TfrmNewBrush.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinHeight := 436;
  MinWidth  := 672;
end;


procedure TfrmNewBrush.FormResize(Sender: TObject);
var
  lPixelSize : integer;
  xc, yc : integer;
  pxc, pyc : integer;

begin
  xc := ClientWidth - 70;
  yc := ClientHeight - pMain.Height - 25;

  if pRGBPalette.Visible then xc := xc - pRGBPalette.Width - 10;
  if pColours.Visible    then xc := xc - pColours.Width - 10;

  pxc := Floor(xc / MatrixMain.Matrix.Width);
  pyc := Floor(yc / MatrixMain.Matrix.Height);

  if pxc > pyc then   // use the smallest value
    lPixelSize := pyc
  else
    lPixelSize := pxc;

  FMatrixAutomate.ChangePixelSize(lPixelSize);
end;


procedure TfrmNewBrush.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kNewBrush];

  Label1.Caption := GLanguageHandler.Text[kBuiltInTypes];
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientHorizontalUp]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientHorizontalDown]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientVerticalRight]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientVerticalLeft]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientDiagonalUpRight]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kGradientDiagonalDownRight]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kChevronUp]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kChevronDown]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kChevronRight]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kChevronLeft]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kCheckerboard1x1]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kCheckerboard2x2]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kCheckerboard3x3]);
  cbAvailableTypes.Items.Add(GLanguageHandler.Text[kCheckerboard4x4]);
  cbAvailableTypes.ItemIndex := 0;

  bCreate.Caption := GLanguageHandler.Text[kCreate];

  bLoadBrush.Caption := GLanguageHandler.Text[kOpen];
  bSaveBrush.Caption := GLanguageHandler.Text[kSave];

  lColours.Caption := GLanguageHandler.Text[kColours];
  bOpenColours.Caption := GLanguageHandler.Text[kOpen];
  bSaveColours.Caption := GLanguageHandler.Text[kSave];

  sbSave.Caption := GLanguageHandler.Text[kOK];
  sbCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmNewBrush.bAddColourClick(Sender: TObject);
begin
  if cdNewBrush.Execute then begin
    clbMain.AddItem(GLanguageHandler.Text[kColour], TObject(cdNewBrush.Color));
  end;
end;


procedure TfrmNewBrush.bColourDownClick(Sender: TObject);
var
  lII : integer;

begin
  if clbMain.ItemIndex <> -1 then begin
    if clbMain.ItemIndex < clbMain.Items.Count - 1 then begin
      lII := clbMain.ItemIndex;

      clbMain.Items.Move(clbMain.ItemIndex, clbMain.ItemIndex + 1);

      clbMain.ItemIndex := lII + 1;
    end;
  end;
end;


procedure TfrmNewBrush.bColourUpClick(Sender: TObject);
var
  lII : integer;

begin
  if clbMain.ItemIndex <> -1 then begin
    if clbMain.ItemIndex > 0 then begin
      lII := clbMain.ItemIndex;

      clbMain.Items.Move(clbMain.ItemIndex, clbMain.ItemIndex - 1);

      clbMain.ItemIndex := lII - 1;
    end;
  end;
end;


procedure TfrmNewBrush.SetCaption(aPath : string);
begin
  Caption := GLanguageHandler.Text[kNewBrush];

  if aPath <> '' then
    Caption := Caption + ' "' + aPath + '"';
end;


procedure TfrmNewBrush.BitBtn1Click(Sender: TObject);
begin
  clbMain.DeleteSelected;
end;


procedure TfrmNewBrush.bOpenColoursClick(Sender: TObject);
begin
  odLoadBrush.DefaultExt := 'colours';
  odLoadBrush.Filter     := GLanguageHandler.Text[kColourLists] + ' (*.colours)|*.colours';
  odLoadBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\colours\';

  if odLoadBrush.Execute then begin
    clbMain.Items.Clear;

    LoadColours(odLoadBrush.FileName);
  end;
end;


procedure TfrmNewBrush.bSaveColoursClick(Sender: TObject);
begin
  sdSaveBrush.DefaultExt := 'colours';
  sdSaveBrush.Filter     := GLanguageHandler.Text[kColourLists] + ' (*.colours)|*.colours';
  sdSaveBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\colours\';

  if sdSaveBrush.Execute then begin
    SaveColours(sdSaveBrush.FileName);
  end;
end;


procedure TfrmNewBrush.bLoadBrushClick(Sender: TObject);
begin
  odLoadBrush.DefaultExt := 'leds';
  odLoadBrush.Filter     := GLanguageHandler.Text[kBrushes] + ' (*.leds)|*.leds';
  odLoadBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'brushes\';

  if odLoadBrush.Execute then begin
    LoadBrush(odLoadBrush.FileName);

    SetCaption(odLoadBrush.FileName);
  end;
end;


procedure TfrmNewBrush.bSaveBrushClick(Sender: TObject);
begin
  sdSaveBrush.DefaultExt := 'leds';
  sdSaveBrush.Filter     := GLanguageHandler.Text[kBrushes] + ' (*.leds)|*.leds';
  sdSaveBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'brushes\';

  if sdSaveBrush.Execute then begin
    SaveBrush(sdSaveBrush.FileName);

    SetCaption(sdSaveBrush.FileName);
  end;
end;


procedure TfrmNewBrush.bCreateClick(Sender: TObject);
begin
  if clbMain.Count <> 0 then begin
    case cbAvailableTypes.ItemIndex of
      CTGradientHorizontalUp      : GradientHorizontal(CTGradientHorizontalUp);
      CTGradientHorizontalDown    : GradientHorizontal(CTGradientHorizontalDown);
      CTGradientVerticalRight     : GradientVertical(CTGradientVerticalRight);
      CTGradientVerticalLeft      : GradientVertical(CTGradientVerticalLeft);
      CTGradientDiagonalUpRight   : GradientDiagonal(CTGradientDiagonalUpRight);
      CTGradientDiagonalDownRight : GradientDiagonal(CTGradientDiagonalDownRight);
      CTChevronUp                 : GenerateChevron(CTChevronUp);
      CTChevronDown               : GenerateChevron(CTChevronDown);
      CTChevronRight              : GenerateChevron(CTChevronRight);
      CTChevronLeft               : GenerateChevron(CTChevronLeft);
      CTCheckerboard1x1           : GenerateCheckerboard(CTCheckerboard1x1);
      CTCheckerboard2x2           : GenerateCheckerboard(CTCheckerboard2x2);
      CTCheckerboard3x3           : GenerateCheckerboard(CTCheckerboard3x3);
      CTCheckerboard4x4           : GenerateCheckerboard(CTCheckerboard4x4);
    end;
  end
  else
    MessageDlg(GLanguageHandler.Text[kYouMustAdCcoloursBelowBeforeYouCanGenerateAPattern], mtWarning, [mbOk], 0);
end;


procedure TfrmNewBrush.eRedKeyPress(Sender: TObject; var Key: Char);
var
 s : string;
 lValue : integer;

begin
  if (key = #13) then begin
    s := TEdit(Sender).Text;

    lValue := StrToIntDef(s, 999);

    if (lValue >= 0) and (lValue <= 255) then begin
      case TEdit(Sender).Tag of
        0 : tbRed.Position   := lValue;
        1 : tbGreen.Position := lValue;
        2 : tbBlue.Position  := lValue;
      end;

      tbRedChange(Nil);
    end;
  end;
end;

procedure TfrmNewBrush.GradientHorizontal(aMode : integer);
var
  lColumn, lRow, lColour, lColourIndex : integer;

begin
  lColourIndex := 0;

  for lRow := 0 to FMatrixSettings.Height - 1 do begin

    lColour := TColor(clbMain.Items.Objects[lColourIndex]);

    for lColumn := 0 to FMatrixSettings.Width - 1 do begin
      if aMode = CTGradientHorizontalDown then
        FMatrixAutomate.SetPixel(1, lColumn, lRow, lColour)
      else
        FMatrixAutomate.SetPixel(1, lColumn, FMatrixSettings.Height - 1 - lRow, lColour);
    end;

    if lColourIndex = clbMain.Items.Count - 1 then
      lColourIndex := 0
    else
      inc(lColourIndex);
  end;

  FMatrixAutomate.Refresh;
end;


procedure TfrmNewBrush.GradientVertical(aMode : integer);
var
  lColumn, lRow, lColour, lColourIndex : integer;

begin
  lColourIndex := 0;

  for lColumn := 0 to FMatrixSettings.Width - 1 do begin

    lColour := TColor(clbMain.Items.Objects[lColourIndex]);

    for lRow := 0 to FMatrixSettings.Height - 1 do begin

      if aMode = CTGradientVerticalRight then
        FMatrixAutomate.SetPixel(1, lColumn, lRow, lColour)
      else
        FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - 1 - lColumn, lRow, lColour);
    end;

    if lColourIndex = clbMain.Items.Count - 1 then
      lColourIndex := 0
    else
      inc(lColourIndex);
  end;

  FMatrixAutomate.Refresh;
end;


procedure TfrmNewBrush.GradientDiagonal(aMode : integer);
var
  lColumn, lRow : integer;
  lColour, lColourOffset, lColourIndex : integer;

begin
  lColourOffset := 0;

  for lColumn := 0 to FMatrixSettings.Width - 1 do begin

    lColourIndex := lColourOffset;

    for lRow := FMatrixSettings.Height - 1 downto 0 do begin
      lColour := TColor(clbMain.Items.Objects[lColourIndex]);

      if (aMode = CTGradientDiagonalUpRight) then
        FMatrixAutomate.SetPixel(1, lColumn, lRow, lColour)
      else
        FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - lColumn - 1, lRow, lColour);

      if lColourIndex = clbMain.Items.Count - 1 then
        lColourIndex := 0
      else
        inc(lColourIndex);
    end;

    if lColourOffset = clbMain.Items.Count - 1 then
      lColourOffset := 0
    else
      inc(lColourOffset);
  end;

  FMatrixAutomate.Refresh;
end;


procedure TfrmNewBrush.GenerateChevron(aMode : integer);
var
  lColumn, lRow, lHeight, lWidth, lColour, lColourIndex, lColourOffset : integer;

begin
  lColourOffset := 0;

  if (aMode = CTChevronLeft) or (aMode = CTChevronRight) then begin

    lHeight := Round(FMatrixSettings.Height / 2);

    for lRow := 0 to lHeight - 1 do begin

      lColourIndex := lColourOffset;

      for lColumn := 0 to FMatrixSettings.Width - 1 do begin

        lColour := TColor(clbMain.Items.Objects[lColourIndex]);

        if aMode = CTChevronLeft then begin
          FMatrixAutomate.SetPixel(1, lColumn, lRow, lColour);
          FMatrixAutomate.SetPixel(1, lColumn, FMatrixSettings.Height - 1 - lRow, lColour);
        end
        else begin
          FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - 1 - lColumn, lRow, lColour);
          FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - 1 - lColumn, FMatrixSettings.Height - 1 - lRow, lColour);
        end;

        if lColourIndex = clbMain.Items.Count - 1 then
          lColourIndex := 0
        else
          inc(lColourIndex);
      end;

        if lColourOffset = clbMain.Items.Count - 1 then
          lColourOffset := 0
        else
          inc(lColourOffset);
    end;
  end
  else begin
    lWidth := Round(FMatrixSettings.Width / 2);

    for lColumn := 0 to lWidth - 1 do begin

      lColourIndex := lColourOffset;

      for lRow := 0 to FMatrixSettings.Height - 1 do begin

        lColour := TColor(clbMain.Items.Objects[lColourIndex]);

        if aMode = CTChevronUp then begin
          FMatrixAutomate.SetPixel(1, lColumn,                             lRow, lColour);
          FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - 1 - lColumn, lRow, lColour);
        end
        else begin
          FMatrixAutomate.SetPixel(1, lColumn,                             FMatrixSettings.Height - 1 - lRow, lColour);
          FMatrixAutomate.SetPixel(1, FMatrixSettings.Width - 1 - lColumn, FMatrixSettings.Height - 1 - lRow, lColour);
        end;

        if lColourIndex = clbMain.Items.Count - 1 then
          lColourIndex := 0
        else
          inc(lColourIndex);
      end;

        if lColourOffset = clbMain.Items.Count - 1 then
          lColourOffset := 0
        else
          inc(lColourOffset);
    end;
  end;

  FMatrixAutomate.Refresh;
end;


procedure TfrmNewBrush.GenerateCheckerboard(aMode : integer);
var
  lColumn, lRow, lColourIndex, lColourIndexOld, lCoeff, lSquareX, lSquareY : integer;

begin
  case aMode of
    CTCheckerboard1x1 : lCoeff := 1;
    CTCheckerboard2x2 : lCoeff := 2;
    CTCheckerboard3x3 : lCoeff := 3;
    CTCheckerboard4x4 : lCoeff := 4;
  else
    lCoeff := 1;
  end;

  lSquareX := 0;
  lSquareY := 0;

  lColourIndex := 0;
  lColourIndexOld := 0;

  for lRow := 0 to FMatrixSettings.Height - 1 do begin

    for lColumn := 0 to FMatrixSettings.Width - 1 do begin

       FMatrixAutomate.SetPixel(1, lColumn, lRow, TColor(clbMain.Items.Objects[lColourIndex]));

       inc(lSquareX);

       if (lSquareX = lCoeff) then begin
         if lColourIndex = clbMain.Items.Count - 1 then
           lColourIndex := 0
         else
           inc(lColourIndex);

         lSquareX := 0;
       end;
    end;

    lSquareX := 0;

    inc(lSquareY);

    if (lSquareY = lCoeff) then begin
      if lColourIndexOld = clbMain.Items.Count - 1 then
        lColourIndexOld := 0
      else
        inc(lColourIndexOld);

      lColourIndex := lColourIndexOld;

      lSquareY := 0;
    end
    else
      lColourIndex := lColourIndexOld;
  end;

  FMatrixAutomate.Refresh;
end;


procedure TfrmNewBrush.SaveColours(aFileName : string);
var
  tf : TextFile;
  i : integer;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  for i := 0 to clbMain.Items.Count - 1 do begin
    writeln(tf, 'col:' + IntToStr(TColor(clbMain.Items.Objects[i])));
  end;

  CloseFile(tf);
end;


procedure TfrmNewBrush.sRGBPaletteColourMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    sSelectionLMB.Brush.Color        := TShape(Sender).Brush.Color;

    FMatrixAutomate.LEDRGBColours[1] := TShape(Sender).Brush.Color;
  end
  else if ssMiddle in Shift then begin
    sSelectionMMB.Brush.Color        := TShape(Sender).Brush.Color;

    FMatrixAutomate.LEDRGBColours[2] := TShape(Sender).Brush.Color;
  end
  else if ssRight in Shift then begin
    sSelectionRMB.Brush.Color        := TShape(Sender).Brush.Color;

    FMatrixAutomate.LEDRGBColours[3] := TShape(Sender).Brush.Color;
  end;

  FMatrixAutomate.SetMouseButtonColours(FMatrixAutomate.LEDRGBColours[1],
                                        FMatrixAutomate.LEDRGBColours[2],
                                        FMatrixAutomate.LEDRGBColours[3]);

  if Sender = sRGBPaletteColour then begin
    AddToPaletteHistory(TShape(Sender).Brush.Color);
  end;
end;

procedure TfrmNewBrush.sRGBPaletteColourMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lPixelColour.Caption := '0x' + IntToHex(TUtility.RGBConvertTo(TShape(Sender).Brush.Color, cmRGB, llBottomRight, 100), 6);
end;

procedure TfrmNewBrush.sSelectionLMBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cdNewBrush.Color := TShape(Sender).Brush.Color;

  if cdNewBrush.Execute then begin
    FMatrixAutomate.LEDRGBColours[TShape(Sender).Tag] := cdNewBrush.Color;

    FMatrixAutomate.SetMouseButtonColours(FMatrixAutomate.LEDRGBColours[1],
                                          FMatrixAutomate.LEDRGBColours[2],
                                          FMatrixAutomate.LEDRGBColours[3]);

    TShape(Sender).Brush.Color := cdNewBrush.Color;
  end;
end;

procedure TfrmNewBrush.tbRedChange(Sender: TObject);
begin
  if (Sender <> Nil) then begin
    case TTrackBar(Sender).Tag of
      0 : eRed.Text   := IntToStr(TTrackBar(Sender).Position);
      1 : eGreen.Text := IntToStr(TTrackBar(Sender).Position);
      2 : eBlue.Text  := IntToStr(TTrackBar(Sender).Position);
    end;
  end;

  lPaletteColourText.Caption := LMSSettings.App.HexPrefix +
                                IntToHex(tbRed.Position, 2) +
                                IntToHex(tbGreen.Position, 2) +
                                IntToHex(tbBlue.Position, 2);

  sRGBPaletteColour.Brush.Color := (tbBlue.Position shl 16) +
                                   (tbGreen.Position shl 8) +
                                    tbRed.Position;
end;


procedure TfrmNewBrush.LoadColours(aFileName : string);
var
  tf : TextFile;
  s : string;
  i : integer;

begin
  AssignFile(tf, aFileName);
  Reset(tf);

  while not(eof(tf)) do begin
    readln(tf, s);

    if (pos('$', s) <> 0) then begin
      i := TUtility.HexToInt(UpperCase(copy(s, 6)));
    end
    else
      i := StrToInt(Copy(s, 5));

    clbMain.AddItem(GLanguageHandler.Text[kColour], TObject(i));
  end;

  CloseFile(tf);
end;


procedure TfrmNewBrush.AddToPaletteHistory(aColour : integer);
var
  lCanAdd : boolean;

begin
  // ensures no duplicates adjacent
  if RGBPaletteHistoryIndex = 0 then begin
    lCanAdd := not (aColour = RGBPaletteHistory[20].Brush.Color);
  end
  else
    lCanAdd := not (aColour = RGBPaletteHistory[RGBPaletteHistoryIndex - 1].Brush.Color);

  if lCanAdd then begin
    RGBPaletteHistory[RGBPaletteHistoryIndex].Brush.Color := aColour;

    if RGBPaletteHistoryIndex = 20 then
      RGBPaletteHistoryIndex := 0
    else
      inc(RGBPaletteHistoryIndex);
  end;
end;


function TfrmNewBrush.LoadDataParameterType(s : string; aHeaderMode, aMatrixMode, aColoursMode : boolean): TLoadData;
begin
  Result := ldUnknown;

  if Pos('header', s) <> 0 then
    Result := ldLoadBlockStartHeader
  else if Pos('colours', s) <> 0 then
    Result := ldLoadBlockStartColours
  else if s[1] = '{' then
    Result := ldLoadBlockBegin
  else if s[1] = '}' then
    Result := ldLoadBlockEnd
  else begin
    if aHeaderMode then begin
      case s[1] of
        'a' : Result := ldLoadHeaderSource;
        'b' : Result := ldLoadHeaderSourceLSB;
        'c' : Result := ldLoadHeaderSourceDirection;
        '1' : Result := ldLoadHeaderPadMode;
        '2' : Result := ldLoadHeaderHexFormat;
        '3' : Result := ldLoadHeaderHexOutput;
        '4' : Result := ldLoadHeaderBrackets;
        'd' : Result := ldLoadHeaderDataSource;
        'e' : Result := ldLoadHeaderOrientation;
        'f' : Result := ldLoadHeaderScanDirection;
        'g' : Result := ldLoadHeaderLSB;
        'h' : Result := ldLoadHeaderLanguage;
        'i' : Result := ldLoadHeaderNumberFormat;
        'j' : Result := ldLoadHeaderNumberSize;
        'k' : Result := ldLoadHeaderLineContent;
        'l' : Result := ldLoadHeaderLineCount;
        'm' : Result := ldLoadHeaderRGBMode;
        'n' : Result := ldLoadHeaderRGBChangePixels;
        'o' : Result := ldLoadHeaderRGBChangeColour;
        'p' : Result := ldLoadHeaderOptimise;
        'x' : Result := ldLoadHeaderMatrixComment;
        'z' : Result := ldLoadHeaderRGBBackground;
        'y' : Result := ldLoadHeaderASCIIIndex;
        '}' : Result := ldLoadHeaderEnd;
      end;
    end
    else if aMatrixMode then begin
      case s[1] of
        'w' : Result := ldLoadMatrixWidth;
        'h' : Result := ldLoadMatrixHeight;
        'r' : Result := ldLoadMatrixData;
      end
    end
    else if aColoursMode then begin
      case s[1] of
        'c' : Result := ldLoadColoursCustom;
      end;
    end;
  end;
end;


procedure TfrmNewBrush.LoadBrush(aFileName : string);
var
  tf : TextFile;
  lRow, tempMaxWidth, tempMaxHeight : integer;
  lMatrixMode : TMatrixMode;
  s,v : string;
  headerMode, matrixMode, coloursmode : boolean;
  lValidMatrix : boolean;

begin
  // ===========================================================================
  // ===========================================================================

  clbMain.Clear;

  // ===========================================================================
  // ===========================================================================

  AssignFile(tf, aFileName);
  Reset(tf);

  lRow                   := 0;
  headerMode             := False;
  coloursmode            := False;
  matrixmode             := False;

  lMatrixMode            := mtMono;

  lValidMatrix           := True;

  tempMaxWidth           := -1;
  tempMaxHeight          := -1;

  // ===========================================================================
  // ===========================================================================

  while not(eof(tf)) do begin
    readln(tf, s);

    if s <> '' then begin
      v := Copy(s, 3, length(s) - 2);

      case LoadDataParameterType(LowerCase(s), headermode, matrixmode, coloursmode) of
        ldLoadBlockStartHeader  : headerMode := True;
        ldLoadBlockBegin        : begin
                                    lRow := 0;

                                    case v[length(v)] of
                                      '2' : lMatrixMode := mtBiSequential;
                                      '3' : lMatrixMode := mtBiBitPlanes;
                                      '4' : lMatrixMode := mtRGB;
                                      '5' : lMatrixMode := mtRGB3BPP;
                                    else
                                      lMatrixMode := mtMono;
                                    end;

                                    if lMatrixMode <> FMatrixAutomate.Matrix.Mode then
                                      lValidMatrix := False;

                                    headerMode := False;
                                    matrixMode := True;
                                  end;
        ldLoadBlockEnd          : begin
                                  end;

        ldLoadBlockStartColours : begin
                                    coloursmode := True;
                                    headerMode  := False;
                                    matrixMode  := False;
                                  end;

        ldLoadColoursCustom     : if coloursmode then
                                    clbMain.AddItem(GLanguageHandler.Text[kColour], TObject(StrToInt(v)));

       // ======================================================================

       //30 : MatrixComment           := v;
       //31 : lRGBBackground          := StrToInt(v);
         ldLoadHeaderEnd : {};

       // ======================================================================

         ldLoadMatrixWidth      : tempMaxWidth  := StrToInt(v);
         ldLoadMatrixHeight     : tempMaxHeight := StrToInt(v);
         ldLoadMatrixData       : begin
                                    if lValidMatrix then begin

                                      FMatrixAutomate.StringToRow(False, v, 1, lRow, 0, False);

                                      inc(lRow);
                                    end;
                                  end;

       // ======================================================================

      end;
    end;
  end;

  CloseFile(tf);

  if not(lValidMatrix) then
    MessageDlg(GLanguageHandler.Text[kBrushDoesNotMatchCurrentMatrixType], mtWarning, [mbOK], 0);

//  Result.MatrixMode := MatrixMode;
//  Result.NewWidth   := tempMaxWidth;
//  Result.NewHeight  := tempMaxHeight;
//  Result.MaxFrames  := Matrix.Count - 1;
//  Result.FontMode   := fontmode;
end;


procedure TfrmNewBrush.SaveBrush(aFileName : string);
var
  tf : TextFile;
  y, i : integer;

begin
  AssignFile(tf, aFileName);

  Rewrite(tf);

  // ===========================================================================

  writeln(tf, '{header');
  //writeln(tf, 'x:' + MatrixComment);
  //writeln(tf, 'z:' + IntToStr(RGBBackground));
  writeln(tf, '}');

  // ===========================================================================

  case FMatrixAutomate.Matrix.Mode of
    mtMono         : writeln(tf, '{brush');
    mtBiSequential : writeln(tf, '{brush2');
    mtBiBitPlanes  : writeln(tf, '{brush3');
    mtRGB          : writeln(tf, '{brush4');
    mtRGB3BPP      : writeln(tf, '{brush5');
  end;

  writeln(tf, 'w:' + IntToStr(FMatrixAutomate.Matrix.Width));
  writeln(tf, 'h:' + IntToStr(FMatrixAutomate.Matrix.Height));

  for y := 0 to FMatrixAutomate.Matrix.Height - 1 do begin
    writeln(tf, 'r:' + FMatrixAutomate.RowToString(1, y));
  end;

  writeln(tf, '}');

  // ===========================================================================

  writeln(tf, '{colours');

  for i := 0 to clbMain.Count - 1 do begin
    writeln(tf, 'c:' + IntToStr(TColor(clbMain.Items.Objects[i])));
  end;

  writeln(tf, '}');

  // ===========================================================================

  CloseFile(tf);
end;


end.
