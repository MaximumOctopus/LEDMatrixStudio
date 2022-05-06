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

unit formExport;


interface


uses
  Windows, Messages, System.UITypes, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, utility, xglobal, ComCtrls, thematrix, Clipbrd, richedit,

  exportoptions, exportutility, profilehandler, languagehandler,

  exportoptions_monobi, exportoptions_rgb, exportoptions_rgb_3bpp,

  example_fastLED,

  matrixconstants;


type
  TfrmExport = class(TForm)
    Panel1: TPanel;
    gbProfiles: TGroupBox;
    sbSave: TBitBtn;
    sbOpen: TBitBtn;
    cbProfileList: TComboBox;
    Bevel2: TBevel;
    GroupBox6: TGroupBox;
    bCancel: TBitBtn;
    bExport: TBitBtn;
    cbAutoPreview: TCheckBox;
    sdExport: TSaveDialog;
    sbDelete: TBitBtn;
    bCopyToClipboard: TBitBtn;
    cdExport: TColorDialog;
    bClose: TBitBtn;
    pcExport: TPageControl;
    tsCode: TTabSheet;
    tsBinary: TTabSheet;
    Panel2: TPanel;
    gbSource: TGroupBox;
    sbDataRows: TSpeedButton;
    sbDataColumns: TSpeedButton;
    lFrame: TLabel;
    Label2: TLabel;
    cbDirection: TComboBox;
    cbScanDirection: TComboBox;
    eFrameStart: TEdit;
    eFrameEnd: TEdit;
    cbOptimise: TCheckBox;
    gbLSB: TGroupBox;
    sbLSBLeft: TSpeedButton;
    sbLSBRight: TSpeedButton;
    gbExportFormat: TGroupBox;
    cbLanguageFormat: TComboBox;
    gbNumberFormat: TGroupBox;
    sbNumberDecimal: TSpeedButton;
    sbNumberBinary: TSpeedButton;
    sbNumberHex: TSpeedButton;
    gbNumberGrouping: TGroupBox;
    sbNumberSize8bit: TSpeedButton;
    sbNumberSize16bit: TSpeedButton;
    sbNumberSize32bit: TSpeedButton;
    sbNumberSize8bitSwap: TSpeedButton;
    sbNumberSize16bitSwap: TSpeedButton;
    gbEachLine: TGroupBox;
    sbOutputRow: TSpeedButton;
    sbOutputFrame: TSpeedButton;
    sbOutputBytes: TSpeedButton;
    cbLineCount: TComboBox;
    gbRGB: TGroupBox;
    sbRGB: TSpeedButton;
    sbBGR: TSpeedButton;
    sbGRB: TSpeedButton;
    shapeBackgroundPixels: TShape;
    Label1: TLabel;
    cbChangeBackgroundPixels: TCheckBox;
    gbNumberGroupingRGB: TGroupBox;
    sbNumberSizeRGB8bits: TSpeedButton;
    sbNumberSizeRGB32bits: TSpeedButton;
    Panel3: TPanel;
    gbSourceBinary: TGroupBox;
    sbBinaryDataRows: TSpeedButton;
    sbBinaryDataColumns: TSpeedButton;
    Label3: TLabel;
    Label4: TLabel;
    cbBinaryDirection: TComboBox;
    cbBinaryScanDirection: TComboBox;
    eBinaryFrameStart: TEdit;
    eBinaryFrameEnd: TEdit;
    cbBinaryOptimise: TCheckBox;
    gbLSBBinary: TGroupBox;
    sbBinaryLSBLeft: TSpeedButton;
    sbBinaryLSBRight: TSpeedButton;
    gbNumberGroupingBinary: TGroupBox;
    sbBinaryNumberSize8bit: TSpeedButton;
    sbBinaryNumberSize8bitSwap: TSpeedButton;
    gbBinaryRGB: TGroupBox;
    sbBinaryRGB: TSpeedButton;
    sbBinaryBGR: TSpeedButton;
    sbBinaryGRB: TSpeedButton;
    shapeBinaryBackgroundPixels: TShape;
    Label5: TLabel;
    cbBinaryChangeBackgroundPixels: TCheckBox;
    gbNumberGroupingBinaryRGB: TGroupBox;
    sbBinaryNumberSizeRGB8bits: TSpeedButton;
    mBinary: TMemo;
    gbFileContents: TGroupBox;
    rbSaveAnimation: TRadioButton;
    rbSaveFrame: TRadioButton;
    sbBinaryNumberSize16bitSwap: TSpeedButton;
    sbBRG: TSpeedButton;
    sbBinaryBRG: TSpeedButton;
    bBuildCode: TBitBtn;
    groupBoxRGBBrightness: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    lSelectiveOutput: TLabel;
    eSelectiveStart: TEdit;
    Label9: TLabel;
    eSelectiveEnd: TEdit;
    lBinarySelectiveOutput: TLabel;
    eBinarySelectiveStart: TEdit;
    Label10: TLabel;
    eBinarySelectiveEnd: TEdit;
    Label8: TLabel;
    groupBoxBinaryRGBBrightness: TEdit;
    Label11: TLabel;
    cbIncludeExample: TCheckBox;
    Panel4: TPanel;
    reExport: TRichEdit;
    pPreviewStatus: TPanel;
    procedure sbDataRowsClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
    procedure FormCreate(Sender: TObject);
    procedure cbDirectionChange(Sender: TObject);
    procedure sbLSBLeftClick(Sender: TObject);
    procedure cbLanguageFormatChange(Sender: TObject);
    procedure sbNumberDecimalClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bExportClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure sbOpenClick(Sender: TObject);
    procedure sbNumberSize8bitClick(Sender: TObject);
    function  ValidateNumberEdit(ne : TEdit): boolean;
    procedure eFrameEndExit(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCopyToClipboardClick(Sender: TObject);
    procedure sbRGBClick(Sender: TObject);
    procedure cbOptimiseClick(Sender: TObject);
    procedure shapeBackgroundPixelsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sbBinaryDataRowsClick(Sender: TObject);
    procedure pcExportChange(Sender: TObject);
    procedure bBuildCodeClick(Sender: TObject);
    procedure reExportMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure reExportMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
  private
    FMatrixMode : TMatrixMode;

    eeo : TExportOptions;
    FBuilding : boolean;

    FPixelCount      : integer;
    FPixelCountFrame : integer;
    FMaxFrameValue   : integer;
    profileextension : string;

    FOutput          : TStringList;
    FLastRow         : integer;
    FLastScrollValue : integer;
    FUpdating        : boolean;

    procedure SetGUILanguageText;

    procedure ToggleControlStatus(aNewStatus : boolean);

    procedure CreateExportOptions;
    procedure CreateBinaryExportOptions;

    procedure Preview;
    procedure PreviewCode;
    procedure PreviewBinary;
    procedure UpdatePreview;

    procedure AddPreviewSection;

    procedure GetProfiles;
    procedure LoadProfile(aFileName : string);
    procedure BuildFromProfile(aEEO : TExportOptions);

    function  SaveBinaryData(aFileName : string): boolean;

    procedure AddExampleCode;
  public
    { Public declarations }
  end;


var
  frmExport: TfrmExport;


function ExportData(aInputEO : TExportOptions; aMode : TExportSource; aMatrixMode : TMatrixMode): TExportOptions; // mode = 0 (animation), 1 = (user memories) TO DO (now .exportopions


implementation


{$R *.dfm}


uses optimisation, exportoutputbinary;


function ExportData(aInputEO : TExportOptions; aMode : TExportSource; aMatrixMode : TMatrixMode): TExportOptions; // mode = 0 (animation), 1 = (user memories) TO DO (now .exportopions
begin
  with TfrmExport.Create(Application) do
    try
      FMatrixMode         := aMatrixMode;
      FLastRow            := 0;

      Result.Valid        := False;

      eeo.ExportMode      := aMode;

      if (eeo.ExportMode = esNone) then
        eeo.ExportMode := esAnimation;

      eeo.IncludePreamble := True;

      case FMatrixMode of
        mtMono,
        mtBiSequential,
        mtBiBitPlanes   : begin
                            profileextension                  := 'ledsexport';

                            gbNumberGrouping.Top              := gbNumberGroupingRGB.Top;
                          end;
        mtRGB           : begin
                            gbNumberGrouping.Visible          := False;

                            gbRGB.Visible                     := True;
                            gbRGB.Visible                     := True;

                            gbNumberGroupingRGB.Visible       := True;
                            gbNumberGroupingBinaryRGB.Visible := True;

                            profileextension                  := 'ledsexportrgb';
                          end;
        mtRGB3BPP       : begin
                            gbNumberGrouping.Visible          := False;

                            gbRGB.Visible                           := True;
                            gbRGB.Height                            := 65;                    // hides background change option
                            gbBinaryRGB.Visible               := True;

                            gbNumberGroupingRGB.Visible       := True;
                            gbNumberGroupingBinaryRGB.Visible := True;

                            profileextension                  := 'ledsexportrgb3bpp';
                          end;
      end;

      if aMode = esAnimation then
        FMaxFrameValue := MatrixMain.FrameCount // anim
      else
        FMaxFrameValue := 10;                   // user memories

      eSelectiveStart.Text       := '1';
      eSelectiveEnd.Text         := IntToStr(MatrixMain.Matrix.Height);

      eBinarySelectiveStart.Text := '1';
      eBinarySelectiveEnd.Text   := IntToStr(MatrixMain.Matrix.Height);

      if aInputEO.ExportMode <> esNone then
        BuildFromProfile(aInputEO);

      // =======================================================================

      ShowModal;

      // =======================================================================

      if ModalResult = mrOK then begin
        Result.Valid           := True;

        Result.ExportMode      := aMode;

        Result.StartFrame      := eeo.StartFrame;
        Result.EndFrame        := eeo.EndFrame;        

        Result.Source          := eeo.Source;
        Result.Orientation     := eeo.Orientation;
        Result.ScanDirection   := eeo.ScanDirection;
        Result.LSB             := eeo.LSB;
        Result.Language        := eeo.Language;
        Result.NumberFormat    := eeo.NumberFormat;
        Result.NumberSize      := eeo.NumberSize;
        Result.LineContent     := eeo.LineContent;
        Result.LineCount       := eeo.LineCount;

        Result.RGBEnabled      := eeo.RGBEnabled;
        Result.RGBMode         := eeo.RGBMode;
        Result.RGBChangePixels := eeo.RGBChangePixels;
        Result.RGBChangeColour := eeo.RGBChangeColour;
      end;
    finally
      Free;
    end;
end;


procedure TfrmExport.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;

  FBuilding             := True;
  FOutput               := TStringList.Create;
  FUpdating             := False;
  FLastScrollValue      := 0;

  if eeo.ExportMode = esUserMemories then
    lFrame.Caption := 'User Memories';

  cbAutoPreview.Checked := False;

  sbDataRowsClick(Nil);
  sbBinaryDataRowsClick(Nil);

  cbLanguageFormat.ItemIndex := 0;

  cbLineCount.Items.Add('8');
  cbLineCount.Items.Add('10');
  cbLineCount.Items.Add('16');
  cbLineCount.Items.Add('20');
  cbLineCount.Items.Add('32');
  cbLineCount.Items.Add('40');
  cbLineCount.Items.Add('50');
  cbLineCount.Items.Add('64');
  cbLineCount.Items.Add('100');
  cbLineCount.Items.Add('128');
  cbLineCount.Items.Add('256');

  cbLineCount.ItemIndex := 1;

  cbAutoPreview.Checked := True;
end;


procedure TfrmExport.FormResize(Sender: TObject);
begin
  FLastScrollValue := 0;
end;


procedure TfrmExport.FormShow(Sender: TObject);
begin
  FPixelCount      := ((StrToIntDef(eSelectiveEnd.Text, 1) - StrToIntDef(eSelectiveStart.Text, 1)) + 1) * MatrixMain.Matrix.Height * MatrixMain.Matrix.Width;
  FPixelCountFrame := MatrixMain.Matrix.Height * MatrixMain.Matrix.Width;

  eFrameEnd.Text := IntToStr(FMaxFrameValue);

  GetProfiles;

  FBuilding := False;

  reExport.Lines[10] := StringReplace(reExport.Lines[10], '$X', IntToStr(LMSSettings.App.ExportPreviewSize), [rfReplaceAll]);

  if (FPixelCount <= LMSSettings.App.ExportUpdateMaxPixels) then
    Preview;
end;


procedure TfrmExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOutput.Free;

  Action := caFree;
end;


procedure TfrmExport.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinHeight := 740;
  MinWidth  := 688;
end;


procedure TfrmExport.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kExportMatrixData];

  tsCode.Caption := GLanguageHandler.Text[kCode];

  gbSource.Caption := GLanguageHandler.Text[kSource];
  sbDataRows.Caption := GLanguageHandler.Text[kRows];
  sbDataColumns.Caption := GLanguageHandler.Text[kColumns];
  lFrame.Caption := GLanguageHandler.Text[kFramexs];
  lSelectiveOutput.Caption := GLanguageHandler.Text[kRowxs];
  cbOptimise.Caption := GLanguageHandler.Text[kOptimiseOutputIfPossible];

  gbEachLine.Caption := GLanguageHandler.Text[kEachLineOfOutput];
  sbOutputRow.Caption := GLanguageHandler.Text[kRow];
  sbOutputFrame.Caption := GLanguageHandler.Text[kFrameC];
  sbOutputBytes.Caption := GLanguageHandler.Text[kBytesC];

  gbRGB.Caption := GLanguageHandler.Text[kRGBColourFormat];
  cbChangeBackgroundPixels.Caption := GLanguageHandler.Text[kChangeBackgroundPixels];
  Label1.Caption := GLanguageHandler.Text[kToC];
  Label6.Caption := GLanguageHandler.Text[kBrightness];

  gbLSB.Caption := GLanguageHandler.Text[kLeastSignificantBitLSB];
  sbLSBLeft.Caption := GLanguageHandler.Text[kLeft];
  sbLSBRight.Caption := GLanguageHandler.Text[kRight];

  gbExportFormat.Caption := GLanguageHandler.Text[kExportFormat];
  cbIncludeExample.Caption := GLanguageHandler.Text[kIncludeExampleCode];

  gbNumberFormat.Caption := GLanguageHandler.Text[kNumberFormat];
  sbNumberDecimal.Caption := GLanguageHandler.Text[kDecimal];
  sbNumberBinary.Caption := GLanguageHandler.Text[kBinary];
  sbNumberHex.Caption := GLanguageHandler.Text[kHex];

  gbNumberGroupingRGB.Caption := GLanguageHandler.Text[kNumberGrouping];
  sbNumberSizeRGB8bits.Caption := GLanguageHandler.Text[k8BitsOneBytePerColour];
  sbNumberSizeRGB32bits.Caption := GLanguageHandler.Text[k32Bits];

  gbNumberGrouping.Caption := GLanguageHandler.Text[kNumberGrouping];
  sbNumberSize8bitSwap.Caption := GLanguageHandler.Text[k8BitSwapNybbles];
  sbNumberSize16bitSwap.Caption := GLanguageHandler.Text[k16BitSwapBytes];

  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCommaSeparated]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPICAXEEEPROM]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCpp1Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCpp2Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCppFastLED]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPython1Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPython2Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportMicrochip]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPascal]);

  //

  tsBinary.Caption := GLanguageHandler.Text[kBinary];

  gbSourceBinary.Caption := GLanguageHandler.Text[kBinary];
  sbBinaryDataRows.Caption := GLanguageHandler.Text[kRows];
  sbBinaryDataColumns.Caption := GLanguageHandler.Text[kColumns];
  Label3.Caption := GLanguageHandler.Text[kFramexs];
  lBinarySelectiveOutput.Caption := GLanguageHandler.Text[kRowxs];
  cbBinaryOptimise.Caption := GLanguageHandler.Text[kOptimiseOutputIfPossible];

  gbLSBBinary.Caption := GLanguageHandler.Text[kLeastSignificantBitLSB];
  sbBinaryLSBLeft.Caption := GLanguageHandler.Text[kLeft];
  sbBinaryLSBRight.Caption := GLanguageHandler.Text[kRight];

  gbBinaryRGB.Caption := GLanguageHandler.Text[kRGBColourFormat];
  cbBinaryChangeBackgroundPixels.Caption := GLanguageHandler.Text[kChangeBackgroundPixels];
  Label5.Caption := GLanguageHandler.Text[kToC];
  Label8.Caption := GLanguageHandler.Text[kBrightness];

  gbNumberGroupingBinary.Caption := GLanguageHandler.Text[kNumberGrouping];
  sbBinaryNumberSize8bitSwap.Caption := GLanguageHandler.Text[k8BitSwapNybbles];

  gbFileContents.Caption := GLanguageHandler.Text[kFileContents];
  rbSaveAnimation.Caption := GLanguageHandler.Text[kEntireAnimation];
  rbSaveFrame.Caption := GLanguageHandler.Text[kFrameOnePerFile];

  gbNumberGroupingBinaryRGB.Caption := GLanguageHandler.Text[kNumberGrouping];
  sbBinaryNumberSizeRGB8bits.Caption := GLanguageHandler.Text[k8BitsOneBytePerColour];

  //

  gbProfiles.Caption := GLanguageHandler.Text[kProfiles];

  sbOpen.Caption := GLanguageHandler.Text[kLoad];
  sbSave.Caption := GLanguageHandler.Text[kSave];
  bBuildCode.Caption := GLanguageHandler.Text[kBuildCode];
  cbAutoPreview.Caption := GLanguageHandler.Text[kAutoBuild];

  GroupBox6.Caption := GLanguageHandler.Text[kOutput];

  bExport.Caption := GLanguageHandler.Text[kExport];

  bClose.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmExport.ToggleControlStatus(aNewStatus : boolean);
begin
  pcExport.Enabled         := aNewStatus;

  sbOpen.Enabled           := aNewStatus;
  cbProfileList.Enabled    := aNewStatus;
  sbSave.Enabled           := aNewStatus;

  bBuildCode.Enabled       := aNewStatus;
  cbAutoPreview.Enabled    := aNewStatus;

  bExport.Enabled          := aNewStatus;
  bCopyToClipboard.Enabled := aNewStatus;

  bClose.Enabled           := aNewStatus;
  bCancel.Enabled          := aNewStatus;
end;


procedure TfrmExport.Preview;
begin
  Caption := GLanguageHandler.Text[kExportMatrixData] + ' :: ' + GLanguageHandler.Text[kBuildingDataPleaseWait];

  ToggleControlStatus(False);

  case pcExport.ActivePageIndex of
    0 : PreviewCode;
    1 : PreviewBinary;
  end;

  ToggleControlStatus(True);

  Caption := GLanguageHandler.Text[kExportMatrixData];;
end;


procedure TfrmExport.PreviewCode;
var
  endframelimit, entrycount : integer;
  lUnique : TStringList;

  procedure ClearForRetry;
   begin
    eeo.IncludePreamble := True;
    eeo.CleanMode       := False;

    FOutput.Clear;
    lUnique.Clear;
  end;

begin
  if ValidateNumberEdit(eFrameStart) and ValidateNumberEdit(eFrameEnd) then begin

    CreateExportOptions;

    if eeo.ExportMode = esAnimation then
      endframelimit := MatrixMain.FrameCount
    else
      endframelimit := 9;

    if (eeo.StartFrame <= eeo.EndFrame) and (eeo.EndFrame <= endframelimit) and (eeo.StartFrame >= 1) then begin

      lUnique := TStringList.Create;
      FOutput.Clear;

      if (MatrixMain.SoftwareMode = smAnimation) then
        eeo.FontMode := False
      else
        eeo.FontMode := True;

      case FMatrixMode of
        mtMono,
        mtBiSequential,
        mtBiBitPlanes   : if cbOptimise.Checked then begin
                            TExportMonoBi.CreateExportAnimation(eeo, FOutput, entrycount, lUnique);

                            if not OptimiseData(eeo, FOutput) then begin
                              ClearForRetry;

                              TExportMonoBi.CreateExportAnimation(eeo, FOutput, entrycount, lUnique);
                            end;
                          end
                          else
                            TExportMonoBi.CreateExportAnimation(eeo, FOutput, entrycount, lUnique);
        mtRGB           : if cbOptimise.Checked then begin
                            TExportRGB.CreateExportAnimationRGB(eeo, FOutput, entrycount, lUnique);

                            if not OptimiseData(eeo, FOutput) then begin
                              ClearForRetry;

                              TExportRGB.CreateExportAnimationRGB(eeo, FOutput, entrycount, lUnique);
                            end;
                          end
                          else
                            TExportRGB.CreateExportAnimationRGB(eeo, FOutput, entrycount, lUnique);
        mtRGB3BPP       : if cbOptimise.Checked then begin
                            TExportRGB3BPP.CreateExportAnimationRGB3BPP(eeo, FOutput, entrycount, lUnique);

                            if not OptimiseData(eeo, FOutput) then begin
                              ClearForRetry;

                              TExportRGB3BPP.CreateExportAnimationRGB3BPP(eeo, FOutput, entrycount, lUnique);
                            end;
                          end
                          else
                            TExportRGB3BPP.CreateExportAnimationRGB3BPP(eeo, FOutput, entrycount, lUnique);
      end;

      // =======================================================================

      UpdatePreview;

      // =======================================================================

      lUnique.Free;
    end
    else begin
      eFrameStart.Color := clFuchsia;
      eFrameEnd.Color   := clFuchsia;
    end;
  end;
end;


procedure TfrmExport.reExportMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  lRect : TRect;
  lLastLineIndex, lLastVisibleLine : integer;

begin
  if not(FUpdating) then begin

    reExport.perform(EM_GETRECT, 0, longint(@lRect));
    lRect.left       := lRect.left + 1;
    lRect.top        := lRect.bottom - 2;
    lLastLineIndex   := reExport.perform(EM_CHARFROMPOS, 0, integer(@lRect.topleft));
    lLastVisibleLine := reExport.perform(EM_EXLINEFROMCHAR, 0, lLastLineIndex);

    if (FLastScrollValue <> lLastVisibleLine) and (lLastVisibleLine >= reExport.Lines.Count) then
      AddPreviewSection;

    FLastScrollValue := lLastVisibleLine;
  end;
end;


procedure TfrmExport.reExportMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FLastScrollValue := 0;
end;


procedure TfrmExport.UpdatePreview;
var
  lRow : integer;

begin
  FUpdating := True;

  if (LMSSettings.App.ExportPreviewSize <> 0) then begin

    reExport.Lines.Clear;
    reExport.Lines.BeginUpdate;

    if (LMSSettings.App.ExportPreviewSize > 0) then begin
      lRow := 0;

      while (lRow < LMSSettings.App.ExportPreviewSize) and (lRow < FOutput.Count) do begin
        reExport.Lines.Add(FOutput[lRow]);

        inc(lRow);
      end;

      FLastRow := lRow;

      pPreviewStatus.Caption := IntToStr(lRow) + ' of ' + IntToStr(FOutput.Count);
    end
    else begin
      reExport.Lines.Text := FOutput.Text;

      pPreviewStatus.Caption := IntToStr(FOutput.Count) + ' of ' + IntToStr(FOutput.Count);
    end;

    reExport.Lines.EndUpdate;

    if eeo.Examples then
      AddExampleCode;

  end;

  FUpdating := False;
end;


procedure TfrmExport.AddPreviewSection;
var
  lRow, x : integer;
  s : string;

begin
  s := Caption;

  Caption := Caption + ' :: updating preview ::';
  pPreviewStatus.Caption := 'Updating...';

  reExport.Lines.BeginUpdate;

  lRow := FLastRow;
  x    := 0;

  while (x < LMSSettings.App.ExportPreviewSize) and (lRow + x < FOutput.Count) do begin
    reExport.Lines.Add(FOutput[lRow + x]);

    inc(x);
  end;

  FLastRow := lRow + x;

  pPreviewStatus.Caption := IntToStr(FLastRow) + ' of ' + IntToStr(FOutput.Count);

  reExport.Lines.EndUpdate;

  Caption := s;
end;


procedure TfrmExport.PreviewBinary;
var
  endframelimit, entrycount : integer;
  lUnique : TStringList;
  lOutput : TStringList;
  t : integer;

  procedure ClearForRetry;
   begin
    eeo.IncludePreamble := True;
    eeo.CleanMode       := False;

    lOutput.Clear;
    lUnique.Clear;
  end;

begin
  if ValidateNumberEdit(eBinaryFrameStart) and ValidateNumberEdit(eBinaryFrameEnd) then begin

    CreateBinaryExportOptions;

    if eeo.ExportMode = esAnimation then
      endframelimit := MatrixMain.FrameCount
    else
      endframelimit := 9;

    if (eeo.StartFrame <= eeo.EndFrame) and (eeo.EndFrame <= endframelimit) and (eeo.StartFrame >= 1) then begin

      lUnique := TStringList.Create;
      lOutput := TStringList.Create;

      if (MatrixMain.SoftwareMode = smAnimation) then
        eeo.FontMode := False
      else
        eeo.FontMode := True;

      if gbRGB.Visible then begin
        if cbBinaryOptimise.Checked then begin
          BinaryCreateExportAnimationRGB(eeo, lOutput, entrycount, lUnique);

          if not OptimiseData(eeo, lOutput) then begin
            ClearForRetry;

            BinaryCreateExportAnimationRGB(eeo, lOutput, entrycount, lUnique);
          end;
        end
        else
          BinaryCreateExportAnimationRGB(eeo, lOutput, entrycount, lUnique);
      end
      else begin
        if cbOptimise.Checked then begin
          BinaryCreateExportAnimation(eeo, lOutput, entrycount, lUnique);

          if not OptimiseData(eeo, lOutput) then begin
            ClearForRetry;

            BinaryCreateExportAnimation(eeo, lOutput, entrycount, lUnique);
          end;
        end
        else
          BinaryCreateExportAnimation(eeo, lOutput, entrycount, lUnique);
      end;

      // =======================================================================

      mBinary.Lines.Clear;

      mBinary.Lines.BeginUpdate;
      for t:= 0 to lOutput.Count - 1 do
        mBinary.Lines.Add(lOutput[t]);

      mBinary.Lines.EndUpdate;

      // =======================================================================

      tsBinary.Caption := GLanguageHandler.Text[kBinary] + ' (' + IntToStr(entrycount) + ' ' + GLanguageHandler.Text[kBytes];

      lOutput.Free;
      lUnique.Free;
    end
    else begin
      eFrameStart.Color := clFuchsia;
      eFrameEnd.Color   := clFuchsia;
    end;
  end;
end;


procedure TfrmExport.bCopyToClipboardClick(Sender: TObject);
begin
  case pcExport.ActivePageIndex of
    0 : begin
          Preview;

          Clipboard.AsText := FOutput.Text;

          //Memo1.SelectAll;
          //Memo1.CopyToClipboard;
        end;
    1 : begin
          PreviewBinary;

          mBinary.SelectAll;
          mBinary.CopyToClipboard;
        end;
  end;
end;


procedure TfrmExport.bExportClick(Sender: TObject);
begin
  case pcExport.ActivePageIndex of
    0 : begin
          sdExport.Filter     := 'C/C++ header file (.h)|*.h|Include file (.inc)|*.inc|Python file (.py)|*.py';
          sdExport.DefaultExt := '.h';

          if sdExport.Execute then begin
            Preview;

            FOutput.SaveToFile(sdExport.Filename);

            ModalResult := mrOK;
          end;
        end;
    1 : begin
          sdExport.Filter     := 'Binary file (.bin)|*.bin|Include file (.inc)|*.inc|Data file (.dat)|*.dat';
          sdExport.DefaultExt := '.bin';

          if sdExport.Execute then begin
            PreviewBinary;

            if SaveBinaryData(sdExport.FileName) then begin
              ModalResult := mrOK;
            end
            else
              MessageDlg('Error Saving Binary Data', mtError, [mbOK], 0);
          end;
        end;
  end;
end;


procedure TfrmExport.bBuildCodeClick(Sender: TObject);
begin
  if not(FBuilding) then
    Preview;
end;


procedure TfrmExport.cbDirectionChange(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.cbLanguageFormatChange(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then begin
    Preview;
  end;
end;


procedure TfrmExport.cbOptimiseClick(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.CreateExportOptions;
var
  lSS, lSE : integer;

begin

  // ===========================================================================

  if cbOptimise.Checked then begin
    eeo.IncludePreamble := False;
    eeo.CleanMode       := True;
  end
  else begin
    eeo.IncludePreamble := True;
    eeo.CleanMode       := False;
  end;

  // ===========================================================================

  if eeo.ExportMode = esAnimation then begin
    eeo.StartFrame := StrToInt(eFrameStart.Text);
    eeo.EndFrame   := StrToInt(eFrameEnd.Text);
  end
  else begin
    eeo.StartFrame := StrToInt(eFrameStart.Text);
    eeo.EndFrame   := StrToInt(eFrameEnd.Text);
  end;

  // ===========================================================================

  lSS := StrToIntDef(eSelectiveStart.Text, 1);

  if sbDataRows.Down then begin
    lSE := StrToIntDef(eSelectiveEnd.Text, MatrixMain.Matrix.Height);

    if (lSE < 1) or (LSE > MatrixMain.Matrix.Height) then
      lSE := MatrixMain.Matrix.Height;
  end
  else begin
    lSE := StrToIntDef(eSelectiveEnd.Text, MatrixMain.Matrix.Width);

    if (lSE < 1) or (LSE > MatrixMain.Matrix.Width) then
      lSE := MatrixMain.Matrix.Width;
  end;

  eeo.SelectiveStart := lSS;
  eeo.SelectiveEnd   := lSE;

  // ===========================================================================

  if sbDataRows.Down then
    eeo.Source := rsRows
  else
    eeo.Source := rsColumns;

  // ===========================================================================

  eeo.orientation   := TInputOrientation(cbDirection.ItemIndex);

  // ===========================================================================

  eeo.ScanDirection := cbScanDirection.ItemIndex;

  // ===========================================================================

  if sbLSBLeft.Down then
    eeo.LSB := llTopLeft
  else
    eeo.LSB := llBottomRight;

  // ===========================================================================

  eeo.Language := TExportLanguage(cbLanguageFormat.ItemIndex);

  eeo.Examples := cbIncludeExample.Checked;

  // ===========================================================================

  if gbNumberFormat.Visible then begin
    if sbNumberDecimal.Down then
      eeo.NumberFormat := nfDecimal
    else if sbNumberBinary.Down then
      eeo.NumberFormat := nfBinary
    else
      eeo.NumberFormat := nfHex;
  end
  else
    eeo.NumberFormat := nfHex;

  // ===========================================================================

  if gbNumberGrouping.Visible then begin
    if sbNumberSize8bit.Down then
      eeo.NumberSize := ns8Bit
    else if sbNumberSize16bit.Down then
      eeo.NumberSize := ns16bit
    else if sbNumberSize32bit.Down then
      eeo.NumberSize := ns32bit
    else if sbNumberSize8bitSwap.Down then
      eeo.NumberSize := ns8bitSwap
    else if sbNumberSize16bitSwap.Down then
      eeo.NumberSize := ns16bitSwap;
  end
  else begin
    if sbNumberSizeRGB8bits.Down then
      eeo.NumberSize := nsRGB8bit
    else if sbNumberSizeRGB32bits.Down then
      eeo.NumberSize := nsRGB32bit
    else
      eeo.NumberSize := nsRGB32bit;
  end;

  // ===========================================================================

  if sbOutputRow.Down then
    eeo.LineContent := lcRowCol
  else if sbOutputFrame.Down then
    eeo.LineContent := lcFrame
  else if sbOutputBytes.Down then
    eeo.LineContent := lcBytes;

  // ===========================================================================

  if gbRGB.Visible then begin
    eeo.RGBEnabled := True;

    if sbRGB.Down then
      eeo.RGBMode := cmRGB
    else if sbBGR.Down then
      eeo.RGBMode := cmBGR
    else if sbGRB.Down then
      eeo.RGBMode := cmGRB
    else if sbBRG.Down then
      eeo.RGBMode := cmBRG;

    eeo.RGBChangePixels := cbChangeBackgroundPixels.Checked;
    eeo.RGBChangeColour := shapeBackgroundPixels.Brush.Color;

    eeo.RGBBrightness   := StrToIntDef(groupBoxRGBBrightness.Text, 100);
  end
  else
    eeo.RGBEnabled := False;

  // ===========================================================================

  eeo.LineCount := StrToInt(cbLineCount.Text);

  // ===========================================================================
  //   binary file specific options
  // ===========================================================================

  if sbBinaryDataRows.Down then
    eeo.BinarySource := rsRows
  else
    eeo.BinarySource := rsColumns;

  // ===========================================================================

  eeo.BinaryOrientation   := TInputOrientation(cbBinaryDirection.ItemIndex);

  // ===========================================================================

  eeo.BinaryScanDirection := cbBinaryScanDirection.ItemIndex;

  // ===========================================================================

  if (sbBinaryLSBLeft.Down) then
    eeo.BinaryLSB := llTopLeft
  else
    eeo.BinaryLSB := llBottomRight;

  // ===========================================================================

  if gbBinaryRGB.Visible then begin
    if sbBinaryRGB.Down then
      eeo.BinaryRGBMode := cmRGB
    else if sbBGR.Down then
      eeo.BinaryRGBMode := cmBGR
    else if sbGRB.Down then
      eeo.BinaryRGBMode := cmGRB
    else if sbBRG.Down then
      eeo.BinaryRGBMode := cmBRG;

    eeo.BinaryRGBChangePixels := cbBinaryChangeBackgroundPixels.Checked;
    eeo.BinaryRGBChangeColour := shapeBinaryBackgroundPixels.Brush.Color;

    eeo.BinaryRGBBrightness   := StrToIntDef(groupBoxBinaryRGBBrightness.Text, 100);
  end;

  // ===========================================================================

  eeo.BinaryNumberSize := nsRGB8bit;

  // ===========================================================================

  if (rbSaveAnimation.Checked) then
    eeo.BinaryFileContents := bfEntireAnimation
  else
    eeo.BinaryFileContents := bfSingleFrame;
end;


procedure TfrmExport.CreateBinaryExportOptions;
var
  lSS, lSE : integer;

begin
//  eeo.Language     := -1; // none
  eeo.LineContent  := lcFrame;  // process in frames
  eeo.NumberFormat := nfHex;             // always in hex format

  // ===========================================================================

  if cbBinaryOptimise.Checked then begin
    eeo.IncludePreamble := False;
    eeo.CleanMode       := True;
  end
  else begin
    eeo.IncludePreamble := True;
    eeo.CleanMode       := False;
  end;

  // ===========================================================================

  if eeo.ExportMode = esAnimation then begin
    eeo.StartFrame := StrToInt(eFrameStart.Text);
    eeo.EndFrame   := StrToInt(eFrameEnd.Text);
  end
  else begin
    eeo.StartFrame := StrToInt(eFrameStart.Text);
    eeo.EndFrame   := StrToInt(eFrameEnd.Text);
  end;

  // ===========================================================================

  lSS := StrToIntDef(eBinarySelectiveStart.Text, 1);

  if sbDataRows.Down then begin
    lSE := StrToIntDef(eBinarySelectiveEnd.Text, MatrixMain.Matrix.Height);

    if (lSE < 1) or (LSE > MatrixMain.Matrix.Height) then
      lSE := MatrixMain.Matrix.Height;
  end
  else begin
    lSE := StrToIntDef(eBinarySelectiveEnd.Text, MatrixMain.Matrix.Width);

    if (lSE < 1) or (LSE > MatrixMain.Matrix.Width) then
      lSE := MatrixMain.Matrix.Width;
  end;

  eeo.SelectiveStart := lSS;
  eeo.SelectiveEnd   := lSE;

  // ===========================================================================  

  if sbBinaryDataRows.Down then
    eeo.Source := rsRows
  else
    eeo.Source := rsColumns;

  // ===========================================================================

  eeo.orientation   := TInputOrientation(cbBinaryDirection.ItemIndex);

  // ===========================================================================

  eeo.ScanDirection := cbBinaryScanDirection.ItemIndex;

  // ===========================================================================

  if sbBinaryLSBLeft.Down then
    eeo.LSB := llTopLeft
  else
    eeo.LSB := llBottomRight;

  // ===========================================================================

  if gbNumberGrouping.Visible then begin
    if sbBinaryNumberSize8bit.Down then
      eeo.NumberSize := ns8Bit
    else if sbBinaryNumberSize8bitSwap.Down then
      eeo.NumberSize := ns8bitSwap
    else if sbBinaryNumberSize16bitSwap.Down then
      eeo.NumberSize := ns16bitSwap;
  end
  else begin
    eeo.NumberSize := nsRGB8bit
  end;

  // ===========================================================================

  if gbRGB.Visible then begin
    eeo.RGBEnabled := True;

    if sbBinaryRGB.Down then
      eeo.RGBMode := cmRGB
    else if sbBinaryBGR.Down then
      eeo.RGBMode := cmBGR
    else if sbBinaryGRB.Down then
      eeo.RGBMode := cmGRB
    else if sbBinaryBRG.Down then
      eeo.RGBMode := cmBRG;

    eeo.RGBChangePixels := cbBinaryChangeBackgroundPixels.Checked;
    eeo.RGBChangeColour := shapeBinaryBackgroundPixels.Brush.Color;

    eeo.RGBBrightness   := StrToIntDef(groupBoxBinaryRGBBrightness.Text, 100);
  end
  else
    eeo.RGBEnabled := False;
end;


procedure TfrmExport.eFrameEndExit(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.BuildFromProfile(aEEO : TExportOptions);
var
  t : integer;

begin
  // ===========================================================================

  if aEEO.Source = rsRows then
    sbDataRows.Down    := True
  else
    sbDataColumns.Down := True;

  sbDataRowsClick(niL);

  cbOptimise.Checked := aEEO.Optimise;

  // ===========================================================================

  if aEEO.LSB = llTopLeft then
    sbLSBLeft.Down  := True
  else
    sbLSBRight.Down := True;

  sbLSBLeftClick(Nil);

  // ===========================================================================

  cbLanguageFormat.ItemIndex := Ord(aEEO.Language);

  cbIncludeExample.Checked   := aEEO.Examples;

  // ===========================================================================

  if gbNumberFormat.Visible then begin
    case aEEO.NumberFormat of
      nfDecimal : sbNumberDecimal.Down := True;
      nfBinary  : sbNumberBinary.Down  := True;
      nfHex     : sbNumberHex.Down     := True;
    end;
  end;

  // ===========================================================================

  if gbNumberGrouping.Visible then begin
    case aEEO.NumberSize of
      ns8Bit      : sbNumberSize8bit.Down      := True;
      ns16bit     : sbNumberSize16bit.Down     := True;
      ns32bit     : sbNumberSize32bit.Down     := True;
      ns8bitSwap  : sbNumberSize8bitSwap.Down  := True;
      ns16bitSwap : sbNumberSize16bitSwap.Down := True;
    end;

    sbNumberSize8bitClick(Nil);
  end
  else begin
    case aEEO.NumberSize of
      nsRGB8bit  : sbNumberSizeRGB8bits.Down  := True;
      nsRGB32bit : sbNumberSizeRGB32bits.Down := True;
    end;

    sbNumberSize8bitClick(Nil);
  end;

  // ===========================================================================

  case aEEO.LineContent of
    lcRowCol : sbOutputRow.Down   := True;
    lcFrame  : sbOutputFrame.Down := True;
    lcBytes  : sbOutputBytes.Down := True;
  end;

  sbDataRowsClick(Nil);

  // ===========================================================================

  for t := 0 to cbLineCount.Items.Count - 1 do begin
    if cbLineCount.Items[t] = IntToStr(eeo.LineCount) then
      cbLineCount.ItemIndex := t;
  end;

  // ===========================================================================

  if gbRGB.Visible then begin
    case aEEO.RGBMode of
      cmRGB : sbRGB.Down := True;
      cmBGR : sbBGR.Down := True;
      cmGRB : sbGRB.Down := True;
      cmBRG : sbBRG.Down := True;
    end;

    cbChangeBackgroundPixels.Checked  := aEEO.RGBChangePixels;
    shapeBackgroundPixels.Brush.Color := aEEO.RGBChangeColour;

    if (aEEO.RGBBrightness > 100) then
      aEEO.RGBBrightness := 100;

    groupBoxRGBBrightness.Text        := IntToStr(aEEO.RGBBrightness);
  end;

  // ===========================================================================

  cbDirection.ItemIndex     := Ord(aEEO.orientation);

  // ===========================================================================

  cbScanDirection.ItemIndex := aEEO.ScanDirection;

  // ===========================================================================

  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbBinaryDataRowsClick(Sender: TObject);
begin
  cbBinaryDirection.Clear;
  cbBinaryScanDirection.Clear;

  if sbBinaryDataRows.Down then begin
    cbBinaryDirection.Items.Add(GLanguageHandler.Text[kTopToBottom]);
    cbBinaryDirection.Items.Add(GLanguageHandler.Text[kBottomToTop]);

    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kLeftToRight]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kRightToLeft]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kAlternateLeftRight]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kAlternateRightLeft]);

    sbBinaryLSBLeft.Caption        := GLanguageHandler.Text[kLeft];
    sbBinaryLSBRight.Caption       := GLanguageHandler.Text[kRight];

    lBinarySelectiveOutput.Caption := GLanguageHandler.Text[kRowxs];
  end
  else begin
    cbBinaryDirection.Items.Add(GLanguageHandler.Text[kLeftToRight]);
    cbBinaryDirection.Items.Add(GLanguageHandler.Text[kRightToLeft]);
    cbBinaryDirection.Items.Add(GLanguageHandler.Text[kSure24x16]);

    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kTopToBottom]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kBottomToTop]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kAlternateDownUp]);
    cbBinaryScanDirection.Items.Add(GLanguageHandler.Text[kAlternateUpDown]);

    sbBinaryLSBLeft.Caption        := GLanguageHandler.Text[kTop];
    sbBinaryLSBRight.Caption       := GLanguageHandler.Text[kBottom];

    lBinarySelectiveOutput.Caption := GLanguageHandler.Text[kColumnxs];
  end;

  cbBinaryDirection.ItemIndex     := 0;
  cbBinaryScanDirection.ItemIndex := 0;

  if (cbAutoPreview.Checked) and not(FBuilding) then
    PreviewBinary;
end;


procedure TfrmExport.sbDataRowsClick(Sender: TObject);
var
  oldIndexD, oldIndexS : integer;

begin
  oldIndexD := cbDirection.ItemIndex;
  oldIndexS := cbScanDirection.ItemIndex;

  cbDirection.Clear;
  cbScanDirection.Clear;

  if sbDataRows.Down then begin
    cbDirection.Items.Add(GLanguageHandler.Text[kTopToBottom]);
    cbDirection.Items.Add(GLanguageHandler.Text[kBottomToTop]);

    cbScanDirection.Items.Add(GLanguageHandler.Text[kLeftToRight]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kRightToLeft]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kAlternateLeftRight]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kAlternateRightLeft]);

    sbLSBLeft.Caption        := GLanguageHandler.Text[kLeft];
    sbLSBRight.Caption       := GLanguageHandler.Text[kRight];

    sbOutputRow.Caption      := GLanguageHandler.Text[kRow];

    lSelectiveOutput.Caption := GLanguageHandler.Text[kRowxs];

    eSelectiveEnd.Text       := IntToStr(MatrixMain.Matrix.Height);
  end
  else begin
    cbDirection.Items.Add(GLanguageHandler.Text[kLeftToRight]);
    cbDirection.Items.Add(GLanguageHandler.Text[kRightToLeft]);
    cbDirection.Items.Add(GLanguageHandler.Text[kSure24x16]);

    cbScanDirection.Items.Add(GLanguageHandler.Text[kTopToBottom]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kBottomToTop]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kAlternateDownUp]);
    cbScanDirection.Items.Add(GLanguageHandler.Text[kAlternateUpDown]);

    sbLSBLeft.Caption        := GLanguageHandler.Text[kTop];
    sbLSBRight.Caption       := GLanguageHandler.Text[kBottom];

    sbOutputRow.Caption      := GLanguageHandler.Text[kColumn];

    lSelectiveOutput.Caption := GLanguageHandler.Text[kColumnxs];

    eSelectiveEnd.Text       := IntToStr(MatrixMain.Matrix.Width);
  end;

  if (Sender <> Nil) then
  begin
    if (TSpeedButton(Sender).Tag = 1) then begin
      cbDirection.ItemIndex     := oldIndexD;
      cbScanDirection.ItemIndex := oldIndexS;
    end
    else begin
      cbDirection.ItemIndex     := 0;
      cbScanDirection.ItemIndex := 0;
    end;
  end
  else begin
    cbDirection.ItemIndex     := 0;
    cbScanDirection.ItemIndex := 0;
  end;

  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbDeleteClick(Sender: TObject);
begin
  if cbProfileList.Enabled then begin
    if MessageDlg(GLanguageHandler.Text[kReallyDeleteThisProfile] + #13#13 +
                  '"' + cbProfileList.Text + '"', mtWarning, [mbYes, mbNo], 0) = mrYes then begin
      if not(TProfileHandler.DeleteExportProfile(ExtractFilePath(Application.ExeName) + 'export\' + cbProfileList.Text + '.' + profileextension)) then
        MessageDlg(GLanguageHandler.Text[kCouldntDeleteProfile], mtError, [mbOK], 0);

      GetProfiles;
    end;
  end;
end;


procedure TfrmExport.sbLSBLeftClick(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbNumberDecimalClick(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbNumberSize8bitClick(Sender: TObject);
begin
 if sbNumberSize8bit.Down then
    sbOutputBytes.Caption := 'Bytes'
  else if sbNumberSize16bit.Down then
    sbOutputBytes.Caption := 'Words'
  else if sbNumberSize32bit.Down then
    sbOutputBytes.Caption := 'LWords'
  else if sbNumberSize8bitSwap.Down then
    sbOutputBytes.Caption := 'Bytes'
  else if sbNumberSize16bitSwap.Down then
    sbOutputBytes.Caption := 'Words';

  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbOpenClick(Sender: TObject);
begin
  if cbProfileList.Enabled then begin
    LoadProfile(cbProfileList.Items[cbProfileList.ItemIndex]);

    Preview;
  end;
end;


procedure TfrmExport.sbRGBClick(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


procedure TfrmExport.sbSaveClick(Sender: TObject);
var
  s : string;

begin
  if InputQuery(GLanguageHandler.Text[kProfileName], '', s) then begin

    CreateExportOptions;

    TProfileHandler.SaveExportProfile(ExtractFilePath(Application.Exename) + 'export\' + s + '.' + profileextension,
                                      gbRGB.Visible,
                                      eeo);

    GetProfiles;
  end;
end;


procedure TfrmExport.shapeBackgroundPixelsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cdExport.Execute then begin
    shapeBackgroundPixels.Brush.Color := cdExport.Color;

    if (cbAutoPreview.Checked) and not(FBuilding) then
      Preview;
  end;
end;


procedure TfrmExport.GetProfiles;
begin
  TProfileHandler.GetExportProfilesList(ExtractFilePath(Application.ExeName) + 'export\*.' + profileextension, cbProfileList);

  if cbProfileList.Items.Count = 0 then
    cbProfileList.Enabled   := False
  else begin
    cbProfileList.Enabled   := True;
    cbProfileList.ItemIndex := 0;
  end;
end;


procedure TfrmExport.LoadProfile(aFileName : string);
begin
  eeo := TProfileHandler.LoadExportProfileFromFile(ExtractFilePath(Application.Exename) + 'export\' + aFileName + '.' + profileextension);

  if (eeo.Valid) then
    BuildFromProfile(eeo)
  else
    MessageDlg(GLanguageHandler.Text[kErrorLoadingProfile], mtError, [mbOK], 0);
end;


procedure TfrmExport.pcExportChange(Sender: TObject);
begin
  if (cbAutoPreview.Checked) and not(FBuilding) then
    Preview;
end;


function TfrmExport.ValidateNumberEdit(ne : TEdit): boolean;
var
  t : integer;

begin
  Result := True;

  if Length(ne.Text) > 0 then begin
    for t := 1 to length(ne.Text) do begin
      if (ord(ne.Text[t]) < 48) or (ord(ne.Text[t]) > 57) then
        Result := False;
    end;
  end
  else
    Result := False;

  if Result then
    ne.Color := clWindow
  else
    ne.Color := clFuchsia;
end;


function TfrmExport.SaveBinaryData(aFileName : string): boolean;
var
  tf          : File of byte;
  lFilePrefix, lOutputFileName : string;
  lAnimFrame  : integer;
  lTempOutput : string;
  lByte       : byte;
  t, z : integer;

begin
  Result      := True;
  lAnimFrame  := StrToInt(eBinaryFrameStart.Text);
  lFilePrefix := TUtility.GetFilePrefix(aFileName);

  if rbSaveAnimation.Checked then
    lOutputFileName := aFileName
  else
    lOutputFileName := ExtractFilePath(aFileName) + lFilePrefix + '_' + IntToStr(lAnimFrame) + ExtractFileExt(aFileName);

  AssignFile(tf, lOutputFileName);
  Rewrite(tf);

  for t := 0 to mBinary.Lines.Count - 1 do begin
    if mBinary.Lines[t] <> '' then begin
      lTempOutput := '';

      for z := 1 to length(mBinary.Lines[t]) do begin
        if mBinary.Lines[t][z] <> ' ' then begin
          lTempOutput := lTempOutput + mBinary.Lines[t][z];
        end
        else begin
          lByte := TUtility.HexToByte(lTempOutput);

          write(tf, lByte);

          lTempOutput := '';
        end;
      end;
    end
    else begin
      if (rbSaveFrame.Checked) and (lAnimFrame < StrToInt(eBinaryFrameEnd.Text)) then begin
        CloseFile(tf);

        inc(lAnimFrame);

        lOutputFileName := ExtractFilePath(aFileName) + lFilePrefix + '_' + IntToStr(lAnimFrame) + ExtractFileExt(aFileName);

        AssignFile(tf, lOutputFileName);
        Rewrite(tf);
      end;
    end;
  end;

  CloseFile(tf);
end;


procedure TfrmExport.AddExampleCode;
var
  s : string;

begin
  s := '';

  case eeo.Language of
    elCSV        : {};
    elPICAXE     : {};
    elC1Dim      : {};
    elC2Dim      : {};
    elCFastLED   : s := TExampleFastLED.GetExample(eeo.StartFrame, eeo.EndFrame, FPixelCountFrame);
    elPython1Dim : {};
    elPython2Dim : {};
    elMicrochip  : {};
    elPascal     : {};
  end;

  if (s <> '') then begin
    FOutput.Add(TExportUtility.GetExampleCodeDisclaimer(eeo));

    FOutput.Add(s);
  end;
end;


end.
