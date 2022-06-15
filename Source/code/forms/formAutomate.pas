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

unit formAutomate;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.UITypes, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,

  fileconstants, matrixconstants,

  formNewBrush,

  utility, languagehandler,

  ActionObject;


type
  TAutomationInput = record
    FrameCurrent : integer;
    FrameMax     : integer;

    Width        : integer;
    Height       : integer;

    MatrixMode   : TMatrixMode;
  end;

  TfrmAutomate = class(TForm)
    bOK: TBitBtn;
    bCancel: TBitBtn;
    PageControl1: TPageControl;
    tsActions: TTabSheet;
    gbActions: TGroupBox;
    sbMirror: TSpeedButton;
    sbFlip: TSpeedButton;
    sbInvert: TSpeedButton;
    sbScrollLeft: TSpeedButton;
    sbScrollRight: TSpeedButton;
    sbScrollUp: TSpeedButton;
    sbScrollDown: TSpeedButton;
    sbRotateL: TSpeedButton;
    sbRotateR: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Label10: TLabel;
    Label11: TLabel;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    gbProcessingOptions: TGroupBox;
    lFrameStart: TLabel;
    lFrameEnd: TLabel;
    Label5: TLabel;
    eFrameStart: TEdit;
    eFrameEnd: TEdit;
    rbProcessMode2: TRadioButton;
    rbProcessMode1: TRadioButton;
    cbErase: TCheckBox;
    gbActionList: TGroupBox;
    sbClear: TSpeedButton;
    sbRemoveSelected: TSpeedButton;
    Label4: TLabel;
    lbActions: TListBox;
    lBrush: TLabel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    tsOptions: TTabSheet;
    gbBrush: TGroupBox;
    gbColourCycling: TGroupBox;
    Label12: TLabel;
    sbCyclingLinear: TSpeedButton;
    SpeedButton10: TSpeedButton;
    clbSource: TColorListBox;
    clbTarget: TColorListBox;
    bAddColour: TBitBtn;
    bDeleteColour: TBitBtn;
    bOpenSourceColours: TBitBtn;
    bSaveSourceColours: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    bOpenTargetColours: TBitBtn;
    bSaveTargetColours: TBitBtn;
    Label13: TLabel;
    Label14: TLabel;
    cdColours: TColorDialog;
    sdSaveBrush: TSaveDialog;
    odLoadBrush: TOpenDialog;
    gbPostProcessing: TGroupBox;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    Label15: TLabel;
    lbPostProcessing: TListBox;
    bCustomBrush1: TBitBtn;
    bCopyBrush2ColoursSource: TBitBtn;
    bColourUp: TBitBtn;
    bColourDown: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    bSaveAutomation: TBitBtn;
    bLoadAutomation: TBitBtn;
    sbLeftRight: TSpeedButton;
    sbRightLeft: TSpeedButton;
    sbUpDown: TSpeedButton;
    sbDownUp: TSpeedButton;
    cbCB1Transparent: TCheckBox;
    sCB1TransparentColour: TShape;
    Label16: TLabel;
    SpeedButton9: TSpeedButton;
    SpeedButton13: TSpeedButton;
    bCustomBrush2: TBitBtn;
    cbCB2Transparent: TCheckBox;
    sCB2TransparentColour: TShape;
    Label17: TLabel;
    bCopyBrush1ColoursSource: TBitBtn;
    Label18: TLabel;
    lSuggestion: TLabel;
    lSpoon: TLabel;
    Label19: TLabel;
    SpeedButton14: TSpeedButton;
    Label20: TLabel;
    rbProcessMode3: TRadioButton;
    sRevealColour: TShape;
    lColour: TLabel;
    cbWipe: TComboBox;
    SpeedButton15: TSpeedButton;
    cbReveal: TComboBox;
    SpeedButton16: TSpeedButton;
    Bevel2: TBevel;
    cbLayer: TComboBox;
    lLayer: TLabel;
    sbClearSourceColours: TBitBtn;
    sbClearTargetColours: TBitBtn;
    clbUser: TColorListBox;
    Label23: TLabel;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    cbTargetSkip: TComboBox;
    Label24: TLabel;
    Label1: TLabel;
    procedure sbMirrorClick(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbRemoveSelectedClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure lbActionsDblClick(Sender: TObject);
    procedure bBrushClick(Sender: TObject);
    procedure bAddColourClick(Sender: TObject);
    procedure bDeleteColourClick(Sender: TObject);
    procedure bOpenSourceColoursClick(Sender: TObject);
    procedure bSaveSourceColoursClick(Sender: TObject);
    procedure bLoadAutomationClick(Sender: TObject);
    procedure bSaveAutomationClick(Sender: TObject);
    procedure sbCyclingLinearClick(Sender: TObject);
    procedure bCopyBrush2ColoursSourceClick(Sender: TObject);
    procedure bColourUpClick(Sender: TObject);
    procedure bColourDownClick(Sender: TObject);
    procedure sCB1TransparentColourMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton15Click(Sender: TObject);
    procedure sbClearSourceColoursClick(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRGBPaletteColours : TRGBPaletteColours;
    FAutomationInput : TAutomationInput;
    FCustomBrush     : array[0..1] of TStringList;
    FLastFileName    : string;

    procedure SetGUILanguageText;

    function  GetActionIDFromName(aName : string): integer;

    procedure LoadColours(aColourList : TColorListBox; aFileName : string);
    procedure SaveColours(aColourList : TColorListBox; aFileName : string);

    procedure SaveAutomation(aFileName : string);
    function  LoadDataParameterType(aInput : string): integer;
    procedure LoadAutomation(aFileName : string);

    procedure SetCaption(aPath : string);
  public
    { Public declarations }
  end;


var
  frmAutomate: TfrmAutomate;


function DoAutomate(aAutomationInput : TAutomationInput; aRGBPaletteColours : TRGBPaletteColours; var aLayers : TStringList; var aUserColours : TStringList; var aAO : TActionObject): word;


implementation


{$R *.dfm}


const
  CActionsCount  = 39;

  CWipeCount     = 8;
  CWipeOddCount  = 4;
  CWipeOddWCount = 6;
  CWipeOddHCount = 6;

  CRevealCount   = 6;


var
  CActions: array [0..CActionsCount] of string = ('Mirror', 'Flip', 'Invert',
                                                 'Scroll left', 'Scroll right', 'Scroll up', 'Scroll down',
                                                 'Rotate Left', 'Rotate Right',
                                                 'Wipe (Vertical)', 'Wipe (Vertical) Clear',
                                                 'Wipe (Horizontal)', 'Wipe (Horizontal) Clear',
                                                 'Jiggle Left', 'Jiggle Right',
                                                 'Jiggle Up', 'Jiggle Down',
                                                 'Bounce left/right', 'Bounce up/down',
                                                 'Brush #1 every frame', 'Brush #1 first frame',
                                                 'Brush #2 every frame', 'Brush #2 first frame',
                                                 'Scroll left/right split', 'Scroll right/left split',
                                                 'Scroll up/downt split', 'Scroll downt/up split',
                                                 'Colour cycling (linear)', 'Colour cycling (bounce)',
                                                 'Alternate up/down scroll',
                                                 'Reveal left/right', 'Reveal right/left', 'Reveal top/bottom', 'Reveal bottom/top', 'Reveal centre in', 'Reveal centre out',
                                                 'Wipe left', 'Wipe right', 'Wipe up', 'Wipe down'
                                                 );

  CActionsWipe     : array[0..CWipeCount - 1] of integer       = ( 9, 10, 11, 12, 36, 37, 38, 39);
  CActionsWipeOdd  : array[0..CWipeOddCount - 1] of integer    = (36, 37, 38, 39);
  CActionsWipeOddW : array[0..CWipeOddWCount - 1] of integer   = (11, 12, 36, 37, 38, 39);
  CActionsWipeOddH : array[0..CWipeOddHCount - 1] of integer   = ( 9, 10, 36, 37, 38, 39);

  CActionsReveal   : array[0..CRevealCount - 1] of integer = (30, 31, 32, 33, 34, 35);


function DoAutomate(aAutomationInput : TAutomationInput; aRGBPaletteColours : TRGBPaletteColours; var aLayers : TStringList; var aUserColours : TStringList; var aAO : TActionObject): word;
var
  t, lAOId : integer;

begin
  with TfrmAutomate.Create(Application) do
    try
      DoubleBuffered := True;

      FLastFileName    := aAO.LastFileName;
      FAutomationInput := aAutomationInput;

      if not(DirectoryExists(ExtractFilePath(FLastFileName))) then
        FLastFileName := ExtractFilePath(Application.ExeName) + 'automate\';

      SetCaption(FLastFileName);

      // =======================================================================

      if Odd(FAutomationInput.Width) and Odd(FAutomationInput.Height) then begin
        for t := 0 to CWipeOddCount - 1 do
          cbWipe.Items.Add(CActions[CActionsWipeOdd[t]]);
      end
      else if Odd(FAutomationInput.Width) then begin
        for t := 0 to CWipeOddWCount - 1 do
          cbWipe.Items.Add(CActions[CActionsWipeOddW[t]]);
      end
      else if Odd(FAutomationInput.Height) then begin
        for t := 0 to CWipeOddHCount - 1 do
          cbWipe.Items.Add(CActions[CActionsWipeOddH[t]]);
      end
      else begin
        for t := 0 to CWipeCount - 1 do
          cbWipe.Items.Add(CActions[CActionsWipe[t]]);
      end;

      for t:= 0 to CRevealCount - 1 do
        cbReveal.Items.Add(CActions[CActionsReveal[t]]);

      cbWipe.ItemIndex   := 0;
      cbReveal.ItemIndex := 0;

      FCustomBrush[0] := TStringList.Create;
      FCustomBrush[1] := TStringList.Create;

      // =======================================================================

      FRGBPaletteColours.Left   := aRGBPaletteColours.Left;
      FRGBPaletteColours.Middle := aRGBPaletteColours.Middle;
      FRGBPaletteColours.Right  := aRGBPaletteColours.Right;

      for t:= 0 to 20 do
        FRGBPaletteColours.History[t] := aRGBPaletteColours.History[t];

      // =======================================================================

      eFrameStart.Text := IntToStr(FAutomationInput.FrameCurrent);
      eFrameEnd.Text   := IntToStr(FAutomationInput.FrameMax);

      // set from actionobject data (previous users settings if they exist)

      case aAO.Source of
        acFirstFrame   : rbProcessMode1.Checked := True;
        acEachFrame    : rbProcessMode2.Checked := True;
        acEachFrameInc : rbProcessMode3.Checked := True;
      end;

      cbErase.Checked   := aAO.EraseBehind;

      for t := 0 to aLayers.Count - 1 do
        cbLayer.Items.Add(aLayers[t]);

      cbLayer.ItemIndex := aAO.Layer;

      if aAO.ActionList.Count <> 0 then begin
        for t := 0 to aAO.ActionList.Count - 1 do
          lbActions.Items.Add(CActions[StrToInt(aAO.ActionList[t])]);

        bOK.Enabled := True;
      end;

      if aAO.PostProcessList.Count <> 0 then begin
        for t := 0 to aAO.PostProcessList.Count - 1 do
          lbPostProcessing.Items.Add(CActions[StrToInt(aAO.PostProcessList[t])]);

        bOK.Enabled := True;
      end;

      sRevealColour.Brush.Color := aAO.ParameterRevealColour;

      for t := 0 to aAO.SourceColours.Count - 1 do
        clbSource.AddItem(TUtility.RGBPlusInteger(StrToInt(aAO.SourceColours[t]), 100), TObject(StrToInt(aAO.SourceColours[t])));

      for t := 0 to aAO.TargetColours.Count - 1 do
        clbTarget.AddItem(TUtility.RGBPlusInteger(StrToInt(aAO.TargetColours[t]), 100), TObject(StrToInt(aAO.TargetColours[t])));

      cbTargetSkip.ItemIndex := aAO.TargetSkip;

      for t := 0 to aUserColours.Count - 1 do
        clbUser.AddItem(TUtility.RGBPlusInteger(StrToInt(aUserColours[t]), 100), TObject(StrToInt(aUserColours[t])));

      // == ensure that all changes are reflected after form closes ============

      aAO.ActionList.Clear;
      aAO.PostProcessList.Clear;
      aAO.SourceColours.Clear;
      aAO.TargetColours.Clear;

      // =======================================================================

      ShowModal;

      //

      Result := ModalResult;

      if ModalResult = mrOK then begin
        if FLastFileName <> '' then
          aAO.LastFileName := FLastFileName;

        aAO.EraseBehind := cbErase.Checked;

        if rbProcessMode1.Checked then
          aAO.Source := acFirstFrame
        else if rbProcessMode2.Checked then
          aAO.Source := acEachFrame
        else
          aAO.Source := acEachFrameInc;

        aAO.FrameStart := StrToIntDef(eFrameStart.Text, -1);
        aAO.FrameEnd   := StrToIntDef(eFrameEnd.Text, -1);

        aAO.ActionList.Clear;

        aAO.Layer := cbLayer.ItemIndex;

        for t := 0 to lbActions.Items.Count - 1 do begin
          lAOId := GetActionIDFromName(lbActions.Items[t]);

          if lAOId <> -1 then
            aAO.ActionList.Add(IntToStr(lAOId));
        end;

        for t := 0 to lbPostProcessing.Items.Count - 1 do begin
          lAOId := GetActionIDFromName(lbPostProcessing.Items[t]);

          if lAOId <> -1 then
            aAO.PostProcessList.Add(IntToStr(lAOId));
        end;

        // brush 1

        aAO.Brushes[0].BrushData.Clear;
        for t := 0 to FCustomBrush[0].Count - 1 do
          aAO.Brushes[0].BrushData.Add(FCustomBrush[0][t]);

        aAO.Brushes[0].Transparent       := cbCB1Transparent.Checked;
        aAO.Brushes[0].TransparentColour := sCB1TransparentColour.Brush.Color;

        // brush 2

        aAO.Brushes[1].BrushData.Clear;
        for t := 0 to FCustomBrush[1].Count - 1 do
          aAO.Brushes[1].BrushData.Add(FCustomBrush[1][t]);

        aAO.Brushes[1].Transparent       := cbCB2Transparent.Checked;
        aAO.Brushes[1].TransparentColour := sCB2TransparentColour.Brush.Color;

        aAO.ParameterRevealColour := sRevealColour.Brush.Color;

        // ==============

        for t := 0 to clbSource.Count - 1 do
          aAO.SourceColours.Add(IntToStr(TColor(clbSource.Items.Objects[t])));

        for t := 0 to clbTarget.Count - 1 do
          aAO.TargetColours.Add(IntToStr(TColor(clbTarget.Items.Objects[t])));

        aAO.TargetSkip := cbTargetSkip.ItemIndex;
      end;
    finally
      FCustomBrush[0].Free;
      FCustomBrush[1].Free;

      Free;
    end;
end;


procedure TfrmAutomate.FormCreate(Sender: TObject);
begin
  SetGUILanguageText
end;


procedure TfrmAutomate.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kAutomate];

  tsActions.Caption := GLanguageHandler.Text[kActions];
  gbActions.Caption := GLanguageHandler.Text[kAvailableActions];
  Label9.Caption := GLanguageHandler.Text[kWipe];
  Label20.Caption := GLanguageHandler.Text[kReveal];
  lColour.Caption := GLanguageHandler.Text[kColour];
  Label6.Caption := GLanguageHandler.Text[kProcess];
  sbMirror.Caption := GLanguageHandler.Text[kMirror];
  sbFlip.Caption := GLanguageHandler.Text[kFlip];
  sbInvert.Caption := GLanguageHandler.Text[kInvert];
  Label7.Caption := GLanguageHandler.Text[kRotate];
  Label8.Caption := GLanguageHandler.Text[kScroll];
  Label10.Caption := GLanguageHandler.Text[kJiggle];
  Label11.Caption := GLanguageHandler.Text[kBounce];
  Label19.Caption := GLanguageHandler.Text[kAlternate];
  lBrush.Caption := GLanguageHandler.Text[kBrushNo1];
  SpeedButton7.Caption := GLanguageHandler.Text[kEveryFrame];
  SpeedButton8.Caption := GLanguageHandler.Text[kFirstFrame];
  Label16.Caption := GLanguageHandler.Text[kBrushNo2];
  SpeedButton9.Caption := GLanguageHandler.Text[kEveryFrame];
  SpeedButton13.Caption := GLanguageHandler.Text[kFirstFrame];
  Label12.Caption := GLanguageHandler.Text[kColourCycle];
  sbCyclingLinear.Caption := GLanguageHandler.Text[kCyclingLinear];
  SpeedButton10.Caption := GLanguageHandler.Text[kCyclingBounce];

  gbProcessingOptions.Caption := GLanguageHandler.Text[kProcessingOptions];
  rbProcessMode1.Caption := GLanguageHandler.Text[kUseFirstFrameAsSource];
  rbProcessMode2.Caption := GLanguageHandler.Text[kEachFrameIndividually];
  rbProcessMode3.Caption := GLanguageHandler.Text[kEachFrameIndividuallyIncrement];
  cbErase.Caption := GLanguageHandler.Text[kEraseWipeJiggleModes];
  lFrameStart.Caption := GLanguageHandler.Text[kFrameStart];
  lFrameEnd.Caption := GLanguageHandler.Text[kFrameEnd];
  lLayer.Caption := GLanguageHandler.Text[kLayer];

  gbActionList.Caption := GLanguageHandler.Text[kActionList];
  Label4.Caption := GLanguageHandler.Text[kProcessedOnEachFrame];
  sbClear.Caption := GLanguageHandler.Text[kClear];
  sbRemoveSelected.Caption := GLanguageHandler.Text[kRemove];

  gbPostProcessing.Caption := GLanguageHandler.Text[kPostProcessing];
  Label15.Caption := GLanguageHandler.Text[kPostProcessingHelp];
  SpeedButton11.Caption := GLanguageHandler.Text[kClear];
  SpeedButton12.Caption := GLanguageHandler.Text[kRemove];

  tsOptions.Caption := GLanguageHandler.Text[kOptions];

  gbBrush.Caption := GLanguageHandler.Text[kBrush];
  Label17.Caption := GLanguageHandler.Text[kBrushNo1];
  bCustomBrush1.Caption := GLanguageHandler.Text[kCustomBrush];
  cbCB1Transparent.Caption := GLanguageHandler.Text[kTransparent];
  bCopyBrush1ColoursSource.Caption := GLanguageHandler.Text[kCopyColoursToSource];

  Label18.Caption := GLanguageHandler.Text[kBrushNo2];
  bCustomBrush2.Caption := GLanguageHandler.Text[kCustomBrush];
  cbCB2Transparent.Caption := GLanguageHandler.Text[kTransparent];
  bCopyBrush2ColoursSource.Caption := GLanguageHandler.Text[kCopyColoursToSource];

  gbColourCycling.Caption := GLanguageHandler.Text[kColourCycling];
  Label13.Caption := GLanguageHandler.Text[kSourceColourxs];
  sbClearSourceColours.Caption := GLanguageHandler.Text[kClear];
  bOpenSourceColours.Caption := GLanguageHandler.Text[kLoad];
  bSaveSourceColours.Caption := GLanguageHandler.Text[kSave];
  Label14.Caption := GLanguageHandler.Text[kTargetColours];
  sbClearTargetColours.Caption := GLanguageHandler.Text[kClear];
  bOpenTargetColours.Caption := GLanguageHandler.Text[kLoad];
  bSaveTargetColours.Caption := GLanguageHandler.Text[kSave];
  Label24.Caption := GLanguageHandler.Text[kSkip];
  Label1.Caption := GLanguageHandler.Text[kFrames];
  Label23.Caption := GLanguageHandler.Text[kFirst32CcoloursFromCurrentAnimation];
  lSpoon.Caption := GLanguageHandler.Text[kColourCyclingHelp];

  lSuggestion.Caption := GLanguageHandler.Text[kAutomateHelp];

  bLoadAutomation.Caption := GLanguageHandler.Text[kLoad];
  bSaveAutomation.Caption := GLanguageHandler.Text[kSave];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmAutomate.SetCaption(aPath : string);
begin
  Caption := GLanguageHandler.Text[kAutomate];

  if aPath <> '' then
    Caption := Caption + ' "' + aPath + '"';
end;


procedure TfrmAutomate.SpeedButton15Click(Sender: TObject);
begin
  case TSpeedButton(Sender).Tag of
    0 : lbActions.Items.Add(cbWipe.Text);
    1 : lbActions.Items.Add(cbReveal.Text);
  end;

  bOK.Enabled := True;
end;

procedure TfrmAutomate.bLoadAutomationClick(Sender: TObject);
begin
  odLoadBrush.DefaultExt := 'automation';
  odLoadBrush.Filter     := GLanguageHandler.Text[kAutomationFiles] + ' (*.automation)|*.automation';

  if (FLastFileName <> '') then
    odLoadBrush.InitialDir := ExtractFilePath(FLastFileName)
  else
    odLoadBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\';

  if odLoadBrush.Execute then begin
    LoadAutomation(odLoadBrush.FileName);

    if lbActions.Count <> 0 then
      bOK.Enabled := True;

    bCopyBrush1ColoursSource.Enabled := (FCustomBrush[0].Count <> 0);
    bCopyBrush2ColoursSource.Enabled := (FCustomBrush[1].Count <> 0);

    FLastFileName := odLoadBrush.FileName;

    SetCaption(FLastFileName);
  end;
end;


procedure TfrmAutomate.bOKClick(Sender: TObject);
var
  lFS, lFE : integer;

begin
  lFS := StrToIntDef(eFrameStart.Text, -1);
  lFE := StrToIntDef(eFrameEnd.Text, -1);

  if (lFS = -1) or (lFE = -1) then
    MessageDlg(GLanguageHandler.Text[kInvalidFrameStartFrameEndValues], mtWarning, [mbOK], 0)
  else
    ModalResult := mrOK;
end;


procedure TfrmAutomate.bSaveAutomationClick(Sender: TObject);
begin
  sdSaveBrush.DefaultExt := 'automation';
  sdSaveBrush.Filter     := GLanguageHandler.Text[kAutomationFiles] + ' (*.automation)|*.automation';

  if (FLastFileName <> '') then
    sdSaveBrush.InitialDir := ExtractFilePath(FLastFileName)
  else
    sdSaveBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\';

  if sdSaveBrush.Execute then begin
    SaveAutomation(sdSaveBrush.FileName);

    FLastFileName := sdSaveBrush.FileName;

    SetCaption(FLastFileName);
  end;
end;


procedure TfrmAutomate.bAddColourClick(Sender: TObject);
begin
  if cdColours.Execute then begin
    if TBitBtn(Sender).Tag = 0 then
      clbSource.AddItem(TUtility.RGBPlusInteger(cdColours.Color, 100), TObject(cdColours.Color))
    else
      clbTarget.AddItem(TUtility.RGBPlusInteger(cdColours.Color, 100), TObject(cdColours.Color));
  end;
end;


procedure TfrmAutomate.bBrushClick(Sender: TObject);
var
  lNewBrush : TNewBrush;
  lMatrixSettings : TMatrixSettings;
  lBrush : TStringList;
  t, lBrushIndex : integer;

begin
  lBrushIndex := TBitbtn(Sender).Tag;

  lMatrixSettings.MatrixMode := FAutomationInput.MatrixMode;
  lMatrixSettings.Width      := FAutomationInput.Width;
  lMatrixSettings.Height     := FAutomationInput.Height;

  lBrush := TStringList.Create;

  if FCustomBrush[lBrushIndex].Count <> 0 then begin
    for t:= 0 to FCustomBrush[lBrushIndex].Count - 1 do
      lBrush.Add(FCustomBrush[lBrushIndex][t]);
  end;

  lNewBrush := DoNewBrush(lBrush, lMatrixSettings, FRGBPaletteColours);

  if (lNewBrush.Proceed) then begin
    FCustomBrush[lBrushIndex].Clear;

    for t := 0 to lBrush.Count - 1 do
      FCustomBrush[lBrushIndex].Add(lBrush[t]);

    if lBrushIndex = 0 then
      bCopyBrush1ColoursSource.Enabled := True
    else
      bCopyBrush2ColoursSource.Enabled := True
  end;

  lBrush.Free;
end;


procedure TfrmAutomate.bColourUpClick(Sender: TObject);
var
  lII : integer;
  lColorListBox : TColorListBox;

begin
  if TBitBtn(Sender).Tag = 0 then
    lColorListBox := clbSource
  else
    lColorListBox := clbTarget;

  if lColorListBox.ItemIndex <> -1 then begin
    if lColorListBox.ItemIndex > 0 then begin
      lII := lColorListBox.ItemIndex;

      lColorListBox.Items.Move(lColorListBox.ItemIndex, lColorListBox.ItemIndex - 1);

      lColorListBox.ItemIndex := lII - 1;
    end;
  end;
end;


procedure TfrmAutomate.bColourDownClick(Sender: TObject);
var
  lII : integer;
  lColorListBox : TColorListBox;

begin
  if TBitBtn(Sender).Tag = 0 then
    lColorListBox := clbSource
  else
    lColorListBox := clbTarget;

  if lColorListBox.ItemIndex <> -1 then begin
    if lColorListBox.ItemIndex < lColorListBox.Items.Count - 1 then begin
      lII := lColorListBox.ItemIndex;

      lColorListBox.Items.Move(lColorListBox.ItemIndex, lColorListBox.ItemIndex + 1);

      lColorListBox.ItemIndex := lII + 1;
    end;
  end;
end;


procedure TfrmAutomate.bCopyBrush2ColoursSourceClick(Sender: TObject);
var
  lColourList : TStringList;
  lRow, i, lBrushIndex : integer;
  lColour : string;

begin
  lBrushIndex := TBitbtn(Sender).Tag;

  lColourList := TStringList.Create;
  lColourList.Sorted := True;

  lColour := '';

  for lRow := 0 to FCustomBrush[lBrushIndex].Count - 1 do begin

    for i:=1 to length(FCustomBrush[lBrushIndex][lRow]) do begin
      if (FCustomBrush[lBrushIndex][lRow][i] = ' ') or (i = length(FCustomBrush[lBrushIndex][lRow])) then begin

        if lColourList.IndexOf(lColour) = -1 then
          lColourList.Add(lColour);

        lColour := '';
      end
      else
        lColour := lColour + FCustomBrush[lBrushIndex][lRow][i];
    end;
  end;

  if lColourList.Count <> 0 then begin
    clbSource.Clear;

    for i := 0 to lColourList.Count - 1 do
      clbSource.AddItem(TUtility.RGBPlusInteger(TUtility.HexToInt(lColourList[i]), 100), TObject(TUtility.HexToInt(lColourList[i])));
  end;

  lColourList.Free;
end;



procedure TfrmAutomate.bDeleteColourClick(Sender: TObject);
begin
  if TBitBtn(Sender).Tag = 0 then
    clbSource.DeleteSelected
  else
    clbTarget.DeleteSelected;
end;


procedure TfrmAutomate.sbClearSourceColoursClick(Sender: TObject);
begin
  if (TSpeedButton(Sender).Tag = 0) then
    clbSource.Clear
  else
    clbTarget.Clear;
end;


procedure TfrmAutomate.BitBtn12Click(Sender: TObject);
begin
  if clbUser.ItemIndex <> -1 then begin

    if TSpeedButton(Sender).Tag = 0 then
      clbSource.AddItem(TUtility.RGBPlusInteger(TColor(clbUser.Items.Objects[clbUser.ItemIndex]), 100), clbUser.Items.Objects[clbUser.ItemIndex])
    else
      clbTarget.AddItem(TUtility.RGBPlusInteger(TColor(clbUser.Items.Objects[clbUser.ItemIndex]), 100), clbUser.Items.Objects[clbUser.ItemIndex]);
  end;
end;


procedure TfrmAutomate.bOpenSourceColoursClick(Sender: TObject);
begin
  odLoadBrush.DefaultExt := 'colours';
  odLoadBrush.Filter     := GLanguageHandler.Text[kColourLists] + ' (*.colours)|*.colours';
  odLoadBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\colours\';

  if odLoadBrush.Execute then begin
    if TBitBtn(Sender).Tag = 0 then
      LoadColours(clbSource, odLoadBrush.FileName)
    else
      LoadColours(clbTarget, odLoadBrush.FileName);
  end;
end;


procedure TfrmAutomate.bSaveSourceColoursClick(Sender: TObject);
begin
  sdSaveBrush.DefaultExt := 'colours';
  sdSaveBrush.Filter     := GLanguageHandler.Text[kColourLists] + ' (*.colours)|*.colours';
  sdSaveBrush.InitialDir := ExtractFilePath(Application.ExeName) + 'automate\colours\';

  if sdSaveBrush.Execute then begin
    if TBitBtn(Sender).Tag = 0 then
      SaveColours(clbSource, sdSaveBrush.FileName)
    else
      SaveColours(clbTarget, sdSaveBrush.FileName);
  end;
end;


procedure TfrmAutomate.LoadColours(aColourList : TColorListBox; aFileName : string);
var
  tf : TextFile;
  s : string;
  i : integer;

begin
  aColourList.Clear;

  AssignFile(tf, aFileName);
  Reset(tf);

  while not(eof(tf)) do begin
    readln(tf, s);

    if (pos('$', s) <> 0) then begin
      i := TUtility.HexToInt(UpperCase(copy(s, 6)));
    end
    else
      i := StrToInt(Copy(s, 5));

    aColourList.AddItem(TUtility.RGBPlusInteger(i, 100), TObject(i));
  end;

  CloseFile(tf);
end;


procedure TfrmAutomate.SaveColours(aColourList : TColorListBox; aFileName : string);
var
  tf : TextFile;
  i : integer;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  for i := 0 to aColourList.Items.Count - 1 do begin
    writeln(tf, kColoursData + ':' + IntToStr(TColor(aColourList.Items.Objects[i])));
  end;

  CloseFile(tf);
end;


function TfrmAutomate.GetActionIDFromName(aName : string): integer;
var
   t : integer;

begin
   Result := -1;

   for t:= 0 to CActionsCount do begin
     if aName = CActions[t] then
       Result := t;
   end;
end;


procedure TfrmAutomate.lbActionsDblClick(Sender: TObject);
begin
  if TListBox(Sender).Tag = 0 then
    sbRemoveSelectedClick(lbActions)
  else
    sbRemoveSelectedClick(lbPostProcessing);
end;


procedure TfrmAutomate.sbClearClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Tag = 0 then
    lbActions.Clear
  else
    lbPostProcessing.Clear;

  if (lbActions.Count = 0) and (lbPostProcessing.Count = 0) then
    bOK.Enabled := False
  else
    bOK.Enabled := True;
end;


procedure TfrmAutomate.sbMirrorClick(Sender: TObject);
var
  id: integer;
  lCanAdd : boolean;

begin
  lCanAdd := False;

  id := TSpeedButton(Sender).Tag;

  case id of
    19, 20 : begin
               if FCustomBrush[0].Count <> 0 then
                 lCanAdd := True
               else
                 MessageDlg(GLanguageHandler.Text[kNoCustomBrush1Selected], mtWarning, [mbOK], 0);
             end;
    21, 22 : begin
               if FCustomBrush[1].Count <> 0 then
                 lCanAdd := True
               else
                 MessageDlg(GLanguageHandler.Text[kNoCustomBrush2Selected], mtWarning, [mbOK], 0);
             end;
  else
    lCanAdd := True;
  end;

  if lCanAdd then begin
    lbActions.Items.Add(CActions[id]);

    bOK.Enabled := True;
  end;
end;


procedure TfrmAutomate.sbRemoveSelectedClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Tag = 0 then
    lbActions.DeleteSelected
  else
    lbPostProcessing.DeleteSelected;

  if (lbActions.Count = 0) and (lbPostProcessing.Count = 0) then
    bOK.Enabled := False
  else
    bOK.Enabled := True;
end;


procedure TfrmAutomate.sCB1TransparentColourMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cdColours.Color := TShape(Sender).Brush.Color;

  if cdColours.Execute then
    TShape(Sender).Brush.Color := cdColours.Color;
end;


procedure TfrmAutomate.sbCyclingLinearClick(Sender: TObject);
var
  id: integer;
  lCanAdd : boolean;

begin
  lCanAdd := False;

  id := TSpeedButton(Sender).Tag;

  case id of
    27, 28 : begin
               if (clbSource.Count <> 0) and (clbTarget.Count <> 0) then
                 lCanAdd := True
               else
                 MessageDlg(GLanguageHandler.Text[kNoSourcTargetColoursSelected], mtWarning, [mbOK], 0);
             end;
  else
    lCanAdd := True;
  end;

  if lCanAdd then begin
    lbPostProcessing.Items.Add(CActions[id]);

    bOK.Enabled := True;
  end;
end;

procedure TfrmAutomate.SaveAutomation(aFileName : string);
var
  tf : TextFile;
  t : integer;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  // ===========================================================================

  writeln(tf, '{' + kFileHeaderData);

  if (rbProcessMode1.Checked) then
    writeln(tf, kAutomationProcessMode + ':1')
  else if rbProcessMode2.Checked then
    writeln(tf, kAutomationProcessMode + ':0')
  else
    writeln(tf, kAutomationProcessMode + ':2');

  writeln(tf, kAutomationStartFrame + ':' + eFrameStart.Text);
  writeln(tf, kAutomationEndFrame +   ':' + eFrameEnd.Text);

  if cbErase.Checked then
    writeln(tf, kAutomationErase + ':1')
  else
    writeln(tf, kAutomationErase + ':0');

  writeln(tf, kDataBlockEnd);

  // ===========================================================================

  if lbActions.Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderActions);

    for t := 0 to lbActions.Count - 1 do begin
      writeln(tf, kAutomationActionItem + ':' + lbActions.Items[t]);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  if lbPostProcessing.Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderPostProcessing);

    for t:= 0 to lbPostProcessing.Count - 1 do begin
      writeln(tf, kAutomationPostProcessingItem + ':' + lbPostProcessing.Items[t]);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  if FCustomBrush[0].Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderBrush1);

    writeln(tf, kAutomationBrushColour +      ':' + IntToStr(sCB1TransparentColour.Brush.Color));
    writeln(tf, kAutomationBrushTransparent + ':' + IntToStr(TUtility.BoolToInt(cbCB1Transparent.Checked)));

    for t := 0 to FCustomBrush[0].Count - 1 do begin
      writeln(tf, kAutomationBrushRowData + ':' + FCustomBrush[0][t]);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  if FCustomBrush[1].Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderBrush2);

    writeln(tf, kAutomationBrushColour +      ':' + IntToStr(sCB2TransparentColour.Brush.Color));
    writeln(tf, kAutomationBrushTransparent + ':' + IntToStr(TUtility.BoolToInt(cbCB2Transparent.Checked)));

    for t := 0 to FCustomBrush[1].Count - 1 do begin
      writeln(tf, kAutomationBrushRowData + ':' + FCustomBrush[1][t]);
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  if clbSource.Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderSource);

    for t := 0 to clbSource.Count - 1 do begin
      writeln(tf, kAutomationColor + ':' + IntToStr(TColor(clbSource.Items.Objects[t])));
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  if clbTarget.Count <> 0 then begin
    writeln(tf, '{' + kFileHeaderTarget);

    for t := 0 to clbTarget.Count - 1 do begin
      writeln(tf, kAutomationColor + ':' + IntToStr(TColor(clbTarget.Items.Objects[t])));
    end;

    writeln(tf, kDataBlockEnd);
  end;

  // ===========================================================================

  CloseFile(tf);
end;


function TfrmAutomate.LoadDataParameterType(aInput : string): integer;
begin
  Result := -1;

  if Pos('{' + kFileHeaderData, aInput) <> 0 then
    Result := 1
  else if Pos('{' + kFileHeaderActions, aInput) <> 0 then
    Result := 2
  else if Pos('{' + kFileHeaderPostProcessing, aInput) <> 0 then
    Result := 3
  else if Pos('{' + kFileHeaderBrush1, aInput) <> 0 then
    Result := 4
  else if Pos('{' + kFileHeaderBrush2, aInput) <> 0 then
    Result := 5
  else if Pos('{' + kFileHeaderSource, aInput) <> 0 then
    Result := 6
  else if Pos('{' + kFileHeaderTarget, aInput) <> 0 then
    Result := 7
  else if aInput[1] = kDataBlockEnd then
    Result := 8
  else begin
    case aInput[1] of
      kAutomationBrushColour      : Result := 10;
      kAutomationBrushTransparent : Result := 11;
      kAutomationColor            : Result := 20;
      kAutomationActionItem       : Result := 21;
      kAutomationProcessMode      : Result := 30;
      kAutomationStartFrame       : Result := 31;
      kAutomationEndFrame         : Result := 32;
      kAutomationErase            : Result := 33;
    end;
  end;
end;


procedure TfrmAutomate.LoadAutomation(aFileName : string);
var
  tf : TextFile;
  s, lInput : string;
  lProcessMode : integer;

begin
  lbActions.Clear;
  lbPostProcessing.Clear;
  clbSource.Clear;
  clbTarget.Clear;
  FCustomBrush[0].Clear;
  FCustomBrush[1].Clear;

  lProcessMode := 0;

  AssignFile(tf, aFileName);
  Reset(tf);

  while not(eof(tf)) do begin
    readln(tf, s);

    if s <> '' then begin
      lInput := Copy(s, 3);

      case LoadDataParameterType(LowerCase(s)) of
         1 : lProcessMode := 1;
         2 : lProcessMode := 2;
         3 : lProcessMode := 3;
         4 : lProcessMode := 4;
         5 : lProcessMode := 5;
         6 : lProcessMode := 6;
         7 : lProcessMode := 7;
         8 : lProcessMode := 0;

        10 : begin
               case lProcessMode of
                 4 : sCB1TransparentColour.Brush.Color := StrToInt(lInput);
                 5 : sCB2TransparentColour.Brush.Color := StrToInt(lInput);
               end;
             end;
        11 : begin
               case lProcessMode of
                 4 : cbCB1Transparent.Checked := TUtility.IntToBool(StrToInt(lInput));
                 5 : cbCB2Transparent.Checked := TUtility.IntToBool(StrToInt(lInput));
               end;
             end;

        20 : begin
               if (lProcessMode = 6) then
                 clbSource.AddItem(TUtility.RGBPlusInteger(StrToInt(lInput), 100), TObject(StrToInt(lInput)))
               else if (lProcessMode = 7) then
                 clbTarget.AddItem(TUtility.RGBPlusInteger(StrToInt(lInput), 100), TObject(StrToInt(lInput)));
             end;

        21 : begin
               case lProcessMode of
                 2 : lbActions.Items.Add(lInput);
                 3 : lbPostProcessing.Items.Add(lInput);
                 4 : FCustomBrush[0].Add(lInput);
                 5 : FCustomBrush[1].Add(lInput);
               end;
             end;

        30 : begin
               if lProcessMode = 1 then begin
                 if lInput = '1' then
                   rbProcessMode1.Checked := True
                 else if lInput = '0' then
                   rbProcessMode2.Checked := True
                 else
                   rbProcessMode3.Checked := True;
               end;
             end;
        31 : begin
               if lProcessMode = 1 then
                 eFrameStart.Text := lInput;
             end;
        32 : begin
               if lProcessMode = 1 then
                 eFrameEnd.Text := lInput;
             end;
        33 : begin
               if lProcessMode = 1 then begin
                 if lInput = '1' then
                   cbErase.Checked := True
                 else
                   cbErase.Checked := False;
               end;
             end;
      end;
    end;
  end;

  CloseFile(tf);
end;


end.
