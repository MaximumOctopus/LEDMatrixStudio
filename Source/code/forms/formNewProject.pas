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

unit formNewProject;



interface

uses
  Windows, Messages, System.UITypes, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, utility, ExtCtrls, ComCtrls, math,

  matrixconstants, Vcl.Imaging.pngimage,

  presethandler, languagehandler,

  projectsettings;


type
  TfrmNewProject = class(TForm)
    bOK: TBitBtn;
    gbAnimation: TGroupBox;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Label3: TLabel;
    Label5: TLabel;
    Bevel19: TBevel;
    pcNew: TPageControl;
    tsCustom: TTabSheet;
    tsFromPreset: TTabSheet;
    gbMatrixOptions: TGroupBox;
    Label1: TLabel;
    cbHeight: TComboBox;
    cbWidth: TComboBox;
    cbMatrixType: TComboBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    cbPresets: TComboBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label8: TLabel;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox10: TComboBox;
    cbClearAll: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lPresetType: TLabel;
    lPresetWidth: TLabel;
    lPresetHeight: TLabel;
    rbCommon: TRadioButton;
    rbAll: TRadioButton;
    gbPixelShape: TGroupBox;
    GroupBox8: TGroupBox;
    Label10: TLabel;
    ComboBox4: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    shapeSquare: TShape;
    shapeCircle: TShape;
    rbPixelSquare: TRadioButton;
    rbPixelCircle: TRadioButton;
    rbPixelRoundRect: TRadioButton;
    shapeRoundRect: TShape;
    sBackground: TShape;
    lBackground: TLabel;
    cdNewProject: TColorDialog;
    mHelp: TMemo;
    cbFrames: TComboBox;
    Image1: TImage;
    cbCustomShape: TComboBox;
    cbCustomShapeParam: TComboBox;
    Label11: TLabel;
    bCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbPresetsChange(Sender: TObject);
    procedure rbCommonClick(Sender: TObject);
    procedure shapeSquareMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbMatrixTypeChange(Sender: TObject);
    procedure cbCustomShapeChange(Sender: TObject);
    procedure cbWidthChange(Sender: TObject);
  private
    FOldWidth  : string;
    FOldHeight : string;

    procedure SetGUILanguageText;

    procedure BuildPresetList;

    procedure UpdateHelp(aMatrixMode : TMatrixMode);
  public
    { Public declarations }
  end;


var
  frmNewProject: TfrmNewProject;


function DoNewProject(aOldSettings : TProjectSettings; appstatus : boolean): TProjectSettings;


implementation


{$R *.dfm}


uses xglobal;


const CCommonSizes : array[1..15] of string = ('1', '2', '4', '5', '7', '8', '12', '16', '24', '32', '48', '60', '64', '128', '256');


var
 clearstatus : boolean;


function DoNewProject(aOldSettings : TProjectSettings; appstatus : boolean): TProjectSettings;
begin
  with TfrmNewProject.Create(Application) do begin
    try
      Result.Valid       := False;
      Result.MatrixMode  := mtMono;
      Result.width       := -1;
      Result.height      := -1;
      Result.clear       := False;
      Result.CustomShape := csNone;

      case aOldSettings.pixel of
        psSquare    : rbPixelSquare.Checked    := True;
        psCircle    : rbPixelCircle.Checked    := True;
        psRoundRect : rbPixelRoundRect.Checked := True;
      end;

      clearstatus   := appstatus;

      sBackground.Brush.Color := aOldSettings.Background;

      cbMatrixType.ItemIndex  := Ord(aOldSettings.MatrixMode);

      cbMatrixTypeChange(Nil);

      // ===========================================================================

      if (aOldSettings.Width < 1) or (aOldSettings.Height < 1) then begin
        aOldSettings.width  := 8;
        aOldSettings.height := 8;
      end;


      FOldWidth  := IntToStr(aOldSettings.width);
      FOldHeight := IntToStr(aOldSettings.height);

      if Result.sizetype then
        rbCommon.Checked := True
      else
        rbAll.Checked    := True;

      rbCommonClick(Nil);

      // ===========================================================================

      BuildPresetList;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Valid := True;

        if pcNew.ActivePageIndex = 0 then begin
          Result.MatrixMode := TMatrixMode(cbMatrixType.ItemIndex);
          Result.width      := StrToInt(cbWidth.Text);
          Result.height     := StrToInt(cbHeight.Text);
        end
        else begin
          Result.MatrixMode := TMatrixMode(lPresetType.Tag);
          Result.width      := StrToInt(lPresetWidth.Caption);
          Result.height     := StrToInt(lPresetHeight.Caption);
        end;

        Result.CustomShape      := TCustomShape(cbCustomShape.ItemIndex);
        Result.CustomShapeParam := cbCustomShapeParam.ItemIndex;

        Result.Background       := sBackground.Brush.Color;

        Result.Clear            := cbClearAll.Checked;
        Result.Special          := StrToInt(cbFrames.Text);

        if rbPixelSquare.Checked then
          Result.Pixel := psSquare
        else if rbPixelCircle.Checked then
          Result.Pixel := psCircle
        else
          Result.Pixel := psRoundRect;

        if rbCommon.Checked then
          Result.SizeType := True
        else
          Result.SizeType := False;
      end;
    finally
      Free;
    end;
  end;
end;


procedure TfrmNewProject.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;

  cbFrames.Items.Add('1');
  cbFrames.Items.Add('2');
  cbFrames.Items.Add('4');
  cbFrames.Items.Add('5');
  cbFrames.Items.Add('10');
  cbFrames.Items.Add('16');
  cbFrames.Items.Add('20');
  cbFrames.Items.Add('25');
  cbFrames.Items.Add('32');
  cbFrames.Items.Add('50');
  cbFrames.Items.Add('64');
  cbFrames.Items.Add('100');
end;


procedure TfrmNewProject.SetGUILanguageText;
var
  t : integer;

begin
  Caption := GLanguageHandler.Text[kCreate];

  tsCustom.Caption := GLanguageHandler.Text[kCustom];

  gbMatrixOptions.Caption := GLanguageHandler.Text[kMatrixOptions];

  for t := 0 to 4 do
    cbMatrixType.Items.Add(TUtility.GetTypeName(TMatrixMode(t)));

  cbMatrixType.ItemIndex := 0;


  cbCustomShape.Items.Add(GLanguageHandler.Text[kNoCustomShape]);
  cbCustomShape.Items.Add(GLanguageHandler.Text[kCircle]);
  cbCustomShape.Items.Add(GLanguageHandler.Text[kFrameBorderNoCentre]);
  cbCustomShape.Items.Add(GLanguageHandler.Text[kTriangle]);
  cbCustomShape.ItemIndex := 0;

  lBackground.Caption := GLanguageHandler.Text[kBackground];
  rbCommon.Caption := GLanguageHandler.Text[kCommon];
  rbAll.Caption := GLanguageHandler.Text[kAll];
  Label11.Caption := GLanguageHandler.Text[kBorder];


  tsFromPreset.Caption := GLanguageHandler.Text[kFromPreset];

  Label6.Caption := GLanguageHandler.Text[kType];
  Label7.Caption := GLanguageHandler.Text[kWidth];
  Label9.Caption := GLanguageHandler.Text[kHeight];



  gbPixelShape.Caption := GLanguageHandler.Text[kPixelShape];

  gbAnimation.Caption := GLanguageHandler.Text[kAnimation];
  Label3.Caption := GLanguageHandler.Text[kStartWith];
  Label5.Caption := GLanguageHandler.Text[kAnimationFrames];

  cbClearAll.Caption := GLanguageHandler.Text[kClearAllAnimationData];


  bOK.Caption := GLanguageHandler.Text[kCreate];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmNewProject.bOKClick(Sender: TObject);
begin
  if (TUtility.ValidateNumber(cbFrames.Text, 100000)) then begin
    if clearstatus then begin
      if cbClearAll.Checked then begin
        if MessageDlg(GLanguageHandler.Text[kAreYouSureYouWantToDeleteTheCurrentMatrix], mtWarning, [mbYes, mbNo], 0) = mrYes then
          ModalResult := mrOK;
      end
      else
        ModalResult := mrOK;
    end
    else
      ModalResult := mrOK;
  end
  else begin
    cbFrames.SelectAll;
    cbFrames.SetFocus;
  end;
end;


procedure TfrmNewProject.BuildPresetList;
begin
  if TPresetHandler.GetMatrixPresetList(ExtractFilePath(Application.ExeName) + 'presets\*.ledspreset', cbPresets) then begin
    cbPresets.Enabled := True;

    cbPresets.ItemIndex := 0;
    cbPresetsChange(Nil);
  end
  else begin
    cbPresets.Enabled := False;
  end;
end;


procedure TfrmNewProject.cbCustomShapeChange(Sender: TObject);
var
  t, c : integer;

begin
  cbCustomShapeParam.Clear;

  case cbCustomShape.ItemIndex of
    customShapeNone        : begin
                               cbCustomShapeParam.Items.Add(GLanguageHandler.Text[kNA]);
                             end;
    customShapeCircle      : begin
                               cbCustomShapeParam.Items.Add(GLanguageHandler.Text[kNA]);
                             end;
    customShapeJustBorders : begin
                               c := Floor(Min(StrToInt(cbWidth.Text), StrToInt(cbHeight.Text)) / 2);

                               for t := 1 to c do
                                 cbCustomShapeParam.Items.Add(IntToStr(t));
                             end;
    customShapeTriangle    : begin
                               cbCustomShapeParam.Items.Add(GLanguageHandler.Text[kNA]);
                             end;
  end;

  cbCustomShapeParam.ItemIndex := 0;
end;


procedure TfrmNewProject.cbMatrixTypeChange(Sender: TObject);
var
  lStatus : boolean;

begin
  lStatus := False;

  if (cbMatrixType.ItemIndex = 3) then begin
    lStatus := True;
  end;

  lBackground.Visible := lStatus;
  sBackground.Visible := lStatus;

  UpdateHelp(TMatrixMode(cbMatrixType.ItemIndex));
end;


procedure TfrmNewProject.cbPresetsChange(Sender: TObject);
var
 lMPP : TMatrixPreset;

 begin
  lMPP := TPresetHandler.LoadMatrixPreset(ExtractFilePath(Application.ExeName) + 'presets\' + cbPresets.Text + '.ledspreset');

  lPresetWidth.Caption  := IntToStr(lMPP.Width);
  lPresetHeight.Caption := IntToStr(lMPP.Height);

  lPresetType.Caption   := lMPP.MatrixModeText;
  lPresetType.Tag       := lMPP.MatrixModeTag;
end;


procedure TfrmNewProject.cbWidthChange(Sender: TObject);
begin
  cbCustomShapeChange(Nil);
end;


procedure TfrmNewProject.rbCommonClick(Sender: TObject);
var
  x : integer;

begin
  cbWidth.Clear;
  cbHeight.Clear;

  if rbAll.Checked then begin
    for x := 1 to 256 do begin
      cbWidth.Items.Add(IntToStr(x));
      cbHeight.Items.Add(IntToStr(x));
    end;

    cbWidth.ItemIndex  := cbWidth.Items.IndexOf(FOldWidth);
    cbHeight.ItemIndex := cbHeight.Items.IndexOf(FOldHeight);
  end
  else begin
    for x := 1 to 15 do begin
      cbWidth.Items.Add(CCommonSizes[x]);
      cbHeight.Items.Add(CCommonSizes[x]);
    end;

    cbWidth.ItemIndex  := cbWidth.Items.IndexOf(FOldWidth);
    cbHeight.ItemIndex := cbHeight.Items.IndexOf(FOldHeight);

    if cbHeight.ItemIndex = -1 then
      cbHeight.ItemIndex := 7;

    if cbWidth.ItemIndex = -1 then
      cbWidth.ItemIndex := 7;
  end;

  FOldWidth  := cbWidth.Text;
  FOldHeight := cbHeight.Text;
end;


procedure TfrmNewProject.sBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (cdNewProject.Execute) then begin
    sBackground.Brush.Color := cdNewProject.Color;
  end;
end;


procedure TfrmNewProject.shapeSquareMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case TShape(Sender).Tag of
    0 : rbPixelSquare.Checked    := True;
    1 : rbPixelCircle.Checked    := True;
    2 : rbPixelRoundRect.Checked := True;
  end;
end;


procedure TfrmNewProject.UpdateHelp(aMatrixMode : TMatrixMode);
begin
  case aMatrixMode of
    mtMono          : mHelp.Text := GLanguageHandler.Text[kNPModeMono];
    mtBiSequential  : mHelp.Text := GLanguageHandler.Text[kNPModeBiSequential];
    mtBiBitPlanes   : mHelp.Text := GLanguageHandler.Text[kNPModeBiBitplane];
    mtRGB           : mHelp.Text := GLanguageHandler.Text[kNPModeRGB];
    mtRGB3BPP       : mHelp.Text := GLanguageHandler.Text[kNPModeRGB3BPP];
  end;
end;


end.
