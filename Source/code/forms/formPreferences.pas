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

unit formPreferences;


interface


uses
  Windows, Messages, System.UITypes, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Vcl.Imaging.pngimage,

  languagehandler;


type
  TfrmPreferences = class(TForm)
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Bevel1: TBevel;
    colorDialogPrefs: TColorDialog;
    Image1: TImage;
    bResetToDefaults: TSpeedButton;
    gbColours: TGroupBox;
    lShapeOn1: TLabel;
    sMono2: TShape;
    lShapeOff: TLabel;
    sMono1: TShape;
    Label2: TLabel;
    Label1: TLabel;
    sBi2: TShape;
    Label5: TLabel;
    sBi1: TShape;
    Label8: TLabel;
    Label9: TLabel;
    sBi4: TShape;
    Label10: TLabel;
    sBi3: TShape;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lSelector: TLabel;
    ShapeSelection: TShape;
    lLightBox: TLabel;
    ShapeLightBox: TShape;
    Label3: TLabel;
    Label4: TLabel;
    gbMisc: TGroupBox;
    sbClearRecentFileList: TSpeedButton;
    gbLimiter: TGroupBox;
    eMaxPixels: TEdit;
    Label15: TLabel;
    Label17: TLabel;
    eExportPreview: TEdit;
    Label18: TLabel;
    lHexFormat: TLabel;
    cbHexFormat: TComboBox;
    procedure sMono1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbClearRecentFileListClick(Sender: TObject);
    procedure bResetToDefaultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
  end;


var
  frmPreferences: TfrmPreferences;


function DoPrefs: word;


implementation


{$R *.dfm}


uses utility;


function DoPrefs: word;
begin
  with TfrmPreferences.Create(Application) do
    try
      sMono1.Brush.Color         := MatrixMain.LEDColoursSingle[0];
      sMono2.Brush.Color         := MatrixMain.LEDColoursSingle[1];

      sBi1.Brush.Color           := MatrixMain.LEDColoursBi[0];
      sBi2.Brush.Color           := MatrixMain.LEDColoursBi[1];
      sBi3.Brush.Color           := MatrixMain.LEDColoursBi[2];
      sBi4.Brush.Color           := MatrixMain.LEDColoursBi[3];

      ShapeSelection.Brush.Color := MatrixMain.LEDColours[4];
      ShapeLightBox.Brush.Color  := MatrixMain.LEDColours[5];

      eMaxPixels.Text            := IntToStr(LMSSettings.App.ExportUpdateMaxPixels);
      eExportPreview.Text        := IntToStr(LMSSettings.App.ExportPreviewSize);

      if (LMSSettings.App.HexPrefix = '') then
        cbHexFormat.ItemIndex := 0
      else
        cbHexFormat.Text := LMSSettings.App.HexPrefix;

      // =======================================================================

      ShowModal;

      // =======================================================================

      if ModalResult = mrOK then begin
        MatrixMain.LEDColoursSingle[0] := sMono1.Brush.Color;
        MatrixMain.LEDColoursSingle[1] := sMono2.Brush.Color;

        MatrixMain.LEDColoursBi[0]     := sBi1.Brush.Color;
        MatrixMain.LEDColoursBi[1]     := sBi2.Brush.Color;
        MatrixMain.LEDColoursBi[2]     := sBi3.Brush.Color;
        MatrixMain.LEDColoursBi[3]     := sBi4.Brush.Color;

        MatrixMain.LEDColoursSingle[4] := ShapeSelection.Brush.Color;
        MatrixMain.LEDColoursSingle[5] := ShapeLightBox.Brush.Color;
        MatrixMain.LEDColoursBi[4]     := ShapeSelection.Brush.Color;
        MatrixMain.LEDColoursBi[5]     := ShapeLightBox.Brush.Color;

        MatrixMain.CopyLEDColours;

        LMSSettings.App.ExportUpdateMaxPixels := StrToIntDef(eMaxPixels.Text, 100000);
        LMSSettings.App.ExportPreviewSize     := StrToIntDef(eExportPreview.Text, 512);

        if (cbHexFormat.ItemIndex = 0) then
          LMSSettings.App.HexPrefix := ''
        else
          LMSSettings.App.HexPrefix := cbHexFormat.Text;
      end;

      Result := ModalResult;
    finally
      Free;
    end;
end;


procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmPreferences.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kPreferences];

  gbColours.Caption := GLanguageHandler.Text[kColours];

  Label2.Caption := GLanguageHandler.Text[kColourRepresenting1];
  Label8.Caption := GLanguageHandler.Text[kColourRepresenting2];
  Label3.Caption := GLanguageHandler.Text[kColourRepresenting3];
  Label4.Caption := GLanguageHandler.Text[kColourRepresentingLightbox];

  lLightBox.Caption := GLanguageHandler.Text[kLightBox];
  lSelector.Caption := GLanguageHandler.Text[kSelector];

  gbLimiter.Caption := GLanguageHandler.Text[kExportAutoGenerateOnStartupLimiter];
  Label15.Caption := GLanguageHandler.Text[kMaxPixels];
  Label17.Caption := GLanguageHandler.Text[kPreviewShowFirst];
  Label18.Caption := GLanguageHandler.Text[kLines];

  gbMisc.Caption := GLanguageHandler.Text[kMisc];
  sbClearRecentFileList.Caption := GLanguageHandler.Text[kClearRecentFilesList];

  lHexFormat.Caption := GLanguageHandler.Text[kHexFormat];

  sbClearRecentFileList.Caption := GLanguageHandler.Text[kClearRecentFilesList];

  bResetToDefaults.Caption := GLanguageHandler.Text[kResetToDefaults];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmPreferences.sMono1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if colorDialogPrefs.Execute then begin
    TShape(Sender).Brush.Color := colorDialogPrefs.Color;
  end;
end;


procedure TfrmPreferences.sbClearRecentFileListClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kAreYouSure], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    LMSSettings.FileHistory.Clear;
  end;
end;


procedure TfrmPreferences.bResetToDefaultsClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kAreYouSure], mtWarning, [mbYes, mbNo], 0) = mrYes then begin
    sMono1.Brush.Color         := clBlack;
    sMono2.Brush.Color         := clWhite;

    sBi1.Brush.Color           := clBlack;
    sBi2.Brush.Color           := clRed;
    sBi3.Brush.Color           := clGreen;
    sBi4.Brush.Color           := clYellow;

    ShapeSelection.Brush.Color := clBlue;
    ShapeLightBox.Brush.Color  := $00DDDDDD;

    eMaxPixels.Text            := '100000';
    eExportPreview.Text        := '256';
  end;
end;


end.
