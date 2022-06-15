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

unit formMerge;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ExtCtrls,

  languagehandler;


type
  TfrmMerge = class(TForm)
    Bevel1: TBevel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    odMain: TOpenDialog;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    miMerge: TSpeedButton;
    lStartFrame: TLabel;
    rbMergeBottom: TRadioButton;
    rbMergeTop: TRadioButton;
    rbMergeNewLayer: TRadioButton;
    rbMergeCurrentLayer: TRadioButton;
    eFileName: TEdit;
    eStartFrame: TEdit;
    procedure miMergeClick(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
    procedure eStartFrameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
    { Public declarations }
  end;


  TMergeMode = (moAnimationBottom, moAnimationTop, moNewLayer, moCurrentFrame);


  TMergeObject = record
    Process    : boolean;
    FileName   : string;
    Mode       : TMergeMode;
    StartFrame : integer;
  end;


function DoMerge: TMergeObject;


var
  frmMerge: TfrmMerge;


implementation


{$R *.dfm}


function DoMerge: TMergeObject;
begin
  with TfrmMerge.Create(Application) do
    try
      Result.Process := False;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process    := True;

        Result.FileName   := eFileName.Text;
        Result.StartFrame := StrToInt(eStartFrame.Text);

        if rbMergeBottom.Checked then
          Result.Mode := moAnimationBottom
        else if rbMergeTop.Checked then
          Result.Mode := moAnimationTop
        else if rbMergeNewLayer.Checked then
          Result.Mode := moNewLayer
        else
          Result.Mode := moCurrentFrame;
      end;

    finally
      Free;
    end;
end;


procedure TfrmMerge.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmMerge.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kMerge];
  Label1.Caption := GLanguageHandler.Text[kFileName];
  miMerge.Caption := GLanguageHandler.Text[kOpen];

  rbMergeBottom.Caption := GLanguageHandler.Text[kMergeInToAnimationBottomHasPriority];
  rbMergeTop.Caption := GLanguageHandler.Text[kMergeInToAnimationTopHasPriority];
  rbMergeNewLayer.Caption := GLanguageHandler.Text[kMergeInToNewLayer];
  rbMergeCurrentLayer.Caption := GLanguageHandler.Text[kMergeInToCurrentLayer];

  lStartFrame.Caption := GLanguageHandler.Text[kStartFrame];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmMerge.eFileNameChange(Sender: TObject);
begin
  if eFileName.Text <> '' then
    bOK.Enabled := True
  else
    bOK.Enabled := False;
end;


procedure TfrmMerge.eStartFrameChange(Sender: TObject);
begin
  if StrToIntDef(eStartFrame.Text, -1) = -1 then
    bOK.Enabled := False
  else
    bOK.Enabled := True;
end;


procedure TfrmMerge.miMergeClick(Sender: TObject);
begin
  if odMain.Execute then begin
    eFileName.Text := odMain.FileName;

    bOK.Enabled := True;
  end;
end;


end.
