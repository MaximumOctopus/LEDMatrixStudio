// ===================================================================
//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit formPlaybackSpeed;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler;

type
  TfrmCustomPlayback = class(TForm)
    GroupBox1: TGroupBox;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    eSpeed: TEdit;
    Label1: TLabel;
    Image1: TImage;
    Bevel1: TBevel;
    lEquality: TLabel;
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
  end;


var
  frmCustomPlayback: TfrmCustomPlayback;


function DoCustomPlaybackSpeed(aOldCustom : integer): integer;


implementation


{$R *.dfm}


function DoCustomPlaybackSpeed(aOldCustom : integer): integer;
begin
  with TfrmCustomPlayback.Create(Application) do
    try
      eSpeed.Text := IntToStr(aOldCustom);

      ShowModal;

      if ModalResult = mrOK then
        Result := StrToIntDef(eSpeed.Text, 0)
      else
        Result := 0;
    finally
      Free;
    end;
end;


procedure TfrmCustomPlayback.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmCustomPlayback.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kCustomPlaybackSpeed];
  lEquality.Caption := GLanguageHandler.Text[k1000ms1Second];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmCustomPlayback.bOKClick(Sender: TObject);
var
  lSpeed : integer;

begin
  lSpeed := StrToIntDef(eSpeed.Text, 0);

  if (lSpeed > 0) then
    ModalResult := mrOK;
end;


end.
