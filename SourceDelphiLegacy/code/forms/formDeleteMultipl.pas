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

unit formDeleteMultipl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler;


type
  TfrmDeleteMultiple = class(TForm)
    Bevel1: TBevel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    lWarning: TLabel;
    Image1: TImage;
    GroupBox1: TGroupBox;
    lFrom: TLabel;
    lTO: TLabel;
    eStartFrame: TEdit;
    eEndFrame: TEdit;
    procedure eStartFrameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;

    function ValidateInputs: boolean;
  public
  end;

  TDeleteMultipleObject = record
    Process : boolean;
    StartFrame, EndFrame : integer;
  end;


function DoDeleteMultiple: TDeleteMultipleObject;


var
  frmDeleteMultiple: TfrmDeleteMultiple;


implementation


{$R *.dfm}


function DoDeleteMultiple: TDeleteMultipleObject;
begin
  with TfrmDeleteMultiple.Create(Application) do
    try
      Result.Process := False;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process    := True;
        Result.StartFrame := StrToInt(eStartFrame.Text);
        Result.EndFrame   := StrToInt(eEndFrame.Text);
      end;

    finally
      Free;
    end;
end;


procedure TfrmDeleteMultiple.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmDeleteMultiple.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kDeleteMultipleFramesC];

  lFrom.Caption := GLanguageHandler.Text[kFrom];
  lTo.Caption := GLanguageHandler.Text[kToC];
  lWarning.Caption := GLanguageHandler.Text[kWarningThisActionCannotBeUndone];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmDeleteMultiple.eStartFrameChange(Sender: TObject);
begin
  bOk.Enabled := ValidateInputs;
end;


function TfrmDeleteMultiple.ValidateInputs: boolean;
var
  sf, ef : integer;

begin
  sf := StrToIntDef(eStartFrame.Text, -1);
  ef := StrToIntDef(eEndFrame.Text, -1);


  Result := (sf <> -1) and (ef <> -1) and (sf <= ef);
end;


end.
