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


unit formSaveRange;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler;


type
  TfrmSaveRange = class(TForm)
    Bevel1: TBevel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Image1: TImage;
    GroupBox1: TGroupBox;
    lStart: TLabel;
    lEnd: TLabel;
    eStartFrame: TEdit;
    eEndFrame: TEdit;
    procedure eStartFrameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFrameCount : integer;

    procedure SetGUILanguageText;

    function ValidateInputs: boolean;
  end;


  TSaveFrameRangeObject = record
    Process : boolean;
    StartFrame, EndFrame : integer;
  end;


var
  frmSaveRange: TfrmSaveRange;


function DoSaveFrameRange(aFrameCount : integer): TSaveFrameRangeObject;


implementation


{$R *.dfm}


function DoSaveFrameRange(aFrameCount : integer): TSaveFrameRangeObject;
begin
  with TfrmSaveRange.Create(Application) do
    try
      FFrameCount := aFrameCount;

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


procedure TfrmSaveRange.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmSaveRange.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kSaveARangeOfFrames];

  lStart.Caption := GLanguageHandler.Text[kStart];
  lEnd.Caption := GLanguageHandler.Text[kEnd];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmSaveRange.eStartFrameChange(Sender: TObject);
begin
  bOk.Enabled := ValidateInputs;
end;


function TfrmSaveRange.ValidateInputs: boolean;
var
  sf, ef : integer;

begin
  sf := StrToIntDef(eStartFrame.Text, -1);
  ef := StrToIntDef(eEndFrame.Text, -1);

  Result := (sf <> -1) and (ef <> -1) and (sf <= ef);
end;


end.
