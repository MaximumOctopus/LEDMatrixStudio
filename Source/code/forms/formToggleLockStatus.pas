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


unit formToggleLockStatus;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler;


type
  TfrmToggleLockStatus = class(TForm)
    Bevel1: TBevel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    eStartFrame: TEdit;
    eEndFrame: TEdit;
    cbLockStatus: TCheckBox;
    procedure eStartFrameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFrameCount : integer;

    procedure SetGUILanguageText;

    function ValidateInputs: boolean;
  end;


  TToggleLockFrameRangeObject = record
    Process              : boolean;
    LockStatus           : boolean;
    StartFrame, EndFrame : integer;
  end;


var
  frmToggleLockStatus: TfrmToggleLockStatus;


function DoToggleLockFrameRange(aFrameCount : integer): TToggleLockFrameRangeObject;


implementation


{$R *.dfm}


function DoToggleLockFrameRange(aFrameCount : integer): TToggleLockFrameRangeObject;
begin
  with TfrmToggleLockStatus.Create(Application) do
    try
      FFrameCount := aFrameCount;

      Result.Process := False;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process    := True;
        Result.LockStatus := cbLockStatus.Checked;
        Result.StartFrame := StrToInt(eStartFrame.Text);
        Result.EndFrame   := StrToInt(eEndFrame.Text);
      end;

    finally
      Free;
    end;
end;


procedure TfrmToggleLockStatus.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmToggleLockStatus.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kToggleFrameLockStatus];
  Label1.Caption := GLanguageHandler.Text[kStart];
  Label2.Caption := GLanguageHandler.Text[kEnd];
  cbLockStatus.Caption := GLanguageHandler.Text[kLock];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmToggleLockStatus.eStartFrameChange(Sender: TObject);
begin
  bOk.Enabled := ValidateInputs;
end;


function TfrmToggleLockStatus.ValidateInputs: boolean;
var
  sf, ef : integer;

begin
  sf := StrToIntDef(eStartFrame.Text, -1);
  ef := StrToIntDef(eEndFrame.Text, -1);

  Result := (sf <> -1) and (ef <> -1) and (sf <= ef);
end;


end.
