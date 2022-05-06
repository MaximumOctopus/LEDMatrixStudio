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

unit formCopyMultiple;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,

  languagehandler;

type
  TframeCopyMultiple = class(TForm)
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Bevel1: TBevel;
    Label4: TLabel;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lWarningMessage: TLabel;
    eStartFrame: TEdit;
    eEndFrame: TEdit;
    eCopyTo: TEdit;
    cbSourceLayer: TComboBox;
    cbDestinationLayer: TComboBox;
    cbAllLayers: TCheckBox;
    procedure eStartFrameChange(Sender: TObject);
    procedure cbAllLayersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFrameCount : integer;

    procedure SetGUILanguageText;

    function ValidateInputs: boolean;
  public
    { Public declarations }
  end;

  TCopyMultipleObject = record
    Process : boolean;
    StartFrame, EndFrame, CopyTo : integer;
    Source, Destination : integer;
    AllLayers : boolean;
  end;


function DoCopyMultiple(aFrameCount : integer; var aLayers : TStringList): TCopyMultipleObject;


var
  frameCopyMultiple : TFrameCopyMultiple;


implementation


{$R *.dfm}


function DoCopyMultiple(aFrameCount : integer; var aLayers : TStringList): TCopyMultipleObject;
var
  lLayer : integer;

begin
  with TFrameCopyMultiple.Create(Application) do
    try
      FFrameCount := aFrameCount;

      Result.Process := False;

      for lLayer := 0 to aLayers.Count - 1 do begin
        cbSourceLayer.Items.Add(aLayers[lLayer]);
        cbDestinationLayer.Items.Add(aLayers[lLayer]);
      end;

      cbSourceLayer.ItemIndex      := 0;
      cbDestinationLayer.ItemIndex := 0;

      // =======================================================================

      ShowModal;

      // =======================================================================

      if ModalResult = mrOK then begin
        Result.Process     := True;
        Result.StartFrame  := StrToInt(eStartFrame.Text);
        Result.EndFrame    := StrToInt(eEndFrame.Text);
        Result.CopyTo      := StrToInt(eCopyTo.Text);

        Result.Source      := cbSourceLayer.ItemIndex;
        Result.Destination := cbDestinationLayer.ItemIndex;

        Result.AllLayers   := cbAllLayers.Checked;
      end;

    finally
      Free;
    end;
end;


procedure TframeCopyMultiple.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TframeCopyMultiple.FormShow(Sender: TObject);
begin
  eStartFrameChange(Nil);
end;


procedure TframeCopyMultiple.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kCopyMultipleFrames];

  Label1.Caption := GLanguageHandler.Text[kStart];
  Label2.Caption := GLanguageHandler.Text[kEnd];
  Label3.Caption := GLanguageHandler.Text[kCopyTo];
  Label5.Caption := GLanguageHandler.Text[kDestinationFramesMustExist];
  lWarningMessage.Caption := GLanguageHandler.Text[kWarningYouWillLoseData];
  Label7.Caption := GLanguageHandler.Text[kLayers];
  Label6.Caption := GLanguageHandler.Text[kSource];
  Label8.Caption := GLanguageHandler.Text[kDestination];
  cbAllLayers.Caption := GLanguageHandler.Text[kAllLayers];

  Label4.Caption := GLanguageHandler.Text[kWarningThisActionCannotBeUndone];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TframeCopyMultiple.cbAllLayersClick(Sender: TObject);
begin
  cbDestinationLayer.Enabled := not(cbAllLayers.Checked);

  eStartFrameChange(Nil);
end;


procedure TframeCopyMultiple.eStartFrameChange(Sender: TObject);
begin
  bOk.Enabled := ValidateInputs;
end;


function TFrameCopyMultiple.ValidateInputs: boolean;
var
  lStartFrame, lEndFrame, lCopyTo, lMaxFrames : integer;
  lWarning : boolean;

begin
  lWarning := False;

  lStartFrame := StrToIntDef(eStartFrame.Text, -1);
  lEndFrame   := StrToIntDef(eEndFrame.Text, -1);
  lCopyTo     := StrToIntDef(eCopyTo.Text, -1);
  lMaxFrames  := lCopyTo + (lEndFrame - lStartFrame);               // max frame to write to

  if cbAllLayers.Checked then begin
    if (lCopyTo >= lStartFrame) and (lCopyTo <= lEndFrame) then
      lWarning := True;
  end
  else begin
    if (lCopyTo >= lStartFrame) and (lCopyTo <= lEndFrame) and (cbSourceLayer.ItemIndex = cbDestinationLayer.ItemIndex) then
      lWarning := True;
  end;

  lWarningMessage.Visible := lWarning;

  Result := (lStartFrame <> -1) and (lEndFrame <> -1) and (lCopyTo <> -1) and
            (lStartFrame <= lEndFrame) and (lMaxFrames <= FFrameCount) and (lMaxFrames >= 1) and
            not(lWarning);
end;


end.
