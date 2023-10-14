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


unit formPreviewPopout;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Buttons,

  languagehandler;


type
  TCommand = procedure(aValue : integer) of object;

  TfrmPreviewPopout = class(TForm)
    Panel1: TPanel;
    pAnimationToolbar: TPanel;
    lFrame: TLabel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    bPlayAnimation: TBitBtn;
    bStopAnimation: TBitBtn;
    bPreviousFrame: TBitBtn;
    bNextFrame: TBitBtn;
    tbFrames: TTrackBar;
    bStartFrame: TBitBtn;
    bEndFrame: TBitBtn;
    procedure bPlayAnimationClick(Sender: TObject);
    procedure tbFramesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOnCommand  : TCommand;
    FOnNewFrame : TCommand;

    procedure SetGUILanguageText;

  public
    property OnCommand  : TCommand read FOnCommand  write FOnCommand;
    property OnNewFrame : TCommand read FOnNewFrame write FOnNewFrame;
  end;


var
  frmPreviewPopout: TfrmPreviewPopout;


implementation


{$R *.dfm}


procedure TfrmPreviewPopout.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmPreviewPopout.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kPreview];
end;


procedure TfrmPreviewPopout.bPlayAnimationClick(Sender: TObject);
begin
  if Assigned(FOnCommand) then
    FOnCommand(TBitBtn(Sender).Tag);
end;


procedure TfrmPreviewPopout.tbFramesChange(Sender: TObject);
begin
  if Assigned(FOnNewFrame) then
    FOnNewFrame(tbFrames.Position);
end;


end.
