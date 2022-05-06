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

unit formColourChange;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, System.UITypes,

  languagehandler,

  utility;

type
  TfrmColourChange = class(TForm)
    Bevel1: TBevel;
    Label3: TLabel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    cdChanger: TColorDialog;
    Image1: TImage;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sFrom: TShape;
    sTo: TShape;
    Label4: TLabel;
    clbUserFrom: TColorListBox;
    clbUserTo: TColorListBox;
    procedure sFromMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure clbUserFromDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
    { Public declarations }
  end;


  TColourChangeObject = record
                          Process            : boolean;
                          ColourFrom, ColourTo : integer;
  end;


function DoColourChange(var aColours : TStringList): TColourChangeObject;


var
  frmColourChange: TfrmColourChange;


implementation


{$R *.dfm}


function DoColourChange(var aColours : TStringList): TColourChangeObject;
var
  t : integer;

begin
  with TfrmColourChange.Create(Application) do
    try
      Result.Process := False;

      for t := 0 to aColours.Count - 1 do begin
        clbUserFrom.AddItem(TUtility.RGBPlusInteger(StrToInt(aColours[t]), 100), TObject(StrToInt(aColours[t])));
        clbUserTo.AddItem(TUtility.RGBPlusInteger(StrToInt(aColours[t]), 100), TObject(StrToInt(aColours[t])));
      end;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process    := True;
        Result.ColourFrom := sFrom.Brush.Color;
        Result.ColourTo   := sTo.Brush.Color;
      end;

    finally
      Free;
    end;
end;


procedure TfrmColourChange.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmColourChange.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kColourChanger];
  Label1.Caption := GLanguageHandler.Text[kFrom];
  Label2.Caption := GLanguageHandler.Text[kToC];
  Label4.Caption := GLanguageHandler.Text[kWarningThisActionCannotBeUndone];
  Label3.Caption := GLanguageHandler.Text[kFirstThirtyTwoColoursFromTheCurrentAnimation];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmColourChange.clbUserFromDblClick(Sender: TObject);
begin
  if TColorListBox(Sender).ItemIndex <> -1 then begin
    if TColorListBox(Sender).Tag = 0 then
      sFrom.Brush.Color := TColor(clbUserFrom.Items.Objects[clbUserFrom.ItemIndex])
    else
      sTo.Brush.Color   := TColor(clbUserTo.Items.Objects[clbUserTo.ItemIndex]);
  end;
end;


procedure TfrmColourChange.sFromMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (cdChanger.Execute) then begin
    TShape(Sender).Brush.Color := cdChanger.Color;
  end;
end;


end.
