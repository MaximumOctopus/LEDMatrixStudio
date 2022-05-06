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

unit formAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, Vcl.Imaging.pngimage;

type
  TfrmAbout = class(TForm)
    Label3: TLabel;
    lVersion: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    lDate: TLabel;
    Label5: TLabel;
    lUndos: TLabel;
    Image1: TImage;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;


implementation


{$R *.DFM}


uses utility;


procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lDate.Caption    := LEDStudioDate;
  lVersion.Caption := 'LED Matrix Studio';
end;


procedure TfrmAbout.FormShow(Sender: TObject);
begin
  lUndos.Caption := IntToStr(MatrixMain.GetTotalUndos) + ' undos';
end;


procedure TfrmAbout.Image1Click(Sender: TObject);
begin
  Close;
end;


procedure TfrmAbout.Label13Click(Sender: TObject);
begin
  TUtility.ExecuteFile(0, LowerCase((Sender As TLabel).Caption), '', '');
end;


procedure TfrmAbout.Label5Click(Sender: TObject);
begin
  case TLabel(Sender).Tag of
    0 : TUtility.ExecuteFile(0, 'http://https://sourceforge.net/projects/led-matrix-studio/', '', '');
    1 : TUtility.ExecuteFile(0, 'http://twitter.com/maximumoctopus', '', '');
    2 : TUtility.ExecuteFIle(0, 'mailto:freeware@freshney.org', '', '');
  end;
end;


end.
