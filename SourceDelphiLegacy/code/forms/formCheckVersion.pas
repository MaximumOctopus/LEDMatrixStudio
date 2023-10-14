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

unit formCheckVersion;


interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, StdCtrls, Buttons,

  languagehandler;


type
  TfrmCheckVersion = class(TForm)
    httpMain: TIdHTTP;
    bClose: TBitBtn;
    bHistory: TBitBtn;
    mHistory: TMemo;
    Shape1: TShape;
    Label5: TLabel;
    bWebsite: TBitBtn;
    gbInstalledVersion: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    gbLatestVersion: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    lIVDate: TLabel;
    lIVVersion: TLabel;
    lLADate: TLabel;
    lLAVersion: TLabel;
    GroupBox1: TGroupBox;
    lWhat: TLabel;
    procedure FormShow(Sender: TObject);
    procedure bHistoryClick(Sender: TObject);
    procedure bWebsiteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
    { Public declarations }
  end;


var
  frmCheckVersion: TfrmCheckVersion;


function CheckForNewVersion(cv,cd,appdat : string; autoclosemode : boolean): word;


implementation


{$R *.dfm}


uses utility, Registry;


var
 ApplicationVersionFile : string;
 AutoClose : boolean;


function CheckForNewVersion(cv,cd,appdat : string; autoclosemode : boolean): word;
 begin
  with TfrmCheckVersion.Create(Application) do
    try
      ApplicationVersionFile := appdat;

      autoclose              := autoclosemode;

      lIVDate.Caption        := cd;
      lIVVersion.Caption     := cv;

      ShowModal;

      Result := ModalResult;
    finally
      Free;
    end;
end;


procedure TfrmCheckVersion.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmCheckVersion.FormShow(Sender: TObject);
 var
  s : string;
  x : TStringList;

 begin
  x := TStringList.Create;

  try
    s := httpMain.Get('http://www.maximumoctopus.com/versions/d' + ApplicationVersionFile);

    x.Text := s;

    lLADate.Caption    := x.Strings[0];
    lLAVersion.Caption := x.Strings[1];

    if (lLADate.Caption <> lIVDate.Caption) or
       (lLAVersion.Caption <> lIVVersion.Caption) then begin
      lWhat.Caption := GLanguageHandler.Text[kANewVersionIsAvailable];

      s := httpMain.Get('http://www.maximumoctopus.com/versions/h' + ApplicationVersionFile);

      mHistory.Text      := s;

      bHistory.Enabled   := True;
    end
    else begin
      lWhat.Caption := GLanguageHandler.Text[kNoNewVersionIsAvailable];

      Close;
    end;
  except
    lLADate.Caption    := GLanguageHandler.Text[kError];
    lLAVersion.Caption := GLanguageHandler.Text[kError];

    lWhat.Caption      := GLanguageHandler.Text[kUnableToConnectTomaximumoctopuscom];
  end;

  x.Free;
end;


procedure TfrmCheckVersion.SetGUILanguageText;
begin
  Label5.Caption := GLanguageHandler.Text[kUpdateCheck];

  gbInstalledVersion.Caption := GLanguageHandler.Text[kInstalledVersion];
  Label6.Caption := GLanguageHandler.Text[kDate];
  Label7.Caption := GLanguageHandler.Text[kVersion];

  gbLatestVersion.Caption := GLanguageHandler.Text[kLatestAvailableOnline];
  Label10.Caption := GLanguageHandler.Text[kDate];
  Label11.Caption := GLanguageHandler.Text[kVersion];

  bWebsite.Caption := GLanguageHandler.Text[kWebsite];
  bHistory.Caption := GLanguageHandler.Text[kHistory] + '>>';
  bClose.Caption := GLanguageHandler.Text[kClose];
end;


procedure TfrmCheckVersion.bHistoryClick(Sender: TObject);
 begin
  if bHistory.Tag = 0 then begin
    bHistory.Tag     := 1;
    bHistory.Caption := GLanguageHandler.Text[kHistory] + ' <<';
    ClientHeight     := 494;
  end
  else begin
    bHistory.Tag     := 0;
    bHistory.Caption := GLanguageHandler.Text[kHistory] + ' >>';
    ClientHeight     := 271;
  end;
end;


procedure TfrmCheckVersion.bWebsiteClick(Sender: TObject);
 begin
  TUtility.ExecuteFile(0, 'https://sourceforge.net/projects/led-matrix-studio/', '', '');
end;


end.
