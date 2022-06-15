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

unit frameQuickData;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls,  Vcl.Menus,

  languagehandler, utility;


type
  TframeSimpleExport = class(TFrame)
    Panel1: TPanel;
    Label2: TLabel;
    cbSource: TComboBox;
    bCopySourceData: TBitBtn;
    cbSourceDirection: TComboBox;
    cbSourceLSB: TComboBox;
    Panel2: TPanel;
    mData: TMemo;
    cbCombineNybbles: TCheckBox;
    PopupMenu1: TPopupMenu;
    miHexNone: TMenuItem;
    N0xx1: TMenuItem;
    N1: TMenuItem;
    lHexPrefix: TLabel;
    procedure cbSourceChange(Sender: TObject);
    procedure cbSourceDirectionChange(Sender: TObject);
    procedure bCopySourceDataClick(Sender: TObject);
    procedure miHexNoneClick(Sender: TObject);
  private
    function GetCombineNybbles: boolean;
    function GetDirection: integer;
    function GetHex: boolean;
    function GetLSB: integer;
    function GetSource: integer;
  public
    FOnChange : TNotifyEvent;

    procedure SetGUILanguageText;

    procedure SetText(aText : string);

    property OnChange       : TNotifyEvent read FOnChange write FOnChange;

    property CombineNybbles : boolean      read GetCombineNybbles;
    property Direction      : integer      read GetDirection;
    property Hex            : boolean      read GetHex;
    property LSB            : integer      read GetLSB;
    property Source         : integer      read GetSource;
  end;


implementation


{$R *.dfm}


function TframeSimpleExport.GetCombineNybbles: boolean;
begin
  Result := cbCombineNybbles.Checked;
end;


function TframeSimpleExport.GetDirection: integer;
begin
  Result := cbSourceDirection.ItemIndex;
end;


function TframeSimpleExport.GetHex: boolean;
begin
  Result := True;//cbHex.Checked;
end;


function TframeSimpleExport.GetLSB: integer;
begin
  Result := cbSourceLSB.ItemIndex;
end;


function TframeSimpleExport.GetSource: integer;
begin
  Result := cbSource.ItemIndex;
end;


procedure TframeSimpleExport.miHexNoneClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0 : LMSSettings.App.HexPrefix := '';
    1 : LMSSettings.App.HexPrefix := '0x';
    2 : LMSSettings.App.HexPrefix := '$';
  end;

  if (Assigned(OnChange)) then
    OnChange(Nil);
end;


procedure TframeSimpleExport.bCopySourceDataClick(Sender: TObject);
begin
  mData.SelectAll;
  mData.CopyToClipboard;
end;


procedure TframeSimpleExport.cbSourceChange(Sender: TObject);
begin
  cbSourceLSB.Clear;
  cbSourceDirection.Clear;

  if cbSource.ItemIndex = 0 then begin
    cbSourceLSB.Items.Add(GLanguageHandler.Text[kLSBAtLeft]);
    cbSourceLSB.Items.Add(GLanguageHandler.Text[kLSBAtRight]);

    cbSourceDirection.Items.Add(GLanguageHandler.Text[kTopToBottom]);
    cbSourceDirection.Items.Add(GLanguageHandler.Text[kBottomToTop]);
  end
  else begin
    cbSourceLSB.Items.Add(GLanguageHandler.Text[kLSBAtTop]);
    cbSourceLSB.Items.Add(GLanguageHandler.Text[kLSBAtBottom]);

    cbSourceDirection.Items.Add(GLanguageHandler.Text[kLeftToRight]);
    cbSourceDirection.Items.Add(GLanguageHandler.Text[kRightToLeft]);
    cbSourceDirection.Items.Add('Sure 24x16');
  end;

  cbSourceLSB.ItemIndex       := 0;
  cbSourceDirection.ItemIndex := 0;

  if (Assigned(OnChange)) then
    OnChange(Nil);
end;


procedure TframeSimpleExport.cbSourceDirectionChange(Sender: TObject);
begin
  if (Assigned(OnChange)) then
    OnChange(Nil);
end;


procedure TframeSimpleExport.SetText(aText : string);
begin
  mData.Text := aText;
end;


procedure TframeSimpleExport.SetGUILanguageText;
begin
  Label2.Caption := GLanguageHandler.Text[kSimpleExport];

  cbSource.Items.Add(GLanguageHandler.Text[kRows]);
  cbSource.Items.Add(GLanguageHandler.Text[kColumns]);
  cbSource.ItemIndex := 0;

  if (LMSSettings.App.HexPrefix = '') then
    lHexPrefix.Caption := '<none>'
  else
    lHexPrefix.Caption := LMSSettings.App.HexPrefix;

  lHexPrefix.Hint := GLanguageHandler.Text[kHexFormat];

  cbSourceChange(Nil);
end;


end.
