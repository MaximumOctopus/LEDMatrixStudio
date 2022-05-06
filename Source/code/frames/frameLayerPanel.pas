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

unit frameLayerPanel;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, System.UITypes, Vcl.Menus, thematrix,

  utility, languagehandler,

  matrixconstants, formAddLayer;


type
  TLayerMenu = procedure(aItem : integer) of object;


  TframeLayers = class(TFrame)
    Panel1: TPanel;
    sbLayersRename: TSpeedButton;
    sbDeleteLayer: TSpeedButton;
    sbAddNewLayer: TSpeedButton;
    sbLayerPanelClose: TSpeedButton;
    sbLayerUp: TSpeedButton;
    sbLayerDown: TSpeedButton;
    cbSyncAllLayers: TCheckBox;
    sgLayers: TStringGrid;
    puLayerTable: TPopupMenu;
    Clearselectedlayer1: TMenuItem;
    procedure sbLayersRenameClick(Sender: TObject);
    procedure sbAddNewLayerClick(Sender: TObject);
    procedure sbDeleteLayerClick(Sender: TObject);
    procedure sbLayerUpClick(Sender: TObject);
    procedure sbLayerDownClick(Sender: TObject);
    procedure sbLayerPanelCloseClick(Sender: TObject);
    procedure sgLayersClick(Sender: TObject);
    procedure sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure Clearselectedlayer1Click(Sender: TObject);
  private
    FOnClose : TNotifyEvent;
    FOnMenu  : TLayerMenu;

    procedure SetGUILanguageText;

    procedure SetSyncAll(aSync : boolean);
    function  GetSyncAll: boolean;
  public
    procedure UpdateLayerTable;

    property OnClose : TNotifyEvent read FOnClose   write FOnClose;
    property OnMenu  : TLayerMenu   read FOnMenu    write FOnMenu;
    property SyncAll : boolean      read GetSyncAll write SetSyncAll;
  end;


implementation


{$R *.dfm}


const
  CCellVisible = 0;
  CCellLocked  = 1;
  CCellName    = 2;


procedure TframeLayers.SetGUILanguageText;
begin
  cbSyncAllLayers.Caption := GLanguageHandler.Text[kSyncAll];
end;


procedure TframeLayers.UpdateLayerTable;
var
  t, lCount : integer;

begin
  lCount            := MatrixMain.GetLayerCount;

  sgLayers.RowCount := lCount + 1;

  for t := 0 to lCount - 1 do begin
    sgLayers.Cells[CCellName, 1 + lCount - 1 - t] := MatrixMain.GetLayerName(t);

    if MatrixMain.IsVisible(t) then
      sgLayers.Cells[CCellVisible, 1 + lCount - 1 - t] := 'x'
    else begin
      if lCount = 1 then begin
        MatrixMain.SetVisibility(t, True);

        sgLayers.Cells[CCellVisible, 1 + lCount - 1 - t] := 'x';
      end
      else
        sgLayers.Cells[CCellVisible, 1 + lCount - 1 - t] := '';
    end;

    if MatrixMain.IsLayerLocked(t) then
      sgLayers.Cells[CCellLocked, 1 + lCount - 1 - t] := 'L'
    else
      sgLayers.Cells[CCellLocked, 1 + lCount - 1 - t] := '';
  end;

  if (MatrixMain.SoftwareMode = smAnimation) and (lCount > 1) then
    sbDeleteLayer.Enabled := True
  else
    sbDeleteLayer.Enabled := False;

  if (MatrixMain.SoftwareMode = smAnimation) then
    sbAddNewLayer.Enabled := True
  else
    sbAddNewLayer.Enabled := False;

  sgLayers.Selection := TGridRect(Rect(0, 1 + lCount - 1 - MatrixMain.CurrentLayer, 1, 1 + lCount - 1 - MatrixMain.CurrentLayer));

  if (MatrixMain.CurrentLayer = 0) then
    sbLayerDown.Enabled := False
  else
    sbLayerDown.Enabled := True;

  if (MatrixMain.CurrentLayer = MatrixMain.GetLayerCount - 1) then
    sbLayerUp.Enabled := False
  else
    sbLayerUp.Enabled := True;
end;


procedure TframeLayers.SetSyncAll(aSync : boolean);
begin
  cbSyncAllLayers.Checked := True;
end;


procedure TframeLayers.sgLayersClick(Sender: TObject);
begin
  MatrixMain.CurrentLayer := sgLayers.RowCount - 1 - sgLayers.Selection.Top;

  if (MatrixMain.CurrentLayer = 0) then
    sbLayerDown.Enabled := False
  else
    sbLayerDown.Enabled := True;

  if (MatrixMain.CurrentLayer = MatrixMain.GetLayerCount - 1) then
    sbLayerUp.Enabled := False
  else
    sbLayerUp.Enabled := True;
end;


procedure TframeLayers.sgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  lLayer : integer;

begin
  CanSelect := True;

  if (ACol = 0) and (ARow > 0) then begin
    lLayer := sgLayers.RowCount - 1 - ARow;

    if sgLayers.Cells[ACol, ARow] = 'x' then begin
      sgLayers.Cells[ACol, ARow] := '';

      MatrixMain.SetVisibility(lLayer, False);
    end
    else begin
      sgLayers.Cells[ACol, ARow] := 'x';

      MatrixMain.SetVisibility(lLayer, True);
    end;
  end
  else if (ACol = 1) and (ARow > 0) then begin
    lLayer := sgLayers.RowCount - 1 - ARow;

    if sgLayers.Cells[ACol, ARow] = 'L' then begin
      sgLayers.Cells[ACol, ARow] := '';

      MatrixMain.UnlockLayer(lLayer);
    end
    else begin
      sgLayers.Cells[ACol, ARow] := 'L';

      MatrixMain.LockLayer(lLayer);
    end;
  end;
end;


procedure TframeLayers.Clearselectedlayer1Click(Sender: TObject);
begin
  if Assigned(FOnMenu) then
    FOnMenu(TMenuItem(Sender).Tag);
end;


function TframeLayers.GetSyncAll: boolean;
begin
  Result := cbSyncAllLayers.Checked;
end;


procedure TframeLayers.sbAddNewLayerClick(Sender: TObject);
var
  t : integer;
  lLayers : TStringList;
  lAddObject : TAddObject;

begin
  lLayers := TStringList.Create;

  for t := 0 to MatrixMain.GetLayerCount - 1 do
    lLayers.Add(MatrixMain.GetLayerName(t));

  lAddObject := DoAddLayer(lLayers);

  if (lAddObject.Process) then begin
    if lAddObject.Name <> '' then begin
      if lAddObject.CopyFrom then
        MatrixMain.AddLayerAsCopy(lAddObject.Name, lAddObject.CopyLayer)
      else
        MatrixMain.AddLayer(lAddObject.Name);
    end;
  end;

  lLayers.Free;
end;


procedure TframeLayers.sbDeleteLayerClick(Sender: TObject);
begin
  if MessageDlg(GLanguageHandler.Text[kDeleteCurrentLayer] + #13#13 + GLanguageHandler.Text[kThisCannotBeUndone], mtWarning, mbYesNo, 0) = mrYes then begin
    MatrixMain.DeleteLayer(sgLayers.RowCount - 1 - sgLayers.Selection.Top);
  end;
end;


procedure TframeLayers.sbLayerDownClick(Sender: TObject);
begin
  MatrixMain.MoveDown(MatrixMain.CurrentLayer);
end;


procedure TframeLayers.sbLayerPanelCloseClick(Sender: TObject);
begin
 if (Assigned(FOnClose)) then
   FOnClose(Self);
end;


procedure TframeLayers.sbLayersRenameClick(Sender: TObject);
var
  lLayerName : string;

begin
  if (sgLayers.Selection.Top >= 1) then begin

    lLayerName := InputBox(GLanguageHandler.Text[kChangeLayerName], GLanguageHandler.Text[kNewName], sgLayers.Cells[CCellLocked, sgLayers.Selection.Top]);

    if lLayerName <> '' then begin
      MatrixMain.SetLayerName(sgLayers.RowCount - 1 - sgLayers.Selection.Top, lLayerName);
    end;
  end;
end;


procedure TframeLayers.sbLayerUpClick(Sender: TObject);
begin
  MatrixMain.MoveUp(MatrixMain.CurrentLayer);
end;


end.
