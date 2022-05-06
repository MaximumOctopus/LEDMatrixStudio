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

unit layer;


interface


uses System.Generics.Collections,

     matrix;


type
  TLayer = class
    constructor Create(aName : string);
    destructor  Destory;
  private
    FName    : string;
    FVisible : boolean;
    FLocked  : boolean;
  public
    Frames   : TObjectList<TMatrix>;

    property Locked  : boolean read FLocked  write FLocked;
    property Name    : string  read FName    write FName;
    property Visible : boolean read FVisible write FVisible;
  end;


implementation


constructor TLayer.Create(aName : string);
begin
  FName    := aName;
  FVisible := True;

  Frames   := TObjectList<TMatrix>.Create;
end;


destructor TLayer.Destory;
begin
  Frames.Free;
end;


end.
