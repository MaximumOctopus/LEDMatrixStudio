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


unit actionobject;


interface


uses System.Classes, System.SysUtils;


type
  TActionObjectBrush = record
                         BrushData         : TStringList;

                         Transparent       : boolean;
                         TransparentColour : integer;
                       end;


  TAutomateSource   = (acFirstFrame, acEachFrame, acEachFrameInc);
  TCyclingDirection = (cdForwards, cdBackwards);


  TActionObject = class
  public
    LastFileName          : string;

    ProcesingStage        : integer;

    FrameStart            : integer;
    FrameEnd              : integer;

    Layer                 : integer;

    Source                : TAutomateSource;

    EraseBehind           : boolean;

    ActionList            : TStringList;
    PostProcessList       : TStringList;

    Parameter1            : integer;
    Parameter2            : integer;
    ParameterReveal       : integer;
    ParameterRevealColour : integer;

    SourceColours         : TStringList;
    TargetColours         : TStringList;
    TargetSkip            : integer;
    TargetSkipIndex       : integer;

    CCSourceIndex         : integer;  //
    CCTargetIndex         : integer;
    CCDirection           : TCyclingDirection;

    Brushes               : array[0..1] of TActionObjectBrush;

    constructor Create;
    destructor  Destroy; Override;

    procedure   SetParameterReveal(aWidth, aHeight : integer);
  end;


const
  actionTypeRevealLeftRight = 30;
  actionTypeRevealRightLeft = 31;
  actionTypeRevealTopBottom = 32;
  actionTypeRevealBottomTop = 33;
  actionTypeRevealCentreOut = 34;
  actionTypeRevealCentreIn  = 35;


implementation


constructor TActionObject.Create;
begin
  ActionList           := TStringList.Create;
  PostProcessList      := TStringList.Create;
  Brushes[0].BrushData := TStringList.Create;
  Brushes[1].BrushData := TStringList.Create;

  SourceColours        := TStringList.Create;
  TargetColours        := TStringList.Create;

  Source               := acFirstFrame;

  Parameter1           := 0;
  Parameter2           := 0;
  ParameterReveal      := 0;

  ProcesingStage       := 0;

  LastFileName         := '';
end;


procedure TActionObject.SetParameterReveal(aWidth, aHeight : integer);
var
  t, lSource : integer;

begin
  for t := 0 to ActionList.Count - 1 do begin

    lSource := StrToInt(ActionList[t]);

    case lSource of
      actionTypeRevealLeftRight  : ParameterReveal := 0;
      actionTypeRevealRightLeft  : ParameterReveal := aWidth;
      actionTypeRevealTopBottom  : ParameterReveal := 0;
      actionTypeRevealBottomTop  : ParameterReveal := aHeight;
      actionTypeRevealCentreOut  : ParameterReveal := 0; // to do
      actionTypeRevealCentreIn   : ParameterReveal := 0;
    end;
  end;
end;


destructor TActionObject.Destroy;
begin
  TargetColours.Free;
  SourceColours.Free;

  Brushes[0].BrushData.Free;
  Brushes[1].BrushData.Free;
  PostProcessList.Free;
  ActionList.Free;
end;


end.
