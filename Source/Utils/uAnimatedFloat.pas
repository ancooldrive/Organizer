unit uAnimatedFloat;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, LCLIntf, Math;

type
  TAnimatedFloatEvent = procedure(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double) of object;

  TAnimatedFloatType = (aftLinear, aftTangensH);

  { TAnimatedFloat }

  TAnimatedFloat = class
  private
    FTimer: TTimer;
    FAnimatedFloatType: TAnimatedFloatType;
    FStartTime, FTime: Cardinal;
    FStartValue, FStopValue: Double;
    FAnimatedFloatEvent: TAnimatedFloatEvent;
    procedure OnTimer(Sender: TObject);
    function GetIsRunning: Boolean;
    function Linear(Procent: Double): Double;
    function TangensH(Procent: Double): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Time: Cardinal read FTime write FTime;
    property StartValue: Double read FStartValue write FStartValue;
    property StopValue: Double read FStopValue write FStopValue;
    property IsRunning: Boolean read GetIsRunning;
    property AnimatedFloatType: TAnimatedFloatType read FAnimatedFloatType write FAnimatedFloatType;
    property AnimatedFloatEvent: TAnimatedFloatEvent read FAnimatedFloatEvent write FAnimatedFloatEvent;
  end;

implementation

{ TAnimatedFloat }

procedure TAnimatedFloat.OnTimer(Sender: TObject);
var
  vCurrentTime: Cardinal;
  vProcent: Double;
begin
  if Assigned(FAnimatedFloatEvent) then
  begin
    vProcent := 0;
    if FStopValue - FStartValue <> 0 then
    begin
      vCurrentTime := GetTickCount - FStartTime;
      if (vCurrentTime <> 0) then
        vProcent := vCurrentTime / FTime;
      if vProcent >= 1 then
      begin
        vProcent := 1;
        FTimer.Enabled := False;
      end;
    end else
      FTimer.Enabled := False;
    if vProcent = 1 then
      FAnimatedFloatEvent(Self, 100, 100, FStopValue)
    else
      case AnimatedFloatType of
        aftLinear: FAnimatedFloatEvent(Self, vProcent * 100, vProcent * 100, Linear(vProcent));
        aftTangensH: FAnimatedFloatEvent(Self, vProcent * 100, tanh(vProcent * 5) * 100, TangensH(vProcent));
      end;
  end else
    FTimer.Enabled := False;
end;

function TAnimatedFloat.GetIsRunning: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TAnimatedFloat.Linear(Procent: Double): Double;
begin
  Result := FStartValue + (FStopValue - FStartValue) * Procent;
end;

function TAnimatedFloat.TangensH(Procent: Double): Double;
begin
  Result := Linear(tanh(Procent * 5));
end;

constructor TAnimatedFloat.Create;
begin
  FTime := 0;
  FAnimatedFloatType := aftLinear;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := @OnTimer;
end;

destructor TAnimatedFloat.Destroy;
begin
  Stop;
  FTimer.Free;
  inherited Destroy;
end;

procedure TAnimatedFloat.Start;
begin
  FStartTime := GetTickCount;
  if Assigned(FAnimatedFloatEvent) then
    if FTime = 0 then
      FAnimatedFloatEvent(Self, 100, 100, FStopValue)
    else
      FTimer.Enabled := True;
end;

procedure TAnimatedFloat.Stop;
begin
  FTimer.Enabled := False;
end;

end.

