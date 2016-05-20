unit uOrganizerControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Controls, StdCtrls, ExtCtrls, Graphics, Grids,
  Forms, LMessages, LCLType,
  BCLabel, BCTypes, BCPanel, BGRABitmapTypes, BGRABitmap, BGRAGraphicControl,

  uAnimatedFloat,
  uOrganizerForm;

type
  TOrganizerControlType = (octDefault, octMenuButton);

  { TOrganizerCustomControl }

  TOrganizerCustomControl = class(TComponent)
  private
    FOwner: TWinControl;
  protected
    procedure InitControl; virtual;
  public
    constructor Create(TheOwner: TWinControl); reintroduce;

    property Owner: TWinControl read FOwner;
  end;

  { TOrganizerMenuButton }

  TOrganizerMenuButton = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FBackground, FBorder, FImage: TImage;
    FCaptionLabel: TBCLabel;
    FMouseOverAnimation: TAnimatedFloat;
    FLeftColor: TColor;
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure MouseOverAnimationEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; Caption: String; Picture: String;
      LeftColor: TColor); overload;
    destructor Destroy; override;
  end;

  { TOrganizerEdit }

  TOrganizerEdit = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FEditContainer: TPanel;
    FEdit: TEdit;
    FEditBackground: TPanel;
    FLabel: TLabel;
    FDefaultText: String;
    procedure OnClick(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; DefaultText: String); overload;
  end;

  { TOrganizerCheckBox }

  TOrganizerCheckBox = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FImage: TBGRAGraphicControl;
    FTextLeftMargin: TImage;
    FLabel: TBCLabel;
    FChecked: Boolean;
    FCheckedPicture: String;
    procedure SetChecked(Value: Boolean);
    procedure OnClick(Sender: TObject);
    procedure OnRedrawEvent(Sender: TObject; Bitmap: TBGRABitmap);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; Caption, CheckedPicture: String); overload;

    property Checked: Boolean read FChecked write SetChecked;
  end;

  { TOrganizerButton }

  TOrganizerButton = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FBackground: TImage;
    FLabel: TBCLabel;
    FCurrentAnimationValue: Integer;
    FMouseOverAnimation: TAnimatedFloat;
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseOverAnimationEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; Caption: String); overload;
    destructor Destroy; override;
  end;

  { TOrganizerCancelButton }

  TOrganizerCancelButton = class(TOrganizerButton)
  private
    procedure OnBackgroundResize(Sender: TObject);
    procedure MouseOverAnimationEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; Caption: String); overload;
  end;

  {TOrganizerComboBoxList}

  TOrganizerComboBoxList = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FItems: TList;
    FStringItems: Array of String;
    procedure OnItemMouseEnter(Sender: TObject);
    procedure OnItemMouseLeave(Sender: TObject);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl; Items: Array of String); reintroduce;
    destructor Destroy; override;
  end;

  { TOrganizerComboBox }

  TOrganizerComboBox = class(TOrganizerButton)
  private
    FExpanderImage: TImage;
    procedure OnClick(Sender: TObject);
  protected
    procedure InitControl; override;
  public
  end;

  { TOrganizerScrollBox }

  TOrganizerScrollBox = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FContent: TPanel;
    FScrollBox: TPanel;
    FScrollBoxBackground: TImage;
    FScrollBar: TImage;
    FScrollBoxMoveTimer: TTimer;
    FAnimatedFloat: TAnimatedFloat;
    FMouseDownPosY: Integer;
    procedure MouseWheelUpEvent(Sender: TObject; Shift: TShiftState;
              MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelDownEvent(Sender: TObject; Shift: TShiftState;
              MousePos: TPoint; var Handled: Boolean);
    procedure OnScrollBarResize(Sender: TObject);
    procedure OnScrollBoxBackgroundResize(Sender: TObject);
    procedure OnScrollBoxMoveTimer(Sender: TObject);
    procedure OnScrollBarMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBarMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBoxMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBarChangeBounds(Sender: TObject);
    procedure AnimatedFloatEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
    procedure CalculateScrollBarHeight;
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

  { TOrganizerListBox }

  TOrganizerListBox = class(TOrganizerCustomControl)
  private
    FContainer: TPanel;
    FHeaderPanel: TPanel;
    FHeaderLabel: TLabel;
    FBorderImage: TImage;
    FGridPanel: TPanel;
    FScrollBarBackround: TImage;
    FScrollBar: TImage;
    FDrawGrid: TDrawGrid;
    FScrollBarMoveTimer: TTimer;
    FScrollMouseDownClick: TPoint;
    FCorrenctScrollPositionAnimation: TAnimatedFloat;
    FResizeScrollAnimation: TAnimatedFloat;
    FSetTopRowAnimation: TAnimatedFloat;
    procedure OnScrollBarMoveTimer(Sender: TObject);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBarResize(Sender: TObject);
    procedure OnScrollBarBackroundResize(Sender: TObject);
    procedure OnTopLeftChanged(Sender: TObject);
    procedure OnDrawCell(Sender: TObject; aCol, aRow: Integer;
              aRect: TRect; aState: TGridDrawState);
    procedure OnCorrenctScrollPositionAnimationEvent(Sender: TObject;
              CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
    procedure OnResizeScrollAnimationEvent(Sender: TObject;
              CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
    procedure OnSetTopRowAnimationEvent(Sender: TObject;
              CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
    procedure ResizeUpScrollBar(Sender: TObject);
    procedure ResizeDownScrollBar(Sender: TObject);
    function Calc1: Double;
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

  TOrganizerCalendar = class;
  TOrganizerCalendarCell = class;
  TOrganizerCellEvent = class;

  { TOrganizerCellEvent }

  TOrganizerCellEvent = class(TPanel)
  private
    FMoveTimer: TTimer;
    FMouseDownLocalPos: TPoint;
    FControlPos: TPoint;
    FCorrectPosAnimation: TAnimatedFloat;
    FOrganizerCalendar: TOrganizerCalendar;
    FOrganizerCalendarCell: TOrganizerCalendarCell;
    FOrganizerCalendarCellParent: TOrganizerCalendarCell;
    FCorrectPosStartX,
    FCorrectPosStartY,
    FCorrectPosStopX,
    FCorrectPosStopY: Integer;
    FNewTop: Integer;

    //Data
    FId: Integer;
    FDateTime: TDateTime;

    procedure SetDateTime(DateTime: TDateTime);

    procedure OnMoveTimer(Sender: TObject);
    procedure OnCorrectPosAnimationEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
  protected
    procedure Paint; override;
    procedure BoundsChanged; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(OrganizerCalendar: TOrganizerCalendar); reintroduce;
    destructor Destroy; override;

    property ID: Integer read FId;
    property DateTime: TDateTime read FDateTime write SetDateTime;
  end;

  { TOrganizerCalendarCell }

  TOrganizerCalendarCell = class(TPanel)
  private
    FOrganizerCalendar: TOrganizerCalendar;
    FDate: TDate;
    FDayNr: TLabel;
    FMinHeight: Integer;
    FAnimatedFloat: TAnimatedFloat;
    FPointInMainForm: TPoint;
    FCellEventList: TList;
    function GetComponentsHeight: Integer;
    procedure SetPointInMainForm;
    procedure SetDate(Value: TDate);
    procedure OnAnimatedFloat(Sender: TObject;
              CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
    procedure UpdateEvents;
    procedure SetNewTop(OrganizerCellEvent: TOrganizerCellEvent);
    procedure SortEvents;
    procedure AddEvent(OrganizerCellEvent: TOrganizerCellEvent); overload;
    procedure RemoveEvent(OrganizerCellEvent: TOrganizerCellEvent);
  protected
    procedure Resize; override;
  public
    constructor Create(OrganizerCalendar: TOrganizerCalendar); reintroduce;
    destructor Destroy; override;

    property Date: TDate read FDate write SetDate;
  end;

  { TOrganizerCalendar }

  TOrganizerCalendarSelectDataEvent = procedure(Sender: TObject; FromDate, ToDate: TDateTime) of object;
  TOrganizerCalendarChangeDataEvent = procedure(Sender: TObject; OrganizerCellEvent: TOrganizerCellEvent; var Success: Boolean) of object;

  TOrganizerCalendar = class(TOrganizerCustomControl)
  private
    FDate,
    FFromDate,
    FToDate: TDate;
    FFirstDayWeekOfMonth: Byte;
    FCalendarDataEventList: TList;
    FOrganizerScrollBox: TOrganizerScrollBox;
    FHeader: array [0..6] of TPanel;
    FCells: array [0..5, 0..6] of TOrganizerCalendarCell;
    FRows: Integer;
    FSundayFirst: Boolean;
    FLoadingData: Boolean;
    FActiveMonthFontColor,
    FNonActiveMonthFontColor: TColor;

    //Events
    FOnSelectDataEvent: TOrganizerCalendarSelectDataEvent;
    FOnInsertDataEvent,
    FOnUpdateDataEvent,
    FOnDeleteDataEvent: TOrganizerCalendarChangeDataEvent;

    function GetLongDayNames(DayIndex: Integer): String;
    function GetFirstDayWeekOfMonth: Integer;
    function GetDayCountOfMounth: Integer;
    procedure SetDate(Value: TDate);
    procedure SetFromToDates;
    procedure OnResize(Sender: TObject);
    function GetOrganizerCellEventById(ID: Integer): TOrganizerCellEvent;
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;

    procedure ClearDataEvents;
    procedure AddDataEvent(ID: Integer; DateTime: TDateTime); overload;
    procedure RemoveDataEvent(Value: TOrganizerCellEvent);

    procedure UpdateCalendar;

    property Date: TDate read FDate write SetDate;
    property FromDate: TDate read FFromDate;
    property ToDate: TDate read FToDate;

    property OnSelectDataEvent: TOrganizerCalendarSelectDataEvent read FOnSelectDataEvent write FOnSelectDataEvent;
    property OnInsertDataEvent: TOrganizerCalendarChangeDataEvent read FOnInsertDataEvent write FOnInsertDataEvent;
    property OnUpdateDataEvent: TOrganizerCalendarChangeDataEvent read FOnUpdateDataEvent write FOnUpdateDataEvent;
    property OnDeleteDataEvent: TOrganizerCalendarChangeDataEvent read FOnDeleteDataEvent write FOnDeleteDataEvent;
  end;

  { TOrganizerContent }

  TOrganizerContent = class(TOrganizerCustomControl)
  private
    FCurrentContent: TWinControl;
    FNextContent: TWinControl;
    FAnimatedFloat: TAnimatedFloat;

    procedure SetContent(Value: TWinControl);
    procedure OnAnimatedFloatEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
  protected
    procedure InitControl; override;
  public
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;

    property Content: TWinControl read FCurrentContent write SetContent;
  end;

const
  cPrimaryColor = $00A76177;

implementation

const

  //Colors
  cMenuBackgroundColor = $00555555;
  cMenuBackgroundMouseOverColor = $00333333;
  cMenuFontColor = $00C8C8C8;

  cBevelColor = $00C8C8C8;

  //fonts
  cMenuFontName = 'Raleway';

function AddToColor(Color: TColor; Value: Integer): TColor;
  function AddValue(Old, New: Integer): Byte;
  var
    vTemp: Integer;
  begin
    vTemp := Old + New;
    if vTemp > 255 then
      vTemp := 255
    else
      if vTemp < 0 then
        vTemp := 0;
    Result := vTemp;
  end;
var
  vR, vG, vB: Byte;
begin
  vR := AddValue(Red(Color), Value);
  vG := AddValue(Green(Color), Value);
  vB := AddValue(Blue(Color), Value);
  Result := RGBToColor(vR, vG, vB);
end;

{ TOrganizerComboBoxList }

procedure TOrganizerComboBoxList.OnItemMouseEnter(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Color := cPrimaryColor;
    Font.Color := clWhite;
  end;
end;

procedure TOrganizerComboBoxList.OnItemMouseLeave(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Color := clWhite;
    Font.Color := clBlack;
  end;
end;

procedure TOrganizerComboBoxList.InitControl;
const
  vItemHeight = 25;
var
  I: Integer;
  vPanelItem: TPanel;
begin
  inherited InitControl;
  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Width := Owner.Width;
  FContainer.Height := vItemHeight * Length(FStringItems) + 2;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.Caption := '';
  FContainer.BorderWidth := 1;
  FContainer.Color := cBevelColor;

  for I := High(FStringItems) downto Low(FStringItems) do
  begin
    vPanelItem := TPanel.Create(FContainer);
    vPanelItem.Parent := FContainer;
    vPanelItem.Align := alTop;
    vPanelItem.Height := vItemHeight;
    vPanelItem.BevelOuter := TPanelBevel.bvNone;
    vPanelItem.Caption := FStringItems[I];
    vPanelItem.Font.Color := clBlack;
    vPanelItem.Color := clWhite;
    vPanelItem.OnMouseEnter := @OnItemMouseEnter;
    vPanelItem.OnMouseLeave := @OnItemMouseLeave;
  end;
end;

constructor TOrganizerComboBoxList.Create(TheOwner: TWinControl;
  Items: array of String);
var
  I: Integer;
begin
  FItems := TList.Create;
  SetLength(FStringItems, Length(Items));
  for I := Low(Items) to High(Items) do
    FStringItems[I] := Items[I];
  inherited Create(TheOwner);
end;

destructor TOrganizerComboBoxList.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

{ TOrganizerEdit }

procedure TOrganizerEdit.OnClick(Sender: TObject);
begin
  FEdit.SetFocus;
end;

procedure TOrganizerEdit.OnEditChange(Sender: TObject);
begin
  FEditBackground.Visible := FEdit.Text = '';
end;

procedure TOrganizerEdit.InitControl;
begin
  inherited InitControl;

  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.Caption := '';
  FContainer.BorderWidth := 1;
  FContainer.Color := cBevelColor;

  FEditContainer := TPanel.Create(FContainer);
  FEditContainer.Parent := FContainer;
  FEditContainer.Color := clRed;
  FEditContainer.Align := alClient;
  FEditContainer.BevelOuter := TPanelBevel.bvNone;
  FEditContainer.Caption := '';

  FEdit := TEdit.Create(FEditContainer);
  FEdit.Parent := FEditContainer;
  FEdit.BorderStyle := bsNone;
  FEdit.Align := alClient;
  FEdit.Font.Size := 13;
  FEdit.OnChange := @OnEditChange;

  FEditBackground := TPanel.Create(FContainer);
  FEditBackground.Parent := FContainer;
  FEditBackground.Left := FEditContainer.Left + 6;
  FEditBackground.Top := FEditContainer.Top + 1;
  FEditBackground.Width := FEditContainer.Width - 7;
  FEditBackground.Height := FEditContainer.Height - 2;
  FEditBackground.Anchors := [akBottom, akLeft, akRight, akTop];
  FEditBackground.BevelOuter := TPanelBevel.bvNone;
  FEditBackground.Color := clWhite;
  FEditBackground.Caption := '';

  FLabel := TLabel.Create(FEditBackground);
  FLabel.Parent := FEditBackground;
  FLabel.Align := alClient;
  FLabel.Caption := FDefaultText;
  FLabel.Font.Color := cBevelColor;
  FLabel.Layout := TTextLayout.tlCenter;
  FLabel.Cursor := crIBeam;
  FLabel.OnClick := @OnClick;
  FLabel.Font.Size := 13;
end;

constructor TOrganizerEdit.Create(TheOwner: TWinControl; DefaultText: String);
begin
  FDefaultText := DefaultText;
  inherited Create(TheOwner);
end;

{ TOrganizerCellEvent }

procedure TOrganizerCellEvent.SetDateTime(DateTime: TDateTime);
var
  I, J: Integer;
begin
  if FDateTime <> DateTime then
  begin
    FDateTime := DateTime;
    FOrganizerCalendarCellParent.RemoveEvent(Self);
    FOrganizerCalendarCellParent := Nil;
    FOrganizerCalendarCell := Nil;
    Parent := nil;
    for I := 0 to FOrganizerCalendar.FRows do
      for J := 0 to 6 do
        FOrganizerCalendar.FCells[I, J].UpdateEvents;
  end;
end;

procedure TOrganizerCellEvent.OnMoveTimer(Sender: TObject);
var
  vMousePos: TPoint;
  vTempPoint: TPoint;
begin
  if Parent <> Application.MainForm then
    Parent := Application.MainForm;
  vMousePos := Mouse.CursorPos;
  vTempPoint := Application.MainForm.ScreenToClient(vMousePos);
  Left := vTempPoint.X - FMouseDownLocalPos.X;
  Top := vTempPoint.Y - FMouseDownLocalPos.Y;
end;

procedure TOrganizerCellEvent.OnCorrectPosAnimationEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  if CurrentProcent <> 0 then
  begin
    Left := Round(FCorrectPosStartX + (FCorrectPosStopX - FCorrectPosStartX) * (CurrentProcent / 100));
    Top := Round(FCorrectPosStartY + (FCorrectPosStopY - FCorrectPosStartY) * (CurrentProcent / 100));
  end;
  if (CurrentProgressProcent = 100) and Assigned(FOrganizerCalendarCellParent) then
  begin
    Parent := FOrganizerCalendarCellParent;
    Top := FNewTop;
    Left := 0;
    Anchors := [akLeft, akRight, akTop];
    FOrganizerCalendarCellParent.SortEvents;
  end;
end;

procedure TOrganizerCellEvent.Paint;
begin
  inherited Paint;
  Canvas.Brush.Color := cPrimaryColor;
  Canvas.Pen.Color := cPrimaryColor;
  Canvas.RoundRect(1, 1, Canvas.Width - 1, Canvas.Height - 1, 5, 5);
end;

procedure TOrganizerCellEvent.BoundsChanged;
var
  I, J: Integer;
  vPoint: TPoint;
begin
  inherited BoundsChanged;
  if Assigned(FOrganizerCalendar)
    and (Parent = Application.MainForm)
    and not FCorrectPosAnimation.IsRunning
    and FMoveTimer.Enabled
  then
  begin
    FOrganizerCalendarCell := FOrganizerCalendarCellParent;
    for I := 0 to FOrganizerCalendar.FRows do
      for J := 0 to 6 do
      begin
        vPoint := FOrganizerCalendar.FCells[I,J].FPointInMainForm;

        if (Left + FMouseDownLocalPos.X >= vPoint.X)
          and (Top + FMouseDownLocalPos.Y >= vPoint.Y)
          and (Left + FMouseDownLocalPos.X <= vPoint.X + FOrganizerCalendar.FCells[I,J].Width)
          and (Top + FMouseDownLocalPos.Y <= vPoint.Y + FOrganizerCalendar.FCells[I,J].Height)
        then
        begin
          FOrganizerCalendarCell := FOrganizerCalendar.FCells[I,J];
          FOrganizerCalendar.FCells[I,J].Color := clCream
        end
        else
          FOrganizerCalendar.FCells[I,J].Color := clWhite;
      end;
  end;
end;

procedure TOrganizerCellEvent.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  vSuccess: Boolean;
  vTempDate: TDateTime;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMoveTimer.Enabled then
  begin
    FMoveTimer.Enabled := False;

    FCorrectPosAnimation.Stop;
    FCorrectPosStartX := Left;
    FCorrectPosStartY := Top;
    FCorrectPosAnimation.StartValue := 0;
    FCorrectPosAnimation.StopValue := 100;

    if FOrganizerCalendarCellParent <> FOrganizerCalendarCell then
    begin
      vSuccess := True;
      vTempDate := FDateTime;
      try
        FDateTime := FOrganizerCalendarCell.Date + (FDateTime - Int(FDateTime) - 1);  //TODO
        if Assigned(FOrganizerCalendar.OnUpdateDataEvent) then
          FOrganizerCalendar.OnUpdateDataEvent(FOrganizerCalendar, Self, vSuccess);
        if vSuccess then
        begin
          FOrganizerCalendarCellParent.RemoveEvent(Self);
          FOrganizerCalendarCellParent := FOrganizerCalendarCell;
          FOrganizerCalendarCellParent.AddEvent(Self);
          FOrganizerCalendarCellParent.FOrganizerCalendar.OnResize(FOrganizerCalendarCellParent.FOrganizerCalendar);
          FOrganizerCalendarCell.SetPointInMainForm;
        end;
      finally
        if not vSuccess then
          FDateTime := vTempDate;
      end;
    end;

    FOrganizerCalendarCell.Color := clWhite;
    FCorrectPosStopX := FOrganizerCalendarCellParent.FPointInMainForm.X;
    FCorrectPosStopY := FOrganizerCalendarCellParent.FPointInMainForm.Y + FNewTop;
    FCorrectPosAnimation.Start;
  end;
end;

procedure TOrganizerCellEvent.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I, J: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  FCorrectPosAnimation.Stop;
  if Parent <> Application.MainForm then
  begin
    FNewTop := Top;
    for I := 0 to FOrganizerCalendar.FRows do
      for J := 0 to 6 do
        FOrganizerCalendar.FCells[I, J].SetPointInMainForm;
    Anchors := [];
  end;
  FMouseDownLocalPos := Point(X, Y);
  FControlPos := ClientToParent(Point(0,0), Application.MainForm);
  FMoveTimer.Enabled := True;
end;

constructor TOrganizerCellEvent.Create(OrganizerCalendar: TOrganizerCalendar);
begin
  FOrganizerCalendar := nil;
  FOrganizerCalendarCellParent := nil;
  FOrganizerCalendarCell := nil;
  inherited Create(FOrganizerCalendar);
  BevelOuter := TPanelBevel.bvNone;
  Caption := '';
  BorderWidth := 1;

  FOrganizerCalendar := OrganizerCalendar;

  FMoveTimer := TTimer.Create(nil);
  FMoveTimer.Interval := 1;
  FMoveTimer.Enabled := False;
  FMoveTimer.OnTimer := @OnMoveTimer;

  FCorrectPosAnimation := TAnimatedFloat.Create;
  FCorrectPosAnimation.Time := 1000;
  FCorrectPosAnimation.AnimatedFloatType := aftTangensH;
  FCorrectPosAnimation.AnimatedFloatEvent := @OnCorrectPosAnimationEvent;
end;

destructor TOrganizerCellEvent.Destroy;
begin
  if Assigned(FOrganizerCalendarCell) then
    FOrganizerCalendarCell.FCellEventList.Remove(Self);

  FMoveTimer.Free;
  inherited Destroy;
end;

{ TOrganizerScrollBox }

procedure TOrganizerScrollBox.MouseWheelUpEvent(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FAnimatedFloat.Stop;
  FAnimatedFloat.StartValue := FScrollBar.Top;
  FAnimatedFloat.StopValue := FScrollBar.Top - Round(300 / (FContent.Height / FContainer.Height));
  if FAnimatedFloat.StopValue < 0 then
    FAnimatedFloat.StopValue := 0;
  FAnimatedFloat.Start;
end;

procedure TOrganizerScrollBox.MouseWheelDownEvent(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FAnimatedFloat.Stop;
  FAnimatedFloat.StartValue := FScrollBar.Top;
  FAnimatedFloat.StopValue := FScrollBar.Top + Round(300 / (FContent.Height / FContainer.Height));
  if FAnimatedFloat.StopValue > FScrollBox.Height - FScrollBar.Height then
    FAnimatedFloat.StopValue := FScrollBox.Height - FScrollBar.Height;
  FAnimatedFloat.Start;
end;

procedure TOrganizerScrollBox.OnScrollBarResize(Sender: TObject);
begin
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := cPrimaryColor;
    Canvas.FillRect(Canvas.ClipRect);
  end;
end;

procedure TOrganizerScrollBox.OnScrollBoxBackgroundResize(Sender: TObject);
begin
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := AddToColor(cBevelColor, 40);
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Pen.Color := cBevelColor;
    Canvas.Rectangle(Canvas.ClipRect);
  end;
end;

procedure TOrganizerScrollBox.OnScrollBoxMoveTimer(Sender: TObject);
begin
  FScrollBar.Top := Mouse.CursorPos.Y - FMouseDownPosY;
end;

procedure TOrganizerScrollBox.OnScrollBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FScrollBoxMoveTimer.Enabled := False;
end;

procedure TOrganizerScrollBox.OnScrollBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAnimatedFloat.Stop;
  FScrollBoxMoveTimer.Enabled := False;
  FMouseDownPosY := Mouse.CursorPos.Y - FScrollBar.Top;
  FScrollBoxMoveTimer.Enabled := True;
end;

procedure TOrganizerScrollBox.OnScrollBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAnimatedFloat.Stop;
  FAnimatedFloat.StartValue := FScrollBar.Top;
  FAnimatedFloat.StopValue := Y - (FScrollBar.Height / 2);
  FAnimatedFloat.Start;
end;

procedure TOrganizerScrollBox.OnScrollBarChangeBounds(Sender: TObject);
var
  vTop: Integer;
begin
  vTop := FScrollBar.Top;
  if vTop + FScrollBar.Height > FScrollBox.Height then
    vTop := FScrollBox.Height - FScrollBar.Height;
  if vTop < 0 then
    vTop := 0;
  FScrollBar.Top := vTop;
  FContent.Anchors := [];
  try
    FContent.Top := -1 * Round(FScrollBar.Top * FContent.Height / FContainer.Height);
    if FScrollBar.Top + FScrollBar.Height = FScrollBox.Height then
      FContent.Top := FContainer.Height - FContent.Height - 1;
  finally
    FContent.Anchors := [akBottom, akLeft, akRight, akTop];
  end;
end;

procedure TOrganizerScrollBox.AnimatedFloatEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FScrollBar.Top := Round(CurrentValue);
end;

procedure TOrganizerScrollBox.CalculateScrollBarHeight;
var
  vTempWidth: Integer;
begin
  FScrollBar.Height := Round(FContainer.Height / (FContent.Height / FContainer.Height));
  FScrollBox.Visible := FScrollBar.Height < FContainer.Height;

  vTempWidth := FScrollBox.Width;
  if FScrollBox.Visible then
    FScrollBox.Width := 10
  else
    FScrollBox.Width := 0;
  if vTempWidth <> FScrollBox.Width then
    FContent.Width := FContainer.Width - FScrollBox.Width - 1;
end;

procedure TOrganizerScrollBox.InitControl;
begin
  inherited InitControl;

  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.Caption := '';
  FContainer.OnMouseWheelUp := @MouseWheelUpEvent;
  FContainer.OnMouseWheelDown := @MouseWheelDownEvent;

  FScrollBox := TPanel.Create(FContainer);
  FScrollBox.Parent := FContainer;
  FScrollBox.Align := alRight;
  FScrollBox.Width := 0;
  FScrollBox.BevelOuter := TPanelBevel.bvNone;
  FScrollBox.Caption := '';
  FScrollBox.Color := clBlack;
  FScrollBox.BorderWidth := 0;
  FScrollBox.Visible := False;

  FContent := TPanel.Create(FContainer);
  FContent.Parent := FContainer;
  FContent.BevelOuter := TPanelBevel.bvNone;

  FScrollBoxBackground := TImage.Create(FScrollBox);
  FScrollBoxBackground.Parent := FScrollBox;
  FScrollBoxBackground.OnResize := @OnScrollBoxBackgroundResize;
  FScrollBoxBackground.OnMouseDown := @OnScrollBoxMouseDown;
  FScrollBoxBackground.Align := alClient;

  FScrollBar := TImage.Create(FScrollBox);
  FScrollBar.Parent := FScrollBox;
  FScrollBar.OnResize := @OnScrollBarResize;
  FScrollBar.Left := 0;
  FScrollBar.Width := FScrollBoxBackground.Width;
  FScrollBar.OnMouseUp := @OnScrollBarMouseUp;
  FScrollBar.OnMouseDown := @OnScrollBarMouseDown;
  FScrollBar.OnChangeBounds := @OnScrollBarChangeBounds;

  FScrollBar.Height := 100;
end;

constructor TOrganizerScrollBox.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);
  FScrollBoxMoveTimer := TTimer.Create(nil);
  FScrollBoxMoveTimer.Interval := 1;
  FScrollBoxMoveTimer.Enabled := False;
  FScrollBoxMoveTimer.OnTimer := @OnScrollBoxMoveTimer;

  FAnimatedFloat := TAnimatedFloat.Create;
  FAnimatedFloat.Time := 1000;
  FAnimatedFloat.AnimatedFloatType := aftTangensH;
  FAnimatedFloat.AnimatedFloatEvent := @AnimatedFloatEvent;
end;

destructor TOrganizerScrollBox.Destroy;
begin
  FAnimatedFloat.Free;
  FScrollBoxMoveTimer.Free;
  inherited Destroy;
end;

{ TOrganizerCalendarCell }

function TOrganizerCalendarCell.GetComponentsHeight: Integer;
var
  I: Integer;
begin
  if Assigned(FDayNr) then
    Result := FDayNr.Height
  else
    Result := 0;
  for I := 0 to FCellEventList.Count - 1 do
    Result := Result + TOrganizerCellEvent(FCellEventList[I]).Height;
end;

procedure TOrganizerCalendarCell.SetPointInMainForm;
begin
  if Assigned(Parent) then
    FPointInMainForm := ClientToParent(Point(0,0), Application.MainForm);
end;

procedure TOrganizerCalendarCell.SetDate(Value: TDate);
var
  I: Integer;
begin
  if FDate <> Value then
  begin
    FDate := Value;
    if MonthOf(FDate) = MonthOf(FOrganizerCalendar.Date) then
      FDayNr.Font.Color := FOrganizerCalendar.FActiveMonthFontColor
    else
      FDayNr.Font.Color := FOrganizerCalendar.FNonActiveMonthFontColor;
    FDayNr.Caption := IntToStr(DayOfTheMonth(FDate));
  end;
  UpdateEvents;
end;

procedure TOrganizerCalendarCell.OnAnimatedFloat(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FMinHeight := Round(CurrentValue);
  FDayNr.Caption := IntToStr(FMinHeight);
  FOrganizerCalendar.OnResize(nil);
end;

procedure TOrganizerCalendarCell.UpdateEvents;
var
  I: Integer;
  vAddedEvent: Boolean;
begin
  vAddedEvent := False;
  with FOrganizerCalendar do
    for I := 0 to FCalendarDataEventList.Count - 1 do
      with TOrganizerCellEvent(FCalendarDataEventList[I]) do
        if not Assigned(Parent)
          and (Int(FDateTime) = Int(Self.FDate))
        then
        begin
          FOrganizerCalendarCell := Self;
          FOrganizerCalendarCellParent := Self;
          Width := Self.Width;
          Parent := Self;
          FCellEventList.Add(FCalendarDataEventList[I]);
          vAddedEvent := True;
        end;
  if vAddedEvent then
    SortEvents;
end;

procedure TOrganizerCalendarCell.SetNewTop(
  OrganizerCellEvent: TOrganizerCellEvent);
var
  I: Integer;
begin
  OrganizerCellEvent.FNewTop := FDayNr.Top + FDayNr.Height;
  for I := 0 to FCellEventList.Count - 1 do
    if TOrganizerCellEvent(FCellEventList[I]) <> OrganizerCellEvent then
      OrganizerCellEvent.FNewTop := OrganizerCellEvent.FNewTop + TOrganizerCellEvent(FCellEventList[I]).Height;
end;

procedure TOrganizerCalendarCell.SortEvents;
var
  I: Integer;
  vHeight: Integer;
begin
  for I := 0 to FCellEventList.Count - 1 do
    TOrganizerCellEvent(FCellEventList[I]).Top := I * TOrganizerCellEvent(FCellEventList[I]).Height + FDayNr.Height;
end;

procedure TOrganizerCalendarCell.Resize;
var
  I: Integer;
begin
  inherited Resize;
  FMinHeight := GetComponentsHeight;
  for I := 0 to FCellEventList.Count - 1 do
    TOrganizerCellEvent(FCellEventList[I]).Width := Width;
end;

constructor TOrganizerCalendarCell.Create(OrganizerCalendar: TOrganizerCalendar);
begin
  FOrganizerCalendar := OrganizerCalendar;
  FDayNr := nil;
  FCellEventList := TList.Create;
  inherited Create(FOrganizerCalendar);
  Parent := FOrganizerCalendar.FOrganizerScrollBox.FContent;

  FDate := 0;
  FMinHeight := 0;

  FDayNr := TLabel.Create(Self);
  FDayNr.Parent := Self;
  FDayNr.Top := 0;
  FDayNr.Left := 0;
  FDayNr.Width := Width - 5;
  FDayNr.Height := 15;
  FDayNr.Anchors := [akTop, akLeft, akRight];
  FDayNr.Alignment := taRightJustify;

  FAnimatedFloat := TAnimatedFloat.Create;
  FAnimatedFloat.Time := 1000;
  FAnimatedFloat.AnimatedFloatEvent := @OnAnimatedFloat;
end;

destructor TOrganizerCalendarCell.Destroy;
begin
  FAnimatedFloat.Free;
  FCellEventList.Free;
  inherited Destroy;
end;

//procedure TOrganizerCalendarCell.AddEvent(CalendarDataObject: TCalendarDataObject);
//var
//  vOrganizerCellEvent: TOrganizerCellEvent;
//begin
//  vOrganizerCellEvent := TOrganizerCellEvent.Create(Self);
//  vOrganizerCellEvent.Parent := Self;
//  vOrganizerCellEvent.Width := Self.Width;
//  vOrganizerCellEvent.Height := 20;
//  vOrganizerCellEvent.Anchors := [akLeft, akRight, akTop];
//  CalendarDataObject.OrganizerCellEvent := vOrganizerCellEvent;
//  AddEvent(vOrganizerCellEvent);
//  vOrganizerCellEvent.Top := vOrganizerCellEvent.FNewTop;
//end;

procedure TOrganizerCalendarCell.AddEvent(
  OrganizerCellEvent: TOrganizerCellEvent);
begin
  FCellEventList.Add(OrganizerCellEvent);
  SetNewTop(OrganizerCellEvent);
  FMinHeight := GetComponentsHeight;
end;

procedure TOrganizerCalendarCell.RemoveEvent(
  OrganizerCellEvent: TOrganizerCellEvent);
begin
  FCellEventList.Remove(OrganizerCellEvent);
  SortEvents;
  FMinHeight := GetComponentsHeight;
end;

{ TOrganizerCalendar }

function TOrganizerCalendar.GetLongDayNames(DayIndex: Integer): String;
var
  vTemp: Integer;
begin
  if FSundayFirst then
    Result := LongDayNames[DayIndex]
  else
  begin
    vTemp := DayIndex + 1;
    if vTemp > 7 then
      vTemp := 1;
    Result := LongDayNames[vTemp];
  end;
end;

function TOrganizerCalendar.GetFirstDayWeekOfMonth: Integer;
begin
  if FSundayFirst then
    Result := DayOfWeek(IncDay(FDate, - DayOfTheMonth(FDate) + 1))
  else
    Result := DayOfTheWeek(IncDay(FDate, - DayOfTheMonth(FDate) + 1));
end;

function TOrganizerCalendar.GetDayCountOfMounth: Integer;
begin
  Result := DateUtils.DaysInAMonth(YearOf(FDate), MonthOf(FDate));
end;

procedure TOrganizerCalendar.OnResize(Sender: TObject);
const
  cHeaderHeight = 20;
var
  I, J: Integer;
  vWidth: Integer;
  vHeight: Integer;
  vLastWidth: Integer;
  vLastHeight: Integer;
  vCurrentTop: Integer;
  vCurrentHeight: Integer;
  vMinHeight: Integer;
begin
  if FRows > 0 then
  begin
    vWidth := Round((FOrganizerScrollBox.FContent.Width - 8) / 7);
    vHeight := Round((FOrganizerScrollBox.FContainer.Height - cHeaderHeight - (FRows + 3)) / (FRows + 1));
    vLastWidth := FOrganizerScrollBox.FContent.Width - (5 * vWidth + 5 + 1 + vWidth) - 2;
    vLastHeight := FOrganizerScrollBox.FContainer.Height - ((FRows - 1) * vHeight + (FRows - 1) + 2 + cHeaderHeight + vHeight) - 2;
    for I := 0 to 6 do
    begin
      FHeader[I].Top := 1;
      FHeader[I].Height := cHeaderHeight;
      FHeader[I].Left := I * vWidth + I + 1;
      if I = 6 then
        FHeader[I].Width := vLastWidth
      else
        FHeader[I].Width := vWidth;
    end;

    for I := 0 to FRows do
    begin
      if I > 0 then
        vCurrentTop := FCells[I - 1, 0].Top + FCells[I - 1, 0].Height + 1
      else
        vCurrentTop := 2 + cHeaderHeight;
      if I = FRows then
        vCurrentHeight := vLastHeight
      else
        vCurrentHeight := vHeight;

      vMinHeight := 0;
      for J := 0 to 6 do
        if FCells[I, J].FMinHeight > vMinHeight then
          vMinHeight := FCells[I, J].FMinHeight;
      if vCurrentHeight < vMinHeight then
        vCurrentHeight := vMinHeight;
      for J := 0 to 6 do
      begin
        FCells[I, J].Top := vCurrentTop;
        FCells[I, J].Left := J * vWidth + J + 1;
        if J = 6 then
          FCells[I, J].Width := vLastWidth
        else
          FCells[I, J].Width := vWidth;
        FCells[I, J].Height := vCurrentHeight;
      end;
    end;

    FOrganizerScrollBox.FContent.Height := FCells[I, 0].Top + FCells[I, 0].Height + 1;

    FOrganizerScrollBox.CalculateScrollBarHeight;
  end;
end;

function TOrganizerCalendar.GetOrganizerCellEventById(ID: Integer
  ): TOrganizerCellEvent;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCalendarDataEventList.Count - 1 do
    if TOrganizerCellEvent(FCalendarDataEventList[I]).ID = ID then
    begin
      Result := TOrganizerCellEvent(FCalendarDataEventList[I]);
      Exit;
    end;
end;

procedure TOrganizerCalendar.SetDate(Value: TDate);
begin
  if Round(FDate) <> Round(Value) then
  begin
    FDate := Value;
    FFirstDayWeekOfMonth := GetFirstDayWeekOfMonth;
    SetFromToDates;
    ClearDataEvents;
    UpdateCalendar;
  end;
end;

procedure TOrganizerCalendar.SetFromToDates;
var
  I: Integer;
  vRows: Integer;
begin
  FFromDate := IncDay(FDate, 1 - DayOfTheMonth(FDate) - FFirstDayWeekOfMonth + 1);
  if MonthOf(IncDay(FDate, 35 - DayOfTheMonth(FDate) - FFirstDayWeekOfMonth + 2)) > MonthOf(FDate) then
  begin
    vRows := 4;
    FToDate := IncDay(FDate, 35 - DayOfTheMonth(FDate) - FFirstDayWeekOfMonth + 1)
  end
  else
  begin
    vRows := 5;
    FToDate := IncDay(FDate, 42 - DayOfTheMonth(FDate) - FFirstDayWeekOfMonth + 1)
  end;

  if vRows <> FRows then
  begin
    FRows := vRows;
    if FRows = 4 then
      for I := 0 to 6 do
        FCells[5, I].Visible := False
    else
      if FRows = 5 then
        for I := 0 to 6 do
          FCells[5, I].Visible := True;
    OnResize(nil);
  end;
end;

procedure TOrganizerCalendar.UpdateCalendar;
var
  I, J: Integer;
  vDayCounter: Integer;
begin
  vDayCounter := 0;

  if Assigned(FOnSelectDataEvent) then
  begin
    FLoadingData := True;
    try
      FOnSelectDataEvent(Self, FFromDate, FToDate);
    finally
      FLoadingData := False;
    end;
  end;

  for I := 0 to FRows do
    for J := 0 to 6 do
    begin
      Inc(vDayCounter);
      FCells[I, J].Date := IncDay(FDate, vDayCounter - DayOfTheMonth(FDate) - FFirstDayWeekOfMonth + 1);
    end;
end;

procedure TOrganizerCalendar.InitControl;
var
  I, J: Integer;
begin
  inherited InitControl;

  FDate := 0;
  FSundayFirst := False;

  FOrganizerScrollBox := TOrganizerScrollBox.Create(Owner);
  FOrganizerScrollBox.FContent.OnResize := @OnResize;
  FOrganizerScrollBox.FContent.Left := 0;
  FOrganizerScrollBox.FContent.Top := 0;
  FOrganizerScrollBox.FContent.Width := FOrganizerScrollBox.FContainer.Width - FOrganizerScrollBox.FScrollBox.Width - 1;
  FOrganizerScrollBox.FContent.Height := FOrganizerScrollBox.FContainer.Height;
  FOrganizerScrollBox.FContent.Anchors := [akBottom, akLeft, akRight, akTop];

  FOrganizerScrollBox.FContent.BevelOuter := TPanelBevel.bvNone;
  FOrganizerScrollBox.FContent.Caption := '';
  FOrganizerScrollBox.FContent.Color := cBevelColor;

  FActiveMonthFontColor := AddToColor(cBevelColor, -70);
  FNonActiveMonthFontColor := cBevelColor;

  for I := 0 to 6 do
  begin
    FHeader[I] := TPanel.Create(FOrganizerScrollBox.FContent);
    FHeader[I].Parent := FOrganizerScrollBox.FContent;
    FHeader[I].Color := clWhite;
    FHeader[I].BevelOuter := TPanelBevel.bvNone;
    with TLabel.Create(FHeader[I]) do
    begin
      Parent := FHeader[I];
      Align := alClient;
      Alignment := taCenter;
      Layout := TTextLayout.tlCenter;
      Font.Color := FActiveMonthFontColor;
      Caption := GetLongDayNames(I + 1);
    end;
  end;

  for I := 0 to 5 do
    for J := 0 to 6 do
    begin
      FCells[I, J] := TOrganizerCalendarCell.Create(Self);
      FCells[I, J].Color := clWhite;
      FCells[I, J].BevelOuter := TPanelBevel.bvNone;
    end;
end;

constructor TOrganizerCalendar.Create(TheOwner: TWinControl);
begin
  FCalendarDataEventList := TList.Create;
  FLoadingData := False;
  FRows := -1;
  inherited Create(TheOwner);
end;

destructor TOrganizerCalendar.Destroy;
begin
  ClearDataEvents;
  inherited Destroy;
  FCalendarDataEventList.Free;
end;

procedure TOrganizerCalendar.ClearDataEvents;
var
  I: Integer;
begin
  for I := FCalendarDataEventList.Count - 1 downto 0 do
    TOrganizerCellEvent(FCalendarDataEventList[i]).Free;
  FCalendarDataEventList.Clear;
end;

procedure TOrganizerCalendar.AddDataEvent(ID: Integer; DateTime: TDateTime);
var
  vSuccess: Boolean;
  vOrganizerCellEvent: TOrganizerCellEvent;
begin
  if (Int(FromDate) <= Int(DateTime))
    and (Int(ToDate) >= Int(DateTime))
    and (GetOrganizerCellEventById(ID) = nil)
  then
  begin
    vOrganizerCellEvent := TOrganizerCellEvent.Create(Self);
    vOrganizerCellEvent.Parent := nil;
    vOrganizerCellEvent.Height := 20;

    vOrganizerCellEvent.FId := ID;
    vOrganizerCellEvent.FDateTime := DateTime;

    vSuccess := True;
    if not FLoadingData and Assigned(FOnInsertDataEvent) then
      FOnInsertDataEvent(Self, vOrganizerCellEvent, vSuccess);
    if vSuccess then
      FCalendarDataEventList.Add(vOrganizerCellEvent)
    else
      vOrganizerCellEvent.Free;
  end;
end;

procedure TOrganizerCalendar.RemoveDataEvent(Value: TOrganizerCellEvent);
begin

end;

{ TOrganizerCancelButton }

procedure TOrganizerCancelButton.OnBackgroundResize(Sender: TObject);
begin
  with FBackground.Picture.Bitmap do
  begin
    SetSize(FBackground.Width, FBackground.Height);
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(FBackground.ClientRect);
    Canvas.Pen.Color := cBevelColor;
    Canvas.Rectangle(FBackground.ClientRect);
  end;
end;

procedure TOrganizerCancelButton.MouseOverAnimationEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  with FBackground.Picture.Bitmap do
  begin
    SetSize(FBackground.Width, FBackground.Height);
    FCurrentAnimationValue := Round(CurrentValue);
    Canvas.Brush.Color := AddToColor(clWhite, FCurrentAnimationValue * - 1);
    Canvas.FillRect(FBackground.ClientRect);
    Canvas.Pen.Color := AddToColor(cBevelColor, FCurrentAnimationValue * - 1); ;
    Canvas.Rectangle(FBackground.ClientRect);

    FLabel.FontEx.Color := AddToColor(cBevelColor, -50 + FCurrentAnimationValue * - 1);
  end;
end;

procedure TOrganizerCancelButton.InitControl;
begin
  inherited InitControl;
  FBackground.OnResize := @OnBackgroundResize;
  FLabel.FontEx.Color := AddToColor(cBevelColor, -50);
end;

constructor TOrganizerCancelButton.Create(TheOwner: TWinControl; Caption: String
  );
begin
  inherited Create(TheOwner, Caption);
  FMouseOverAnimation.AnimatedFloatEvent := @MouseOverAnimationEvent;
end;

{ TOrganizerContent }

procedure TOrganizerContent.SetContent(Value: TWinControl);
begin
  if (FCurrentContent <> Value) and (FNextContent <> Value) then
  begin
    if Assigned(FCurrentContent) then
    begin
      FCurrentContent.Align := alNone;
      FCurrentContent.Left := 0;
      FCurrentContent.Top := 0;
      FCurrentContent.Width := Owner.Width;
      FCurrentContent.Height := Owner.Height;
    end;
    FAnimatedFloat.Stop;
    FNextContent := Value;
    FNextContent.Width := Owner.Width;
    FNextContent.Height := Owner.Height;
    FNextContent.Top := 0;
    FNextContent.Left := FNextContent.Width * -1;
    FNextContent.Visible := True;
    FNextContent.BringToFront;
    FAnimatedFloat.StartValue := FNextContent.Left;
    FAnimatedFloat.StopValue := 0;
    FAnimatedFloat.Start;
  end;
end;

procedure TOrganizerContent.OnAnimatedFloatEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FNextContent.Left := Round(CurrentValue);
  if CurrentProgressProcent = 100 then
  begin
    if Assigned(FCurrentContent) then
      FCurrentContent.Visible := False;
    FCurrentContent := FNextContent;
    FCurrentContent.Align := alClient;
    FNextContent := nil;
  end;
end;

procedure TOrganizerContent.InitControl;
begin
  inherited InitControl;
  if Owner is TPanel then
    with Owner as TPanel do
    begin
      Caption := '';
      BevelOuter := TPanelBevel.bvNone;
    end;
end;

constructor TOrganizerContent.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);
  FCurrentContent := nil;
  FNextContent := nil;

  FAnimatedFloat := TAnimatedFloat.Create;
  FAnimatedFloat.Time := 500;
  FAnimatedFloat.AnimatedFloatType := aftTangensH;
  FAnimatedFloat.AnimatedFloatEvent := @OnAnimatedFloatEvent;
end;

destructor TOrganizerContent.Destroy;
begin
  FAnimatedFloat.Free;
  inherited Destroy;
end;

{ TOrganizerListBox }

procedure TOrganizerListBox.OnScrollBarMoveTimer(Sender: TObject);
var
  vNewPosition: Integer;
begin
  vNewPosition := Mouse.CursorPos.y - FScrollMouseDownClick.y;
  if vNewPosition > FGridPanel.BorderWidth then
  begin
    if vNewPosition > FScrollBarBackround.Height - FScrollBar.Height + FGridPanel.BorderWidth then
      vNewPosition := FScrollBarBackround.Height - FScrollBar.Height + FGridPanel.BorderWidth;
    FScrollBar.Top := vNewPosition;
  end
  else
    FScrollBar.Top := FGridPanel.BorderWidth;
  FDrawGrid.TopRow := Round((FScrollBar.Top - FGridPanel.BorderWidth) / Calc1);
end;

procedure TOrganizerListBox.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCorrenctScrollPositionAnimation.Stop;
  FSetTopRowAnimation.Stop;
  FScrollMouseDownClick := Point(X, Mouse.CursorPos.Y - FScrollBar.Top);
  FScrollBarMoveTimer.Enabled := True;
end;

procedure TOrganizerListBox.OnScrollBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vTopBar: Integer;
begin
  if (FScrollBarBackround.Height <> 0) and (FDrawGrid.RowCount <> 0) and (FDrawGrid.Height <> 0) and (FDrawGrid.DefaultRowHeight <> 0) then
    vTopBar := Round(Y / FDrawGrid.Height * (FDrawGrid.RowCount - (FDrawGrid.Height / FDrawGrid.DefaultRowHeight)))
  else
    vTopBar := 0;
  FSetTopRowAnimation.Stop;
  FSetTopRowAnimation.StartValue := FDrawGrid.TopRow;
  FSetTopRowAnimation.StopValue := vTopBar;
  FSetTopRowAnimation.Start;
end;

procedure TOrganizerListBox.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FScrollBarMoveTimer.Enabled := False;

  FCorrenctScrollPositionAnimation.Stop;
  FCorrenctScrollPositionAnimation.StartValue := FScrollBar.Top;
  FCorrenctScrollPositionAnimation.StopValue := Round(FDrawGrid.TopRow * Calc1) + FGridPanel.BorderWidth;
  FCorrenctScrollPositionAnimation.Start;
end;

procedure TOrganizerListBox.OnScrollBarResize(Sender: TObject);
  procedure DrawLine(YPos: Integer);
  begin
    with TImage(Sender).Picture.Bitmap do
    begin
      Canvas.Pen.Color := $00F0F0F0;
      Canvas.Line(2, YPos, TImage(Sender).Width - 2, YPos);
      Canvas.Pen.Color := clBlack;
      Canvas.Line(3, YPos + 1, TImage(Sender).Width - 1, YPos + 1);
    end;
  end;

var
  vLineMiddleY: Integer;
  vHeight: Integer;
begin
  if (FScrollBarBackround.Height <> 0) and (FDrawGrid.RowCount <> 0) and (FDrawGrid.Height <> 0) and (FDrawGrid.DefaultRowHeight <> 0) then
    vHeight := Round(FScrollBarBackround.Height / (FDrawGrid.RowCount / (FDrawGrid.Height / FDrawGrid.DefaultRowHeight)))
  else
    vHeight := 0;
  if vHeight < 20 then
    vHeight := 20;
  TImage(Sender).Height := vHeight;
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := cPrimaryColor;
    Canvas.FillRect(Canvas.ClipRect);

    vLineMiddleY := Round(TImage(Sender).Height / 2);
    DrawLine(vLineMiddleY);
    DrawLine(vLineMiddleY - 3);
    DrawLine(vLineMiddleY + 3);
  end;
end;

procedure TOrganizerListBox.OnScrollBarBackroundResize(Sender: TObject);
begin
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := FContainer.Color;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Pen.Color := cBevelColor;
    Canvas.Rectangle(Canvas.ClipRect);
  end;
end;

procedure TOrganizerListBox.OnTopLeftChanged(Sender: TObject);
begin
  if not FScrollBarMoveTimer.Enabled then
  begin
    FCorrenctScrollPositionAnimation.Stop;
    FCorrenctScrollPositionAnimation.StartValue := FScrollBar.Top;
    FCorrenctScrollPositionAnimation.StopValue := Round(FDrawGrid.TopRow * Calc1) + FGridPanel.BorderWidth;
    FCorrenctScrollPositionAnimation.Start;
  end;
end;

procedure TOrganizerListBox.OnDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  vBackground: TColor;
  vTextStyle: TTextStyle;
begin
  with TDrawGrid(Sender) do
  begin
    if (aRow mod 2 = 0) or (aRow = TopRow) then
      vBackground := clWhite
    else
      vBackground := RGBToColor(249, 249, 249);
    Canvas.Brush.Color := vBackground;
    Canvas.FillRect(aRect);
    if aRow <> TopRow then
    begin
      Canvas.Pen.Color := RGBToColor(221, 221, 221);
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      if aRow = TopRow + 1 then
        Canvas.Line(aRect.Left, aRect.Top + 1, aRect.Right, aRect.Top + 1);
    end;
    if aRow = TopRow then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];
    Canvas.Font.Size := 13;
    Canvas.Font.Color := RGBToColor(118, 118, 118);
    vTextStyle.Alignment := TAlignment.taCenter;
    vTextStyle.Layout := TTextLayout.tlCenter;
    if aRow = TopRow then
      Canvas.TextRect(aRect, aRect.Left, aRect.Top, '0', vTextStyle)
    else
      Canvas.TextRect(aRect, aRect.Left, aRect.Top, IntToStr(aRow), vTextStyle);
  end;
end;

procedure TOrganizerListBox.OnCorrenctScrollPositionAnimationEvent(
  Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FScrollBar.Top := Round(CurrentValue)
end;

procedure TOrganizerListBox.OnResizeScrollAnimationEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FScrollBarBackround.Width := Round(CurrentValue) * 2;
  FScrollBar.Width := FScrollBarBackround.Width;
  FScrollBarBackround.Left := FGridPanel.Width - FGridPanel.BorderWidth + 6 - Round(CurrentValue);
  FScrollBar.Left := FScrollBarBackround.Left;
end;

procedure TOrganizerListBox.OnSetTopRowAnimationEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
begin
  FDrawGrid.TopRow := Round(CurrentValue);
end;

procedure TOrganizerListBox.ResizeUpScrollBar(Sender: TObject);
begin
  FResizeScrollAnimation.Stop;
  FResizeScrollAnimation.StartValue := FScrollBarBackround.Width / 2;
  FResizeScrollAnimation.StopValue := 5;
  FResizeScrollAnimation.Start;
end;

procedure TOrganizerListBox.ResizeDownScrollBar(Sender: TObject);
begin
  FResizeScrollAnimation.Stop;
  FResizeScrollAnimation.StartValue := FScrollBarBackround.Width / 2;
  FResizeScrollAnimation.StopValue := 2;
  FResizeScrollAnimation.Start;
end;

function TOrganizerListBox.Calc1: Double;
begin
  if (FScrollBarBackround.Height <> 0) and (FDrawGrid.RowCount <> 0) and (FDrawGrid.Height <> 0) and (FDrawGrid.DefaultRowHeight <> 0) then
    Result := ((FScrollBarBackround.Height - FScrollBar.Height) / (FDrawGrid.RowCount - Round(FDrawGrid.Height / FDrawGrid.DefaultRowHeight)))
  else
    Result := 0;
end;

procedure TOrganizerListBox.InitControl;
begin
  inherited InitControl;
  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.Caption := '';

  FBorderImage := TImage.Create(FContainer);
  FBorderImage.Parent := FContainer;
  FBorderImage.Align := alTop;
  FBorderImage.Height := 2;

  FHeaderPanel := TPanel.Create(FContainer);
  FHeaderPanel.Parent := FContainer;
  FHeaderPanel.Align := alTop;
  FHeaderPanel.BevelOuter := TPanelBevel.bvNone;
  FHeaderPanel.BorderWidth := 10;
  FHeaderPanel.Color := $00FAFAFA;
  FHeaderPanel.Height := 42;

  FHeaderLabel := TLabel.Create(FHeaderPanel);
  FHeaderLabel.Parent := FHeaderPanel;
  FHeaderLabel.Align := alClient;
  FHeaderLabel.Layout := TTextLayout.tlCenter;
  FHeaderLabel.Font.Color := $00767676;
  FHeaderLabel.Font.Size := 14;
  FHeaderLabel.Caption := 'Table';

  FGridPanel := TPanel.Create(FContainer);
  FGridPanel.Parent := FContainer;
  FGridPanel.BevelOuter := TPanelBevel.bvNone;
  FGridPanel.Caption := '';
  FGridPanel.Align := alClient;
  FGridPanel.BorderWidth := 20;
  FGridPanel.Color := clWhite;
  FGridPanel.OnMouseEnter := @ResizeUpScrollBar;
  FGridPanel.OnMouseLeave := @ResizeDownScrollBar;

  FDrawGrid := TDrawGrid.Create(FGridPanel);
  FDrawGrid.Parent := FGridPanel;
  FDrawGrid.Align := alClient;
  FDrawGrid.BorderStyle := bsNone;
  FDrawGrid.FocusRectVisible := False;
  FDrawGrid.GridLineWidth := 0;
  FDrawGrid.FixedRows := 0;
  FDrawGrid.DefaultColWidth := 150;
  FDrawGrid.DefaultRowHeight := 35;
  FDrawGrid.MouseWheelOption := mwGrid;
  FDrawGrid.Options := FDrawGrid.Options + [goSmoothScroll, goThumbTracking];
  FDrawGrid.ScrollBars := TScrollStyle.ssNone;
  FDrawGrid.OnDrawCell := @OnDrawCell;
  FDrawGrid.OnTopLeftChanged := @OnTopLeftChanged;
  FDrawGrid.OnMouseEnter := @ResizeUpScrollBar;
  FDrawGrid.OnMouseLeave := @ResizeDownScrollBar;

  FScrollBarBackround := TImage.Create(FGridPanel);
  FScrollBarBackround.Parent := FGridPanel;
  FScrollBarBackround.Width := 4;
  FScrollBarBackround.Height := FGridPanel.Height - FGridPanel.BorderWidth * 2;
  FScrollBarBackround.Top := FGridPanel.BorderWidth;
  FScrollBarBackround.Left := FGridPanel.Width - FGridPanel.BorderWidth + 6;
  FScrollBarBackround.Anchors := [akRight, akTop, akBottom];
  FScrollBarBackround.OnResize := @OnScrollBarBackroundResize;
  FScrollBarBackround.OnMouseDown := @OnScrollBackgroundMouseDown;
  FScrollBarBackround.OnMouseEnter := @ResizeUpScrollBar;
  FScrollBarBackround.OnMouseLeave := @ResizeDownScrollBar;

  FScrollBar := TImage.Create(FGridPanel);
  FScrollBar.Parent := FGridPanel;
  FScrollBar.Width := FScrollBarBackround.Width;
  FScrollBar.Top := FGridPanel.BorderWidth;
  FScrollBar.Left := FScrollBarBackround.Left;
  FScrollBar.Anchors := [akTop, akRight];
  FScrollBar.OnMouseDown := @OnMouseDown;
  FScrollBar.OnMouseUp := @OnMouseUp;
  FScrollBar.OnResize := @OnScrollBarResize;
  FScrollBar.OnMouseEnter := @ResizeUpScrollBar;
  FScrollBar.OnMouseLeave := @ResizeDownScrollBar;

  FScrollBarMoveTimer := TTimer.Create(FContainer);
  FScrollBarMoveTimer.Interval := 1;
  FScrollBarMoveTimer.Enabled := False;
  FScrollBarMoveTimer.OnTimer := @OnScrollBarMoveTimer;

  FDrawGrid.ColCount := 3;
  FDrawGrid.RowCount := 100;
end;

constructor TOrganizerListBox.Create(TheOwner: TWinControl);
begin
  FCorrenctScrollPositionAnimation := TAnimatedFloat.Create;
  FCorrenctScrollPositionAnimation.Time := 500;
  FCorrenctScrollPositionAnimation.AnimatedFloatType := aftTangensH;
  FCorrenctScrollPositionAnimation.AnimatedFloatEvent := @OnCorrenctScrollPositionAnimationEvent;
  FResizeScrollAnimation := TAnimatedFloat.Create;
  FResizeScrollAnimation.Time := 500;
  FResizeScrollAnimation.AnimatedFloatType := aftTangensH;
  FResizeScrollAnimation.AnimatedFloatEvent := @OnResizeScrollAnimationEvent;
  FSetTopRowAnimation := TAnimatedFloat.Create;
  FSetTopRowAnimation.Time := 500;
  FSetTopRowAnimation.AnimatedFloatType := aftTangensH;
  FSetTopRowAnimation.AnimatedFloatEvent := @OnSetTopRowAnimationEvent;
  inherited Create(TheOwner);
end;

destructor TOrganizerListBox.Destroy;
begin
  FSetTopRowAnimation.Free;
  FResizeScrollAnimation.Free;
  FCorrenctScrollPositionAnimation.Free;
  inherited Destroy;
end;

{ TOrganizerComboBox }

procedure TOrganizerComboBox.OnClick(Sender: TObject);
var
  vOrganizerPopup: TOrganizerPopup;
  vPoint: TPoint;
begin
  vOrganizerPopup := TOrganizerPopup.Create(Self);
  vOrganizerPopup.SetBounds(0, 0, FContainer.Width, 0);
  TOrganizerComboBoxList.Create(vOrganizerPopup, ['Poniedziałek','Wtorek','Środa', 'Czwartek', 'Piątek', 'Sobota', 'Niedziela']);
  vOrganizerPopup.AutoSize := True;
  vPoint := FContainer.ClientToScreen(Point(0,0));
  vOrganizerPopup.Left := vPoint.X;
  vOrganizerPopup.Top := vPoint.Y + FContainer.Height;
  vOrganizerPopup.Width := FContainer.Width;
  vOrganizerPopup.ShowAnimation;
end;

procedure TOrganizerComboBox.InitControl;
begin
  inherited InitControl;
  FContainer.Color := AddToColor(cPrimaryColor, -30);
  FContainer.OnClick := @OnClick;

  FBackground.Width := FContainer.Width - 24;

  FExpanderImage := TImage.Create(FContainer);
  FExpanderImage.Parent := FContainer;
  FExpanderImage.Width := 24;
  FExpanderImage.Align := alRight;
  FExpanderImage.Center := True;
  FExpanderImage.Enabled := False;
  FExpanderImage.Picture.LoadFromFile('Icons/fa-angle-down_15_0_c8c8c8_none.png');
end;

{ TOrganizerButton }

procedure TOrganizerButton.MouseEnter(Sender: TObject);
begin
  FMouseOverAnimation.Stop;
  FMouseOverAnimation.StartValue := 0;
  FMouseOverAnimation.StopValue := 30;
  FMouseOverAnimation.Time := 500;
  FMouseOverAnimation.Start;
end;

procedure TOrganizerButton.MouseLeave(Sender: TObject);
begin
  if FCurrentAnimationValue <> 0 then
  begin
    FMouseOverAnimation.Stop;
    FMouseOverAnimation.StartValue := 30;
    FMouseOverAnimation.StopValue := 0;
    FMouseOverAnimation.Time := 500;
    FMouseOverAnimation.Start;
  end;
end;

procedure TOrganizerButton.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseOverAnimation.Stop;
  FMouseOverAnimation.StartValue := 0;
  FMouseOverAnimation.StopValue := -30;
  FMouseOverAnimation.Time := 0;
  FMouseOverAnimation.Start;
end;

procedure TOrganizerButton.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseOverAnimation.Stop;
  FMouseOverAnimation.StartValue := -30;
  if PtInRect(Point(X, Y), FContainer.ClientRect) then
    FMouseOverAnimation.StopValue := 30
  else
    FMouseOverAnimation.StopValue := 0;
  FMouseOverAnimation.Time := 0;
  FMouseOverAnimation.Start;
end;

procedure TOrganizerButton.MouseOverAnimationEvent(Sender: TObject;
  CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
var
  vR, vG, vB: Byte;
begin
  vR := Red(cPrimaryColor);
  vG := Green(cPrimaryColor);
  vB := Blue(cPrimaryColor);
  FCurrentAnimationValue := Round(CurrentValue);
  with FBackground.Picture.Bitmap do
  begin
    Canvas.Brush.Color := AddToColor(cPrimaryColor, FCurrentAnimationValue);
    Canvas.FillRect(FBackground.ClientRect);
  end;
end;

procedure TOrganizerButton.InitControl;
begin
  inherited InitControl;
  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.OnClick := Owner.OnClick;
  FContainer.OnMouseEnter := @MouseEnter;
  FContainer.OnMouseLeave := @MouseLeave;
  FContainer.OnMouseUp := @MouseUp;
  FContainer.OnMouseDown := @MouseDown;

  FBackground := TImage.Create(FContainer);
  FBackground.Parent := FContainer;
  FBackground.Left := 0;
  FBackground.Top := 0;
  FBackground.Width := FContainer.Width;
  FBackground.Height := FContainer.Height;
  FBackground.Anchors := [akTop, akRight, akLeft, akBottom];
  FBackground.Enabled := False;
  with FBackground.Picture.Bitmap do
  begin
    SetSize(FBackground.Width, FBackground.Height);
    Canvas.Brush.Color := cPrimaryColor;
    Canvas.FillRect(FBackground.ClientRect);
  end;

  FLabel := TBCLabel.Create(FContainer);
  FLabel.Parent := FContainer;
  FLabel.Align := alClient;
  FLabel.FontEx.TextAlignment := bcaCenter;
  FLabel.Enabled := False;
  FLabel.FontEx.Color := $00F0F0F0;
  FLabel.FontEx.Height := 15;
end;

constructor TOrganizerButton.Create(TheOwner: TWinControl; Caption: String);
begin
  FMouseOverAnimation := TAnimatedFloat.Create;
  FMouseOverAnimation.AnimatedFloatType := aftTangensH;
  FMouseOverAnimation.AnimatedFloatEvent := @MouseOverAnimationEvent;
  FCurrentAnimationValue := 0;
  inherited Create(TheOwner);
  FLabel.Caption := Caption;
end;

destructor TOrganizerButton.Destroy;
begin
  FMouseOverAnimation.Free;
  inherited Destroy;
end;

{ TOrganizerCheckBox }

constructor TOrganizerCheckBox.Create(TheOwner: TWinControl; Caption,
  CheckedPicture: String);
begin
  inherited Create(TheOwner);
  FChecked := False;
  FCheckedPicture := CheckedPicture;
  FLabel.Caption := Caption;
  FLabel.Enabled := FLabel.Caption <> '';
  FTextLeftMargin.Enabled := FLabel.Enabled;
end;

procedure TOrganizerCheckBox.InitControl;
begin
  inherited InitControl;
  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;

  FLabel := TBCLabel.Create(FContainer);
  FLabel.Parent := FContainer;
  FLabel.Align := alLeft;
  FLabel.AutoSize := True;
  FLabel.FontEx.TextAlignment := TBCAlignment.bcaLeftCenter;
  FLabel.FontEx.Color := cMenuBackgroundColor;
  FLabel.FontEx.Height := 15;
  FLabel.OnClick := @OnClick;

  FTextLeftMargin := TImage.Create(FContainer);
  FTextLeftMargin.Parent := FContainer;
  FTextLeftMargin.Align := alLeft;
  FTextLeftMargin.Width := 5;
  FTextLeftMargin.OnClick := @OnClick;

  FImage := TBGRAGraphicControl.Create(FContainer);
  FImage.Parent := FContainer;
  FImage.Align := alLeft;
  FImage.BevelOuter := TPanelBevel.bvNone;
  FImage.Width := 16;
  FImage.OnClick := @OnClick;
  FImage.OnRedraw := @OnRedrawEvent;
end;

procedure TOrganizerCheckBox.SetChecked(Value: Boolean);
begin
  FChecked := not FChecked;
  FImage.RedrawBitmap;
end;

procedure TOrganizerCheckBox.OnClick(Sender: TObject);
begin
  Checked := not Checked;
end;

procedure TOrganizerCheckBox.OnRedrawEvent(Sender: TObject; Bitmap: TBGRABitmap);
var
  vTemp: TBGRABitmap;
begin
  if FChecked then
  begin
    Bitmap.RoundRect(0, 0, 16, 16, 5, 5, ColorToBGRA(cPrimaryColor), ColorToBGRA(cPrimaryColor));
    vTemp := TBGRABitmap.Create(FCheckedPicture);
    try
      Bitmap.BlendImage(1, 1, vTemp, boLinearBlend);
    finally
      vTemp.Free;
    end;
  end
  else
    Bitmap.RoundRect(0, 0, 16, 16, 5, 5, ColorToBGRA(cMenuFontColor), ColorToBGRA(cMenuFontColor));
end;

{ TOrganizerCustomControl }

procedure TOrganizerCustomControl.InitControl;
begin

end;

constructor TOrganizerCustomControl.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);
  FOwner := TheOwner;
  if FOwner is TPanel then
    TPanel(FOwner).BevelOuter := TPanelBevel.bvNone;
  InitControl;
end;

{ TOrganizerMenuButton }

procedure TOrganizerMenuButton.MouseEnter(Sender: TObject);
begin
  //FBackground.Color := cMenuBackgroundMouseOverColor;
  FMouseOverAnimation.Stop;
  FMouseOverAnimation.StartValue := $55;
  FMouseOverAnimation.StopValue := $33;
  FMouseOverAnimation.Start;
end;

procedure TOrganizerMenuButton.MouseLeave(Sender: TObject);
begin
  FMouseOverAnimation.Stop;
  FMouseOverAnimation.StartValue := $33;
  FMouseOverAnimation.StopValue := $55;
  FMouseOverAnimation.Start;
end;

procedure TOrganizerMenuButton.MouseOverAnimationEvent(Sender: TObject; CurrentProgressProcent, CurrentProcent, CurrentValue: Double);
var
  vWidth: Integer;
  vCurrentColor: TColor;
begin
  vCurrentColor := RGBToColor(Round(CurrentValue), Round(CurrentValue), Round(CurrentValue));
  with FBackground.Picture.Bitmap do
  begin
    SetSize(FContainer.Width, FContainer.Height);
    Canvas.Brush.Color := vCurrentColor;
    Canvas.FillRect(0, 0, FBackground.Width, FBackground.Height);
  end;

  with FBorder.Picture.Bitmap do
  begin
    vWidth := Round(CurrentProcent / 5);
    if FMouseOverAnimation.StartValue < FMouseOverAnimation.StopValue then
      vWidth := 20 - vWidth;
    if vWidth > 20 then
      vWidth := 20;
    if vWidth < 0 then
      vWidth := 0;
    vWidth := vWidth + 4;
    SetSize(24, FBorder.Height);
    Canvas.Brush.Color := FLeftColor;
    Canvas.FillRect(0, 0, vWidth, FBorder.Height);
    Canvas.Brush.Color := vCurrentColor;
    Canvas.FillRect(vWidth, 0, FBorder.Width, FBorder.Height);
  end;
end;

procedure TOrganizerMenuButton.InitControl;
begin
  inherited InitControl;
  FContainer := TPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BevelOuter := TPanelBevel.bvNone;
  FContainer.OnMouseEnter := @MouseEnter;
  FContainer.OnMouseLeave := @MouseLeave;
  FContainer.OnClick := Owner.OnClick;

  FBackground := TImage.Create(FContainer);
  FBackground.Parent := FContainer;
  FBackground.Left := 0;
  FBackground.Top := 0;
  FBackground.Width := FContainer.Width;
  FBackground.Height := FContainer.Height;
  FBackground.Anchors := [akBottom, akLeft, akRight, akTop];
  FBackground.Enabled := False;

  FImage := TImage.Create(FContainer);
  FImage.Parent := FContainer;
  FImage.Align := alLeft;
  FImage.Center := True;
  FImage.Width := 30;
  FImage.Enabled := False;

  FBorder := TImage.Create(FContainer);
  FBorder.Parent := FContainer;
  FBorder.Align := alLeft;
  FBorder.Width := 24;
  FBorder.Height := FContainer.Height;
  FBorder.Anchors := [akBottom, akLeft, akTop];
  FBorder.Enabled := False;

  FCaptionLabel := TBCLabel.Create(FContainer);
  FCaptionLabel.Parent := FContainer;
  FCaptionLabel.Align := alClient;
  FCaptionLabel.FontEx.TextAlignment := bcaLeftCenter;
  FCaptionLabel.FontEx.Name := cMenuFontName;
  FCaptionLabel.FontEx.Height := 17;
  FCaptionLabel.FontEx.Color := cMenuFontColor;
  FCaptionLabel.FontEx.FontQuality := fqFineClearTypeBGR;
  FCaptionLabel.Enabled := False;
end;

constructor TOrganizerMenuButton.Create(TheOwner: TWinControl; Caption: String;
  Picture: String; LeftColor: TColor);
begin
  FMouseOverAnimation := TAnimatedFloat.Create;
  FMouseOverAnimation.Time := 500;
  FMouseOverAnimation.AnimatedFloatType := aftTangensH;
  FMouseOverAnimation.AnimatedFloatEvent := @MouseOverAnimationEvent;
  inherited Create(TheOwner);
  FImage.Picture.LoadFromFile(Picture);
  FCaptionLabel.Caption := Caption;

  FLeftColor := LeftColor;
  with FBorder.Picture.Bitmap do
  begin
    SetSize(24, FBorder.Height);
    Canvas.Brush.Color := FLeftColor;
    Canvas.FillRect(0, 0, 4, FBorder.Height);
    Canvas.Brush.Color := cMenuBackgroundColor;
    Canvas.FillRect(4, 0, FBorder.Width, FBorder.Height);
  end;
end;

destructor TOrganizerMenuButton.Destroy;
begin
  FMouseOverAnimation.Free;
  inherited Destroy;
end;

end.

