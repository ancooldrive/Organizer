unit uOrganizerControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, Graphics, Grids,
  BCLabel, BCTypes, BCPanel, BGRABitmapTypes, BGRABitmap, BGRAGraphicControl,

  uAnimatedFloat;

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
    FContainer: TBCPanel;
    FEdit: TEdit;
    FDefaultText: String;
    procedure OnEditChange(Sender: TObject);
    procedure OnClickOrEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnAfterRenderConteiner(Sender: TObject; const ABGRA: TBGRABitmap; ARect: TRect);
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

  { TOrganizerComboBox }

  TOrganizerComboBox = class(TOrganizerButton)
  private
    FExpanderImage: TImage;
  protected
    procedure InitControl; override;
  public
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
    procedure OnScrollBarMoveTimer(Sender: TObject);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure OnScrollBarResize(Sender: TObject);
    procedure OnScrollBarBackroundResize(Sender: TObject);
    procedure OnDrawCell(Sender: TObject; aCol, aRow: Integer;
              aRect: TRect; aState: TGridDrawState);
  protected
    procedure InitControl; override;
  public
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
end;

procedure TOrganizerListBox.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FScrollMouseDownClick := Point(X, Mouse.CursorPos.Y - FScrollBar.Top);
  FScrollBarMoveTimer.Enabled := True;
end;

procedure TOrganizerListBox.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FScrollBarMoveTimer.Enabled := False;
end;

procedure TOrganizerListBox.OnScrollBarResize(Sender: TObject);
begin
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := cPrimaryColor;
    Canvas.FillRect(Canvas.ClipRect);
  end;
end;

procedure TOrganizerListBox.OnScrollBarBackroundResize(Sender: TObject);
begin
  with TImage(Sender).Picture.Bitmap do
  begin
    SetSize(TImage(Sender).Width, TImage(Sender).Height);
    Canvas.Brush.Color := cMenuFontColor;
    Canvas.FillRect(Canvas.ClipRect);
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
    if aRow mod 2 = 0 then
      vBackground := clWhite
    else
      vBackground := RGBToColor(249, 249, 249);
    Canvas.Brush.Color := vBackground;
    Canvas.FillRect(aRect);
    if aRow <> 0 then
    begin
      Canvas.Pen.Color := RGBToColor(221, 221, 221);
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      if aRow = 1 then
        Canvas.Line(aRect.Left, aRect.Top + 1, aRect.Right, aRect.Top + 1);
    end;
    if aRow = 0 then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];
    Canvas.Font.Size := 13;
    Canvas.Font.Color := RGBToColor(118, 118, 118);
    vTextStyle.Alignment := TAlignment.taCenter;
    vTextStyle.Layout := TTextLayout.tlCenter;
    Canvas.TextRect(aRect, aRect.Left, aRect.Top, 'test', vTextStyle);
  end;
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

  FDrawGrid := TDrawGrid.Create(FGridPanel);
  FDrawGrid.Parent := FGridPanel;
  FDrawGrid.Align := alClient;
  FDrawGrid.OnDrawCell := @OnDrawCell;
  FDrawGrid.BorderStyle := bsNone;
  FDrawGrid.FocusRectVisible := False;
  FDrawGrid.GridLineWidth := 0;

  FScrollBarBackround := TImage.Create(FGridPanel);
  FScrollBarBackround.Parent := FGridPanel;
  FScrollBarBackround.Width := 4;
  FScrollBarBackround.Height := FGridPanel.Height - FGridPanel.BorderWidth * 2;
  FScrollBarBackround.Top := FGridPanel.BorderWidth;
  FScrollBarBackround.Left := FGridPanel.Width - FGridPanel.BorderWidth + 1;
  FScrollBarBackround.Anchors := [akRight, akTop, akBottom];
  FScrollBarBackround.OnResize := @OnScrollBarBackroundResize;

  FScrollBar := TImage.Create(FGridPanel);
  FScrollBar.Parent := FGridPanel;
  FScrollBar.Width := 8;
  FScrollBar.Height := 100;
  FScrollBar.Top := FGridPanel.BorderWidth;
  FScrollBar.Left := FGridPanel.Width - FGridPanel.BorderWidth;
  FScrollBar.Anchors := [akTop, akRight];
  FScrollBar.OnMouseDown := @OnMouseDown;
  FScrollBar.OnMouseUp := @OnMouseUp;
  FScrollBar.OnResize := @OnScrollBarResize;

  FScrollBarMoveTimer := TTimer.Create(FContainer);
  FScrollBarMoveTimer.Interval := 1;
  FScrollBarMoveTimer.Enabled := False;
  FScrollBarMoveTimer.OnTimer := @OnScrollBarMoveTimer;

  FDrawGrid.ColCount := 3;
  FDrawGrid.FixedRows := 0;
  FDrawGrid.DefaultColWidth := 150;
  FDrawGrid.DefaultRowHeight := 35;

end;

{ TOrganizerComboBox }

procedure TOrganizerComboBox.InitControl;
begin
  inherited InitControl;
  FContainer.Color := AddToColor(cPrimaryColor, -30);

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

{ TOrganizerEdit }

procedure TOrganizerEdit.OnEditChange(Sender: TObject);
begin
  if FEdit.Text <> '' then
    FEdit.Align := alClient
  else
  begin
    FEdit.Align := alLeft;
    FEdit.Width := 3;
  end;
end;

procedure TOrganizerEdit.OnClickOrEnter(Sender: TObject);
begin
  FEdit.Visible := True;
  FEdit.SetFocus;
end;

procedure TOrganizerEdit.OnExit(Sender: TObject);
begin
  if (FEdit.Text = '') and not FEdit.Focused then
    FEdit.Visible := False;
end;

procedure TOrganizerEdit.OnAfterRenderConteiner(Sender: TObject;
  const ABGRA: TBGRABitmap; ARect: TRect);
var
  vRect: TRect;
begin
  ABGRA.FontHeight := 17;
  ABGRA.FontName := cMenuFontName;
  ABGRA.FillRect(ARect, clWhite);
  ABGRA.Rectangle(0, 0, ABGRA.Width, ABGRA.Height, ColorToBGRA(cBevelColor, 255), BGRA(0, 0, 0, 0), TDrawMode.dmDrawWithTransparency);
  vRect := ARect;
  vRect.Left := 4;
  ABGRA.TextRect(vRect, FDefaultText, TAlignment.taLeftJustify, TTextLayout.tlCenter, ColorToBGRA(cBevelColor, 255));
end;

procedure TOrganizerEdit.InitControl;
begin
  inherited InitControl;
  FContainer := TBCPanel.Create(Owner);
  FContainer.Parent := Owner;
  FContainer.Align := alClient;
  FContainer.BorderWidth := 1;
  FContainer.Cursor := crIBeam;

  FEdit := TEdit.Create(FContainer);
  FEdit.Parent := FContainer;
  FEdit.Align := alLeft;
  FEdit.BorderStyle := bsNone;
  FEdit.Font.Color := cMenuBackgroundColor;
  FEdit.Font.Size := 14;
  FEdit.Width := 3;
  FEdit.Visible := True;
  FEdit.OnChange := @OnEditChange;
  FEdit.OnExit := @OnExit;

  FContainer.TabStop := True;
  FContainer.OnClick := @OnClickOrEnter;
  FContainer.OnEnter := @OnClickOrEnter;
  FContainer.OnAfterRenderBCPanel := @OnAfterRenderConteiner;
end;

constructor TOrganizerEdit.Create(TheOwner: TWinControl; DefaultText: String);
begin
  FDefaultText := DefaultText;
  inherited Create(TheOwner);
end;


{ TOrganizerCustomControl }

procedure TOrganizerCustomControl.InitControl;
begin

end;

constructor TOrganizerCustomControl.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);
  FOwner := TheOwner;
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

