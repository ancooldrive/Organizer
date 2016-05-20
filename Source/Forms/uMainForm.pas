unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Menus, DateUtils,

  BGRAGraphicControl, BGRABitmap, BGRABitmapTypes,

  uOrganizerForm,
  uAddEditCalendarEventForm,
  uOrganizerControls;

type

  { TMainForm }

  TMainForm = class(TOrganizerForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    Image1: TImage;
    Image2: TImage;
    Image5: TImage;
    pAlarms: TPanel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    pCalendarControl: TPanel;
    pCalendarControl1: TPanel;
    pCalendarTest: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    pCalendarContent: TPanel;
    pCalendarTest1: TPanel;
    pSettingsContent: TPanel;
    pToDoContent: TPanel;
    pOrganizerContent: TPanel;
    pDashboardContent: TPanel;
    Panel18: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pBevel1: TPanel;
    pCalendar: TPanel;
    pContacts: TPanel;
    pContent: TPanel;
    pDashboard: TPanel;
    pHeader: TPanel;
    pLogo: TPanel;
    pMainContent: TPanel;
    pMeetings: TPanel;
    pMenu: TPanel;
    pSettings: TPanel;
    pToDo: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure pAlarmsClick(Sender: TObject);
    procedure pCalendarClick(Sender: TObject);
    procedure pContactsClick(Sender: TObject);
    procedure pDashboardClick(Sender: TObject);
    procedure pMeetingsClick(Sender: TObject);
    procedure pSettingsClick(Sender: TObject);
    procedure pToDoClick(Sender: TObject);
  private
    FFirstRun: Boolean;
    FOrganizerContent: TOrganizerContent;
    FOrganizerCalendar: TOrganizerCalendar;
    FOrganizerCalendar1: TOrganizerCalendar;

    FStringListTest: TStringList;

    procedure OnOrganizerSelectDataEvent(Sender: TObject; FromDate, ToDate: TDateTime);
    procedure OnOrganizerInserDataEvent(Sender: TObject; OrganizerCellEvent: TOrganizerCellEvent; var Success: Boolean);
    procedure OnOrganizerUpdateDataEvent(Sender: TObject; OrganizerCellEvent: TOrganizerCellEvent; var Success: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FOrganizerCalendar.AddDataEvent(FStringListTest.Count, Now);
  FOrganizerCalendar.UpdateCalendar;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  FOrganizerCalendar.Date := IncMonth(FOrganizerCalendar.Date, -1)
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  FOrganizerCalendar.Date := IncMonth(FOrganizerCalendar.Date, 1);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if FFirstRun then
  begin
    FOrganizerContent.Content := pDashboardContent;
    FFirstRun := False;
  end;
end;

procedure TMainForm.pAlarmsClick(Sender: TObject);
begin
  FOrganizerContent.Content := pCalendarTest1;
end;

procedure TMainForm.pCalendarClick(Sender: TObject);
begin
  FOrganizerContent.Content := pCalendarContent;
end;

procedure TMainForm.pContactsClick(Sender: TObject);
begin
  FOrganizerContent.Content := pCalendarTest;
end;

procedure TMainForm.pDashboardClick(Sender: TObject);
begin
  FOrganizerContent.Content := pDashboardContent;
end;

procedure TMainForm.pMeetingsClick(Sender: TObject);
begin
  ShowAddEditCalendarEventForm(Self);
end;

procedure TMainForm.pSettingsClick(Sender: TObject);
begin
  FOrganizerContent.Content := pSettingsContent;
end;

procedure TMainForm.pToDoClick(Sender: TObject);
begin
  FOrganizerContent.Content := pToDoContent;
end;

procedure TMainForm.OnOrganizerSelectDataEvent(Sender: TObject; FromDate,
  ToDate: TDateTime);
var
  I: Integer;
  vStringList: TStringList;
  vID: Integer;
  vDateTime: TDateTime;
begin
  vStringList := TStringList.Create;
  try
    for I := 0 to FStringListTest.Count - 1 do
      if Trim(FStringListTest[I]) <> '' then
      begin
        vStringList.CommaText := FStringListTest[I];
        if (vStringList.Count = 2) and TryStrToInt(vStringList[0], vID) and TryStrToDateTime(vStringList[1], vDateTime) then
          FOrganizerCalendar.AddDataEvent(vID, vDateTime);
      end;
  finally
    vStringList.Free;
  end;
end;

procedure TMainForm.OnOrganizerInserDataEvent(Sender: TObject;
  OrganizerCellEvent: TOrganizerCellEvent; var Success: Boolean);
var
  vStringList: TStringList;
begin
  vStringList := TStringList.Create;
  try
    vStringList.Add(IntToStr(OrganizerCellEvent.ID));
    vStringList.Add(DateTimeToStr(OrganizerCellEvent.DateTime));
    FStringListTest.Add(vStringList.CommaText);
  finally
    vStringList.Free;
  end;
  Success := True;
end;

procedure TMainForm.OnOrganizerUpdateDataEvent(Sender: TObject;
  OrganizerCellEvent: TOrganizerCellEvent; var Success: Boolean);
var
  I: Integer;
  vStringList: TStringList;
  vID: Integer;
  vDateTime: TDateTime;
begin
  vStringList := TStringList.Create;
  try
    for I := 0 to FStringListTest.Count - 1 do
      if Trim(FStringListTest[I]) <> '' then
      begin
        vStringList.CommaText := FStringListTest[I];
        if (vStringList.Count = 2)
           and TryStrToInt(vStringList[0], vID)
           and TryStrToDateTime(vStringList[1], vDateTime)
           and (vID = OrganizerCellEvent.ID)
        then
        begin
          vStringList.Clear;
          vStringList.Add(IntToStr(OrganizerCellEvent.ID));
          vStringList.Add(DateTimeToStr(OrganizerCellEvent.DateTime));
          FStringListTest[I] := vStringList.CommaText;
          Exit;
        end;
      end;
  finally
    vStringList.Free;
    Success := True;
  end;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFirstRun := True;

  FStringListTest := TStringList.Create;
  if FileExists('TestData.txt') then
    FStringListTest.LoadFromFile('TestData.txt');

  pLogo.Color := cPrimaryColor;

  pCalendarContent.Visible := False;
  pToDoContent.Visible := False;

  TOrganizerMenuButton.Create(pDashboard, 'Dashboard', 'Icons/fa-dashcube_16_0_c8c8c8_none.png', $002b39c0);
  TOrganizerMenuButton.Create(pCalendar, 'Calendar', 'Icons/fa-calendar_16_0_c8c8c8_none.png', $000054d3);
  TOrganizerMenuButton.Create(pToDo, 'To-do list', 'Icons/octicon-checklist_16_0_c8c8c8_none.png', $00129cf3);
  TOrganizerMenuButton.Create(pMeetings, 'Meetings', 'Icons/icomoon-user-tie_16_0_c8c8c8_none.png', $000fc4f1);
  TOrganizerMenuButton.Create(pContacts, 'Contact list', 'Icons/fa-users_16_0_c8c8c8_none.png', $0060ae27);
  TOrganizerMenuButton.Create(pAlarms, 'Alarms', 'Icons/icomoon-alarm_16_0_c8c8c8_none.png', $00b98029);
  TOrganizerMenuButton.Create(pSettings, 'Settings', 'Icons/octicon-settings_16_0_c8c8c8_none.png', $00A76177);

  Panel18.Color := $002b39c0;
  Panel19.Color := $000054d3;
  Panel20.Color := $00129cf3;
  Panel21.Color := $00A76177;

  TOrganizerEdit.Create(Panel9, 'First name');

  TOrganizerCheckBox.Create(Panel10, 'Active alarm', 'Icons/icomoon-checkmark_14_0_ffffff_none.png');

  TOrganizerButton.Create(Panel1, 'Post');
  TOrganizerCancelButton.Create(Panel16, 'Cancel');

  TOrganizerComboBox.Create(Panel2, 'ComboBox');

  TOrganizerButton.Create(Panel4, 'Poniedziałek');
  TOrganizerButton.Create(Panel5, 'Wtorek');
  TOrganizerButton.Create(Panel6, 'Środa');
  TOrganizerButton.Create(Panel7, 'Czwartek');
  TOrganizerButton.Create(Panel8, 'Piątek');
  TOrganizerButton.Create(Panel11, 'Sobota');
  TOrganizerButton.Create(Panel12, 'Niedziela');
  TOrganizerButton.Create(Panel13, 'Marzec');
  TOrganizerButton.Create(Panel14, 'Kwiecien');

  TOrganizerListBox.Create(Panel15);

  TOrganizerComboBoxList.Create(Panel23, ['Poniedziałek', 'Wtorek', 'Środa', 'Czwartek']);

  FOrganizerCalendar := TOrganizerCalendar.Create(pCalendarControl);
  FOrganizerCalendar.OnSelectDataEvent := @OnOrganizerSelectDataEvent;
  FOrganizerCalendar.OnInsertDataEvent := @OnOrganizerInserDataEvent;
  FOrganizerCalendar.OnUpdateDataEvent := @OnOrganizerUpdateDataEvent;
  FOrganizerCalendar.Date := Now;

  FOrganizerCalendar1 := TOrganizerCalendar.Create(pCalendarControl1);

  FOrganizerContent := TOrganizerContent.Create(pOrganizerContent);
end;

destructor TMainForm.Destroy;
begin
  inherited Destroy;
  FStringListTest.SaveToFile('TestData.txt');
  FStringListTest.Free;
end;

end.

