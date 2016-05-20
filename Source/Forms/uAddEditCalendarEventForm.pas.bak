unit uAddEditCalendarEventForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,

  uOrganizerControls,
  uOrganizerForm;

type

  { TAddEditCalendarEventForm }

  TAddEditCalendarEventForm = class(TOrganizerForm)
    Panel2: TPanel;
    pCancel1: TPanel;
    pPost: TPanel;
    pCancel: TPanel;
    procedure pCancelClick(Sender: TObject);
    procedure pPostClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

procedure ShowAddEditCalendarEventForm(OwnerForm: TOrganizerForm);

implementation

{$R *.lfm}

procedure ShowAddEditCalendarEventForm(OwnerForm: TOrganizerForm);
var
  vAddEditCalendarEventForm: TAddEditCalendarEventForm;
begin
  vAddEditCalendarEventForm := TAddEditCalendarEventForm.Create(OwnerForm);
  try
    vAddEditCalendarEventForm.TransparentShowModal;
  finally
    vAddEditCalendarEventForm.Free;
  end;
end;

{ TAddEditCalendarEventForm }

procedure TAddEditCalendarEventForm.pPostClick(Sender: TObject);
begin
  Close;
end;

procedure TAddEditCalendarEventForm.pCancelClick(Sender: TObject);
begin
  Close;
end;

constructor TAddEditCalendarEventForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  TOrganizerEdit.Create(pCancel1, 'Enter event name');

  TOrganizerComboBox.Create(Panel2, 'Dzień tygodnia');

  TOrganizerButton.Create(pPost, 'Save');
  TOrganizerCancelButton.Create(pCancel, 'Cancel');
end;

end.

