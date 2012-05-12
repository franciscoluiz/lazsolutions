unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Controls, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    UpdateListButton: TButton;
    RunningProcessesButton: TButton;
    KillProcessButton: TButton;
    ProcessListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure KillProcessButtonClick(Sender: TObject);
    procedure RunningProcessesButtonClick(Sender: TObject);
    procedure ProcessListBoxClick(Sender: TObject);
    procedure UpdateListButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSUtils;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateListButton.Click;
end;

procedure TMainForm.KillProcessButtonClick(Sender: TObject);
var
  S: string;
begin
  S := ProcessListBox.GetSelectedText;
  if MessageDlg('Kill process', 'Kill process "' + S + '"?', mtConfirmation,
    mbYesNo, 0) = mrYes then
    if LSKillProcess(S) then
    begin
      UpdateListButton.Click;
      Caption := 'Process';
    end;
end;

procedure TMainForm.RunningProcessesButtonClick(Sender: TObject);
var
  S: string = '';
begin
  if InputQuery('Find process', 'Put process name', S) then
    if LSProcessIsRunning(S) then
      ShowMessage('Process "' + S + '" is running.')
    else
      ShowMessage('Process "' + S + '" is not running.');
end;

procedure TMainForm.ProcessListBoxClick(Sender: TObject);
begin
  Caption := 'Process - ' + ProcessListBox.GetSelectedText;
  KillProcessButton.Enabled := ProcessListBox.ItemIndex > -1;
end;

procedure TMainForm.UpdateListButtonClick(Sender: TObject);
var
  VProcess: TStringList;
begin
  VProcess := TStringList.Create;
  try
    LSListProcess(VProcess);
    ProcessListBox.Items := VProcess;
  finally
    VProcess.Free;
  end;
end;

end.

