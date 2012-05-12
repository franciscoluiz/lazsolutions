unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    InfoLabel: TLabel;
    RunOnceMonitorTimer: TTimer;
    procedure RunOnceMonitorTimerTimer(Sender: TObject);
  end;

const
  CMyAppAtom = 'myapp';

var
  MainForm: TMainForm;
  FirstStart: Boolean = True;

implementation

{$R *.lfm}

uses
  LSUtils;

{ TMainForm }

procedure TMainForm.RunOnceMonitorTimerTimer(Sender: TObject);
begin
  if LSGlobalFindAtom(CMyAppAtom) then
  begin
    LSGlobalDeleteAtom(CMyAppAtom);
    if not FirstStart then
    begin
      if WindowState = wsMinimized then
        Application.Restore
      else
        BringToFront;
    end;
    FirstStart := False;
  end;
end;

end.

