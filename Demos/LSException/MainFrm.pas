unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CauseErrorButton: TButton;
    procedure CauseErrorButtonClick(Sender: TObject);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSException;

{ TMainForm }

procedure TMainForm.CauseErrorButtonClick(Sender: TObject);
begin
  StrToInt('a');
end;

initialization
  TLSException.Register(True, 'Your Name', 'destination@gmail.com',
    'youremail@gmail.com', 'yourpassword', 'smtp.gmail.com', '465', True, True);

end.

