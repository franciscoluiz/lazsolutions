unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Dialogs, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    LoadButton: TButton;
    SaveButton: TButton;
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSConfig;

{ TMainForm }

procedure TMainForm.LoadButtonClick(Sender: TObject);
var
  VString: string = '';
  VBoolean: Boolean = False;
  VInteger: Integer = 0;
  VLSConfigOutputValues: TLSConfigOutputValues = nil;
begin
  LSLoadConfig(['vstring', 'vboolean', 'vinteger'], [VString, VBoolean,
    VInteger], [@VString, @VBoolean, @VInteger]);
  ShowMessage('String value is: ' + VString + ', Boolean value is: ' +
    BoolToStr(VBoolean, 'True', 'False') + ', Integer value is: ' +
    IntToStr(VInteger));
  LSLoadConfig(['vstring', 'vboolean', 'vinteger'], VLSConfigOutputValues);
  ShowMessage('String value is: ' + VLSConfigOutputValues[0].AsString +
    ', Boolean value is: ' + BoolToStr(VLSConfigOutputValues[1].AsBoolean,
    'True', 'False') + ', Integer value is: ' +
    IntToStr(VLSConfigOutputValues[2].AsInteger));
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  LSSaveConfig(['vstring', 'vboolean', 'vinteger'], ['Test', True, 7]);
end;

end.

