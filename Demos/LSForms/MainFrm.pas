unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSForms;

initialization
  LSRegisterEnterAsTAB([TCustomMemo, TCustomButton // to skip memos and buttons...
    ]);

end.

