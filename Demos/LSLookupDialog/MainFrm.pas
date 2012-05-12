unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, SysUtils, Variants;

type

  { TMainForm }

  TMainForm = class(TForm)
    Test2Button: TButton;
    Value2Edit: TEdit;
    Description2Label: TLabel;
    TestButton: TButton;
    ValueEdit: TEdit;
    DescriptionLabel: TLabel;
    procedure Test2ButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
  end;

const
  CSelectItem = 'Select a item';
  CJSON = '{"Item 1": 1, "Item 2": "A", "Item 3": true, "Item 4": 1.5}';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSDialogs, FPJSON;

{ TMainForm }

procedure TMainForm.TestButtonClick(Sender: TObject);
var
  VJSONObject: TJSONObject = nil;
  VText: TJSONStringType = '';
begin
  VText := DescriptionLabel.Caption;
  if LSJSONLookupDialog(CSelectItem, VText, CJSON, VJSONObject) then
  begin
    ValueEdit.Text := VJSONObject.AsString;
    DescriptionLabel.Caption := VText;
    FreeAndNil(VJSONObject);
  end;
end;

procedure TMainForm.Test2ButtonClick(Sender: TObject);
var
  VValue: Variant;
  VText: TJSONStringType = '';
begin
  VValue := Null;
  VText := Description2Label.Caption;
  if LSJSONLookupDialog(CSelectItem, VText, CJSON, VValue) then
  begin
    Value2Edit.Text := VarToStr(VValue);
    Description2Label.Caption := VText;
  end;
end;

end.

