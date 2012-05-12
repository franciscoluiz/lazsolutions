unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  LSExpression, LSConsts, Forms, ExtCtrls, StdCtrls, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    CalculateButton: TButton;
    ExpressionEdit: TLabeledEdit;
    ResultEdit: TLabeledEdit;
    RPNEdit: TLabeledEdit;
    procedure CalculateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    FLSExpression: TLSExpression;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FLSExpression := TLSExpression.Create;
end;

procedure TMainForm.CalculateButtonClick(Sender: TObject);
begin
  FLSExpression.Calculate(ExpressionEdit.Text);
  if FLSExpression.Error <> CLSExpressionNoError then
  begin
    RPNEdit.Clear;
    ResultEdit.Text := FLSExpression.ErrorStr;
  end
  else
  begin
    RPNEdit.Text := FLSExpression.RPN;
    ResultEdit.Text := FloatToStr(FLSExpression.Result);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLSExpression.Free;
end;

end.

