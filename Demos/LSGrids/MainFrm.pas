unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, Dialogs, LSGrids;

type

  { TMainForm }

  TMainForm = class(TForm)
    FindButton: TButton;
    ClearButton: TButton;
    FindDialog1: TFindDialog;
    JSONPopulateGridButton: TButton;
    JSONLoadButton: TButton;
    JSONSaveButton: TButton;
    JSONLSStringGrid: TLSStringGrid;
    InfoLabel: TLabel;
    MainPageControl: TPageControl;
    JSONTabSheet: TTabSheet;
    JSONBottomPanel: TPanel;
    procedure ClearButtonClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JSONLoadButtonClick(Sender: TObject);
    procedure JSONLSStringGridClick(Sender: TObject);
    procedure JSONPopulateGridButtonClick(Sender: TObject);
    procedure JSONSaveButtonClick(Sender: TObject);
  end;

const
  CJSONData =
    '[ { "ID": 1, "Name": "Blaise Pascal", "Age": null},' + LineEnding +
    '  { "ID": 2, "Name": "Linus Torvalds", "Age": 42},' + LineEnding +
    '  { "ID": 3, "Name": "Tux", "Age": []},' + LineEnding +
    '  { "ID": 4, "Name": { "Name": null } , "Age": false},' + LineEnding +
    '  { "ID": 5, "Name": "Lazarus and Free Pascal", "Age": 20.5},' + LineEnding +
    '  { "ID": 6, "Name": "LazSolutions", "Age": 1.5},' + LineEnding +
    '  { "ID": 7, "Name": "silvioprog - Silvio Clécio", "Age": true}' + LineEnding +
    ']';

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSDialogs;

{ TMainForm }

procedure TMainForm.JSONPopulateGridButtonClick(Sender: TObject);
begin
  JSONLSStringGrid.LoadFromJSONString(CJSONData);
end;

procedure TMainForm.JSONSaveButtonClick(Sender: TObject);
var
  VFileName: TFileName;
begin
  VFileName := 'griddata.json';
  LSSaveDialog(VFileName);
  JSONLSStringGrid.SaveToJSONFile(VFileName);
end;

procedure TMainForm.JSONLoadButtonClick(Sender: TObject);
begin
  JSONLSStringGrid.LoadFromJSONFile(LSOpenDialog);
end;

procedure TMainForm.JSONLSStringGridClick(Sender: TObject);
begin
  ShowMessage(JSONLSStringGrid.SelectedRow.AsJSON);
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  JSONLSStringGrid.EmptyGrid;
end;

procedure TMainForm.FindButtonClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TMainForm.FindDialog1Find(Sender: TObject);
begin
  JSONLSStringGrid.Find(FindDialog1.FindText,
    frMatchCase in FindDialog1.Options);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FindDialog1.Options := [frDown, frHideWholeWord, frHideUpDown];
end;

end.

