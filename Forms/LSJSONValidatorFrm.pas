(*
  LazSolutions, JSON validator Form
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSJSONValidatorFrm;

{$I lazsolutions.inc}

interface

uses
{$IFNDEF LSNEWFPC}
  JSONScanner,
{$ENDIF}
  LSDialogs, LSJSONPropStorage, LSLazIDEIntf, FPJSON, JSONParser, SysUtils,
  Forms, Controls, Graphics, ExtCtrls, StdCtrls, SynEdit, LCLType, ComCtrls,
  Menus, SynEditTypes, SynHighlighterJava;

type

  { TLSJSONValidatorForm }

  TLSJSONValidatorForm = class(TForm)
    GoToErrorButton: TButton;
    JSONMainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    ConfigLSJSONPropStorage: TLSJSONPropStorage;
    N1: TMenuItem;
    SaveMenuItem: TMenuItem;
    N3: TMenuItem;
    GoToErrorMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    N2: TMenuItem;
    ExitMenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    ValidateMenuItem: TMenuItem;
    OKButton: TButton;
    JSONStatusBar: TStatusBar;
    JSONSynJavaSyn: TSynJavaSyn;
    ValidateButton: TButton;
    BottomPanel: TPanel;
    JSONSynEdit: TSynEdit;
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FileMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure GoToErrorButtonClick(Sender: TObject);
    procedure GoToErrorMenuItemClick(Sender: TObject);
    procedure JSONSynEditChange(Sender: TObject);
    procedure JSONSynEditChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    procedure JSONSynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure OKButtonClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure ScriptMenuItemClick(Sender: TObject);
    procedure ValidateButtonClick(Sender: TObject);
    procedure ValidateMenuItemClick(Sender: TObject);
  private
    FJSONFileName: TFileName;
    FJSONChanged: Boolean;
    FCurColumn: Integer;
    FCurRow: Integer;
  public
    class procedure Execute;
    procedure ValidateJSON;
    procedure GoToError;
  end;

  { TLocalJSONParser }

  TLocalJSONParser = class(TJSONParser)
  public
{$IFDEF LSNEWFPC}
    property Scanner;
{$ELSE}
    property Scanner: TJSONScanner read FScanner;
{$ENDIF}
  end;

var
  LSJSONValidatorForm: TLSJSONValidatorForm;

implementation

{$R *.lfm}

{ TLSJSONValidatorForm }

{$HINTS OFF}
procedure TLSJSONValidatorForm.ValidateButtonClick(Sender: TObject);
begin
  ValidateJSON;
end;

procedure TLSJSONValidatorForm.ValidateMenuItemClick(Sender: TObject);
begin
  ValidateJSON;
end;

procedure TLSJSONValidatorForm.JSONSynEditChangeUpdating(ASender: TObject;
  AnUpdating: Boolean);
begin
  JSONStatusBar.SimpleText := Format(' Line: %d - Position: %d',
    [JSONSynEdit.CaretY, JSONSynEdit.CaretX]);
end;

procedure TLSJSONValidatorForm.JSONSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scSelection in Changes) and (scCaretX in Changes) and
    (scCaretY in Changes) then
    JSONSynEdit.SelectedColor.Background := clHighlight;
end;

procedure TLSJSONValidatorForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TLSJSONValidatorForm.OpenMenuItemClick(Sender: TObject);
begin
  FJSONFileName := LSOpenDialog;
  JSONSynEdit.Lines.LoadFromFile(FJSONFileName);
  JSONSynEdit.OnChange(Sender);
  FJSONChanged := False;
end;

procedure TLSJSONValidatorForm.SaveAsMenuItemClick(Sender: TObject);
var
  VFileName: TFileName;
begin
  VFileName := '*.json';
  if LSSaveDialog(VFileName, sdtFile, '', 'JSON file (*.json)|*.json', 0, True,
    True, 'json') then
  begin
    FJSONFileName := VFileName;
    JSONSynEdit.Lines.SaveToFile(FJSONFileName);
    FJSONChanged := False;
  end;
end;

procedure TLSJSONValidatorForm.SaveMenuItemClick(Sender: TObject);
begin
  if FJSONFileName = '' then
    SaveAsMenuItem.Click
  else
  begin
    JSONSynEdit.Lines.SaveToFile(FJSONFileName);
    FJSONChanged := False;
  end;
end;

procedure TLSJSONValidatorForm.ScriptMenuItemClick(Sender: TObject);
begin
  ValidateMenuItem.Enabled := ValidateButton.Enabled;
  N3.Visible := GoToErrorButton.Visible;
  GoToErrorMenuItem.Visible := GoToErrorButton.Visible;
end;

procedure TLSJSONValidatorForm.GoToErrorButtonClick(Sender: TObject);
begin
  GoToError;
end;

procedure TLSJSONValidatorForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TLSJSONValidatorForm.FileMenuItemClick(Sender: TObject);
begin
  SaveMenuItem.Enabled := FJSONChanged;
end;

procedure TLSJSONValidatorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  LSJSONValidatorForm.ConfigLSJSONPropStorage.
    StoredValue['FirstOpening'] := 'false';
  LSJSONValidatorForm.ConfigLSJSONPropStorage.Save;
  LSJSONValidatorForm := nil;
  CloseAction := caFree;
end;

procedure TLSJSONValidatorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if FJSONChanged then
  begin
    case Application.MessageBox('Save changes?', PChar(Caption),
      MB_ICONQUESTION + MB_YESNOCANCEL) of
      mrYes: SaveAsMenuItem.Click;
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TLSJSONValidatorForm.FormCreate(Sender: TObject);
begin
  ConfigLSJSONPropStorage.FileName := LSGetExpertsConfigFileName;
end;

procedure TLSJSONValidatorForm.GoToErrorMenuItemClick(Sender: TObject);
begin
  GoToError;
end;

procedure TLSJSONValidatorForm.JSONSynEditChange(Sender: TObject);
begin
  FJSONChanged := True;
  ValidateButton.Enabled := Trim(JSONSynEdit.Text) <> '';
  GoToErrorButton.Hide;
  if JSONSynEdit.Color <> clWhite then
    JSONSynEdit.Color := clWhite;
end;
{$HINTS ON}

class procedure TLSJSONValidatorForm.Execute;
begin
  if not Assigned(LSJSONValidatorForm) then
  begin
    Application.CreateForm(TLSJSONValidatorForm, LSJSONValidatorForm);
    LSJSONValidatorForm.ConfigLSJSONPropStorage.Restore;
    if LSJSONValidatorForm.ConfigLSJSONPropStorage.
        StoredValue['FirstOpening'] = '' then
      LSJSONValidatorForm.Position := poDesktopCenter;
  end;
  LSJSONValidatorForm.Show;
end;

procedure TLSJSONValidatorForm.ValidateJSON;
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  if not ValidateButton.Enabled then
    Exit;
  VJSONParser := TLocalJSONParser.Create(JSONSynEdit.Text);
  try
    try
      GoToErrorButton.Hide;
      VJSONParser.Strict := True;
      VJSONData := VJSONParser.Parse;
      JSONSynEdit.Text := VJSONData.FormatJSON([], 2);
      JSONSynEdit.Color := $00C2EFE7;
      FJSONChanged := JSONSynEdit.Modified;
      Application.MessageBox('Valid JSON!', PChar(Caption),
        MB_ICONINFORMATION + MB_OK);
    except
      on E: Exception do
      begin
        JSONSynEdit.Color := $00E4E3FB;
        GoToErrorButton.Show;
        Application.MessageBox(PChar('JSON ERROR: ' + E.Message),
          PChar(Caption), MB_ICONWARNING + MB_OK);
        FCurColumn := VJSONParser.Scanner.CurColumn;
        FCurRow := VJSONParser.Scanner.CurRow;
        JSONSynEdit.CaretX := FCurColumn;
        JSONSynEdit.CaretY := FCurRow;
        JSONSynEdit.SelectedColor.Background := $004080FF;
        JSONSynEdit.SelectLine;
        JSONSynEdit.SetFocus;
      end;
    end;
  finally
    VJSONData.Free;
    VJSONParser.Free;
  end;
end;

procedure TLSJSONValidatorForm.GoToError;
begin
  if not GoToErrorButton.Visible then
    Exit;
  JSONSynEdit.CaretX := FCurColumn;
  JSONSynEdit.CaretY := FCurRow;
  JSONSynEdit.SetFocus;
end;

end.

