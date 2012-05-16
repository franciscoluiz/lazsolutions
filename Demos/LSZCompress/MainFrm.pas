unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CompressStringButton: TButton;
    UnCompressStringButton: TButton;
    CompressFileButton: TButton;
    UnCompressFileButton: TButton;
    InputStringEdit: TEdit;
    ResultEdit: TEdit;
    OutputStringEdit: TEdit;
    MainPageControl: TPageControl;
    CompressStringTabSheet: TTabSheet;
    CompressFileTabSheet: TTabSheet;
    procedure CompressFileButtonClick(Sender: TObject);
    procedure CompressStringButtonClick(Sender: TObject);
    procedure UnCompressFileButtonClick(Sender: TObject);
    procedure UnCompressStringButtonClick(Sender: TObject);
  end; 

var
  MainForm: TMainForm;
  ZString, FileIn, FileOut: string;

implementation

{$R *.lfm}

uses
  LSZCompress, LSDialogs, LCLIntf;

{ TMainForm }

procedure TMainForm.CompressStringButtonClick(Sender: TObject);
begin
  ZString := LSZCompressString(InputStringEdit.Text, clMax);
  ResultEdit.Text := ZString;
end;

procedure TMainForm.UnCompressFileButtonClick(Sender: TObject);
begin
  FileIn := LSOpenDialog;
  if FileOut = '' then
    FileOut := 'Test.txt';
  LSSaveDialog(FileOut);
  LSZUnCompressFile(FileIn, FileOut);
  OpenDocument(FileOut);
end;

procedure TMainForm.CompressFileButtonClick(Sender: TObject);
begin
  FileIn := LSOpenDialog;
  if FileOut = '' then
    FileOut := 'Test.txt';
  LSSaveDialog(FileOut);
  LSZCompressFile(FileIn, FileOut, clMax);
end;

procedure TMainForm.UnCompressStringButtonClick(Sender: TObject);
begin
  OutputStringEdit.Text := LSZUnCompressString(ZString);
end;

end.

