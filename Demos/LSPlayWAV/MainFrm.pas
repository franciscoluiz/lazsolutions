unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    AsynchronousCheckBox: TCheckBox;
    PlayButton: TButton;
    SongsRadioGroup: TRadioGroup;
    procedure PlayButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSPlayWAV;

{ TMainForm }

procedure TMainForm.PlayButtonClick(Sender: TObject);
begin
  LSPlayWAVFile(ExtractFilePath(ParamStr(0)) +
    LowerCase(SongsRadioGroup.Items[SongsRadioGroup.ItemIndex]) + '.wav',
    AsynchronousCheckBox.Checked);
end;

end.

