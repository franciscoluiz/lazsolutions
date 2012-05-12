unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, ComCtrls, ExtCtrls, Classes;

type

  { TMainForm }

  TMainForm = class(TForm)
    CityCountryCodeLabel: TLabel;
    CityFlagImage: TImage;
    CountryGetButton: TButton;
    CityGetButton: TButton;
    CountryGroupBox: TGroupBox;
    CountryCodeLabel: TLabel;
    CityGroupBox: TGroupBox;
    CityIPEdit: TEdit;
    CityIPLabel: TLabel;
    CountryNameLabel: TLabel;
    CountryIPEdit: TEdit;
    CountryFlagImage: TImage;
    CountryIPLabel: TLabel;
    CityCountryNameLabel: TLabel;
    CityInfoLabel: TLabel;
    CountryDBInfoLabel: TLabel;
    CityDBInfoLabel: TLabel;
    GoogleMapsImage: TImage;
    MainPageControl: TPageControl;
    CountryTabSheet: TTabSheet;
    CityTabSheet: TTabSheet;
    procedure CityGetButtonClick(Sender: TObject);
    procedure CountryGetButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    function GetFlagFileName(const ACountryCode: ShortString): TFileName;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSGeoIP, LSHTTPSend, GeoIP, LConvEncoding;

{ TMainForm }

procedure TMainForm.CountryGetButtonClick(Sender: TObject);
var
  VCountry: TGeoIPCountry;
begin
  CountryFlagImage.Picture.Clear;
  VCountry := LSGeoIPLookupCountry(CountryIPEdit.Text);
  CountryCodeLabel.Caption := VCountry.CountryCode;
  CountryNameLabel.Caption := VCountry.CountryName;
  if VCountry.CountryCode <> '' then
    CountryFlagImage.Picture.LoadFromFile(GetFlagFileName(VCountry.CountryCode));
end;

procedure TMainForm.CityGetButtonClick(Sender: TObject);
const
  CCityInfo =
    'Region: %s, City: %s, Postal code: %s, Latitude: %f' + LineEnding +
    'Longitude: %f, Dma code: %d, Area code: %d';
var
  VCity: TGeoIPCity;
  VImg: TStream;
begin
  CityFlagImage.Picture.Clear;
  GoogleMapsImage.Picture.Clear;
  VCity := LSGeoIPLookupCity(CityIPEdit.Text);
  CityCountryCodeLabel.Caption := VCity.CountryCode;
  CityCountryNameLabel.Caption := VCity.CountryName;
  CityInfoLabel.Caption := Format(CCityInfo, [VCity.Region,
    CP1252ToUTF8(VCity.City), VCity.PostalCode, VCity.Latitude, VCity.Longitude,
    VCity.DmaCode, VCity.AreaCode]);
  if VCity.CountryCode <> '' then
  begin
    CityFlagImage.Picture.LoadFromFile(GetFlagFileName(VCity.CountryCode));
    VImg := TMemoryStream.Create;
    try
      if LSGeoIPLookupGoogleMaps(VImg, VCity.Longitude, VCity.Latitude) then
        GoogleMapsImage.Picture.LoadFromStream(VImg);
    finally
      VImg.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CountryDBInfoLabel.Caption := LSGeoIPLookupDBInfo;
  CityDBInfoLabel.Caption := LSGeoIPLookupDBInfo(dbiCity);
  CountryIPEdit.Text := LSGetIP(iptExternal);
  CityIPEdit.Text := CountryIPEdit.Text;
end;

function TMainForm.GetFlagFileName(const ACountryCode: ShortString): TFileName;
const
  CFlagPath = 'Flags' + DirectorySeparator + '%s.png';
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    Format(CFlagPath, [LowerCase(ACountryCode)]);
end;

end.

