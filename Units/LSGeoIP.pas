(*
  LazSolutions, GeoIP unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSGeoIP;

{$I lazsolutions.inc}

interface

uses
  LSConsts, LSHTTPSend, GeoIP, Classes, SysUtils;

type
  TLSGeoIPDBInfo = (dbiCountry, dbiCity);

{ Lookup information from the MaxMind database. }
function LSGeoIPLookupDBInfo(const ADBInfo: TLSGeoIPDBInfo = dbiCountry): string;
{ Lookup country. (Code and name) }
function LSGeoIPLookupCountry(const AIP: ShortString): TGeoIPCountry;
{ Lookup city. (code, country name, region, city, postal code, latitude,
  longitude, Dma code and  areaCode). }
function LSGeoIPLookupCity(const AIP: ShortString): TGeoIPCity;
{ Lookup map via Google Maps API. }
function LSGeoIPLookupGoogleMaps(var AImage: TStream; const ALatitude,
  ALongitude: Double; const AGoogleMapsAPIKey: string = '';
  const AImageWidthSize: Integer = 390; const AImageHeightSize: Integer = 175;
  const AImageZoom: Byte = 8): Boolean;

implementation

function LSGeoIPLookupDBInfo(const ADBInfo: TLSGeoIPDBInfo): string;
var
  VGeoIP: TGeoIP;
begin
  case ADBInfo of
    dbiCountry: VGeoIP := TGeoIP.Create(CLSGeoIPCountryDATFileName);
    dbiCity: VGeoIP := TGeoIP.Create(CLSGeoIPCityDATFileName);
  end;
  try
    Result := VGeoIP.GetDatabaseInfo;
  finally
    VGeoIP.Free;
  end;
end;

function LSGeoIPLookupCountry(const AIP: ShortString): TGeoIPCountry;
var
  VGeoIP: TGeoIP;
begin
  VGeoIP := TGeoIP.Create(CLSGeoIPCountryDATFileName);
  try
    if not (VGeoIP.GetCountry(AIP, Result) = GEOIP_SUCCESS) then
      with Result do
      begin
        CountryCode := '';
        CountryName := '';
      end;
  finally
    VGeoIP.Free;
  end;
end;

function LSGeoIPLookupCity(const AIP: ShortString): TGeoIPCity;
var
  VGeoIP: TGeoIP;
begin
  VGeoIP := TGeoIP.Create(CLSGeoIPCityDATFileName);
  try
    if not (VGeoIP.GetCity(AIP, Result) = GEOIP_SUCCESS) then
      with Result do
      begin
        CountryCode := '';
        CountryName := '';
        Region := '';
        City := '';
        PostalCode := '';
        Latitude := 0;
        Longitude := 0;
        DmaCode := 0;
        AreaCode := 0;
      end;
  finally
    VGeoIP.Free;
  end;
end;

function LSGeoIPLookupGoogleMaps(var AImage: TStream; const ALatitude,
  ALongitude: Double; const AGoogleMapsAPIKey: string;
  const AImageWidthSize, AImageHeightSize: Integer;
  const AImageZoom: Byte): Boolean;
var
  VOldDecSep: Char;
begin
  VOldDecSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    Result := LSHTTPGetBinary(Format(CLSGeoIPGoogleMapsAPIURL,
      [AGoogleMapsAPIKey, AImageWidthSize, AImageHeightSize, ALongitude,
      ALatitude, AImageZoom]), AImage);
    if Result then
      AImage.Position := 0;
  finally
    DefaultFormatSettings.DecimalSeparator := VOldDecSep;
  end;
end;

end.

