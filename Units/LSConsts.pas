(*
  LazSolutions, Consts unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSConsts;

{$I lazsolutions.inc}

interface

uses
  SysUtils;

const
  CLazSolutionsHomePage = 'https://github.com/silvioprog/lazsolutions';
  CLazSolutionsDescription = 'LazSolutions';
  CLSExpertsConfigFile = 'lazsolutionsexpertsconfig.json';
  CLSUtilsHexCharsArray: array[0..15] of Char = ('0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  CLSUtilsFormatMACMask = '%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x';
  CLSUtilsUUIDMask = '%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x';
{$IFDEF UNIX}
  CLSUtilsDistros =
    'Annvix: /etc/annvix-release' + LineEnding +
    'Arch Linux: /etc/arch-release' + LineEnding +
    'Arklinux: /etc/arklinux-release' + LineEnding +
    'Aurox Linux: /etc/aurox-release' + LineEnding +
    'BlackCat: /etc/blackcat-release' + LineEnding +
    'Cobalt: /etc/cobalt-release' + LineEnding +
    'Conectiva: /etc/conectiva-release' + LineEnding +
    'Debian: /etc/debian_version, /etc/debian_release' + LineEnding +
    'Fedora Core: /etc/fedora-release' + LineEnding +
    'Gentoo Linux: /etc/gentoo-release' + LineEnding +
    'Immunix: /etc/immunix-release' + LineEnding +
    'Knoppix: knoppix_version' + LineEnding +
    'Linux-From-Scratch: /etc/lfs-release' + LineEnding +
    'Linux-PPC: /etc/linuxppc-release' + LineEnding +
    'Mandrake: /etc/mandrake-release' + LineEnding +
    'Mandriva/Mandrake Linux: /etc/mandriva-release, /etc/mandrake-release, /etc/mandakelinux-release' + LineEnding +
    'MkLinux: /etc/mklinux-release' + LineEnding +
    'Novell Linux Desktop: /etc/nld-release' + LineEnding +
    'PLD Linux: /etc/pld-release' + LineEnding +
    'Red Hat: /etc/redhat-release, /etc/redhat_version' + LineEnding +
    'Slackware: /etc/slackware-version, /etc/slackware-release' + LineEnding +
    'SME Server (Formerly E-Smith): /etc/e-smith-release' + LineEnding +
    'Solaris SPARC: /etc/release' + LineEnding +
    'Sun JDS: /etc/sun-release' + LineEnding +
    'SUSE Linux: /etc/SuSE-release, /etc/novell-release' + LineEnding +
    'SUSE Linux ES9: /etc/sles-release' + LineEnding +
    'Tiny Sofa: /etc/tinysofa-release' + LineEnding +
    'TurboLinux: /etc/turbolinux-release' + LineEnding +
    'Ubuntu Linux: /etc/lsb-release' + LineEnding +
    'UltraPenguin: /etc/ultrapenguin-release' + LineEnding +
    'UnitedLinux: /etc/UnitedLinux-release' + LineEnding +
    'VA-Linux/RH-VALE: /etc/va-release' + LineEnding +
    'Yellow Dog: /etc/yellowdog-release';
  CLSUtilsDistrosPlayer =
    'Annvix: TODO' + LineEnding +
    'Arch Linux: TODO' + LineEnding +
    'Arklinux: TODO' + LineEnding +
    'Aurox Linux: TODO' + LineEnding +
    'BlackCat: TODO' + LineEnding +
    'Cobalt: TODO' + LineEnding +
    'Conectiva: TODO' + LineEnding +
    'Debian: TODO' + LineEnding +
    'Fedora Core: TODO' + LineEnding +
    'Gentoo Linux: TODO' + LineEnding +
    'Immunix: TODO' + LineEnding +
    'Knoppix: TODO' + LineEnding +
    'Linux-From-Scratch: TODO' + LineEnding +
    'Linux-PPC: TODO' + LineEnding +
    'Mandrake: TODO' + LineEnding +
    'Mandriva/Mandrake Linux: aplay' + LineEnding +
    'MkLinux: TODO' + LineEnding +
    'Novell Linux Desktop: TODO' + LineEnding +
    'PLD Linux: TODO' + LineEnding +
    'Red Hat: aplay' + LineEnding +
    'Slackware: TODO' + LineEnding +
    'SME Server (Formerly E-Smith): TODO' + LineEnding +
    'Solaris SPARC: TODO' + LineEnding +
    'Sun JDS: TODO' + LineEnding +
    'SUSE Linux: aplay' + LineEnding +
    'SUSE Linux ES9: TODO' + LineEnding +
    'Tiny Sofa: TODO' + LineEnding +
    'TurboLinux: TODO' + LineEnding +
    'Ubuntu Linux: aplay, paplay' + LineEnding +
    'UltraPenguin: TODO' + LineEnding +
    'UnitedLinux: TODO' + LineEnding +
    'VA-Linux/RH-VALE: TODO' + LineEnding +
    'Yellow Dog: TODO';
{$ENDIF}
  CLSUtilsProcessNameValueSeparator: Char = '=';
  CLSUtilsHintName = 'LSHintWindow_32E6BF9804A241DD9362AC950C41693E';
  CLSConfigKey = 'DA58C019EE1C49DF8AACF8D5CC535D87';
  CLSCONFIGFILENAME = 'lsconfig.conf';
  CLSHTTPSendGetIPAPIURL =
    'http://api.silvioprog.com.br/ls/v1/getip.ls?key=' +
    '0a92fab3230134cca6eadd9898325b9b2ae67998';
  CLSHTTPSendGETMethod = 'GET';
  CLSHTTPSendDELETEMethod = 'DELETE';
  CLSHTTPSendPOSTMethod = 'POST';
  CLSHTTPSendPUTMethod = 'PUT';
  CLSHTTPSendContentType_x_www_form_urlencoded =
    'application/x-www-form-urlencoded';
  CLSHTTPSendContentType_octet_string = 'application/octet-string';
  CLSHTTPSendContentType_json = 'application/json';
  CLSHTTPSendContentType_xml = 'application/xml';
  CLSHTTPSendFormatIPMask = '%d.%d.%d.%d';
  CLSHTTPSendResultCodeOK = 200;
  CLSHTTPSendResultCodeRedirect = 302;
  CLSHTTPSendResultCodeTemporaryRedirect = 307;
  CLSHTTPSendResultCodeUnauthorized = 401;
  CLSHTTPSendResultCodeForbidden = 403;
  CLSHTTPSendResultCodeProxyAuthenticationRequired = 407;
  CLSHTTPSendBoundaryEndPart = '_LazSolutions_boundary';
  CLSSMTPSendDispNotificationTo = 'Disposition-Notification-To: ';
  CLSHTTPSENDGLOBALPROXYCONFIGHOST = 'lshttpsendproxyhost';
  CLSHTTPSENDGLOBALPROXYCONFIGPORT = 'lshttpsendproxyport';
  CLSHTTPSENDGLOBALPROXYCONFIGUSER = 'lshttpsendproxyuser';
  CLSHTTPSENDGLOBALPROXYCONFIGPASS = 'lshttpsendproxypass';
  CLSHTTPSENDGLOBALPROXYCONFIGFILENAME = 'lshttpsendproxy.conf';
  CLSHTTPSENDGLOBALUPCONFIGUSER = 'lshttpsenduser';
  CLSHTTPSENDGLOBALUPCONFIGPASS = 'lshttpsendpass';
  CLSHTTPSENDGLOBALUPCONFIGFILENAME = 'lshttpsendup.conf';
  CLSExpressionNoError = 0;
  CLSExpressionEmptyError = 1;
  CLSExpressionExprError = 2;
  CLSExpressionSolveError = 3;
  CLSExpressionDivByZeroError = 4;
  CLSExpressionFuncError = 5;
  CLSExpressionFuncNotFoundError = 6;
  CLSEllipsis = '...';
  CLSDuplicateName = 'A named "%s" already exists.';
  CLSInvalidName = '"%s" is not a valid name.';
  CLSDateEditDisplayFrmtMDY = 'mm/dd/yyyy';
  CLSDateEditDisplayFrmtDMY = 'dd/mm/yyyy';
  CLSDateEditDisplayFrmtYMD = 'yyyy/mm/dd';
  CLSDateEditMaskMDY = '99/99/9999;1;_';
  CLSDateEditMaskDMY = '99/99/9999;1;_';
  CLSDateEditMaskYMD = '9999/99/99;1;_';
  CLSGeoIPGoogleMapsAPIURL =
    'http://maps.google.com/staticmap?key=%s&size=%dx%d&markers=%f,%f&zoom=%d';

var
  CLSHTTPSendAttempts: Byte = 3;
  CLSConstsNullDate: TDateTime = 0;
  CLSConstsNullTime: TDateTime = 0;
  CLSConstsTimeFormat: ShortString = 'hh:mm:ss';
  CLSConstsDateFormat: ShortString = 'yyyy/mm/dd';
  CLSConstsExtractIPRegEx: string =
    '^(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9])\.(25[0-5]' +
    '|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4]' +
    '[0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4][0-9]|[' +
    '0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])$';
  CLSConstsExtractEmailRegEx: string = '([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)';
  CLSConstsExtractEmailRegEx2: string =
    '[^\w\d\-\.]([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)[^\w\d\-\.]';
  CLSConstsExtractURLRegEx: string =
    '((http)|(https)|(ftp)):\/\/([\- \w]+\.)+\w{2,3}(\/ [%\-\w]+(\.\w{2,})?)*';
  {
    Matches
    (12) 123 1234 | (01512) 123 1234 | (0xx12) 1234 1234
    Non-Matches
    12 123 1234 | (012) 123/1234 | (012) 123 12345

    See: http://regexlib.com/REDetails.aspx?regexp_id=34
  }
  CLSConstsExtractTelRegEx: string =
    '\(([0-9]{2}|0{1}((x|[0-9]){2}[0-9]{2}))\)\s*[0-9]{3,4}[- ]*[0-9]{4}';
  {
    Matches
    18008793262 | 800-879-3262 | 0-800.879.3262
    Non-Matches
    879 3262 | 077 879 3262 | 879-3262
  }
  CLSConstsExtractTelRegEx2: string =
    '^[01]?[- .]?\(?[2-9]\d{2}\)?[- .]?\d{3}[- .]?\d{4}$';
  CLSSendMailPath: string =
{$IFDEF UNIX}
    '/usr/bin/lssendmail'
{$ELSE}
    '%s\LazSolutions\LSSendMail\lssendmail.exe'
{$ENDIF};
  CLSTimeEditDisplayFrmtHM: string = 't';
  CLSTimeEditDisplayFrmtHMS: string = 'tt';
  CLSTimeEditMaskHM: string = '!#9%s99;1;_';
  CLSTimeEditMaskHMS: string = '!#9%s99%s99;1;_';
  CLSGeoIPCountryDATFileName: TFileName = 'GeoIP.dat';
  CLSGeoIPCityDATFileName: TFileName = 'GeoLiteCity.dat';

implementation

initialization
{$IFDEF MSWINDOWS}
  CLSSendMailPath := Format(CLSSendMailPath, [GetEnvironmentVariable(
    'programfiles')]);
{$ENDIF}
  CLSTimeEditMaskHM := Format(CLSTimeEditMaskHM,
    [DefaultFormatSettings.TimeSeparator]);
  CLSTimeEditMaskHMS := Format(CLSTimeEditMaskHMS,
    [DefaultFormatSettings.TimeSeparator, DefaultFormatSettings.TimeSeparator]);

end.

