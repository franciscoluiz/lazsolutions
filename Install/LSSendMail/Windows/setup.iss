; LazSolutions, Send mail module
; Copyright (C) 2010-2012 Silvio Clecio.
;
; http://silvioprog.com.br
;
; See the file LICENSE.txt, included in this distribution,
; for details about the copyright.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

[Setup]
AppId={{C00D1D82-196A-46E6-94D4-C00B0F3FD961}
AppName=LSSendMail
AppVerName=LSSendMail 1.6
AppPublisher=LazSolutions
AppPublisherURL=https://github.com/silvioprog/lazsolutions
AppSupportURL=https://github.com/silvioprog/lazsolutions
AppUpdatesURL=https://github.com/silvioprog/lazsolutions
DefaultDirName={pf}\LazSolutions\LSSendMail
DefaultGroupName=LSSendMail
DisableProgramGroupPage=true
LicenseFile=..\..\..\LGPL.2.1.txt
InfoBeforeFile=..\..\..\Modules\LSSendMail\Readme.txt
OutputBaseFilename=lssendmail_1.6_win32_all
Compression=lzma/ultra
SolidCompression=true
VersionInfoVersion=1.6
VersionInfoCompany=LazSolutions
VersionInfoDescription=LSSendMail is a module to send e-mail.
VersionInfoTextVersion=1.6
VersionInfoCopyright=Copyright (C) 2010-2012 Silvio Clecio
VersionInfoProductName=LSSendMail
VersionInfoProductVersion=1.6
AppCopyright=Copyright © 2010-2012 Silvio Clecio
UserInfoPage=true
AppVersion=1.6
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp
ShowTasksTreeLines=true
DisableStartupPrompt=false
ShowLanguageDialog=no
PrivilegesRequired=none
DirExistsWarning=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
Source: ..\..\..\Modules\LSSendMail\lssendmail.exe; DestDir: {app}; Flags: ignoreversion; Languages: 
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: ..\..\..\..\media\lib\win32\ssleay32.dll; DestDir: {app}
Source: ..\..\..\..\media\lib\win32\libeay32.dll; DestDir: {app}
Source: ..\..\..\LGPL.2.1.txt; DestDir: {app}
Source: ..\..\..\Modules\LSSendMail\Readme.txt; DestDir: {app}

[Icons]
Name: {group}\{cm:ProgramOnTheWeb,LSSendMail}; Filename: https://github.com/silvioprog/lazsolutions
Name: {group}\{cm:UninstallProgram,LSSendMail}; Filename: {uninstallexe}

[Messages]
BeveledLabel=LazSolutions - https://github.com/silvioprog/lazsolutions
[Registry]
Root: HKLM; SubKey: SOFTWARE\LazSolutions\LSSendMail; ValueType: string; ValueName: lssendmailpath; ValueData: {app}\lssendmail.exe; Flags: uninsdeletekey

[UninstallDelete]
Name: {app}; Type: filesandordirs;
