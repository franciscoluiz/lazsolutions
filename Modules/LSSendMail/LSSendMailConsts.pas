(*
  LazSolutions, Send mail module consts
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSSendMailConsts;

{$I lssendmail.inc}

interface

const
  CLSVersion = {$I Version.inc};
  CLSDispNotificationTo = 'Disposition-Notification-To: ';
  // App parameters
  CLSAboutParam = '--about';
  CLSHelpParam1 = '-h';
  CLSHelpParam2 = '--help';
  CLSFromParam = '-from';
  CLSToParam = '-to';
  CLSCCParam = '-cc';
  CLSBCCParam = '-bcc';
  CLSSubjectParam = '-subject';
  CLSMessageParam = '-message';
  CLSMessageFileParam = '-messagefile';
  CLSMessageTypeParam = '-messagetype';
  CLSAttachedParam = '-attached';
  CLSPriorityParam = '-priority';
  CLSConfirmReadingParam = '-confirmreading';
  CLSUserParam = '-user';
  CLSPasswordParam = '-password';
  CLSHostParam = '-host';
  CLSPortParam = '-port';
  CLSSSLParam = '-ssl';
  CLSTLSParam = '-tls';
  CLSAttemptParam = '-attempt';
{$IFDEF LSOPENSSL}
  CLSCheckOpenSSLParam = '--checkopenssl';
{$ENDIF}
  CLSVersionParam = '--version';
  CLSDebugModeParam = '-debugmode';

implementation

end.

