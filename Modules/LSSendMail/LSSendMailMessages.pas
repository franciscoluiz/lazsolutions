(*
  LazSolutions, Send mail module messages
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSSendMailMessages;

{$I lssendmail.inc}

interface

resourcestring
  {$I LSSendMailMessages.inc}

var
  SLSAboutMsg: string = SLSAboutMsg_rst;
  SLSHelpMsg: string = SLSHelpMsg_rst;
  SLSInsufficientParameters: string = SLSInsufficientParameters_rst;
  SLSParamEmptyError: string = SLSParamEmptyError_rst;
  SLSUnrecognizedParamError: string = SLSUnrecognizedParamError_rst;
  SLSUnknownError: string = SLSUnknownError_rst;

implementation

end.

