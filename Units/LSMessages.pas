(*
  LazSolutions, Messages unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSMessages;

{$I lazsolutions.inc}

interface

resourcestring
  {$I LSMessages.inc}

var
{$IFDEF UNIX}
  // Get Linux distro msgs
  SLSUtilsGetLinuxDistroError: string = SLSUtilsGetLinuxDistroError_rst;
  // Play WAV msgs
  SLSPlayWAVPlayerNotFound: string = SLSPlayWAVPlayerNotFound_rst;
{$ENDIF}
{$IFDEF MSWINDOWS}
  // List process msgs
  SLSUtilsListProcessError: string = SLSUtilsListProcessError_rst;
{$ENDIF}
  // Process thread msgs
  SLSProcessThreadCanceled: string = SLSProcessThreadCanceled_rst;
  // Exception msgs
  SLSExceptionDlgCaption: string = SLSExceptionDlgCaption_rst;
  SLSExceptionDlgCopyActCaption: string = SLSExceptionDlgCopyActCaption_rst;
  SLSExceptionDlgEmailActCaption: string = SLSExceptionDlgEmailActCaption_rst;
  SLSExceptionDlgTerminateActCaption: string = SLSExceptionDlgTerminateActCaption_rst;
  SLSExceptionDlgCloseActCaption: string = SLSExceptionDlgCloseActCaption_rst;
  SLSExceptionDlgCopyActHint: string = SLSExceptionDlgCopyActHint_rst;
  SLSExceptionDlgEmailActHint: string = SLSExceptionDlgEmailActHint_rst;
  SLSExceptionDlgTerminateActHint: string = SLSExceptionDlgTerminateActHint_rst;
  SLSExceptionDlgCloseActHint: string = SLSExceptionDlgCloseActHint_rst;
  SLSExceptionDlgMsg: string = SLSExceptionDlgMsg_rst;
  SLSExceptionDlgTerminateConfirmCaption: string = SLSExceptionDlgTerminateConfirmCaption_rst;
  SLSExceptionDlgTerminateConfirmMsg: string = SLSExceptionDlgTerminateConfirmMsg_rst;
  // HTTP msgs
  SLSHTTPSendGetIPError: string = SLSHTTPSendGetIPError_rst;
  SLSHTTPSendUnableToGetError: string = SLSHTTPSendUnableToGetError_rst;
  SLSHTTPSendDownloadCompleteMsg: string = SLSHTTPSendDownloadCompleteMsg_rst;
  SLSHTTPSendTotalDownloadSizeMsg: string = SLSHTTPSendTotalDownloadSizeMsg_rst;
  SLSHTTPSendProxyDlgCaption: string = SLSHTTPSendProxyDlgCaption_rst;
  SLSHTTPSendProxyDlgProxyGBoxCaption: string = SLSHTTPSendProxyDlgProxyGBoxCaption_rst;
  SLSHTTPSendProxyDlgAuthenticationGBox: string = SLSHTTPSendProxyDlgAuthenticationGBox_rst;
  SLSHTTPSendProxyDlgHostLblCaption: string = SLSHTTPSendProxyDlgHostLblCaption_rst;
  SLSHTTPSendProxyDlgPortLblCaption: string = SLSHTTPSendProxyDlgPortLblCaption_rst;
  SLSHTTPSendProxyDlgUserLblCaption: string = SLSHTTPSendProxyDlgUserLblCaption_rst;
  SLSHTTPSendProxyDlgPasswordLblCaption: string = SLSHTTPSendProxyDlgPasswordLblCaption_rst;
  SLSHTTPSendProxyDlgOKBtnCaption: string = SLSHTTPSendProxyDlgOKBtnCaption_rst;
  SLSHTTPSendProxyDlgCancelBtnCaption: string = SLSHTTPSendProxyDlgCancelBtnCaption_rst;
  SLSHTTPSendUPDlgCaption: string = SLSHTTPSendUPDlgCaption_rst;
  SLSHTTPSendUPDlgAuthenticationGBox: string = SLSHTTPSendUPDlgAuthenticationGBox_rst;
  SLSHTTPSendUPDlgUserLblCaption: string = SLSHTTPSendUPDlgUserLblCaption_rst;
  SLSHTTPSendUPDlgPasswordLblCaption: string = SLSHTTPSendUPDlgPasswordLblCaption_rst;
  SLSHTTPSendUPDlgOKBtnCaption: string = SLSHTTPSendUPDlgOKBtnCaption_rst;
  SLSHTTPSendUPDlgCancelBtnCaption: string = SLSHTTPSendUPDlgCancelBtnCaption_rst;
  // SMTP msgs
  SLSSMTPSendError: string = SLSSMTPSendError_rst;
  SLSSMTPSendEmailSentSuccessfully: string = SLSSMTPSendEmailSentSuccessfully_rst;
  // Expression msgs
  SLSExpressionNoError: string = SLSExpressionNoError_rst;
  SLSExpressionEmptyError: string = SLSExpressionEmptyError_rst;
  SLSExpressionExprError: string = SLSExpressionExprError_rst;
  SLSExpressionSolveError: string = SLSExpressionSolveError_rst;
  SLSExpressionDivByZeroError: string = SLSExpressionDivByZeroError_rst;
  SLSExpressionFuncError: string = SLSExpressionFuncError_rst;
  SLSExpressionFuncNotFoundError: string = SLSExpressionFuncNotFoundError_rst;
  SLSExpressionUnknownError: string = SLSExpressionUnknownError_rst;
  // Controls msgs
  SLSControlsRequiredFieldMsg: string = SLSControlsRequiredFieldMsg_rst;
  SLSControlsEmailFieldMsg: string = SLSControlsEmailFieldMsg_rst;
  SLSControlsURLFieldMsg: string = SLSControlsURLFieldMsg_rst;
  SLSControlsTelFieldMsg: string = SLSControlsTelFieldMsg_rst;
  SLSControlsPatternFieldMsg: string = SLSControlsPatternFieldMsg_rst;
  // JSON msgs
  SLSJSONEmptyDatabaseError: string = SLSJSONEmptyDatabaseError_rst;
  SLSJSONEmptyArrayError: string = SLSJSONEmptyArrayError_rst;
  SLSJSONEmptyObjectError: string = SLSJSONEmptyObjectError_rst;
  SLSJSONIsNotArrayError: string = SLSJSONIsNotArrayError_rst;
  SLSJSONIsNotObjectError: string = SLSJSONIsNotObjectError_rst;
  // Grids columns manager msgs
  SLSGridsColumnsManagerCaption: string = SLSGridsColumnsManagerCaption_rst;
  SLSGridsColumnsManagerOKBtnCaption: string = SLSGridsColumnsManagerOKBtnCaption_rst;
  SLSGridsColumnsManagerCancelBtnCaption: string = SLSGridsColumnsManagerCancelBtnCaption_rst;
  // Lookup dialog msgs
  SLSLookupDialogCaption: string = SLSLookupDialogCaption_rst;
  SLSLookupDialogFindLabelCaption: string = SLSLookupDialogFindLabelCaption_rst;
  SLSLookupDialogOKButton: string = SLSLookupDialogOKButton_rst;
  SLSLookupDialogCancelButton: string = SLSLookupDialogCancelButton_rst;
  // Date popup msgs
  SLSDateEditPopupOKButton: string = SLSDateEditPopupOKButton_rst;
  SLSDateEditPopupCancelButton: string = SLSDateEditPopupCancelButton_rst;

implementation

end.

