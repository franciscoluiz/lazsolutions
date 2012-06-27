{$DEFINE LSLANGUAGE_EN}
{.$DEFINE LSLANGUAGE_PTBR}
{$IFDEF LSLANGUAGE_EN}
  SLSUtilsGetLinuxDistroError_rst = 'ERROR: Could not determine the distro name.';
  SLSUtilsListProcessError_rst = 'ERROR: Error showing process list.';
  SLSPlayWAVPlayerNotFound_rst = 'ERROR: Could not determine the player name.';
  SLSProcessThreadCanceled_rst = 'Process canceled for user.';
  SLSExceptionDlgCaption_rst = 'LSException';
  SLSExceptionDlgCopyActCaption_rst = '&Copy';
  SLSExceptionDlgEmailActCaption_rst = '&E-mail';
  SLSExceptionDlgTerminateActCaption_rst = '&Terminate';
  SLSExceptionDlgCloseActCaption_rst = '&Close';
  SLSExceptionDlgCopyActHint_rst = 'Copy message';
  SLSExceptionDlgEmailActHint_rst = 'Send e-mail with the error';
  SLSExceptionDlgTerminateActHint_rst = 'Terminate application';
  SLSExceptionDlgCloseActHint_rst = 'Close';
  SLSExceptionDlgMsg_rst = 'Error message:' + LineEnding +
                        '----------------------------------------' + LineEnding +
                        '%s' + LineEnding + LineEnding + 'Date/Time:' + LineEnding +
                        '----------------------------------------' + LineEnding +
                        '%s' + LineEnding + LineEnding + '%s';
  SLSExceptionDlgTerminateConfirmCaption_rst = 'Confirmation';
  SLSExceptionDlgTerminateConfirmMsg_rst = 'Terminate application?';
  SLSHTTPSendGetIPError_rst = 'Could not connect to the site to obtain the IP.';
  SLSHTTPSendUnableToGetError_rst = 'Unable to get the "%s".';
  SLSHTTPSendDownloadCompleteMsg_rst = 'Download "%s" complete and saved in "%s".';
  SLSHTTPSendTotalDownloadSizeMsg_rst = 'Total download size: %n';
  SLSHTTPSendProxyDlgCaption_rst = 'Proxy configuration';
  SLSHTTPSendProxyDlgProxyGBoxCaption_rst = 'P&roxy';
  SLSHTTPSendProxyDlgAuthenticationGBox_rst = '&Authentication';
  SLSHTTPSendProxyDlgHostLblCaption_rst = '&Host';
  SLSHTTPSendProxyDlgPortLblCaption_rst = '&Port';
  SLSHTTPSendProxyDlgUserLblCaption_rst = '&User';
  SLSHTTPSendProxyDlgPasswordLblCaption_rst = 'Pass&word';
  SLSHTTPSendProxyDlgOKBtnCaption_rst = '&OK';
  SLSHTTPSendProxyDlgCancelBtnCaption_rst = '&Cancel';
  SLSHTTPSendUPDlgCaption_rst = 'HTTP configuration';
  SLSHTTPSendUPDlgAuthenticationGBox_rst = '&Authentication';
  SLSHTTPSendUPDlgUserLblCaption_rst = '&User';
  SLSHTTPSendUPDlgPasswordLblCaption_rst = 'Pass&word';
  SLSHTTPSendUPDlgOKBtnCaption_rst = '&OK';
  SLSHTTPSendUPDlgCancelBtnCaption_rst = '&Cancel';
  SLSSMTPSendError_rst = 'SMTP error: %s.' + LineEnding + '%s%s' + LineEnding + LineEnding +
                  'Try sending again.';
  SLSSMTPSendEmailSentSuccessfully_rst = '-E-mail sent successfully!-';
  SLSExpressionNoError_rst = 'No error';
  SLSExpressionEmptyError_rst = 'Empty expression';
  SLSExpressionExprError_rst = 'Error in expression';
  SLSExpressionSolveError_rst = 'Could not solve expression';
  SLSExpressionDivByZeroError_rst = 'Division by zero';
  SLSExpressionFuncError_rst = 'Error in function';
  SLSExpressionFuncNotFoundError_rst = 'Function not found';
  SLSExpressionUnknownError_rst = 'Unknown error';
  SLSControlsRequiredFieldMsg_rst = 'Please, fill this field';
  SLSControlsEmailFieldMsg_rst = 'Please, put a valid email address';
  SLSControlsURLFieldMsg_rst = 'Please, put a valid internet address';
  SLSControlsTelFieldMsg_rst = 'Please, put a valid telephone number';
  SLSControlsPatternFieldMsg_rst = 'Please, put a valid value';
  SLSJSONEmptyDatabaseError_rst = 'JSON ERROR in class (or routine) "%s": Empty database.';
  SLSJSONEmptyArrayError_rst = 'JSON ERROR in class (or routine) "%s": Empty array.';
  SLSJSONEmptyObjectError_rst = 'JSON ERROR in class (or routine) "%s": Empty object.';
  SLSJSONIsNotArrayError_rst = 'JSON ERROR in class (or routine) "%s": Got "%s", expected "TJSONArray".';
  SLSJSONIsNotObjectError_rst = 'JSON ERROR in class (or routine) "%s": Got "%s", expected "TJSONObject".';
  SLSGridsColumnsManagerCaption_rst = 'Columns';
  SLSGridsColumnsManagerOKBtnCaption_rst = '&OK';
  SLSGridsColumnsManagerCancelBtnCaption_rst = '&Cancel';
  SLSLookupDialogCaption_rst = 'LSLookupDialog';
  SLSLookupDialogFindLabelCaption_rst = '&Find';
  SLSLookupDialogOKButton_rst = '&OK';
  SLSLookupDialogCancelButton_rst = '&Cancel';
  SLSDateEditPopupOKButton_rst = '&OK';
  SLSDateEditPopupCancelButton_rst = '&Cancel';
{$ENDIF}
{$IFDEF LSLANGUAGE_PTBR}
  SLSUtilsGetLinuxDistroError_rst = 'ERRO: Não foi possível determinar o nome da distro.';
  SLSUtilsListProcessError_rst = 'ERRO: Erro ao mostrar lista de processos.';
  SLSPlayWAVPlayerNotFound_rst = 'ERRO: Não foi possível determinar o nome do player.';
  SLSProcessThreadCanceled_rst = 'Processo cancelado pelo usuário.';
  SLSExceptionDlgCaption_rst = 'LSException';
  SLSExceptionDlgCopyActCaption_rst = '&Copiar';
  SLSExceptionDlgEmailActCaption_rst = '&E-mail';
  SLSExceptionDlgTerminateActCaption_rst = '&Cessar';
  SLSExceptionDlgCloseActCaption_rst = '&Fechar';
  SLSExceptionDlgCopyActHint_rst = 'Copiar mensagem';
  SLSExceptionDlgEmailActHint_rst = 'Enviar e-mail com o erro';
  SLSExceptionDlgTerminateActHint_rst = 'Cessar aplicativo';
  SLSExceptionDlgCloseActHint_rst = 'Fechar';
  SLSExceptionDlgMsg_rst = 'Mensagem de erro:' + LineEnding +
                        '----------------------------------------' + LineEnding +
                        '%s' + LineEnding + LineEnding + 'Data/Hora:' + LineEnding +
                        '----------------------------------------' + LineEnding +
                        '%s' + LineEnding + LineEnding + '%s';
  SLSExceptionDlgTerminateConfirmCaption_rst = 'Confirmação';
  SLSExceptionDlgTerminateConfirmMsg_rst = 'Cessar aplicação?';
  SLSHTTPSendGetIPError_rst = 'Não foi possível conectar ao site para obter o IP.';
  SLSHTTPSendUnableToGetError_rst = 'Não foi possível obter o "%s".';
  SLSHTTPSendDownloadCompleteMsg_rst = 'Download "%s" completo e salvo em "%s".';
  SLSHTTPSendTotalDownloadSizeMsg_rst = 'Tamanho total do download: %n';
  SLSHTTPSendProxyDlgCaption_rst = 'Configuração de proxy';
  SLSHTTPSendProxyDlgProxyGBoxCaption_rst = 'P&roxy';
  SLSHTTPSendProxyDlgAuthenticationGBox_rst = '&Autenticação';
  SLSHTTPSendProxyDlgHostLblCaption_rst = '&Host';
  SLSHTTPSendProxyDlgPortLblCaption_rst = '&Porta';
  SLSHTTPSendProxyDlgUserLblCaption_rst = '&Usuário';
  SLSHTTPSendProxyDlgPasswordLblCaption_rst = '&Senha';
  SLSHTTPSendProxyDlgOKBtnCaption_rst = '&OK';
  SLSHTTPSendProxyDlgCancelBtnCaption_rst = '&Cancelar';
  SLSHTTPSendUPDlgCaption_rst = 'Configuração de HTTP';
  SLSHTTPSendUPDlgAuthenticationGBox_rst = '&Autenticação';
  SLSHTTPSendUPDlgUserLblCaption_rst = '&Usuário';
  SLSHTTPSendUPDlgPasswordLblCaption_rst = '&Senha';
  SLSHTTPSendUPDlgOKBtnCaption_rst = '&OK';
  SLSHTTPSendUPDlgCancelBtnCaption_rst = '&Cancelar';
  SLSSMTPSendError_rst = 'SMTP erro: %s.' + LineEnding + '%s%s' + LineEnding + LineEnding +
                  'Tente enviar novamente.';
  SLSSMTPSendEmailSentSuccessfully_rst = '-E-mail enviado com sucesso!-';
  SLSExpressionNoError_rst = 'Nenhum erro';
  SLSExpressionEmptyError_rst = 'Expressão vazia';
  SLSExpressionExprError_rst = 'Erro na expressão';
  SLSExpressionSolveError_rst = 'Não foi possível resolver a expressão';
  SLSExpressionDivByZeroError_rst = 'Divisão por zero';
  SLSExpressionFuncError_rst = 'Erro na função';
  SLSExpressionFuncNotFoundError_rst = 'Função não encontrada';
  SLSExpressionUnknownError_rst = 'Erro desconhecido';
  SLSControlsRequiredFieldMsg_rst = 'Por favor, preencha este campo';
  SLSControlsEmailFieldMsg_rst = 'Por favor, informe um email válido';
  SLSControlsURLFieldMsg_rst = 'Por favor, informe um endereço válido de internet';
  SLSControlsTelFieldMsg_rst = 'Por favor, informe um número válido de telefone';
  SLSControlsPatternFieldMsg_rst = 'Por favor, informe um valor válido';
  SLSJSONEmptyDatabaseError_rst = 'ERRO JSON na classe (ou rotina) "%s": Database vazio.';
  SLSJSONEmptyArrayError_rst = 'ERRO JSON na classe (ou rotina) "%s": Array vazio.';
  SLSJSONEmptyObjectError_rst = 'ERRO JSON na classe (ou rotina) "%s": Objeto vazio.';
  SLSJSONIsNotArrayError_rst = 'ERRO JSON na classe (ou rotina) "%s": Recebeu "%s", esperado "TJSONArray".';
  SLSJSONIsNotObjectError_rst = 'ERRO JSON na classe (ou rotina) "%s": Recebeu "%s", esperado "TJSONObject".';
  SLSGridsColumnsManagerCaption_rst = 'Colunas';
  SLSGridsColumnsManagerOKBtnCaption_rst = '&OK';
  SLSGridsColumnsManagerCancelBtnCaption_rst = '&Cancelar';
  SLSLookupDialogCaption_rst = 'LSLookupDialog';
  SLSLookupDialogFindLabelCaption_rst = '&Encontrar';
  SLSLookupDialogOKButton_rst = '&OK';
  SLSLookupDialogCancelButton_rst = '&Cancelar';
  SLSDateEditPopupOKButton_rst = '&OK';
  SLSDateEditPopupCancelButton_rst = '&Cancelar';
{$ENDIF}
